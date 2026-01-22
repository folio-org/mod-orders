package org.folio.service.orders;

import static io.vertx.core.Future.succeededFuture;
import static java.util.Collections.emptyList;
import static java.util.stream.Collectors.groupingBy;
import static org.folio.orders.utils.HelperUtils.buildConversionQuery;
import static org.folio.orders.utils.HelperUtils.calculateCostUnitsTotal;
import static org.folio.orders.utils.QueryUtils.convertIdsToCqlQuery;
import static org.folio.rest.RestConstants.MAX_IDS_FOR_GET_RQ_15;
import static org.folio.rest.RestVerticle.OKAPI_HEADER_TENANT;
import static org.folio.rest.acq.model.finance.ExchangeRate.OperationMode.DIVIDE;
import static org.folio.rest.acq.model.finance.LedgerFiscalYearRolloverError.ErrorType.ORDER_ROLLOVER;
import static org.folio.rest.jaxrs.model.RolloverStatus.ERROR;
import static org.folio.rest.jaxrs.model.RolloverStatus.SUCCESS;

import java.math.BigDecimal;
import java.util.ArrayList;
import java.util.HashSet;
import java.util.List;
import java.util.Map;
import java.util.Objects;
import java.util.UUID;
import java.util.stream.Collectors;

import javax.money.Monetary;
import javax.money.MonetaryAmount;
import javax.money.convert.CurrencyConversion;

import org.apache.logging.log4j.LogManager;
import org.apache.logging.log4j.Logger;
import org.folio.dao.FailedLedgerRolloverPoLineDao;
import org.folio.models.FailedLedgerRolloverPoLineDto;
import org.folio.models.PoLineEncumbrancesHolder;
import org.folio.rest.acq.model.finance.Encumbrance;
import org.folio.rest.acq.model.finance.ExchangeRate;
import org.folio.rest.acq.model.finance.Fund;
import org.folio.rest.acq.model.finance.LedgerFiscalYearRolloverProgress;
import org.folio.rest.acq.model.finance.Transaction;
import org.folio.rest.core.exceptions.ErrorCodes;
import org.folio.rest.core.exceptions.HttpException;
import org.folio.rest.core.models.RequestContext;
import org.folio.rest.jaxrs.model.Cost;
import org.folio.rest.jaxrs.model.EncumbranceRollover;
import org.folio.rest.jaxrs.model.FundDistribution;
import org.folio.rest.jaxrs.model.FundDistribution.DistributionType;
import org.folio.rest.jaxrs.model.LedgerFiscalYearRollover;
import org.folio.rest.jaxrs.model.PoLine;
import org.folio.rest.jaxrs.model.PurchaseOrder;
import org.folio.rest.jaxrs.model.RolloverStatus;
import org.folio.service.caches.CommonSettingsCache;
import org.folio.service.exchange.CacheableExchangeRateService;
import org.folio.service.exchange.CustomExchangeRateProvider;
import org.folio.service.finance.FundService;
import org.folio.service.finance.rollover.LedgerRolloverErrorService;
import org.folio.service.finance.rollover.LedgerRolloverProgressService;
import org.folio.service.finance.transaction.TransactionService;
import org.folio.utils.iterators.FutureIterator;
import org.javamoney.moneta.Money;

import io.vertx.core.Future;
import io.vertx.core.json.JsonObject;

public class OrderRolloverService {

  private static final Logger logger = LogManager.getLogger();

  private static final String PO_LINE_FUND_DISTR_QUERY = "poLine.fundDistribution =/@fundId %s";
  // Following cql query filters orders whose po lines do not contain 'encumbrance' value in the entire fundDistribution array.
  // That condition skips orders already processed in previous fiscal years.
  private static final String PO_LINE_NON_EMPTY_ENCUMBRANCE_QUERY = "poLine.fundDistribution == \"*\\\"encumbrance\\\": \\\"*\"";
  private static final String ORDER_TYPE_QUERY = "orderType == %s";
  private static final String ENCUMBR_FY_QUERY = "fiscalYearId == \"%s\"";
  private static final String OR = " or ";
  private static final String AND = " and ";

  private final FundService fundService;
  private final PurchaseOrderStorageService purchaseOrderStorageService;
  private final PurchaseOrderLineService purchaseOrderLineService;
  private final TransactionService transactionService;
  private final CommonSettingsCache commonSettingsCache;
  private final LedgerRolloverProgressService ledgerRolloverProgressService;
  private final LedgerRolloverErrorService ledgerRolloverErrorService;
  private final FailedLedgerRolloverPoLineDao failedLedgerRolloverPoLineDao;
  private final CacheableExchangeRateService cacheableExchangeRateService;

  public OrderRolloverService(FundService fundService, PurchaseOrderStorageService purchaseOrderStorageService,
      PurchaseOrderLineService purchaseOrderLineService, TransactionService transactionService,
      CommonSettingsCache commonSettingsCache, LedgerRolloverProgressService ledgerRolloverProgressService,
      LedgerRolloverErrorService ledgerRolloverErrorService, FailedLedgerRolloverPoLineDao failedLedgerRolloverPoLineDao,
      CacheableExchangeRateService cacheableExchangeRateService) {
    this.fundService = fundService;
    this.purchaseOrderStorageService = purchaseOrderStorageService;
    this.purchaseOrderLineService = purchaseOrderLineService;
    this.transactionService = transactionService;
    this.commonSettingsCache = commonSettingsCache;
    this.ledgerRolloverProgressService = ledgerRolloverProgressService;
    this.ledgerRolloverErrorService = ledgerRolloverErrorService;
    this.failedLedgerRolloverPoLineDao = failedLedgerRolloverPoLineDao;
    this.cacheableExchangeRateService = cacheableExchangeRateService;
  }

  public Future<Void> rollover(LedgerFiscalYearRollover ledgerFYRollover, RequestContext requestContext) {
    return prepareRollover(ledgerFYRollover, requestContext)
      // order rollover should be executed asynchronously, because of it .onSuccess is used here
      .onSuccess(v -> ledgerRolloverProgressService.getRolloversProgressByRolloverId(ledgerFYRollover.getId(), requestContext)
        .compose(progress -> startRollover(ledgerFYRollover, progress, requestContext)));
  }

  public Future<Void> prepareRollover(LedgerFiscalYearRollover ledgerFYRollover, RequestContext requestContext) {
    return ledgerRolloverProgressService.getRolloversProgressByRolloverId(ledgerFYRollover.getId(), requestContext)
      .map(progress -> progress.withOrdersRolloverStatus(RolloverStatus.IN_PROGRESS))
      .compose(progressToUpdate -> ledgerRolloverProgressService.updateRolloverProgress(progressToUpdate, requestContext));
  }

  public Future<Void> startRollover(LedgerFiscalYearRollover ledgerFYRollover, LedgerFiscalYearRolloverProgress progress,
      RequestContext requestContext) {
    return commonSettingsCache.getSystemCurrency(requestContext)
      .compose(systemCurrency -> getLedgerFundIds(ledgerFYRollover.getLedgerId(), requestContext)
        .compose(fundIds -> composeRolloverOrders(fundIds, ledgerFYRollover, systemCurrency, requestContext)))
      .recover(t -> handleOrderRolloverError(t, ledgerFYRollover, progress, requestContext))
      .compose(v -> calculateAndUpdateOverallProgressStatus(progress.withOrdersRolloverStatus(SUCCESS), requestContext))
      .onSuccess(v -> logger.info("Order Rollover success : All orders processed"))
      .onFailure(t -> logger.error("Order Rollover failed", t));
  }

  private Future<List<String>> getLedgerFundIds(String ledgerId, RequestContext requestContext) {
    return fundService.getFundsByLedgerId(ledgerId, requestContext)
      .map(ledgerFunds -> ledgerFunds.stream().map(Fund::getId).toList());
  }

  private Future<Void> composeRolloverOrders(List<String> fundIds, LedgerFiscalYearRollover ledgerFYRollover, String systemCurrency,
      RequestContext requestContext) {
    // Call rolloverOrders() for open orders and other orders
    // Recover errors so that both are called even if the first one fails
    // andThen() does not work for that (it does not wait for the future parameter completion)
    List<Throwable> errors = new ArrayList<>();
    return rolloverOrders(fundIds, ledgerFYRollover, systemCurrency, true, requestContext)
      .recover(t -> {
        errors.add(t);
        return succeededFuture();
      })
      .compose(v -> rolloverOrders(fundIds, ledgerFYRollover, systemCurrency, false, requestContext))
      .recover(t -> {
        errors.add(t);
        return succeededFuture();
      })
      .map(v -> {
        for (Throwable error : errors) {
          throw new RuntimeException(error.getCause());
        }
        return null;
      });
  }

  private Future<Void> rolloverOrders(List<String> fundIds, LedgerFiscalYearRollover ledgerFYRollover, String systemCurrency,
      boolean openOrders, RequestContext requestContext) {
    // Rollover the orders by funds without ever loading all orders or all order lines into memory.
    // Batches are optimized to avoid processing too few or too many at a time.
    logger.info("rolloverOrders(openOrders={}):: start", openOrders);
    var poIdIterator = getOrderIds(fundIds, ledgerFYRollover, openOrders, requestContext);
    var poLinesIterator = getPoLines(poIdIterator, requestContext);
    var lineHolderIterator = getEncumbrances(poLinesIterator, ledgerFYRollover, requestContext);
    var modifiedPoLineIterator = FutureIterator.applyFunction(lineHolderIterator,
      holder -> rolloverPoLine(holder, systemCurrency, openOrders, requestContext));
    return saveModifiedLines(modifiedPoLineIterator, ledgerFYRollover, openOrders, requestContext)
      .onSuccess(v -> logger.info("rolloverOrders(openOrders={}):: success", openOrders))
      .onFailure(t -> logger.error("rolloverOrders(openOrders={}):: error", openOrders, t));
  }

  /**
   * Get the order ids from orders matching the funds, without loading all orders into memory
   */
  private FutureIterator<String> getOrderIds(List<String> fundIds, LedgerFiscalYearRollover ledgerFYRollover,
      boolean openOrders, RequestContext requestContext) {
    var fundIdsIterator = FutureIterator.chunk(FutureIterator.fromIterator(fundIds.listIterator()), MAX_IDS_FOR_GET_RQ_15);
    var posIteratorIterator = FutureIterator.applyFunction(fundIdsIterator,
      fundIdsChunk -> {
        String baseQuery = buildBaseOrderQuery(fundIdsChunk, openOrders, ledgerFYRollover);
        return succeededFuture(purchaseOrderStorageService.getPurchaseOrderIterator(baseQuery, requestContext));
      });
    var poIterator = FutureIterator.dechunk(FutureIterator.flatten(posIteratorIterator));
    var poIdIterator = FutureIterator.applyFunction(poIterator, order -> succeededFuture(order.getId()));
    // Remove duplicate order ids - this loads all ids into memory
    HashSet<String> processed = new HashSet<>();
    return FutureIterator.applyFunction(poIdIterator, id -> {
      if (processed.contains(id)) {
        return succeededFuture(null);
      }
      processed.add(id);
      return succeededFuture(id);
    });
  }

  protected String buildBaseOrderQuery(List<String> fundIds, boolean openOrders, LedgerFiscalYearRollover ledgerFYRollover) {
    StringBuilder resultQuery = new StringBuilder();
    if (!ledgerFYRollover.getEncumbrancesRollover().isEmpty()) {
      resultQuery.append("(").append(buildOrderTypesQuery(ledgerFYRollover)).append(")").append(AND);
    }
    resultQuery.append("(").append(buildOrderStatusQuery(openOrders)).append(")")
      .append(AND)
      .append("(").append(buildFundIdQuery(fundIds)).append(")");
    if (!openOrders) {
      // MODORDERS-904 Avoid rollover re-processing of old already processed closed orders in previous fiscal years
      resultQuery.append(AND).append("(").append(PO_LINE_NON_EMPTY_ENCUMBRANCE_QUERY).append(")");
    }
    return resultQuery.toString();
  }

  private String buildOrderTypesQuery(LedgerFiscalYearRollover ledgerFYRollover) {
    return ledgerFYRollover.getEncumbrancesRollover().stream()
      .map(encumbrancesRollover -> convertToOrderType(encumbrancesRollover.getOrderType()))
      .map(PurchaseOrder.OrderType::toString)
      .distinct()
      .map(orderType -> String.format(ORDER_TYPE_QUERY, orderType))
      .collect(Collectors.joining(OR));
  }

  private PurchaseOrder.OrderType convertToOrderType(EncumbranceRollover.OrderType encumberRolloverType) {
    if (EncumbranceRollover.OrderType.ONE_TIME == encumberRolloverType) {
      return PurchaseOrder.OrderType.ONE_TIME;
    }
    return PurchaseOrder.OrderType.ONGOING;
  }

  private String buildOrderStatusQuery(boolean openOrders) {
    String statusQuery = "workflowStatus";
    if (openOrders) {
      statusQuery += "==";
    } else {
      statusQuery += "<>";
    }
    statusQuery += "Open";
    return statusQuery;
  }

  private String buildFundIdQuery(List<String> fundIds) {
    String fundIdsString = "(" + String.join(OR, fundIds) + ")";
    return String.format(PO_LINE_FUND_DISTR_QUERY, fundIdsString);
  }

  /**
   * Get the po lines matching the po ids, without loading them all into memory.
   * Use chunks of MAX_IDS_FOR_GET_RQ_15 order ids when getting the orders.
   */
  private FutureIterator<PoLine> getPoLines(FutureIterator<String> poIdIterator, RequestContext requestContext) {
    var poIdsIterator = FutureIterator.chunk(poIdIterator, MAX_IDS_FOR_GET_RQ_15);
    var poLinesIteratorIterator = FutureIterator.applyFunction(poIdsIterator,
      poIdsChunk -> succeededFuture(purchaseOrderLineService.getOrderLinesByOrderIds(poIdsChunk, requestContext)));
    return FutureIterator.dechunk(FutureIterator.flatten(poLinesIteratorIterator));
  }

  private FutureIterator<PoLineEncumbrancesHolder> getEncumbrances(FutureIterator<PoLine> poLineIterator,
      LedgerFiscalYearRollover ledgerFYRollover, RequestContext requestContext) {
    var poLinesIterator = FutureIterator.chunk(poLineIterator, MAX_IDS_FOR_GET_RQ_15);
    var linesAndEncumbrancesIterator = FutureIterator.applyFunction(poLinesIterator, poLines -> {
      var poLineIds = poLines.stream()
        .map(PoLine::getId)
        .toList();
      String query = buildQueryEncumbrancesForRollover(poLineIds, ledgerFYRollover);
      return transactionService.getTransactions(query, requestContext)
        .map(transactions -> poLines.stream()
          .map(poLine -> {
            List<Transaction> poLineTransactions = transactions.stream()
              .filter(t -> poLine.getId().equals(t.getEncumbrance().getSourcePoLineId()))
              .toList();
            return new PoLineEncumbrancesHolder(poLine)
              .withEncumbrances(poLineTransactions);
          })
          .toList());
    });
    return FutureIterator.dechunk(linesAndEncumbrancesIterator);
  }

  private String buildQueryEncumbrancesForRollover(List<String> poLineIds, LedgerFiscalYearRollover ledgerFYRollover) {
    String fiscalYearIdsQuery = String.format(ENCUMBR_FY_QUERY, ledgerFYRollover.getToFiscalYearId());
    String orderLineIdsQuery = convertIdsToCqlQuery(poLineIds, "encumbrance.sourcePoLineId");
    return "(" + fiscalYearIdsQuery + ")" + AND + "(" + orderLineIdsQuery + ")";
  }

  private Future<PoLine> rolloverPoLine(PoLineEncumbrancesHolder holder, String systemCurrency, boolean openOrders, RequestContext requestContext) {
    logger.debug("rolloverPoLine(openOrders={})", openOrders);
    if (!openOrders) {
      return succeededFuture(removeEncumbranceLinks(holder.getPoLine()));
    }
    if (holder.getEncumbrances().isEmpty()) {
      // If an order has a fund in the ledger but encumbrances were not created yet in the fiscal year rollover
      // (because it also has funds in another ledger that was not rolled over), updating the po line is skipped.
      // This prevents updating the po line twice.
      return succeededFuture(null);
    }
    return getExchangeRatesPerPoLine(holder, systemCurrency, requestContext)
      .map(v -> {
        applyPoLineRolloverChanges(holder);
        return holder.getPoLine();
      });
  }

  private PoLine removeEncumbranceLinks(PoLine poLine) {
    var modified = false;
    for (FundDistribution fd : poLine.getFundDistribution()) {
      if (fd.getEncumbrance() != null) {
        fd.setEncumbrance(null);
        modified = true;
      }
    }
    return modified ? poLine : null;
  }

  private Future<Void> getExchangeRatesPerPoLine(PoLineEncumbrancesHolder holder, String fromCurrency, RequestContext requestContext) {
    var poLine = holder.getPoLine();
    var cost = poLine.getCost();
    var isCustomExchangeRate = Objects.nonNull(cost.getExchangeRate());
    return cacheableExchangeRateService.getExchangeRate(fromCurrency, cost.getCurrency(), cost.getExchangeRate(), requestContext)
      .map(exchangeRate -> {
        var conversion = retrieveCurrencyConversion(exchangeRate, isCustomExchangeRate);
        holder.withCurrencyConversion(conversion)
          .withSystemCurrency(exchangeRate.getFrom());
        return null;
      });
  }

  private void applyPoLineRolloverChanges(PoLineEncumbrancesHolder holder) {
    logger.debug("Starting apply rollover changes");
    var currEncumbranceFundIdMap = holder.getEncumbrances().stream().collect(groupingBy(Transaction::getFromFundId));
    updatePoLineCostWithFundDistribution(holder, currEncumbranceFundIdMap);
  }

  private void updatePoLineCostWithFundDistribution(PoLineEncumbrancesHolder holder, Map<String, List<Transaction>> currEncumbranceFundIdMap) {
    PoLine poLine = holder.getPoLine();
    BigDecimal totalCurrEncumbranceInitialAmount = calculateTotalInitialAmountEncumbered(holder);
    Double poLineEstimatedPriceBeforeRollover = poLine.getCost().getPoLineEstimatedPrice();
    Double poLineEstimatedPriceAfterRollover = totalCurrEncumbranceInitialAmount.doubleValue();
    Double fyroAdjustmentAmount = calculateFYROAdjustmentAmount(holder, totalCurrEncumbranceInitialAmount).getNumber().doubleValue();
    poLine.getCost().setFyroAdjustmentAmount(fyroAdjustmentAmount);
    poLine.getCost().setPoLineEstimatedPrice(poLineEstimatedPriceAfterRollover);
    updateFundDistribution(poLine, currEncumbranceFundIdMap, poLineEstimatedPriceBeforeRollover, poLineEstimatedPriceAfterRollover);
  }

  protected BigDecimal calculateTotalInitialAmountEncumbered(PoLineEncumbrancesHolder holder) {
    BigDecimal totalAmountBeforeConversion = holder.getEncumbrances().stream()
      .map(Transaction::getEncumbrance)
      .map(Encumbrance::getInitialAmountEncumbered)
      .map(BigDecimal::valueOf).reduce(BigDecimal.ZERO, BigDecimal::add);
    Cost cost = holder.getPoLine().getCost();
    logger.debug("calculateTotalInitialAmountEncumbered:: initiating currency conversion: {} -> {}", holder.getSystemCurrency(), cost.getCurrency());
    logger.debug("calculateTotalInitialAmountEncumbered:: totalAmountBeforeConversion: {}", totalAmountBeforeConversion.doubleValue());
    Number totalAmountAfterConversion = amountWithConversion(totalAmountBeforeConversion, holder).getNumber();
    logger.debug("calculateTotalInitialAmountEncumbered:: totalAmountAfterConversion: {}", totalAmountAfterConversion.doubleValue());
    return BigDecimal.valueOf(totalAmountAfterConversion.doubleValue());
  }

  private MonetaryAmount amountWithConversion(BigDecimal totalInitialAmountEncumbered, PoLineEncumbrancesHolder holder) {
    return Money.of(totalInitialAmountEncumbered, holder.getSystemCurrency())
      .with(holder.getCurrencyConversion())
      .with(Monetary.getDefaultRounding());
  }

  private MonetaryAmount calculateFYROAdjustmentAmount(PoLineEncumbrancesHolder holder, BigDecimal totalCurrEncumbranceInitialAmount) {
    Cost cost = holder.getPoLine().getCost();
    MonetaryAmount costUnitsTotal = calculateCostUnitsTotal(cost);
    MonetaryAmount totalMonetaryCurrEncumbranceInitialAmount = Money.of(totalCurrEncumbranceInitialAmount, cost.getCurrency());
    return totalMonetaryCurrEncumbranceInitialAmount.subtract(costUnitsTotal).with(Monetary.getDefaultRounding());
  }

  private void updateFundDistribution(PoLine poLine, Map<String, List<Transaction>> currEncumbranceFundIdMap,
      Double poLineEstimatedPriceBeforeRollover, Double poLineEstimatedPriceAfterRollover) {
    poLine.getFundDistribution().forEach(fundDistribution -> {
      if (fundDistribution.getDistributionType().equals(DistributionType.AMOUNT)) {
        MonetaryAmount fdAmount = Money.of(fundDistribution.getValue(), poLine.getCost().getCurrency());
        MonetaryAmount newFdAmount = (poLineEstimatedPriceBeforeRollover != 0)
          ? fdAmount.divide(poLineEstimatedPriceBeforeRollover).multiply(poLineEstimatedPriceAfterRollover)
          : Money.of(0, poLine.getCost().getCurrency());
        fundDistribution.setValue(newFdAmount.getNumber().doubleValue());
      }
      var currEncumbrances = currEncumbranceFundIdMap.getOrDefault(fundDistribution.getFundId(), emptyList());
      currEncumbrances.forEach(encumbrance -> {
        if (encumbrance.getExpenseClassId() != null && fundDistribution.getExpenseClassId() != null) {
          if (encumbrance.getExpenseClassId().equals(fundDistribution.getExpenseClassId())) {
            fundDistribution.setEncumbrance(encumbrance.getId());
          }
        } else if (encumbrance.getExpenseClassId() == null && fundDistribution.getExpenseClassId() == null) {
          fundDistribution.setEncumbrance(encumbrance.getId());
        }
      });
    });
  }

  protected CurrencyConversion retrieveCurrencyConversion(ExchangeRate exchangeRate, boolean isCustomExchangeRate) {
    var query = buildConversionQuery(exchangeRate.getFrom(), exchangeRate.getTo(), exchangeRate.getExchangeRate());
    var operationMode = isCustomExchangeRate ? DIVIDE : exchangeRate.getOperationMode();
    logger.debug("retrieveCurrencyConversion:: Using operationMode: {}", operationMode);
    var provider = new CustomExchangeRateProvider(operationMode);
    return provider.getCurrencyConversion(query);
  }

  private Future<Void> saveModifiedLines(FutureIterator<PoLine> modifiedPoLineIterator, LedgerFiscalYearRollover ledgerFYRollover,
      boolean openOrders, RequestContext requestContext) {
    int polPartitionSize = purchaseOrderLineService.getPoLinePartitionSize();
    var modifiedPoLinesIterator = FutureIterator.chunk(modifiedPoLineIterator, polPartitionSize);
    return modifiedPoLinesIterator.applyToAll(poLines -> saveOrderLines(poLines, ledgerFYRollover, openOrders, requestContext));
  }

  private Future<Void> saveOrderLines(List<PoLine> orderLines, LedgerFiscalYearRollover ledgerFYRollover,
      boolean openOrders, RequestContext requestContext) {
    return purchaseOrderLineService.saveOrderLinesWithoutSearchLocationsUpdate(orderLines, requestContext)
      .recover(t -> handlePoLineUpdateFailures(orderLines, ledgerFYRollover, openOrders, t, requestContext)
        .map(v -> {
          throw new HttpException(400, ErrorCodes.PO_LINE_ROLLOVER_FAILED.toError());
        }));
  }

  private Future<Void> handlePoLineUpdateFailures(List<PoLine> poLines, LedgerFiscalYearRollover ledgerFYRollover,
      boolean openOrders, Throwable t, RequestContext requestContext) {
    return Future.join(poLines.stream()
        .map(poLine -> handlePoLineUpdateFailure(poLine, ledgerFYRollover, openOrders, t, requestContext))
        .toList())
      .mapEmpty();
  }

  private Future<Void> handlePoLineUpdateFailure(PoLine poLine, LedgerFiscalYearRollover ledgerFYRollover, boolean openOrders,
      Throwable t, RequestContext requestContext) {
    logger.error("PO line {} update failed while making rollover", poLine.getId(), t);
    var failureDto = new FailedLedgerRolloverPoLineDto(
      UUID.randomUUID(), // id
      UUID.fromString(ledgerFYRollover.getId()), // rolloverId
      UUID.fromString(ledgerFYRollover.getLedgerId()), // ledgerId
      UUID.fromString(poLine.getId()), // poLineId
      JsonObject.mapFrom(poLine).encode(), // requestBody
      t.getMessage(), // responseBody
      ((HttpException) t).getCode(), // statusCode
      openOrders ? "Open" : "Closed OR Pending" // purchase order workflow status (saved as text)
    );
    return failedLedgerRolloverPoLineDao.saveFailedRolloverRecord(failureDto, requestContext.getHeaders().get(OKAPI_HEADER_TENANT));
  }

  private Future<Void> handleOrderRolloverError(Throwable t, LedgerFiscalYearRollover rollover, LedgerFiscalYearRolloverProgress progress,
      RequestContext requestContext) {
    logger.error("Orders rollover failed for ledger {}", rollover.getLedgerId(), t);
    return ledgerRolloverErrorService.saveRolloverError(rollover.getId(), t, ORDER_ROLLOVER, "Overall order rollover", requestContext)
      .compose(v -> ledgerRolloverProgressService.updateRolloverProgress(progress.withOrdersRolloverStatus(ERROR).withOverallRolloverStatus(ERROR), requestContext))
      .compose(v -> Future.failedFuture(t));
  }

  private Future<Void> calculateAndUpdateOverallProgressStatus(LedgerFiscalYearRolloverProgress progress, RequestContext requestContext) {
    return ledgerRolloverErrorService.getRolloverErrorsByRolloverId(progress.getLedgerRolloverId(), requestContext)
      .compose(rolloverErrors -> {
        if (rolloverErrors.getTotalRecords() == 0) {
          progress.setOverallRolloverStatus(SUCCESS);
        } else {
          progress.setOverallRolloverStatus(ERROR);
        }
        return ledgerRolloverProgressService.updateRolloverProgress(progress, requestContext);
      });
  }
}
