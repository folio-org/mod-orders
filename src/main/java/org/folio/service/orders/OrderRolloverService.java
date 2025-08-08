package org.folio.service.orders;

import static java.util.Collections.emptyList;
import static java.util.stream.Collectors.groupingBy;
import static one.util.streamex.StreamEx.ofSubLists;
import static org.folio.orders.utils.HelperUtils.buildConversionQuery;
import static org.folio.orders.utils.HelperUtils.calculateCostUnitsTotal;
import static org.folio.orders.utils.HelperUtils.collectResultsOnSuccess;
import static org.folio.rest.RestConstants.MAX_IDS_FOR_GET_RQ_15;
import static org.folio.rest.RestVerticle.OKAPI_HEADER_TENANT;
import static org.folio.rest.acq.model.finance.ExchangeRate.OperationMode.DIVIDE;
import static org.folio.rest.acq.model.finance.LedgerFiscalYearRolloverError.ErrorType.ORDER_ROLLOVER;
import static org.folio.rest.jaxrs.model.RolloverStatus.ERROR;
import static org.folio.rest.jaxrs.model.RolloverStatus.SUCCESS;

import java.math.BigDecimal;
import java.util.ArrayList;
import java.util.Collection;
import java.util.List;
import java.util.Map;
import java.util.NoSuchElementException;
import java.util.Objects;
import java.util.UUID;
import java.util.concurrent.atomic.AtomicInteger;
import java.util.stream.Collectors;
import java.util.stream.IntStream;

import javax.money.Monetary;
import javax.money.MonetaryAmount;
import javax.money.convert.CurrencyConversion;

import io.vertx.core.AsyncResult;
import org.apache.commons.collections4.MapUtils;
import org.apache.commons.lang3.StringUtils;
import org.apache.commons.lang3.tuple.Pair;
import org.apache.logging.log4j.LogManager;
import org.apache.logging.log4j.Logger;
import org.folio.dao.FailedLedgerRolloverPoLineDao;
import org.folio.models.FailedLedgerRolloverPoLineDto;
import org.folio.models.PoLineEncumbrancesHolder;
import org.folio.models.RolloverConversionHolder;
import org.folio.okapi.common.GenericCompositeFuture;
import org.folio.orders.utils.HelperUtils;
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
import org.folio.rest.jaxrs.model.PoLineCollection;
import org.folio.rest.jaxrs.model.PurchaseOrder;
import org.folio.rest.jaxrs.model.RolloverStatus;
import org.folio.service.caches.CommonSettingsCache;
import org.folio.service.exchange.CacheableExchangeRateService;
import org.folio.service.exchange.CustomExchangeRateProvider;
import org.folio.service.finance.FundService;
import org.folio.service.finance.rollover.LedgerRolloverErrorService;
import org.folio.service.finance.rollover.LedgerRolloverProgressService;
import org.folio.service.finance.transaction.TransactionService;
import org.javamoney.moneta.Money;

import com.google.common.collect.Lists;

import io.vertx.core.Future;
import io.vertx.core.json.JsonObject;

public class OrderRolloverService {

  private static final Logger logger = LogManager.getLogger();

  private static final String PO_LINE_FUND_DISTR_QUERY = "fundDistribution =/@fundId \"%s\"";
  // Following cql query filters po_lines which do not contain 'encumbrance' value in entire fundDistribution array.
  // That condition skips po_lines already processed in previous fiscal years.
  private static final String PO_LINE_NON_EMPTY_ENCUMBRANCE_QUERY = "fundDistribution == \"*\\\"encumbrance\\\": \\\"*\"";
  private static final String ORDER_TYPE_QUERY = "purchaseOrder.orderType == %s";
  private static final String ENCUMBR_FY_QUERY = "fiscalYearId == \"%s\"";
  private static final String ENCUMBRANCE_BY_POLINE_ID_QUERY = "encumbrance.sourcePoLineId == \"%s\"";
  private static final String OR = " or ";
  private static final String AND = " and ";
  private static final int POLINES_CHUNK_SIZE_200 = 200;

  private final FundService fundService;
  private final PurchaseOrderLineService purchaseOrderLineService;
  private final TransactionService transactionService;
  private final CommonSettingsCache commonSettingsCache;
  private final LedgerRolloverProgressService ledgerRolloverProgressService;
  private final LedgerRolloverErrorService ledgerRolloverErrorService;
  private final FailedLedgerRolloverPoLineDao failedLedgerRolloverPoLineDao;
  private final CacheableExchangeRateService cacheableExchangeRateService;

  public OrderRolloverService(FundService fundService, PurchaseOrderLineService purchaseOrderLineService, TransactionService transactionService,
                              CommonSettingsCache commonSettingsCache, LedgerRolloverProgressService ledgerRolloverProgressService,
                              LedgerRolloverErrorService ledgerRolloverErrorService, FailedLedgerRolloverPoLineDao failedLedgerRolloverPoLineDao,
                              CacheableExchangeRateService cacheableExchangeRateService) {
    this.fundService = fundService;
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

  public Future<Void> startRollover(LedgerFiscalYearRollover ledgerFYRollover, LedgerFiscalYearRolloverProgress progress, RequestContext requestContext) {
    var fundIdsFuture = fundService.getFundsByLedgerId(ledgerFYRollover.getLedgerId(), requestContext)
      .map(ledgerFunds -> ledgerFunds.stream().map(Fund::getId).toList());
    return fundIdsFuture
      .compose(ledgerFundIds -> commonSettingsCache.getSystemCurrency(requestContext)
        .compose(systemCurrency -> rolloverOrdersByFundIds(ledgerFundIds, ledgerFYRollover, systemCurrency, requestContext)))
      .recover(t -> handleOrderRolloverError(t, ledgerFYRollover, progress, requestContext))
      .compose(v -> calculateAndUpdateOverallProgressStatus(progress.withOrdersRolloverStatus(SUCCESS), requestContext))
      .onSuccess(v -> logger.info("Order Rollover success : All orders processed"))
      .onFailure(t -> logger.error("Order Rollover failed", t));
  }

  private Future<Void> handleOrderRolloverError(Throwable t, LedgerFiscalYearRollover rollover, LedgerFiscalYearRolloverProgress progress, RequestContext requestContext) {
    logger.error("Orders rollover failed for ledger {}", rollover.getLedgerId(), t);
    return ledgerRolloverErrorService.saveRolloverError(rollover.getId(), t, ORDER_ROLLOVER, "Overall order rollover", requestContext)
      .compose(v -> ledgerRolloverProgressService.updateRolloverProgress(progress.withOrdersRolloverStatus(ERROR).withOverallRolloverStatus(ERROR), requestContext))
      .compose(v -> Future.failedFuture(t));
  }

  private Future<Void> rolloverOrdersByFundIds(List<String> ledgerFundIds, LedgerFiscalYearRollover ledgerFYRollover, String systemCurrency, RequestContext requestContext) {
    List<List<String>> fundIdChunks = Lists.partition(ledgerFundIds, MAX_IDS_FOR_GET_RQ_15);
    return HelperUtils.executeWithSemaphores(requestContext.getContext(), 1, fundIdChunks, fundIds ->
        rolloverOrdersByFundIds(fundIds, ledgerFYRollover, systemCurrency, true, requestContext)
          .andThen(openOrdersResult -> rolloverOrdersByFundIds(fundIds, ledgerFYRollover, systemCurrency, false, requestContext)
            .andThen(closedOrdersResult -> throwExceptionIfOneOfFailed(openOrdersResult, closedOrdersResult))))
      .mapEmpty();
  }

  private void throwExceptionIfOneOfFailed(AsyncResult<Void> openOrdersResult, AsyncResult<Void> closedOrdersResult) {
    if (openOrdersResult.failed()) {
      throw new RuntimeException(openOrdersResult.cause());
    }
    if (closedOrdersResult.failed()) {
      throw new RuntimeException(closedOrdersResult.cause());
    }
  }

  private Future<Void> rolloverOrdersByFundIds(List<String> chunkFundIds, LedgerFiscalYearRollover ledgerFYRollover,
                                               String systemCurrency, boolean openOrders, RequestContext requestContext) {
    var query = buildOpenOrClosedOrderQueryByFundIdsAndTypes(chunkFundIds, openOrders, ledgerFYRollover);
    return purchaseOrderLineService.getOrderLineCollection(query, 0, 0, requestContext)
      .map(PoLineCollection::getTotalRecords)
      .compose(totalRecords -> {
        var numberOfChunks = (int) Math.ceil((double) totalRecords / POLINES_CHUNK_SIZE_200);
        var atomicChunkCounter = new AtomicInteger();
        var chunkList = IntStream.range(0, numberOfChunks).boxed().toList();
        return HelperUtils.executeWithSemaphores(requestContext.getContext(), 1, chunkList, chunk ->
          purchaseOrderLineService.getOrderLines(query, atomicChunkCounter.getAndIncrement() * POLINES_CHUNK_SIZE_200, POLINES_CHUNK_SIZE_200, requestContext)
            .compose(poLines -> rolloverOrders(systemCurrency, poLines, ledgerFYRollover, openOrders, requestContext))
            .compose(modifiedPoLines -> saveOrderLines(modifiedPoLines, ledgerFYRollover, openOrders, requestContext)));
      }).mapEmpty();
  }

  private Future<Void> saveOrderLines(List<PoLine> orderLines, LedgerFiscalYearRollover ledgerFYRollover,
                                      boolean openOrders, RequestContext requestContext) {
    logger.info("saveOrderLines:: Saving POLs after rollover processing, size: {}", orderLines.size());
    return purchaseOrderLineService.saveOrderLinesWithoutSearchLocationsUpdate(orderLines, requestContext)
      .recover(t -> handlePoLineUpdateFailures(orderLines, ledgerFYRollover, openOrders, t, requestContext)
        .map(v -> {
          throw new HttpException(400, ErrorCodes.PO_LINE_ROLLOVER_FAILED.toError());
        }));
  }

  private Future<Void> handlePoLineUpdateFailures(List<PoLine> poLines, LedgerFiscalYearRollover ledgerFYRollover,
                                                  boolean openOrders, Throwable t, RequestContext requestContext) {
    return GenericCompositeFuture.join(poLines.stream()
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

  private Future<List<PoLine>> rolloverOrders(String systemCurrency, List<PoLine> poLines, LedgerFiscalYearRollover ledgerFYRollover,
                                              boolean openOrders, RequestContext requestContext) {
    if (poLines.isEmpty()) {
      return Future.succeededFuture(emptyList());
    }
    var poLineIds = poLines.stream()
      .map(PoLine::getId)
      .toList();
    return getEncumbrancesForRollover(poLineIds, ledgerFYRollover, requestContext)
      .compose(encumbrances -> getExchangeRatesPerPoLine(poLines, systemCurrency, requestContext)
        .compose(poLineExchangeRates -> Future.succeededFuture(Pair.of(encumbrances, poLineExchangeRates))))
      .compose(encumbrancePoLineExchangeRates -> {
        if (openOrders) {
          var holders = buildPoLineEncumbrancesHolders(poLines, encumbrancePoLineExchangeRates);
          var modifiedPoLines = applyPoLinesRolloverChanges(holders);
          return Future.succeededFuture(modifiedPoLines);
        } else {
          var encumbrances = encumbrancePoLineExchangeRates.getLeft();
          return removeEncumbrancesFromPoLines(poLines, encumbrances, requestContext);
        }
      });
  }

  private Future<List<PoLine>> removeEncumbrancesFromPoLines(List<PoLine> poLines, List<Transaction> transactions, RequestContext requestContext) {
    return Future.succeededFuture()
      .compose(v -> {
        if (transactions.isEmpty()) {
          return Future.succeededFuture();
        } else {
          List<String> idsOfTransactionsToDelete = transactions.stream().map(Transaction::getId).toList();
          return transactionService.batchDelete(idsOfTransactionsToDelete, requestContext);
        }
      })
      .map(v -> removeEncumbranceLinks(poLines));
  }

  public Future<Void> calculateAndUpdateOverallProgressStatus(LedgerFiscalYearRolloverProgress progress, RequestContext requestContext) {
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

  private List<PoLine> applyPoLinesRolloverChanges(List<PoLineEncumbrancesHolder> poLineEncumbrancesHolders) {
    logger.debug("Starting apply rollover changes");
    poLineEncumbrancesHolders.forEach(holder -> {
      var currEncumbranceFundIdMap = holder.getEncumbrances().stream().collect(groupingBy(Transaction::getFromFundId));
      if (!MapUtils.isEmpty(currEncumbranceFundIdMap)) {
        updatePoLineCostWithFundDistribution(holder, currEncumbranceFundIdMap);
      }
    });
    return poLineEncumbrancesHolders.stream().map(PoLineEncumbrancesHolder::getPoLine).toList();
  }

  private List<PoLine> removeEncumbranceLinks(List<PoLine> poLines) {
    for (PoLine pol : poLines) {
      for (FundDistribution fd : pol.getFundDistribution()) {
        fd.setEncumbrance(null);
      }
    }
    return poLines;
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

  private MonetaryAmount calculateFYROAdjustmentAmount(PoLineEncumbrancesHolder holder, BigDecimal totalCurrEncumbranceInitialAmount) {
    Cost cost = holder.getPoLine().getCost();
    MonetaryAmount costUnitsTotal = calculateCostUnitsTotal(cost);
    MonetaryAmount totalMonetaryCurrEncumbranceInitialAmount = Money.of(totalCurrEncumbranceInitialAmount, cost.getCurrency());
    return totalMonetaryCurrEncumbranceInitialAmount.subtract(costUnitsTotal).with(Monetary.getDefaultRounding());
  }

  protected BigDecimal calculateTotalInitialAmountEncumbered(PoLineEncumbrancesHolder holder) {
    BigDecimal totalAmountBeforeConversion = holder.getEncumbrances().stream()
      .map(Transaction::getEncumbrance)
      .map(Encumbrance::getInitialAmountEncumbered)
      .map(BigDecimal::valueOf).reduce(BigDecimal.ZERO, BigDecimal::add);
    Cost cost = holder.getPoLine().getCost();
    logger.info("calculateTotalInitialAmountEncumbered:: initiating currency conversion: {} -> {}", holder.getSystemCurrency(), cost.getCurrency());
    logger.info("calculateTotalInitialAmountEncumbered:: totalAmountBeforeConversion: {}", totalAmountBeforeConversion.doubleValue());
    Number totalAmountAfterConversion = amountWithConversion(totalAmountBeforeConversion, holder).getNumber();
    logger.info("calculateTotalInitialAmountEncumbered:: totalAmountAfterConversion: {}", totalAmountAfterConversion.doubleValue());
    return BigDecimal.valueOf(totalAmountAfterConversion.doubleValue());
  }

  protected CurrencyConversion retrieveCurrencyConversion(ExchangeRate exchangeRate, boolean isCustomExchangeRate) {
    var query = buildConversionQuery(exchangeRate.getFrom(), exchangeRate.getTo(), exchangeRate.getExchangeRate());
    var operationMode = isCustomExchangeRate ? DIVIDE : exchangeRate.getOperationMode();
    logger.info("retrieveCurrencyConversion:: Using operationMode: {}", operationMode);
    var provider = new CustomExchangeRateProvider(operationMode);
    return provider.getCurrencyConversion(query);
  }

  private MonetaryAmount amountWithConversion(BigDecimal totalInitialAmountEncumbered, PoLineEncumbrancesHolder holder) {
    return Money.of(totalInitialAmountEncumbered, holder.getSystemCurrency())
      .with(holder.getCurrencyConversion())
      .with(Monetary.getDefaultRounding());
  }

  private List<PoLineEncumbrancesHolder> buildPoLineEncumbrancesHolders(List<PoLine> poLines,
                                                                        Pair<List<Transaction>, List<RolloverConversionHolder>> encumbrancePoLineExchangeRates) {
    var poLineEncumbrancesHolders = new ArrayList<PoLineEncumbrancesHolder>();
    var encumbrances = encumbrancePoLineExchangeRates.getLeft();
    var poLineExchangeRates = encumbrancePoLineExchangeRates.getRight();
    poLines.forEach(poLine -> {
      var rolloverConversion = getRolloverConversionByPoLineId(poLine, poLineExchangeRates);
      var exchangeRate = rolloverConversion.getExchangeRate();
      var conversion = retrieveCurrencyConversion(exchangeRate, rolloverConversion.isCustomExchangeRate());
      var holder = new PoLineEncumbrancesHolder(poLine)
        .withCurrencyConversion(conversion)
        .withSystemCurrency(exchangeRate.getFrom());
      extractPoLineEncumbrances(poLine, encumbrances).forEach(encumbrance -> {
        holder.addEncumbrance(encumbrance);
        poLineEncumbrancesHolders.add(holder);
      });
    });
    return poLineEncumbrancesHolders;
  }

  private RolloverConversionHolder getRolloverConversionByPoLineId(PoLine poLine, List<RolloverConversionHolder> poLineExchangeRates) {
    return poLineExchangeRates.stream()
      .filter(rc -> StringUtils.equals(rc.getPoLineId(), poLine.getId()))
      .findFirst().orElseThrow(() -> new NoSuchElementException(String.format("Cannot find rollover conversion for poLineId: %s", poLine.getId())));
  }

  private List<Transaction> extractPoLineEncumbrances(PoLine poLine, List<Transaction> encumbrances) {
    return encumbrances.stream()
      .filter(encumbrance -> poLine.getId().equals(encumbrance.getEncumbrance().getSourcePoLineId()))
      .toList();
  }

  private Future<List<Transaction>> getEncumbrancesForRollover(List<String> poLineIds, LedgerFiscalYearRollover ledgerFYRollover, RequestContext requestContext) {
    var futures = ofSubLists(new ArrayList<>(poLineIds), MAX_IDS_FOR_GET_RQ_15)
      .map(ids -> buildQueryEncumbrancesForRollover(ids, ledgerFYRollover))
      .map(query -> transactionService.getTransactions(query, requestContext))
      .toList();
    return collectResultsOnSuccess(futures)
      .map(lists -> lists.stream()
        .flatMap(Collection::stream)
        .toList());
  }

  private Future<List<RolloverConversionHolder>> getExchangeRatesPerPoLine(List<PoLine> poLines, String fromCurrency, RequestContext requestContext) {
    var poLineExchangeRateFutures = new ArrayList<Future<RolloverConversionHolder>>();
    poLines.forEach(poLine -> {
      var cost = poLine.getCost();
      var isCustomExchangeRate = Objects.nonNull(cost.getExchangeRate());
      poLineExchangeRateFutures.add(cacheableExchangeRateService.getExchangeRate(fromCurrency, cost.getCurrency(), cost.getExchangeRate(), requestContext)
        .compose(exchangeRate -> Future.succeededFuture(new RolloverConversionHolder()
          .withPoLineId(poLine.getId()).withExchangeRate(exchangeRate).withCustomExchangeRate(isCustomExchangeRate))));
    });
    return collectResultsOnSuccess(poLineExchangeRateFutures);
  }

  private String buildQueryEncumbrancesForRollover(List<String> poLineIds, LedgerFiscalYearRollover ledgerFYRollover) {
    String fiscalYearIdsQuery = buildQuery(List.of(ledgerFYRollover.getToFiscalYearId()), ENCUMBR_FY_QUERY);
    String orderLineIdsQuery = buildQuery(poLineIds, ENCUMBRANCE_BY_POLINE_ID_QUERY);
    return "(" + fiscalYearIdsQuery + ")" + AND + "(" + orderLineIdsQuery + ")";
  }

  private String buildQuery(List<String> orderIds, String queryTemplate) {
    return orderIds.stream()
      .map(orderId -> String.format(queryTemplate, orderId))
      .collect(Collectors.joining(OrderRolloverService.OR));
  }

  protected String buildOpenOrClosedOrderQueryByFundIdsAndTypes(List<String> fundIds, boolean openOrders,
                                                                LedgerFiscalYearRollover ledgerFYRollover) {
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
    resultQuery.append(" sortBy metadata.createdDate");
    logger.info("buildOpenOrClosedOrderQueryByFundIdsAndTypes:: Resulting PO line query: {}", resultQuery);
    return resultQuery.toString();
  }

  private String buildFundIdQuery(List<String> fundIds) {
    return fundIds.stream()
      .map(fundId -> String.format(PO_LINE_FUND_DISTR_QUERY, fundId))
      .collect(Collectors.joining(OR));
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
    String statusQuery = "purchaseOrder.workflowStatus";
    if (openOrders) {
      statusQuery += "==";
    } else {
      statusQuery += "<>";
    }
    statusQuery += "Open";
    return statusQuery;
  }
}
