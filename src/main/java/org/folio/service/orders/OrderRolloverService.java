package org.folio.service.orders;

import static java.util.Collections.emptyList;
import static java.util.stream.Collectors.groupingBy;
import static java.util.stream.Collectors.toList;
import static one.util.streamex.StreamEx.ofSubLists;
import static org.folio.orders.utils.HelperUtils.calculateCostUnitsTotal;
import static org.folio.orders.utils.HelperUtils.collectResultsOnSuccess;
import static org.folio.orders.utils.HelperUtils.getCurrencyFromTransactionByPoLineId;
import static org.folio.rest.RestConstants.MAX_IDS_FOR_GET_RQ_15;
import static org.folio.rest.RestVerticle.OKAPI_HEADER_TENANT;
import static org.folio.rest.acq.model.finance.LedgerFiscalYearRolloverError.ErrorType.ORDER_ROLLOVER;
import static org.folio.rest.jaxrs.model.PurchaseOrder.WorkflowStatus.CLOSED;
import static org.folio.rest.jaxrs.model.PurchaseOrder.WorkflowStatus.OPEN;
import static org.folio.rest.jaxrs.model.RolloverStatus.ERROR;
import static org.folio.rest.jaxrs.model.RolloverStatus.SUCCESS;

import java.math.BigDecimal;
import java.util.ArrayList;
import java.util.Collection;
import java.util.List;
import java.util.Map;
import java.util.UUID;
import java.util.concurrent.atomic.AtomicInteger;
import java.util.stream.Collectors;

import javax.money.Monetary;
import javax.money.MonetaryAmount;
import javax.money.convert.ConversionQuery;
import javax.money.convert.CurrencyConversion;
import javax.money.convert.ExchangeRateProvider;

import io.vertx.core.AsyncResult;
import org.apache.commons.collections4.CollectionUtils;
import org.apache.commons.collections4.MapUtils;
import org.apache.logging.log4j.LogManager;
import org.apache.logging.log4j.Logger;
import org.folio.dao.FailedLedgerRolloverPoLineDao;
import org.folio.models.FailedLedgerRolloverPoLineDto;
import org.folio.models.PoLineEncumbrancesHolder;
import org.folio.okapi.common.GenericCompositeFuture;
import org.folio.orders.utils.HelperUtils;
import org.folio.rest.acq.model.finance.Encumbrance;
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
import org.folio.service.caches.ConfigurationEntriesCache;
import org.folio.service.exchange.ExchangeRateProviderResolver;
import org.folio.service.finance.FundService;
import org.folio.service.finance.rollover.LedgerRolloverErrorService;
import org.folio.service.finance.rollover.LedgerRolloverProgressService;
import org.folio.service.finance.transaction.TransactionService;
import org.javamoney.moneta.Money;

import com.google.common.collect.Lists;

import io.vertx.core.Future;
import io.vertx.core.json.JsonObject;
import io.vertxconcurrent.Semaphore;

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
  private final ConfigurationEntriesCache configurationEntriesCache;
  private final ExchangeRateProviderResolver exchangeRateProviderResolver;
  private final LedgerRolloverProgressService ledgerRolloverProgressService;
  private final LedgerRolloverErrorService ledgerRolloverErrorService;
  private final FailedLedgerRolloverPoLineDao failedLedgerRolloverPoLineDao;

  public OrderRolloverService(FundService fundService, PurchaseOrderLineService purchaseOrderLineService, TransactionService transactionService,
    ConfigurationEntriesCache configurationEntriesCache, ExchangeRateProviderResolver exchangeRateProviderResolver,
    LedgerRolloverProgressService ledgerRolloverProgressService, LedgerRolloverErrorService ledgerRolloverErrorService,
    FailedLedgerRolloverPoLineDao failedLedgerRolloverPoLineDao) {
    this.fundService = fundService;
    this.purchaseOrderLineService = purchaseOrderLineService;
    this.transactionService = transactionService;
    this.configurationEntriesCache = configurationEntriesCache;
    this.exchangeRateProviderResolver = exchangeRateProviderResolver;
    this.ledgerRolloverProgressService = ledgerRolloverProgressService;
    this.ledgerRolloverErrorService = ledgerRolloverErrorService;
    this.failedLedgerRolloverPoLineDao = failedLedgerRolloverPoLineDao;
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
      .map(ledgerFunds -> ledgerFunds.stream()
        .map(Fund::getId)
        .toList());

    return fundIdsFuture
      .compose(ledgerFundIds -> configurationEntriesCache.getSystemCurrency(requestContext)
        .compose(systemCurrency -> rolloverOrdersByFundIds(ledgerFundIds, ledgerFYRollover, systemCurrency, requestContext)))
      .recover(t -> handleOrderRolloverError(t, ledgerFYRollover, progress, requestContext))
      .compose(aVoid -> calculateAndUpdateOverallProgressStatus(progress.withOrdersRolloverStatus(SUCCESS), requestContext))
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
    if (CollectionUtils.isEmpty(ledgerFundIds)) return Future.succeededFuture();

    var fundIdChunks = Lists.partition(ledgerFundIds, MAX_IDS_FOR_GET_RQ_15);

    return requestContext.getContext().<List<Future<Void>>>executeBlocking(promise -> {
        List<Future<Void>> futures = new ArrayList<>();
        // process fundId chunks one by one
        Semaphore semaphore = new Semaphore(1, requestContext.getContext().owner());
        for (List<String> fundIds : fundIdChunks) {
          semaphore.acquire(() -> {
            // perform rollover for open orders and then for closed orders
            var future = rolloverOrdersByFundIds(fundIds, ledgerFYRollover, systemCurrency, OPEN, requestContext)
              .andThen(openOrdersResult -> rolloverOrdersByFundIds(fundIds, ledgerFYRollover, systemCurrency, CLOSED, requestContext)
              .andThen(closedOrdersResult -> throwExceptionIfOneOfFailed(openOrdersResult, closedOrdersResult)))
              .onComplete(asyncResult -> semaphore.release());

            futures.add(future);
            if (futures.size() == fundIdChunks.size()) {
              promise.complete(futures);
            }
          });
        }
      })
      .compose(GenericCompositeFuture::join)
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
      String systemCurrency, PurchaseOrder.WorkflowStatus workflowStatus, RequestContext requestContext) {

    String query = buildOpenOrClosedOrderQueryByFundIdsAndTypes(chunkFundIds, workflowStatus, ledgerFYRollover);
    var totalRecordsFuture = purchaseOrderLineService.getOrderLineCollection(query, 0, 0, requestContext)
      .map(PoLineCollection::getTotalRecords);

    var ctx = requestContext.getContext();
    return totalRecordsFuture.compose(totalRecords -> ctx.<List<Future<Void>>>executeBlocking(promise -> {
        List<Future<Void>> futures = new ArrayList<>();
        if (totalRecords == 0) {
          promise.complete(emptyList());
          return;
        }

        int numberOfChunks = (int) Math.ceil((double) totalRecords / POLINES_CHUNK_SIZE_200);
        // only 1 active thread because of chunk size = 200 records
        Semaphore semaphore = new Semaphore(1, ctx.owner());

        AtomicInteger atomicChunkCounter = new AtomicInteger();
        for (int chunkNumber = 0; chunkNumber < numberOfChunks; chunkNumber++) {
          // can produce "thread blocked" warnings because of large number of data
          semaphore.acquire(() -> {
            Future<Void> future = purchaseOrderLineService.getOrderLines(query, atomicChunkCounter.getAndIncrement() * POLINES_CHUNK_SIZE_200, POLINES_CHUNK_SIZE_200, requestContext)
              .compose(poLines -> rolloverOrders(systemCurrency, poLines, ledgerFYRollover, workflowStatus, requestContext))
              .compose(modifiedPoLines -> saveOrderLines(modifiedPoLines, ledgerFYRollover, workflowStatus, requestContext))
              .onComplete(asyncResult -> semaphore.release());

            futures.add(future);
            if (futures.size() == numberOfChunks) {
              promise.complete(futures);
            }
          });
        }
      }))
      .compose(GenericCompositeFuture::join)
      .mapEmpty();
  }

  private Future<Void> saveOrderLines(List<PoLine> orderLines, LedgerFiscalYearRollover ledgerFYRollover,
      PurchaseOrder.WorkflowStatus workflowStatus, RequestContext requestContext) {
    return purchaseOrderLineService.saveOrderLines(orderLines, requestContext)
      .recover(t -> handlePoLineUpdateFailures(orderLines, ledgerFYRollover, workflowStatus, t, requestContext)
        .map(a -> {
          throw new HttpException(400, ErrorCodes.PO_LINE_ROLLOVER_FAILED.toError());
        }));
  }

  private Future<Void> handlePoLineUpdateFailures(List<PoLine> poLines, LedgerFiscalYearRollover ledgerFYRollover, PurchaseOrder.WorkflowStatus workflowStatus,
                                                 Throwable t, RequestContext requestContext) {
    return GenericCompositeFuture.join(poLines.stream()
        .map(poLine -> handlePoLineUpdateFailure(poLine, ledgerFYRollover, workflowStatus, t, requestContext))
        .toList())
      .mapEmpty();
  }

  private Future<Void> handlePoLineUpdateFailure(PoLine poLine, LedgerFiscalYearRollover ledgerFYRollover, PurchaseOrder.WorkflowStatus workflowStatus,
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
      workflowStatus.value() // purchase order workflow status (Open / Closed)
    );

    return failedLedgerRolloverPoLineDao.saveFailedRolloverRecord(failureDto, requestContext.getHeaders().get(OKAPI_HEADER_TENANT));
  }

  private Future<List<PoLine>> rolloverOrders(String systemCurrency, List<PoLine> poLines, LedgerFiscalYearRollover ledgerFYRollover,
    PurchaseOrder.WorkflowStatus workflowStatus, RequestContext requestContext) {
    if (poLines.isEmpty())
      return Future.succeededFuture(emptyList());

    var poLineIds = poLines.stream()
      .map(PoLine::getId)
      .collect(toList());

    return getEncumbrancesForRollover(poLineIds, ledgerFYRollover, requestContext)
      .compose(encumbrances -> {
        if (OPEN.equals(workflowStatus)) {
          var holders = buildPoLineEncumbrancesHolders(systemCurrency, poLines, encumbrances, requestContext);
          var modifiedPoLines = applyPoLinesRolloverChanges(holders);
          return Future.succeededFuture(modifiedPoLines);

        } else {
          return removeEncumbrancesFromClosedPoLines(poLines, encumbrances, requestContext);
        }
      });
  }

  private Future<List<PoLine>> removeEncumbrancesFromClosedPoLines(List<PoLine> poLines, List<Transaction> transactions,
      RequestContext requestContext) {
    return Future.succeededFuture()
      .compose(v -> {
        if (transactions.isEmpty()) {
          return Future.succeededFuture();
        } else {
          List<String> idsOfTransactionsToDelete =  transactions.stream().map(Transaction::getId).toList();
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
    return poLineEncumbrancesHolders.stream().map(PoLineEncumbrancesHolder::getPoLine).collect(toList());
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
    poLine.getFundDistribution().forEach(fundDistr -> {
      if (fundDistr.getDistributionType().equals(DistributionType.AMOUNT)) {
        MonetaryAmount fdAmount = Money.of(fundDistr.getValue(), poLine.getCost().getCurrency());
        MonetaryAmount newFdAmount = (poLineEstimatedPriceBeforeRollover != 0) ? fdAmount.divide(poLineEstimatedPriceBeforeRollover).multiply(poLineEstimatedPriceAfterRollover) : Money.of(0, poLine.getCost().getCurrency());
        fundDistr.setValue(newFdAmount.getNumber().doubleValue());
      }
      var currEncumbrances = currEncumbranceFundIdMap.getOrDefault(fundDistr.getFundId(), emptyList());
      currEncumbrances.forEach(encumbr -> {
        if (encumbr.getExpenseClassId() != null && fundDistr.getExpenseClassId() != null) {
          if (encumbr.getExpenseClassId().equals(fundDistr.getExpenseClassId())) {
            fundDistr.setEncumbrance(encumbr.getId());
          }
        } else if (encumbr.getExpenseClassId() == null && fundDistr.getExpenseClassId() == null)
        {
          fundDistr.setEncumbrance(encumbr.getId());
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

  private BigDecimal calculateTotalInitialAmountEncumbered(PoLineEncumbrancesHolder holder) {
    logger.info("calculateTotalInitialAmountEncumbered:: holder: {}", JsonObject.mapFrom(holder).encodePrettily());
    BigDecimal totalAmountBeforeConversion = holder.getEncumbrances().stream()
                       .map(Transaction::getEncumbrance)
                       .map(Encumbrance::getInitialAmountEncumbered)
                       .map(BigDecimal::valueOf).reduce(BigDecimal.ZERO, BigDecimal::add);
    logger.info("calculateTotalInitialAmountEncumbered:: totalAmountBeforeConversion: {}", totalAmountBeforeConversion);
    Number totalAmountAfterConversion = amountWithConversion(totalAmountBeforeConversion, holder).getNumber();
    logger.info("calculateTotalInitialAmountEncumbered:: totalAmountAfterConversion: {}", totalAmountAfterConversion);
    return BigDecimal.valueOf(totalAmountAfterConversion.doubleValue());
  }

  private CurrencyConversion retrieveCurrencyConversion(String termCurrency, PoLine poLine, RequestContext requestContext) {
    ConversionQuery conversionQuery = HelperUtils.buildConversionQuery(poLine, termCurrency);
    ExchangeRateProvider exchangeRateProvider = exchangeRateProviderResolver.resolve(conversionQuery, requestContext);
    return exchangeRateProvider.getCurrencyConversion(conversionQuery);
  }

  private MonetaryAmount amountWithConversion(BigDecimal totalInitialAmountEncumbered, PoLineEncumbrancesHolder holder) {
    String encumbranceTransactionCurrency = getCurrencyFromTransactionByPoLineId(holder.getEncumbrances(), holder.getPoLine(), null);
    return Money.of(totalInitialAmountEncumbered, encumbranceTransactionCurrency)
      .with(holder.getCurrencyConversion())
      .with(Monetary.getDefaultRounding());
  }

  private List<PoLineEncumbrancesHolder> buildPoLineEncumbrancesHolders(String  systemCurrency, List<PoLine> poLines,
                                                                        List<Transaction> encumbrances, RequestContext requestContext) {
    List<PoLineEncumbrancesHolder> poLineEncumbrancesHolders = new ArrayList<>();
    poLines.forEach(poLine -> {
      String termCurrency = getCurrencyFromTransactionByPoLineId(encumbrances, poLine, systemCurrency);
      CurrencyConversion currencyConversion = retrieveCurrencyConversion(termCurrency, poLine, requestContext);
      PoLineEncumbrancesHolder holder = new PoLineEncumbrancesHolder(poLine).withCurrencyConversion(currencyConversion);
      extractPoLineEncumbrances(poLine, encumbrances).forEach(encumbrance -> {
        holder.addEncumbrance(encumbrance);
        poLineEncumbrancesHolders.add(holder);
      });
    });
    return poLineEncumbrancesHolders;
  }

  private List<Transaction> extractPoLineEncumbrances(PoLine poLine, List<Transaction> encumbrances) {
    return encumbrances.stream()
                .filter(encumbrance -> poLine.getId().equals(encumbrance.getEncumbrance().getSourcePoLineId()))
                .collect(toList());
  }

  private Future<List<Transaction>> getEncumbrancesForRollover(List<String> polineIds, LedgerFiscalYearRollover ledgerFYRollover, RequestContext requestContext) {
    var futures = ofSubLists(new ArrayList<>(polineIds), MAX_IDS_FOR_GET_RQ_15)
      .map(ids -> buildQueryEncumbrancesForRollover(ids, ledgerFYRollover))
      .map(query -> transactionService.getTransactions(query, requestContext))
      .toList();
    return collectResultsOnSuccess(futures)
      .map(lists -> lists.stream()
        .flatMap(Collection::stream)
        .collect(Collectors.toList()));
  }

  private String buildQueryEncumbrancesForRollover(List<String> polineIds, LedgerFiscalYearRollover ledgerFYRollover) {
    String fiscalYearIdsQuery = buildQuery(List.of(ledgerFYRollover.getToFiscalYearId()), ENCUMBR_FY_QUERY, OR);
    String orderLineIdsQuery = buildQuery(polineIds, ENCUMBRANCE_BY_POLINE_ID_QUERY, OR);
    return "(" + fiscalYearIdsQuery + ")" + AND + "(" + orderLineIdsQuery + ")";
  }

  private String buildQuery(List<String> orderIds, String queryTemplate, String delimiter) {
    return orderIds.stream()
            .map(orderId -> String.format(queryTemplate, orderId))
                   .collect(Collectors.joining(delimiter));
  }

  private String buildOpenOrClosedOrderQueryByFundIdsAndTypes(List<String> fundIds, PurchaseOrder.WorkflowStatus workflowStatus,
    LedgerFiscalYearRollover ledgerFYRollover) {
    String typesQuery = buildOrderTypesQuery(ledgerFYRollover);
    String fundIdsQuery = fundIds.stream().map(fundId -> String.format(PO_LINE_FUND_DISTR_QUERY, fundId)).collect(Collectors.joining(OR));
    String sortByCreatedDate = " sortBy metadata.createdDate";

    var resultQuery = "(" + typesQuery + ")" + AND + " (purchaseOrder.workflowStatus==" + workflowStatus + ") " + AND + "(" + fundIdsQuery + ")";

    if (workflowStatus == CLOSED) {
      // MODORDERS-904 Avoid rollover re-processing of old already processed closed orders in previous fiscal years
      String nonEmptyEncumbranceFilter = " AND ("  + PO_LINE_NON_EMPTY_ENCUMBRANCE_QUERY + ")";
      resultQuery = resultQuery + nonEmptyEncumbranceFilter;
    }

    resultQuery = resultQuery + sortByCreatedDate;
    logger.debug("buildOpenOrClosedOrderQueryByFundIdsAndTypes :: resulting PO line query: {}", resultQuery);

    return resultQuery;
  }

  private String buildOrderTypesQuery(LedgerFiscalYearRollover ledgerFYRollover) {
    return ledgerFYRollover.getEncumbrancesRollover().stream()
                                        .map(encumrRol -> convertToOrderType(encumrRol.getOrderType()))
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
}
