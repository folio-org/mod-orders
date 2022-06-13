package org.folio.service.orders;

import static java.util.Collections.emptyList;
import static java.util.concurrent.CompletableFuture.completedFuture;
import static java.util.stream.Collectors.groupingBy;
import static java.util.stream.Collectors.toList;
import static one.util.streamex.StreamEx.ofSubLists;
import static org.folio.orders.utils.HelperUtils.calculateCostUnitsTotal;
import static org.folio.orders.utils.HelperUtils.collectResultsOnSuccess;
import static org.folio.rest.RestConstants.MAX_IDS_FOR_GET_RQ;
import static org.folio.rest.jaxrs.model.PurchaseOrder.WorkflowStatus.CLOSED;
import static org.folio.rest.jaxrs.model.PurchaseOrder.WorkflowStatus.OPEN;

import java.math.BigDecimal;
import java.util.ArrayList;
import java.util.Collection;
import java.util.List;
import java.util.Map;
import java.util.concurrent.CompletableFuture;
import java.util.concurrent.CompletionException;
import java.util.concurrent.CompletionStage;
import java.util.stream.Collectors;
import java.util.stream.Stream;

import javax.money.Monetary;
import javax.money.MonetaryAmount;
import javax.money.convert.ConversionQuery;
import javax.money.convert.CurrencyConversion;
import javax.money.convert.ExchangeRateProvider;

import org.apache.commons.collections4.MapUtils;
import org.apache.commons.lang3.tuple.Pair;
import org.apache.logging.log4j.LogManager;
import org.apache.logging.log4j.Logger;
import org.folio.models.PoLineEncumbrancesHolder;
import org.folio.rest.core.exceptions.HttpException;
import org.folio.rest.core.exceptions.ErrorCodes;
import org.folio.orders.utils.HelperUtils;
import org.folio.rest.acq.model.finance.Encumbrance;
import org.folio.rest.acq.model.finance.Fund;
import org.folio.rest.acq.model.finance.Transaction;
import org.folio.rest.core.models.RequestContext;
import org.folio.rest.jaxrs.model.Cost;
import org.folio.rest.jaxrs.model.EncumbranceRollover;
import org.folio.rest.jaxrs.model.FundDistribution;
import org.folio.rest.jaxrs.model.FundDistribution.DistributionType;
import org.folio.rest.jaxrs.model.LedgerFiscalYearRollover;
import org.folio.rest.jaxrs.model.PoLine;
import org.folio.rest.jaxrs.model.PurchaseOrder;
import org.folio.rest.jaxrs.model.PurchaseOrderCollection;
import org.folio.service.configuration.ConfigurationEntriesService;
import org.folio.service.exchange.ExchangeRateProviderResolver;
import org.folio.service.finance.FundService;
import org.folio.service.finance.transaction.TransactionService;
import org.javamoney.moneta.Money;

public class OrderRolloverService {
  private static final Logger logger = LogManager.getLogger();

  private static final String PO_LINE_FUND_DISTR_QUERY = "poLine.fundDistribution == \"*\\\"fundId\\\":*\\\"%s\\\"*\"";
  private static final String ORDER_TYPE_QUERY = "orderType == %s";
  private static final String ORDER_LINE_BY_ORDER_ID_QUERY = "(purchaseOrderId == %s)";
  private static final String ENCUMBR_FY_QUERY = "fiscalYearId == %s";
  private static final String ENCUMBR_BY_ORDER_ID_QUERY = "encumbrance.sourcePurchaseOrderId == %s";

  public static final String WORKFLOW_STATUS_OPEN_OR_CLOSED_QUERY = " (workflowStatus==Open or workflowStatus==Closed) ";
  private static final String OR = " or ";
  private static final String AND = " and ";
  private static final int ORDERS_CHUNK = 200;

  private final FundService fundService;
  private final PurchaseOrderStorageService purchaseOrderStorageService;
  private final PurchaseOrderLineService purchaseOrderLineService;
  private final TransactionService transactionService;
  private final ConfigurationEntriesService configurationService;
  private final ExchangeRateProviderResolver exchangeRateProviderResolver;

  public OrderRolloverService(FundService fundService, PurchaseOrderStorageService purchaseOrderStorageService,
                              PurchaseOrderLineService purchaseOrderLineService, TransactionService transactionService,
                              ConfigurationEntriesService configurationEntriesService, ExchangeRateProviderResolver exchangeRateProviderResolver) {
    this.fundService = fundService;
    this.purchaseOrderStorageService = purchaseOrderStorageService;
    this.purchaseOrderLineService = purchaseOrderLineService;
    this.transactionService = transactionService;
    this.configurationService = configurationEntriesService;
    this.exchangeRateProviderResolver = exchangeRateProviderResolver;
  }

  public CompletableFuture<Void> rollover(LedgerFiscalYearRollover ledgerFYRollover, RequestContext requestContext) {
    return fundService.getFundsByLedgerId(ledgerFYRollover.getLedgerId(), requestContext)
      .thenApply(ledgerFunds -> ledgerFunds.stream().map(Fund::getId).collect(toList()))
      .thenCompose(ledgerFundIds -> getFundsOrders(ledgerFundIds, ledgerFYRollover, requestContext))
      .thenCombine(configurationService.getSystemCurrency(requestContext),
                      (orders, systemCurrency) -> Pair.of(systemCurrency, orders))
      .thenCompose(pair -> rolloverOrderLinesByChunks(pair.getKey(), pair.getValue(), ledgerFYRollover, requestContext))
      .thenCompose(poLines -> purchaseOrderLineService.saveOrderLines(poLines, requestContext))
      .thenAccept(v -> logger.debug("Order Rollover : All order processed"));
  }

  private CompletableFuture<List<PoLine>> rolloverOrderLinesByChunks(String systemCurrency, List<PurchaseOrder> orders,
                                                                     LedgerFiscalYearRollover ledgerFYRollover, RequestContext requestContext) {
    logger.debug("Start : All order processed");
    List<CompletableFuture<List<PoLine>>> futures = new ArrayList<>();
    ofSubLists(orders, MAX_IDS_FOR_GET_RQ).forEach(chunkOrders ->
      futures.add(rolloverPoLinesChunk(systemCurrency, chunkOrders, ledgerFYRollover, requestContext))
    );
    return collectResultsOnSuccess(futures).thenApply(results -> results.stream()
      .flatMap(List::stream)
      .collect(Collectors.toList())
    );
  }

  private CompletableFuture<List<PurchaseOrder>> getFundsOrders(List<String> ledgerFundIds,
      LedgerFiscalYearRollover ledgerFYRollover, RequestContext requestContext) {

    List<CompletableFuture<List<PurchaseOrder>>> futures = new ArrayList<>();
    ofSubLists(ledgerFundIds, MAX_IDS_FOR_GET_RQ).forEach(chunkFundIds ->
      futures.add(getFundsOrdersByChunk(chunkFundIds, ledgerFYRollover, requestContext))
    );
    return collectResultsOnSuccess(futures).thenApply(results -> results.stream()
      .flatMap(Collection::stream)
      .collect(toList())
    );
  }

  private CompletableFuture<List<PurchaseOrder>> getFundsOrdersByChunk(List<String> chunkFundIds,
      LedgerFiscalYearRollover ledgerFYRollover, RequestContext requestContext) {

    String query = buildOpenOrClosedOrderQueryByFundIdsAndTypes(chunkFundIds, ledgerFYRollover);
    return purchaseOrderStorageService.getPurchaseOrders(query, 0, 0, requestContext)
      .thenApply(PurchaseOrderCollection::getTotalRecords)
      .thenCompose(orderTotalRecords -> getOrdersByChunks(requestContext, query, orderTotalRecords))
      .exceptionally(t -> {
        logger.error(ErrorCodes.RETRIEVE_ROLLOVER_ORDER_ERROR.getDescription());
        throw new CompletionException(new HttpException(500, ErrorCodes.RETRIEVE_ROLLOVER_ORDER_ERROR));
      });
  }

  private CompletionStage<List<PurchaseOrder>> getOrdersByChunks(RequestContext requestContext, String query,
      Integer orderTotalRecords) {

    List<CompletableFuture<List<PurchaseOrder>>> futures = new ArrayList<>();
    int numberOfChuncks = orderTotalRecords/ORDERS_CHUNK + 1;
    for (int chunkNumber = 0; chunkNumber < numberOfChuncks; chunkNumber++)  {
      futures.add(purchaseOrderStorageService.getPurchaseOrders(query, ORDERS_CHUNK, chunkNumber * ORDERS_CHUNK, requestContext)
        .thenApply(PurchaseOrderCollection::getPurchaseOrders));
      logger.debug("Order chunk query : {}", query);
    }
    return collectResultsOnSuccess(futures).thenApply(results -> results.stream()
      .flatMap(Collection::stream)
      .collect(toList())
    );
  }

  private CompletableFuture<List<PoLine>> rolloverPoLinesChunk(String systemCurrency, List<PurchaseOrder> orders,
      LedgerFiscalYearRollover ledgerFYRollover, RequestContext requestContext) {

    List<String> openOrderIds = orders.stream()
      .filter(po -> po.getWorkflowStatus() == OPEN)
      .map(PurchaseOrder::getId)
      .collect(Collectors.toList());
    List<String> closedOrderIds = orders.stream()
      .filter(po -> po.getWorkflowStatus() == CLOSED)
      .map(PurchaseOrder::getId)
      .collect(Collectors.toList());
    return rolloverOpenOrders(systemCurrency, openOrderIds, ledgerFYRollover, requestContext)
      .thenCompose(openPoLines -> rolloverClosedOrders(closedOrderIds, ledgerFYRollover, requestContext)
        .thenApply(closedPoLines -> Stream.concat(openPoLines.stream(), closedPoLines.stream())
          .collect(Collectors.toList())))
      .exceptionally(t -> {
        logger.error(ErrorCodes.ROLLOVER_PO_LINES_ERROR.getDescription());
        throw new CompletionException(new HttpException(500, ErrorCodes.ROLLOVER_PO_LINES_ERROR));
      });
  }

  private CompletableFuture<List<PoLine>> rolloverOpenOrders(String systemCurrency, List<String> orderIds,
      LedgerFiscalYearRollover ledgerFYRollover, RequestContext requestContext) {

    if (orderIds.size() == 0)
      return completedFuture(emptyList());
    return getPoLinesByOrderIds(orderIds, requestContext)
      .thenCompose(poLines -> getEncumbrancesForRollover(orderIds, ledgerFYRollover, requestContext)
        .thenApply(transactions -> buildPoLineEncumbrancesHolders(systemCurrency, poLines, transactions, requestContext))
        .thenApply(this::applyPoLinesRolloverChanges));
  }

  private CompletableFuture<List<PoLine>> rolloverClosedOrders(List<String> orderIds,
     LedgerFiscalYearRollover ledgerFYRollover, RequestContext requestContext) {

    if (orderIds.size() == 0)
      return completedFuture(emptyList());
    return getPoLinesByOrderIds(orderIds, requestContext)
      .thenCompose(poLines -> getEncumbrancesForRollover(orderIds, ledgerFYRollover, requestContext)
        .thenCompose(transactions -> {
          if (transactions.size() == 0)
            return completedFuture(null);
          return transactionService.deleteTransactions(transactions, requestContext);
        })
        .thenApply(v -> removeEncumbranceLinks(poLines)));
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
        MonetaryAmount newFdAmount = fdAmount.divide(poLineEstimatedPriceBeforeRollover).multiply(poLineEstimatedPriceAfterRollover);
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
    BigDecimal totalAmountBeforeConversion = holder.getEncumbrances().stream()
                       .map(Transaction::getEncumbrance)
                       .map(Encumbrance::getInitialAmountEncumbered)
                       .map(BigDecimal::valueOf).reduce(BigDecimal.ZERO, BigDecimal::add);
    Number totalAmountAfterConversion = amountWithConversion(totalAmountBeforeConversion, holder).getNumber();
    return BigDecimal.valueOf(totalAmountAfterConversion.doubleValue());
  }

  private CurrencyConversion retrieveCurrencyConversion(String systemCurrency, PoLine poLine, RequestContext requestContext) {
    ConversionQuery conversionQuery = HelperUtils.buildConversionQuery(poLine, systemCurrency);
    ExchangeRateProvider exchangeRateProvider = exchangeRateProviderResolver.resolve(conversionQuery, requestContext);
    return exchangeRateProvider.getCurrencyConversion(conversionQuery);
  }

  private MonetaryAmount amountWithConversion(BigDecimal totalInitialAmountEncumbered, PoLineEncumbrancesHolder holder) {
    return Money.of(totalInitialAmountEncumbered, holder.getPoLine().getCost().getCurrency()).with(holder.getCurrencyConversion());
  }

  private List<PoLineEncumbrancesHolder> buildPoLineEncumbrancesHolders(String  systemCurrency, List<PoLine> poLines,
                                                                        List<Transaction> encumbrances, RequestContext requestContext) {
    List<PoLineEncumbrancesHolder> poLineEncumbrancesHolders = new ArrayList<>();
    poLines.forEach(poLine -> {
      CurrencyConversion currencyConversion = retrieveCurrencyConversion(systemCurrency, poLine, requestContext);
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

  private CompletableFuture<List<Transaction>> getEncumbrancesForRollover(List<String> orderIds, LedgerFiscalYearRollover ledgerFYRollover,
                                                                          RequestContext requestContext) {
    String query = buildQueryEncumbrancesForRollover(orderIds, ledgerFYRollover);
    return transactionService.getTransactions(query, requestContext);
  }

  private String buildQueryEncumbrancesForRollover(List<String> orderIds, LedgerFiscalYearRollover ledgerFYRollover) {
    String fiscalYearIdsQuery = buildQuery(List.of(ledgerFYRollover.getToFiscalYearId()), ENCUMBR_FY_QUERY, OR);
    String orderIdsQuery = buildQuery(orderIds, ENCUMBR_BY_ORDER_ID_QUERY, OR);
    return "(" + fiscalYearIdsQuery + ")" + AND + "(" + orderIdsQuery + ")";
  }

  private CompletableFuture<List<PoLine>> getPoLinesByOrderIds(List<String> orderIds, RequestContext requestContext) {
    String query = buildQuery(orderIds, ORDER_LINE_BY_ORDER_ID_QUERY, OR);
    return purchaseOrderLineService.getOrderLines(query, 0, Integer.MAX_VALUE, requestContext);
  }

  private String buildQuery(List<String> orderIds, String queryTemplate, String delimiter) {
    return orderIds.stream()
            .map(orderId -> String.format(queryTemplate, orderId))
                   .collect(Collectors.joining(delimiter));
  }

  private String buildOpenOrClosedOrderQueryByFundIdsAndTypes(List<String> fundIds, LedgerFiscalYearRollover ledgerFYRollover) {
    String typesQuery = buildOrderTypesQuery(ledgerFYRollover);
    String fundIdsQuery = fundIds.stream().map(fundId -> String.format(PO_LINE_FUND_DISTR_QUERY, fundId)).collect(Collectors.joining(OR));
    return "(" + typesQuery + ")" +  AND + WORKFLOW_STATUS_OPEN_OR_CLOSED_QUERY + AND + "(" + fundIdsQuery + ")";
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
