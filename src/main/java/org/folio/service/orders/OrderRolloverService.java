package org.folio.service.orders;

import static java.util.stream.Collectors.groupingBy;
import static java.util.stream.Collectors.toList;
import static one.util.streamex.StreamEx.ofSubLists;
import static org.folio.orders.utils.HelperUtils.calculateCostUnitsTotal;
import static org.folio.orders.utils.HelperUtils.collectResultsOnSuccess;
import static org.folio.rest.RestConstants.MAX_IDS_FOR_GET_RQ;

import java.math.BigDecimal;
import java.util.ArrayList;
import java.util.Collections;
import java.util.List;
import java.util.Map;
import java.util.Set;
import java.util.concurrent.CompletableFuture;
import java.util.concurrent.CompletionException;
import java.util.concurrent.CompletionStage;
import java.util.stream.Collectors;

import javax.money.MonetaryAmount;

import org.apache.commons.collections4.MapUtils;
import org.apache.logging.log4j.LogManager;
import org.apache.logging.log4j.Logger;
import org.folio.models.PoLineEncumbrancesHolder;
import org.folio.orders.rest.exceptions.HttpException;
import org.folio.orders.utils.ErrorCodes;
import org.folio.rest.acq.model.finance.Encumbrance;
import org.folio.rest.acq.model.finance.Fund;
import org.folio.rest.acq.model.finance.Transaction;
import org.folio.rest.acq.model.finance.TransactionCollection;
import org.folio.rest.core.RestClient;
import org.folio.rest.core.models.RequestContext;
import org.folio.rest.jaxrs.model.Cost;
import org.folio.rest.jaxrs.model.EncumbranceRollover;
import org.folio.rest.jaxrs.model.LedgerFiscalYearRollover;
import org.folio.rest.jaxrs.model.PoLine;
import org.folio.rest.jaxrs.model.PurchaseOrder;
import org.folio.rest.jaxrs.model.PurchaseOrderCollection;
import org.folio.service.finance.FundService;
import org.folio.service.finance.TransactionService;
import org.javamoney.moneta.Money;

public class OrderRolloverService {
  private static final Logger logger = LogManager.getLogger();

  private static final String PO_LINE_FUND_DISTR_QUERY = "poLine.fundDistribution == \"*\\\"fundId\\\":*\\\"%s\\\"*\"";
  private static final String ORDER_TYPE_QUERY = "orderType == %s";
  private static final String ORDER_LINE_BY_ORDER_ID_QUERY = "(purchaseOrderId == %s)";
  private static final String ENCUMBR_FY_QUERY = "fiscalYearId == %s";
  private static final String ENCUMBR_BY_ORDER_ID_QUERY = "encumbrance.sourcePurchaseOrderId == %s";

  public static final String WORKFLOW_STATUS_OPEN_QUERY = " (workflowStatus==Open) ";
  private static final String OR = " or ";
  private static final String AND = " and ";
  private static final int ORDERS_CHUNK = 200;

  private final FundService fundService;
  private final PurchaseOrderService purchaseOrderService;
  private final PurchaseOrderLineService purchaseOrderLineService;
  private final TransactionService transactionService;

  public OrderRolloverService(FundService fundService, PurchaseOrderService purchaseOrderService,
                              PurchaseOrderLineService purchaseOrderLineService, TransactionService transactionService) {
    this.fundService = fundService;
    this.purchaseOrderService = purchaseOrderService;
    this.purchaseOrderLineService = purchaseOrderLineService;
    this.transactionService = transactionService;
  }

  public CompletableFuture<Void> rollover(LedgerFiscalYearRollover ledgerFYRollover, RequestContext requestContext) {
    return fundService.getFundsByLedgerId(ledgerFYRollover.getLedgerId(), requestContext)
                      .thenApply(ledgerFunds -> ledgerFunds.stream().map(Fund::getId).collect(toList()))
                      .thenCompose(ledgerFundIds -> getFundsOrderIds(ledgerFundIds, ledgerFYRollover, requestContext))
                      .thenCompose(orderIds -> rolloverOrderLinesByChunks(orderIds, ledgerFYRollover, requestContext))
                      .thenCompose(poLines -> purchaseOrderLineService.updateOrderLines(poLines, requestContext))
                      .thenAccept(v -> logger.debug("Order Rollover : All order processed"));
  }

  private CompletableFuture<List<PoLine>> rolloverOrderLinesByChunks(Set<String> orderIds, LedgerFiscalYearRollover ledgerFYRollover, RequestContext requestContext) {
    logger.debug("Start : All order processed");
    List<CompletableFuture<List<PoLine>>> futures = new ArrayList<>();
    ofSubLists(new ArrayList<>(orderIds), MAX_IDS_FOR_GET_RQ).forEach(chunkOrderIds ->
      futures.add(rolloverPoLinesChunk(chunkOrderIds, ledgerFYRollover, requestContext))
    );
    return collectResultsOnSuccess(futures).thenApply(results -> results.stream()
      .flatMap(List::stream)
      .collect(Collectors.toList())
    );
  }

  private CompletableFuture<Set<String>> getFundsOrderIds(List<String> ledgerFundIds, LedgerFiscalYearRollover ledgerFYRollover,
                                                   RequestContext requestContext) {
    List<CompletableFuture<Set<String>>> futures = new ArrayList<>();
    ofSubLists(ledgerFundIds, MAX_IDS_FOR_GET_RQ).forEach(chunkFundIds ->
      futures.add(getFundsOrderIdsByChunk(chunkFundIds, ledgerFYRollover, requestContext))
    );
    return collectResultsOnSuccess(futures).thenApply(results -> results.stream()
      .flatMap(Set::stream)
      .collect(Collectors.toSet())
    );
  }

  private CompletableFuture<Set<String>> getFundsOrderIdsByChunk(List<String> chunkFundIds, LedgerFiscalYearRollover ledgerFYRollover,
                                                                 RequestContext requestContext) {

    String query = buildOpenOrderQueryByFundIdsAndTypes(chunkFundIds, ledgerFYRollover);
    return purchaseOrderService.getPurchaseOrders(query, 0, 0, requestContext)
              .thenApply(PurchaseOrderCollection::getTotalRecords)
              .thenCompose(orderTotalRecords -> getOrderIdsByChunks(requestContext, query, orderTotalRecords))
              .exceptionally(t -> {
                logger.error(ErrorCodes.RETRIEVE_ROLLOVER_ORDER_ERROR.getDescription());
                throw new CompletionException(new HttpException(500, ErrorCodes.RETRIEVE_ROLLOVER_ORDER_ERROR));
              });
  }

  private CompletionStage<Set<String>> getOrderIdsByChunks(RequestContext requestContext, String query, Integer orderTotalRecords) {
    List<CompletableFuture<Set<String>>> futures = new ArrayList<>();
    int numberOfChuncks = orderTotalRecords/ORDERS_CHUNK + 1;
    for (int chunkNumber = 0; chunkNumber < numberOfChuncks; chunkNumber++)  {
      futures.add(purchaseOrderService.getPurchaseOrders(query, ORDERS_CHUNK, chunkNumber * ORDERS_CHUNK, requestContext)
        .thenApply(this::extractOrderIds)
      );
      logger.debug("Order chunk query : {}", query);
    }
    return collectResultsOnSuccess(futures).thenApply(results -> results.stream()
      .flatMap(Set::stream)
      .collect(Collectors.toSet())
    );
  }

  private CompletableFuture<List<PoLine>> rolloverPoLinesChunk(List<String> orderIds, LedgerFiscalYearRollover ledgerFYRollover,
                                                                                 RequestContext requestContext) {
    CompletableFuture<List<PoLine>> poLinesFuture = getPoLinesByOrderIds(orderIds, requestContext);
    CompletableFuture<List<Transaction>> encumbrancesFuture = getEncumbrancesForRollover(orderIds, ledgerFYRollover, requestContext);

    return CompletableFuture.allOf(poLinesFuture, encumbrancesFuture)
      .thenApply(v -> poLinesFuture.join())
      .thenApply(poLines -> buildPoLineEncumbrancesHolders(poLines, encumbrancesFuture.join()))
      .thenApply(this::applyPoLinesRolloverChanges)
      .exceptionally(t -> {
        logger.error(ErrorCodes.ROLLOVER_PO_LINES_ERROR.getDescription());
        throw new CompletionException(new HttpException(500, ErrorCodes.ROLLOVER_PO_LINES_ERROR));
      });
  }

  private List<PoLine> applyPoLinesRolloverChanges(List<PoLineEncumbrancesHolder> poLineEncumbrancesHolders) {
    poLineEncumbrancesHolders.forEach(holder -> {
      PoLine poLine = holder.getPoLine();
      var currEncumbranceFundIdMap = holder.getEncumbrances().stream().collect(groupingBy(Transaction::getFromFundId));
      if (!MapUtils.isEmpty(currEncumbranceFundIdMap)) {
        updatePoLineCost(holder, poLine);
        updateFundDistribution(poLine, currEncumbranceFundIdMap);
      }
    });
    return poLineEncumbrancesHolders.stream().map(PoLineEncumbrancesHolder::getPoLine).collect(toList());
  }

  private void updatePoLineCost(PoLineEncumbrancesHolder holder, PoLine poLine) {
    BigDecimal totalCurrEncumbranceInitialAmount = calculateTotalInitialAmountEncumbered(holder.getEncumbrances());
    Double fyroAdjustmentAmount = calculateFYROAdjustmentAmount(holder, totalCurrEncumbranceInitialAmount).getNumber().doubleValue();
    poLine.getCost().setFyroAdjustmentAmount(fyroAdjustmentAmount);
    poLine.getCost().setPoLineEstimatedPrice(totalCurrEncumbranceInitialAmount.doubleValue());
  }

  private void updateFundDistribution(PoLine poLine, Map<String, List<Transaction>> currEncumbranceFundIdMap) {
    poLine.getFundDistribution().forEach(fundDistr -> {
      var currEncumbrances = currEncumbranceFundIdMap.getOrDefault(fundDistr.getFundId(), Collections.emptyList());
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
    return totalMonetaryCurrEncumbranceInitialAmount.subtract(costUnitsTotal);
  }

  private BigDecimal calculateTotalInitialAmountEncumbered(List<Transaction> encumbrances) {
    return encumbrances.stream()
                       .map(Transaction::getEncumbrance)
                       .map(Encumbrance::getInitialAmountEncumbered)
                       .map(BigDecimal::valueOf).reduce(BigDecimal.ZERO, BigDecimal::add);
  }

  private List<PoLineEncumbrancesHolder> buildPoLineEncumbrancesHolders(List<PoLine> poLines, List<Transaction> encumbrances) {
    List<PoLineEncumbrancesHolder> poLineEncumbrancesHolders = new ArrayList<>();
    poLines.forEach(poLine -> {
      PoLineEncumbrancesHolder holder = new PoLineEncumbrancesHolder(poLine);
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
    return transactionService.getTransactions(query, 0, Integer.MAX_VALUE, requestContext)
                                .thenApply(TransactionCollection::getTransactions);
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

  private Set<String> extractOrderIds(PurchaseOrderCollection orderCollection) {
    return orderCollection.getPurchaseOrders().stream().map(PurchaseOrder::getId).collect(Collectors.toSet());
  }

  private String buildOpenOrderQueryByFundIdsAndTypes(List<String> fundIds, LedgerFiscalYearRollover ledgerFYRollover) {
    String typesQuery = buildOrderTypesQuery(ledgerFYRollover);
    String fundIdsQuery = fundIds.stream().map(fundId -> String.format(PO_LINE_FUND_DISTR_QUERY, fundId)).collect(Collectors.joining(OR));
    return "(" + typesQuery + ")" +  AND + WORKFLOW_STATUS_OPEN_QUERY + AND + "(" + fundIdsQuery + ")";
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
