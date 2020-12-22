package org.folio.service.orders;

import org.apache.commons.collections4.CollectionUtils;
import org.apache.commons.collections4.MapUtils;
import org.folio.models.PoLineEncumbrancesHolder;
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
import org.folio.service.TransactionService;
import org.folio.service.finance.FundService;
import org.javamoney.moneta.Money;
import javax.money.MonetaryAmount;
import java.math.BigDecimal;
import java.util.ArrayList;
import java.util.Iterator;
import java.util.List;
import java.util.Optional;
import java.util.Set;
import java.util.concurrent.CompletableFuture;
import java.util.stream.Collectors;
import io.vertx.core.logging.Logger;
import io.vertx.core.logging.LoggerFactory;
import static java.util.stream.Collectors.groupingBy;
import static java.util.stream.Collectors.toList;
import static one.util.streamex.StreamEx.ofSubLists;
import static org.folio.orders.utils.HelperUtils.calculateCostUnitsTotal;
import static org.folio.orders.utils.HelperUtils.collectResultsOnSuccess;
import static org.folio.rest.RestConstants.MAX_IDS_FOR_GET_RQ;
import static org.folio.rest.jaxrs.model.EncumbranceRollover.OrderType.ONE_TIME;

public class RolloverOrderService {
  private static final Logger LOG = LoggerFactory.getLogger(RolloverOrderService.class);

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
  private final RestClient transactionRestClient;


  public RolloverOrderService(FundService fundService, PurchaseOrderService purchaseOrderService,
                              PurchaseOrderLineService purchaseOrderLineService, RestClient transactionRestClient) {
    this.fundService = fundService;
    this.purchaseOrderService = purchaseOrderService;
    this.purchaseOrderLineService = purchaseOrderLineService;
    this.transactionRestClient = transactionRestClient;
  }

  public CompletableFuture<Void> rollover(LedgerFiscalYearRollover ledgerFYRollover, RequestContext requestContext) {
    return fundService.getFundsByLedgerId(ledgerFYRollover.getLedgerId(), requestContext)
                      .thenApply(ledgerFunds -> ledgerFunds.stream().map(Fund::getId).collect(toList()))
                      .thenCompose(ledgerFundIds -> getFundsOrderIds(ledgerFundIds, ledgerFYRollover, requestContext))
                      .thenCompose(orderIds -> rolloverOrderLinesByChunks(orderIds, ledgerFYRollover, requestContext))
                      .thenApply(poLines -> {
                        String s ="d";
                        return poLines;
                      })
                      .thenAccept(v -> LOG.debug("Rollover : All order processed"));
  }

  private CompletableFuture<List<PoLine>> rolloverOrderLinesByChunks(Set<String> orderIds, LedgerFiscalYearRollover ledgerFYRollover, RequestContext requestContext) {
    LOG.debug("Start : All order processed" + orderIds);
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
    List<CompletableFuture<Set<String>>> futures = new ArrayList<>();
    String query = buildOpenOrderQueryByFundIdsAndTypes(chunkFundIds, ledgerFYRollover);
    return purchaseOrderService.getPurchaseOrdersByFunds(query, 0, 0, requestContext)
              .thenApply(PurchaseOrderCollection::getTotalRecords)
              .thenCompose(orderTotalRecords ->  {
                int chunkTotals = orderTotalRecords/ORDERS_CHUNK + 1;
                for (int i = 0; i < chunkTotals; i++) {
                  futures.add(purchaseOrderService.getPurchaseOrdersByFunds(query, ORDERS_CHUNK, i * ORDERS_CHUNK, requestContext)
                                                  .thenApply(this::extractOrderIds)
                  );
                }
                return collectResultsOnSuccess(futures).thenApply(results -> results.stream()
                  .flatMap(Set::stream)
                  .collect(Collectors.toSet())
                );
              });
  }

  private CompletableFuture<List<PoLine>> rolloverPoLinesChunk(List<String> orderIds, LedgerFiscalYearRollover ledgerFYRollover,
                                                                                 RequestContext requestContext) {
    CompletableFuture<List<PoLine>> poLinesFuture = getPoLinesByOrderIds(orderIds, requestContext);
    CompletableFuture<List<Transaction>> encumbrancesFuture = getEncumbrancesForRollover(orderIds, ledgerFYRollover, requestContext);

    return CompletableFuture.allOf(poLinesFuture, encumbrancesFuture)
      .thenApply(v -> poLinesFuture.join())
      .thenApply(poLines -> buildPoLineEncumbrancesHolders(ledgerFYRollover, poLines, encumbrancesFuture.join()))
      .thenApply(this::applyPoLinesRolloverChanges);
  }

  private List<PoLine> applyPoLinesRolloverChanges(List<PoLineEncumbrancesHolder> poLineEncumbrancesHolders) {
    poLineEncumbrancesHolders.forEach(holder -> {
      PoLine poLine = holder.getPoLine();
      var currEncumbranceFundIdMap = holder.getCurrEncumbrances().stream().collect(groupingBy(Transaction::getFromFundId));
      if (!MapUtils.isEmpty(currEncumbranceFundIdMap)) {
        poLine.getFundDistribution().forEach(fundDistribution -> {
          var currEncumbrances = currEncumbranceFundIdMap.get(fundDistribution.getFundId());
          if (!CollectionUtils.isEmpty(currEncumbrances)) {
            fundDistribution.setEncumbrance(currEncumbrances.get(0).getId());
            Double fyroAdjustmentAmount = calculateFYROAdjustmentAmount(holder).getNumber().doubleValue();
            poLine.getCost().setFyroAdjustmentAmount(fyroAdjustmentAmount);
            Double estimatedPrice = calculateEstimatedPrice(holder);
            poLine.getCost().setPoLineEstimatedPrice(estimatedPrice);
          }
        });
      }
    });
    return poLineEncumbrancesHolders.stream().map(PoLineEncumbrancesHolder::getPoLine).collect(toList());
  }

  private Double calculateEstimatedPrice(PoLineEncumbrancesHolder holder) {
    BigDecimal totalCurrEncumbranceInitialAmount = calculateTotalInitialAmountEncumbered(holder.getCurrEncumbrances());
    return totalCurrEncumbranceInitialAmount.doubleValue();
  }

  private MonetaryAmount calculateFYROAdjustmentAmount(PoLineEncumbrancesHolder holder) {
    Cost cost = holder.getPoLine().getCost();
    BigDecimal totalCurrEncumbranceInitialAmount = calculateTotalInitialAmountEncumbered(holder.getCurrEncumbrances());
    MonetaryAmount costUnitsTotal = calculateCostUnitsTotal(cost);
    MonetaryAmount totalMonetaryCurrEncumbranceInitialAmount = Money.of(totalCurrEncumbranceInitialAmount, cost.getCurrency());
    if (ONE_TIME.value().equals(holder.getOrderType())) {
      return costUnitsTotal.subtract(totalMonetaryCurrEncumbranceInitialAmount);
    }
    return totalMonetaryCurrEncumbranceInitialAmount.subtract(costUnitsTotal);
  }

  private BigDecimal calculateTotalInitialAmountEncumbered(List<Transaction> encumbrances) {
    return encumbrances.stream()
                       .map(Transaction::getEncumbrance)
                       .map(Encumbrance::getInitialAmountEncumbered)
                       .map(BigDecimal::valueOf).reduce(BigDecimal.ZERO, BigDecimal::add);
  }

  private List<PoLineEncumbrancesHolder> buildPoLineEncumbrancesHolders(LedgerFiscalYearRollover ledgerFYRollover, List<PoLine> poLines,
                                                                        List<Transaction> encumbrances) {
    List<PoLineEncumbrancesHolder> poLineEncumbrancesHolders = new ArrayList<>();
    String prevFYId = ledgerFYRollover.getFromFiscalYearId();
    String currFYId = ledgerFYRollover.getToFiscalYearId();
    poLines.forEach(poLine -> {
      PoLineEncumbrancesHolder holder = new PoLineEncumbrancesHolder(poLine);
      extractPoLineEncumbrances(poLine, encumbrances).forEach(encumbrance -> {
        String encumbranceFYId = encumbrance.getFiscalYearId();
        if (encumbranceFYId.equals(prevFYId)) {
          holder.addPrevEncumbrance(encumbrance);
        } else if (encumbranceFYId.equals(currFYId)) {
          holder.addCurrEncumbrance(encumbrance);
        }
        holder.withOrderType(encumbrance.getEncumbrance().getOrderType().value());
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
    return transactionRestClient.get(query, 0, Integer.MAX_VALUE, requestContext, TransactionCollection.class)
                                .thenApply(TransactionCollection::getTransactions);
  }

  private String buildQueryEncumbrancesForRollover(List<String> orderIds, LedgerFiscalYearRollover ledgerFYRollover) {
    String fiscalYearIdsQuery = buildQuery(List.of(ledgerFYRollover.getFromFiscalYearId(), ledgerFYRollover.getToFiscalYearId()), ENCUMBR_FY_QUERY, OR);
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
  //poLine.fundDistribution == "*\"fundId\":*\"65032151-39a5-4cef-8810-5350eb316300\"*" or poLine.fundDistribution == "*\"fundId\":*\"65032151-39a5-4cef-8810-5350eb316301\"*"
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
    if (ONE_TIME == encumberRolloverType) {
      return PurchaseOrder.OrderType.ONE_TIME;
    }
    return PurchaseOrder.OrderType.ONGOING;
  }
}
