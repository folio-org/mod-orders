package org.folio.service.orders;

import static java.util.concurrent.CompletableFuture.completedFuture;
import static org.hamcrest.MatcherAssert.assertThat;
import static org.hamcrest.Matchers.equalTo;
import static org.junit.Assert.assertFalse;
import static org.junit.Assert.assertTrue;
import static org.mockito.ArgumentMatchers.any;
import static org.mockito.ArgumentMatchers.anyInt;
import static org.mockito.ArgumentMatchers.anyString;
import static org.mockito.ArgumentMatchers.eq;
import static org.mockito.Mockito.doReturn;

import java.util.Arrays;
import java.util.List;
import java.util.UUID;
import java.util.concurrent.CompletableFuture;

import org.folio.rest.acq.model.finance.Encumbrance;
import org.folio.rest.acq.model.finance.Fund;
import org.folio.rest.acq.model.finance.Transaction;
import org.folio.rest.acq.model.finance.TransactionCollection;
import org.folio.rest.core.RestClient;
import org.folio.rest.core.models.RequestContext;
import org.folio.rest.jaxrs.model.Cost;
import org.folio.rest.jaxrs.model.EncumbranceRollover;
import org.folio.rest.jaxrs.model.FundDistribution;
import org.folio.rest.jaxrs.model.LedgerFiscalYearRollover;
import org.folio.rest.jaxrs.model.PoLine;
import org.folio.rest.jaxrs.model.PurchaseOrder;
import org.folio.rest.jaxrs.model.PurchaseOrderCollection;
import org.folio.service.finance.FundService;
import org.folio.service.finance.TransactionService;
import org.junit.jupiter.api.BeforeEach;
import org.junit.jupiter.api.Test;
import org.mockito.InjectMocks;
import org.mockito.Mock;
import org.mockito.MockitoAnnotations;

public class OrderRolloverServiceTest {
  @InjectMocks
  private OrderRolloverService orderRolloverService;

  @Mock
  private FundService fundService;
  @Mock
  private TransactionService transactionService;
  @Mock
  private PurchaseOrderService purchaseOrderService;
  @Mock
  private PurchaseOrderLineService purchaseOrderLineService;
  @Mock
  private RequestContext requestContext;

  @BeforeEach
  public void initMocks() {
    MockitoAnnotations.openMocks(this);
  }


  @Test
  void shouldUpdateOrderLinesCostAndEncumbranceLinks() {
    String fromFiscalYearId = UUID.randomUUID().toString();
    String ledgerId = UUID.randomUUID().toString();
    String toFiscalYearId = UUID.randomUUID().toString();
    String fundId1 = UUID.randomUUID().toString();
    String fundId2 = UUID.randomUUID().toString();
    String orderId1 = UUID.randomUUID().toString();
    String orderId2 = UUID.randomUUID().toString();
    String poLineId1 = UUID.randomUUID().toString();
    String poLineId2 = UUID.randomUUID().toString();
    String prevEncumbrId1 = UUID.randomUUID().toString();
    String prevEncumbrId2 = UUID.randomUUID().toString();
    String currEncumbrId1 = UUID.randomUUID().toString();
    String currEncumbrId2 = UUID.randomUUID().toString();
    String expClassId2 = UUID.randomUUID().toString();

    EncumbranceRollover ongoingEncumbrance = new EncumbranceRollover()
        .withOrderType(EncumbranceRollover.OrderType.ONGOING).withBasedOn(EncumbranceRollover.BasedOn.EXPENDED);
    EncumbranceRollover oneTimeEncumbrance = new EncumbranceRollover()
      .withOrderType(EncumbranceRollover.OrderType.ONE_TIME).withBasedOn(EncumbranceRollover.BasedOn.REMAINING);


    LedgerFiscalYearRollover ledgerFiscalYearRollover = new LedgerFiscalYearRollover()
      .withId(UUID.randomUUID().toString())
      .withFromFiscalYearId(fromFiscalYearId)
      .withLedgerId(ledgerId)
      .withToFiscalYearId(toFiscalYearId)
      .withEncumbrancesRollover(List.of(ongoingEncumbrance, oneTimeEncumbrance));

    List<Fund> funds = List.of(new Fund().withId(fundId1).withLedgerId(ledgerId), new Fund().withId(fundId2).withLedgerId(ledgerId));

    PurchaseOrder purchaseOrder1 = new PurchaseOrder().withId(orderId1).withWorkflowStatus(PurchaseOrder.WorkflowStatus.OPEN);
    PurchaseOrder purchaseOrder2 = new PurchaseOrder().withId(orderId2).withWorkflowStatus(PurchaseOrder.WorkflowStatus.OPEN);
    List<PurchaseOrder> orders = List.of(purchaseOrder1, purchaseOrder2);
    PurchaseOrderCollection purchaseOrderCollection = new PurchaseOrderCollection().withPurchaseOrders(orders).withTotalRecords(2);

    FundDistribution fundDistributionOneTime = new FundDistribution().withFundId(fundId1).withValue(100d).withEncumbrance(prevEncumbrId1);
    FundDistribution fundDistributionOngoing = new FundDistribution().withFundId(fundId2).withValue(100d).withEncumbrance(prevEncumbrId2).withExpenseClassId(expClassId2);

    Cost costOneTime = new Cost().withListUnitPrice(100d).withQuantityPhysical(1).withCurrency("USD").withPoLineEstimatedPrice(100d);
    PoLine poLineOneTime = new PoLine().withId(poLineId1).withPurchaseOrderId(orderId1).withCost(costOneTime).withFundDistribution(List.of(fundDistributionOneTime));

    Cost costOngoing = new Cost().withListUnitPrice(100d).withQuantityPhysical(1).withCurrency("USD").withPoLineEstimatedPrice(100d);
    PoLine poLineOngoing = new PoLine().withId(poLineId2).withPurchaseOrderId(orderId2).withCost(costOngoing).withFundDistribution(List.of(fundDistributionOngoing));
    List<PoLine> poLines = List.of(poLineOneTime, poLineOngoing);



    doReturn(completedFuture(funds)).when(fundService).getFundsByLedgerId(ledgerId, requestContext);
    doReturn(completedFuture(purchaseOrderCollection)).when(purchaseOrderService).getPurchaseOrders(anyString(), anyInt(), anyInt(), any());
    doReturn(completedFuture(poLines)).when(purchaseOrderLineService).getOrderLines(anyString(), anyInt(), anyInt(), any());
    doReturn(completedFuture(null)).when(purchaseOrderLineService).updateOrderLines(eq(poLines), any());

    Encumbrance encumbranceOneTime = new Encumbrance().withSourcePurchaseOrderId(orderId1).withSourcePoLineId(poLineId1)
                                        .withOrderType(Encumbrance.OrderType.ONE_TIME).withInitialAmountEncumbered(60d);
    Transaction transactionOneTime = new Transaction().withId(currEncumbrId1).withFromFundId(fundId1).withEncumbrance(encumbranceOneTime);
    Encumbrance encumbranceOngoing = new Encumbrance().withSourcePurchaseOrderId(orderId2).withSourcePoLineId(poLineId2)
                                        .withOrderType(Encumbrance.OrderType.ONGOING).withInitialAmountEncumbered(90d);
    Transaction transactionOngoing = new Transaction().withId(currEncumbrId2).withFromFundId(fundId2).withEncumbrance(encumbranceOngoing).withExpenseClassId(expClassId2);
    List<Transaction> encumbrances = List.of(transactionOneTime, transactionOngoing);
    TransactionCollection encumbranceCollection = new TransactionCollection().withTransactions(encumbrances).withTotalRecords(2);
    doReturn(completedFuture(encumbranceCollection)).when(transactionService).getTransactions(anyString(), anyInt(), anyInt(), any());

    CompletableFuture<Void> future = orderRolloverService.rollover(ledgerFiscalYearRollover, requestContext);
    future.join();
    assertFalse(future.isCompletedExceptionally());

    assertThat(fundDistributionOneTime.getEncumbrance(), equalTo(currEncumbrId1));
    assertThat(fundDistributionOngoing.getEncumbrance(), equalTo(currEncumbrId2));

    assertThat(costOneTime.getPoLineEstimatedPrice(), equalTo(60d));
    assertThat(costOngoing.getPoLineEstimatedPrice(), equalTo(90d));

    assertThat(costOneTime.getFyroAdjustmentAmount(), equalTo(-40d));
    assertThat(costOngoing.getFyroAdjustmentAmount(), equalTo(-10d));
  }

}
