package org.folio.service.orders;

import org.folio.rest.acq.model.finance.Fund;
import org.folio.rest.acq.model.finance.FundCollection;
import org.folio.rest.acq.model.finance.Transaction;
import org.folio.rest.acq.model.finance.TransactionCollection;
import org.folio.rest.core.RestClient;
import org.folio.rest.core.models.RequestContext;
import org.folio.rest.jaxrs.model.LedgerFiscalYearRollover;
import org.folio.rest.jaxrs.model.PoLine;
import org.folio.rest.jaxrs.model.PurchaseOrder;
import org.folio.rest.jaxrs.model.PurchaseOrderCollection;
import org.folio.service.finance.FundService;
import org.junit.jupiter.api.BeforeEach;
import org.junit.jupiter.api.Test;
import org.mockito.InjectMocks;
import org.mockito.Mock;
import org.mockito.MockitoAnnotations;
import java.util.ArrayList;
import java.util.List;
import java.util.UUID;
import static java.util.concurrent.CompletableFuture.completedFuture;
import static org.mockito.ArgumentMatchers.anyInt;
import static org.mockito.ArgumentMatchers.anyString;
import static org.mockito.Mockito.doReturn;
import static org.mockito.ArgumentMatchers.any;
import static org.mockito.ArgumentMatchers.anyInt;
import static org.mockito.ArgumentMatchers.anyString;
import static org.mockito.ArgumentMatchers.eq;

public class OrderRolloverServiceTest {
  @InjectMocks
  private OrderRolloverService orderRolloverService;

  @Mock
  private FundService fundService;
  @Mock
  private PurchaseOrderService purchaseOrderService;
  @Mock
  private PurchaseOrderLineService purchaseOrderLineService;
  @Mock
  private RestClient transactionRestClient;
  @Mock
  private RequestContext requestContext;

  @BeforeEach
  public void initMocks() {
    MockitoAnnotations.initMocks(this);
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

    LedgerFiscalYearRollover ledgerFiscalYearRollover = new LedgerFiscalYearRollover()
      .withId(UUID.randomUUID().toString())
      .withFromFiscalYearId(fromFiscalYearId)
      .withLedgerId(ledgerId)
      .withToFiscalYearId(toFiscalYearId);

    List<Fund> funds = List.of(new Fund().withId(fundId1).withLedgerId(ledgerId), new Fund().withId(fundId2).withLedgerId(ledgerId));

    PurchaseOrder purchaseOrder1 = new PurchaseOrder().withId(orderId1).withWorkflowStatus(PurchaseOrder.WorkflowStatus.OPEN);
    PurchaseOrder purchaseOrder2 = new PurchaseOrder().withId(orderId2).withWorkflowStatus(PurchaseOrder.WorkflowStatus.OPEN);
    List<PurchaseOrder> orders = List.of(purchaseOrder1, purchaseOrder2);
    PurchaseOrderCollection purchaseOrderCollection = new PurchaseOrderCollection().withPurchaseOrders(orders).withTotalRecords(2);

    List<PoLine> poLines = new ArrayList<>();

    doReturn(completedFuture(funds)).when(fundService).getFundsByLedgerId(ledgerId, requestContext);
    doReturn(completedFuture(purchaseOrderCollection)).when(purchaseOrderService).getPurchaseOrders(anyString(), anyInt(), anyInt(), any());
    doReturn(completedFuture(poLines)).when(purchaseOrderLineService).getOrderLines(anyString(), anyInt(), anyInt(), any());
    List<Transaction> encumbrances = new ArrayList<>();
    TransactionCollection encumbranceCollection = new TransactionCollection().withTransactions(encumbrances).withTotalRecords(2);
    doReturn(completedFuture(encumbranceCollection)).when(transactionRestClient).get(anyString(), anyInt(), anyInt(), any(), any());

    orderRolloverService.rollover(ledgerFiscalYearRollover, requestContext);
  }

}
