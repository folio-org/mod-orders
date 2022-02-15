package org.folio.service.finance.transaction;

import io.restassured.http.Header;
import io.vertx.core.Context;
import io.vertx.core.Vertx;
import org.folio.rest.acq.model.finance.Encumbrance;
import org.folio.rest.acq.model.finance.Transaction;
import org.folio.rest.acq.model.finance.TransactionCollection;
import org.folio.rest.acq.model.invoice.Adjustment;
import org.folio.rest.acq.model.invoice.InvoiceLine;
import org.folio.rest.core.models.RequestContext;
import org.folio.rest.jaxrs.model.CompositePoLine;
import org.folio.rest.jaxrs.model.CompositePurchaseOrder;
import org.folio.rest.jaxrs.model.FundDistribution;
import org.folio.rest.jaxrs.model.PoLine;
import org.folio.service.invoice.InvoiceLineService;
import org.folio.service.orders.OrderInvoiceRelationService;
import org.junit.jupiter.api.BeforeEach;
import org.junit.jupiter.api.Test;
import org.mockito.InOrder;
import org.mockito.InjectMocks;
import org.mockito.Mock;
import org.mockito.MockitoAnnotations;

import java.math.BigDecimal;
import java.util.ArrayList;
import java.util.Arrays;
import java.util.HashMap;
import java.util.List;
import java.util.Map;
import java.util.UUID;
import java.util.concurrent.CompletableFuture;

import static java.util.concurrent.CompletableFuture.completedFuture;
import static org.folio.TestConfig.mockPort;
import static org.folio.TestConstants.X_OKAPI_TOKEN;
import static org.folio.TestConstants.X_OKAPI_USER_ID;
import static org.folio.TestUtils.getMockAsJson;
import static org.folio.rest.RestConstants.OKAPI_URL;
import static org.folio.rest.RestVerticle.OKAPI_HEADER_TENANT;
import static org.folio.rest.impl.MockServer.BASE_MOCK_DATA_PATH;
import static org.folio.rest.impl.MockServer.ENCUMBRANCE_PATH;
import static org.junit.jupiter.api.Assertions.assertEquals;
import static org.junit.jupiter.api.Assertions.assertFalse;
import static org.mockito.ArgumentMatchers.any;
import static org.mockito.ArgumentMatchers.anyInt;
import static org.mockito.ArgumentMatchers.anyList;
import static org.mockito.ArgumentMatchers.anyString;
import static org.mockito.ArgumentMatchers.argThat;
import static org.mockito.ArgumentMatchers.eq;
import static org.mockito.Mockito.doReturn;
import static org.mockito.Mockito.inOrder;
import static org.mockito.Mockito.never;
import static org.mockito.Mockito.times;
import static org.mockito.Mockito.verify;
import static org.mockito.Mockito.when;

public class EncumbranceServiceTest {
  private static final String ORDER_ID = "1ab7ef6a-d1d4-4a4f-90a2-882aed18af14";
  private static final String ORDER_PATH = BASE_MOCK_DATA_PATH + "compositeOrders/" + ORDER_ID + ".json";
  public static final String TENANT_ID = "ordertest";
  public static final Header X_OKAPI_TENANT = new Header(OKAPI_HEADER_TENANT, TENANT_ID);

  @InjectMocks
  private EncumbranceService encumbranceService;

  private RequestContext requestContextMock;

  @Mock
  private TransactionService transactionService;
  @Mock
  private TransactionSummariesService transactionSummariesService;
  @Mock
  private OrderInvoiceRelationService orderInvoiceRelationService;
  @Mock
  private InvoiceLineService invoiceLineService;

  @BeforeEach
  public void initMocks(){
    Context ctxMock = Vertx.vertx().getOrCreateContext();
    Map<String, String> okapiHeadersMock = new HashMap<>();
    okapiHeadersMock.put(OKAPI_URL, "http://localhost:" + mockPort);
    okapiHeadersMock.put(X_OKAPI_TOKEN.getName(), X_OKAPI_TOKEN.getValue());
    okapiHeadersMock.put(X_OKAPI_TENANT.getName(), X_OKAPI_TENANT.getValue());
    okapiHeadersMock.put(X_OKAPI_USER_ID.getName(), X_OKAPI_USER_ID.getValue());
    requestContextMock = new RequestContext(ctxMock, okapiHeadersMock);
    MockitoAnnotations.openMocks(this);
  }

  @Test
  void testShouldInvokeUpdateTransactionTimesEqualToTransactionQuantity() {
    //given

    Transaction encumbrance = getMockAsJson(ENCUMBRANCE_PATH).getJsonArray("transactions").getJsonObject(0).mapTo(Transaction.class);
    doReturn(completedFuture(null)).when(transactionService).updateTransaction(any(), eq(requestContextMock));
    doReturn(completedFuture(null)).when(transactionSummariesService).updateOrderTransactionSummary(anyString(), anyInt(), eq(requestContextMock));
    //When
    encumbranceService.updateEncumbrances(Arrays.asList(encumbrance, encumbrance), requestContextMock);
    //Then
    verify(transactionService, times(1)).updateTransactions(any(), eq(requestContextMock));
  }

  @Test
  void testShouldUpdateEncumbranceWithNewAmountFromLineAndFundFromLine() {
    //Given

    CompositePurchaseOrder order = getMockAsJson(ORDER_PATH).mapTo(CompositePurchaseOrder.class);
    CompositePoLine line = order.getCompositePoLines().get(0);
    FundDistribution fundDistribution = order.getCompositePoLines().get(0).getFundDistribution().get(0);
    Transaction encumbrance = getMockAsJson(ENCUMBRANCE_PATH).getJsonArray("transactions").getJsonObject(0).mapTo(Transaction.class);
    //When
    encumbranceService.updateEncumbrance(fundDistribution, line, encumbrance);
    //Then
    assertEquals(BigDecimal.valueOf(line.getCost().getListUnitPrice()), BigDecimal.valueOf(encumbrance.getAmount()));
    assertEquals(BigDecimal.valueOf(encumbrance.getAmount()), BigDecimal.valueOf(encumbrance.getEncumbrance().getInitialAmountEncumbered()));
  }

  @Test
  void shouldSimplyDeleteWhenDeleteOrderEncumbrances() {
    //Given

    String orderId = UUID.randomUUID().toString();
    Transaction encumbrance = new Transaction()
            .withId(UUID.randomUUID().toString())
            .withEncumbrance(new Encumbrance().withSourcePurchaseOrderId(orderId));
    Transaction encumbrance2 = new Transaction()
            .withId(UUID.randomUUID().toString())
            .withEncumbrance(new Encumbrance().withSourcePurchaseOrderId(orderId));
    List<Transaction> transactions = new ArrayList<>();
    transactions.add(encumbrance);
    transactions.add(encumbrance2);
    TransactionCollection transactionCollection = new TransactionCollection().withTransactions(transactions).withTotalRecords(1);
    doReturn(CompletableFuture.completedFuture(transactionCollection)).when(transactionService).getTransactions(anyString(), anyInt(), anyInt(), eq(requestContextMock));

    doReturn(CompletableFuture.completedFuture(null)).when(transactionSummariesService).updateOrderTransactionSummary(anyString(), anyInt(), eq(requestContextMock));
    doReturn(CompletableFuture.completedFuture(null)).when(transactionService).updateTransactions(any(), eq(requestContextMock));
    doReturn(CompletableFuture.completedFuture(null)).when(transactionService).deleteTransactions(any(), eq(requestContextMock));
    //When
    encumbranceService.deleteOrderEncumbrances(orderId, requestContextMock).join();

    //Then
    InOrder inOrder = inOrder(transactionService);
    inOrder.verify(transactionService).deleteTransactions(any(), eq(requestContextMock));

  }

  @Test
  void shouldSimplyDeleteWhenDeletePoLineEncumbrances() {
    //Given

    String lineId = UUID.randomUUID().toString();
    String orderId = UUID.randomUUID().toString();
    Transaction encumbrance = new Transaction()
            .withId(UUID.randomUUID().toString())
            .withEncumbrance(new Encumbrance()
                    .withSourcePurchaseOrderId(orderId)
                    .withSourcePoLineId(lineId));
    Transaction encumbrance2 = new Transaction()
            .withId(UUID.randomUUID().toString())
            .withEncumbrance(new Encumbrance()
                    .withSourcePurchaseOrderId(orderId)
                    .withSourcePoLineId(lineId));
    List<Transaction> transactions = new ArrayList<>();
    transactions.add(encumbrance);
    transactions.add(encumbrance2);
    TransactionCollection transactionCollection = new TransactionCollection().withTransactions(transactions).withTotalRecords(1);
    doReturn(CompletableFuture.completedFuture(transactionCollection)).when(transactionService).getTransactions(anyString(), anyInt(), anyInt(), any());

    doReturn(CompletableFuture.completedFuture(null)).when(transactionSummariesService).updateOrderTransactionSummary(any(), anyInt(), eq(requestContextMock));
    doReturn(CompletableFuture.completedFuture(null)).when(transactionService).updateTransactions(any(), eq(requestContextMock));
    doReturn(CompletableFuture.completedFuture(null)).when(transactionService).deleteTransactions(any(), eq(requestContextMock));
    //When
    encumbranceService.deletePoLineEncumbrances(new PoLine(), requestContextMock).join();

    //Then
    InOrder inOrder = inOrder(transactionService);
    inOrder.verify(transactionService).deleteTransactions(any(), eq(requestContextMock));

  }

  @Test
  void shouldCallRemoveEncumbranceLinks() {
    //Given
    String poLineId1 = UUID.randomUUID().toString();
    String poLineId2 = UUID.randomUUID().toString();
    String orderId = UUID.randomUUID().toString();
    String encumbrance1Id = UUID.randomUUID().toString();
    String encumbrance2Id = UUID.randomUUID().toString();
    String encumbrance3Id = UUID.randomUUID().toString();
    Transaction encumbrance1 = new Transaction()
      .withId(encumbrance1Id)
      .withEncumbrance(new Encumbrance()
        .withSourcePurchaseOrderId(orderId)
        .withSourcePoLineId(poLineId1));
    Transaction encumbrance2 = new Transaction()
      .withId(encumbrance2Id)
      .withEncumbrance(new Encumbrance()
        .withSourcePurchaseOrderId(orderId)
        .withSourcePoLineId(poLineId1));
    Transaction encumbrance3 = new Transaction()
      .withId(encumbrance3Id)
      .withEncumbrance(new Encumbrance()
        .withSourcePurchaseOrderId(orderId)
        .withSourcePoLineId(poLineId2));
    List<Transaction> transactions = List.of(encumbrance1, encumbrance2, encumbrance3);
    List<String> transactionIds = List.of(encumbrance1Id, encumbrance2Id, encumbrance3Id);

    InvoiceLine invoiceLine1 = new InvoiceLine()
      .withId(UUID.randomUUID().toString())
      .withPoLineId(poLineId1)
      .withFundDistributions(List.of(new org.folio.rest.acq.model.invoice.FundDistribution()
        .withEncumbrance(encumbrance1Id)))
      .withAdjustments(List.of(new Adjustment().withFundDistributions(List.of(
        new org.folio.rest.acq.model.invoice.FundDistribution().withEncumbrance(encumbrance2Id)))));
    InvoiceLine invoiceLine2 = new InvoiceLine()
      .withId(UUID.randomUUID().toString())
      .withPoLineId(poLineId2)
      .withAdjustments(List.of(new Adjustment().withFundDistributions(List.of(
        new org.folio.rest.acq.model.invoice.FundDistribution().withEncumbrance(encumbrance3Id)))));
    List<InvoiceLine> invoiceLines = List.of(invoiceLine1, invoiceLine2);

    when(orderInvoiceRelationService.isOrderLinkedToAnInvoice(eq(orderId), eq(requestContextMock)))
      .thenReturn(CompletableFuture.completedFuture(true));
    when(invoiceLineService.getInvoiceLinesByOrderLineIds(anyList(), eq(requestContextMock)))
      .thenReturn(CompletableFuture.completedFuture(invoiceLines));
    when(invoiceLineService.removeEncumbranceLinks(anyList(), anyList(), eq(requestContextMock)))
      .thenReturn(CompletableFuture.completedFuture(null));

    //When
    CompletableFuture<Void> result = encumbranceService.deleteEncumbranceLinksInInvoiceLines(transactions, requestContextMock);
    assertFalse(result.isCompletedExceptionally());
    result.join();

    //Then
    verify(invoiceLineService, times(1)).removeEncumbranceLinks(
      argThat(lines -> lines.size() == 2),
      argThat(ids -> ids.containsAll(transactionIds)),
      eq(requestContextMock));
  }

  @Test
  void shouldNotCallRemoveEncumbranceLinks() {
    //Given
    String poLineId = UUID.randomUUID().toString();
    String orderId = UUID.randomUUID().toString();
    String encumbrance1Id = UUID.randomUUID().toString();
    String encumbrance2Id = UUID.randomUUID().toString();
    Transaction encumbrance1 = new Transaction()
      .withId(encumbrance1Id)
      .withEncumbrance(new Encumbrance()
        .withSourcePurchaseOrderId(orderId)
        .withSourcePoLineId(poLineId));
    Transaction encumbrance2 = new Transaction()
      .withId(encumbrance2Id)
      .withEncumbrance(new Encumbrance()
        .withSourcePurchaseOrderId(orderId)
        .withSourcePoLineId(poLineId));
    List<Transaction> transactions = List.of(encumbrance1, encumbrance2);

    when(orderInvoiceRelationService.isOrderLinkedToAnInvoice(eq(orderId), eq(requestContextMock)))
      .thenReturn(CompletableFuture.completedFuture(false));

    //When
    CompletableFuture<Void> result = encumbranceService.deleteEncumbranceLinksInInvoiceLines(transactions, requestContextMock);
    assertFalse(result.isCompletedExceptionally());
    result.join();

    //Then
    verify(orderInvoiceRelationService, times(1))
      .isOrderLinkedToAnInvoice(eq(orderId), eq(requestContextMock));
    verify(invoiceLineService, never()).getInvoiceLinesByOrderLineIds(any(), any());
    verify(invoiceLineService, never()).removeEncumbranceLinks(any(), any(), any());
  }

}
