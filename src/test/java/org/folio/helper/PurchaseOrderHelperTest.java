package org.folio.helper;

import static java.util.concurrent.CompletableFuture.completedFuture;
import static org.folio.ApiTestSuite.mockPort;
import static org.folio.rest.RestConstants.OKAPI_URL;
import static org.folio.rest.impl.MockServer.BASE_MOCK_DATA_PATH;
import static org.folio.rest.impl.MockServer.ENCUMBRANCE_PATH;
import static org.folio.rest.impl.PurchaseOrdersApiTest.X_OKAPI_TENANT;
import static org.junit.jupiter.api.Assertions.assertEquals;
import static org.junit.jupiter.api.Assertions.assertFalse;
import static org.junit.jupiter.api.Assertions.assertThrows;
import static org.junit.jupiter.api.Assertions.assertTrue;
import static org.mockito.ArgumentMatchers.any;
import static org.mockito.ArgumentMatchers.anyInt;
import static org.mockito.ArgumentMatchers.anyString;
import static org.mockito.Mockito.CALLS_REAL_METHODS;
import static org.mockito.Mockito.doNothing;
import static org.mockito.Mockito.doReturn;
import static org.mockito.Mockito.mock;
import static org.mockito.Mockito.spy;
import static org.mockito.Mockito.verify;

import java.util.Collections;
import java.util.HashMap;
import java.util.Map;
import java.util.UUID;
import java.util.concurrent.CompletableFuture;
import java.util.concurrent.CompletionException;

import org.folio.orders.rest.exceptions.HttpException;
import org.folio.orders.utils.ErrorCodes;
import org.folio.rest.acq.model.OrderInvoiceRelationship;
import org.folio.rest.acq.model.OrderInvoiceRelationshipCollection;
import org.folio.rest.acq.model.finance.Encumbrance;
import org.folio.rest.acq.model.finance.FiscalYear;
import org.folio.rest.acq.model.finance.Fund;
import org.folio.rest.acq.model.finance.LedgerFiscalYearRolloverError;
import org.folio.rest.acq.model.finance.LedgerFiscalYearRolloverErrorCollection;
import org.folio.rest.acq.model.finance.Transaction;
import org.folio.rest.core.RestClient;
import org.folio.rest.core.models.RequestContext;
import org.folio.rest.impl.ApiTestBase;
import org.folio.rest.jaxrs.model.CompositePurchaseOrder;
import org.folio.rest.jaxrs.model.LedgerFiscalYearRollover;
import org.folio.rest.jaxrs.model.LedgerFiscalYearRolloverCollection;
import org.folio.rest.tools.client.HttpClientFactory;
import org.folio.rest.tools.client.interfaces.HttpClientInterface;
import org.folio.service.finance.FundService;
import org.folio.service.orders.OrderInvoiceRelationService;
import org.junit.jupiter.api.BeforeEach;
import org.junit.jupiter.api.Test;
import org.junit.jupiter.api.extension.ExtendWith;
import org.mockito.Mock;

import io.vertx.core.Context;
import io.vertx.core.Vertx;
import io.vertx.junit5.VertxExtension;
import io.vertx.junit5.VertxTestContext;

@ExtendWith(VertxExtension.class)
public class PurchaseOrderHelperTest  extends ApiTestBase {
  private static final String ORDER_ID = "1ab7ef6a-d1d4-4a4f-90a2-882aed18af20";
  private static final String ORDER_PATH = BASE_MOCK_DATA_PATH + "compositeOrders/" + ORDER_ID + ".json";
  public static final String TWO_ENCUMBRANCE_PATH = BASE_MOCK_DATA_PATH + "encumbrances/two_pending_encumbrances.json";

  @Mock
  private PoNumberHelper poNumberHelper;

  private  Map<String, String> okapiHeadersMock;

  private Context ctxMock;

  private HttpClientInterface httpClient;

  @BeforeEach
  public void initMocks() {
    super.setUp();
    ctxMock = Vertx.vertx().getOrCreateContext();
    okapiHeadersMock = new HashMap<>();
    okapiHeadersMock.put(OKAPI_URL, "http://localhost:" + mockPort);
    okapiHeadersMock.put(X_OKAPI_TOKEN.getName(), X_OKAPI_TOKEN.getValue());
    okapiHeadersMock.put(X_OKAPI_TENANT.getName(), X_OKAPI_TENANT.getValue());
    okapiHeadersMock.put(X_OKAPI_USER_ID.getName(), X_OKAPI_USER_ID.getValue());
    String okapiURL = okapiHeadersMock.getOrDefault(OKAPI_URL, "");
    httpClient = HttpClientFactory.getHttpClient(okapiURL, X_OKAPI_TENANT.getValue());
  }

  @Test
  public void testShouldSetEncumbrancesToPending() {
    //given
    PurchaseOrderLineHelper orderLineHelper = mock(PurchaseOrderLineHelper.class, CALLS_REAL_METHODS);
    FinanceHelper financeHelper = mock(FinanceHelper.class, CALLS_REAL_METHODS);
    OrderInvoiceRelationService orderInvoiceRelationService = mock(OrderInvoiceRelationService.class, CALLS_REAL_METHODS);
    PurchaseOrderHelper serviceSpy = spy(
        new PurchaseOrderHelper(httpClient, okapiHeadersMock, ctxMock, "en", poNumberHelper, orderLineHelper, financeHelper, orderInvoiceRelationService));
    CompositePurchaseOrder order = getMockAsJson(ORDER_PATH).mapTo(CompositePurchaseOrder.class);

    Transaction encumbrance = getMockAsJson(ENCUMBRANCE_PATH).getJsonArray("transactions").getJsonObject(0).mapTo(Transaction.class);

    doReturn(completedFuture(order)).when(serviceSpy).updateAndGetOrderWithLines(any(CompositePurchaseOrder.class));
    doReturn(completedFuture(Collections.singletonList(encumbrance))).when(financeHelper).getOrderEncumbrances(any());
    doReturn(completedFuture(null)).when(financeHelper).updateTransactions(any());
    doReturn(completedFuture(null)).when(financeHelper).updateOrderTransactionSummary(anyString(), anyInt());
    doNothing().when(financeHelper).closeHttpClient();
    doNothing().when(orderLineHelper).closeHttpClient();
    doReturn(completedFuture(null)).when(orderLineHelper).updatePoLinesSummary(any());
    //When
    serviceSpy.unOpenOrder(order).join();
    //Then
    assertEquals(0d, encumbrance.getAmount(), 0.0);
    assertEquals(0d, encumbrance.getEncumbrance().getInitialAmountEncumbered(), 0.0);
    assertEquals(Encumbrance.Status.PENDING, encumbrance.getEncumbrance().getStatus());

    verify(financeHelper).makeEncumbrancesPending(any());
    verify(financeHelper).getOrderEncumbrances(any());
    verify(financeHelper).updateTransactions(any());
    verify(orderLineHelper).updatePoLinesSummary(any());
  }

  @Test
  public void testShouldEndExceptionallyAndCloseConnection() {
    //given
    PurchaseOrderLineHelper orderLineHelper = mock(PurchaseOrderLineHelper.class, CALLS_REAL_METHODS);
    FinanceHelper financeHelper = mock(FinanceHelper.class, CALLS_REAL_METHODS);
    OrderInvoiceRelationService orderInvoiceRelationService = mock(OrderInvoiceRelationService.class, CALLS_REAL_METHODS);

    PurchaseOrderHelper serviceSpy = spy(new PurchaseOrderHelper(httpClient, okapiHeadersMock, ctxMock, "en", poNumberHelper,
        orderLineHelper, financeHelper, orderInvoiceRelationService));
    CompositePurchaseOrder order = getMockAsJson(ORDER_PATH).mapTo(CompositePurchaseOrder.class);

    doReturn(completedFuture(order)).when(serviceSpy).updateAndGetOrderWithLines(any(CompositePurchaseOrder.class));
    doReturn(completedFuture(null)).when(financeHelper).getOrderEncumbrances(any());
    doNothing().when(financeHelper).closeHttpClient();
    doNothing().when(orderLineHelper).closeHttpClient();
    //When
    CompletableFuture<Void> act = serviceSpy.unOpenOrder(order);
    //Then
    assertTrue(act.isCompletedExceptionally());
  }

  @Test
  public void testPopulateNeedReEncumberField() {
    // given
    PurchaseOrderLineHelper orderLineHelper = mock(PurchaseOrderLineHelper.class, CALLS_REAL_METHODS);
    FinanceHelper financeHelper = mock(FinanceHelper.class, CALLS_REAL_METHODS);
    FundService fundService = mock(FundService.class, CALLS_REAL_METHODS);
    OrderInvoiceRelationService orderInvoiceRelationService = mock(OrderInvoiceRelationService.class, CALLS_REAL_METHODS);

    PurchaseOrderHelper serviceSpy = spy(
        new PurchaseOrderHelper(httpClient, okapiHeadersMock, ctxMock, "en", poNumberHelper, orderLineHelper, financeHelper, orderInvoiceRelationService));
    CompositePurchaseOrder order = getMockAsJson(ORDER_PATH).mapTo(CompositePurchaseOrder.class);

    Fund sampleFund = new Fund()
      .withLedgerId(UUID.randomUUID().toString())
      .withFundStatus(Fund.FundStatus.ACTIVE)
      .withCode(UUID.randomUUID().toString());

    LedgerFiscalYearRolloverCollection ledgerFiscalYearRollover = new LedgerFiscalYearRolloverCollection()
      .withLedgerFiscalYearRollovers(Collections.singletonList(new LedgerFiscalYearRollover()))
      .withTotalRecords(1);

    LedgerFiscalYearRolloverErrorCollection ledgerFiscalYearRolloverErrors = new LedgerFiscalYearRolloverErrorCollection()
      .withLedgerFiscalYearRolloverErrors(Collections.singletonList(new LedgerFiscalYearRolloverError()));

    // LedgerFiscalYearRolloverErrorCollection is not empty. Expected "needReEncumber" = true
    doReturn(completedFuture(sampleFund)).when(fundService).retrieveFundById(any(), any());
    doReturn(completedFuture(new FiscalYear())).when(financeHelper).getCurrentFiscalYear(any());
    doReturn(completedFuture(ledgerFiscalYearRollover)).when(financeHelper).getLedgerFyRollovers(any(), any());
    doReturn(completedFuture(ledgerFiscalYearRolloverErrors)).when(financeHelper).getLedgerFyRolloverErrors(any(), any());

    CompositePurchaseOrder compOrder = serviceSpy.populateNeedReEncumberFlag(order).join();
    assertTrue(compOrder.getNeedReEncumber());

    // LedgerFiscalYearRolloverErrorCollection is empty. Expected "needReEncumber" = false
    doReturn(completedFuture(new LedgerFiscalYearRolloverErrorCollection())).when(financeHelper).getLedgerFyRolloverErrors(any(), any());
    compOrder = serviceSpy.populateNeedReEncumberFlag(order).join();
    assertFalse(compOrder.getNeedReEncumber());

    // LedgerFyRollover not exists. Expected "needReEncumber" = false
    doReturn(completedFuture(new LedgerFiscalYearRolloverCollection())).when(financeHelper).getLedgerFyRollovers(any(), any());
    compOrder = serviceSpy.populateNeedReEncumberFlag(order).join();
    assertFalse(compOrder.getNeedReEncumber());

  }

  @Test
  public void testDeleteOrderLinkedToInvoiceWithError(VertxTestContext ctx) throws Throwable {
    // given
    RestClient restClient = mock(RestClient.class, CALLS_REAL_METHODS);
    OrderInvoiceRelationService orderInvoiceRelationService = spy(new OrderInvoiceRelationService(restClient));

    // for returning non empty collection
    OrderInvoiceRelationshipCollection oirCollection = new OrderInvoiceRelationshipCollection()
      .withOrderInvoiceRelationships(Collections.singletonList(new OrderInvoiceRelationship()))
      .withTotalRecords(1);

    doReturn(completedFuture(oirCollection)).when(restClient).get(anyString(), anyInt(), anyInt(), any(), any());

    CompletableFuture<Void> future = orderInvoiceRelationService.checkOrderInvoiceRelationship(ORDER_ID, new RequestContext(ctxMock, okapiHeadersMock));
    assertThrows(CompletionException.class, future::join);

    future.exceptionally(t -> {
      HttpException httpException = (HttpException) t.getCause();
      assertEquals(ErrorCodes.ORDER_RELATES_TO_INVOICE.getDescription(), httpException.getMessage());
      ctx.completeNow();
      return null;
    });
    checkVertxContextCompletion(ctx);
  }
}
