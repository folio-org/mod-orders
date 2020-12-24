package org.folio.helper;

import static java.util.concurrent.CompletableFuture.completedFuture;
import static org.folio.rest.impl.MockServer.BASE_MOCK_DATA_PATH;
import static org.folio.rest.impl.MockServer.ENCUMBRANCE_PATH;
import static org.junit.jupiter.api.Assertions.assertEquals;
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
import java.util.Map;
import java.util.concurrent.CompletableFuture;

import org.folio.rest.acq.model.finance.Encumbrance;
import org.folio.rest.acq.model.finance.Transaction;
import org.folio.rest.impl.ApiTestBase;
import org.folio.rest.jaxrs.model.CompositePurchaseOrder;
import org.folio.rest.tools.client.interfaces.HttpClientInterface;
import org.junit.jupiter.api.BeforeEach;
import org.junit.jupiter.api.Test;
import org.mockito.Mock;
import org.mockito.MockitoAnnotations;

import io.vertx.core.impl.EventLoopContext;

public class PurchaseOrderHelperTest  extends ApiTestBase {
  private static final String ORDER_ID = "1ab7ef6a-d1d4-4a4f-90a2-882aed18af20";
  private static final String ORDER_PATH = BASE_MOCK_DATA_PATH + "compositeOrders/" + ORDER_ID + ".json";
  public static final String TWO_ENCUMBRANCE_PATH = BASE_MOCK_DATA_PATH + "encumbrances/two_pending_encumbrances.json";

  @Mock
  private PoNumberHelper poNumberHelper;
  @Mock
  private Map<String, String> okapiHeadersMock;
  @Mock
  private EventLoopContext ctxMock;
  @Mock
  private HttpClientInterface httpClient;

  @BeforeEach
  public void initMocks(){
    MockitoAnnotations.openMocks(this);
  }

  @Test
  public void testShouldSetEncumbrancesToPending() {
    //given
    PurchaseOrderLineHelper orderLineHelper = mock(PurchaseOrderLineHelper.class, CALLS_REAL_METHODS);
    FinanceHelper financeHelper = mock(FinanceHelper.class, CALLS_REAL_METHODS);
    PurchaseOrderHelper serviceSpy = spy(
        new PurchaseOrderHelper(httpClient, okapiHeadersMock, ctxMock, "en", poNumberHelper, orderLineHelper, financeHelper));
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
    PurchaseOrderHelper serviceSpy = spy(new PurchaseOrderHelper(httpClient, okapiHeadersMock, ctxMock, "en"
      , poNumberHelper, orderLineHelper, financeHelper));
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
}
