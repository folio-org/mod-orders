package org.folio.helper;

import static java.util.concurrent.CompletableFuture.completedFuture;
import static org.folio.rest.impl.MockServer.BASE_MOCK_DATA_PATH;
import static org.folio.rest.impl.MockServer.ENCUMBRANCE_PATH;
import static org.junit.Assert.assertEquals;
import static org.junit.Assert.assertTrue;
import static org.mockito.ArgumentMatchers.any;
import static org.mockito.ArgumentMatchers.anyInt;
import static org.mockito.ArgumentMatchers.anyString;
import static org.mockito.Mockito.CALLS_REAL_METHODS;
import static org.mockito.Mockito.doNothing;
import static org.mockito.Mockito.doReturn;
import static org.mockito.Mockito.mock;
import static org.mockito.Mockito.spy;
import static org.mockito.Mockito.verify;

import java.util.ArrayList;
import java.util.Arrays;
import java.util.Collections;
import java.util.List;
import java.util.Map;
import java.util.concurrent.CompletableFuture;

import org.folio.models.EncumbranceRelationsHolder;
import org.folio.models.EncumbrancesProcessingHolder;
import org.folio.rest.acq.model.finance.Encumbrance;
import org.folio.rest.acq.model.finance.Transaction;
import org.folio.rest.impl.ApiTestBase;
import org.folio.rest.jaxrs.model.CompositePurchaseOrder;
import org.folio.rest.tools.client.interfaces.HttpClientInterface;
import org.junit.Before;
import org.junit.Ignore;
import org.junit.Rule;
import org.junit.Test;
import org.junit.rules.ExpectedException;
import org.mockito.Mock;
import org.mockito.MockitoAnnotations;

import io.vertx.core.impl.EventLoopContext;
import io.vertx.core.json.JsonArray;

public class PurchaseOrderHelperTest  extends ApiTestBase {
  private static final String ORDER_ID = "1ab7ef6a-d1d4-4a4f-90a2-882aed18af20";
  private static final String ORDER_PATH = BASE_MOCK_DATA_PATH + "compositeOrders/" + ORDER_ID + ".json";
  public static final String TWO_ENCUMBRANCE_PATH = BASE_MOCK_DATA_PATH + "encumbrances/two_pending_encumbrances.json";

  @Rule
  public ExpectedException expectedException = ExpectedException.none();
  @Mock
  private PoNumberHelper poNumberHelper;
  @Mock
  private Map<String, String> okapiHeadersMock;
  @Mock
  private EventLoopContext ctxMock;
  @Mock
  private HttpClientInterface httpClient;

  @Before
  public void initMocks(){
    MockitoAnnotations.initMocks(this);
  }

  @Test
  public void testShouldSetEncumbrancesToPending() {
    //given
    PurchaseOrderLineHelper orderLineHelper = mock(PurchaseOrderLineHelper.class, CALLS_REAL_METHODS);
    FinanceHelper financeHelper = mock(FinanceHelper.class, CALLS_REAL_METHODS);
    PurchaseOrderHelper serviceSpy = spy(new PurchaseOrderHelper(httpClient, okapiHeadersMock, ctxMock, "en"
      , poNumberHelper, orderLineHelper, financeHelper));
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
  @Ignore
  public void testShouldCreateOneNewEncumbranceAndUpdateTwoExistedZeroForRelease() {
    //given
    PurchaseOrderLineHelper orderLineHelper = mock(PurchaseOrderLineHelper.class, CALLS_REAL_METHODS);
    FinanceHelper financeHelper = mock(FinanceHelper.class, CALLS_REAL_METHODS);
    PurchaseOrderHelper serviceSpy = spy(new PurchaseOrderHelper(httpClient, okapiHeadersMock, ctxMock, "en"
      , poNumberHelper, orderLineHelper, financeHelper));
    CompositePurchaseOrder order = getMockAsJson(ORDER_PATH).mapTo(CompositePurchaseOrder.class);

    JsonArray encumbrances = getMockAsJson(TWO_ENCUMBRANCE_PATH).getJsonArray("transactions");
    Transaction encumbrance1 = encumbrances.getJsonObject(0).mapTo(Transaction.class);
    Transaction encumbrance2 = encumbrances.getJsonObject(1).mapTo(Transaction.class);
    List<EncumbranceRelationsHolder> holders = new ArrayList<>();

    doReturn(completedFuture(null)).when(serviceSpy).createEncumbrancesAndUpdatePoLines(any(List.class));
    doReturn(completedFuture(Arrays.asList(encumbrance1, encumbrance2))).when(financeHelper).getOrderEncumbrances(any());
    doReturn(completedFuture(null)).when(financeHelper).updateTransactions(any());
    doReturn(completedFuture(null)).when(financeHelper).releaseEncumbrances(any());
    doReturn(completedFuture(null)).when(financeHelper).updateOrderTransactionSummary(anyString(), anyInt());


    doNothing().when(financeHelper).closeHttpClient();
    doNothing().when(orderLineHelper).closeHttpClient();
    doReturn(completedFuture(null)).when(orderLineHelper).updatePoLinesSummary(any());
    //When
    EncumbrancesProcessingHolder holder = serviceSpy.processEncumbrances(order).join();
    //Then

    assertEquals(1, holder.getEncumbrancesForCreate().size());
    holder.getEncumbrancesForCreate().forEach(encumbranceC -> {
      assertEquals("55f48dc6-efa7-4cfe-bc7c-4786efe493e3", encumbranceC.getTransaction().getFromFundId());
      assertEquals(Encumbrance.Status.UNRELEASED, encumbranceC.getTransaction().getEncumbrance().getStatus());
      assertTrue(encumbranceC.getTransaction().getEncumbrance().getInitialAmountEncumbered() > 0);
      assertTrue(encumbranceC.getTransaction().getAmount() > 0);
    });
    assertEquals(2, holder.getEncumbrancesForUpdate().size());
    holder.getEncumbrancesForUpdate().forEach(encumbranceU -> {
      assertEquals(Encumbrance.Status.UNRELEASED, encumbranceU.getTransaction().getEncumbrance().getStatus());
      assertTrue(encumbranceU.getTransaction().getEncumbrance().getInitialAmountEncumbered() > 0);
      assertTrue(encumbranceU.getTransaction().getAmount() > 0);
    });
    assertEquals(0, holder.getEncumbrancesForRelease().size());
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
