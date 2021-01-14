package org.folio.helper;

import static java.util.concurrent.CompletableFuture.completedFuture;
import static org.folio.ApiTestSuite.mockPort;
import static org.folio.rest.RestConstants.OKAPI_URL;
import static org.folio.rest.impl.MockServer.BASE_MOCK_DATA_PATH;
import static org.folio.rest.impl.MockServer.ENCUMBRANCE_PATH;
import static org.folio.rest.impl.PurchaseOrdersApiTest.X_OKAPI_TENANT;
import static org.junit.jupiter.api.Assertions.assertEquals;
import static org.junit.jupiter.api.Assertions.assertFalse;
import static org.junit.jupiter.api.Assertions.assertTrue;
import static org.mockito.ArgumentMatchers.any;
import static org.mockito.ArgumentMatchers.anyInt;
import static org.mockito.ArgumentMatchers.anyString;
import static org.mockito.ArgumentMatchers.eq;
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

import org.folio.rest.acq.model.finance.Encumbrance;
import org.folio.rest.acq.model.finance.FiscalYear;
import org.folio.rest.acq.model.finance.Fund;
import org.folio.rest.acq.model.finance.LedgerFiscalYearRolloverError;
import org.folio.rest.acq.model.finance.LedgerFiscalYearRolloverErrorCollection;
import org.folio.rest.acq.model.finance.Transaction;
import org.folio.rest.core.models.RequestContext;
import org.folio.rest.impl.ApiTestBase;
import org.folio.rest.jaxrs.model.CompositePurchaseOrder;
import org.folio.rest.jaxrs.model.LedgerFiscalYearRollover;
import org.folio.rest.jaxrs.model.LedgerFiscalYearRolloverCollection;
import org.folio.rest.tools.client.HttpClientFactory;
import org.folio.rest.tools.client.interfaces.HttpClientInterface;
import org.folio.service.finance.EncumbranceService;
import org.folio.service.finance.EncumbranceWorkflowStrategyFactory;
import org.folio.service.finance.FundService;
import org.folio.service.finance.OpenToPendingEncumbranceStrategy;
import org.junit.jupiter.api.BeforeEach;
import org.junit.jupiter.api.Test;
import org.mockito.Mock;

import io.vertx.core.Context;
import io.vertx.core.Vertx;

public class PurchaseOrderHelperTest  extends ApiTestBase {
  private static final String ORDER_ID = "1ab7ef6a-d1d4-4a4f-90a2-882aed18af20";
  public static final String ORDER_PATH = BASE_MOCK_DATA_PATH + "compositeOrders/" + ORDER_ID + ".json";
  public static final String TWO_ENCUMBRANCE_PATH = BASE_MOCK_DATA_PATH + "encumbrances/two_pending_encumbrances.json";

  @Mock
  private PoNumberHelper poNumberHelper;
  @Mock
  private EncumbranceWorkflowStrategyFactory encumbranceWorkflowStrategyFactory;
  @Mock
  private OpenToPendingEncumbranceStrategy openToPendingEncumbranceStrategy;

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
  void testShouldSetEncumbrancesToPending() {
    //given
    PurchaseOrderLineHelper orderLineHelper = mock(PurchaseOrderLineHelper.class, CALLS_REAL_METHODS);
    EncumbranceService encumbranceService = mock(EncumbranceService.class, CALLS_REAL_METHODS);
    PurchaseOrderHelper serviceSpy = spy(
        new PurchaseOrderHelper(httpClient, okapiHeadersMock, ctxMock, "en", poNumberHelper, orderLineHelper, encumbranceService));
    CompositePurchaseOrder order = getMockAsJson(ORDER_PATH).mapTo(CompositePurchaseOrder.class);

    Transaction encumbrance = getMockAsJson(ENCUMBRANCE_PATH).getJsonArray("transactions").getJsonObject(0).mapTo(Transaction.class);

    doReturn(completedFuture(order)).when(serviceSpy).updateAndGetOrderWithLines(any(CompositePurchaseOrder.class));
    doReturn(openToPendingEncumbranceStrategy).when(encumbranceWorkflowStrategyFactory).getStrategy(any());
    doReturn(completedFuture(null)).when(openToPendingEncumbranceStrategy).processEncumbrances(any(), any());
    doNothing().when(orderLineHelper).closeHttpClient();
    doReturn(completedFuture(null)).when(orderLineHelper).updatePoLinesSummary(any());
    //When
    serviceSpy.unOpenOrder(order).join();
    //Then
    assertEquals(0d, encumbrance.getAmount(), 0.0);
    assertEquals(0d, encumbrance.getEncumbrance().getInitialAmountEncumbered(), 0.0);
    assertEquals(Encumbrance.Status.PENDING, encumbrance.getEncumbrance().getStatus());

    verify(orderLineHelper).updatePoLinesSummary(any());
  }

  @Test
  void testShouldEndExceptionallyAndCloseConnection() {
    //given
    PurchaseOrderLineHelper orderLineHelper = mock(PurchaseOrderLineHelper.class, CALLS_REAL_METHODS);
    EncumbranceService encumbranceService = mock(EncumbranceService.class, CALLS_REAL_METHODS);

    PurchaseOrderHelper serviceSpy = spy(new PurchaseOrderHelper(httpClient, okapiHeadersMock, ctxMock, "en", poNumberHelper,
        orderLineHelper, encumbranceService));
    CompositePurchaseOrder order = getMockAsJson(ORDER_PATH).mapTo(CompositePurchaseOrder.class);

    doReturn(completedFuture(order)).when(serviceSpy).updateAndGetOrderWithLines(any(CompositePurchaseOrder.class));
    doReturn(openToPendingEncumbranceStrategy).when(encumbranceWorkflowStrategyFactory).getStrategy(any());
    doReturn(completedFuture(null)).when(openToPendingEncumbranceStrategy).processEncumbrances(any(), any());

    doNothing().when(orderLineHelper).closeHttpClient();
    //When
    CompletableFuture<Void> act = serviceSpy.unOpenOrder(order);
    //Then
    assertTrue(act.isCompletedExceptionally());
  }


}
