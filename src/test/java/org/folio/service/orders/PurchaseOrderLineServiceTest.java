package org.folio.service.orders;

import static org.folio.TestConfig.clearServiceInteractions;
import static org.folio.TestConfig.getFirstContextFromVertx;
import static org.folio.TestConfig.getVertx;
import static org.folio.TestConfig.initSpringContext;
import static org.folio.TestConfig.isVerticleNotDeployed;
import static org.folio.TestConfig.mockPort;
import static org.folio.TestConstants.X_OKAPI_TOKEN;
import static org.folio.TestConstants.X_OKAPI_USER_ID;
import static org.folio.rest.RestConstants.OKAPI_URL;
import static org.folio.rest.impl.PurchaseOrdersApiTest.X_OKAPI_TENANT;
import static org.junit.jupiter.api.Assertions.assertEquals;
import static org.mockito.ArgumentMatchers.any;
import static org.mockito.ArgumentMatchers.eq;
import static org.mockito.Mockito.verify;
import static org.mockito.Mockito.when;

import java.util.Collections;
import java.util.HashMap;
import java.util.List;
import java.util.Map;
import java.util.UUID;
import java.util.concurrent.ExecutionException;
import java.util.concurrent.TimeoutException;

import org.folio.ApiTestSuite;
import org.folio.config.ApplicationConfig;
import org.folio.rest.core.RestClient;
import org.folio.rest.core.models.RequestContext;
import org.folio.rest.core.models.RequestEntry;
import org.folio.rest.jaxrs.model.PoLine;
import org.folio.rest.jaxrs.model.PoLineCollection;
import org.junit.jupiter.api.AfterAll;
import org.junit.jupiter.api.AfterEach;
import org.junit.jupiter.api.BeforeAll;
import org.junit.jupiter.api.BeforeEach;
import org.junit.jupiter.api.Test;
import org.junit.jupiter.api.extension.ExtendWith;
import org.mockito.InjectMocks;
import org.mockito.Mock;
import org.mockito.MockitoAnnotations;

import io.vertx.core.Context;
import io.vertx.core.Future;
import io.vertx.junit5.VertxExtension;
import io.vertx.junit5.VertxTestContext;

@ExtendWith(VertxExtension.class)
public class PurchaseOrderLineServiceTest {
  @InjectMocks
  private PurchaseOrderLineService purchaseOrderLineService;
  @Mock
  private RestClient restClientMock;
  @Mock
  private Context ctxMock;

  private Map<String, String> okapiHeadersMock;
  private RequestContext requestContext;
  private static boolean runningOnOwn;

  @BeforeAll
  static void before() throws InterruptedException, ExecutionException, TimeoutException {
    if (isVerticleNotDeployed()) {
      ApiTestSuite.before();
      runningOnOwn = true;
    }
    initSpringContext(ApplicationConfig.class);
  }

  @BeforeEach
  public void initMocks() {
    MockitoAnnotations.openMocks(this);
    ctxMock = getFirstContextFromVertx(getVertx());
    okapiHeadersMock = new HashMap<>();
    okapiHeadersMock.put(OKAPI_URL, "http://localhost:" + mockPort);
    okapiHeadersMock.put(X_OKAPI_TOKEN.getName(), X_OKAPI_TOKEN.getValue());
    okapiHeadersMock.put(X_OKAPI_TENANT.getName(), X_OKAPI_TENANT.getValue());
    okapiHeadersMock.put(X_OKAPI_USER_ID.getName(), X_OKAPI_USER_ID.getValue());
    requestContext = new RequestContext(ctxMock, okapiHeadersMock);

  }

  @AfterEach
  void afterEach() {
    clearServiceInteractions();
  }

  @AfterAll
  static void after() {
    if (runningOnOwn) {
      ApiTestSuite.after();
    }
  }

  @Test
  void successRetrievePurchaseOrderLinesByQuery(VertxTestContext vertxTestContext) {
    String orderLineId = UUID.randomUUID().toString();
    List<PoLine> purchaseOrderLines = Collections.singletonList(new PoLine()
      .withId(orderLineId));

    PoLineCollection expLines = new PoLineCollection()
      .withPoLines(purchaseOrderLines)
      .withTotalRecords(1);

    when(restClientMock.get(any(RequestEntry.class), any(), any())).thenReturn(Future.succeededFuture(expLines));

    String expectedQuery =  String.format("id==%s", orderLineId);
    var future = purchaseOrderLineService.getOrderLines(expectedQuery,  0, Integer.MAX_VALUE, requestContext);
    vertxTestContext.assertComplete(future)
      .onComplete(result -> {
        verify(restClientMock).get(any(RequestEntry.class), eq(PoLineCollection.class), eq(requestContext));
        assertEquals(purchaseOrderLines, result.result());
        vertxTestContext.completeNow();
      });

  }


  @Test
  void successUpdateSinglePurchaseOrderLine(VertxTestContext vertxTestContext) {
    String orderLineId = UUID.randomUUID().toString();
    PoLine purchaseOrderLine = new PoLine().withId(orderLineId);

    when(restClientMock.put(any(RequestEntry.class), any(PoLine.class), eq(requestContext))).thenReturn(Future.succeededFuture(null));

    var future = purchaseOrderLineService.saveOrderLine(purchaseOrderLine, requestContext);
    vertxTestContext.assertComplete(future)
      .onComplete(result -> {
        verify(restClientMock).put(any(RequestEntry.class), eq(purchaseOrderLine), eq(requestContext));
        vertxTestContext.completeNow();
      });
  }

  @Test
  void successUpdatePurchaseOrderLines(VertxTestContext vertxTestContext) {
    String orderLineId1 = UUID.randomUUID().toString();
    String orderLineId2 = UUID.randomUUID().toString();
    List<PoLine> purchaseOrderLines = List.of(new PoLine().withId(orderLineId1), new PoLine().withId(orderLineId2));
    PoLineCollection poLineCollection = new PoLineCollection().withPoLines(purchaseOrderLines).withTotalRecords(purchaseOrderLines.size());
    when(restClientMock.put(any(RequestEntry.class), any(PoLineCollection.class), eq(requestContext))).thenReturn(Future.succeededFuture(null));

    var future = purchaseOrderLineService.saveOrderLines(purchaseOrderLines, requestContext);
    vertxTestContext.assertComplete(future)
      .onComplete(result -> {
        verify(restClientMock).put(any(RequestEntry.class), eq(poLineCollection), eq(requestContext));
        vertxTestContext.completeNow();
      });

  }
}
