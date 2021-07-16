package org.folio.rest.impl;

import static java.util.concurrent.CompletableFuture.completedFuture;
import static javax.ws.rs.core.MediaType.APPLICATION_JSON;
import static org.folio.RestTestUtils.prepareHeaders;
import static org.folio.RestTestUtils.verifyGet;
import static org.folio.TestConfig.X_OKAPI_URL;
import static org.folio.TestConfig.autowireDependencies;
import static org.folio.TestConfig.clearServiceInteractions;
import static org.folio.TestConfig.getFirstContextFromVertx;
import static org.folio.TestConfig.getVertx;
import static org.folio.TestConfig.initSpringContext;
import static org.folio.TestConfig.isVerticleNotDeployed;
import static org.folio.TestConfig.mockPort;
import static org.folio.TestConstants.EMPTY_CONFIG_X_OKAPI_TENANT;
import static org.folio.TestConstants.X_OKAPI_TOKEN;
import static org.folio.TestConstants.X_OKAPI_USER_ID;
import static org.folio.rest.RestConstants.OKAPI_URL;
import static org.folio.rest.impl.PurchaseOrdersApiTest.X_OKAPI_TENANT;
import static org.mockito.ArgumentMatchers.any;
import static org.mockito.ArgumentMatchers.eq;
import static org.mockito.Mockito.doReturn;
import static org.mockito.Mockito.mock;
import static org.mockito.Mockito.times;
import static org.mockito.Mockito.verify;

import java.util.HashMap;
import java.util.Map;
import java.util.UUID;
import java.util.concurrent.ExecutionException;
import java.util.concurrent.TimeoutException;

import org.folio.ApiTestSuite;
import org.folio.rest.core.models.RequestContext;
import org.folio.rest.jaxrs.model.HoldingSummaryCollection;
import org.folio.service.orders.HoldingsSummaryService;
import org.junit.jupiter.api.AfterAll;
import org.junit.jupiter.api.AfterEach;
import org.junit.jupiter.api.BeforeAll;
import org.junit.jupiter.api.BeforeEach;
import org.junit.jupiter.api.Test;
import org.mockito.MockitoAnnotations;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.context.annotation.Bean;

import io.vertx.core.Context;

public class HoldingsSummaryAPITest {

  public static String HOLDING_SUMMARY_ENDPOINT = "/orders/holding-summary/%s";

  private static boolean runningOnOwn;

  @Autowired
  private HoldingsSummaryService holdingsSummaryService;
  private RequestContext requestContext;
  private Context ctxMock;
  private Map<String, String> okapiHeadersMock;


  @BeforeAll
  static void before() throws InterruptedException, ExecutionException, TimeoutException {
    if (isVerticleNotDeployed()) {
      ApiTestSuite.before();
      runningOnOwn = true;
    }
    initSpringContext(HoldingsSummaryAPITest.ContextConfiguration.class);
  }


  @BeforeEach
  void beforeEach() {
    MockitoAnnotations.openMocks(this);
    autowireDependencies(this);
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
  void shouldSuccessReturnHoldingSummary() {
    String holdingId = UUID.randomUUID().toString();
    String endpoint = String.format(HOLDING_SUMMARY_ENDPOINT, holdingId);
    HoldingSummaryCollection summaryCollection = new HoldingSummaryCollection().withTotalRecords(0);
    doReturn(completedFuture(summaryCollection)).when(holdingsSummaryService).getHoldingsSummary(eq(holdingId), any(RequestContext.class));

    verifyGet(endpoint, prepareHeaders(X_OKAPI_URL, EMPTY_CONFIG_X_OKAPI_TENANT), APPLICATION_JSON, 200);

    verify(holdingsSummaryService, times(1)).getHoldingsSummary(eq(holdingId), any(RequestContext.class));
  }

  /**
   * Define unit test specific beans to override actual ones
   */
  static class ContextConfiguration {
    @Bean
    public HoldingsSummaryService holdingsSummaryService() {
      return mock(HoldingsSummaryService.class);
    }
  }
}
