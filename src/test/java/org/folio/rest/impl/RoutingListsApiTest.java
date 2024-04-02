package org.folio.rest.impl;

import static io.vertx.core.Future.succeededFuture;
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
import static org.folio.TestConstants.ROUTING_LIST_ID;
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
import java.util.concurrent.ExecutionException;
import java.util.concurrent.TimeoutException;

import io.vertx.core.Context;
import io.vertx.core.json.JsonObject;
import org.apache.logging.log4j.LogManager;
import org.apache.logging.log4j.Logger;
import org.folio.ApiTestSuite;
import org.folio.rest.core.models.RequestContext;
import org.folio.service.RoutingListsService;
import org.junit.jupiter.api.AfterAll;
import org.junit.jupiter.api.AfterEach;
import org.junit.jupiter.api.BeforeAll;
import org.junit.jupiter.api.BeforeEach;
import org.junit.jupiter.api.Test;
import org.mockito.MockitoAnnotations;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.context.annotation.Bean;

public class RoutingListsApiTest {

  private static final Logger logger = LogManager.getLogger();
  private static final String TEMPLATE_PROCESSING_REQUEST_ENDPOINT = "orders/routing-lists/" + ROUTING_LIST_ID + "/template";
  private static boolean runningOnOwn;
  @Autowired
  private RoutingListsService routingListsService;
  private RequestContext requestContext;
  private Context ctxMock;
  private Map<String, String> okapiHeadersMock;
  private AutoCloseable mockitoMocks;


  @BeforeAll
  static void before() throws InterruptedException, ExecutionException, TimeoutException {
    if (isVerticleNotDeployed()) {
      ApiTestSuite.before();
      runningOnOwn = true;
    }
    initSpringContext(RoutingListsApiTest.ContextConfiguration.class);
  }


  @BeforeEach
  void beforeEach() {
    mockitoMocks = MockitoAnnotations.openMocks(this);
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
  void afterEach() throws Exception {
    mockitoMocks.close();
    clearServiceInteractions();
  }

  @AfterAll
  static void after() {
    if (runningOnOwn) {
      ApiTestSuite.after();
    }
  }

  @Test
  void testProcessTemplateRequest() {
    logger.info("=== Test Execute template processing request ===");

    doReturn(succeededFuture(new JsonObject())).when(routingListsService).processTemplateRequest(eq(ROUTING_LIST_ID), any(RequestContext.class));

    verifyGet(TEMPLATE_PROCESSING_REQUEST_ENDPOINT, prepareHeaders(X_OKAPI_URL, EMPTY_CONFIG_X_OKAPI_TENANT),
      APPLICATION_JSON, 200);

    verify(routingListsService, times(1)).processTemplateRequest(eq(ROUTING_LIST_ID), any(RequestContext.class));
  }

  static class ContextConfiguration {
    @Bean
    public RoutingListsService routingListsService() {
      return mock(RoutingListsService.class);
    }
  }
}
