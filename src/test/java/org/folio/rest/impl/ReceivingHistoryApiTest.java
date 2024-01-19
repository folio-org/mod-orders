package org.folio.rest.impl;

import static javax.ws.rs.core.MediaType.APPLICATION_JSON;
import static javax.ws.rs.core.MediaType.TEXT_PLAIN;
import static javax.ws.rs.core.Response.Status.INTERNAL_SERVER_ERROR;
import static org.folio.RestTestUtils.prepareHeaders;
import static org.folio.RestTestUtils.verifyGet;
import static org.folio.RestTestUtils.verifySuccessGet;
import static org.folio.TestConfig.X_OKAPI_URL;
import static org.folio.TestConfig.clearServiceInteractions;
import static org.folio.TestConfig.initSpringContext;
import static org.folio.TestConfig.isVerticleNotDeployed;
import static org.folio.TestConstants.BAD_QUERY;
import static org.folio.TestConstants.NON_EXIST_CONFIG_X_OKAPI_TENANT;
import static org.folio.TestConstants.PROTECTED_READ_ONLY_TENANT;
import static org.folio.TestConstants.X_OKAPI_USER_ID_WITH_ACQ_UNITS;
import static org.folio.orders.utils.ResourcePathResolver.ACQUISITIONS_MEMBERSHIPS;
import static org.folio.orders.utils.ResourcePathResolver.ACQUISITIONS_UNITS;
import static org.folio.orders.utils.ResourcePathResolver.RECEIVING_HISTORY;
import static org.folio.rest.impl.MockServer.getQueryParams;
import static org.hamcrest.MatcherAssert.assertThat;
import static org.hamcrest.Matchers.containsString;
import static org.hamcrest.Matchers.equalTo;
import static org.hamcrest.Matchers.hasSize;
import static org.hamcrest.Matchers.is;
import static org.hamcrest.Matchers.not;

import java.util.List;
import java.util.concurrent.ExecutionException;
import java.util.concurrent.TimeoutException;

import org.apache.logging.log4j.LogManager;
import org.apache.logging.log4j.Logger;
import org.folio.ApiTestSuite;
import org.folio.config.ApplicationConfig;
import org.folio.rest.acq.model.ReceivingHistoryCollection;
import org.folio.rest.jaxrs.model.AcquisitionsUnitMembershipCollection;
import org.junit.jupiter.api.AfterAll;
import org.junit.jupiter.api.AfterEach;
import org.junit.jupiter.api.BeforeAll;
import org.junit.jupiter.api.BeforeEach;
import org.junit.jupiter.api.Test;
import org.mockito.MockitoAnnotations;

import io.restassured.http.Headers;
import io.vertx.core.http.HttpMethod;

public class ReceivingHistoryApiTest {

  private static final Logger logger = LogManager.getLogger();

  static final String RECEIVING_HISTORY_PURCHASE_ORDER_ID = "0804ddec-6545-404a-b54d-a693f505681d";
  private static final String ORDERS_RECEIVING_HISTORY_ENDPOINT = "/orders/receiving-history";
  private static final String ACQUISITIONS_UNIT_IDS = "acqUnitIds";
  private static final String NO_ACQ_UNIT_ASSIGNED_CQL = "cql.allRecords=1 not " + ACQUISITIONS_UNIT_IDS + " <> []";

  private static boolean runningOnOwn;

  @BeforeAll
  static void before() throws InterruptedException, ExecutionException, TimeoutException {
    if (isVerticleNotDeployed()) {
      ApiTestSuite.before();
      runningOnOwn = true;
    }
    initSpringContext(ApplicationConfig.class);
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

  @BeforeEach
  void initMocks(){
    MockitoAnnotations.openMocks(this);
  }

  @Test
  void testGetReceivingHistory() {
    logger.info("=== Test Get Receiving History - With empty query ===");
    final ReceivingHistoryCollection receivingHistory = verifySuccessGet(ORDERS_RECEIVING_HISTORY_ENDPOINT, ReceivingHistoryCollection.class, PROTECTED_READ_ONLY_TENANT);

    assertThat(receivingHistory.getTotalRecords(), is(0));

    assertThat(MockServer.serverRqRs.get(RECEIVING_HISTORY, HttpMethod.GET), hasSize(1));
    assertThat(MockServer.serverRqRs.get(ACQUISITIONS_UNITS, HttpMethod.GET), hasSize(1));
    assertThat(MockServer.serverRqRs.get(ACQUISITIONS_MEMBERSHIPS, HttpMethod.GET), hasSize(1));

    List<String> queryParams = getQueryParams(RECEIVING_HISTORY);
    assertThat(queryParams, hasSize(1));
    assertThat(queryParams.get(0), equalTo(NO_ACQ_UNIT_ASSIGNED_CQL));
  }

  @Test
  void testGetReceivingHistoryForPurchaseOrder() {
    logger.info("=== Test Get Receiving History - With purchase Order query ===");
    String endpointQuery = String.format("%s?query=purchaseOrderId=%s", ORDERS_RECEIVING_HISTORY_ENDPOINT, RECEIVING_HISTORY_PURCHASE_ORDER_ID);

    final ReceivingHistoryCollection receivingHistory = verifySuccessGet(endpointQuery, ReceivingHistoryCollection.class, PROTECTED_READ_ONLY_TENANT);

    assertThat(receivingHistory.getTotalRecords(), is(1));

    assertThat(MockServer.serverRqRs.get(RECEIVING_HISTORY, HttpMethod.GET), hasSize(1));
    assertThat(MockServer.serverRqRs.get(ACQUISITIONS_UNITS, HttpMethod.GET), hasSize(1));
    assertThat(MockServer.serverRqRs.get(ACQUISITIONS_MEMBERSHIPS, HttpMethod.GET), hasSize(1));

    List<String> queryParams = getQueryParams(RECEIVING_HISTORY);
    assertThat(queryParams, hasSize(1));
    String queryToStorage = queryParams.get(0);
    assertThat(queryToStorage, containsString(RECEIVING_HISTORY_PURCHASE_ORDER_ID));
    assertThat(queryToStorage, not(containsString(ACQUISITIONS_UNIT_IDS + "=")));
    assertThat(queryToStorage, containsString(NO_ACQ_UNIT_ASSIGNED_CQL));
  }

  @Test
  void testGetReceivingHistoryForUserAssignedToAcqUnits() {
    logger.info("=== Test Get Receiving History - user assigned to acq units ===");
    String endpointQuery = String.format("%s?query=purchaseOrderId=%s", ORDERS_RECEIVING_HISTORY_ENDPOINT, RECEIVING_HISTORY_PURCHASE_ORDER_ID);

    Headers headers = prepareHeaders(X_OKAPI_URL, NON_EXIST_CONFIG_X_OKAPI_TENANT, X_OKAPI_USER_ID_WITH_ACQ_UNITS);
    verifyGet(endpointQuery, headers, APPLICATION_JSON, 200);

    assertThat(MockServer.serverRqRs.get(RECEIVING_HISTORY, HttpMethod.GET), hasSize(1));
    assertThat(MockServer.serverRqRs.get(ACQUISITIONS_UNITS, HttpMethod.GET), hasSize(1));
    assertThat(MockServer.serverRqRs.get(ACQUISITIONS_MEMBERSHIPS, HttpMethod.GET), hasSize(1));

    List<String> queryParams = getQueryParams(RECEIVING_HISTORY);
    assertThat(queryParams, hasSize(1));
    String queryToStorage = queryParams.get(0);
    assertThat(queryToStorage, containsString(ACQUISITIONS_UNIT_IDS + "="));
    assertThat(queryToStorage, containsString(NO_ACQ_UNIT_ASSIGNED_CQL));

    MockServer.serverRqRs.get(ACQUISITIONS_MEMBERSHIPS, HttpMethod.GET)
      .get(0)
      .mapTo(AcquisitionsUnitMembershipCollection.class)
      .getAcquisitionsUnitMemberships()
      .forEach(member -> assertThat(queryToStorage, containsString(member.getAcquisitionsUnitId())));
  }

  @Test
  void testGetReceivingHistoryForPurchaseOrderWithError() {
    logger.info("=== Test Get Receiving History - With purchase Order query Error===");
    String endpointQuery = String.format("%s?query=purchaseOrderId=%s", ORDERS_RECEIVING_HISTORY_ENDPOINT, INTERNAL_SERVER_ERROR.getReasonPhrase());

    verifyGet(endpointQuery, APPLICATION_JSON, 500);

  }

  @Test
  void testGetReceivingHistoryBadRequest() {
    logger.info("=== Test Get Receiving History - With Bad Request");
    verifyGet(ORDERS_RECEIVING_HISTORY_ENDPOINT+"?query=" + BAD_QUERY, APPLICATION_JSON, 400);

  }
}
