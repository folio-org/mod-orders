package org.folio.rest.impl;

import io.restassured.RestAssured;
import io.restassured.response.Response;
import io.vertx.core.logging.Logger;
import io.vertx.core.logging.LoggerFactory;
import org.folio.rest.acq.model.ReceivingHistoryCollection;
import org.junit.Test;

import static org.folio.rest.impl.PurchaseOrdersApiApiTest.BAD_REQUEST;
import static org.folio.rest.impl.MockServer.INTERNAL_SERVER_ERROR;
import static org.junit.Assert.assertEquals;

public class ReceivingHistoryApiApiTest extends ApiTestBase {

  private static final Logger logger = LoggerFactory.getLogger(ReceivingHistoryApiApiTest.class);

  static final String RECEIVING_HISTORY_PURCHASE_ORDER_ID = "0804ddec-6545-404a-b54d-a693f505681d";
  private static final String ORDERS_RECEIVING_HISTORY_ENDPOINT = "/orders/receiving-history";

  @Test
  public void testGetReceivingHistory() {
    logger.info("=== Test Get Receiving History - With empty query ===");

    final Response resp = RestAssured
      .with()
        .header(X_OKAPI_URL)
        .header(EXIST_CONFIG_X_OKAPI_TENANT_LIMIT_10)
      .get(ORDERS_RECEIVING_HISTORY_ENDPOINT)
        .then()
          .statusCode(200)
          .extract()
            .response();

    assertEquals(0, resp.getBody().as(ReceivingHistoryCollection.class).getTotalRecords().intValue());
  }

  @Test
  public void testGetReceivingHistoryForPurchaseOrder() {
    logger.info("=== Test Get Receiving History - With purchase Order query ===");
    String endpointQuery = String.format("%s?query=purchaseOrderId=%s", ORDERS_RECEIVING_HISTORY_ENDPOINT, RECEIVING_HISTORY_PURCHASE_ORDER_ID);

    final Response resp = RestAssured
      .with()
        .header(X_OKAPI_URL)
        .header(EXIST_CONFIG_X_OKAPI_TENANT_LIMIT_10)
      .get(endpointQuery)
        .then()
          .statusCode(200)
          .extract()
            .response();

    assertEquals(1, resp.getBody().as(ReceivingHistoryCollection.class).getTotalRecords().intValue());
  }

  @Test
  public void testGetReceivingHistoryForPurchaseOrderWithError() {
    logger.info("=== Test Get Receiving History - With purchase Order query Error===");
    String endpointQuery = String.format("%s?query=purchaseOrderId=%s", ORDERS_RECEIVING_HISTORY_ENDPOINT, INTERNAL_SERVER_ERROR);

    RestAssured
      .with()
        .header(X_OKAPI_URL)
        .header(EXIST_CONFIG_X_OKAPI_TENANT_LIMIT_10)
      .get(endpointQuery)
        .then()
          .statusCode(500)
          .extract()
            .response();

  }

  @Test
  public void testGetReceivingHistoryBadRequest() {
    logger.info("=== Test Get Receiving History - With Bad Request");

    RestAssured
      .with()
        .header(X_OKAPI_URL)
        .header(EXIST_CONFIG_X_OKAPI_TENANT_LIMIT_10)
      .get(ORDERS_RECEIVING_HISTORY_ENDPOINT+"?query="+BAD_REQUEST)
        .then()
          .statusCode(400)
          .extract()
            .response();

  }
}
