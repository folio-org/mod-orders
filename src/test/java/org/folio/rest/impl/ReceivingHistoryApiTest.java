package org.folio.rest.impl;

import io.vertx.core.logging.Logger;
import io.vertx.core.logging.LoggerFactory;
import org.folio.rest.acq.model.ReceivingHistoryCollection;
import org.junit.Test;

import static javax.ws.rs.core.MediaType.APPLICATION_JSON;
import static org.folio.rest.impl.MockServer.INTERNAL_SERVER_ERROR;
import static org.junit.Assert.assertEquals;

public class ReceivingHistoryApiTest extends ApiTestBase {

  private static final Logger logger = LoggerFactory.getLogger(ReceivingHistoryApiTest.class);

  static final String RECEIVING_HISTORY_PURCHASE_ORDER_ID = "0804ddec-6545-404a-b54d-a693f505681d";
  private static final String ORDERS_RECEIVING_HISTORY_ENDPOINT = "/orders/receiving-history";

  @Test
  public void testGetReceivingHistory() {
    logger.info("=== Test Get Receiving History - With empty query ===");
    final ReceivingHistoryCollection receivingHistory = verifySuccessGet(ORDERS_RECEIVING_HISTORY_ENDPOINT, ReceivingHistoryCollection.class);

    assertEquals(0, receivingHistory.getTotalRecords().intValue());
  }

  @Test
  public void testGetReceivingHistoryForPurchaseOrder() {
    logger.info("=== Test Get Receiving History - With purchase Order query ===");
    String endpointQuery = String.format("%s?query=purchaseOrderId=%s", ORDERS_RECEIVING_HISTORY_ENDPOINT, RECEIVING_HISTORY_PURCHASE_ORDER_ID);

    final ReceivingHistoryCollection receivingHistory = verifySuccessGet(endpointQuery, ReceivingHistoryCollection.class);

    assertEquals(1, receivingHistory.getTotalRecords().intValue());
  }

  @Test
  public void testGetReceivingHistoryForPurchaseOrderWithError() {
    logger.info("=== Test Get Receiving History - With purchase Order query Error===");
    String endpointQuery = String.format("%s?query=purchaseOrderId=%s", ORDERS_RECEIVING_HISTORY_ENDPOINT, INTERNAL_SERVER_ERROR);

    verifyGet(endpointQuery, APPLICATION_JSON, 500);

  }

  @Test
  public void testGetReceivingHistoryBadRequest() {
    logger.info("=== Test Get Receiving History - With Bad Request");

    verifyGet(ORDERS_RECEIVING_HISTORY_ENDPOINT+"?query="+BAD_QUERY, APPLICATION_JSON, 400);

  }
}
