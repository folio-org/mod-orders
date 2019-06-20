package org.folio.orders.events.handlers;

import static org.folio.rest.impl.MockServer.getPoLineSearches;
import static org.folio.rest.impl.MockServer.getPoLineUpdates;
import static org.folio.rest.impl.MockServer.PIECE_RECORDS_MOCK_DATA_PATH;
import static org.folio.rest.impl.MockServer.getPieceSearches;

import java.io.IOException;
import java.util.concurrent.ExecutionException;
import java.util.concurrent.TimeoutException;

import static org.hamcrest.MatcherAssert.assertThat;
import static org.hamcrest.Matchers.equalTo;
import static org.hamcrest.Matchers.hasSize;
import static org.hamcrest.Matchers.is;
import static org.junit.Assert.assertEquals;
//import static org.junit.Assert.assertThat;
import static org.junit.Assert.assertTrue;
import static org.junit.Assert.fail;
import static org.hamcrest.Matchers.nullValue;
import static org.hamcrest.Matchers.instanceOf;

import org.folio.rest.jaxrs.model.Piece;
import org.folio.rest.jaxrs.model.PoLine.ReceiptStatus;
import org.folio.rest.acq.model.PoLine;
import org.folio.rest.acq.model.Piece.ReceivingStatus;
import org.folio.rest.impl.ApiTestBase;
import org.junit.BeforeClass;
import org.junit.Test;
import org.junit.runner.RunWith;

import javax.ws.rs.core.Response;
import io.vertx.core.AsyncResult;
import io.vertx.core.Handler;
import io.vertx.core.Vertx;
import io.vertx.core.eventbus.DeliveryOptions;
import io.vertx.core.eventbus.Message;
import io.vertx.core.eventbus.ReplyException;
import io.vertx.core.json.JsonObject;
import io.vertx.core.logging.Logger;
import io.vertx.core.logging.LoggerFactory;
import io.vertx.ext.unit.TestContext;
import io.vertx.ext.unit.junit.VertxUnitRunner;

@RunWith(VertxUnitRunner.class)
public class ReceiptStatusConsistencyTest extends ApiTestBase {
  private static final Logger logger = LoggerFactory.getLogger(ReceiptStatusConsistencyTest.class);

  private static final String TEST_ADDRESS = "testReceiptStatusAddress";
  private static final String VALID_PIECE_ID = "af372ac8-5ffb-4560-8b96-3945a12e121b";
  private static final String PIECE_ID_WITH_NO_PO_LINE = "5b454292-6aaa-474f-9510-b59a564e0c8d";

  private static Vertx vertx;

  @BeforeClass
  public static void before() throws InterruptedException, ExecutionException, TimeoutException {
    ApiTestBase.before();

    vertx = Vertx.vertx();
    vertx.eventBus().consumer(TEST_ADDRESS, new ReceiptStatusConsistency(vertx));
  }

  @Test
  public void testPieceReceiptStatusIsNotConsistent(TestContext context) {
    logger.info("=== Test case when piece receipt status changes from Received to Expected ===");
    
    // explicitly setting RECEIVED to create inconsistency
    sendEvent(createBody("RECEIVED", VALID_PIECE_ID), context.asyncAssertSuccess(result -> {
      logger.info("getPoLineSearches()--->" + getPoLineSearches());
      logger.info("getPoLineUpdates()--->" + getPoLineUpdates());
      logger.info("getPieceSearches()--->" + getPieceSearches());
      //assertThat(getPoLineUpdates(), hasSize(1));
      assertEquals(getPieceSearches().get(0).getJsonArray("pieces").size(), 2);
      
      Piece piece0 = getPieceSearches().get(0).getJsonArray("pieces").getJsonObject(0).mapTo(Piece.class);
      Piece piece1 = getPieceSearches().get(0).getJsonArray("pieces").getJsonObject(1).mapTo(Piece.class);
      assertEquals(piece0.getReceivingStatus().toString().toLowerCase(),ReceivingStatus.EXPECTED.value().toLowerCase());
      assertEquals(piece1.getReceivingStatus().toString().toLowerCase(),ReceivingStatus.EXPECTED.value().toLowerCase());
      
      PoLine poLine = getPoLineUpdates().get(0).mapTo(PoLine.class);
      logger.info("poLine.getReceiptStatus()--->" + poLine.getReceiptStatus());
      logger.info("ReceiptStatus.AWAITING_RECEIPT.toString()--->" + poLine.getReceiptStatus());
      assertEquals(poLine.getReceiptStatus().toString(), ReceiptStatus.AWAITING_RECEIPT.toString());

      assertEquals(result.body(), Response.Status.OK.getReasonPhrase());
    }));
  }
  
  @Test
  public void testPieceReceiptStatusFailureNoMatchingPoLineForPiece(TestContext context) {
    logger.info("=== Test case when no poLines exists referenced by a piece which should throw a 404 Exception ===");
    
    // explicitly setting RECEIVED to create inconsistency
    sendEvent(createBody("RECEIVED", PIECE_ID_WITH_NO_PO_LINE), context.asyncAssertFailure(result -> {
      logger.info("getPoLineSearches()--->" + getPoLineSearches());
      logger.info("getPoLineUpdates()--->" + getPoLineUpdates());
      logger.info("getPieceSearches()--->" + getPieceSearches());
      assertThat(getPoLineUpdates(), nullValue());
      assertEquals(getPieceSearches().get(0).getJsonArray("pieces").size(), 0);

      assertThat(result, instanceOf(ReplyException.class));
      assertThat(((ReplyException) result).failureCode(), is(404));
    }));
  }
  
  @Test
  public void testPieceReceiptStatusFailureNoInconsistencyDetected(TestContext context) {
    logger.info("=== Test case receipt status is consistent between piece and poLine ===");
    
    // Set to Expected to make piece and poLine receipt status consistent
    sendEvent(createBody("EXPECTED", VALID_PIECE_ID), context.asyncAssertSuccess(result -> {
      String pieceReqData;
      try {
        pieceReqData = getMockData(PIECE_RECORDS_MOCK_DATA_PATH + "pieceRecord-af372ac8-5ffb-4560-8b96-3945a12e121b.json");
        JsonObject pieceJsonObj = new JsonObject(pieceReqData);
        Piece piece = pieceJsonObj.mapTo(Piece.class);
        assertEquals(piece.getReceivingStatus().toString(), "Expected");
      } catch (IOException e) {
        fail(e.getMessage());
      }
      assertEquals(result.body(), Response.Status.OK.getReasonPhrase());
    }));
  }
  
//  @Test
//  public void testPieceReceiptStatusIsConsistent(TestContext context) {
//    logger.info("=== Test case when receipt status is consistent ===");
//    sendEvent(createBody("EXPCTED"), context.asyncAssertFailure(result -> {
//      assertThat(getPoLineSearches(), nullValue());
//      assertThat(getPoLineUpdates(), nullValue());
//      assertThat(getPieceSearches(), nullValue());
//      //assertThat(result, instanceOf(ReplyException.class));
//      assertThat(((ReplyException) result).failureCode(), is(404));
//    }));
//  }

  private JsonObject createBody(String receiptStatus, String pieceId) {
    JsonObject jsonObj = new JsonObject();
    jsonObj.put("receivingStatusBeforeUpdate", receiptStatus);
    jsonObj.put("pieceIdUpdate", pieceId);
    return jsonObj;
  }

  private void sendEvent(JsonObject data, Handler<AsyncResult<Message<String>>> replyHandler) {
    // Add okapi url header
    DeliveryOptions deliveryOptions = new DeliveryOptions().addHeader(X_OKAPI_URL.getName(), X_OKAPI_URL.getValue());
    deliveryOptions.addHeader(X_OKAPI_TOKEN.getName(), X_OKAPI_TOKEN.getValue());
    deliveryOptions.addHeader(EXIST_CONFIG_X_OKAPI_TENANT_LIMIT_10.getName(),EXIST_CONFIG_X_OKAPI_TENANT_LIMIT_10.getValue());
    
    vertx.eventBus().send(TEST_ADDRESS, data, deliveryOptions, replyHandler);
  }
}
