package org.folio.orders.events.handlers;

import static org.folio.rest.impl.MockServer.getPoLineSearches;
import static org.folio.rest.impl.MockServer.getPoLineUpdates;
import static org.folio.rest.impl.MockServer.PIECE_RECORDS_MOCK_DATA_PATH;
import static org.folio.rest.impl.MockServer.getPieceSearches;

import java.io.IOException;
import java.util.concurrent.ExecutionException;
import java.util.concurrent.TimeoutException;

import static org.hamcrest.MatcherAssert.assertThat;
import static org.hamcrest.Matchers.is;
import static org.junit.Assert.assertEquals;
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

  public static final String TEST_ADDRESS = "testReceiptStatusAddress";
  private static final String VALID_PIECE_ID = "af372ac8-5ffb-4560-8b96-3945a12e121b";
  private static final String PIECE_ID_WITH_NO_PO_LINE = "5b454292-6aaa-474f-9510-b59a564e0c8d";
  private static final String PIECE_ID_WITH_500_ON_PO_LINE = "7d0aa803-a659-49f0-8a95-968f277c87d7";
  private static final String EXPECTED = "Expected";
  private static final String RECEIVED = "Received";

  private static Vertx vertx;

  @BeforeClass
  public static void before() throws InterruptedException, ExecutionException, TimeoutException {
    ApiTestBase.before();

    vertx = Vertx.vertx();
    vertx.eventBus().consumer(TEST_ADDRESS, new ReceiptStatusConsistency(vertx));
  }

  @Test
  public void testReceiptStatusWhenReceiptStatusBetweenPiecesAndPoLineNotConsistent(TestContext context) {
    logger.info("=== Test case when piece receipt status changes from Received to Expected ===");
    
    // explicitly setting RECEIVED to create inconsistency
    sendEvent(createBody(RECEIVED, VALID_PIECE_ID), context.asyncAssertSuccess(result -> {
      logger.info("getPoLineSearches()--->" + getPoLineSearches());
      logger.info("getPoLineUpdates()--->" + getPoLineUpdates());
      logger.info("getPieceSearches()--->" + getPieceSearches());
      assertEquals(2, getPieceSearches().get(0).getJsonArray("pieces").size());
      
      Piece piece0 = getPieceSearches().get(0).getJsonArray("pieces").getJsonObject(0).mapTo(Piece.class);
      Piece piece1 = getPieceSearches().get(0).getJsonArray("pieces").getJsonObject(1).mapTo(Piece.class);
      assertEquals(piece0.getReceivingStatus().toString().toLowerCase(),ReceivingStatus.EXPECTED.value().toLowerCase());
      assertEquals(piece1.getReceivingStatus().toString().toLowerCase(),ReceivingStatus.EXPECTED.value().toLowerCase());
      
      PoLine poLine = getPoLineUpdates().get(0).mapTo(PoLine.class);
      assertEquals(poLine.getReceiptStatus().toString(), ReceiptStatus.AWAITING_RECEIPT.toString());

      assertEquals(result.body(), Response.Status.OK.getReasonPhrase());
    }));
  }

  @Test
  public void testPieceReceiptStatusWhenPieceAndPoLineAreConsistent(TestContext context) {
    logger.info("=== Test case receipt status is consistent between piece and poLine ===");

    // Set to Expected to make piece and poLine receipt status consistent
    sendEvent(createBody(EXPECTED, VALID_PIECE_ID), context.asyncAssertSuccess(result -> {
      String pieceReqData;
      try {
        pieceReqData = getMockData(PIECE_RECORDS_MOCK_DATA_PATH + "pieceRecord-af372ac8-5ffb-4560-8b96-3945a12e121b.json");
        JsonObject pieceJsonObj = new JsonObject(pieceReqData);
        Piece piece = pieceJsonObj.mapTo(Piece.class);
        assertEquals(EXPECTED, piece.getReceivingStatus().toString());
      } catch (IOException e) {
        fail(e.getMessage());
      }
      assertEquals(result.body(), Response.Status.OK.getReasonPhrase());
    }));
  }

  @Test
  public void testPieceReceiptStatusFailureWhenNoMatchingPoLineForPiece(TestContext context) {
    logger.info("=== Test case when no poLines exists referenced by a piece which should throw a 404 Exception ===");
    
    // explicitly setting RECEIVED to create inconsistency
    sendEvent(createBody(RECEIVED, PIECE_ID_WITH_NO_PO_LINE), context.asyncAssertFailure(result -> {
      logger.info("getPoLineSearches()--->" + getPoLineSearches());
      logger.info("getPoLineUpdates()--->" + getPoLineUpdates());
      logger.info("getPieceSearches()--->" + getPieceSearches());
      assertThat(getPoLineUpdates(), nullValue());
      assertEquals(0, getPieceSearches().get(0).getJsonArray("pieces").size());

      assertThat(result, instanceOf(ReplyException.class));
      assertThat(((ReplyException) result).failureCode(), is(404));
    }));
  }

  @Test
  public void testPieceReceiptStatus500ErrorWhenGettingPiecesByPoLineId(TestContext context) {
    logger.info("=== Test case when getting all pieces by poLineId gives internal server error Exception 500 ===");

    // explicitly setting RECEIVED to create inconsistency
    sendEvent(createBody(RECEIVED, PIECE_ID_WITH_500_ON_PO_LINE), context.asyncAssertFailure(result -> {
      assertThat(getPoLineUpdates(), nullValue());
      
      assertThat(result, instanceOf(ReplyException.class));
      assertThat(((ReplyException) result).failureCode(), is(500));
    }));
  }

  private JsonObject createBody(String receiptStatus, String pieceId) {
    JsonObject jsonObj = new JsonObject();
    jsonObj.put("receivingStatusBeforeUpdate", receiptStatus);
    jsonObj.put("pieceIdUpdate", pieceId);
    return jsonObj;
  }

  private void sendEvent(JsonObject data, Handler<AsyncResult<Message<String>>> replyHandler) {
    // Add okapi url header
    DeliveryOptions deliveryOptions = new DeliveryOptions().addHeader(X_OKAPI_URL.getName(), X_OKAPI_URL.getValue());
    
    vertx.eventBus().send(TEST_ADDRESS, data, deliveryOptions, replyHandler);
  }
}
