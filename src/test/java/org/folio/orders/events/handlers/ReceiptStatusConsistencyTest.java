package org.folio.orders.events.handlers;

import static org.folio.rest.impl.MockServer.getPoLineSearches;
import static org.folio.rest.impl.MockServer.getPoLineUpdates;
import static org.folio.rest.impl.MockServer.getPieceSearches;

import java.util.concurrent.ExecutionException;
import java.util.concurrent.TimeoutException;

import static org.hamcrest.MatcherAssert.assertThat;
import static org.hamcrest.Matchers.is;
import static org.junit.Assert.assertEquals;
import static org.hamcrest.Matchers.nullValue;
import static org.hamcrest.Matchers.instanceOf;

import org.folio.rest.acq.model.Piece;
import org.folio.rest.acq.model.PoLine.ReceiptStatus;
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
  private static final String BAD_PO_LINE_404 = "5b454292-6aaa-474f-9510-b59a564e0c8d";
  private static final String PO_LINE_ID_TIED_TO_PIECE_WHEN_TOTAL_PIECES_EMPTY = "0dd8f1d2-ac2e-4155-a407-72071f6d5f4a";
  private static final String POLINE_UUID_TIED_TO_PIECE = "d471d766-8dbb-4609-999a-02681dea6c22";
  private static final String POLINE_UUID_TIED_TO_PIECE_PARTIALLY_RECEIVED = "fe47e95d-24e9-4a9a-9dc0-bcba64b51f56";

  private static Vertx vertx;

  @BeforeClass
  public static void before() throws InterruptedException, ExecutionException, TimeoutException {
    ApiTestBase.before();

    vertx = Vertx.vertx();
    vertx.eventBus().consumer(TEST_ADDRESS, new ReceiptStatusConsistency(vertx));
  }

  @Test
  public void testSuccessReceiptStatusWhenReceiptStatusBetweenPiecesAndPoLineNotConsistent(TestContext context) {
    logger.info("=== Test case when piece receipt status changes from Received to Expected ===");

    sendEvent(createBody(POLINE_UUID_TIED_TO_PIECE), context.asyncAssertSuccess(result -> {
      logger.info("getPoLineSearches()--->" + getPoLineSearches());
      logger.info("getPoLineUpdates()--->" + getPoLineUpdates());
      logger.info("getPieceSearches()--->" + getPieceSearches());
      assertEquals(2, getPieceSearches().get(0).getJsonArray("pieces").size());
      
      Piece piece0 = getPieceSearches().get(0).getJsonArray("pieces").getJsonObject(0).mapTo(Piece.class);
      Piece piece1 = getPieceSearches().get(0).getJsonArray("pieces").getJsonObject(1).mapTo(Piece.class);
      assertEquals(ReceivingStatus.EXPECTED, piece0.getReceivingStatus());
      assertEquals(ReceivingStatus.EXPECTED, piece1.getReceivingStatus());
      
      PoLine poLine = getPoLineUpdates().get(0).mapTo(PoLine.class);
      assertEquals(ReceiptStatus.AWAITING_RECEIPT, poLine.getReceiptStatus());

      assertEquals(result.body(), Response.Status.OK.getReasonPhrase());
    }));
  }

  @Test
  public void testSuccessPartiallyReceivedStatusWhenAtleastOneSuccessfullyReceivedPiece(TestContext context) {
    logger.info("=== Test case for partially received status when at least one successfully received piece ===");

    sendEvent(createBody(POLINE_UUID_TIED_TO_PIECE_PARTIALLY_RECEIVED), context.asyncAssertSuccess(result -> {
      logger.info("getPoLineSearches()--->" + getPoLineSearches());
      logger.info("getPoLineUpdates()--->" + getPoLineUpdates());
      logger.info("getPieceSearches()--->" + getPieceSearches());
      assertEquals(5, getPieceSearches().get(0).getJsonArray("pieces").size());
      
      Piece piece0 = getPieceSearches().get(0).getJsonArray("pieces").getJsonObject(0).mapTo(Piece.class);
      Piece piece1 = getPieceSearches().get(0).getJsonArray("pieces").getJsonObject(1).mapTo(Piece.class);
      assertEquals(ReceivingStatus.RECEIVED, piece0.getReceivingStatus());
      assertEquals(ReceivingStatus.EXPECTED, piece1.getReceivingStatus());
      
      PoLine poLine = getPoLineUpdates().get(0).mapTo(PoLine.class);
      assertEquals(ReceiptStatus.PARTIALLY_RECEIVED, poLine.getReceiptStatus());

      assertEquals(result.body(), Response.Status.OK.getReasonPhrase());
    }));
  }
  
  @Test
  public void testSuccessReceiptStatusWhenTotalPiecesEmpty(TestContext context) {
    logger.info("=== Test case to verify receipt status when total pieces empty ===");

    sendEvent(createBody(PO_LINE_ID_TIED_TO_PIECE_WHEN_TOTAL_PIECES_EMPTY), context.asyncAssertSuccess(result -> {
      logger.info("getPoLineSearches()--->" + getPoLineSearches());
      logger.info("getPoLineUpdates()--->" + getPoLineUpdates());
      logger.info("getPieceSearches()--->" + getPieceSearches());
      assertEquals(0, getPieceSearches().get(0).getJsonArray("pieces").size());
      assertThat(getPoLineUpdates(), nullValue());

      assertEquals(result.body(), Response.Status.OK.getReasonPhrase());
    }));
  }
  
  @Test
  public void testPieceReceiptStatusFailureWhenNoMatchingPoLineForPiece(TestContext context) {
    logger.info("=== Test case when no poLines exists referenced by a piece which should throw a 404 Exception ===");
    
    sendEvent(createBody(BAD_PO_LINE_404), context.asyncAssertFailure(result -> {
      logger.info("getPoLineSearches()--->" + getPoLineSearches());
      logger.info("getPoLineUpdates()--->" + getPoLineUpdates());
      logger.info("getPieceSearches()--->" + getPieceSearches());
      assertThat(getPoLineUpdates(), nullValue());
      assertEquals(0, getPieceSearches().get(0).getJsonArray("pieces").size());

      assertThat(result, instanceOf(ReplyException.class));
      assertThat(((ReplyException) result).failureCode(), is(404));
    }));
  }

  private JsonObject createBody(String poLineId) {
    JsonObject jsonObj = new JsonObject();
    jsonObj.put("poLineIdUpdate",  poLineId);
    return jsonObj;
  }

  private void sendEvent(JsonObject data, Handler<AsyncResult<Message<String>>> replyHandler) {
    // Add okapi url header
    DeliveryOptions deliveryOptions = new DeliveryOptions().addHeader(X_OKAPI_URL.getName(), X_OKAPI_URL.getValue());
    
    vertx.eventBus().send(TEST_ADDRESS, data, deliveryOptions, replyHandler);
  }
}
