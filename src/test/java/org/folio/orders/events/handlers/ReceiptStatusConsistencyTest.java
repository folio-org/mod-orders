package org.folio.orders.events.handlers;

import static org.folio.rest.impl.MockServer.POLINES_COLLECTION;
import static org.folio.rest.impl.MockServer.PO_LINES_MOCK_DATA_PATH;
import static org.folio.rest.impl.MockServer.getPieceSearches;
import static org.folio.rest.impl.MockServer.getPoLineSearches;
import static org.folio.rest.impl.MockServer.getPoLineUpdates;
import static org.hamcrest.MatcherAssert.assertThat;
import static org.hamcrest.Matchers.instanceOf;
import static org.hamcrest.Matchers.is;
import static org.hamcrest.Matchers.nullValue;
import static org.junit.jupiter.api.Assertions.assertEquals;

import java.util.Collections;
import java.util.concurrent.ExecutionException;
import java.util.concurrent.TimeoutException;

import javax.ws.rs.core.Response;

import org.folio.rest.acq.model.Piece;
import org.folio.rest.acq.model.Piece.ReceivingStatus;
import org.folio.rest.acq.model.PoLine;
import org.folio.rest.acq.model.PoLine.ReceiptStatus;
import org.folio.rest.impl.ApiTestBase;
import org.folio.rest.impl.MockServer;
import org.folio.rest.jaxrs.model.CompositePoLine;
import org.junit.jupiter.api.BeforeAll;
import org.junit.jupiter.api.Test;
import org.junit.jupiter.api.extension.ExtendWith;

import io.vertx.core.AsyncResult;
import io.vertx.core.Handler;
import io.vertx.core.Vertx;
import io.vertx.core.eventbus.DeliveryOptions;
import io.vertx.core.eventbus.Message;
import io.vertx.core.eventbus.ReplyException;
import io.vertx.core.json.JsonObject;
import io.vertx.core.logging.Logger;
import io.vertx.core.logging.LoggerFactory;
import io.vertx.junit5.VertxExtension;
import io.vertx.junit5.VertxTestContext;

@ExtendWith(VertxExtension.class)
public class ReceiptStatusConsistencyTest extends ApiTestBase {
  private static final Logger logger = LoggerFactory.getLogger(ReceiptStatusConsistencyTest.class);

  public static final String TEST_ADDRESS = "testReceiptStatusAddress";
  private static final String BAD_PO_LINE_404 = "5b454292-6aaa-474f-9510-b59a564e0c8d";
  private static final String PO_LINE_ID_TIED_TO_PIECE_WHEN_TOTAL_PIECES_EMPTY = "0dd8f1d2-ac2e-4155-a407-72071f6d5f4a";
  private static final String POLINE_UUID_TIED_TO_PIECE = "d471d766-8dbb-4609-999a-02681dea6c22";
  private static final String POLINE_UUID_TIED_TO_PIECE_PARTIALLY_RECEIVED = "fe47e95d-24e9-4a9a-9dc0-bcba64b51f56";
  private static final String POLINE_UUID_TIED_TO_PIECE_FULLY_RECEIVED = "2f443dbe-b3f6-417a-a4ec-db2623920b4a";

  private static Vertx vertx;

  @BeforeAll
  public static void before() throws InterruptedException, ExecutionException, TimeoutException {
    ApiTestBase.before();

    vertx = Vertx.vertx();
    vertx.eventBus().consumer(TEST_ADDRESS, new ReceiptStatusConsistency(vertx));
  }

  @Test
  public void testSuccessReceiptStatusWhenReceiptStatusBetweenPiecesAndPoLineNotConsistent(VertxTestContext context) throws Throwable {
    logger.info("=== Test case when piece receipt status changes from Received to Expected ===");

    sendEvent(createBody(POLINE_UUID_TIED_TO_PIECE), context.succeeding(result -> {
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
      context.completeNow();
    }));
    checkVertxContextCompletion(context);
  }

  @Test
  public void testSuccessPartiallyReceivedStatusWhenAtleastOneSuccessfullyReceivedPiece(VertxTestContext context) throws Throwable {
    logger.info("=== Test case to verify partially received status when at least one successfully received piece ===");

    CompositePoLine compositePoLine = getMockAsJson(POLINES_COLLECTION).getJsonArray("poLines").getJsonObject(5).mapTo(CompositePoLine.class);
    MockServer.addMockTitles(Collections.singletonList(compositePoLine));

    sendEvent(createBody(POLINE_UUID_TIED_TO_PIECE_PARTIALLY_RECEIVED), context.succeeding(result -> {
      logger.info("getPoLineSearches()--->" + getPoLineSearches());
      logger.info("getPoLineUpdates()--->" + getPoLineUpdates());
      logger.info("getPieceSearches()--->" + getPieceSearches());
      assertEquals(5, getPieceSearches().get(0).getJsonArray("pieces").size());

      Piece piece0 = getPieceSearches().get(0).getJsonArray("pieces").getJsonObject(0).mapTo(Piece.class);
      Piece piece1 = getPieceSearches().get(0).getJsonArray("pieces").getJsonObject(1).mapTo(Piece.class);
      Piece piece2 = getPieceSearches().get(0).getJsonArray("pieces").getJsonObject(2).mapTo(Piece.class);
      Piece piece3 = getPieceSearches().get(0).getJsonArray("pieces").getJsonObject(3).mapTo(Piece.class);
      Piece piece4 = getPieceSearches().get(0).getJsonArray("pieces").getJsonObject(4).mapTo(Piece.class);
      assertEquals(ReceivingStatus.RECEIVED, piece0.getReceivingStatus());
      assertEquals(ReceivingStatus.EXPECTED, piece1.getReceivingStatus());
      assertEquals(ReceivingStatus.EXPECTED, piece2.getReceivingStatus());
      assertEquals(ReceivingStatus.EXPECTED, piece3.getReceivingStatus());
      assertEquals(ReceivingStatus.EXPECTED, piece4.getReceivingStatus());

      PoLine poLine = getPoLineUpdates().get(0).mapTo(PoLine.class);
      assertEquals(ReceiptStatus.PARTIALLY_RECEIVED, poLine.getReceiptStatus());

      assertEquals(result.body(), Response.Status.OK.getReasonPhrase());
      context.completeNow();
    }));
    checkVertxContextCompletion(context);
  }

  @Test
  public void testSuccessFullyReceivedStatusWhenAllPiecesSuccessfullyReceived(VertxTestContext context) throws Throwable {
    logger.info("=== Test case to verify fully received status when all pieces successfully received ===");

    CompositePoLine compositePoLine = getMockAsJson(PO_LINES_MOCK_DATA_PATH, POLINE_UUID_TIED_TO_PIECE_FULLY_RECEIVED).mapTo(CompositePoLine.class);
    MockServer.addMockTitles(Collections.singletonList(compositePoLine));

    sendEvent(createBody(POLINE_UUID_TIED_TO_PIECE_FULLY_RECEIVED), context.succeeding(result -> {
      logger.info("getPoLineSearches()--->" + getPoLineSearches());
      logger.info("getPoLineUpdates()--->" + getPoLineUpdates());
      logger.info("getPieceSearches()--->" + getPieceSearches());
      assertEquals(5, getPieceSearches().get(0).getJsonArray("pieces").size());

      Piece piece0 = getPieceSearches().get(0).getJsonArray("pieces").getJsonObject(0).mapTo(Piece.class);
      Piece piece1 = getPieceSearches().get(0).getJsonArray("pieces").getJsonObject(1).mapTo(Piece.class);
      Piece piece2 = getPieceSearches().get(0).getJsonArray("pieces").getJsonObject(2).mapTo(Piece.class);
      Piece piece3 = getPieceSearches().get(0).getJsonArray("pieces").getJsonObject(3).mapTo(Piece.class);
      Piece piece4 = getPieceSearches().get(0).getJsonArray("pieces").getJsonObject(4).mapTo(Piece.class);
      assertEquals(ReceivingStatus.RECEIVED, piece0.getReceivingStatus());
      assertEquals(ReceivingStatus.RECEIVED, piece1.getReceivingStatus());
      assertEquals(ReceivingStatus.RECEIVED, piece2.getReceivingStatus());
      assertEquals(ReceivingStatus.RECEIVED, piece3.getReceivingStatus());
      assertEquals(ReceivingStatus.RECEIVED, piece4.getReceivingStatus());

      PoLine poLine = getPoLineUpdates().get(0).mapTo(PoLine.class);
      assertEquals(ReceiptStatus.FULLY_RECEIVED, poLine.getReceiptStatus());

      assertEquals(result.body(), Response.Status.OK.getReasonPhrase());
      context.completeNow();
    }));
    checkVertxContextCompletion(context);
  }

  @Test
  public void testSuccessReceiptStatusWhenTotalPiecesEmpty(VertxTestContext context) throws Throwable {
    logger.info("=== Test case to verify receipt status when total pieces empty ===");

    sendEvent(createBody(PO_LINE_ID_TIED_TO_PIECE_WHEN_TOTAL_PIECES_EMPTY), context.succeeding(result -> {
      logger.info("getPoLineSearches()--->" + getPoLineSearches());
      logger.info("getPoLineUpdates()--->" + getPoLineUpdates());
      logger.info("getPieceSearches()--->" + getPieceSearches());
      assertEquals(0, getPieceSearches().get(0).getJsonArray("pieces").size());
      assertThat(getPoLineUpdates(), nullValue());

      assertEquals(result.body(), Response.Status.OK.getReasonPhrase());
      context.completeNow();
    }));
    checkVertxContextCompletion(context);
  }

  @Test
  public void testPieceReceiptStatusFailureWhenNoMatchingPoLineForPiece(VertxTestContext context) throws Throwable {
    logger.info("=== Test case when no poLines exists referenced by a piece which should throw a 404 Exception ===");

    sendEvent(createBody(BAD_PO_LINE_404), context.failing(result -> {
      logger.info("getPoLineSearches()--->" + getPoLineSearches());
      logger.info("getPoLineUpdates()--->" + getPoLineUpdates());
      logger.info("getPieceSearches()--->" + getPieceSearches());
      assertThat(getPoLineUpdates(), nullValue());
      assertEquals(0, getPieceSearches().get(0).getJsonArray("pieces").size());

      assertThat(result, instanceOf(ReplyException.class));
      assertThat(((ReplyException) result).failureCode(), is(404));
      context.completeNow();
    }));
    checkVertxContextCompletion(context);
  }

  private JsonObject createBody(String poLineId) {
    JsonObject jsonObj = new JsonObject();
    jsonObj.put("poLineIdUpdate",  poLineId);
    return jsonObj;
  }

  private void sendEvent(JsonObject data, Handler<AsyncResult<Message<String>>> replyHandler) {
    // Add okapi url header
    DeliveryOptions deliveryOptions = new DeliveryOptions().addHeader(X_OKAPI_URL.getName(), X_OKAPI_URL.getValue());

    vertx.eventBus().request(TEST_ADDRESS, data, deliveryOptions, replyHandler);
  }
}
