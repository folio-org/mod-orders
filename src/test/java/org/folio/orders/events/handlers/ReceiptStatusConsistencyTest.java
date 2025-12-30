package org.folio.orders.events.handlers;

import static org.folio.TestConfig.X_OKAPI_URL;
import static org.folio.TestConfig.autowireDependencies;
import static org.folio.TestConfig.clearServiceInteractions;
import static org.folio.TestConfig.initSpringContext;
import static org.folio.TestConfig.isVerticleNotDeployed;
import static org.folio.TestUtils.checkVertxContextCompletion;
import static org.folio.TestUtils.getMockAsJson;
import static org.folio.rest.impl.MockServer.PO_LINES_COLLECTION;
import static org.folio.orders.events.utils.EventUtils.POL_UPDATE_FIELD;
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

import org.apache.logging.log4j.LogManager;
import org.apache.logging.log4j.Logger;
import org.folio.ApiTestSuite;
import org.folio.config.ApplicationConfig;
import org.folio.rest.impl.MockServer;
import org.folio.rest.jaxrs.model.Piece;
import org.folio.rest.jaxrs.model.Piece.ReceivingStatus;
import org.folio.rest.jaxrs.model.PoLine;
import org.folio.rest.jaxrs.model.PoLine.ReceiptStatus;
import org.folio.service.orders.PurchaseOrderLineService;
import org.folio.service.pieces.PieceStorageService;
import org.junit.jupiter.api.AfterAll;
import org.junit.jupiter.api.AfterEach;
import org.junit.jupiter.api.BeforeAll;
import org.junit.jupiter.api.BeforeEach;
import org.junit.jupiter.api.Test;
import org.junit.jupiter.api.extension.ExtendWith;
import org.springframework.beans.factory.annotation.Autowired;

import io.vertx.core.AsyncResult;
import io.vertx.core.Handler;
import io.vertx.core.Vertx;
import io.vertx.core.eventbus.DeliveryOptions;
import io.vertx.core.eventbus.Message;
import io.vertx.core.eventbus.ReplyException;
import io.vertx.core.json.JsonObject;
import io.vertx.junit5.VertxExtension;
import io.vertx.junit5.VertxTestContext;

@ExtendWith(VertxExtension.class)
public class ReceiptStatusConsistencyTest {
  private static final Logger logger = LogManager.getLogger();

  public static final String TEST_ADDRESS = "testReceiptStatusAddress";
  private static final String BAD_PO_LINE_404 = "5b454292-6aaa-474f-9510-b59a564e0c8d";
  private static final String PO_LINE_ID_TIED_TO_PIECE_WHEN_TOTAL_PIECES_EMPTY = "0dd8f1d2-ac2e-4155-a407-72071f6d5f4a";
  private static final String POLINE_UUID_TIED_TO_PIECE = "d471d766-8dbb-4609-999a-02681dea6c22";
  private static final String POLINE_UUID_TIED_TO_PIECE_PARTIALLY_RECEIVED = "fe47e95d-24e9-4a9a-9dc0-bcba64b51f56";
  private static final String POLINE_UUID_TIED_TO_PIECE_FULLY_RECEIVED = "2f443dbe-b3f6-417a-a4ec-db2623920b4a";
  private static Vertx vertx;
  private static boolean runningOnOwn;

  @Autowired
  private PurchaseOrderLineService purchaseOrderLineService;
  @Autowired
  private PieceStorageService pieceStorageService;

  @BeforeAll
  static void before() throws InterruptedException, ExecutionException, TimeoutException {
    if (isVerticleNotDeployed()) {
      ApiTestSuite.before();
      runningOnOwn = true;
    }

    vertx = Vertx.vertx();
    initSpringContext(ApplicationConfig.class);
  }

  @BeforeEach
  void setUp() {
    autowireDependencies(this);
    vertx.eventBus().consumer(TEST_ADDRESS, new ReceiptStatusConsistency(vertx, pieceStorageService, purchaseOrderLineService));
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
  void testSuccessReceiptStatusWhenReceiptStatusBetweenPiecesAndPoLineNotConsistent(VertxTestContext context) throws Throwable {
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
  void testSuccessPartiallyReceivedStatusWhenAtleastOneSuccessfullyReceivedPiece(VertxTestContext context) throws Throwable {
    logger.info("=== Test case to verify partially received status when at least one successfully received piece ===");

    PoLine poLine1 = getMockAsJson(PO_LINES_COLLECTION).getJsonArray("poLines").getJsonObject(5).mapTo(PoLine.class);
    MockServer.addMockOrderData(Collections.singletonList(poLine1));

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

      PoLine poLine2 = getPoLineUpdates().get(0).mapTo(PoLine.class);
      assertEquals(ReceiptStatus.PARTIALLY_RECEIVED, poLine2.getReceiptStatus());

      assertEquals(result.body(), Response.Status.OK.getReasonPhrase());
      context.completeNow();
    }));
    checkVertxContextCompletion(context);
  }

  @Test
  void testSuccessFullyReceivedStatusWhenAllPiecesSuccessfullyReceived(VertxTestContext context) throws Throwable {
    logger.info("=== Test case to verify fully received status when all pieces successfully received ===");

    PoLine poLine1 = getMockAsJson(PO_LINES_MOCK_DATA_PATH, POLINE_UUID_TIED_TO_PIECE_FULLY_RECEIVED).mapTo(PoLine.class);
    MockServer.addMockOrderData(Collections.singletonList(poLine1));

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

      PoLine poLine2 = getPoLineUpdates().get(0).mapTo(PoLine.class);
      assertEquals(ReceiptStatus.FULLY_RECEIVED, poLine2.getReceiptStatus());

      assertEquals(result.body(), Response.Status.OK.getReasonPhrase());
      context.completeNow();
    }));
    checkVertxContextCompletion(context);
  }

  @Test
  void testSuccessReceiptStatusWhenTotalPiecesEmpty(VertxTestContext context) throws Throwable {
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
  void testPieceReceiptStatusFailureWhenNoMatchingPoLineForPiece(VertxTestContext context) throws Throwable {
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
    jsonObj.put(POL_UPDATE_FIELD,  poLineId);
    return jsonObj;
  }

  private void sendEvent(JsonObject data, Handler<AsyncResult<Message<String>>> replyHandler) {
    // Add okapi url header
    DeliveryOptions deliveryOptions = new DeliveryOptions().addHeader(X_OKAPI_URL.getName(), X_OKAPI_URL.getValue());

    vertx.eventBus().<String>request(TEST_ADDRESS, data, deliveryOptions).onComplete(replyHandler);
  }
}
