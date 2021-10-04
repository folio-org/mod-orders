package org.folio.rest.impl;

import static javax.ws.rs.core.MediaType.APPLICATION_JSON;
import static javax.ws.rs.core.MediaType.TEXT_PLAIN;
import static org.folio.RestTestUtils.prepareHeaders;
import static org.folio.RestTestUtils.verifyDeleteResponse;
import static org.folio.RestTestUtils.verifyPostResponse;
import static org.folio.RestTestUtils.verifyPut;
import static org.folio.TestConfig.clearServiceInteractions;
import static org.folio.TestConfig.initSpringContext;
import static org.folio.TestConfig.isVerticleNotDeployed;
import static org.folio.TestConstants.EXISTED_ITEM_ID;
import static org.folio.TestConstants.EXIST_CONFIG_X_OKAPI_TENANT_LIMIT_10;
import static org.folio.TestConstants.ID_BAD_FORMAT;
import static org.folio.TestConstants.ID_DOES_NOT_EXIST;
import static org.folio.TestConstants.ID_FOR_INTERNAL_SERVER_ERROR;
import static org.folio.TestConstants.X_ECHO_STATUS;
import static org.folio.TestConstants.X_OKAPI_USER_ID;
import static org.folio.TestUtils.getMockAsJson;
import static org.folio.TestUtils.getMockData;
import static org.folio.rest.core.exceptions.ErrorCodes.REQUEST_FOUND;
import static org.folio.orders.utils.ResourcePathResolver.PIECES_STORAGE;
import static org.folio.orders.utils.ResourcePathResolver.PO_LINES;
import static org.folio.orders.utils.ResourcePathResolver.PURCHASE_ORDER;
import static org.folio.rest.impl.MockServer.PIECE_RECORDS_MOCK_DATA_PATH;
import static org.hamcrest.MatcherAssert.assertThat;
import static org.hamcrest.Matchers.hasSize;
import static org.hamcrest.Matchers.is;
import static org.hamcrest.Matchers.notNullValue;
import static org.hamcrest.Matchers.nullValue;
import static org.junit.jupiter.api.Assertions.assertNull;

import java.util.Collections;
import java.util.List;
import java.util.UUID;
import java.util.concurrent.ExecutionException;
import java.util.concurrent.TimeoutException;

import org.apache.logging.log4j.LogManager;
import org.apache.logging.log4j.Logger;
import org.folio.ApiTestSuite;
import org.folio.HttpStatus;
import org.folio.config.ApplicationConfig;
import org.folio.orders.events.handlers.HandlersTestHelper;

import org.folio.rest.jaxrs.model.CompositePoLine;
import org.folio.rest.jaxrs.model.CompositePurchaseOrder;
import org.folio.rest.jaxrs.model.Eresource;
import org.folio.rest.jaxrs.model.Error;
import org.folio.rest.jaxrs.model.Errors;
import org.folio.rest.jaxrs.model.Physical;
import org.folio.rest.jaxrs.model.Piece;
import org.folio.rest.jaxrs.model.PurchaseOrder;
import org.junit.jupiter.api.AfterAll;
import org.junit.jupiter.api.AfterEach;
import org.junit.jupiter.api.BeforeAll;
import org.junit.jupiter.api.Test;

import io.restassured.http.Header;
import io.vertx.core.json.JsonObject;

public class PieceApiTest {

  private static final Logger logger = LogManager.getLogger();

  public static final String PIECES_ENDPOINT = "/orders/pieces";
  private static final String PIECES_ID_PATH = PIECES_ENDPOINT + "/%s";
  static final String VALID_UUID = "c3e26c0e-d6a6-46fb-9309-d494cd0c82de";
  static final String CONSISTENT_RECEIVED_STATUS_PIECE_UUID = "7d0aa803-a659-49f0-8a95-968f277c87d7";
  private JsonObject pieceJsonReqData = getMockAsJson(PIECE_RECORDS_MOCK_DATA_PATH + "pieceRecord.json");

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

  @Test
  void testPostPiece() {
    logger.info("=== Test POST Piece (Create Piece) ===");

    Piece postPieceRq = pieceJsonReqData.mapTo(Piece.class);
    // To skip unit's permission validation
    postPieceRq.setPoLineId("2bafc9e1-9dd3-4ede-9f23-c4a03f8bb2d5");

    // Positive cases
    // Piece id is null initially
    assertThat(postPieceRq.getId(), nullValue());

    Piece postPieceRs = verifyPostResponse(PIECES_ENDPOINT, JsonObject.mapFrom(postPieceRq).encode(),
      prepareHeaders(EXIST_CONFIG_X_OKAPI_TENANT_LIMIT_10, X_OKAPI_USER_ID), APPLICATION_JSON, HttpStatus.HTTP_CREATED.toInt()).as(Piece.class);

    // Piece id not null
    assertThat(postPieceRs.getId(), notNullValue());

    // Negative cases
    // Unable to create piece test
    int status400 = HttpStatus.HTTP_BAD_REQUEST.toInt();
    verifyPostResponse(PIECES_ENDPOINT, JsonObject.mapFrom(postPieceRq).encode(), prepareHeaders(EXIST_CONFIG_X_OKAPI_TENANT_LIMIT_10, X_OKAPI_USER_ID,
      new Header(X_ECHO_STATUS, String.valueOf(status400))), APPLICATION_JSON, status400);

    // Internal error on mod-orders-storage test
    int status500 = HttpStatus.HTTP_INTERNAL_SERVER_ERROR.toInt();
    verifyPostResponse(PIECES_ENDPOINT, JsonObject.mapFrom(postPieceRq).encode(), prepareHeaders(EXIST_CONFIG_X_OKAPI_TENANT_LIMIT_10, X_OKAPI_USER_ID,
      new Header(X_ECHO_STATUS, String.valueOf(status500))), APPLICATION_JSON, status500);
  }

  @Test
  void testPostShouldSuccessfullyCreatePieceWithoutReceiptDate() {
    logger.info("=== Test POST Piece (Create Piece) without receiptDate===");

    Piece postPieceRq = pieceJsonReqData.mapTo(Piece.class);
    // To skip unit's permission validation
    postPieceRq.setPoLineId("2bafc9e1-9dd3-4ede-9f23-c4a03f8bb2d5");
    postPieceRq.setReceiptDate(null);

    Piece piece = verifyPostResponse(PIECES_ENDPOINT, JsonObject.mapFrom(postPieceRq).encode(),
      prepareHeaders(EXIST_CONFIG_X_OKAPI_TENANT_LIMIT_10, X_OKAPI_USER_ID), APPLICATION_JSON, 201).as(Piece.class);

    assertNull(piece.getReceiptDate());
  }

  @Test
  void testPutPiecesByIdTest() throws Exception {
    logger.info("=== Test update piece by id - valid Id 204 ===");

    String reqData = getMockData(PIECE_RECORDS_MOCK_DATA_PATH + "pieceRecord.json");

    verifyPut(String.format(PIECES_ID_PATH, VALID_UUID), reqData, "", 204);

    // Message sent to event bus
    HandlersTestHelper.verifyReceiptStatusUpdateEvent(1);
  }

  @Test
  void testPutPiecesByIdConsistentReceiptStatusTest() throws Exception {
    logger.info("=== Test update piece by id when receipt status is consistent - valid Id 204 ===");

    String reqData = getMockData(PIECE_RECORDS_MOCK_DATA_PATH + "pieceRecord-received-consistent-receipt-status-5b454292-6aaa-474f-9510-b59a564e0c8d2.json");

    verifyPut(String.format(PIECES_ID_PATH, CONSISTENT_RECEIVED_STATUS_PIECE_UUID), reqData, "", 204);

    // Message not sent to event bus
    HandlersTestHelper.verifyReceiptStatusUpdateEvent(0);
  }

  @Test
  void testPutPiecesByNonExistentId() {
    logger.info("=== Test update piece by id - Id does not exists 404 ===");

    Piece reqData = pieceJsonReqData.mapTo(Piece.class);
    reqData.setId(ID_DOES_NOT_EXIST);
    String jsonBody = JsonObject.mapFrom(reqData)
      .encode();

    verifyPut(String.format(PIECES_ID_PATH, ID_DOES_NOT_EXIST), jsonBody, APPLICATION_JSON, 404);
  }

  @Test
  void testPutPiecesWithError() {
    logger.info("=== Test update piece by id - internal error from storage 500 ===");

    Piece reqData = pieceJsonReqData.mapTo(Piece.class);
    reqData.setId(ID_FOR_INTERNAL_SERVER_ERROR);
    String jsonBody = JsonObject.mapFrom(reqData)
      .encode();

    verifyPut(String.format(PIECES_ID_PATH, ID_FOR_INTERNAL_SERVER_ERROR), jsonBody, APPLICATION_JSON, 500);
  }

  @Test
  void shouldNotInvokeItemDeletionButInvokeDeletePieceByIdWhenItemAlreadyDeletedTest() {
    logger.info("=== Test delete piece by id - item has already deleted ===");

    CompositePurchaseOrder order = new CompositePurchaseOrder().withId(UUID.randomUUID().toString());
    CompositePoLine poLine = new CompositePoLine().withId(UUID.randomUUID().toString()).withPurchaseOrderId(order.getId())
      .withPhysical(new Physical().withCreateInventory(Physical.CreateInventory.INSTANCE_HOLDING_ITEM));
    Piece piece = new Piece().withId(UUID.randomUUID().toString()).withItemId(ID_DOES_NOT_EXIST).withPoLineId(poLine.getId());
    order.setCompositePoLines(Collections.singletonList(poLine));
    MockServer.addMockEntry(PIECES_STORAGE, JsonObject.mapFrom(piece));
    MockServer.addMockEntry(PO_LINES, JsonObject.mapFrom(poLine));
    MockServer.addMockEntry(PURCHASE_ORDER, JsonObject.mapFrom(order));

    verifyDeleteResponse(String.format(PIECES_ID_PATH, piece.getId()), "", 204);
    assertNull(MockServer.getItemDeletions());
    assertThat(MockServer.getPieceDeletions(), hasSize(1));
  }

  @Test
  void shouldNotDeletePieceAndItemIfGetItemByIdTrowInternalServerErrorTest() {
    logger.info("=== Test delete piece by id - item deletion Internal Server Error ===");

    CompositePurchaseOrder order = new CompositePurchaseOrder().withId(UUID.randomUUID().toString());
    CompositePoLine poLine = new CompositePoLine().withId(UUID.randomUUID().toString()).withPurchaseOrderId(order.getId())
      .withPhysical(new Physical().withCreateInventory(Physical.CreateInventory.INSTANCE_HOLDING_ITEM));
    Piece piece = new Piece().withId(UUID.randomUUID().toString()).withItemId(ID_FOR_INTERNAL_SERVER_ERROR).withPoLineId(poLine.getId());
    order.setCompositePoLines(Collections.singletonList(poLine));
    MockServer.addMockEntry(PIECES_STORAGE, JsonObject.mapFrom(piece));
    MockServer.addMockEntry(PO_LINES, JsonObject.mapFrom(poLine));
    MockServer.addMockEntry(PURCHASE_ORDER, JsonObject.mapFrom(order));

    verifyDeleteResponse(String.format(PIECES_ID_PATH, piece.getId()), "", 500);
    assertNull(MockServer.getItemDeletions());
    assertNull(MockServer.getPieceDeletions());
  }

  @Test
  void deletePieceByIdWithoutItemDeletionTest() {
    logger.info("=== Test delete piece by id - piece without item id ===");

    CompositePurchaseOrder order = new CompositePurchaseOrder().withId(UUID.randomUUID().toString());
    CompositePoLine poLine = new CompositePoLine().withId(UUID.randomUUID().toString()).withPurchaseOrderId(order.getId())
      .withPhysical(new Physical().withCreateInventory(Physical.CreateInventory.NONE));
    Piece piece = new Piece().withId(UUID.randomUUID().toString()).withPoLineId(poLine.getId());
    order.setCompositePoLines(Collections.singletonList(poLine));
    MockServer.addMockEntry(PIECES_STORAGE, JsonObject.mapFrom(piece));
    MockServer.addMockEntry(PO_LINES, JsonObject.mapFrom(poLine));
    MockServer.addMockEntry(PURCHASE_ORDER, JsonObject.mapFrom(order));

    verifyDeleteResponse(String.format(PIECES_ID_PATH, piece.getId()), "", 204);
    assertThat(MockServer.getItemDeletions(), nullValue());
    assertThat(MockServer.getPieceDeletions(), hasSize(1));
  }

  @Test
  void deletePieceByIdWithItemDeletionTest() {
    logger.info("=== Test delete piece by id - item deleted ===");

    CompositePurchaseOrder order = new CompositePurchaseOrder().withId(UUID.randomUUID().toString());
    CompositePoLine poLine = new CompositePoLine().withId(UUID.randomUUID().toString()).withPurchaseOrderId(order.getId())
      .withEresource(new Eresource().withCreateInventory(Eresource.CreateInventory.INSTANCE_HOLDING_ITEM));
    Piece piece = new Piece().withId(UUID.randomUUID().toString()).withItemId(EXISTED_ITEM_ID).withPoLineId(poLine.getId());
    order.setCompositePoLines(Collections.singletonList(poLine));
    MockServer.addMockEntry(PIECES_STORAGE, JsonObject.mapFrom(piece));
    MockServer.addMockEntry(PO_LINES, JsonObject.mapFrom(poLine));
    MockServer.addMockEntry(PURCHASE_ORDER, JsonObject.mapFrom(order));

    verifyDeleteResponse(String.format(PIECES_ID_PATH, piece.getId()), "", 204);
    assertThat(MockServer.getItemDeletions(), hasSize(1));
    assertThat(MockServer.getPieceDeletions(), hasSize(1));
  }

  @Test
  void deletePieceByIdWithRequestsTest() {
    logger.info("=== Test delete piece by id without requests ===");

    PurchaseOrder order = new PurchaseOrder().withId(UUID.randomUUID().toString());
    CompositePoLine poLine = new CompositePoLine().withId(UUID.randomUUID().toString()).withPurchaseOrderId(order.getId());
    Piece piece = new Piece().withId(UUID.randomUUID().toString()).withItemId("522a501a-56b5-48d9-b28a-3a8f02482d98").withPoLineId(poLine.getId());

    MockServer.addMockEntry(PIECES_STORAGE, JsonObject.mapFrom(piece));
    MockServer.addMockEntry(PO_LINES, JsonObject.mapFrom(poLine));
    MockServer.addMockEntry(PURCHASE_ORDER, JsonObject.mapFrom(order));

    Errors response = verifyDeleteResponse(String.format(PIECES_ID_PATH, piece.getId()), "", 422).as(Errors.class);
    List<Error> errors = response.getErrors();
    assertThat(errors, hasSize(1));
    Error error = errors.get(0);
    assertThat(error.getCode(), is(REQUEST_FOUND.getCode()));

    assertThat(MockServer.getItemDeletions(), nullValue());
    assertThat(MockServer.getPieceDeletions(), nullValue());
  }

  @Test
  void deletePiecesByIdWithInvalidFormatTest() {
    logger.info("=== Test delete piece by id - bad Id format 400 ===");
    verifyDeleteResponse(String.format(PIECES_ID_PATH, ID_BAD_FORMAT), TEXT_PLAIN, 400);
  }

  @Test
  void deleteNotExistentPieceTest() {
    logger.info("=== Test delete piece by id - id does not exists 404 ===");
    verifyDeleteResponse(String.format(PIECES_ID_PATH, ID_DOES_NOT_EXIST), APPLICATION_JSON, 404);
  }

  @Test
  void deletePieceInternalErrorOnStorageTest() {
    logger.info("=== Test delete piece by id - internal error from storage 500 ===");
    verifyDeleteResponse(String.format(PIECES_ID_PATH, ID_FOR_INTERNAL_SERVER_ERROR), APPLICATION_JSON, 500);
  }
}
