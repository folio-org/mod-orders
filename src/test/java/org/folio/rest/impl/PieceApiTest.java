package org.folio.rest.impl;

import static javax.ws.rs.core.MediaType.APPLICATION_JSON;
import static javax.ws.rs.core.MediaType.TEXT_PLAIN;
import static org.folio.orders.utils.ErrorCodes.THERE_ARE_NO_REQUESTS;
import static org.folio.orders.utils.ResourcePathResolver.*;
import static org.folio.rest.impl.MockServer.PIECE_RECORDS_MOCK_DATA_PATH;
import static org.hamcrest.MatcherAssert.assertThat;
import static org.hamcrest.Matchers.*;
import static org.junit.Assert.assertNull;

import org.folio.HttpStatus;
import org.folio.rest.acq.model.Piece;
import org.folio.rest.jaxrs.model.*;
import org.folio.rest.jaxrs.model.Error;
import org.junit.Test;

import io.restassured.http.Header;
import io.vertx.core.json.JsonObject;
import io.vertx.core.logging.Logger;
import io.vertx.core.logging.LoggerFactory;

import java.util.List;
import java.util.UUID;

public class PieceApiTest extends ApiTestBase {

  private static final Logger logger = LoggerFactory.getLogger(PieceApiTest.class);

  public static final String PIECES_ENDPOINT = "/orders/pieces";
  private static final String PIECES_ID_PATH = PIECES_ENDPOINT + "/%s";
  static final String VALID_UUID = "c3e26c0e-d6a6-46fb-9309-d494cd0c82de";
  static final String CONSISTENT_RECEIVED_STATUS_PIECE_UUID = "7d0aa803-a659-49f0-8a95-968f277c87d7";
  private JsonObject pieceJsonReqData = getMockAsJson(PIECE_RECORDS_MOCK_DATA_PATH + "pieceRecord.json");

  @Test
  public void testPostPiece() {
    logger.info("=== Test POST Piece (Create Piece) ===");

    Piece postPieceRq = pieceJsonReqData.mapTo(Piece.class);
    // To skip unit's permission validation
    postPieceRq.setPoLineId("0009662b-8b80-4001-b704-ca10971f175d");

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
  public void testPostShouldSuccessfullyCreatePieceWithoutReceiptDate() {
    logger.info("=== Test POST Piece (Create Piece) without receiptDate===");

    Piece postPieceRq = pieceJsonReqData.mapTo(Piece.class);
    // To skip unit's permission validation
    postPieceRq.setPoLineId("0009662b-8b80-4001-b704-ca10971f175d");
    postPieceRq.setReceiptDate(null);

    Piece piece = verifyPostResponse(PIECES_ENDPOINT, JsonObject.mapFrom(postPieceRq).encode(),
      prepareHeaders(EXIST_CONFIG_X_OKAPI_TENANT_LIMIT_10, X_OKAPI_USER_ID), APPLICATION_JSON, 201).as(Piece.class);

    assertNull(piece.getReceiptDate());
  }

  @Test
  public void testPutPiecesByIdTest() throws Exception {
    logger.info("=== Test update piece by id - valid Id 204 ===");

    String reqData = getMockData(PIECE_RECORDS_MOCK_DATA_PATH + "pieceRecord.json");

    verifyPut(String.format(PIECES_ID_PATH, VALID_UUID), reqData, "", 204);

    // Message sent to event bus
    verifyReceiptStatusUpdateEvent(1);
  }

  @Test
  public void testPutPiecesByIdConsistentReceiptStatusTest() throws Exception {
    logger.info("=== Test update piece by id when receipt status is consistent - valid Id 204 ===");

    String reqData = getMockData(PIECE_RECORDS_MOCK_DATA_PATH + "pieceRecord-received-consistent-receipt-status-5b454292-6aaa-474f-9510-b59a564e0c8d2.json");

    verifyPut(String.format(PIECES_ID_PATH, CONSISTENT_RECEIVED_STATUS_PIECE_UUID), reqData, "", 204);

    // Message not sent to event bus
    verifyReceiptStatusUpdateEvent(0);
  }

  @Test
  public void testPutPiecesByNonExistentId() {
    logger.info("=== Test update piece by id - Id does not exists 404 ===");

    Piece reqData = pieceJsonReqData.mapTo(Piece.class);
    reqData.setId(ID_DOES_NOT_EXIST);
    String jsonBody = JsonObject.mapFrom(reqData)
      .encode();

    verifyPut(String.format(PIECES_ID_PATH, ID_DOES_NOT_EXIST), jsonBody, APPLICATION_JSON, 404);
  }

  @Test
  public void testPutPiecesWithError() {
    logger.info("=== Test update piece by id - internal error from storage 500 ===");

    Piece reqData = pieceJsonReqData.mapTo(Piece.class);
    reqData.setId(ID_FOR_INTERNAL_SERVER_ERROR);
    String jsonBody = JsonObject.mapFrom(reqData)
      .encode();

    verifyPut(String.format(PIECES_ID_PATH, ID_FOR_INTERNAL_SERVER_ERROR), jsonBody, APPLICATION_JSON, 500);
  }

  @Test
  public void deletePieceByIdTest() {
    logger.info("=== Test delete piece by id ===");

    verifyDeleteResponse(String.format(PIECES_ID_PATH, VALID_UUID), "", 204);
  }

  @Test
  public void deletePieceByIdWitoutRequestsTest() {
    logger.info("=== Test delete piece by id without requests ===");

    PurchaseOrder order = new PurchaseOrder().withId(UUID.randomUUID().toString());
    CompositePoLine poLine = new CompositePoLine().withId(UUID.randomUUID().toString()).withPurchaseOrderId(order.getId());
    Piece piece = new Piece().withId(UUID.randomUUID().toString()).withItemId(UUID.randomUUID().toString()).withPoLineId(poLine.getId());

    MockServer.addMockEntry(PIECES, JsonObject.mapFrom(piece));
    MockServer.addMockEntry(PO_LINES, JsonObject.mapFrom(poLine));
    MockServer.addMockEntry(PURCHASE_ORDER, JsonObject.mapFrom(order));

    Errors response = verifyDeleteResponse(String.format(PIECES_ID_PATH, piece.getId()), "", 422).as(Errors.class);
    List<Error> errors = response.getErrors();
    assertThat(errors, hasSize(1));
    Error error = errors.get(0);
    assertThat(error.getCode(), is(THERE_ARE_NO_REQUESTS.getCode()));
  }

  @Test
  public void deletePiecesByIdWithInvalidFormatTest() {
    logger.info("=== Test delete piece by id - bad Id format 400 ===");
    verifyDeleteResponse(String.format(PIECES_ID_PATH, ID_BAD_FORMAT), TEXT_PLAIN, 400);
  }

  @Test
  public void deleteNotExistentPieceTest() {
    logger.info("=== Test delete piece by id - id does not exists 404 ===");
    verifyDeleteResponse(String.format(PIECES_ID_PATH, ID_DOES_NOT_EXIST), APPLICATION_JSON, 404);
  }

  @Test
  public void deletePieceInternalErrorOnStorageTest() {
    logger.info("=== Test delete piece by id - internal error from storage 500 ===");
    verifyDeleteResponse(String.format(PIECES_ID_PATH, ID_FOR_INTERNAL_SERVER_ERROR), APPLICATION_JSON, 500);
  }
}
