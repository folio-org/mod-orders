package org.folio.rest.impl;

import io.restassured.http.Header;
import io.vertx.core.json.JsonObject;
import io.vertx.core.logging.Logger;
import io.vertx.core.logging.LoggerFactory;
import org.folio.HttpStatus;
import org.folio.rest.acq.model.Piece;
import org.junit.Test;

import static javax.ws.rs.core.MediaType.APPLICATION_JSON;
import static javax.ws.rs.core.MediaType.TEXT_PLAIN;
import static org.folio.rest.impl.MockServer.PIECE_RECORDS_MOCK_DATA_PATH;
import static org.hamcrest.MatcherAssert.assertThat;
import static org.hamcrest.Matchers.notNullValue;
import static org.hamcrest.Matchers.nullValue;

public class PieceApiTest extends ApiTestBase {

  private static final Logger logger = LoggerFactory.getLogger(PieceApiTest.class);

  private static final String PIECES_ENDPOINT = "/orders/pieces";
  private static final String PIECES_ID_PATH = PIECES_ENDPOINT + "/%s";
  static final String VALID_UUID = "c3e26c0e-d6a6-46fb-9309-d494cd0c82de";
  private JsonObject pieceJsonReqData = getMockAsJson(PIECE_RECORDS_MOCK_DATA_PATH + "pieceRecord.json");
  
  @Test
  public void testPostPiece() {
    logger.info("=== Test POST Piece (Create Piece) ===");

    Piece postPieceRq = pieceJsonReqData.mapTo(Piece.class);

    // Positive cases
    // Piece id is null initially
    assertThat(postPieceRq.getId(), nullValue());

    Piece postPieceRs = verifyPostResponse(PIECES_ENDPOINT, JsonObject.mapFrom(postPieceRq).encode(),
      prepareHeaders(EXIST_CONFIG_X_OKAPI_TENANT_LIMIT_10), APPLICATION_JSON, HttpStatus.HTTP_CREATED.toInt()).as(Piece.class);

    // Piece id not null
    assertThat(postPieceRs.getId(), notNullValue());

    // Negative cases
    // Unable to create piece test
    int status400 = HttpStatus.HTTP_BAD_REQUEST.toInt();
    verifyPostResponse(PIECES_ENDPOINT, JsonObject.mapFrom(postPieceRq).encode(), prepareHeaders(EXIST_CONFIG_X_OKAPI_TENANT_LIMIT_10,
      new Header(X_ECHO_STATUS, String.valueOf(status400))), APPLICATION_JSON, status400);

    // Internal error on mod-orders-storage test
    int status500 = HttpStatus.HTTP_INTERNAL_SERVER_ERROR.toInt();
    verifyPostResponse(PIECES_ENDPOINT, JsonObject.mapFrom(postPieceRq).encode(), prepareHeaders(EXIST_CONFIG_X_OKAPI_TENANT_LIMIT_10,
      new Header(X_ECHO_STATUS, String.valueOf(status500))), APPLICATION_JSON, status500);
  }
  
  @Test
  public void testPutPiecesByIdTest() throws Exception {
    logger.info("=== Test update piece by id - valid Id 204 ===");
    
    String reqData = getMockData(PIECE_RECORDS_MOCK_DATA_PATH + "pieceRecord.json");

    verifyPut(String.format(PIECES_ID_PATH, VALID_UUID), reqData, "", 204);
  }

  @Test
  public void testPutPiecesByNonExistentId() throws Exception {
    logger.info("=== Test update piece by id - Id does not exists 404 ===");
    
    Piece reqData = pieceJsonReqData.mapTo(Piece.class);
    reqData.setId(ID_DOES_NOT_EXIST);
    String jsonBody = JsonObject.mapFrom(reqData)
      .encode();

    verifyPut(String.format(PIECES_ID_PATH, ID_DOES_NOT_EXIST), jsonBody, APPLICATION_JSON, 404);
  }

  @Test
  public void testPutPiecesWithError() throws Exception {
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
