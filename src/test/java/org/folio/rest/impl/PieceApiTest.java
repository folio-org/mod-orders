package org.folio.rest.impl;

import io.restassured.http.Header;
import io.vertx.core.json.JsonObject;
import io.vertx.core.logging.Logger;
import io.vertx.core.logging.LoggerFactory;
import org.folio.HttpStatus;
import org.folio.rest.acq.model.Piece;
import org.junit.Test;

import static javax.ws.rs.core.MediaType.APPLICATION_JSON;
import static org.folio.rest.impl.MockServer.PIECE_RECORDS_MOCK_DATA_PATH;
import static org.hamcrest.MatcherAssert.assertThat;
import static org.hamcrest.Matchers.notNullValue;
import static org.hamcrest.Matchers.nullValue;

public class PieceApiTest extends ApiTestBase {

  private static final Logger logger = LoggerFactory.getLogger(PieceApiTest.class);

  private static final String PIECES_ENDPOINT = "/orders/pieces";

  @Test
  public void testPostPiece() {
    logger.info("=== Test POST Piece (Create Piece) ===");

    Piece postPieceRq = getMockAsJson(PIECE_RECORDS_MOCK_DATA_PATH + "pieceRecord.json").mapTo(Piece.class);

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
}
