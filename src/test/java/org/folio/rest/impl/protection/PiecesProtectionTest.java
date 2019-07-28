package org.folio.rest.impl.protection;

import io.vertx.core.json.JsonObject;
import io.vertx.core.logging.Logger;
import io.vertx.core.logging.LoggerFactory;
import org.folio.HttpStatus;
import org.folio.rest.jaxrs.model.Piece;
import org.junit.Test;

import java.util.Arrays;

import static javax.ws.rs.core.MediaType.APPLICATION_JSON;
import static org.folio.rest.impl.PieceApiTest.PIECES_ENDPOINT;

public class PiecesProtectionTest extends ProtectedEntityTestBase {

  private static final Logger logger = LoggerFactory.getLogger(PiecesProtectionTest.class);

  @Override
  public ProtectedOperations[] geProtectedOperations() {
    ProtectedOperations[] operations = {ProtectedOperations.CREATE} ;
    return operations;
  }


  @Override
  public String getSampleForFlowWithNonExistedUnits() {
    Piece piece = getMinimalContentPiece();
    piece.setId(PIECE_WITH_PO_LINE_FOR_ORDER_WITHOUT_UNITS_ID);
    piece.setPoLineId(PO_LINE_FOR_ORDER_WITHOUT_UNITS_ID);
    return JsonObject.mapFrom(piece).encode();
  }

  @Override
  public String getSampleForFlowWithAllowedUnits() {
    Piece piece = getMinimalContentPiece();
    piece.setId(PIECE_WITH_PO_LINE_FOR_ORDER_WITH_NON_PROTECTED_UNITS_ID);
    piece.setPoLineId(PO_LINE_FOR_ORDER_WITH_NON_PROTECTED_UNITS_ID);
    return JsonObject.mapFrom(piece).encode();
  }

  @Override
  public String getSampleForProtectedUnitsAndAllowedUserFlow() {
    Piece piece = getMinimalContentPiece();
    piece.setId(PIECE_WITH_PO_LINE_FOR_ORDER_WITH_PROTECTED_UNITS_ALLOWED_USER_ID);
    piece.setPoLineId(PO_LINE_FOR_ORDER_WITH_PROTECTED_UNITS_ALLOWED_USER_ID);
    return JsonObject.mapFrom(piece).encode();
  }

  @Override
  public String getSampleForRestrictedFlow() {
    Piece piece = getMinimalContentPiece();
    piece.setId(PIECE_WITH_PO_LINE_FOR_ORDER_WITH_PROTECTED_UNITS_AND_FORBIDDEN_USER_ID);
    piece.setPoLineId(PO_LINE_FOR_ORDER_WITH_PROTECTED_UNITS_AND_FORBIDDEN_USER_ID);
    return JsonObject.mapFrom(piece).encode();
  }

  @Test
  public void testOperationWithNonExistedUnits() {
    logger.info("=== Test corresponding order hasn't units - expecting of call only to Assignments API ===");

    for (ProtectedOperations operation : Arrays.asList(ProtectedOperations.CREATE)) {
      // Composite PO already contains acquisition unit IDs
      operation.process(PIECES_ENDPOINT, getSampleForFlowWithNonExistedUnits(),
        prepareHeaders(EXIST_CONFIG_X_OKAPI_TENANT_LIMIT_10, X_OKAPI_USER_ID), APPLICATION_JSON, operation.getCode());
      // Verify number of sub-requests
      validateNumberOfRequests(0, 0, 1);
    }
  }

  @Test
  public void testOperationWithAllowedUnits() {
    logger.info("=== Test corresponding order has units allowed operation - expecting of call only to Assignments API and Units API ===");
    for (ProtectedOperations operation : geProtectedOperations()) {
      operation.process(PIECES_ENDPOINT, getSampleForFlowWithAllowedUnits(),
        prepareHeaders(EXIST_CONFIG_X_OKAPI_TENANT_LIMIT_10, X_OKAPI_USER_ID), APPLICATION_JSON, operation.getCode());
      // Verify number of sub-requests
      validateNumberOfRequests(1, 0, 1);
    }
  }

  @Test
  public void testWithRestrictedUnitsAndAllowedUser() {
    logger.info("=== Test corresponding order has units, units protect operation, user is member of order's units - expecting of calls to Units, Memberships, Assignments API and allowance of operation ===");
    Arrays.stream(ALLOWED_CREATION_HEADERS).forEach(header -> {
      for (ProtectedOperations operation : geProtectedOperations()) {
        operation.process(PIECES_ENDPOINT, getSampleForProtectedUnitsAndAllowedUserFlow(),
          prepareHeaders(EXIST_CONFIG_X_OKAPI_TENANT_LIMIT_10, header), APPLICATION_JSON, operation.getCode());
        // Verify number of sub-requests
        validateNumberOfRequests(1, 1, 1);
      }
    });
  }

  @Test
  public void testWithProtectedUnitsAndForbiddenUser() {
    logger.info("=== Test corresponding order has units, units protect operation, user isn't member of order's units - expecting of calls to Units, Memberships, Assignments API and restriction of operation ===");
    Arrays.stream(FORBIDDEN_CREATION_HEADERS).forEach(header -> {
      for (ProtectedOperations operation : geProtectedOperations()) {
        operation.process(PIECES_ENDPOINT, getSampleForRestrictedFlow(),
          prepareHeaders(EXIST_CONFIG_X_OKAPI_TENANT_LIMIT_10, header), APPLICATION_JSON, HttpStatus.HTTP_FORBIDDEN.toInt());
        // Verify number of sub-requests
        validateNumberOfRequests(1, 1, 1);
      }
    });
  }
}
