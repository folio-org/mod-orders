package org.folio.rest.impl;

import io.restassured.http.Headers;
import io.vertx.junit5.VertxExtension;
import lombok.extern.log4j.Log4j2;
import org.folio.ApiTestSuite;
import org.folio.HttpStatus;
import org.folio.Organization;
import org.folio.config.ApplicationConfig;
import org.folio.rest.jaxrs.model.PoLine;
import org.folio.rest.jaxrs.model.Piece;
import org.folio.rest.jaxrs.model.WrapperPiece;
import org.folio.rest.jaxrs.model.WrapperPieceCollection;
import org.junit.jupiter.api.AfterAll;
import org.junit.jupiter.api.AfterEach;
import org.junit.jupiter.api.BeforeAll;
import org.junit.jupiter.api.BeforeEach;
import org.junit.jupiter.api.Test;
import org.junit.jupiter.api.extension.ExtendWith;

import java.util.concurrent.ExecutionException;
import java.util.concurrent.TimeoutException;

import static javax.ws.rs.core.MediaType.APPLICATION_JSON;
import static org.folio.RestTestUtils.prepareHeaders;
import static org.folio.RestTestUtils.verifyGet;
import static org.folio.TestConfig.clearServiceInteractions;
import static org.folio.TestConfig.initSpringContext;
import static org.folio.TestConfig.isVerticleNotDeployed;
import static org.folio.TestConstants.EXIST_CONFIG_X_OKAPI_TENANT_LIMIT_10_CLAIMS;
import static org.folio.TestConstants.WRAPPER_PIECES_ENDPOINT;
import static org.folio.TestUtils.getMinimalOrder;
import static org.folio.TestUtils.getMockAsJson;
import static org.folio.orders.utils.ResourcePathResolver.ORGANIZATION_STORAGE;
import static org.folio.orders.utils.ResourcePathResolver.PIECES_STORAGE;
import static org.folio.orders.utils.ResourcePathResolver.PO_LINES_STORAGE;
import static org.folio.orders.utils.ResourcePathResolver.PURCHASE_ORDER_STORAGE;
import static org.folio.rest.impl.MockServer.ORGANIZATION_COLLECTION;
import static org.folio.rest.impl.MockServer.PIECES_COLLECTION;
import static org.folio.rest.impl.MockServer.PO_LINES_COLLECTION;
import static org.folio.rest.impl.MockServer.addMockEntry;
import static org.folio.rest.impl.PiecesClaimingApiTest.ORGANIZATIONS_KEY;
import static org.folio.rest.impl.PiecesClaimingApiTest.PIECES_KEY;
import static org.folio.rest.impl.PiecesClaimingApiTest.PO_LINES_KEY;
import static org.junit.jupiter.api.Assertions.assertEquals;

@Log4j2
@ExtendWith(VertxExtension.class)
public class WrapperPiecesAPITest {

  private static final Headers HEADERS = prepareHeaders(EXIST_CONFIG_X_OKAPI_TENANT_LIMIT_10_CLAIMS);
  private static final String EXPECTED_VENDOR_ID = "e0fb5df2-cdf1-11e8-a8d5-f2801f1b9fd1";
  private static final String EXPECTED_PIECE_ID = "dcd0ba36-b660-4751-b9fe-c8ac61ff6f99";
  private static final int ORGANIZATION_IDX = 0;
  private static final int PO_LINE_IDX = 17;
  private static final int PIECE_IDX = 69;
  private static boolean runningOnOwn;

  @BeforeAll
  static void before() throws InterruptedException, ExecutionException, TimeoutException {
    if (isVerticleNotDeployed()) {
      ApiTestSuite.before();
      runningOnOwn = true;
    }
    initSpringContext(ApplicationConfig.class);
  }

  @BeforeEach
  void beforeEach() {
    var organization = getMockAsJson(ORGANIZATION_COLLECTION)
      .getJsonArray(ORGANIZATIONS_KEY).getJsonObject(ORGANIZATION_IDX)
      .mapTo(Organization.class);
    var poLine = getMockAsJson(PO_LINES_COLLECTION)
      .getJsonArray(PO_LINES_KEY)
      .getJsonObject(PO_LINE_IDX)
      .mapTo(PoLine.class);
    var purchaseOrder = getMinimalOrder(poLine)
      .withVendor(EXPECTED_VENDOR_ID);
    var piece = getMockAsJson(PIECES_COLLECTION)
      .getJsonArray(PIECES_KEY)
      .getJsonObject(PIECE_IDX)
      .mapTo(Piece.class);

    addMockEntry(ORGANIZATION_STORAGE, organization);
    addMockEntry(PURCHASE_ORDER_STORAGE, purchaseOrder);
    addMockEntry(PO_LINES_STORAGE, poLine);
    addMockEntry(PIECES_STORAGE, piece);
  }

  @AfterEach
  void afterEach() throws Exception {
    clearServiceInteractions();
  }

  @AfterAll
  static void after() {
    if (runningOnOwn) {
      ApiTestSuite.after();
    }
  }

  @Test
  void testGetOrdersWrapperPieces() {
    log.info("Testing testGetOrdersWrapperPieces");

    var response = verifyGet(WRAPPER_PIECES_ENDPOINT, HEADERS, APPLICATION_JSON, HttpStatus.HTTP_OK.toInt());
    var wrapperPieces = response.as(WrapperPieceCollection.class);

    assertEquals(1, wrapperPieces.getTotalRecords());
    assertEquals(EXPECTED_VENDOR_ID, wrapperPieces.getWrapperPieces().get(0).getVendorId());
    assertEquals(EXPECTED_PIECE_ID, wrapperPieces.getWrapperPieces().get(0).getPiece().getId());
  }

  @Test
  void testGetOrdersWrapperPiecesById() {
    log.info("Testing testGetOrdersWrapperPiecesById");

    var response = verifyGet(WRAPPER_PIECES_ENDPOINT + "/" + EXPECTED_PIECE_ID, HEADERS, APPLICATION_JSON, HttpStatus.HTTP_OK.toInt());
    var wrapperPiece = response.as(WrapperPiece.class);

    assertEquals(EXPECTED_VENDOR_ID, wrapperPiece.getVendorId());
    assertEquals(EXPECTED_PIECE_ID, wrapperPiece.getPiece().getId());
  }
}
