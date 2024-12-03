package org.folio.rest.impl;

import io.restassured.http.Header;
import io.vertx.core.json.JsonObject;
import org.apache.logging.log4j.LogManager;
import org.apache.logging.log4j.Logger;
import org.folio.ApiTestSuite;
import org.folio.Organization;
import org.folio.config.ApplicationConfig;
import org.folio.rest.jaxrs.model.ClaimingCollection;
import org.folio.rest.jaxrs.model.ClaimingPieceResult;
import org.folio.rest.jaxrs.model.ClaimingResults;
import org.folio.rest.jaxrs.model.CompositePoLine;
import org.folio.rest.jaxrs.model.Piece;
import org.junit.jupiter.api.AfterAll;
import org.junit.jupiter.api.AfterEach;
import org.junit.jupiter.api.BeforeAll;
import org.junit.jupiter.params.ParameterizedTest;
import org.junit.jupiter.params.provider.Arguments;
import org.junit.jupiter.params.provider.MethodSource;

import java.util.Objects;
import java.util.concurrent.ExecutionException;
import java.util.concurrent.TimeoutException;
import java.util.stream.Stream;

import static io.netty.handler.codec.http.HttpResponseStatus.CREATED;
import static javax.ws.rs.core.MediaType.APPLICATION_JSON;
import static org.folio.RestTestUtils.prepareHeaders;
import static org.folio.RestTestUtils.verifyPostResponse;
import static org.folio.TestConfig.clearServiceInteractions;
import static org.folio.TestConfig.initSpringContext;
import static org.folio.TestConfig.isVerticleNotDeployed;
import static org.folio.TestConstants.EXIST_CONFIG_X_OKAPI_TENANT_LIMIT_10;
import static org.folio.TestConstants.EXIST_CONFIG_X_OKAPI_TENANT_LIMIT_10_CLAIMS;
import static org.folio.TestConstants.ORDERS_CLAIMING_ENDPOINT;
import static org.folio.TestUtils.getMinimalOrder;
import static org.folio.TestUtils.getMockAsJson;
import static org.folio.orders.utils.ResourcePathResolver.ORGANIZATION_STORAGE;
import static org.folio.orders.utils.ResourcePathResolver.PIECES_STORAGE;
import static org.folio.orders.utils.ResourcePathResolver.PO_LINES_STORAGE;
import static org.folio.orders.utils.ResourcePathResolver.PURCHASE_ORDER_STORAGE;
import static org.folio.rest.impl.MockServer.BASE_MOCK_DATA_PATH;
import static org.folio.rest.impl.MockServer.ORGANIZATION_COLLECTION;
import static org.folio.rest.impl.MockServer.PIECES_COLLECTION;
import static org.folio.rest.impl.MockServer.PO_LINES_COLLECTION;
import static org.folio.rest.impl.MockServer.addMockEntry;
import static org.folio.rest.impl.MockServer.getOrganizationSearches;
import static org.folio.rest.impl.MockServer.getPieceSearches;
import static org.folio.rest.impl.MockServer.getPieceUpdates;
import static org.folio.rest.impl.MockServer.getPoLineSearches;
import static org.folio.rest.impl.MockServer.getPurchaseOrderRetrievals;
import static org.hamcrest.MatcherAssert.assertThat;
import static org.hamcrest.Matchers.equalTo;
import static org.hamcrest.Matchers.hasSize;
import static org.hamcrest.Matchers.is;
import static org.hamcrest.Matchers.not;
import static org.hamcrest.Matchers.nullValue;

public class ClaimingApiTest {

  private static final Logger logger = LogManager.getLogger();
  private static boolean runningOnOwn;

  private static final String ORGANIZATIONS_KEY = "organizations";
  private static final String PO_LINES_KEY = "poLines";
  private static final String PIECES_KEY = "pieces";
  private static final String CLAIMING_MOCK_DATA_FOLDER = "claiming/";

  @BeforeAll
  static void before() throws InterruptedException, ExecutionException, TimeoutException {
    if (isVerticleNotDeployed()) {
      ApiTestSuite.before();
      runningOnOwn = true;
    }
    initSpringContext(ApplicationConfig.class);
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

  private static Stream<Arguments> testPostOrdersClaimArgs() {
    return Stream.of(
      Arguments.of("One piece One vendor One Job", 0, 17, 69, new MockHitDto(1, 1, 1, 1, 1, 1),
        "send-claims-1-piece-1-vendor-1-job.json", EXIST_CONFIG_X_OKAPI_TENANT_LIMIT_10_CLAIMS, ClaimingPieceResult.Status.SUCCESS),
      Arguments.of("One piece One vendor No Job", 0, 17, 69, null,
        "send-claims-1-piece-1-vendor-1-job.json", EXIST_CONFIG_X_OKAPI_TENANT_LIMIT_10, ClaimingPieceResult.Status.FAILURE)
    );
  }

  static class MockHitDto {

    private final int pieceSearches;
    private final int polSearches;
    private final int purchaseOrderRetrievals;
    private final int organizationSearches;
    private final int pieceUpdates;
    private final int claimingResults;

    public MockHitDto(int pieceSearches, int polSearches, int purchaseOrderRetrievals,
                      int organizationSearches, int pieceUpdates, int claimingResults) {
      this.pieceSearches = pieceSearches;
      this.polSearches = polSearches;
      this.purchaseOrderRetrievals = purchaseOrderRetrievals;
      this.organizationSearches = organizationSearches;
      this.pieceUpdates = pieceUpdates;
      this.claimingResults = claimingResults;
    }
  }

  @ParameterizedTest
  @MethodSource("testPostOrdersClaimArgs")
  void testPostOrdersClaim(String name, int vendorIdx, int polIdx, int pieceIdx, MockHitDto dto,
                           String payloadFile, Header header, ClaimingPieceResult.Status expectedStatus) {
    logger.info("Testing postOrdersClaim, name: {}", name);

    var organization = getMockAsJson(ORGANIZATION_COLLECTION)
      .getJsonArray(ORGANIZATIONS_KEY).getJsonObject(vendorIdx)
      .mapTo(Organization.class);
    var poLine = getMockAsJson(PO_LINES_COLLECTION)
      .getJsonArray(PO_LINES_KEY)
      .getJsonObject(polIdx)
      .mapTo(CompositePoLine.class);
    var purchaseOrder = getMinimalOrder(poLine)
      .withVendor(organization.getId());
    var piece = getMockAsJson(PIECES_COLLECTION)
      .getJsonArray(PIECES_KEY)
      .getJsonObject(pieceIdx)
      .mapTo(Piece.class);

    addMockEntry(ORGANIZATION_STORAGE, organization);
    addMockEntry(PURCHASE_ORDER_STORAGE, purchaseOrder);
    addMockEntry(PO_LINES_STORAGE, poLine);
    addMockEntry(PIECES_STORAGE, piece);

    var headers = prepareHeaders(header);
    var mockDataPath = BASE_MOCK_DATA_PATH + CLAIMING_MOCK_DATA_FOLDER + payloadFile;
    var request = JsonObject.mapFrom(getMockAsJson(mockDataPath).mapTo(ClaimingCollection.class)).encode();
    var response = verifyPostResponse(ORDERS_CLAIMING_ENDPOINT, request, headers, APPLICATION_JSON, CREATED.code()).as(ClaimingResults.class);

    var pieceSearches = getPieceSearches();
    var polSearches = getPoLineSearches();
    var purchaseOrderRetrievals = getPurchaseOrderRetrievals();
    var organizationSearches = getOrganizationSearches();
    var pieceUpdates = getPieceUpdates();

    if (Objects.nonNull(dto)) {
      assertThat(pieceSearches, not(nullValue()));
      assertThat(polSearches, not(nullValue()));
      assertThat(purchaseOrderRetrievals, not(nullValue()));
      assertThat(organizationSearches, not(nullValue()));
      assertThat(pieceUpdates, not(nullValue()));
      assertThat(pieceSearches, hasSize(dto.pieceSearches));
      assertThat(polSearches, hasSize(dto.polSearches));
      assertThat(purchaseOrderRetrievals, hasSize(dto.purchaseOrderRetrievals));
      assertThat(organizationSearches, hasSize(dto.organizationSearches));
      assertThat(pieceUpdates, hasSize(dto.pieceUpdates));
      assertThat(response.getClaimingPieceResults().size(), equalTo(dto.claimingResults));
    }

    response.getClaimingPieceResults().forEach(result -> {
      if (expectedStatus == ClaimingPieceResult.Status.SUCCESS) {
        assertThat(result.getPieceId(), not(nullValue()));
        assertThat(result.getStatus(), is(expectedStatus));
        assertThat(result.getError(), is(nullValue()));
      }
    });
  }
}
