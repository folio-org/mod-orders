package org.folio.rest.impl;

import io.restassured.http.Header;
import io.vertx.core.json.JsonObject;
import io.vertx.junit5.VertxExtension;
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
import org.folio.rest.jaxrs.model.PieceCollection;
import org.junit.jupiter.api.AfterAll;
import org.junit.jupiter.api.AfterEach;
import org.junit.jupiter.api.BeforeAll;
import org.junit.jupiter.api.extension.ExtendWith;
import org.junit.jupiter.params.ParameterizedTest;
import org.junit.jupiter.params.provider.Arguments;
import org.junit.jupiter.params.provider.MethodSource;

import java.util.Collection;
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
import static org.folio.TestConstants.PIECES_CLAIMING_ENDPOINT;
import static org.folio.TestUtils.getMinimalOrder;
import static org.folio.TestUtils.getMockAsJson;
import static org.folio.models.claiming.IntegrationDetailField.CLAIM_PIECE_IDS;
import static org.folio.models.claiming.IntegrationDetailField.EXPORT_TYPE_SPECIFIC_PARAMETERS;
import static org.folio.models.claiming.IntegrationDetailField.VENDOR_EDI_ORDERS_EXPORT_CONFIG;
import static org.folio.orders.utils.ResourcePathResolver.ORGANIZATION_STORAGE;
import static org.folio.orders.utils.ResourcePathResolver.PIECES_STORAGE;
import static org.folio.orders.utils.ResourcePathResolver.PO_LINES_STORAGE;
import static org.folio.orders.utils.ResourcePathResolver.PURCHASE_ORDER_STORAGE;
import static org.folio.rest.impl.MockServer.BASE_MOCK_DATA_PATH;
import static org.folio.rest.impl.MockServer.ORGANIZATION_COLLECTION;
import static org.folio.rest.impl.MockServer.PIECES_COLLECTION;
import static org.folio.rest.impl.MockServer.PO_LINES_COLLECTION;
import static org.folio.rest.impl.MockServer.addMockEntry;
import static org.folio.rest.impl.MockServer.getDataExportSpringJobCreations;
import static org.folio.rest.impl.MockServer.getDataExportSpringJobExecutions;
import static org.folio.rest.impl.MockServer.getOrganizationSearches;
import static org.folio.rest.impl.MockServer.getPieceSearches;
import static org.folio.rest.impl.MockServer.getPieceUpdates;
import static org.folio.rest.impl.MockServer.getPoLineSearches;
import static org.folio.rest.impl.MockServer.getPurchaseOrderRetrievals;
import static org.folio.rest.jaxrs.model.ClaimingPieceResult.Status.FAILURE;
import static org.folio.rest.jaxrs.model.ClaimingPieceResult.Status.SUCCESS;
import static org.hamcrest.MatcherAssert.assertThat;
import static org.hamcrest.Matchers.equalTo;
import static org.hamcrest.Matchers.hasSize;
import static org.hamcrest.Matchers.is;
import static org.hamcrest.Matchers.not;
import static org.hamcrest.Matchers.notNullValue;
import static org.hamcrest.Matchers.nullValue;

@ExtendWith(VertxExtension.class)
public class PiecesClaimingApiTest {

  private static final Logger logger = LogManager.getLogger();
  private static boolean runningOnOwn;

  private static final String ORGANIZATIONS_KEY = "organizations";
  private static final String PO_LINES_KEY = "poLines";
  private static final String PIECES_KEY = "pieces";
  private static final String ENTRY_ID = "id";
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

  private static Stream<Arguments> testPostPiecesClaimArgs() {
    var payloadFile = "send-claims-1-piece-1-vendor-1-job.json";
    var mockHitDto = new MockHitDto(3, 2, 2, 1, 1, 1, 1, 1);
    return Stream.of(
      Arguments.of("One piece One vendor One Job", 0, 17, 69, mockHitDto, payloadFile, EXIST_CONFIG_X_OKAPI_TENANT_LIMIT_10_CLAIMS, SUCCESS),
      Arguments.of("One piece One vendor No Job", 0, 17, 69, null, payloadFile, EXIST_CONFIG_X_OKAPI_TENANT_LIMIT_10, FAILURE)
    );
  }

  static class MockHitDto {

    private final int pieceSearches;
    private final int polSearches;
    private final int purchaseOrderRetrievals;
    private final int organizationSearches;
    private final int pieceUpdates;
    private final int claimingResults;
    private final int jobCreations;
    private final int jobExecutions;

    public MockHitDto(int pieceSearches, int polSearches, int purchaseOrderRetrievals,
                      int organizationSearches, int pieceUpdates, int claimingResults,
                      int jobCreations, int jobExecutions) {
      this.pieceSearches = pieceSearches;
      this.polSearches = polSearches;
      this.purchaseOrderRetrievals = purchaseOrderRetrievals;
      this.organizationSearches = organizationSearches;
      this.pieceUpdates = pieceUpdates;
      this.claimingResults = claimingResults;
      this.jobCreations = jobCreations;
      this.jobExecutions = jobExecutions;
    }
  }

  @ParameterizedTest
  @MethodSource("testPostPiecesClaimArgs")
  void testPostPiecesClaim(String name, int vendorIdx, int poLineIdx, int pieceIdx, MockHitDto dto,
                           String payloadFile, Header header, ClaimingPieceResult.Status expectedStatus) {
    logger.info("Testing postPiecesClaim, name: {}", name);

    var organization = getMockAsJson(ORGANIZATION_COLLECTION)
      .getJsonArray(ORGANIZATIONS_KEY).getJsonObject(vendorIdx)
      .mapTo(Organization.class);
    var poLine = getMockAsJson(PO_LINES_COLLECTION)
      .getJsonArray(PO_LINES_KEY)
      .getJsonObject(poLineIdx)
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

    var mockDataPath = BASE_MOCK_DATA_PATH + CLAIMING_MOCK_DATA_FOLDER + payloadFile;
    var request =  getMockAsJson(mockDataPath).mapTo(ClaimingCollection.class);
    var response = verifyPostResponse(PIECES_CLAIMING_ENDPOINT, JsonObject.mapFrom(request).encode(), prepareHeaders(header), APPLICATION_JSON, CREATED.code())
      .as(ClaimingResults.class);

    // Filter out any dummy pieces without ids that are loaded from other tests
    var pieceSearches = getPieceSearches().stream()
      .map(JsonObject::mapFrom).map(json -> json.mapTo(PieceCollection.class))
      .map(collection -> collection.getPieces().stream()
        .filter(entry -> Objects.nonNull(entry.getId())).filter(entry -> entry.getId().equals(piece.getId()))
        .toList())
      .flatMap(Collection::stream)
      .toList();
    var polSearches = getPoLineSearches().stream()
      .map(JsonObject::mapFrom).map(json -> json.getString(ENTRY_ID))
      .filter(Objects::nonNull).filter(poLineId -> poLineId.equals(poLine.getId()))
      .toList();
    var purchaseOrderRetrievals = getPurchaseOrderRetrievals().stream()
      .map(JsonObject::mapFrom).map(json -> json.getString(ENTRY_ID))
      .filter(Objects::nonNull).filter(poLineId -> poLineId.equals(purchaseOrder.getId()))
      .toList();
    purchaseOrderRetrievals.forEach(entry -> logger.info("PurchaseOrders: {}", entry));
    var organizationSearches = getOrganizationSearches();
    var pieceUpdates = getPieceUpdates();
    var jobCreations = getDataExportSpringJobCreations();
    var jobExecutions = getDataExportSpringJobExecutions();

    if (Objects.nonNull(dto)) {
      assertThat(pieceSearches, not(nullValue()));
      assertThat(polSearches, not(nullValue()));
      assertThat(purchaseOrderRetrievals, not(nullValue()));
      assertThat(organizationSearches, not(nullValue()));
      assertThat(pieceUpdates, not(nullValue()));
      assertThat(jobCreations, not(nullValue()));
      assertThat(jobExecutions, not(nullValue()));
      assertThat(pieceSearches, hasSize(dto.pieceSearches));
      assertThat(polSearches, hasSize(dto.polSearches));
      assertThat(purchaseOrderRetrievals, hasSize(dto.purchaseOrderRetrievals));
      assertThat(organizationSearches, hasSize(dto.organizationSearches));
      assertThat(pieceUpdates, hasSize(dto.pieceUpdates));
      assertThat(jobCreations, hasSize(dto.jobCreations));
      assertThat(jobExecutions, hasSize(dto.jobExecutions));
      assertThat(response.getClaimingPieceResults().size(), equalTo(dto.claimingResults));
      pieceUpdates.forEach(pieceUpdate -> logger.info("Updated piece: {}", pieceUpdate.encodePrettily()));
      var claimedPieceIds = jobCreations.stream()
        .peek(job -> logger.info("Created job: {}", JsonObject.mapFrom(job).encodePrettily()))
        .map(job -> job.getJsonObject(EXPORT_TYPE_SPECIFIC_PARAMETERS.getValue())
          .getJsonObject(VENDOR_EDI_ORDERS_EXPORT_CONFIG.getValue())
          .getJsonArray(CLAIM_PIECE_IDS.getValue()).size())
        .mapToInt(value -> value).sum();
      assertThat(claimedPieceIds, equalTo(request.getClaimingPieceIds().size()));
    }

    response.getClaimingPieceResults()
      .forEach(result -> {
        if (expectedStatus == SUCCESS) {
          assertThat(result.getPieceId(), not(nullValue()));
          assertThat(result.getError(), is(nullValue()));
        } else {
          assertThat(result.getError(), is(notNullValue()));
        }
        assertThat(result.getStatus(), is(expectedStatus));
      });
  }
}
