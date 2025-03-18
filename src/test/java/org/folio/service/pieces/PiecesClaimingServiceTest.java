package org.folio.service.pieces;

import io.vertx.core.Future;
import io.vertx.core.json.JsonObject;
import io.vertx.junit5.VertxExtension;
import io.vertx.junit5.VertxTestContext;
import org.folio.CopilotGenerated;
import org.folio.rest.acq.model.Organization;
import org.folio.rest.core.exceptions.HttpException;
import org.folio.rest.core.models.RequestContext;
import org.folio.rest.jaxrs.model.ClaimingCollection;
import org.folio.rest.jaxrs.model.ClaimingPiece;
import org.folio.rest.jaxrs.model.ClaimingPieceResult;
import org.folio.rest.jaxrs.model.Piece;
import org.folio.rest.jaxrs.model.PoLine;
import org.folio.rest.jaxrs.model.PurchaseOrder;
import org.folio.service.caches.ConfigurationEntriesCache;
import org.folio.service.consortium.ConsortiumConfigurationService;
import org.folio.service.orders.PurchaseOrderLineService;
import org.folio.service.orders.PurchaseOrderStorageService;
import org.folio.service.organization.OrganizationService;
import org.folio.service.pieces.flows.update.PieceUpdateFlowManager;
import org.folio.rest.core.RestClient;
import org.junit.jupiter.api.AfterEach;
import org.junit.jupiter.api.Assertions;
import org.junit.jupiter.api.BeforeEach;
import org.junit.jupiter.api.Test;
import org.junit.jupiter.api.extension.ExtendWith;
import org.mockito.InjectMocks;
import org.mockito.Mock;
import org.mockito.MockitoAnnotations;

import java.util.ArrayList;
import java.util.Comparator;
import java.util.List;
import java.util.UUID;

import static org.folio.models.claiming.IntegrationDetailField.CLAIM_PIECE_IDS;
import static org.folio.models.claiming.IntegrationDetailField.TENANT;
import static org.folio.models.claiming.IntegrationDetailField.VENDOR_EDI_ORDERS_EXPORT_CONFIG;
import static org.folio.models.claiming.IntegrationDetailField.EXPORT_TYPE_SPECIFIC_PARAMETERS;
import static org.folio.orders.utils.ResourcePathResolver.DATA_EXPORT_SPRING_CREATE_JOB;
import static org.folio.orders.utils.ResourcePathResolver.resourcesPath;
import static org.folio.rest.core.exceptions.ErrorCodes.CANNOT_FIND_PIECES_WITH_LATE_STATUS_TO_PROCESS;
import static org.folio.rest.core.exceptions.ErrorCodes.CANNOT_RETRIEVE_CONFIG_ENTRIES;
import static org.folio.rest.core.exceptions.ErrorCodes.CANNOT_SEND_CLAIMS_PIECE_IDS_ARE_EMPTY;
import static org.folio.rest.core.exceptions.ErrorCodes.UNABLE_TO_GENERATE_CLAIMS_FOR_ORG_NO_INTEGRATION_DETAILS;
import static org.junit.jupiter.api.Assertions.assertEquals;
import static org.mockito.ArgumentMatchers.any;
import static org.mockito.Mockito.*;

@ExtendWith(VertxExtension.class)
@CopilotGenerated(partiallyGenerated = true)
public class PiecesClaimingServiceTest {

  @Mock private ConfigurationEntriesCache configurationEntriesCache;
  @Mock private PieceStorageService pieceStorageService;
  @Mock private PurchaseOrderLineService purchaseOrderLineService;
  @Mock private PurchaseOrderStorageService purchaseOrderStorageService;
  @Mock private OrganizationService organizationService;
  @Mock private PieceUpdateFlowManager pieceUpdateFlowManager;
  @Mock private ConsortiumConfigurationService consortiumConfigurationService;
  @Mock private RestClient restClient;
  @InjectMocks private PiecesClaimingService piecesClaimingService;

  private AutoCloseable openedMocks;

  @BeforeEach
  void setUp() {
    openedMocks = MockitoAnnotations.openMocks(this);
  }

  @AfterEach
  void tearDown() throws Exception {
    openedMocks.close();
  }

  @Test
  void testSendClaims_emptyClaimingPieceIds(VertxTestContext testContext) {
    var claimingCollection = new ClaimingCollection().withClaimingPieces(List.of());
    var requestContext = mock(RequestContext.class);

    var throwable = Assertions.assertThrows(HttpException.class, () -> piecesClaimingService.sendClaims(claimingCollection, requestContext));
    Assertions.assertInstanceOf(HttpException.class, throwable);
    var error = throwable.getErrors().getErrors().getFirst();
    assertEquals(CANNOT_SEND_CLAIMS_PIECE_IDS_ARE_EMPTY.getCode(), error.getCode());
    assertEquals(CANNOT_SEND_CLAIMS_PIECE_IDS_ARE_EMPTY.getDescription(), error.getMessage());
    Assertions.assertTrue(error.getParameters().isEmpty());
    testContext.completeNow();
  }

  @Test
  void testSendClaims_noConfigEntries(VertxTestContext testContext) {
    var pieceId1 = UUID.randomUUID().toString();
    var claimingCollection = new ClaimingCollection().withClaimingPieces(List.of(new ClaimingPiece().withId(pieceId1))).withClaimingInterval(1);
    var requestContext = mock(RequestContext.class);

    when(configurationEntriesCache.loadConfiguration(any(), any())).thenReturn(Future.succeededFuture(new JsonObject()));

    piecesClaimingService.sendClaims(claimingCollection, requestContext)
      .onComplete(testContext.failing(throwable -> testContext.verify(() -> {
        Assertions.assertInstanceOf(HttpException.class, throwable);
        var httpException = (HttpException) throwable;
        var error = httpException.getErrors().getErrors().getFirst();
        assertEquals(CANNOT_RETRIEVE_CONFIG_ENTRIES.getCode(), error.getCode());
        assertEquals(CANNOT_RETRIEVE_CONFIG_ENTRIES.getDescription(), error.getMessage());
        Assertions.assertEquals(1, error.getParameters().size());
        var parameter = error.getParameters().getFirst();
        Assertions.assertEquals("pieceId", parameter.getKey());
        Assertions.assertEquals(pieceId1, parameter.getValue());
        testContext.completeNow();
      })));
  }

  @Test
  void testSendClaims_noPiecesFound(VertxTestContext testContext) {
    var pieceId1 = UUID.randomUUID().toString();
    var claimingCollection = new ClaimingCollection().withClaimingPieces(List.of(new ClaimingPiece().withId(pieceId1))).withClaimingInterval(1);
    var requestContext = mock(RequestContext.class);

    when(configurationEntriesCache.loadConfiguration(any(), any())).thenReturn(Future.succeededFuture(new JsonObject().put("key", "value")));
    when(pieceStorageService.getPiecesByIds(any(), any())).thenReturn(Future.succeededFuture(List.of()));

    piecesClaimingService.sendClaims(claimingCollection, requestContext)
      .onComplete(testContext.failing(throwable -> testContext.verify(() -> {
        Assertions.assertInstanceOf(HttpException.class, throwable);
        var httpException = (HttpException) throwable;
        var error = httpException.getErrors().getErrors().getFirst();
        assertEquals(CANNOT_FIND_PIECES_WITH_LATE_STATUS_TO_PROCESS.getCode(), error.getCode());
        assertEquals(CANNOT_FIND_PIECES_WITH_LATE_STATUS_TO_PROCESS.getDescription(), error.getMessage());
        Assertions.assertEquals(1, error.getParameters().size());
        var parameter = error.getParameters().getFirst();
        Assertions.assertEquals("pieceId", parameter.getKey());
        Assertions.assertEquals(pieceId1, parameter.getValue());
        testContext.completeNow();
      })));
  }

  @Test
  void testSendClaims_pieceStatusNotLate(VertxTestContext testContext) {
    var claimingCollection = new ClaimingCollection().withClaimingPieces(List.of(new ClaimingPiece().withId("pieceId1"))).withClaimingInterval(1);
    var requestContext = mock(RequestContext.class);

    when(configurationEntriesCache.loadConfiguration(any(), any())).thenReturn(Future.succeededFuture(new JsonObject().put("CLAIMS_vendorId", createIntegrationDetail())));
    when(pieceStorageService.getPiecesByIds(any(), any())).thenReturn(Future.succeededFuture(List.of(new Piece().withId("pieceId1").withPoLineId("poLineId1").withReceivingStatus(Piece.ReceivingStatus.RECEIVED))));
    when(purchaseOrderLineService.getOrderLineById(any(), any())).thenReturn(Future.succeededFuture(new PoLine().withPurchaseOrderId("orderId1")));
    when(purchaseOrderStorageService.getPurchaseOrderById(any(), any())).thenReturn(Future.succeededFuture(new PurchaseOrder().withVendor("vendorId")));
    when(organizationService.getVendorById(any(), any())).thenReturn(Future.succeededFuture(new Organization().withId("vendorId").withIsVendor(true)));
    when(pieceUpdateFlowManager.updatePiecesStatuses(any(), any(), any(), any(), any(), any())).thenReturn(Future.succeededFuture());
    when(restClient.post(eq(resourcesPath(DATA_EXPORT_SPRING_CREATE_JOB)), any(), any(), any())).thenReturn(Future.succeededFuture(new JsonObject().put("status", "CREATED")));
    when(restClient.postEmptyResponse(any(), any(), any())).thenReturn(Future.succeededFuture());

    piecesClaimingService.sendClaims(claimingCollection, requestContext)
     .onComplete(testContext.failing(throwable -> testContext.verify(() -> {
        Assertions.assertInstanceOf(HttpException.class, throwable);
        var httpException = (HttpException) throwable;
        var error = httpException.getErrors().getErrors().getFirst();
        assertEquals(CANNOT_FIND_PIECES_WITH_LATE_STATUS_TO_PROCESS.getCode(), error.getCode());
        assertEquals(CANNOT_FIND_PIECES_WITH_LATE_STATUS_TO_PROCESS.getDescription(), error.getMessage());
        Assertions.assertEquals(1, error.getParameters().size());
        var parameter = error.getParameters().getFirst();
        Assertions.assertEquals("pieceId", parameter.getKey());
        Assertions.assertEquals("pieceId1", parameter.getValue());
        testContext.completeNow();
      })));
  }

  @Test
  void testSendClaims_missingOrganizationIntegrationDetailsForTwoOrganizations(VertxTestContext testContext) {
    var pieceId = UUID.randomUUID().toString();
    var piece = new Piece().withId(pieceId).withPoLineId("poLineId").withReceivingStatus(Piece.ReceivingStatus.LATE);
    var claimingCollection = new ClaimingCollection().withClaimingPieces(List.of(new ClaimingPiece().withId(pieceId)));
    var requestContext = mock(RequestContext.class);

    when(configurationEntriesCache.loadConfiguration(any(), any())).thenReturn(Future.succeededFuture(new JsonObject()
      .put("CLAIMS_vendorId1", createIntegrationDetail())));
    when(pieceStorageService.getPiecesByIds(any(), any())).thenReturn(Future.succeededFuture(List.of(piece)));
    when(purchaseOrderLineService.getOrderLineById(any(), any())).thenReturn(Future.succeededFuture(new PoLine().withPurchaseOrderId("orderId")));
    when(purchaseOrderStorageService.getPurchaseOrderById(any(), any())).thenReturn(Future.succeededFuture(new PurchaseOrder().withVendor("vendorId2")));
    when(organizationService.getVendorById(any(), any())).thenReturn(Future.succeededFuture(new Organization().withId("vendorId2").withCode("VENDOR2").withIsVendor(true)));

    piecesClaimingService.sendClaims(claimingCollection, requestContext)
      .onComplete(testContext.failing(throwable -> testContext.verify(() -> {
        Assertions.assertInstanceOf(HttpException.class, throwable);
        var httpException = (HttpException) throwable;
        var error = httpException.getErrors().getErrors().getFirst();
        assertEquals(UNABLE_TO_GENERATE_CLAIMS_FOR_ORG_NO_INTEGRATION_DETAILS.getCode(), error.getCode());
        assertEquals(String.format(UNABLE_TO_GENERATE_CLAIMS_FOR_ORG_NO_INTEGRATION_DETAILS.getDescription(), "VENDOR2"), error.getMessage());
        Assertions.assertEquals(2, error.getParameters().size());
        var parameter1 = error.getParameters().getFirst();
        Assertions.assertEquals("pieceId", parameter1.getKey());
        Assertions.assertEquals(pieceId, parameter1.getValue());
        var parameter2 = error.getParameters().get(1);
        Assertions.assertEquals("vendorCode", parameter2.getKey());
        Assertions.assertEquals("VENDOR2", parameter2.getValue());
        testContext.completeNow();
      })));
  }

  @Test
  void testSendClaims_missingOrganizationIntegrationDetailsForThreeOrganizations(VertxTestContext testContext) {
    var pieceId1 = UUID.randomUUID().toString();
    var pieceId2 = UUID.randomUUID().toString();
    var pieceId3 = UUID.randomUUID().toString();
    var piece1 = new Piece().withId(pieceId1).withPoLineId("poLineId1").withReceivingStatus(Piece.ReceivingStatus.LATE);
    var piece2 = new Piece().withId(pieceId2).withPoLineId("poLineId2").withReceivingStatus(Piece.ReceivingStatus.LATE);
    var piece3 = new Piece().withId(pieceId3).withPoLineId("poLineId3").withReceivingStatus(Piece.ReceivingStatus.LATE);
    var claimingCollection = new ClaimingCollection().withClaimingPieces(List.of(new ClaimingPiece().withId(pieceId1), new ClaimingPiece().withId(pieceId2), new ClaimingPiece().withId(pieceId3)));
    var requestContext = mock(RequestContext.class);

    when(configurationEntriesCache.loadConfiguration(any(), any())).thenReturn(Future.succeededFuture(new JsonObject()
      .put("CLAIMS_vendorId1", createIntegrationDetail())));
    when(pieceStorageService.getPiecesByIds(any(), any())).thenReturn(Future.succeededFuture(List.of(piece1, piece2, piece3)));
    when(purchaseOrderLineService.getOrderLineById(any(), any())).thenAnswer(invocation -> {
      String poLineId = invocation.getArgument(0);
      return Future.succeededFuture(new PoLine().withPurchaseOrderId(poLineId.equals("poLineId1") ? "orderId1" : poLineId.equals("poLineId2") ? "orderId2" : "orderId3"));
    });
    when(purchaseOrderStorageService.getPurchaseOrderById(any(), any())).thenAnswer(invocation -> {
      String orderId = invocation.getArgument(0);
      return Future.succeededFuture(new PurchaseOrder().withVendor(orderId.equals("orderId1") ? "vendorId1" : orderId.equals("orderId2") ? "vendorId2" : "vendorId3"));
    });
    when(organizationService.getVendorById(any(), any())).thenAnswer(invocation -> {
      String vendorId = invocation.getArgument(0);
      return Future.succeededFuture(new Organization().withId(vendorId).withCode(vendorId.equals("vendorId1") ? "VENDOR1" : vendorId.equals("vendorId2") ? "VENDOR2" : "VENDOR3").withIsVendor(true));
    });

    piecesClaimingService.sendClaims(claimingCollection, requestContext)
      .onComplete(testContext.failing(throwable -> testContext.verify(() -> {
        Assertions.assertInstanceOf(HttpException.class, throwable);
        var httpException = (HttpException) throwable;
        var error = httpException.getErrors().getErrors().getFirst();
        assertEquals(UNABLE_TO_GENERATE_CLAIMS_FOR_ORG_NO_INTEGRATION_DETAILS.getCode(), error.getCode());
        assertEquals(String.format(UNABLE_TO_GENERATE_CLAIMS_FOR_ORG_NO_INTEGRATION_DETAILS.getDescription(), "VENDOR2"), error.getMessage());
        Assertions.assertEquals(4, error.getParameters().size());
        var parameter1 = error.getParameters().getFirst();
        Assertions.assertEquals("pieceId", parameter1.getKey());
        Assertions.assertEquals(pieceId1, parameter1.getValue());
        var parameter2 = error.getParameters().get(1);
        Assertions.assertEquals("pieceId", parameter2.getKey());
        Assertions.assertEquals(pieceId2, parameter2.getValue());
        var parameter3 = error.getParameters().get(2);
        Assertions.assertEquals("pieceId", parameter3.getKey());
        Assertions.assertEquals(pieceId3, parameter3.getValue());
        var parameter4 = error.getParameters().get(3);
        Assertions.assertEquals("vendorCode", parameter4.getKey());
        Assertions.assertEquals("VENDOR2", parameter4.getValue());
        testContext.completeNow();
      })));
  }

  @Test
  void testSendClaims_success(VertxTestContext testContext) {
    var claimingCollection = new ClaimingCollection().withClaimingPieces(List.of(new ClaimingPiece().withId("pieceId1"))).withClaimingInterval(1);
    var requestContext = mock(RequestContext.class);

    when(configurationEntriesCache.loadConfiguration(any(), any())).thenReturn(Future.succeededFuture(new JsonObject().put("CLAIMS_vendorId", createIntegrationDetail())));
    when(pieceStorageService.getPiecesByIds(any(), any())).thenReturn(Future.succeededFuture(List.of(new Piece().withId("pieceId1").withPoLineId("poLineId1").withReceivingStatus(Piece.ReceivingStatus.LATE))));
    when(purchaseOrderLineService.getOrderLineById(any(), any())).thenReturn(Future.succeededFuture(new PoLine().withPurchaseOrderId("orderId1")));
    when(purchaseOrderStorageService.getPurchaseOrderById(any(), any())).thenReturn(Future.succeededFuture(new PurchaseOrder().withVendor("vendorId")));
    when(organizationService.getVendorById(any(), any())).thenReturn(Future.succeededFuture(new Organization().withId("vendorId").withCode("VENDOR").withIsVendor(true)));
    when(pieceUpdateFlowManager.updatePiecesStatuses(any(), any(), any(), any(), any(), any())).thenReturn(Future.succeededFuture());
    when(restClient.post(eq(resourcesPath(DATA_EXPORT_SPRING_CREATE_JOB)), any(), any(), any())).thenReturn(Future.succeededFuture(new JsonObject().put("status", "CREATED")));
    when(restClient.postEmptyResponse(any(), any(), any())).thenReturn(Future.succeededFuture());

    piecesClaimingService.sendClaims(claimingCollection, requestContext)
      .onComplete(testContext.succeeding(result -> testContext.verify(() -> {
        assertEquals(1, result.getClaimingPieceResults().size());
        assertEquals("pieceId1", result.getClaimingPieceResults().getFirst().getPieceId());
        assertEquals(ClaimingPieceResult.Status.SUCCESS, result.getClaimingPieceResults().getFirst().getStatus());
        testContext.completeNow();
      })));
  }

  @Test
  void testSendClaims_successWithReceivingTenantId(VertxTestContext testContext) {
    var claimingCollection = new ClaimingCollection().withClaimingPieces(List.of(new ClaimingPiece().withId("pieceId1").withReceivingTenantId("receivingTenantId"))).withClaimingInterval(1);
    var requestContext = mock(RequestContext.class);

    when(consortiumConfigurationService.overrideContextToCentralTenantIfNeeded(eq(requestContext))).thenReturn(Future.succeededFuture(requestContext));
    when(configurationEntriesCache.loadConfiguration(any(), any())).thenReturn(Future.succeededFuture(new JsonObject().put("CLAIMS_vendorId", createIntegrationDetail())));
    when(pieceStorageService.getPiecesByIds(any(), any())).thenReturn(Future.succeededFuture(List.of(new Piece().withId("pieceId1").withPoLineId("poLineId1").withReceivingStatus(Piece.ReceivingStatus.LATE))));
    when(purchaseOrderLineService.getOrderLineById(any(), any())).thenReturn(Future.succeededFuture(new PoLine().withPurchaseOrderId("orderId1")));
    when(purchaseOrderStorageService.getPurchaseOrderById(any(), any())).thenReturn(Future.succeededFuture(new PurchaseOrder().withVendor("vendorId")));
    when(organizationService.getVendorById(any(), any())).thenReturn(Future.succeededFuture(new Organization().withId("vendorId").withCode("VENDOR").withIsVendor(true)));
    when(pieceUpdateFlowManager.updatePiecesStatuses(any(), any(), any(), any(), any(), any())).thenReturn(Future.succeededFuture());
    when(restClient.post(eq(resourcesPath(DATA_EXPORT_SPRING_CREATE_JOB)), any(), any(), any())).thenReturn(Future.succeededFuture(new JsonObject().put("status", "CREATED")));
    when(restClient.postEmptyResponse(any(), any(), any())).thenReturn(Future.succeededFuture());

    piecesClaimingService.sendClaims(claimingCollection, requestContext)
      .onComplete(testContext.succeeding(result -> testContext.verify(() -> {
        assertEquals(1, result.getClaimingPieceResults().size());
        assertEquals("pieceId1", result.getClaimingPieceResults().getFirst().getPieceId());
        assertEquals(ClaimingPieceResult.Status.SUCCESS, result.getClaimingPieceResults().getFirst().getStatus());
        testContext.completeNow();
      })));
  }

  @Test
  void testSendClaims_successWithTwoPieces(VertxTestContext testContext) {
    var claimingCollection = new ClaimingCollection().withClaimingPieces(List.of(new ClaimingPiece().withId("pieceId1"), new ClaimingPiece().withId("pieceId2"))).withClaimingInterval(1);
    var requestContext = mock(RequestContext.class);

    when(configurationEntriesCache.loadConfiguration(any(), any())).thenReturn(Future.succeededFuture(new JsonObject().put("CLAIMS_vendorId", createIntegrationDetail())));
    when(pieceStorageService.getPiecesByIds(any(), any())).thenReturn(Future.succeededFuture(List.of(
      new Piece().withId("pieceId1").withPoLineId("poLineId1").withReceivingStatus(Piece.ReceivingStatus.LATE),
      new Piece().withId("pieceId2").withPoLineId("poLineId2").withReceivingStatus(Piece.ReceivingStatus.LATE)
    )));
    when(purchaseOrderLineService.getOrderLineById(any(), any())).thenReturn(Future.succeededFuture(new PoLine().withPurchaseOrderId("orderId1")));
    when(purchaseOrderStorageService.getPurchaseOrderById(any(), any())).thenReturn(Future.succeededFuture(new PurchaseOrder().withVendor("vendorId")));
    when(organizationService.getVendorById(any(), any())).thenReturn(Future.succeededFuture(new Organization().withId("vendorId").withCode("VENDOR").withIsVendor(true)));
    when(pieceUpdateFlowManager.updatePiecesStatuses(any(), any(), any(), any(), any(), any())).thenReturn(Future.succeededFuture());
    when(restClient.post(eq(resourcesPath(DATA_EXPORT_SPRING_CREATE_JOB)), any(), any(), any())).thenReturn(Future.succeededFuture(new JsonObject().put("status", "CREATED")));
    when(restClient.postEmptyResponse(any(), any(), any())).thenReturn(Future.succeededFuture());

    piecesClaimingService.sendClaims(claimingCollection, requestContext)
      .onComplete(testContext.succeeding(result -> testContext.verify(() -> {
        assertEquals(2, result.getClaimingPieceResults().size());
        assertEquals("pieceId1", result.getClaimingPieceResults().getFirst().getPieceId());
        assertEquals(ClaimingPieceResult.Status.SUCCESS, result.getClaimingPieceResults().getFirst().getStatus());
        assertEquals("pieceId2", result.getClaimingPieceResults().get(1).getPieceId());
        assertEquals(ClaimingPieceResult.Status.SUCCESS, result.getClaimingPieceResults().get(1).getStatus());
        testContext.completeNow();
      })));
  }

  @Test
  void testSendClaims_successWithMultipleOrganizations(VertxTestContext testContext) {
    var claimingCollection = new ClaimingCollection().withClaimingPieces(List.of(new ClaimingPiece().withId("pieceId1"), new ClaimingPiece().withId("pieceId2"),
      new ClaimingPiece().withId("pieceId3"), new ClaimingPiece().withId("pieceId4"), new ClaimingPiece().withId("pieceId5"))).withClaimingInterval(1);
    var requestContext = mock(RequestContext.class);

    when(configurationEntriesCache.loadConfiguration(any(), any())).thenReturn(Future.succeededFuture(new JsonObject()
      .put("CLAIMS_vendorId1", createIntegrationDetail())
      .put("CLAIMS_vendorId2", createIntegrationDetail())));

    when(pieceStorageService.getPiecesByIds(any(), any())).thenReturn(Future.succeededFuture(List.of(
      new Piece().withId("pieceId1").withPoLineId("poLineId1").withReceivingStatus(Piece.ReceivingStatus.LATE),
      new Piece().withId("pieceId2").withPoLineId("poLineId2").withReceivingStatus(Piece.ReceivingStatus.LATE),
      new Piece().withId("pieceId3").withPoLineId("poLineId3").withReceivingStatus(Piece.ReceivingStatus.LATE),
      new Piece().withId("pieceId4").withPoLineId("poLineId4").withReceivingStatus(Piece.ReceivingStatus.LATE),
      new Piece().withId("pieceId5").withPoLineId("poLineId5").withReceivingStatus(Piece.ReceivingStatus.LATE)
    )));

    when(purchaseOrderLineService.getOrderLineById(any(), any())).thenAnswer(invocation -> {
      String poLineId = invocation.getArgument(0);
      return Future.succeededFuture(new PoLine().withPurchaseOrderId(poLineId.startsWith("poLineId1") || poLineId.startsWith("poLineId2") ? "orderId1" : "orderId2"));
    });

    when(purchaseOrderStorageService.getPurchaseOrderById(any(), any())).thenAnswer(invocation -> {
      String orderId = invocation.getArgument(0);
      return Future.succeededFuture(new PurchaseOrder().withVendor(orderId.equals("orderId1") ? "vendorId1" : "vendorId2"));
    });

    when(organizationService.getVendorById(any(), any())).thenAnswer(invocation -> {
      String vendorId = invocation.getArgument(0);
      return Future.succeededFuture(new Organization().withId(vendorId).withCode(vendorId.equals("vendorId1") ? "VENDOR1" : "VENDOR2").withIsVendor(true));
    });

    when(pieceUpdateFlowManager.updatePiecesStatuses(any(), any(), any(), any(), any(), any())).thenReturn(Future.succeededFuture());
    when(restClient.post(eq(resourcesPath(DATA_EXPORT_SPRING_CREATE_JOB)), any(), any(), any())).thenReturn(Future.succeededFuture(new JsonObject().put("status", "CREATED")));
    when(restClient.postEmptyResponse(any(), any(), any())).thenReturn(Future.succeededFuture());

    piecesClaimingService.sendClaims(claimingCollection, requestContext)
      .onComplete(testContext.succeeding(result -> testContext.verify(() -> {
        assertEquals(5, result.getClaimingPieceResults().size());
        var copiedSortedResults = new ArrayList<>(result.getClaimingPieceResults());
        copiedSortedResults.sort(Comparator.comparing(ClaimingPieceResult::getPieceId));
        assertEquals("pieceId1", copiedSortedResults.getFirst().getPieceId());
        assertEquals(ClaimingPieceResult.Status.SUCCESS, copiedSortedResults.getFirst().getStatus());
        assertEquals("pieceId2", copiedSortedResults.get(1).getPieceId());
        assertEquals(ClaimingPieceResult.Status.SUCCESS, copiedSortedResults.get(1).getStatus());
        assertEquals("pieceId3", copiedSortedResults.get(2).getPieceId());
        assertEquals(ClaimingPieceResult.Status.SUCCESS, copiedSortedResults.get(2).getStatus());
        assertEquals("pieceId4", copiedSortedResults.get(3).getPieceId());
        assertEquals(ClaimingPieceResult.Status.SUCCESS, copiedSortedResults.get(3).getStatus());
        assertEquals("pieceId5", copiedSortedResults.get(4).getPieceId());
        assertEquals(ClaimingPieceResult.Status.SUCCESS, copiedSortedResults.get(4).getStatus());
        testContext.completeNow();
      })));
  }

  @Test
  void testSendClaims_successWithMultipleOrganizationsAndOneIntegrationDetail(VertxTestContext testContext) {
    var claimingCollection = new ClaimingCollection().withClaimingPieces(List.of(new ClaimingPiece().withId("pieceId1"), new ClaimingPiece().withId("pieceId2"),
      new ClaimingPiece().withId("pieceId3"), new ClaimingPiece().withId("pieceId4"), new ClaimingPiece().withId("pieceId5"))).withClaimingInterval(1);
    var requestContext = mock(RequestContext.class);

    when(configurationEntriesCache.loadConfiguration(any(), any())).thenReturn(Future.succeededFuture(new JsonObject()
      .put("CLAIMS_vendorId1", createIntegrationDetail())
      .put("CLAIMS_vendorId2", createIntegrationDetail())
    ));

    when(pieceStorageService.getPiecesByIds(any(), any())).thenReturn(Future.succeededFuture(List.of(
      new Piece().withId("pieceId1").withPoLineId("poLineId1").withReceivingStatus(Piece.ReceivingStatus.LATE),
      new Piece().withId("pieceId2").withPoLineId("poLineId2").withReceivingStatus(Piece.ReceivingStatus.LATE),
      new Piece().withId("pieceId3").withPoLineId("poLineId3").withReceivingStatus(Piece.ReceivingStatus.LATE),
      new Piece().withId("pieceId4").withPoLineId("poLineId4").withReceivingStatus(Piece.ReceivingStatus.LATE),
      new Piece().withId("pieceId5").withPoLineId("poLineId5").withReceivingStatus(Piece.ReceivingStatus.LATE)
    )));

    when(purchaseOrderLineService.getOrderLineById(any(), any())).thenAnswer(invocation -> {
      String poLineId = invocation.getArgument(0);
      return Future.succeededFuture(new PoLine().withPurchaseOrderId(poLineId.startsWith("poLineId1") || poLineId.startsWith("poLineId2") ? "orderId1" : "orderId2"));
    });

    when(purchaseOrderStorageService.getPurchaseOrderById(any(), any())).thenAnswer(invocation -> {
      String orderId = invocation.getArgument(0);
      return Future.succeededFuture(new PurchaseOrder().withVendor(orderId.equals("orderId1") ? "vendorId1" : "vendorId2"));
    });

    when(organizationService.getVendorById(any(), any())).thenAnswer(invocation -> {
      String vendorId = invocation.getArgument(0);
      return Future.succeededFuture(new Organization().withId(vendorId).withCode(vendorId.equals("vendorId1") ? "VENDOR1" : "VENDOR2").withIsVendor(true));
    });

    when(pieceUpdateFlowManager.updatePiecesStatuses(any(), any(), any(), any(), any(), any())).thenReturn(Future.succeededFuture());
    when(restClient.post(eq(resourcesPath(DATA_EXPORT_SPRING_CREATE_JOB)), any(), any(), any())).thenReturn(Future.succeededFuture(new JsonObject().put("status", "CREATED")));
    when(restClient.postEmptyResponse(any(), any(), any())).thenReturn(Future.succeededFuture());

    piecesClaimingService.sendClaims(claimingCollection, requestContext)
      .onComplete(testContext.succeeding(result -> testContext.verify(() -> {
        assertEquals(5, result.getClaimingPieceResults().size());
        var copiedSortedResults = new ArrayList<>(result.getClaimingPieceResults());
        copiedSortedResults.sort(Comparator.comparing(ClaimingPieceResult::getPieceId));
        assertEquals("pieceId1", copiedSortedResults.getFirst().getPieceId());
        assertEquals(ClaimingPieceResult.Status.SUCCESS, copiedSortedResults.getFirst().getStatus());
        assertEquals("pieceId2", copiedSortedResults.get(1).getPieceId());
        assertEquals(ClaimingPieceResult.Status.SUCCESS, copiedSortedResults.get(1).getStatus());
        assertEquals("pieceId3", copiedSortedResults.get(2).getPieceId());
        assertEquals(ClaimingPieceResult.Status.SUCCESS, copiedSortedResults.get(1).getStatus());
        assertEquals("pieceId4", copiedSortedResults.get(3).getPieceId());
        assertEquals(ClaimingPieceResult.Status.SUCCESS, copiedSortedResults.get(1).getStatus());
        assertEquals("pieceId5", copiedSortedResults.get(4).getPieceId());
        assertEquals(ClaimingPieceResult.Status.SUCCESS, copiedSortedResults.get(1).getStatus());
        testContext.completeNow();
      })));
  }

  private JsonObject createIntegrationDetail() {
    var vendorEditOrdersExportConfig = new JsonObject().put(CLAIM_PIECE_IDS.getValue(), List.of());
    var exportTypeSpecificParameters = new JsonObject().put(VENDOR_EDI_ORDERS_EXPORT_CONFIG.getValue(), vendorEditOrdersExportConfig);
    return new JsonObject().put(EXPORT_TYPE_SPECIFIC_PARAMETERS.getValue(), exportTypeSpecificParameters).put(TENANT.getValue(), "folio_shared");
  }
}
