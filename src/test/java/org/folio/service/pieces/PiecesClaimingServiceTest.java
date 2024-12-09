package org.folio.service.pieces;

import io.vertx.core.Future;
import io.vertx.core.json.JsonObject;
import io.vertx.junit5.VertxExtension;
import io.vertx.junit5.VertxTestContext;
import org.folio.CopilotGenerated;
import org.folio.rest.acq.model.Organization;
import org.folio.rest.core.models.RequestContext;
import org.folio.rest.jaxrs.model.ClaimingCollection;
import org.folio.rest.jaxrs.model.ClaimingPieceResult;
import org.folio.rest.jaxrs.model.Piece;
import org.folio.rest.jaxrs.model.PoLine;
import org.folio.rest.jaxrs.model.PurchaseOrder;
import org.folio.service.caches.ConfigurationEntriesCache;
import org.folio.service.orders.PurchaseOrderLineService;
import org.folio.service.orders.PurchaseOrderStorageService;
import org.folio.service.organization.OrganizationService;
import org.folio.service.pieces.flows.update.PieceUpdateFlowManager;
import org.folio.rest.core.RestClient;
import org.junit.jupiter.api.AfterEach;
import org.junit.jupiter.api.BeforeEach;
import org.junit.jupiter.api.Test;
import org.junit.jupiter.api.extension.ExtendWith;
import org.mockito.InjectMocks;
import org.mockito.Mock;
import org.mockito.MockitoAnnotations;

import java.util.ArrayList;
import java.util.Comparator;
import java.util.List;

import static org.folio.models.claiming.ClaimingError.CANNOT_FIND_PIECES_WITH_LATE_STATUS_TO_PROCESS;
import static org.folio.models.claiming.ClaimingError.CANNOT_RETRIEVE_CONFIG_ENTRIES;
import static org.folio.models.claiming.ClaimingError.CANNOT_SEND_CLAIMS_PIECE_IDS_ARE_EMPTY;
import static org.folio.models.claiming.IntegrationDetailField.CLAIM_PIECE_IDS;
import static org.folio.models.claiming.IntegrationDetailField.EXPORT_TYPE_SPECIFIC_PARAMETERS;
import static org.folio.models.claiming.IntegrationDetailField.VENDOR_EDI_ORDERS_EXPORT_CONFIG;
import static org.folio.orders.utils.ResourcePathResolver.DATA_EXPORT_SPRING_CREATE_JOB;
import static org.folio.orders.utils.ResourcePathResolver.resourcesPath;
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
    var claimingCollection = new ClaimingCollection().withClaimingPieceIds(List.of());
    var requestContext = mock(RequestContext.class);

    piecesClaimingService.sendClaims(claimingCollection, requestContext)
      .onComplete(testContext.succeeding(result -> testContext.verify(() -> {
        assertEquals(1, result.getClaimingPieceResults().size());
        assertEquals(CANNOT_SEND_CLAIMS_PIECE_IDS_ARE_EMPTY.getValue(), result.getClaimingPieceResults().get(0).getError().getMessage());
        testContext.completeNow();
      })));
  }

  @Test
  void testSendClaims_noConfigEntries(VertxTestContext testContext) {
    var claimingCollection = new ClaimingCollection().withClaimingPieceIds(List.of("pieceId1"));
    var requestContext = mock(RequestContext.class);

    when(configurationEntriesCache.loadConfiguration(any(), any())).thenReturn(Future.succeededFuture(new JsonObject()));

    piecesClaimingService.sendClaims(claimingCollection, requestContext)
      .onComplete(testContext.succeeding(result -> testContext.verify(() -> {
        assertEquals(1, result.getClaimingPieceResults().size());
        assertEquals(CANNOT_RETRIEVE_CONFIG_ENTRIES.getValue(), result.getClaimingPieceResults().get(0).getError().getMessage());
        testContext.completeNow();
      })));
  }

  @Test
  void testSendClaims_noPiecesFound(VertxTestContext testContext) {
    var claimingCollection = new ClaimingCollection().withClaimingPieceIds(List.of("pieceId1"));
    var requestContext = mock(RequestContext.class);

    when(configurationEntriesCache.loadConfiguration(any(), any())).thenReturn(Future.succeededFuture(new JsonObject().put("key", "value")));
    when(pieceStorageService.getPiecesByIds(any(), any())).thenReturn(Future.succeededFuture(List.of()));

    piecesClaimingService.sendClaims(claimingCollection, requestContext)
      .onComplete(testContext.succeeding(result -> testContext.verify(() -> {
        assertEquals(1, result.getClaimingPieceResults().size());
        assertEquals(CANNOT_FIND_PIECES_WITH_LATE_STATUS_TO_PROCESS.getValue(), result.getClaimingPieceResults().get(0).getError().getMessage());
        testContext.completeNow();
      })));
  }

  @Test
  void testSendClaims_pieceStatusNotLate(VertxTestContext testContext) {
    var claimingCollection = new ClaimingCollection().withClaimingPieceIds(List.of("pieceId1"));
    var requestContext = mock(RequestContext.class);

    when(configurationEntriesCache.loadConfiguration(any(), any())).thenReturn(Future.succeededFuture(new JsonObject().put("CLAIMS_vendorId", createIntegrationDetail())));
    when(pieceStorageService.getPiecesByIds(any(), any())).thenReturn(Future.succeededFuture(List.of(new Piece().withId("pieceId1").withPoLineId("poLineId1").withReceivingStatus(Piece.ReceivingStatus.RECEIVED))));
    when(purchaseOrderLineService.getOrderLineById(any(), any())).thenReturn(Future.succeededFuture(new PoLine().withPurchaseOrderId("orderId1")));
    when(purchaseOrderStorageService.getPurchaseOrderById(any(), any())).thenReturn(Future.succeededFuture(new PurchaseOrder().withVendor("vendorId")));
    when(organizationService.getVendorById(any(), any())).thenReturn(Future.succeededFuture(new Organization().withId("vendorId").withIsVendor(true)));
    when(pieceUpdateFlowManager.updatePiecesStatuses(any(), any(), any())).thenReturn(Future.succeededFuture());
    when(restClient.post(eq(resourcesPath(DATA_EXPORT_SPRING_CREATE_JOB)), any(), any(), any())).thenReturn(Future.succeededFuture(new JsonObject().put("status", "CREATED")));
    when(restClient.postEmptyResponse(any(), any(), any())).thenReturn(Future.succeededFuture());

    piecesClaimingService.sendClaims(claimingCollection, requestContext)
      .onComplete(testContext.succeeding(result -> testContext.verify(() -> {
        assertEquals(1, result.getClaimingPieceResults().size());
        assertEquals(CANNOT_FIND_PIECES_WITH_LATE_STATUS_TO_PROCESS.getValue(), result.getClaimingPieceResults().get(0).getError().getMessage());
        testContext.completeNow();
      })));
  }

  @Test
  void testSendClaims_success(VertxTestContext testContext) {
    var claimingCollection = new ClaimingCollection().withClaimingPieceIds(List.of("pieceId1"));
    var requestContext = mock(RequestContext.class);

    when(configurationEntriesCache.loadConfiguration(any(), any())).thenReturn(Future.succeededFuture(new JsonObject().put("CLAIMS_vendorId", createIntegrationDetail())));
    when(pieceStorageService.getPiecesByIds(any(), any())).thenReturn(Future.succeededFuture(List.of(new Piece().withId("pieceId1").withPoLineId("poLineId1").withReceivingStatus(Piece.ReceivingStatus.LATE))));
    when(purchaseOrderLineService.getOrderLineById(any(), any())).thenReturn(Future.succeededFuture(new PoLine().withPurchaseOrderId("orderId1")));
    when(purchaseOrderStorageService.getPurchaseOrderById(any(), any())).thenReturn(Future.succeededFuture(new PurchaseOrder().withVendor("vendorId")));
    when(organizationService.getVendorById(any(), any())).thenReturn(Future.succeededFuture(new Organization().withId("vendorId").withIsVendor(true)));
    when(pieceUpdateFlowManager.updatePiecesStatuses(any(), any(), any())).thenReturn(Future.succeededFuture());
    when(restClient.post(eq(resourcesPath(DATA_EXPORT_SPRING_CREATE_JOB)), any(), any(), any())).thenReturn(Future.succeededFuture(new JsonObject().put("status", "CREATED")));
    when(restClient.postEmptyResponse(any(), any(), any())).thenReturn(Future.succeededFuture());

    piecesClaimingService.sendClaims(claimingCollection, requestContext)
      .onComplete(testContext.succeeding(result -> testContext.verify(() -> {
        assertEquals(1, result.getClaimingPieceResults().size());
        assertEquals("pieceId1", result.getClaimingPieceResults().get(0).getPieceId());
        assertEquals(ClaimingPieceResult.Status.SUCCESS, result.getClaimingPieceResults().get(0).getStatus());
        testContext.completeNow();
      })));
  }

  @Test
  void testSendClaims_successWithTwoPieces(VertxTestContext testContext) {
    var claimingCollection = new ClaimingCollection().withClaimingPieceIds(List.of("pieceId1", "pieceId2"));
    var requestContext = mock(RequestContext.class);

    when(configurationEntriesCache.loadConfiguration(any(), any())).thenReturn(Future.succeededFuture(new JsonObject().put("CLAIMS_vendorId", createIntegrationDetail())));
    when(pieceStorageService.getPiecesByIds(any(), any())).thenReturn(Future.succeededFuture(List.of(
      new Piece().withId("pieceId1").withPoLineId("poLineId1").withReceivingStatus(Piece.ReceivingStatus.LATE),
      new Piece().withId("pieceId2").withPoLineId("poLineId2").withReceivingStatus(Piece.ReceivingStatus.LATE)
    )));
    when(purchaseOrderLineService.getOrderLineById(any(), any())).thenReturn(Future.succeededFuture(new PoLine().withPurchaseOrderId("orderId1")));
    when(purchaseOrderStorageService.getPurchaseOrderById(any(), any())).thenReturn(Future.succeededFuture(new PurchaseOrder().withVendor("vendorId")));
    when(organizationService.getVendorById(any(), any())).thenReturn(Future.succeededFuture(new Organization().withId("vendorId").withIsVendor(true)));
    when(pieceUpdateFlowManager.updatePiecesStatuses(any(), any(), any())).thenReturn(Future.succeededFuture());
    when(restClient.post(eq(resourcesPath(DATA_EXPORT_SPRING_CREATE_JOB)), any(), any(), any())).thenReturn(Future.succeededFuture(new JsonObject().put("status", "CREATED")));
    when(restClient.postEmptyResponse(any(), any(), any())).thenReturn(Future.succeededFuture());

    piecesClaimingService.sendClaims(claimingCollection, requestContext)
      .onComplete(testContext.succeeding(result -> testContext.verify(() -> {
        assertEquals(2, result.getClaimingPieceResults().size());
        assertEquals("pieceId1", result.getClaimingPieceResults().get(0).getPieceId());
        assertEquals(ClaimingPieceResult.Status.SUCCESS, result.getClaimingPieceResults().get(0).getStatus());
        assertEquals("pieceId2", result.getClaimingPieceResults().get(1).getPieceId());
        assertEquals(ClaimingPieceResult.Status.SUCCESS, result.getClaimingPieceResults().get(1).getStatus());
        testContext.completeNow();
      })));
  }

  @Test
  void testSendClaims_successWithMultipleOrganizations(VertxTestContext testContext) {
    var claimingCollection = new ClaimingCollection().withClaimingPieceIds(List.of("pieceId1", "pieceId2", "pieceId3", "pieceId4", "pieceId5"));
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
      return Future.succeededFuture(new Organization().withId(vendorId).withIsVendor(true));
    });

    when(pieceUpdateFlowManager.updatePiecesStatuses(any(), any(), any())).thenReturn(Future.succeededFuture());
    when(restClient.post(eq(resourcesPath(DATA_EXPORT_SPRING_CREATE_JOB)), any(), any(), any())).thenReturn(Future.succeededFuture(new JsonObject().put("status", "CREATED")));
    when(restClient.postEmptyResponse(any(), any(), any())).thenReturn(Future.succeededFuture());

    piecesClaimingService.sendClaims(claimingCollection, requestContext)
      .onComplete(testContext.succeeding(result -> testContext.verify(() -> {
        assertEquals(5, result.getClaimingPieceResults().size());
        var copiedSortedResults = new ArrayList<>(result.getClaimingPieceResults());
        copiedSortedResults.sort(Comparator.comparing(ClaimingPieceResult::getPieceId));
        assertEquals("pieceId1", copiedSortedResults.get(0).getPieceId());
        assertEquals(ClaimingPieceResult.Status.SUCCESS, copiedSortedResults.get(0).getStatus());
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
    var claimingCollection = new ClaimingCollection().withClaimingPieceIds(List.of("pieceId1", "pieceId2", "pieceId3", "pieceId4", "pieceId5"));
    var requestContext = mock(RequestContext.class);

    when(configurationEntriesCache.loadConfiguration(any(), any())).thenReturn(Future.succeededFuture(new JsonObject()
      .put("CLAIMS_vendorId1", createIntegrationDetail())));

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
      return Future.succeededFuture(new Organization().withId(vendorId).withIsVendor(true));
    });

    when(pieceUpdateFlowManager.updatePiecesStatuses(any(), any(), any())).thenReturn(Future.succeededFuture());
    when(restClient.post(eq(resourcesPath(DATA_EXPORT_SPRING_CREATE_JOB)), any(), any(), any())).thenReturn(Future.succeededFuture(new JsonObject().put("status", "CREATED")));
    when(restClient.postEmptyResponse(any(), any(), any())).thenReturn(Future.succeededFuture());

    piecesClaimingService.sendClaims(claimingCollection, requestContext)
      .onComplete(testContext.succeeding(result -> testContext.verify(() -> {
        assertEquals(2, result.getClaimingPieceResults().size());
        var copiedSortedResults = new ArrayList<>(result.getClaimingPieceResults());
        copiedSortedResults.sort(Comparator.comparing(ClaimingPieceResult::getPieceId));
        assertEquals("pieceId1", copiedSortedResults.get(0).getPieceId());
        assertEquals(ClaimingPieceResult.Status.SUCCESS, copiedSortedResults.get(0).getStatus());
        assertEquals("pieceId2", copiedSortedResults.get(1).getPieceId());
        assertEquals(ClaimingPieceResult.Status.SUCCESS, copiedSortedResults.get(1).getStatus());
        testContext.completeNow();
      })));
  }

  private JsonObject createIntegrationDetail() {
    var vendorEditOrdersExportConfig = new JsonObject().put(CLAIM_PIECE_IDS.getValue(), List.of());
    var exportTypeSpecificParameters = new JsonObject().put(VENDOR_EDI_ORDERS_EXPORT_CONFIG.getValue(), vendorEditOrdersExportConfig);
    return new JsonObject().put(EXPORT_TYPE_SPECIFIC_PARAMETERS.getValue(), exportTypeSpecificParameters);
  }
}
