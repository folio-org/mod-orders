package org.folio.service.pieces;

import io.vertx.core.Future;
import io.vertx.core.json.JsonArray;
import io.vertx.core.json.JsonObject;
import io.vertx.junit5.VertxExtension;
import io.vertx.junit5.VertxTestContext;
import org.folio.CopilotGenerated;
import org.folio.rest.acq.model.Organization;
import org.folio.rest.core.RestClient;
import org.folio.rest.core.exceptions.HttpException;
import org.folio.rest.core.models.RequestContext;
import org.folio.rest.core.models.RequestEntry;
import org.folio.rest.jaxrs.model.ClaimingCollection;
import org.folio.rest.jaxrs.model.ClaimingPieceResult;
import org.folio.rest.jaxrs.model.Piece;
import org.folio.rest.jaxrs.model.PoLine;
import org.folio.rest.jaxrs.model.PurchaseOrder;
import org.folio.service.caches.ExportConfigsCache;
import org.folio.service.dataexport.ExportConfigsRetriever;
import org.folio.service.orders.PurchaseOrderLineService;
import org.folio.service.orders.PurchaseOrderStorageService;
import org.folio.service.organization.OrganizationService;
import org.folio.service.pieces.flows.update.PieceUpdateFlowManager;
import org.junit.jupiter.api.AfterEach;
import org.junit.jupiter.api.Assertions;
import org.junit.jupiter.api.BeforeEach;
import org.junit.jupiter.api.Test;
import org.junit.jupiter.api.extension.ExtendWith;
import org.mockito.ArgumentCaptor;
import org.mockito.InjectMocks;
import org.mockito.Mock;
import org.mockito.MockitoAnnotations;
import org.mockito.Spy;

import java.lang.reflect.InvocationTargetException;
import java.util.ArrayList;
import java.util.Arrays;
import java.util.Comparator;
import java.util.List;
import java.util.Map;
import java.util.Set;
import java.util.TreeSet;
import java.util.UUID;
import java.util.stream.Collectors;

import static org.folio.TestUtils.callPrivateMethod;
import static org.folio.models.claiming.IntegrationDetailField.CLAIM_PIECE_IDS;
import static org.folio.models.claiming.IntegrationDetailField.CONFIGS;
import static org.folio.models.claiming.IntegrationDetailField.CONFIG_ID;
import static org.folio.models.claiming.IntegrationDetailField.CONFIG_NAME;
import static org.folio.models.claiming.IntegrationDetailField.EXPORT_TYPE_SPECIFIC_PARAMETERS;
import static org.folio.models.claiming.IntegrationDetailField.TENANT;
import static org.folio.models.claiming.IntegrationDetailField.VENDOR_EDI_ORDERS_EXPORT_CONFIG;
import static org.folio.orders.utils.ResourcePathResolver.DATA_EXPORT_SPRING_CONFIGURATIONS;
import static org.folio.orders.utils.ResourcePathResolver.DATA_EXPORT_SPRING_CREATE_JOB;
import static org.folio.orders.utils.ResourcePathResolver.DATA_EXPORT_SPRING_EFFECTIVE_CONFIGS;
import static org.folio.orders.utils.ResourcePathResolver.resourcesPath;
import static org.folio.rest.core.exceptions.ErrorCodes.CANNOT_FIND_PIECES_WITH_LATE_STATUS_TO_PROCESS;
import static org.folio.rest.core.exceptions.ErrorCodes.CANNOT_RETRIEVE_CONFIG_ENTRIES;
import static org.folio.rest.core.exceptions.ErrorCodes.CANNOT_SEND_CLAIMS_PIECE_IDS_ARE_EMPTY;
import static org.folio.rest.core.exceptions.ErrorCodes.UNABLE_TO_GENERATE_CLAIMS_FOR_ORG_NO_INTEGRATION_DETAILS;
import static org.junit.jupiter.api.Assertions.assertEquals;
import static org.mockito.ArgumentMatchers.any;
import static org.mockito.Mockito.eq;
import static org.mockito.Mockito.mock;
import static org.mockito.Mockito.never;
import static org.mockito.Mockito.times;
import static org.mockito.Mockito.verify;
import static org.mockito.Mockito.when;

@ExtendWith(VertxExtension.class)
@CopilotGenerated(partiallyGenerated = true)
public class PiecesClaimingServiceTest {

  @Mock
  private PieceStorageService pieceStorageService;
  @Mock
  private PurchaseOrderLineService purchaseOrderLineService;
  @Mock
  private PurchaseOrderStorageService purchaseOrderStorageService;
  @Mock
  private OrganizationService organizationService;
  @Mock
  private PieceUpdateFlowManager pieceUpdateFlowManager;
  @Mock
  private RestClient restClient;
  @Mock
  private RequestContext requestContext;

  private PiecesClaimingService piecesClaimingService;

  private AutoCloseable openedMocks;

  @BeforeEach
  void setUp() throws InvocationTargetException, NoSuchMethodException, IllegalAccessException {
    openedMocks = MockitoAnnotations.openMocks(this);

    var exportConfigsCache = new ExportConfigsCache(new ExportConfigsRetriever(restClient));
    callPrivateMethod(exportConfigsCache, "init");

    piecesClaimingService = new PiecesClaimingService(exportConfigsCache, pieceStorageService, purchaseOrderLineService,
      purchaseOrderStorageService, organizationService, pieceUpdateFlowManager, restClient);
  }

  @AfterEach
  void tearDown() throws Exception {
    openedMocks.close();
  }

  @Test
  void testSendClaims_emptyClaimingPieceIds(VertxTestContext testContext) {
    var claimingCollection = new ClaimingCollection().withClaimingPieceIds(List.of());

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
    var claimingCollection = new ClaimingCollection().withClaimingPieceIds(List.of(pieceId1)).withClaimingInterval(1);
    mockExportConfigs();
    when(pieceStorageService.getPiecesByIds(any(), any())).thenReturn(Future.succeededFuture(List.of(new Piece().withId(pieceId1).withPoLineId("poLineId1").withReceivingStatus(Piece.ReceivingStatus.LATE))));
    when(purchaseOrderLineService.getOrderLineById(any(), any())).thenReturn(Future.succeededFuture(new PoLine().withPurchaseOrderId("orderId1")));
    when(purchaseOrderStorageService.getPurchaseOrderById(any(), any())).thenReturn(Future.succeededFuture(new PurchaseOrder().withVendor("vendorId")));
    when(organizationService.getVendorById(any(), any())).thenReturn(Future.succeededFuture(new Organization().withId("vendorId").withCode("VENDOR").withIsVendor(true)));

    piecesClaimingService.sendClaims(claimingCollection, requestContext)
      .onComplete(testContext.failing(throwable -> testContext.verify(() -> {
        Assertions.assertInstanceOf(HttpException.class, throwable);
        var httpException = (HttpException) throwable;
        var error = httpException.getErrors().getErrors().getFirst();
        verifyNoClaimSent();
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
    var claimingCollection = new ClaimingCollection().withClaimingPieceIds(List.of(pieceId1)).withClaimingInterval(1);

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
    var claimingCollection = new ClaimingCollection().withClaimingPieceIds(List.of("pieceId1")).withClaimingInterval(1);
    mockExportConfigs("CLAIMS_vendorId");

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
    var claimingCollection = new ClaimingCollection().withClaimingPieceIds(List.of(pieceId));
    mockExportConfigs("CLAIMS_vendorId1");

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
    var claimingCollection = new ClaimingCollection().withClaimingPieceIds(List.of(pieceId1, pieceId2, pieceId3));
    mockExportConfigs("CLAIMS_vendorId1");

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
    var claimingCollection = new ClaimingCollection().withClaimingPieceIds(List.of("pieceId1")).withClaimingInterval(1);
    mockExportConfigs("CLAIMS_vendorId");

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
    var claimingCollection = new ClaimingCollection().withClaimingPieceIds(List.of("pieceId1", "pieceId2")).withClaimingInterval(1);
    mockExportConfigs("CLAIMS_vendorId");

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
    var claimingCollection = new ClaimingCollection().withClaimingPieceIds(List.of("pieceId1", "pieceId2", "pieceId3", "pieceId4", "pieceId5")).withClaimingInterval(1);
    mockExportConfigs("CLAIMS_vendorId1", "CLAIMS_vendorId2");


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
        verify(pieceUpdateFlowManager).updatePiecesStatuses(eq(List.of("pieceId1", "pieceId2", "pieceId3", "pieceId4", "pieceId5")), any(), any(), any(), any(), any());
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
    var claimingCollection = new ClaimingCollection().withClaimingPieceIds(List.of("pieceId1", "pieceId2", "pieceId3", "pieceId4", "pieceId5")).withClaimingInterval(1);
    mockExportConfigs("CLAIMS_vendorId1", "CLAIMS_vendorId2");


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
  void testSendClaims_integrationsByAcquisitionMethod_eachJobOnlyItsPieces(VertxTestContext testContext) {
    var claimingCollection = new ClaimingCollection().withClaimingPieceIds(List.of("pieceId1", "pieceId2", "pieceId3")).withClaimingInterval(1);
    mockConfigsByAcquisitionMethod(Map.of("purchase", List.of("CLAIMS_vendorId_x"), "exchange", List.of("CLAIMS_vendorId_y")));
    mockClaimFlow(Map.of("pieceId1", "purchase", "pieceId2", "exchange", "pieceId3", "purchase"));

    piecesClaimingService.sendClaims(claimingCollection, requestContext)
      .onComplete(testContext.succeeding(result -> testContext.verify(() -> {
        assertEquals(3, result.getClaimingPieceResults().size());
        assertEquals(Map.of("CLAIMS_vendorId_x", Set.of("pieceId1", "pieceId3"), "CLAIMS_vendorId_y", Set.of("pieceId2")), createdJobs(2));
        verify(restClient, times(2)).getAsJsonObject(any(), any());
        testContext.completeNow();
      })));
  }

  @Test
  void testSendClaims_oneIntegrationForTwoMethods_oneJobWithAllPieces(VertxTestContext testContext) {
    var claimingCollection = new ClaimingCollection().withClaimingPieceIds(List.of("pieceId1", "pieceId2")).withClaimingInterval(1);
    mockConfigsByAcquisitionMethod(Map.of("purchase", List.of("CLAIMS_vendorId_x"), "exchange", List.of("CLAIMS_vendorId_x")));
    mockClaimFlow(Map.of("pieceId1", "purchase", "pieceId2", "exchange"));

    piecesClaimingService.sendClaims(claimingCollection, requestContext)
      .onComplete(testContext.succeeding(result -> testContext.verify(() -> {
        assertEquals(2, result.getClaimingPieceResults().size());
        assertEquals(Map.of("CLAIMS_vendorId_x", Set.of("pieceId1", "pieceId2")), createdJobs(1));
        testContext.completeNow();
      })));
  }

  @Test
  void testSendClaims_twoIntegrationsForOnePiece_pieceInBothJobsUpdatedOnce(VertxTestContext testContext) {
    var claimingCollection = new ClaimingCollection().withClaimingPieceIds(List.of("pieceId1")).withClaimingInterval(1);
    mockConfigsByAcquisitionMethod(Map.of("purchase", List.of("CLAIMS_vendorId_x", "CLAIMS_vendorId_y")));
    mockClaimFlow(Map.of("pieceId1", "purchase"));

    piecesClaimingService.sendClaims(claimingCollection, requestContext)
      .onComplete(testContext.succeeding(result -> testContext.verify(() -> {
        assertEquals(1, result.getClaimingPieceResults().size());
        assertEquals(Map.of("CLAIMS_vendorId_x", Set.of("pieceId1"), "CLAIMS_vendorId_y", Set.of("pieceId1")), createdJobs(2));
        verify(pieceUpdateFlowManager).updatePiecesStatuses(eq(List.of("pieceId1")), any(), any(), any(), any(), any());
        testContext.completeNow();
      })));
  }

  @Test
  void testSendClaims_samePoLineValues_oneConfigRequest(VertxTestContext testContext) {
    var claimingCollection = new ClaimingCollection().withClaimingPieceIds(List.of("pieceId1", "pieceId2")).withClaimingInterval(1);
    mockConfigsByAcquisitionMethod(Map.of("purchase", List.of("CLAIMS_vendorId_x")));
    mockClaimFlow(Map.of("pieceId1", "purchase", "pieceId2", "purchase"));

    piecesClaimingService.sendClaims(claimingCollection, requestContext)
      .onComplete(testContext.succeeding(result -> testContext.verify(() -> {
        var requests = ArgumentCaptor.forClass(RequestEntry.class);
        verify(restClient).getAsJsonObject(requests.capture(), any());
        assertEquals(resourcesPath(DATA_EXPORT_SPRING_EFFECTIVE_CONFIGS), requests.getValue().getBaseEndpoint());
        assertEquals(Map.of("vendorId", "vendorId", "acquisitionMethod", "purchase", "type", "CLAIMS"), requests.getValue().getQueryParams());
        testContext.completeNow();
      })));
  }

  @Test
  void testSendClaims_poLineWithoutAcquisitionMethod_parameterLeftOut(VertxTestContext testContext) {
    var claimingCollection = new ClaimingCollection().withClaimingPieceIds(List.of("pieceId1")).withClaimingInterval(1);
    mockExportConfigs("CLAIMS_vendorId");
    mockClaimFlow(Map.of());

    piecesClaimingService.sendClaims(claimingCollection, requestContext)
      .onComplete(testContext.succeeding(result -> testContext.verify(() -> {
        var requests = ArgumentCaptor.forClass(RequestEntry.class);
        verify(restClient).getAsJsonObject(requests.capture(), any());
        assertEquals(Map.of("vendorId", "vendorId", "type", "CLAIMS"), requests.getValue().getQueryParams());
        assertEquals(Map.of("CLAIMS_vendorId", Set.of("pieceId1")), createdJobs(1));
        testContext.completeNow();
      })));
  }

  @Test
  void testSendClaims_oneMethodWithoutIntegration_rejectedBeforeAnyWrite(VertxTestContext testContext) {
    var claimingCollection = new ClaimingCollection().withClaimingPieceIds(List.of("pieceId1", "pieceId2")).withClaimingInterval(1);
    mockConfigsByAcquisitionMethod(Map.of("purchase", List.of("CLAIMS_vendorId_x")));
    mockClaimFlow(Map.of("pieceId1", "purchase", "pieceId2", "exchange"));

    piecesClaimingService.sendClaims(claimingCollection, requestContext)
      .onComplete(testContext.failing(throwable -> testContext.verify(() -> {
        var error = ((HttpException) throwable).getErrors().getErrors().getFirst();
        assertEquals(UNABLE_TO_GENERATE_CLAIMS_FOR_ORG_NO_INTEGRATION_DETAILS.getCode(), error.getCode());
        verifyNoClaimSent();
        // At least one piece matched, so no lookup of all Claims configs is needed
        verify(restClient, times(2)).getAsJsonObject(any(), any());
        testContext.completeNow();
      })));
  }

  private void verifyNoClaimSent() {
    verify(pieceUpdateFlowManager, never()).updatePiecesStatuses(any(), any(), any(), any(), any(), any());
    verify(restClient, never()).post(eq(resourcesPath(DATA_EXPORT_SPRING_CREATE_JOB)), any(), any(), any());
  }

  /**
   * One vendor "vendorId" with one PO line per piece, the PO line id being the piece id. Pieces missing in the map have
   * a PO line without acquisition method; "pieceId1" is always returned.
   */
  private void mockClaimFlow(Map<String, String> acquisitionMethodByPieceId) {
    var pieceIds = new TreeSet<>(acquisitionMethodByPieceId.keySet());
    pieceIds.add("pieceId1");
    var pieces = pieceIds.stream()
      .map(pieceId -> new Piece().withId(pieceId).withPoLineId(pieceId).withReceivingStatus(Piece.ReceivingStatus.LATE))
      .toList();
    when(pieceStorageService.getPiecesByIds(any(), any())).thenReturn(Future.succeededFuture(pieces));
    when(purchaseOrderLineService.getOrderLineById(any(), any())).thenAnswer(invocation -> Future.succeededFuture(
      new PoLine().withPurchaseOrderId("orderId").withAcquisitionMethod(acquisitionMethodByPieceId.get(invocation.<String>getArgument(0)))));
    when(purchaseOrderStorageService.getPurchaseOrderById(any(), any())).thenReturn(Future.succeededFuture(new PurchaseOrder().withVendor("vendorId")));
    when(organizationService.getVendorById(any(), any())).thenReturn(Future.succeededFuture(new Organization().withId("vendorId").withCode("VENDOR").withIsVendor(true)));
    when(pieceUpdateFlowManager.updatePiecesStatuses(any(), any(), any(), any(), any(), any())).thenReturn(Future.succeededFuture());
    when(restClient.post(eq(resourcesPath(DATA_EXPORT_SPRING_CREATE_JOB)), any(), any(), any())).thenReturn(Future.succeededFuture(new JsonObject().put("status", "CREATED")));
  }

  private Map<String, Set<String>> createdJobs(int expectedJobs) {
    var jobs = ArgumentCaptor.forClass(Object.class);
    verify(restClient, times(expectedJobs)).post(eq(resourcesPath(DATA_EXPORT_SPRING_CREATE_JOB)), jobs.capture(), any(), any());
    return jobs.getAllValues().stream().map(JsonObject.class::cast).collect(Collectors.toMap(
      job -> job.getString(CONFIG_NAME.getValue()),
      job -> job.getJsonObject(EXPORT_TYPE_SPECIFIC_PARAMETERS.getValue())
        .getJsonObject(VENDOR_EDI_ORDERS_EXPORT_CONFIG.getValue())
        .getJsonArray(CLAIM_PIECE_IDS.getValue()).stream().map(String::valueOf).collect(Collectors.toSet())));
  }

  /**
   * Answers like mod-data-export-spring with the vendor-only Claims rule: /effective-configs returns the configs named
   * CLAIMS_&lt;vendorId&gt; or CLAIMS_&lt;vendorId&gt;_*, and /configs returns all of them.
   */
  private void mockExportConfigs(String... configNames) {
    when(restClient.getAsJsonObject(any(), any())).thenAnswer(invocation -> {
      RequestEntry requestEntry = invocation.getArgument(0);
      if (resourcesPath(DATA_EXPORT_SPRING_CONFIGURATIONS).equals(requestEntry.getBaseEndpoint())) {
        return Future.succeededFuture(createExportConfigs(configNames));
      }
      var configName = "CLAIMS_" + requestEntry.getQueryParams().get("vendorId");
      return Future.succeededFuture(createExportConfigs(Arrays.stream(configNames)
        .filter(name -> name.equals(configName) || name.startsWith(configName + "_"))
        .toArray(String[]::new)));
    });
  }

  private void mockConfigsByAcquisitionMethod(Map<String, List<String>> configNamesByMethod) {
    when(restClient.getAsJsonObject(any(), any())).thenAnswer(invocation -> {
      RequestEntry requestEntry = invocation.getArgument(0);
      var configNames = configNamesByMethod.getOrDefault(requestEntry.getQueryParams().get("acquisitionMethod"), List.of());
      return Future.succeededFuture(createExportConfigs(configNames.toArray(String[]::new)));
    });
  }

  private JsonObject createExportConfigs(String... configNames) {
    return JsonObject.of(CONFIGS.getValue(), JsonArray.of(Arrays.stream(configNames).map(this::createIntegrationDetail).toArray()));
  }

  private Object createIntegrationDetail(String configName) {
    var vendorEditOrdersExportConfig = JsonObject.of(
      CLAIM_PIECE_IDS.getValue(), List.of(),
      CONFIG_NAME.getValue(), configName);
    var exportTypeSpecificParameters = JsonObject.of(VENDOR_EDI_ORDERS_EXPORT_CONFIG.getValue(), vendorEditOrdersExportConfig);
    return JsonObject.of(
      CONFIG_ID.getValue(), "id-" + configName,
      TENANT.getValue(), "folio_shared",
      CONFIG_NAME.getValue(), configName,
      EXPORT_TYPE_SPECIFIC_PARAMETERS.getValue(), exportTypeSpecificParameters);
  }

}
