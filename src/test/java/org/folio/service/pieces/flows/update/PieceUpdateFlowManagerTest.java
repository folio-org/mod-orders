package org.folio.service.pieces.flows.update;

import static io.vertx.core.Future.succeededFuture;
import static org.folio.TestConfig.autowireDependencies;
import static org.folio.TestConfig.clearServiceInteractions;
import static org.folio.TestConfig.clearVertxContext;
import static org.folio.TestConfig.getFirstContextFromVertx;
import static org.folio.TestConfig.getVertx;
import static org.folio.TestConfig.initSpringContext;
import static org.folio.TestConfig.isVerticleNotDeployed;
import static org.folio.TestConstants.ID;
import static org.folio.rest.jaxrs.model.Eresource.CreateInventory.INSTANCE_HOLDING_ITEM;
import static org.folio.rest.jaxrs.model.Piece.ReceivingStatus.EXPECTED;
import static org.folio.rest.jaxrs.model.Piece.ReceivingStatus.RECEIVED;
import static org.folio.rest.jaxrs.model.Piece.ReceivingStatus.UNRECEIVABLE;
import static org.folio.service.inventory.InventoryHoldingManager.HOLDING_PERMANENT_LOCATION_ID;
import static org.folio.service.inventory.InventoryItemManager.ITEM_STATUS;
import static org.folio.service.inventory.InventoryItemManager.ITEM_STATUS_NAME;
import static org.folio.service.orders.utils.StatusUtils.calculatePoLineReceiptStatus;
import static org.junit.jupiter.api.Assertions.*;
import static org.mockito.ArgumentMatchers.any;
import static org.mockito.ArgumentMatchers.anyString;
import static org.mockito.ArgumentMatchers.eq;
import static org.mockito.Mockito.doAnswer;
import static org.mockito.Mockito.doNothing;
import static org.mockito.Mockito.doReturn;
import static org.mockito.Mockito.mock;
import static org.mockito.Mockito.spy;
import static org.mockito.Mockito.times;
import static org.mockito.Mockito.verify;

import java.util.Arrays;
import java.util.List;
import java.util.Map;
import java.util.UUID;
import java.util.concurrent.ExecutionException;
import java.util.concurrent.TimeoutException;

import org.folio.ApiTestSuite;
import org.folio.models.ItemStatus;
import org.folio.models.pieces.PieceUpdateHolder;
import org.folio.orders.utils.PoLineCommonUtil;
import org.folio.orders.utils.ProtectedOperationType;
import org.folio.rest.core.RestClient;
import org.folio.rest.core.models.RequestContext;
import org.folio.rest.jaxrs.model.CompositePoLine;
import org.folio.rest.jaxrs.model.Cost;
import org.folio.rest.jaxrs.model.Eresource;
import org.folio.rest.jaxrs.model.Location;
import org.folio.rest.jaxrs.model.Piece;
import org.folio.rest.jaxrs.model.PoLine;
import org.folio.rest.jaxrs.model.PurchaseOrder;
import org.folio.rest.jaxrs.model.Title;
import org.folio.service.ProtectionService;
import org.folio.service.caches.InventoryCache;
import org.folio.service.orders.PurchaseOrderLineService;
import org.folio.service.pieces.PieceService;
import org.folio.service.pieces.PieceStorageService;
import org.folio.service.pieces.PieceUtil;
import org.folio.service.pieces.flows.BasePieceFlowHolderBuilder;
import org.folio.service.pieces.flows.DefaultPieceFlowsValidator;
import org.junit.jupiter.api.AfterAll;
import org.junit.jupiter.api.AfterEach;
import org.junit.jupiter.api.BeforeAll;
import org.junit.jupiter.api.BeforeEach;
import org.junit.jupiter.api.Test;
import org.junit.jupiter.api.extension.ExtendWith;
import org.mockito.ArgumentCaptor;
import org.mockito.Mock;
import org.mockito.Mockito;
import org.mockito.MockitoAnnotations;
import org.mockito.stubbing.Answer;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.context.annotation.Bean;

import io.vertx.core.Context;
import io.vertx.core.Future;
import io.vertx.core.json.JsonObject;
import io.vertx.junit5.VertxExtension;

@ExtendWith(VertxExtension.class)
public class PieceUpdateFlowManagerTest {
  @Autowired PieceUpdateFlowManager pieceUpdateFlowManager;
  @Autowired PieceStorageService pieceStorageService;
  @Autowired ProtectionService protectionService;
  @Autowired PieceUpdateFlowInventoryManager pieceUpdateFlowInventoryManager;
  @Autowired PieceService pieceService;
  @Autowired BasePieceFlowHolderBuilder basePieceFlowHolderBuilder;
  @Autowired PieceUpdateFlowPoLineService pieceUpdateFlowPoLineService;
  @Autowired PurchaseOrderLineService purchaseOrderLineService;

  private final Context ctx = getFirstContextFromVertx(getVertx());
  @Mock
  private Map<String, String> okapiHeadersMock;

  private RequestContext requestContext;
  private static boolean runningOnOwn;

  @BeforeEach
  void initMocks(){
    MockitoAnnotations.openMocks(this);
    autowireDependencies(this);
    requestContext = new RequestContext(ctx, okapiHeadersMock);
  }

  @BeforeAll
  public static void before() throws InterruptedException, ExecutionException, TimeoutException {
    if (isVerticleNotDeployed()) {
      ApiTestSuite.before();
      runningOnOwn = true;
    }
    initSpringContext(PieceUpdateFlowManagerTest.ContextConfiguration.class);
  }

  @AfterAll
  public static void after() {
    clearVertxContext();
    if (runningOnOwn) {
      ApiTestSuite.after();
    }
  }

  @AfterEach
  void resetMocks() {
    clearServiceInteractions();
    Mockito.reset(pieceStorageService, pieceService, protectionService, pieceUpdateFlowPoLineService,
      pieceUpdateFlowInventoryManager, basePieceFlowHolderBuilder, purchaseOrderLineService);
  }

  @Test
  void shouldNotUpdateLineQuantityIfPoLineIsPackageAndShouldRunProcessInventory() {
    String orderId = UUID.randomUUID().toString();
    String holdingId = UUID.randomUUID().toString();
    String holdingIdTpUpdate = UUID.randomUUID().toString();
    String lineId = UUID.randomUUID().toString();
    String titleId = UUID.randomUUID().toString();
    String itemId = UUID.randomUUID().toString();
    String pieceId = UUID.randomUUID().toString();
    String locationId = UUID.randomUUID().toString();

    JsonObject item = new JsonObject().put(ID, itemId);
    item.put(ITEM_STATUS, new JsonObject().put(ITEM_STATUS_NAME, ItemStatus.ON_ORDER.value()));
    JsonObject holding = new JsonObject().put(ID, holdingId);
    holding.put(HOLDING_PERMANENT_LOCATION_ID, locationId);
    Piece pieceFromStorage = new Piece().withId(pieceId).withPoLineId(lineId).withItemId(itemId).withTitleId(titleId)
      .withHoldingId(holdingId).withFormat(Piece.Format.ELECTRONIC);
    Piece pieceToUpdate = new Piece().withId(pieceId).withPoLineId(lineId).withItemId(itemId).withTitleId(titleId)
      .withHoldingId(holdingIdTpUpdate).withFormat(Piece.Format.ELECTRONIC);
    Cost cost = new Cost().withQuantityElectronic(1);
    Location loc = new Location().withHoldingId(holdingId).withQuantityElectronic(1).withQuantity(1);
    Eresource eresource = new Eresource().withCreateInventory(INSTANCE_HOLDING_ITEM);
    PoLine poLine = new PoLine().withIsPackage(true).withPurchaseOrderId(orderId).withId(lineId)
      .withOrderFormat(PoLine.OrderFormat.ELECTRONIC_RESOURCE)
      .withEresource(eresource)
      .withLocations(List.of(loc)).withCost(cost);
    PurchaseOrder purchaseOrder = new PurchaseOrder().withId(orderId)
      .withOrderType(PurchaseOrder.OrderType.ONE_TIME).withWorkflowStatus(PurchaseOrder.WorkflowStatus.OPEN);
    Title title = new Title().withId(titleId);

    doReturn(succeededFuture(pieceFromStorage)).when(pieceStorageService).getPieceById(pieceToUpdate.getId(), requestContext);
    doReturn(succeededFuture(null)).when(pieceStorageService).updatePiece(pieceToUpdate, requestContext);
    givenPoLineHasPieces(lineId, List.of());

    final ArgumentCaptor<PieceUpdateHolder> pieceUpdateHolderCapture = ArgumentCaptor.forClass(PieceUpdateHolder.class);
    doAnswer((Answer<Future<Void>>) invocation -> {
      PieceUpdateHolder answerHolder = invocation.getArgument(0);
      answerHolder.withOrderInformation(purchaseOrder, poLine);
      return succeededFuture(null);
    }).when(basePieceFlowHolderBuilder).updateHolderWithOrderInformation(pieceUpdateHolderCapture.capture(), eq(requestContext));
    doAnswer((Answer<Future<Void>>) invocation -> {
      PieceUpdateHolder answerHolder = invocation.getArgument(0);
      answerHolder.withTitleInformation(title);
      return succeededFuture(null);
    }).when(basePieceFlowHolderBuilder).updateHolderWithTitleInformation(pieceUpdateHolderCapture.capture(), eq(requestContext));

    doReturn(succeededFuture(null)).when(protectionService).isOperationRestricted(any(), any(ProtectedOperationType.class), eq(requestContext));
    doReturn(succeededFuture(null)).when(pieceUpdateFlowInventoryManager).processInventory(any(PieceUpdateHolder.class), eq(requestContext));
    doNothing().when(pieceService).receiptConsistencyPiecePoLine(anyString(), eq(requestContext));
    doReturn(succeededFuture(null)).when(pieceUpdateFlowPoLineService).updatePoLine(pieceUpdateHolderCapture.capture(), eq(requestContext));
    doReturn(succeededFuture(null))
      .when(purchaseOrderLineService).saveOrderLine(any(CompositePoLine.class),
        eq(PieceUtil.findOrderPieceLineLocation(pieceToUpdate, PoLineCommonUtil.convertToCompositePoLine(poLine))),
        eq(requestContext));

    //When
    pieceUpdateFlowManager.updatePiece(pieceToUpdate, true, true, requestContext).result();
    //Then
    PieceUpdateHolder holder = pieceUpdateHolderCapture.getValue();
    assertNull(poLine.getLocations().get(0).getLocationId());
    assertEquals(holdingId, poLine.getLocations().get(0).getHoldingId());
    verify(basePieceFlowHolderBuilder).updateHolderWithOrderInformation(holder, requestContext);
    verify(pieceUpdateFlowPoLineService, times(0)).updatePoLine(pieceUpdateHolderCapture.capture(), eq(requestContext));
    verify(pieceUpdateFlowInventoryManager).processInventory(any(PieceUpdateHolder.class), eq(requestContext));
    verify(pieceStorageService).updatePiece(pieceToUpdate, requestContext);
  }

  @Test
  void shouldNotUpdateLineQuantityIfManualPieceCreateTrueAndShouldRunProcessInventory() {
    String orderId = UUID.randomUUID().toString();
    String holdingId = UUID.randomUUID().toString();
    String holdingIdTpUpdate = UUID.randomUUID().toString();
    String lineId = UUID.randomUUID().toString();
    String titleId = UUID.randomUUID().toString();
    String itemId = UUID.randomUUID().toString();
    String locationId = UUID.randomUUID().toString();
    String pieceId = UUID.randomUUID().toString();

    JsonObject item = new JsonObject().put(ID, itemId);
    item.put(ITEM_STATUS, new JsonObject().put(ITEM_STATUS_NAME, ItemStatus.ON_ORDER.value()));
    JsonObject holding = new JsonObject().put(ID, holdingId);
    holding.put(HOLDING_PERMANENT_LOCATION_ID, locationId);
    Piece pieceFromStorage = new Piece().withId(pieceId).withPoLineId(lineId).withItemId(itemId).withTitleId(titleId)
      .withHoldingId(holdingId).withFormat(Piece.Format.ELECTRONIC);
    Piece pieceToUpdate = new Piece().withId(pieceId).withPoLineId(lineId).withItemId(itemId).withTitleId(titleId)
      .withHoldingId(holdingIdTpUpdate).withFormat(Piece.Format.ELECTRONIC);
    Cost cost = new Cost().withQuantityElectronic(1);
    Location loc = new Location().withHoldingId(holdingId).withQuantityElectronic(1).withQuantity(1);
    Eresource eresource = new Eresource().withCreateInventory(INSTANCE_HOLDING_ITEM);
    PoLine poLine = new PoLine().withIsPackage(false).withCheckinItems(true).withPurchaseOrderId(orderId).withId(lineId)
      .withOrderFormat(PoLine.OrderFormat.ELECTRONIC_RESOURCE)
      .withEresource(eresource)
      .withLocations(List.of(loc)).withCost(cost);
    PurchaseOrder purchaseOrder = new PurchaseOrder().withId(orderId).withWorkflowStatus(PurchaseOrder.WorkflowStatus.OPEN);
    Title title = new Title().withId(titleId);

    doReturn(succeededFuture(pieceFromStorage)).when(pieceStorageService).getPieceById(pieceToUpdate.getId(), requestContext);
    doReturn(succeededFuture(null)).when(pieceStorageService).updatePiece(pieceToUpdate, requestContext);
    givenPoLineHasPieces(lineId, List.of());

    final ArgumentCaptor<PieceUpdateHolder> pieceUpdateHolderCapture = ArgumentCaptor.forClass(PieceUpdateHolder.class);
    doAnswer((Answer<Future<Void>>) invocation -> {
      PieceUpdateHolder answerHolder = invocation.getArgument(0);
      answerHolder.withOrderInformation(purchaseOrder, poLine);
      return succeededFuture(null);
    }).when(basePieceFlowHolderBuilder).updateHolderWithOrderInformation(pieceUpdateHolderCapture.capture(), eq(requestContext));
    doAnswer((Answer<Future<Void>>) invocation -> {
      PieceUpdateHolder answerHolder = invocation.getArgument(0);
      answerHolder.withTitleInformation(title);
      return succeededFuture(null);
    }).when(basePieceFlowHolderBuilder).updateHolderWithTitleInformation(pieceUpdateHolderCapture.capture(), eq(requestContext));

    doReturn(succeededFuture(null)).when(protectionService).isOperationRestricted(any(), any(ProtectedOperationType.class), eq(requestContext));
    doReturn(succeededFuture(null)).when(pieceUpdateFlowInventoryManager).processInventory(any(PieceUpdateHolder.class), eq(requestContext));
    doNothing().when(pieceService).receiptConsistencyPiecePoLine(anyString(), eq(requestContext));
    doReturn(succeededFuture(null)).when(pieceUpdateFlowPoLineService).updatePoLine(pieceUpdateHolderCapture.capture(), eq(requestContext));
    //When
    pieceUpdateFlowManager.updatePiece(pieceToUpdate, true, true, requestContext).result();
    //Then
    PieceUpdateHolder holder = pieceUpdateHolderCapture.getValue();
    assertNull(poLine.getLocations().get(0).getLocationId());
    assertEquals(holdingId, poLine.getLocations().get(0).getHoldingId());
    verify(basePieceFlowHolderBuilder).updateHolderWithOrderInformation(holder, requestContext);
    verify(pieceUpdateFlowPoLineService, times(0)).updatePoLine(pieceUpdateHolderCapture.capture(), eq(requestContext));
    verify(pieceUpdateFlowInventoryManager).processInventory(any(PieceUpdateHolder.class), eq(requestContext));
    verify(pieceStorageService).updatePiece(pieceToUpdate, requestContext);
  }

  @Test
  void shouldUpdateLineQuantityIfPoLineIsNotPackageAndHoldingReferenceChangedAndShouldRunProcessInventory() {
    String orderId = UUID.randomUUID().toString();
    String oldHoldingId = UUID.randomUUID().toString();
    String holdingIdToUpdate = UUID.randomUUID().toString();
    String lineId = UUID.randomUUID().toString();
    String titleId = UUID.randomUUID().toString();
    String itemId = UUID.randomUUID().toString();
    String locationId = UUID.randomUUID().toString();
    String pieceId = UUID.randomUUID().toString();

    JsonObject item = new JsonObject().put(ID, itemId);
    item.put(ITEM_STATUS, new JsonObject().put(ITEM_STATUS_NAME, ItemStatus.ON_ORDER.value()));
    JsonObject holding = new JsonObject().put(ID, oldHoldingId);
    holding.put(HOLDING_PERMANENT_LOCATION_ID, locationId);
    Piece pieceFromStorage = new Piece().withId(pieceId).withPoLineId(lineId).withItemId(itemId).withTitleId(titleId)
      .withHoldingId(oldHoldingId)
      .withFormat(Piece.Format.ELECTRONIC);
    Piece incomingPieceToUpdate = new Piece().withId(pieceId).withPoLineId(lineId).withItemId(itemId).withTitleId(titleId)
      .withHoldingId(holdingIdToUpdate)
      .withFormat(Piece.Format.ELECTRONIC).withReceivingStatus(Piece.ReceivingStatus.RECEIVED);
    Cost cost = new Cost().withQuantityElectronic(1);
    Location loc = new Location().withHoldingId(oldHoldingId).withQuantityElectronic(1).withQuantity(1);
    PoLine poLine = new PoLine().withIsPackage(false).withPurchaseOrderId(orderId).withId(lineId)
      .withEresource(new Eresource().withCreateInventory(INSTANCE_HOLDING_ITEM))
      .withOrderFormat(PoLine.OrderFormat.ELECTRONIC_RESOURCE).withLocations(List.of(loc)).withCost(cost);
    PurchaseOrder purchaseOrder = new PurchaseOrder().withId(orderId).withWorkflowStatus(PurchaseOrder.WorkflowStatus.OPEN);
    Title title = new Title().withId(titleId);

    doReturn(succeededFuture(pieceFromStorage)).when(pieceStorageService).getPieceById(incomingPieceToUpdate.getId(), requestContext);
    final ArgumentCaptor<Piece> pieceToUpdateCapture = ArgumentCaptor.forClass(Piece.class);
    doReturn(succeededFuture(null)).when(pieceStorageService).updatePiece(pieceToUpdateCapture.capture(), eq(requestContext));
    givenPoLineHasPieces(lineId, List.of());
    doNothing().when(pieceService).receiptConsistencyPiecePoLine(anyString(), eq(requestContext));

    final ArgumentCaptor<PieceUpdateHolder> pieceUpdateHolderCapture = ArgumentCaptor.forClass(PieceUpdateHolder.class);
    doAnswer((Answer<Future<Void>>) invocation -> {
      PieceUpdateHolder answerHolder = invocation.getArgument(0);
      answerHolder.withOrderInformation(purchaseOrder, poLine);
      return succeededFuture(null);
    }).when(basePieceFlowHolderBuilder).updateHolderWithOrderInformation(pieceUpdateHolderCapture.capture(), eq(requestContext));
    doAnswer((Answer<Future<Void>>) invocation -> {
      PieceUpdateHolder answerHolder = invocation.getArgument(0);
      answerHolder.withTitleInformation(title);
      return succeededFuture(null);
    }).when(basePieceFlowHolderBuilder).updateHolderWithTitleInformation(pieceUpdateHolderCapture.capture(), eq(requestContext));

    doReturn(succeededFuture(null)).when(protectionService).isOperationRestricted(any(), any(ProtectedOperationType.class), eq(requestContext));
    doReturn(succeededFuture(null)).when(pieceUpdateFlowInventoryManager).processInventory(pieceUpdateHolderCapture.capture(), eq(requestContext));
    doReturn(succeededFuture(null)).when(pieceUpdateFlowPoLineService).updatePoLine(pieceUpdateHolderCapture.capture(), eq(requestContext));

    //When
    pieceUpdateFlowManager.updatePiece(incomingPieceToUpdate, true, true, requestContext).result();
    //Then
    Piece pieceToUpdate = pieceToUpdateCapture.getValue();
    PieceUpdateHolder pieceUpdateHolder = pieceUpdateHolderCapture.getValue();

    assertNotNull(pieceToUpdate.getStatusUpdatedDate());
    verify(basePieceFlowHolderBuilder).updateHolderWithOrderInformation(pieceUpdateHolderCapture.capture(), eq(requestContext));
    verify(pieceUpdateFlowPoLineService).updatePoLine(pieceUpdateHolderCapture.capture(), eq(requestContext));
    verify(pieceUpdateFlowInventoryManager).processInventory(any(PieceUpdateHolder.class), eq(requestContext));
    verify(pieceStorageService).updatePiece(pieceToUpdate, requestContext);
  }

  @Test
  void poLineStatusWhenReceiveLast() {
    // given
    String poLineId = UUID.randomUUID().toString();
    List<Piece> fromStorage = givenPieces(EXPECTED, RECEIVED, UNRECEIVABLE);
    List<Piece> update = List.of(new Piece().withId(fromStorage.get(0).getId()).withReceivingStatus(RECEIVED));

    // when
    var receiptStatus = calculatePoLineReceiptStatus(poLineId, fromStorage, update);

    // then
    assertEquals(CompositePoLine.ReceiptStatus.FULLY_RECEIVED, receiptStatus);
  }

  @Test
  void poLineStatusWhenExpectLast() {
    // given
    String poLineId = UUID.randomUUID().toString();
    List<Piece> fromStorage = givenPieces(RECEIVED, RECEIVED, UNRECEIVABLE);
    List<Piece> update = List.of(new Piece().withId(fromStorage.get(0).getId()).withReceivingStatus(EXPECTED));

    // when
    var receiptStatus = calculatePoLineReceiptStatus(poLineId, fromStorage, update);

    // then
    assertEquals(CompositePoLine.ReceiptStatus.PARTIALLY_RECEIVED, receiptStatus);
  }

  @Test
  void poLineStatusWhenExpectAll() {
    // given
    String poLineId = UUID.randomUUID().toString();
    List<Piece> fromStorage = givenPieces(RECEIVED);
    List<Piece> update = List.of(new Piece().withId(fromStorage.get(0).getId()).withReceivingStatus(EXPECTED));

    // when
    var receiptStatus = calculatePoLineReceiptStatus(poLineId, fromStorage, update);

    // then
    assertEquals(CompositePoLine.ReceiptStatus.AWAITING_RECEIPT, receiptStatus);
  }

  @Test
  void poLineStatusWhenReceivePart() {
    // given
    String poLineId = UUID.randomUUID().toString();
    List<Piece> fromStorage = givenPieces(EXPECTED, EXPECTED);
    List<Piece> update = List.of(new Piece().withId(fromStorage.get(0).getId()).withReceivingStatus(UNRECEIVABLE));

    // when
    var receiptStatus = calculatePoLineReceiptStatus(poLineId, fromStorage, update);

    // then
    assertEquals(CompositePoLine.ReceiptStatus.PARTIALLY_RECEIVED, receiptStatus);
  }

  private static List<Piece> givenPieces(Piece.ReceivingStatus... statuses) {
    return Arrays.stream(statuses).map(status ->
      new Piece().withId(UUID.randomUUID().toString()).withReceivingStatus(status)
    ).toList();
  }

  private void givenPoLineHasPieces(String lineId, List<Piece> pieces) {
    doReturn(succeededFuture(pieces))
      .when(pieceStorageService)
      .getPiecesByLineId(eq(lineId), eq(requestContext));
  }

  private static class ContextConfiguration {
    @Bean PieceStorageService pieceStorageService() {
      return mock(PieceStorageService.class);
    }
    @Bean ProtectionService protectionService() {
      return mock(ProtectionService.class);
    }
    @Bean PieceService pieceService() {
      return mock(PieceService.class);
    }
    @Bean PieceUpdateFlowInventoryManager pieceUpdateFlowInventoryManager() {
      return mock(PieceUpdateFlowInventoryManager.class);
    }
    @Bean BasePieceFlowHolderBuilder basePieceFlowHolderBuilder() {
      return mock(BasePieceFlowHolderBuilder.class);
    }
    @Bean PieceUpdateFlowPoLineService pieceUpdateFlowPoLineService() {
      return  mock(PieceUpdateFlowPoLineService.class);
    }
    @Bean DefaultPieceFlowsValidator defaultPieceFlowsValidator() {
      return spy(new DefaultPieceFlowsValidator());
    }

    @Bean RestClient restClient() {
      return mock(RestClient.class);
    }

    @Bean InventoryCache inventoryCache() {
      return mock(InventoryCache.class);
    }

    @Bean PurchaseOrderLineService purchaseOrderLineService() {
      return mock(PurchaseOrderLineService.class);
    }

    @Bean
    PieceUpdateFlowManager pieceUpdateFlowManager(PieceStorageService pieceStorageService, PieceService pieceService,
                                                  ProtectionService protectionService, PieceUpdateFlowPoLineService pieceUpdateFlowPoLineService,
                                                  PieceUpdateFlowInventoryManager pieceUpdateFlowInventoryManager, BasePieceFlowHolderBuilder basePieceFlowHolderBuilder,
                                                  DefaultPieceFlowsValidator defaultPieceFlowsValidator, PurchaseOrderLineService purchaseOrderLineService) {
      return new PieceUpdateFlowManager(pieceStorageService, pieceService, protectionService,
        pieceUpdateFlowPoLineService, pieceUpdateFlowInventoryManager, basePieceFlowHolderBuilder,
        defaultPieceFlowsValidator, purchaseOrderLineService);
    }
  }

}
