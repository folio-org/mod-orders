package org.folio.service.pieces.flows.delete;

import static io.vertx.core.Future.succeededFuture;
import static org.folio.TestConfig.autowireDependencies;
import static org.folio.TestConfig.clearServiceInteractions;
import static org.folio.TestConfig.clearVertxContext;
import static org.folio.TestConfig.getFirstContextFromVertx;
import static org.folio.TestConfig.getVertx;
import static org.folio.TestConfig.initSpringContext;
import static org.folio.TestConfig.isVerticleNotDeployed;
import static org.folio.TestConstants.ID;
import static org.folio.service.inventory.InventoryHoldingManager.HOLDING_PERMANENT_LOCATION_ID;
import static org.folio.service.inventory.InventoryItemManager.ITEM_STATUS;
import static org.folio.service.inventory.InventoryItemManager.ITEM_STATUS_NAME;
import static org.junit.jupiter.api.Assertions.assertEquals;
import static org.junit.jupiter.api.Assertions.assertNull;
import static org.mockito.ArgumentMatchers.any;
import static org.mockito.ArgumentMatchers.eq;
import static org.mockito.Mockito.doAnswer;
import static org.mockito.Mockito.doReturn;
import static org.mockito.Mockito.mock;
import static org.mockito.Mockito.times;
import static org.mockito.Mockito.verify;

import java.util.ArrayList;
import java.util.List;
import java.util.Map;
import java.util.UUID;
import java.util.concurrent.ExecutionException;
import java.util.concurrent.TimeoutException;

import io.vertx.core.Context;
import io.vertx.core.Future;
import io.vertx.core.json.JsonObject;
import io.vertx.junit5.VertxExtension;
import org.folio.ApiTestSuite;
import org.folio.models.ItemStatus;
import org.folio.models.pieces.PieceDeletionHolder;
import org.folio.orders.utils.ProtectedOperationType;
import org.folio.rest.core.models.RequestContext;
import org.folio.rest.jaxrs.model.Cost;
import org.folio.rest.jaxrs.model.Eresource;
import org.folio.rest.jaxrs.model.Location;
import org.folio.rest.jaxrs.model.Piece;
import org.folio.rest.jaxrs.model.PieceCollection;
import org.folio.rest.jaxrs.model.PoLine;
import org.folio.rest.jaxrs.model.PurchaseOrder;
import org.folio.rest.jaxrs.model.Title;
import org.folio.service.CirculationRequestsRetriever;
import org.folio.service.ProtectionService;
import org.folio.service.inventory.InventoryHoldingManager;
import org.folio.service.inventory.InventoryItemManager;
import org.folio.service.pieces.PieceStorageService;
import org.folio.service.pieces.PieceUpdateInventoryService;
import org.folio.service.pieces.flows.BasePieceFlowHolderBuilder;
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


@ExtendWith(VertxExtension.class)
public class PieceDeleteFlowManagerTest {
  @Autowired
  PieceDeleteFlowManager pieceDeleteFlowManager;
  @Autowired
  PieceStorageService pieceStorageService;
  @Autowired
  ProtectionService protectionService;
  @Autowired
  InventoryItemManager inventoryItemManager;
  @Autowired
  InventoryHoldingManager inventoryHoldingManager;
  @Autowired
  PieceDeleteFlowPoLineService pieceDeleteFlowPoLineService;
  @Autowired
  PieceUpdateInventoryService pieceUpdateInventoryService;
  @Autowired
  BasePieceFlowHolderBuilder basePieceFlowHolderBuilder;
  @Autowired
  CirculationRequestsRetriever circulationRequestsRetriever;

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
    initSpringContext(PieceDeleteFlowManagerTest.ContextConfiguration.class);
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
    Mockito.reset(pieceStorageService, protectionService, inventoryItemManager, inventoryHoldingManager,
      pieceUpdateInventoryService, pieceDeleteFlowPoLineService, basePieceFlowHolderBuilder);
  }

  @Test
  void shouldNotUpdateLineQuantityIfPoLineIsPackageAndShouldDeleteHoldingAndItemAndPiece()  {
    String orderId = UUID.randomUUID().toString();
    String holdingId = UUID.randomUUID().toString();
    String lineId = UUID.randomUUID().toString();
    String titleId = UUID.randomUUID().toString();
    String itemId = UUID.randomUUID().toString();
    String locationId = UUID.randomUUID().toString();
    JsonObject item = new JsonObject().put(ID, itemId);
    item.put(ITEM_STATUS, new JsonObject().put(ITEM_STATUS_NAME, ItemStatus.ON_ORDER.value()));
    JsonObject holding = new JsonObject().put(ID, holdingId);
    holding.put(HOLDING_PERMANENT_LOCATION_ID, locationId);
    Piece piece = new Piece().withId(UUID.randomUUID().toString()).withPoLineId(lineId).withItemId(itemId).withTitleId(titleId)
      .withHoldingId(holdingId).withFormat(Piece.Format.ELECTRONIC);
    Cost cost = new Cost().withQuantityElectronic(1)
      .withListUnitPriceElectronic(1d).withExchangeRate(1d).withCurrency("USD")
      .withPoLineEstimatedPrice(1d);
    Location loc = new Location().withHoldingId(holdingId).withQuantityElectronic(1).withQuantity(1);
    PoLine poLine = new PoLine().withIsPackage(true).withPurchaseOrderId(orderId).withId(lineId)
                                .withLocations(List.of(loc)).withCost(cost);
    PurchaseOrder purchaseOrder = new PurchaseOrder().withId(orderId).withWorkflowStatus(PurchaseOrder.WorkflowStatus.OPEN);
    Title title = new Title().withId(titleId);

    doReturn(succeededFuture(piece)).when(pieceStorageService).getPieceById(piece.getId(), requestContext);
    doReturn(succeededFuture(null)).when(protectionService).isOperationRestricted(any(), any(ProtectedOperationType.class), eq(requestContext));
    doReturn(succeededFuture(null)).when(pieceStorageService).deletePiece(piece.getId(), true,requestContext);
    doReturn(succeededFuture(null)).when(circulationRequestsRetriever).getNumberOfRequestsByItemId(piece.getItemId(), requestContext);
    doReturn(succeededFuture(item)).when(inventoryItemManager).getItemRecordById(itemId, true, requestContext);
    doReturn(succeededFuture(null)).when(inventoryItemManager).deleteItem(itemId, true, requestContext);

    final ArgumentCaptor<PieceDeletionHolder> PieceDeletionHolderCapture = ArgumentCaptor.forClass(PieceDeletionHolder.class);
    doAnswer((Answer<Future<Void>>) invocation -> {
      PieceDeletionHolder answerHolder = invocation.getArgument(0);
      answerHolder.withOrderInformation(purchaseOrder, poLine);
      return succeededFuture(null);
    }).when(basePieceFlowHolderBuilder).updateHolderWithOrderInformation(PieceDeletionHolderCapture.capture(), eq(requestContext));
    doAnswer((Answer<Future<Void>>) invocation -> {
      PieceDeletionHolder answerHolder = invocation.getArgument(0);
      answerHolder.withTitleInformation(title);
      return succeededFuture(null);
    }).when(basePieceFlowHolderBuilder).updateHolderWithTitleInformation(PieceDeletionHolderCapture.capture(), eq(requestContext));

    doReturn(succeededFuture(holding)).when(inventoryHoldingManager).getHoldingById(holdingId, true, requestContext);
    doReturn(succeededFuture(null)).when(pieceUpdateInventoryService).deleteHoldingConnectedToPiece(piece, requestContext);
    doReturn(succeededFuture(new ArrayList<JsonObject>())).when(inventoryItemManager).getItemsByHoldingId(holdingId,  requestContext);

    final ArgumentCaptor<PieceDeletionHolder> pieceDeletionHolderCapture = ArgumentCaptor.forClass(PieceDeletionHolder.class);
    doReturn(succeededFuture(null)).when(pieceDeleteFlowPoLineService).updatePoLine(pieceDeletionHolderCapture.capture(), eq(requestContext));

    //When
    pieceDeleteFlowManager.deletePiece(piece.getId(), true, requestContext).result();
    //Then
    PieceDeletionHolder holder = PieceDeletionHolderCapture.getValue();
    assertNull(poLine.getLocations().get(0).getLocationId());
    assertEquals(holdingId, poLine.getLocations().get(0).getHoldingId());
    verify(pieceStorageService).deletePiece(eq(piece.getId()), eq(true), eq(requestContext));
    verify(pieceUpdateInventoryService).deleteHoldingConnectedToPiece(piece, requestContext);
    //Then
    assertNull(poLine.getLocations().get(0).getLocationId());
    assertEquals(holdingId, poLine.getLocations().get(0).getHoldingId());
    verify(pieceDeleteFlowPoLineService, times(0)).updatePoLine(pieceDeletionHolderCapture.capture(), eq(requestContext));
    verify(basePieceFlowHolderBuilder).updateHolderWithOrderInformation(holder, requestContext);
  }

  @Test
  void shouldNotUpdateLineQuantityIfPoLineIsNotPackageAndManualPieceCreateTrueAndDeleteHoldingAndItemAndPiece()  {
    String orderId = UUID.randomUUID().toString();
    String holdingId = UUID.randomUUID().toString();
    String lineId = UUID.randomUUID().toString();
    String titleId = UUID.randomUUID().toString();
    String itemId = UUID.randomUUID().toString();
    String locationId = UUID.randomUUID().toString();
    JsonObject item = new JsonObject().put(ID, itemId);
    item.put(ITEM_STATUS, new JsonObject().put(ITEM_STATUS_NAME, ItemStatus.ON_ORDER.value()));
    JsonObject holding = new JsonObject().put(ID, holdingId);
    holding.put(HOLDING_PERMANENT_LOCATION_ID, locationId);
    Piece piece = new Piece().withId(UUID.randomUUID().toString()).withPoLineId(lineId).withItemId(itemId).withTitleId(titleId)
      .withHoldingId(holdingId).withFormat(Piece.Format.ELECTRONIC);
    Location loc = new Location().withHoldingId(holdingId).withQuantityElectronic(1).withQuantity(1);
    Cost cost = new Cost().withQuantityElectronic(1)
      .withListUnitPriceElectronic(1d).withExchangeRate(1d).withCurrency("USD")
      .withPoLineEstimatedPrice(1d);
    PoLine poLine = new PoLine().withIsPackage(false).withPurchaseOrderId(orderId).withId(lineId)
      .withLocations(List.of(loc)).withCost(cost).withCheckinItems(true);
    PurchaseOrder purchaseOrder = new PurchaseOrder().withId(orderId).withWorkflowStatus(PurchaseOrder.WorkflowStatus.OPEN);
    Title title = new Title().withId(titleId);

    doReturn(succeededFuture(piece)).when(pieceStorageService).getPieceById(piece.getId(), requestContext);
    doReturn(succeededFuture(null)).when(protectionService).isOperationRestricted(any(), any(ProtectedOperationType.class), eq(requestContext));
    doReturn(succeededFuture(null)).when(pieceStorageService).deletePiece(eq(piece.getId()), eq(true), eq(requestContext));
    doReturn(succeededFuture(null)).when(circulationRequestsRetriever).getNumberOfRequestsByItemId(eq(piece.getItemId()), eq(requestContext));
    doReturn(succeededFuture(item)).when(inventoryItemManager).getItemRecordById(itemId, true, requestContext);
    doReturn(succeededFuture(null)).when(inventoryItemManager).deleteItem(itemId, true, requestContext);
    doReturn(succeededFuture(holding)).when(inventoryHoldingManager).getHoldingById(holdingId, true, requestContext);
    doReturn(succeededFuture(null)).when(pieceUpdateInventoryService).deleteHoldingConnectedToPiece(piece, requestContext);
    doReturn(succeededFuture(new ArrayList<JsonObject>())).when(inventoryItemManager).getItemsByHoldingId(holdingId,  requestContext);
    final ArgumentCaptor<PieceDeletionHolder> PieceDeletionHolderCapture = ArgumentCaptor.forClass(PieceDeletionHolder.class);
    doAnswer((Answer<Future<Void>>) invocation -> {
      PieceDeletionHolder answerHolder = invocation.getArgument(0);
      answerHolder.withOrderInformation(purchaseOrder, poLine);
      return succeededFuture(null);
    }).when(basePieceFlowHolderBuilder).updateHolderWithOrderInformation(PieceDeletionHolderCapture.capture(), eq(requestContext));
    doAnswer((Answer<Future<Void>>) invocation -> {
      PieceDeletionHolder answerHolder = invocation.getArgument(0);
      answerHolder.withTitleInformation(title);
      return succeededFuture(null);
    }).when(basePieceFlowHolderBuilder).updateHolderWithTitleInformation(PieceDeletionHolderCapture.capture(), eq(requestContext));

    final ArgumentCaptor<PieceDeletionHolder> pieceDeletionHolderCapture = ArgumentCaptor.forClass(PieceDeletionHolder.class);
    doReturn(succeededFuture(null)).when(pieceDeleteFlowPoLineService).updatePoLine(pieceDeletionHolderCapture.capture(), eq(requestContext));
    //When
    pieceDeleteFlowManager.deletePiece(piece.getId(), true, requestContext).result();
    //Then
    PieceDeletionHolder holder = PieceDeletionHolderCapture.getValue();
    assertNull(poLine.getLocations().get(0).getLocationId());
    assertEquals(holdingId, poLine.getLocations().get(0).getHoldingId());
    verify(pieceStorageService).deletePiece(eq(piece.getId()), eq(true), eq(requestContext));
    verify(pieceUpdateInventoryService).deleteHoldingConnectedToPiece(piece, requestContext);
    verify(inventoryItemManager).deleteItem(itemId, true, requestContext);
    verify(pieceDeleteFlowPoLineService, times(0)).updatePoLine(pieceDeletionHolderCapture.capture(), eq(requestContext));
    verify(basePieceFlowHolderBuilder).updateHolderWithOrderInformation(holder, requestContext);
  }

  @Test
  void shouldUpdateLineQuantityIfPoLineIsNotPackageAndManualPieceCreateFalseAndDeleteOnlyPiece()  {
    String orderId = UUID.randomUUID().toString();
    String lineId = UUID.randomUUID().toString();
    String titleId = UUID.randomUUID().toString();
    String locationId = UUID.randomUUID().toString();
    String holdingId = UUID.randomUUID().toString();
    String itemId = UUID.randomUUID().toString();
    Piece piece = new Piece().withId(UUID.randomUUID().toString()).withPoLineId(lineId).withTitleId(titleId)
      .withLocationId(locationId).withFormat(Piece.Format.ELECTRONIC);
    Location loc = new Location().withLocationId(locationId).withQuantityElectronic(1).withQuantity(1);
    Cost cost = new Cost().withQuantityElectronic(1)
      .withListUnitPriceElectronic(1d).withExchangeRate(1d).withCurrency("USD")
      .withPoLineEstimatedPrice(1d);
    PoLine poLine = new PoLine().withIsPackage(false).withCheckinItems(false).withOrderFormat(PoLine.OrderFormat.ELECTRONIC_RESOURCE)
      .withEresource(new Eresource().withCreateInventory(Eresource.CreateInventory.NONE))
                  .withPurchaseOrderId(orderId).withId(lineId).withLocations(List.of(loc)).withCost(cost);
    PurchaseOrder purchaseOrder = new PurchaseOrder().withId(orderId).withWorkflowStatus(PurchaseOrder.WorkflowStatus.OPEN);
    Title title = new Title().withId(titleId);

    doReturn(succeededFuture(piece)).when(pieceStorageService).getPieceById(piece.getId(), requestContext);
    doReturn(succeededFuture(null)).when(protectionService).isOperationRestricted(any(), any(ProtectedOperationType.class), eq(requestContext));
    doReturn(succeededFuture(null)).when(pieceStorageService).deletePiece(eq(piece.getId()), eq(requestContext));
    doReturn(succeededFuture(null)).when(circulationRequestsRetriever).getNumberOfRequestsByItemId(eq(piece.getItemId()), eq(requestContext));
    doReturn(succeededFuture(null)).when(pieceStorageService).deletePiece(eq(piece.getId()), eq(true), eq(requestContext));
    doReturn(succeededFuture(null)).when(circulationRequestsRetriever).getNumberOfRequestsByItemId(eq(piece.getItemId()), eq(requestContext));
    doReturn(succeededFuture(null)).when(inventoryItemManager).deleteItem(itemId, true, requestContext);
    doReturn(succeededFuture(null)).when(pieceUpdateInventoryService).deleteHoldingConnectedToPiece(piece, requestContext);
    final ArgumentCaptor<PieceDeletionHolder> PieceDeletionHolderCapture = ArgumentCaptor.forClass(PieceDeletionHolder.class);
    doAnswer((Answer<Future<Void>>) invocation -> {
      PieceDeletionHolder answerHolder = invocation.getArgument(0);
      answerHolder.withOrderInformation(purchaseOrder, poLine);
      return succeededFuture(null);
    }).when(basePieceFlowHolderBuilder).updateHolderWithOrderInformation(PieceDeletionHolderCapture.capture(), eq(requestContext));
    doAnswer((Answer<Future<Void>>) invocation -> {
      PieceDeletionHolder answerHolder = invocation.getArgument(0);
      answerHolder.withTitleInformation(title);
      return succeededFuture(null);
    }).when(basePieceFlowHolderBuilder).updateHolderWithTitleInformation(PieceDeletionHolderCapture.capture(), eq(requestContext));

    final ArgumentCaptor<PieceDeletionHolder> pieceDeletionHolderCapture = ArgumentCaptor.forClass(PieceDeletionHolder.class);
    doReturn(succeededFuture(null)).when(pieceDeleteFlowPoLineService).updatePoLine(pieceDeletionHolderCapture.capture(), eq(requestContext));

    //When
    pieceDeleteFlowManager.deletePiece(piece.getId(), true, requestContext).result();
    //Then
    PieceDeletionHolder holder = PieceDeletionHolderCapture.getValue();
    verify(pieceStorageService).deletePiece(eq(piece.getId()), eq(true), eq(requestContext));
    verify(inventoryItemManager, times(0)).deleteItem(itemId, true, requestContext);
    verify(inventoryHoldingManager, times(0)).deleteHoldingById(holdingId, true, requestContext);
    verify(pieceStorageService, times(1)).deletePiece(eq(piece.getId()), eq(true), eq(requestContext));
    verify(pieceDeleteFlowPoLineService).updatePoLine(pieceDeletionHolderCapture.capture(), eq(requestContext));
    verify(basePieceFlowHolderBuilder).updateHolderWithOrderInformation(holder, requestContext);
  }

  @Test
  void shouldUpdateLineQuantityIfPoLineIsNotPackageAndManualPieceCreateFalseAndInventoryInstanceVsHoldingAndDeleteHoldingAndPiece()  {
    String orderId = UUID.randomUUID().toString();
    String holdingId = UUID.randomUUID().toString();
    String lineId = UUID.randomUUID().toString();
    String itemId = UUID.randomUUID().toString();
    String locationId = UUID.randomUUID().toString();
    String titleId = UUID.randomUUID().toString();
    JsonObject holding = new JsonObject();
    holding.put(ID, holdingId);
    holding.put(HOLDING_PERMANENT_LOCATION_ID, locationId);
    JsonObject item = new JsonObject().put(ID, itemId);
    item.put(ITEM_STATUS, new JsonObject().put(ITEM_STATUS_NAME, ItemStatus.ON_ORDER.value()));
    Piece piece = new Piece().withId(UUID.randomUUID().toString()).withPoLineId(lineId)
                             .withHoldingId(holdingId).withFormat(Piece.Format.ELECTRONIC);
    Location loc = new Location().withHoldingId(holdingId).withQuantityElectronic(1).withQuantity(1);
    Cost cost = new Cost().withQuantityElectronic(1)
      .withListUnitPriceElectronic(1d).withExchangeRate(1d).withCurrency("USD")
      .withPoLineEstimatedPrice(1d);
    PoLine poLine = new PoLine().withIsPackage(false).withCheckinItems(false).withOrderFormat(PoLine.OrderFormat.ELECTRONIC_RESOURCE)
      .withEresource(new Eresource().withCreateInventory(Eresource.CreateInventory.INSTANCE_HOLDING))
      .withPurchaseOrderId(orderId).withId(lineId).withLocations(List.of(loc)).withCost(cost);
    PurchaseOrder purchaseOrder = new PurchaseOrder().withId(orderId).withWorkflowStatus(PurchaseOrder.WorkflowStatus.OPEN);
    Title title = new Title().withId(titleId);

    doReturn(succeededFuture(piece)).when(pieceStorageService).getPieceById(piece.getId(), requestContext);
    doReturn(succeededFuture(null)).when(protectionService).isOperationRestricted(any(), any(ProtectedOperationType.class), eq(requestContext));
    doReturn(succeededFuture(null)).when(pieceStorageService).deletePiece(eq(piece.getId()), eq(true), eq(requestContext));
    doReturn(succeededFuture(null)).when(circulationRequestsRetriever).getNumberOfRequestsByItemId(eq(piece.getItemId()), eq(requestContext));
    doReturn(succeededFuture(holding)).when(inventoryHoldingManager).getHoldingById(holdingId, false, requestContext);
    doReturn(succeededFuture(null)).when(inventoryItemManager).getItemsByHoldingId(holdingId, requestContext);
    doReturn(succeededFuture(null)).when(inventoryHoldingManager).deleteHoldingById(piece.getHoldingId(), true, requestContext);
    doReturn(succeededFuture(null)).when(inventoryItemManager).getItemRecordById(itemId, true, requestContext);
    doReturn(succeededFuture(null)).when(inventoryItemManager).deleteItem(itemId, true, requestContext);
    doReturn(succeededFuture(holding)).when(inventoryHoldingManager).getHoldingById(holdingId, true, requestContext);
    doReturn(succeededFuture(null)).when(pieceUpdateInventoryService).deleteHoldingConnectedToPiece(piece, requestContext);
    doReturn(succeededFuture(new ArrayList<JsonObject>())).when(inventoryItemManager).getItemsByHoldingId(holdingId,  requestContext);
    final ArgumentCaptor<PieceDeletionHolder> PieceDeletionHolderCapture = ArgumentCaptor.forClass(PieceDeletionHolder.class);
    doAnswer((Answer<Future<Void>>) invocation -> {
      PieceDeletionHolder answerHolder = invocation.getArgument(0);
      answerHolder.withOrderInformation(purchaseOrder, poLine);
      return succeededFuture(null);
    }).when(basePieceFlowHolderBuilder).updateHolderWithOrderInformation(PieceDeletionHolderCapture.capture(), eq(requestContext));
    doAnswer((Answer<Future<Void>>) invocation -> {
      PieceDeletionHolder answerHolder = invocation.getArgument(0);
      answerHolder.withTitleInformation(title);
      return succeededFuture(null);
    }).when(basePieceFlowHolderBuilder).updateHolderWithTitleInformation(PieceDeletionHolderCapture.capture(), eq(requestContext));

    final ArgumentCaptor<PieceDeletionHolder> pieceDeletionHolderCapture = ArgumentCaptor.forClass(PieceDeletionHolder.class);
    doReturn(succeededFuture(null)).when(pieceDeleteFlowPoLineService).updatePoLine(pieceDeletionHolderCapture.capture(), eq(requestContext));
    //When
    pieceDeleteFlowManager.deletePiece(piece.getId(), true, requestContext).result();
    //Then
    PieceDeletionHolder holder = PieceDeletionHolderCapture.getValue();
    verify(pieceStorageService).deletePiece(eq(piece.getId()), eq(true), eq(requestContext));
    verify(inventoryItemManager, times(0)).deleteItem(itemId, true, requestContext);
    verify(pieceUpdateInventoryService).deleteHoldingConnectedToPiece(piece, requestContext);
    verify(pieceStorageService, times(1)).deletePiece(eq(piece.getId()), eq(true), eq(requestContext));
    verify(pieceDeleteFlowPoLineService).updatePoLine(pieceDeletionHolderCapture.capture(), eq(requestContext));
    verify(basePieceFlowHolderBuilder).updateHolderWithOrderInformation(holder, requestContext);
  }

  @Test
  void shouldUpdateLineQuantityIfPoLineIsNotPackageAndManualPieceCreateFalseAndInventoryInstanceVsHoldingAndDeleteHoldingAndPieceInBatch()  {
    String orderId = UUID.randomUUID().toString();
    String holdingId = UUID.randomUUID().toString();
    String lineId = UUID.randomUUID().toString();
    String itemId = UUID.randomUUID().toString();
    String locationId = UUID.randomUUID().toString();
    String titleId = UUID.randomUUID().toString();
    JsonObject holding = new JsonObject();
    holding.put(ID, holdingId);
    holding.put(HOLDING_PERMANENT_LOCATION_ID, locationId);
    JsonObject item = new JsonObject().put(ID, itemId);
    item.put(ITEM_STATUS, new JsonObject().put(ITEM_STATUS_NAME, ItemStatus.ON_ORDER.value()));
    Piece piece = new Piece().withId(UUID.randomUUID().toString()).withPoLineId(lineId)
      .withHoldingId(holdingId).withFormat(Piece.Format.ELECTRONIC);
    Location loc = new Location().withHoldingId(holdingId).withQuantityElectronic(1).withQuantity(1);
    Cost cost = new Cost().withQuantityElectronic(1)
      .withListUnitPriceElectronic(1d).withExchangeRate(1d).withCurrency("USD")
      .withPoLineEstimatedPrice(1d);
    PoLine poLine = new PoLine().withIsPackage(false).withCheckinItems(false).withOrderFormat(PoLine.OrderFormat.ELECTRONIC_RESOURCE)
      .withEresource(new Eresource().withCreateInventory(Eresource.CreateInventory.INSTANCE_HOLDING))
      .withPurchaseOrderId(orderId).withId(lineId).withLocations(List.of(loc)).withCost(cost);
    PurchaseOrder purchaseOrder = new PurchaseOrder().withId(orderId).withWorkflowStatus(PurchaseOrder.WorkflowStatus.OPEN);
    Title title = new Title().withId(titleId);
    List<Piece> ids = new ArrayList<>();
    ids.add(piece);
    PieceCollection pieceCollection = new PieceCollection();
    pieceCollection.withPieces(ids);
    doReturn(succeededFuture(piece)).when(pieceStorageService).getPieceById(piece.getId(), requestContext);
    doReturn(succeededFuture(null)).when(protectionService).isOperationRestricted(any(), any(ProtectedOperationType.class), eq(requestContext));
    doReturn(succeededFuture(null)).when(pieceStorageService).deletePiece(eq(piece.getId()), eq(true), eq(requestContext));
    doReturn(succeededFuture(null)).when(inventoryItemManager).getNumberOfRequestsByItemId(eq(piece.getItemId()), eq(requestContext));
    doReturn(succeededFuture(holding)).when(inventoryHoldingManager).getHoldingById(holdingId, false, requestContext);
    doReturn(succeededFuture(null)).when(inventoryItemManager).getItemsByHoldingId(holdingId, requestContext);
    doReturn(succeededFuture(null)).when(inventoryHoldingManager).deleteHoldingById(piece.getHoldingId(), true, requestContext);
    doReturn(succeededFuture(null)).when(inventoryItemManager).getItemRecordById(itemId, true, requestContext);
    doReturn(succeededFuture(null)).when(inventoryItemManager).deleteItem(itemId, true, requestContext);
    doReturn(succeededFuture(holding)).when(inventoryHoldingManager).getHoldingById(holdingId, true, requestContext);
    doReturn(succeededFuture(null)).when(pieceUpdateInventoryService).deleteHoldingConnectedToPiece(piece, requestContext);
    doReturn(succeededFuture(new ArrayList<JsonObject>())).when(inventoryItemManager).getItemsByHoldingId(holdingId,  requestContext);
    final ArgumentCaptor<PieceDeletionHolder> PieceDeletionHolderCapture = ArgumentCaptor.forClass(PieceDeletionHolder.class);
    doAnswer((Answer<Future<Void>>) invocation -> {
      PieceDeletionHolder answerHolder = invocation.getArgument(0);
      answerHolder.withOrderInformation(purchaseOrder, poLine);
      return succeededFuture(null);
    }).when(basePieceFlowHolderBuilder).updateHolderWithOrderInformation(PieceDeletionHolderCapture.capture(), eq(requestContext));
    doAnswer((Answer<Future<Void>>) invocation -> {
      PieceDeletionHolder answerHolder = invocation.getArgument(0);
      answerHolder.withTitleInformation(title);
      return succeededFuture(null);
    }).when(basePieceFlowHolderBuilder).updateHolderWithTitleInformation(PieceDeletionHolderCapture.capture(), eq(requestContext));

    final ArgumentCaptor<PieceDeletionHolder> pieceDeletionHolderCapture = ArgumentCaptor.forClass(PieceDeletionHolder.class);
    doReturn(succeededFuture(null)).when(pieceDeleteFlowPoLineService).updatePoLine(pieceDeletionHolderCapture.capture(), eq(requestContext));
    //When
    pieceDeleteFlowManager.batchDeletePiece(pieceCollection, requestContext).result();
      verify(pieceStorageService).deletePiece(eq(piece.getId()), eq(true), eq(requestContext));
  }

  private static class ContextConfiguration {

    @Bean
    PieceStorageService pieceStorageService() {
      return mock(PieceStorageService.class);
    }

    @Bean
    ProtectionService protectionService() {
      return mock(ProtectionService.class);
    }

    @Bean PieceDeleteFlowPoLineService pieceDeleteFlowPoLineService() {
      return mock(PieceDeleteFlowPoLineService.class);
    }

    @Bean
    InventoryItemManager inventoryItemManager() {
      return mock(InventoryItemManager.class);
    }

    @Bean
    InventoryHoldingManager inventoryHoldingManager() {
      return mock(InventoryHoldingManager.class);
    }

    @Bean
    PieceUpdateInventoryService pieceUpdateInventoryService() {
      return mock(PieceUpdateInventoryService.class);
    }

    @Bean
    BasePieceFlowHolderBuilder basePieceFlowHolderBuilder() {
      return mock(BasePieceFlowHolderBuilder.class);
    }

    @Bean
    public CirculationRequestsRetriever circulationRequestsRetriever() {
      return mock(CirculationRequestsRetriever.class);
    }

    @Bean PieceDeleteFlowManager pieceDeleteFlowManager(PieceStorageService pieceStorageService,
                                                        ProtectionService protectionService,
                                                        InventoryItemManager inventoryItemManager,
                                                        PieceUpdateInventoryService pieceUpdateInventoryService,
                                                        PieceDeleteFlowPoLineService pieceDeleteFlowPoLineService,
                                                        BasePieceFlowHolderBuilder basePieceFlowHolderBuilder,
                                                        CirculationRequestsRetriever circulationRequestsRetriever) {
      return new PieceDeleteFlowManager(pieceStorageService, protectionService, inventoryItemManager,
        pieceUpdateInventoryService, pieceDeleteFlowPoLineService, basePieceFlowHolderBuilder, circulationRequestsRetriever);
    }

  }
}
