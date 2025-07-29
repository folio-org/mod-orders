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
import static org.folio.service.inventory.InventoryHoldingManager.HOLDING_PERMANENT_LOCATION_ID;
import static org.folio.service.inventory.InventoryItemManager.ITEM_HOLDINGS_RECORD_ID;
import static org.folio.service.inventory.InventoryItemManager.ITEM_PURCHASE_ORDER_LINE_IDENTIFIER;
import static org.folio.service.inventory.InventoryItemManager.ITEM_STATUS;
import static org.folio.service.inventory.InventoryItemManager.ITEM_STATUS_NAME;
import static org.junit.jupiter.api.Assertions.assertEquals;
import static org.mockito.ArgumentMatchers.any;
import static org.mockito.ArgumentMatchers.eq;
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

import org.folio.ApiTestSuite;
import org.folio.models.ItemStatus;
import org.folio.models.pieces.PieceUpdateHolder;
import org.folio.rest.core.models.RequestContext;
import org.folio.rest.jaxrs.model.Cost;
import org.folio.rest.jaxrs.model.Eresource;
import org.folio.rest.jaxrs.model.Location;
import org.folio.rest.jaxrs.model.Physical;
import org.folio.rest.jaxrs.model.Piece;
import org.folio.rest.jaxrs.model.PoLine;
import org.folio.rest.jaxrs.model.PurchaseOrder;
import org.folio.rest.jaxrs.model.Title;
import org.folio.service.inventory.InventoryHoldingManager;
import org.folio.service.inventory.InventoryItemManager;
import org.folio.service.pieces.ItemRecreateInventoryService;
import org.folio.service.pieces.PieceUpdateInventoryService;
import org.folio.service.titles.TitleInstanceService;
import org.folio.service.titles.TitlesService;
import org.junit.jupiter.api.AfterAll;
import org.junit.jupiter.api.AfterEach;
import org.junit.jupiter.api.BeforeAll;
import org.junit.jupiter.api.BeforeEach;
import org.junit.jupiter.api.Test;
import org.junit.jupiter.api.extension.ExtendWith;
import org.mockito.Mock;
import org.mockito.Mockito;
import org.mockito.MockitoAnnotations;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.context.annotation.Bean;

import io.vertx.core.Context;
import io.vertx.core.json.JsonObject;
import io.vertx.junit5.VertxExtension;

@ExtendWith(VertxExtension.class)
public class PieceUpdateFlowInventoryManagerTest {
  @Autowired
  PieceUpdateFlowInventoryManager pieceUpdateFlowInventoryManager;
  @Autowired
  TitlesService titlesService;
  @Autowired
  PieceUpdateInventoryService pieceUpdateInventoryService;
  @Autowired
  ItemRecreateInventoryService itemRecreateInventoryService;
  @Autowired
  InventoryItemManager inventoryItemManager;
  @Autowired
  InventoryHoldingManager inventoryHoldingManager;

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
    initSpringContext(PieceUpdateFlowInventoryManagerTest.ContextConfiguration.class);
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
    Mockito.reset(titlesService, inventoryItemManager, inventoryHoldingManager, pieceUpdateInventoryService, itemRecreateInventoryService);
  }

  @Test
  void shouldDeleteHoldingWhenElecPieceUpdateForPackagePoLineWithCreateInventoryAllAndPieceContainsNewHoldingAndTitleWithInstanceId() {
    String orderId = UUID.randomUUID().toString();
    String holdingFromStorageId = UUID.randomUUID().toString();
    String holdingId = UUID.randomUUID().toString();
    String lineId = UUID.randomUUID().toString();
    String titleId = UUID.randomUUID().toString();
    String itemId = UUID.randomUUID().toString();
    String pieceId = UUID.randomUUID().toString();
    String instanceId = UUID.randomUUID().toString();

    JsonObject holding = new JsonObject().put(ID, holdingFromStorageId);
    holding.put(HOLDING_PERMANENT_LOCATION_ID, UUID.randomUUID().toString());
    Title title = new Title().withId(titleId).withPoLineId(lineId).withInstanceId(instanceId);
    Piece pieceFromStorage = new Piece().withId(pieceId).withTitleId(titleId).withItemId(itemId)
      .withPoLineId(lineId).withHoldingId(holdingFromStorageId).withFormat(Piece.Format.ELECTRONIC);
    Piece pieceToUpdate = new Piece().withId(pieceId).withTitleId(titleId).withItemId(itemId)
      .withPoLineId(lineId).withHoldingId(holdingId).withFormat(Piece.Format.ELECTRONIC);
    Location loc = new Location().withHoldingId(holdingId).withQuantityElectronic(1).withQuantity(1);
    Cost cost = new Cost().withQuantityElectronic(1);

    PoLine poLine = new PoLine().withIsPackage(true).withPurchaseOrderId(orderId).withId(lineId)
      .withOrderFormat(PoLine.OrderFormat.ELECTRONIC_RESOURCE).withId(lineId)
      .withEresource(new Eresource().withCreateInventory(Eresource.CreateInventory.INSTANCE_HOLDING_ITEM))
      .withLocations(List.of(loc)).withCost(cost);
    PurchaseOrder purchaseOrder = new PurchaseOrder().withId(orderId).withWorkflowStatus(PurchaseOrder.WorkflowStatus.OPEN);
    PieceUpdateHolder holder = new PieceUpdateHolder().withPieceToUpdate(pieceToUpdate).withPieceFromStorage(pieceFromStorage)
      .withCreateItem(true).withDeleteHolding(true);
    holder.withOrderInformation(purchaseOrder, poLine);

    doReturn(succeededFuture(title)).when(titlesService).getTitleById(pieceToUpdate.getTitleId(), requestContext);
    doReturn(succeededFuture(new JsonObject())).when(inventoryItemManager).getItemRecordById(pieceToUpdate.getItemId(), true, requestContext);
    doReturn(succeededFuture(List.of(itemId))).when(inventoryItemManager)
      .createMissingElectronicItems(holder.getPurchaseOrderToSave(), holder.getPoLineToSave(), pieceToUpdate, 1, requestContext);
    doReturn(succeededFuture(null)).when(inventoryItemManager).updateItemWithPieceFields(holder.getPieceFromStorage(), holder.getPieceToUpdate(), requestContext);
    doReturn(succeededFuture(holding)).when(inventoryHoldingManager).getHoldingById(holder.getPieceFromStorage().getHoldingId(), true, requestContext);
    doReturn(succeededFuture(new ArrayList())).when(inventoryItemManager).getItemsByHoldingId(holder.getPieceFromStorage().getHoldingId(), requestContext);
    doReturn(succeededFuture(null)).when(pieceUpdateInventoryService).deleteHoldingConnectedToPiece(holder.getPieceFromStorage(), requestContext);
    doReturn(succeededFuture(itemId)).when(pieceUpdateInventoryService).manualPieceFlowCreateItemRecord(pieceToUpdate, holder.getPurchaseOrderToSave(), holder.getPoLineToSave(), requestContext);
    doReturn(succeededFuture(instanceId)).when(titlesService).updateTitleWithInstance(eq(title.getId()), eq(requestContext), eq(requestContext));

    pieceUpdateFlowInventoryManager.processInventory(holder, requestContext).result();

    verify(titlesService).updateTitleWithInstance(pieceToUpdate.getTitleId(), requestContext, requestContext);
    verify(inventoryHoldingManager, times(0)).getOrCreateHoldingsRecord(eq(holder.getInstanceId()), any(Location.class), eq(requestContext));
    verify(pieceUpdateInventoryService).deleteHoldingConnectedToPiece(holder.getPieceFromStorage(), requestContext);
    verify(inventoryItemManager).updateItemWithPieceFields(holder.getPieceFromStorage(), holder.getPieceToUpdate(), requestContext);
    verify(pieceUpdateInventoryService).manualPieceFlowCreateItemRecord(pieceToUpdate, holder.getPurchaseOrderToSave(), holder.getPoLineToSave(), requestContext);
  }

  @Test
  void shouldDeleteHoldingWhenPhysPieceUpdateForPackagePoLineWithCreateInventoryAllAndPieceContainsNewHoldingAndTitleWithInstanceId() {
    String orderId = UUID.randomUUID().toString();
    String holdingFromStorageId = UUID.randomUUID().toString();
    String holdingId = UUID.randomUUID().toString();
    String lineId = UUID.randomUUID().toString();
    String titleId = UUID.randomUUID().toString();
    String itemId = UUID.randomUUID().toString();
    String pieceId = UUID.randomUUID().toString();
    String instanceId = UUID.randomUUID().toString();
    JsonObject holding = new JsonObject().put(ID, holdingFromStorageId);
    holding.put(HOLDING_PERMANENT_LOCATION_ID, UUID.randomUUID().toString());
    Title title = new Title().withId(titleId).withPoLineId(lineId).withInstanceId(instanceId);
    Piece pieceFromStorage = new Piece().withId(pieceId).withTitleId(titleId).withItemId(itemId)
      .withPoLineId(lineId).withHoldingId(holdingFromStorageId).withFormat(Piece.Format.PHYSICAL);
    Piece pieceToUpdate = new Piece().withId(pieceId).withTitleId(titleId).withItemId(itemId)
      .withPoLineId(lineId).withHoldingId(holdingId).withFormat(Piece.Format.PHYSICAL);
    Location loc = new Location().withHoldingId(holdingId).withQuantityElectronic(1).withQuantity(1);
    Cost cost = new Cost().withQuantityElectronic(1);

    PoLine poLine = new PoLine().withIsPackage(true).withPurchaseOrderId(orderId).withId(lineId)
      .withOrderFormat(PoLine.OrderFormat.PHYSICAL_RESOURCE).withId(lineId)
      .withPhysical(new Physical().withCreateInventory(Physical.CreateInventory.INSTANCE_HOLDING_ITEM))
      .withLocations(List.of(loc)).withCost(cost);
    PurchaseOrder purchaseOrder = new PurchaseOrder().withId(orderId).withWorkflowStatus(PurchaseOrder.WorkflowStatus.OPEN);
    PieceUpdateHolder holder = new PieceUpdateHolder().withPieceToUpdate(pieceToUpdate).withPieceFromStorage(pieceFromStorage)
      .withCreateItem(true).withDeleteHolding(true);
    holder.withOrderInformation(purchaseOrder, poLine);

    doReturn(succeededFuture(title)).when(titlesService).getTitleById(pieceToUpdate.getTitleId(), requestContext);
    doReturn(succeededFuture(new JsonObject())).when(inventoryItemManager).getItemRecordById(pieceToUpdate.getItemId(), true, requestContext);
    doReturn(succeededFuture(List.of(itemId))).when(inventoryItemManager)
      .createMissingPhysicalItems(holder.getPurchaseOrderToSave(), holder.getPoLineToSave(), pieceToUpdate, 1, requestContext);
    doReturn(succeededFuture(null)).when(inventoryItemManager).updateItemWithPieceFields(holder.getPieceFromStorage(), holder.getPieceToUpdate(), requestContext);
    doReturn(succeededFuture(holding)).when(inventoryHoldingManager).getHoldingById(holder.getPieceFromStorage().getHoldingId(), true, requestContext);
    doReturn(succeededFuture(new ArrayList())).when(inventoryItemManager).getItemsByHoldingId(holder.getPieceFromStorage().getHoldingId(), requestContext);
    doReturn(succeededFuture(null)).when(pieceUpdateInventoryService).deleteHoldingConnectedToPiece(holder.getPieceFromStorage(), requestContext);
    doReturn(succeededFuture(itemId)).when(pieceUpdateInventoryService).manualPieceFlowCreateItemRecord(pieceToUpdate, holder.getPurchaseOrderToSave(), holder.getPoLineToSave(), requestContext);
    doReturn(succeededFuture(instanceId)).when(titlesService).updateTitleWithInstance(eq(title.getId()), eq(requestContext), eq(requestContext));

    pieceUpdateFlowInventoryManager.processInventory(holder, requestContext).result();

    verify(titlesService).updateTitleWithInstance(pieceToUpdate.getTitleId(), requestContext, requestContext);
    verify(inventoryHoldingManager, times(0)).getOrCreateHoldingsRecord(eq(holder.getInstanceId()), any(Location.class), eq(requestContext));
    verify(pieceUpdateInventoryService).deleteHoldingConnectedToPiece(holder.getPieceFromStorage(), requestContext);
    verify(inventoryItemManager).updateItemWithPieceFields(holder.getPieceFromStorage(), holder.getPieceToUpdate(), requestContext);
    verify(pieceUpdateInventoryService).manualPieceFlowCreateItemRecord(pieceToUpdate, holder.getPurchaseOrderToSave(), holder.getPoLineToSave(), requestContext);
  }

  @Test
  void shouldDeleteHoldingWhenPhysPieceUpdateForPackagePoLineWithCreateInventoryAllAndPieceContainsNewHolding() {
    String orderId = UUID.randomUUID().toString();
    String holdingFromStorageId = UUID.randomUUID().toString();
    String holdingId = UUID.randomUUID().toString();
    String lineId = UUID.randomUUID().toString();
    String titleId = UUID.randomUUID().toString();
    String itemId = UUID.randomUUID().toString();
    String pieceId = UUID.randomUUID().toString();
    String instanceId = UUID.randomUUID().toString();
    Title title = new Title().withId(titleId).withPoLineId(lineId).withInstanceId(instanceId);
    JsonObject holding = new JsonObject().put(ID, holdingFromStorageId);
    holding.put(HOLDING_PERMANENT_LOCATION_ID, UUID.randomUUID().toString());
    Piece pieceFromStorage = new Piece().withId(pieceId).withTitleId(titleId).withItemId(itemId)
      .withPoLineId(lineId).withHoldingId(holdingFromStorageId).withFormat(Piece.Format.PHYSICAL);
    Piece pieceToUpdate = new Piece().withId(pieceId).withTitleId(titleId).withItemId(itemId)
      .withPoLineId(lineId).withHoldingId(holdingId).withFormat(Piece.Format.PHYSICAL);
    Location loc = new Location().withHoldingId(holdingId).withQuantityElectronic(1).withQuantity(1);
    Cost cost = new Cost().withQuantityElectronic(1);

    PoLine poLine = new PoLine().withIsPackage(true).withPurchaseOrderId(orderId).withId(lineId)
      .withOrderFormat(PoLine.OrderFormat.PHYSICAL_RESOURCE).withId(lineId)
      .withPhysical(new Physical().withCreateInventory(Physical.CreateInventory.INSTANCE_HOLDING_ITEM))
      .withLocations(List.of(loc)).withCost(cost);
    PurchaseOrder purchaseOrder = new PurchaseOrder().withId(orderId).withWorkflowStatus(PurchaseOrder.WorkflowStatus.OPEN);
    PieceUpdateHolder holder = new PieceUpdateHolder().withPieceToUpdate(pieceToUpdate).withPieceFromStorage(pieceFromStorage)
      .withCreateItem(true).withDeleteHolding(true);
    holder.withOrderInformation(purchaseOrder, poLine);

    doReturn(succeededFuture(title)).when(titlesService).getTitleById(pieceToUpdate.getTitleId(), requestContext);

    doReturn(succeededFuture(new JsonObject())).when(inventoryItemManager).getItemRecordById(pieceToUpdate.getItemId(), true, requestContext);
    doReturn(succeededFuture(List.of(itemId))).when(inventoryItemManager)
      .createMissingPhysicalItems(holder.getPurchaseOrderToSave(), holder.getPoLineToSave(), pieceToUpdate, 1, requestContext);
    doReturn(succeededFuture(null)).when(inventoryItemManager).updateItemWithPieceFields(holder.getPieceFromStorage(), holder.getPieceToUpdate(), requestContext);
    doReturn(succeededFuture(holding)).when(inventoryHoldingManager).getHoldingById(holder.getPieceFromStorage().getHoldingId(), true, requestContext);
    doReturn(succeededFuture(new ArrayList())).when(inventoryItemManager).getItemsByHoldingId(holder.getPieceFromStorage().getHoldingId(), requestContext);
    doReturn(succeededFuture(null)).when(pieceUpdateInventoryService).deleteHoldingConnectedToPiece(holder.getPieceFromStorage(), requestContext);
    doReturn(succeededFuture(itemId)).when(pieceUpdateInventoryService).manualPieceFlowCreateItemRecord(pieceToUpdate, holder.getPurchaseOrderToSave(), holder.getPoLineToSave(), requestContext);
    doReturn(succeededFuture(instanceId)).when(titlesService).updateTitleWithInstance(eq(title.getId()), eq(requestContext), eq(requestContext));

    pieceUpdateFlowInventoryManager.processInventory(holder, requestContext).result();

    verify(titlesService).updateTitleWithInstance(pieceToUpdate.getTitleId(), requestContext, requestContext);
    verify(inventoryHoldingManager, times(0)).getOrCreateHoldingsRecord(eq(holder.getInstanceId()), any(Location.class), eq(requestContext));
    verify(pieceUpdateInventoryService).deleteHoldingConnectedToPiece(holder.getPieceFromStorage(), requestContext);
    verify(inventoryItemManager).updateItemWithPieceFields(holder.getPieceFromStorage(), holder.getPieceToUpdate(), requestContext);
    verify(pieceUpdateInventoryService).manualPieceFlowCreateItemRecord(pieceToUpdate, holder.getPurchaseOrderToSave(), holder.getPoLineToSave(), requestContext);
  }

  @Test
  void shouldUpdateItemWithNewHoldingIdWhenPhysPieceUpdateForPackagePoLineWithCreateInventoryAllAndPieceContainsNewHolding() {
    String orderId = UUID.randomUUID().toString();
    String holdingFromStorageId = UUID.randomUUID().toString();
    String holdingId = UUID.randomUUID().toString();
    String lineId = UUID.randomUUID().toString();
    String titleId = UUID.randomUUID().toString();
    String itemId = UUID.randomUUID().toString();
    String pieceId = UUID.randomUUID().toString();
    String instanceId = UUID.randomUUID().toString();
    Title title = new Title().withId(titleId).withPoLineId(lineId).withInstanceId(instanceId);
    JsonObject item = new JsonObject().put(ID, itemId);
    item.put(ITEM_STATUS, new JsonObject().put(ITEM_STATUS_NAME, ItemStatus.ON_ORDER.value()));
    item.put(ITEM_HOLDINGS_RECORD_ID, holdingFromStorageId);
    JsonObject holding = new JsonObject().put(ID, holdingFromStorageId);
    holding.put(HOLDING_PERMANENT_LOCATION_ID, UUID.randomUUID().toString());
    Piece pieceFromStorage = new Piece().withId(pieceId).withTitleId(titleId).withItemId(itemId)
      .withPoLineId(lineId).withHoldingId(holdingFromStorageId).withFormat(Piece.Format.PHYSICAL);
    Piece pieceToUpdate = new Piece().withId(pieceId).withTitleId(titleId).withItemId(itemId)
      .withPoLineId(lineId).withHoldingId(holdingId).withFormat(Piece.Format.PHYSICAL);
    Location loc = new Location().withHoldingId(holdingId).withQuantityElectronic(1).withQuantity(1);
    Cost cost = new Cost().withQuantityElectronic(1);

    PoLine poLine = new PoLine().withIsPackage(true).withPurchaseOrderId(orderId).withId(lineId)
      .withOrderFormat(PoLine.OrderFormat.PHYSICAL_RESOURCE).withId(lineId)
      .withPhysical(new Physical().withCreateInventory(Physical.CreateInventory.INSTANCE_HOLDING_ITEM))
      .withLocations(List.of(loc)).withCost(cost);
    PurchaseOrder purchaseOrder = new PurchaseOrder().withId(orderId).withWorkflowStatus(PurchaseOrder.WorkflowStatus.OPEN);
    PieceUpdateHolder holder = new PieceUpdateHolder().withPieceToUpdate(pieceToUpdate).withPieceFromStorage(pieceFromStorage)
                                                      .withCreateItem(true).withDeleteHolding(true);
    holder.withOrderInformation(purchaseOrder, poLine);

    doReturn(succeededFuture(title)).when(titlesService).getTitleById(pieceToUpdate.getTitleId(), requestContext);
    doReturn(succeededFuture(item)).when(inventoryItemManager).getItemRecordById(pieceToUpdate.getItemId(), true, requestContext);
    doReturn(succeededFuture(null)).when(inventoryItemManager).updateItem(item, requestContext);

    doReturn(succeededFuture(List.of(itemId))).when(inventoryItemManager)
      .createMissingPhysicalItems(holder.getPurchaseOrderToSave(), holder.getPoLineToSave(), pieceToUpdate, 1, requestContext);
    doReturn(succeededFuture(null)).when(inventoryItemManager).updateItemWithPieceFields(holder.getPieceFromStorage(), holder.getPieceToUpdate(), requestContext);
    doReturn(succeededFuture(holding)).when(inventoryHoldingManager).getHoldingById(holder.getPieceFromStorage().getHoldingId(), true, requestContext);
    doReturn(succeededFuture(new ArrayList())).when(inventoryItemManager).getItemsByHoldingId(holder.getPieceFromStorage().getHoldingId(), requestContext);
    doReturn(succeededFuture(null)).when(pieceUpdateInventoryService).deleteHoldingConnectedToPiece(holder.getPieceFromStorage(), requestContext);
    doReturn(succeededFuture(instanceId)).when(titlesService).updateTitleWithInstance(eq(title.getId()), eq(requestContext), eq(requestContext));

    pieceUpdateFlowInventoryManager.processInventory(holder, requestContext).result();

    assertEquals(holdingId, item.getString(ITEM_HOLDINGS_RECORD_ID));
    assertEquals(holder.getPoLineToSave().getId(), item.getString(ITEM_PURCHASE_ORDER_LINE_IDENTIFIER));
    verify(titlesService).updateTitleWithInstance(pieceToUpdate.getTitleId(), requestContext, requestContext);
    verify(inventoryHoldingManager, times(0)).getOrCreateHoldingsRecord(eq(holder.getInstanceId()), any(Location.class), eq(requestContext));
    verify(pieceUpdateInventoryService).deleteHoldingConnectedToPiece(holder.getPieceFromStorage(), requestContext);
    verify(inventoryItemManager).updateItemWithPieceFields(holder.getPieceFromStorage(), holder.getPieceToUpdate(), requestContext);
    verify(inventoryItemManager,times(0)).createMissingElectronicItems(holder.getPurchaseOrderToSave(), holder.getPoLineToSave(), pieceToUpdate, 1, requestContext);
    verify(inventoryItemManager,times(0)).createMissingPhysicalItems(holder.getPurchaseOrderToSave(), holder.getPoLineToSave(), pieceToUpdate, 1, requestContext);
    verify(inventoryItemManager).updateItem(item, requestContext);
  }

  @Test
  void shouldDeleteHoldingWhenElecPieceUpdateForNonPackagePoLineWithCreateInventoryAllAndPieceContainsNewHoldingAndTitleWithInstanceId() {
    String orderId = UUID.randomUUID().toString();
    String holdingFromStorageId = UUID.randomUUID().toString();
    String holdingId = UUID.randomUUID().toString();
    String lineId = UUID.randomUUID().toString();
    String titleId = UUID.randomUUID().toString();
    String itemId = UUID.randomUUID().toString();
    String pieceId = UUID.randomUUID().toString();
    String instanceId = UUID.randomUUID().toString();

    JsonObject holding = new JsonObject().put(ID, holdingFromStorageId);
    holding.put(HOLDING_PERMANENT_LOCATION_ID, UUID.randomUUID().toString());
    Title title = new Title().withId(titleId).withPoLineId(lineId).withInstanceId(instanceId);
    Piece pieceFromStorage = new Piece().withId(pieceId).withTitleId(titleId).withItemId(itemId)
                                     .withPoLineId(lineId).withHoldingId(holdingFromStorageId).withFormat(Piece.Format.ELECTRONIC);
    Piece pieceToUpdate = new Piece().withId(pieceId).withTitleId(titleId).withItemId(itemId)
                                     .withPoLineId(lineId).withHoldingId(holdingId).withFormat(Piece.Format.ELECTRONIC);
    Location loc = new Location().withHoldingId(holdingId).withQuantityElectronic(1).withQuantity(1);
    Cost cost = new Cost().withQuantityElectronic(1);

    PoLine poLine = new PoLine().withIsPackage(false).withPurchaseOrderId(orderId).withId(lineId)
      .withOrderFormat(PoLine.OrderFormat.ELECTRONIC_RESOURCE).withId(lineId)
      .withEresource(new Eresource().withCreateInventory(Eresource.CreateInventory.INSTANCE_HOLDING_ITEM))
      .withLocations(List.of(loc)).withCost(cost);
    PurchaseOrder purchaseOrder = new PurchaseOrder().withId(orderId).withWorkflowStatus(PurchaseOrder.WorkflowStatus.OPEN);
    PieceUpdateHolder holder = new PieceUpdateHolder().withPieceToUpdate(pieceToUpdate).withPieceFromStorage(pieceFromStorage)
      .withCreateItem(true).withDeleteHolding(true);
    holder.withOrderInformation(purchaseOrder, poLine);

    doReturn(succeededFuture(title)).when(titlesService).getTitleById(pieceToUpdate.getTitleId(), requestContext);
    doReturn(succeededFuture(new JsonObject())).when(inventoryItemManager).getItemRecordById(pieceToUpdate.getItemId(), true, requestContext);
    doReturn(succeededFuture(List.of(itemId))).when(inventoryItemManager)
                                  .createMissingElectronicItems(holder.getPurchaseOrderToSave(), holder.getPoLineToSave(), pieceToUpdate, 1, requestContext);
    doReturn(succeededFuture(null)).when(inventoryItemManager).updateItemWithPieceFields(holder.getPieceFromStorage(), holder.getPieceToUpdate(), requestContext);
    doReturn(succeededFuture(holding)).when(inventoryHoldingManager).getHoldingById(holder.getPieceFromStorage().getHoldingId(), true, requestContext);
    doReturn(succeededFuture(new ArrayList())).when(inventoryItemManager).getItemsByHoldingId(holder.getPieceFromStorage().getHoldingId(), requestContext);
    doReturn(succeededFuture(null)).when(pieceUpdateInventoryService).deleteHoldingConnectedToPiece(holder.getPieceFromStorage(), requestContext);
    doReturn(succeededFuture(itemId)).when(pieceUpdateInventoryService).manualPieceFlowCreateItemRecord(pieceToUpdate, holder.getPurchaseOrderToSave(), holder.getPoLineToSave(), requestContext);
    doReturn(succeededFuture(instanceId)).when(titlesService).updateTitleWithInstance(eq(title.getId()), eq(requestContext), eq(requestContext));

    pieceUpdateFlowInventoryManager.processInventory(holder, requestContext).result();

    verify(titlesService).updateTitleWithInstance(pieceToUpdate.getTitleId(), requestContext, requestContext);
    verify(inventoryHoldingManager, times(0)).getOrCreateHoldingsRecord(eq(holder.getInstanceId()), any(Location.class), eq(requestContext));
    verify(pieceUpdateInventoryService).deleteHoldingConnectedToPiece(holder.getPieceFromStorage(), requestContext);
    verify(inventoryItemManager).updateItemWithPieceFields(holder.getPieceFromStorage(), holder.getPieceToUpdate(), requestContext);
    verify(pieceUpdateInventoryService).manualPieceFlowCreateItemRecord(pieceToUpdate, holder.getPurchaseOrderToSave(), holder.getPoLineToSave(), requestContext);
  }

  @Test
  void shouldDeleteHoldingWhenPhysPieceUpdateForNonPackagePoLineWithCreateInventoryAllAndPieceContainsNewHoldingAndTitleWithInstanceId() {
    String orderId = UUID.randomUUID().toString();
    String holdingFromStorageId = UUID.randomUUID().toString();
    String holdingId = UUID.randomUUID().toString();
    String lineId = UUID.randomUUID().toString();
    String titleId = UUID.randomUUID().toString();
    String itemId = UUID.randomUUID().toString();
    String pieceId = UUID.randomUUID().toString();
    String instanceId = UUID.randomUUID().toString();
    JsonObject holding = new JsonObject().put(ID, holdingFromStorageId);
    holding.put(HOLDING_PERMANENT_LOCATION_ID, UUID.randomUUID().toString());
    Title title = new Title().withId(titleId).withPoLineId(lineId).withInstanceId(instanceId);
    Piece pieceFromStorage = new Piece().withId(pieceId).withTitleId(titleId).withItemId(itemId)
                                      .withPoLineId(lineId).withHoldingId(holdingFromStorageId).withFormat(Piece.Format.PHYSICAL);
    Piece pieceToUpdate = new Piece().withId(pieceId).withTitleId(titleId).withItemId(itemId)
                                     .withPoLineId(lineId).withHoldingId(holdingId).withFormat(Piece.Format.PHYSICAL);
    Location loc = new Location().withHoldingId(holdingId).withQuantityElectronic(1).withQuantity(1);
    Cost cost = new Cost().withQuantityElectronic(1);

    PoLine poLine = new PoLine().withIsPackage(false).withPurchaseOrderId(orderId).withId(lineId)
      .withOrderFormat(PoLine.OrderFormat.PHYSICAL_RESOURCE).withId(lineId)
      .withPhysical(new Physical().withCreateInventory(Physical.CreateInventory.INSTANCE_HOLDING_ITEM))
      .withLocations(List.of(loc)).withCost(cost);
    PurchaseOrder purchaseOrder = new PurchaseOrder().withId(orderId).withWorkflowStatus(PurchaseOrder.WorkflowStatus.OPEN);
    PieceUpdateHolder holder = new PieceUpdateHolder().withPieceToUpdate(pieceToUpdate).withPieceFromStorage(pieceFromStorage)
      .withCreateItem(true).withDeleteHolding(true);
    holder.withOrderInformation(purchaseOrder, poLine);

    doReturn(succeededFuture(title)).when(titlesService).getTitleById(pieceToUpdate.getTitleId(), requestContext);
    doReturn(succeededFuture(new JsonObject())).when(inventoryItemManager).getItemRecordById(pieceToUpdate.getItemId(), true, requestContext);
    doReturn(succeededFuture(List.of(itemId))).when(inventoryItemManager)
      .createMissingPhysicalItems(holder.getPurchaseOrderToSave(), holder.getPoLineToSave(), pieceToUpdate, 1, requestContext);
    doReturn(succeededFuture(null)).when(inventoryItemManager).updateItemWithPieceFields(holder.getPieceFromStorage(), holder.getPieceToUpdate(), requestContext);
    doReturn(succeededFuture(holding)).when(inventoryHoldingManager).getHoldingById(holder.getPieceFromStorage().getHoldingId(), true, requestContext);
    doReturn(succeededFuture(new ArrayList())).when(inventoryItemManager).getItemsByHoldingId(holder.getPieceFromStorage().getHoldingId(), requestContext);
    doReturn(succeededFuture(null)).when(pieceUpdateInventoryService).deleteHoldingConnectedToPiece(holder.getPieceFromStorage(), requestContext);
    doReturn(succeededFuture(itemId)).when(pieceUpdateInventoryService).manualPieceFlowCreateItemRecord(pieceToUpdate, holder.getPurchaseOrderToSave(), holder.getPoLineToSave(), requestContext);
    doReturn(succeededFuture(instanceId)).when(titlesService).updateTitleWithInstance(eq(title.getId()), eq(requestContext), eq(requestContext));

    pieceUpdateFlowInventoryManager.processInventory(holder, requestContext).result();

    verify(titlesService).updateTitleWithInstance(pieceToUpdate.getTitleId(), requestContext, requestContext);
    verify(inventoryHoldingManager, times(0)).getOrCreateHoldingsRecord(eq(holder.getInstanceId()), any(Location.class), eq(requestContext));
    verify(pieceUpdateInventoryService).deleteHoldingConnectedToPiece(holder.getPieceFromStorage(), requestContext);
    verify(inventoryItemManager).updateItemWithPieceFields(holder.getPieceFromStorage(), holder.getPieceToUpdate(), requestContext);
    verify(pieceUpdateInventoryService).manualPieceFlowCreateItemRecord(pieceToUpdate, holder.getPurchaseOrderToSave(), holder.getPoLineToSave(), requestContext);
  }

  @Test
  void shouldDeleteHoldingWhenPhysPieceUpdateForNonPackagePoLineWithCreateInventoryAllAndPieceContainsNewHoldingAndLineWithInstanceId() {
    String orderId = UUID.randomUUID().toString();
    String holdingFromStorageId = UUID.randomUUID().toString();
    String holdingId = UUID.randomUUID().toString();
    String lineId = UUID.randomUUID().toString();
    String titleId = UUID.randomUUID().toString();
    String itemId = UUID.randomUUID().toString();
    String pieceId = UUID.randomUUID().toString();
    String instanceId = UUID.randomUUID().toString();
    JsonObject holding = new JsonObject().put(ID, holdingFromStorageId);
    holding.put(HOLDING_PERMANENT_LOCATION_ID, UUID.randomUUID().toString());
    Piece pieceFromStorage = new Piece().withId(pieceId).withTitleId(titleId).withItemId(itemId)
      .withPoLineId(lineId).withHoldingId(holdingFromStorageId).withFormat(Piece.Format.PHYSICAL);
    Piece pieceToUpdate = new Piece().withId(pieceId).withTitleId(titleId).withItemId(itemId)
      .withPoLineId(lineId).withHoldingId(holdingId).withFormat(Piece.Format.PHYSICAL);
    Location loc = new Location().withHoldingId(holdingId).withQuantityElectronic(1).withQuantity(1);
    Cost cost = new Cost().withQuantityElectronic(1);

    PoLine poLine = new PoLine().withIsPackage(false).withPurchaseOrderId(orderId).withId(lineId)
      .withInstanceId(instanceId)
      .withOrderFormat(PoLine.OrderFormat.PHYSICAL_RESOURCE).withId(lineId)
      .withPhysical(new Physical().withCreateInventory(Physical.CreateInventory.INSTANCE_HOLDING_ITEM))
      .withLocations(List.of(loc)).withCost(cost);
    PurchaseOrder purchaseOrder = new PurchaseOrder().withId(orderId).withWorkflowStatus(PurchaseOrder.WorkflowStatus.OPEN);
    PieceUpdateHolder holder = new PieceUpdateHolder().withPieceToUpdate(pieceToUpdate).withPieceFromStorage(pieceFromStorage)
      .withCreateItem(true).withDeleteHolding(true);
    holder.withOrderInformation(purchaseOrder, poLine);

    doReturn(succeededFuture(new JsonObject())).when(inventoryItemManager).getItemRecordById(pieceToUpdate.getItemId(), true, requestContext);
    doReturn(succeededFuture(List.of(itemId))).when(inventoryItemManager)
      .createMissingPhysicalItems(holder.getPurchaseOrderToSave(), holder.getPoLineToSave(), pieceToUpdate, 1, requestContext);
    doReturn(succeededFuture(null)).when(inventoryItemManager).updateItemWithPieceFields(holder.getPieceFromStorage(), holder.getPieceToUpdate(), requestContext);
    doReturn(succeededFuture(holding)).when(inventoryHoldingManager).getHoldingById(holder.getPieceFromStorage().getHoldingId(), true, requestContext);
    doReturn(succeededFuture(new ArrayList())).when(inventoryItemManager).getItemsByHoldingId(holder.getPieceFromStorage().getHoldingId(), requestContext);
    doReturn(succeededFuture(null)).when(pieceUpdateInventoryService).deleteHoldingConnectedToPiece(holder.getPieceFromStorage(), requestContext);
    doReturn(succeededFuture(itemId)).when(pieceUpdateInventoryService).manualPieceFlowCreateItemRecord(pieceToUpdate, holder.getPurchaseOrderToSave(), holder.getPoLineToSave(), requestContext);

    pieceUpdateFlowInventoryManager.processInventory(holder, requestContext).result();

    verify(titlesService, times(0)).updateTitleWithInstance(pieceToUpdate.getTitleId(), requestContext, requestContext);
    verify(inventoryHoldingManager, times(0)).getOrCreateHoldingsRecord(eq(holder.getInstanceId()), any(Location.class), eq(requestContext));
    verify(pieceUpdateInventoryService).deleteHoldingConnectedToPiece(holder.getPieceFromStorage(), requestContext);
    verify(inventoryItemManager).updateItemWithPieceFields(holder.getPieceFromStorage(), holder.getPieceToUpdate(), requestContext);
    verify(pieceUpdateInventoryService).manualPieceFlowCreateItemRecord(pieceToUpdate, holder.getPurchaseOrderToSave(), holder.getPoLineToSave(), requestContext);
  }

  @Test
  void shouldUpdateItemWithNewHoldingIdWhenPhysPieceUpdateForNonPackagePoLineWithCreateInventoryAllAndPieceContainsNewHoldingAndLineWithInstanceId() {
    String orderId = UUID.randomUUID().toString();
    String holdingFromStorageId = UUID.randomUUID().toString();
    String holdingId = UUID.randomUUID().toString();
    String lineId = UUID.randomUUID().toString();
    String titleId = UUID.randomUUID().toString();
    String itemId = UUID.randomUUID().toString();
    String pieceId = UUID.randomUUID().toString();
    String instanceId = UUID.randomUUID().toString();
    JsonObject item = new JsonObject().put(ID, itemId);
    item.put(ITEM_STATUS, new JsonObject().put(ITEM_STATUS_NAME, ItemStatus.ON_ORDER.value()));
    item.put(ITEM_HOLDINGS_RECORD_ID, holdingFromStorageId);
    JsonObject holding = new JsonObject().put(ID, holdingFromStorageId);
    holding.put(HOLDING_PERMANENT_LOCATION_ID, UUID.randomUUID().toString());
    Piece pieceFromStorage = new Piece().withId(pieceId).withTitleId(titleId).withItemId(itemId)
      .withPoLineId(lineId).withHoldingId(holdingFromStorageId).withFormat(Piece.Format.PHYSICAL);
    Piece pieceToUpdate = new Piece().withId(pieceId).withTitleId(titleId).withItemId(itemId)
      .withPoLineId(lineId).withHoldingId(holdingId).withFormat(Piece.Format.PHYSICAL);
    Location loc = new Location().withHoldingId(holdingId).withQuantityElectronic(1).withQuantity(1);
    Cost cost = new Cost().withQuantityElectronic(1);

    PoLine poLine = new PoLine().withIsPackage(false).withPurchaseOrderId(orderId).withId(lineId)
      .withInstanceId(instanceId)
      .withOrderFormat(PoLine.OrderFormat.PHYSICAL_RESOURCE).withId(lineId)
      .withPhysical(new Physical().withCreateInventory(Physical.CreateInventory.INSTANCE_HOLDING_ITEM))
      .withLocations(List.of(loc)).withCost(cost);
    PurchaseOrder purchaseOrder = new PurchaseOrder().withId(orderId).withWorkflowStatus(PurchaseOrder.WorkflowStatus.OPEN);
    PieceUpdateHolder holder = new PieceUpdateHolder().withPieceToUpdate(pieceToUpdate).withPieceFromStorage(pieceFromStorage)
      .withCreateItem(true).withDeleteHolding(true);
    holder.withOrderInformation(purchaseOrder, poLine);

    doReturn(succeededFuture(item)).when(inventoryItemManager).getItemRecordById(pieceToUpdate.getItemId(), true, requestContext);
    doReturn(succeededFuture(null)).when(inventoryItemManager).updateItem(item, requestContext);

    doReturn(succeededFuture(List.of(itemId))).when(inventoryItemManager)
      .createMissingPhysicalItems(holder.getPurchaseOrderToSave(), holder.getPoLineToSave(), pieceToUpdate, 1, requestContext);
    doReturn(succeededFuture(null)).when(inventoryItemManager).updateItemWithPieceFields(holder.getPieceFromStorage(), holder.getPieceToUpdate(), requestContext);
    doReturn(succeededFuture(holding)).when(inventoryHoldingManager).getHoldingById(holder.getPieceFromStorage().getHoldingId(), true, requestContext);
    doReturn(succeededFuture(new ArrayList())).when(inventoryItemManager).getItemsByHoldingId(holder.getPieceFromStorage().getHoldingId(), requestContext);
    doReturn(succeededFuture(null)).when(pieceUpdateInventoryService).deleteHoldingConnectedToPiece(holder.getPieceFromStorage(), requestContext);

    pieceUpdateFlowInventoryManager.processInventory(holder, requestContext).result();

    assertEquals(holdingId, item.getString(ITEM_HOLDINGS_RECORD_ID));
    assertEquals(holder.getPoLineToSave().getId(), item.getString(ITEM_PURCHASE_ORDER_LINE_IDENTIFIER));
    verify(titlesService, times(0)).updateTitleWithInstance(pieceToUpdate.getTitleId(), requestContext, requestContext);
    verify(inventoryHoldingManager, times(0)).getOrCreateHoldingsRecord(eq(holder.getInstanceId()), any(Location.class), eq(requestContext));
    verify(pieceUpdateInventoryService).deleteHoldingConnectedToPiece(holder.getPieceFromStorage(), requestContext);
    verify(inventoryItemManager).updateItemWithPieceFields(holder.getPieceFromStorage(), holder.getPieceToUpdate(), requestContext);
    verify(pieceUpdateInventoryService, times(0)).manualPieceFlowCreateItemRecord(pieceToUpdate, holder.getPurchaseOrderToSave(), holder.getPoLineToSave(), requestContext);
    verify(inventoryItemManager).updateItem(item, requestContext);
  }

  @Test
  void shouldNotTryToUpdateItemWhenUpdatingPieceThatDoesNotHaveItem()
  {
    String orderId = UUID.randomUUID().toString();
    String holdingId = UUID.randomUUID().toString();
    String lineId = UUID.randomUUID().toString();
    String titleId = UUID.randomUUID().toString();
    String pieceId = UUID.randomUUID().toString();
    String instanceId = UUID.randomUUID().toString();
    String caption = "updated";
    JsonObject item = new JsonObject();
    JsonObject holding = new JsonObject().put(ID, holdingId);
    holding.put(HOLDING_PERMANENT_LOCATION_ID, UUID.randomUUID().toString());
    Piece pieceFromStorage = new Piece().withId(pieceId).withTitleId(titleId).withPoLineId(lineId).withHoldingId(holdingId).withFormat(Piece.Format.PHYSICAL);
    Piece pieceToUpdate = new Piece().withId(pieceId).withTitleId(titleId).withPoLineId(lineId).withHoldingId(holdingId).withFormat(Piece.Format.PHYSICAL).withDisplaySummary(caption);
    Location loc = new Location().withHoldingId(holdingId).withQuantityElectronic(1).withQuantity(1);
    Cost cost = new Cost().withQuantityElectronic(1);

    PoLine poLine = new PoLine().withIsPackage(false).withPurchaseOrderId(orderId).withId(lineId)
      .withInstanceId(instanceId)
      .withOrderFormat(PoLine.OrderFormat.PHYSICAL_RESOURCE).withId(lineId)
      .withPhysical(new Physical().withCreateInventory(Physical.CreateInventory.INSTANCE_HOLDING_ITEM))
      .withLocations(List.of(loc)).withCost(cost);
    PurchaseOrder purchaseOrder = new PurchaseOrder().withId(orderId).withWorkflowStatus(PurchaseOrder.WorkflowStatus.OPEN);
    PieceUpdateHolder holder = new PieceUpdateHolder().withPieceToUpdate(pieceToUpdate).withPieceFromStorage(pieceFromStorage)
      .withCreateItem(false).withDeleteHolding(false);
    holder.withOrderInformation(purchaseOrder, poLine);

    doReturn(succeededFuture(new JsonObject())).when(inventoryItemManager).getItemRecordById(null, true, requestContext);
    doReturn(succeededFuture(null)).when(inventoryItemManager).updateItemWithPieceFields(holder.getPieceFromStorage(), holder.getPieceToUpdate(), requestContext);

    pieceUpdateFlowInventoryManager.processInventory(holder, requestContext).result();

    verify(inventoryItemManager, times(1)).getItemRecordById(null, true, requestContext);
    verify(pieceUpdateInventoryService, times(0)).manualPieceFlowCreateItemRecord(pieceToUpdate, holder.getPurchaseOrderToSave(), holder.getPoLineToSave(), requestContext);
    verify(inventoryItemManager, times(0)).updateItem(item, requestContext);
  }

  private static class ContextConfiguration {
    @Bean TitlesService titlesService() {
      return mock(TitlesService.class);
    }

    @Bean
    TitleInstanceService titleInstanceService() {
      return mock(TitleInstanceService.class);
    }

    @Bean InventoryItemManager inventoryItemManager() {
      return mock(InventoryItemManager.class);
    }

    @Bean InventoryHoldingManager inventoryHoldingManager() {
      return mock(InventoryHoldingManager.class);
    }

    @Bean PieceUpdateInventoryService pieceUpdateInventoryService() {
      return mock(PieceUpdateInventoryService.class);
    }

    @Bean ItemRecreateInventoryService itemRecreateInventoryService() {
      return mock(ItemRecreateInventoryService.class);
    }

    @Bean
    PieceUpdateFlowInventoryManager pieceUpdateFlowInventoryManager(TitlesService titlesService,
                                                                    PieceUpdateInventoryService pieceUpdateInventoryService,
                                                                    ItemRecreateInventoryService itemRecreateInventoryService,
                                                                    InventoryItemManager inventoryItemManager,
                                                                    InventoryHoldingManager inventoryHoldingManager) {
      return new PieceUpdateFlowInventoryManager(titlesService, pieceUpdateInventoryService,
        itemRecreateInventoryService, inventoryItemManager, inventoryHoldingManager);
    }
  }
}
