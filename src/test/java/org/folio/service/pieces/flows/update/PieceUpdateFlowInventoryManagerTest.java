package org.folio.service.pieces.flows.update;

import static java.util.concurrent.CompletableFuture.completedFuture;
import static org.folio.TestConfig.autowireDependencies;
import static org.folio.TestConfig.clearServiceInteractions;
import static org.folio.TestConfig.clearVertxContext;
import static org.folio.TestConfig.getFirstContextFromVertx;
import static org.folio.TestConfig.getVertx;
import static org.folio.TestConfig.initSpringContext;
import static org.folio.TestConfig.isVerticleNotDeployed;
import static org.folio.TestConstants.ID;
import static org.folio.service.inventory.InventoryManager.HOLDING_PERMANENT_LOCATION_ID;
import static org.folio.service.inventory.InventoryManager.ITEM_HOLDINGS_RECORD_ID;
import static org.folio.service.inventory.InventoryManager.ITEM_PURCHASE_ORDER_LINE_IDENTIFIER;
import static org.folio.service.inventory.InventoryManager.ITEM_STATUS;
import static org.folio.service.inventory.InventoryManager.ITEM_STATUS_NAME;
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
import org.folio.service.inventory.InventoryManager;
import org.folio.service.pieces.PieceUpdateInventoryService;
import org.folio.service.titles.TitlesService;
import org.junit.jupiter.api.AfterAll;
import org.junit.jupiter.api.AfterEach;
import org.junit.jupiter.api.BeforeAll;
import org.junit.jupiter.api.BeforeEach;
import org.junit.jupiter.api.Test;
import org.mockito.Mock;
import org.mockito.Mockito;
import org.mockito.MockitoAnnotations;
import org.mockito.Spy;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.context.annotation.Bean;

import io.vertx.core.Context;
import io.vertx.core.json.JsonObject;

public class PieceUpdateFlowInventoryManagerTest {
  @Autowired
  PieceUpdateFlowInventoryManager pieceUpdateFlowInventoryManager;
  @Autowired
  TitlesService titlesService;
  @Autowired
  PieceUpdateInventoryService pieceUpdateInventoryService;
  @Autowired
  InventoryManager inventoryManager;


  @Spy
  private Context ctxMock = getFirstContextFromVertx(getVertx());
  @Mock
  private Map<String, String> okapiHeadersMock;

  private RequestContext requestContext;
  private static boolean runningOnOwn;

  @BeforeEach
  void initMocks(){
    MockitoAnnotations.openMocks(this);
    autowireDependencies(this);
    requestContext = new RequestContext(ctxMock, okapiHeadersMock);
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
    Mockito.reset(titlesService, inventoryManager, pieceUpdateInventoryService);
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
    PieceUpdateHolder holder = new PieceUpdateHolder(pieceToUpdate, true).withPieceFromStorage(pieceFromStorage);
    holder.shallowCopy(new PieceUpdateHolder(purchaseOrder, poLine));

    doReturn(completedFuture(title)).when(titlesService).getTitleById(pieceToUpdate.getTitleId(), requestContext);
    doReturn(completedFuture(new JsonObject())).when(inventoryManager).getItemRecordById(pieceToUpdate.getItemId(), true, requestContext);
    doReturn(completedFuture(List.of(itemId))).when(inventoryManager)
      .createMissingElectronicItems(holder.getPoLineToSave(), holdingId, 1, requestContext);
    doReturn(completedFuture(null)).when(inventoryManager).updateItemWithPoLineId(holder.getPieceToUpdate().getItemId(), holder.getPieceToUpdate().getPoLineId(), requestContext);
    doReturn(completedFuture(holding)).when(inventoryManager).getHoldingById(holder.getPieceFromStorage().getHoldingId(), true, requestContext);
    doReturn(completedFuture(new ArrayList())).when(inventoryManager).getItemsByHoldingId(holder.getPieceFromStorage().getHoldingId(), requestContext);
    doReturn(completedFuture(null)).when(pieceUpdateInventoryService).deleteHoldingById(holder.getPieceFromStorage().getHoldingId(), requestContext);
    doReturn(completedFuture(itemId)).when(pieceUpdateInventoryService).createItemRecord(holder.getPoLineToSave(), holdingId, requestContext);

    pieceUpdateFlowInventoryManager.processInventory(holder, requestContext).join();

    verify(titlesService).getTitleById(pieceToUpdate.getTitleId(), requestContext);
    verify(inventoryManager, times(0)).getOrCreateHoldingsRecord(eq(holder.getInstanceId()), any(Location.class), eq(requestContext));
    verify(pieceUpdateInventoryService).deleteHoldingById(holder.getPieceFromStorage().getHoldingId(), requestContext);
    verify(inventoryManager).updateItemWithPoLineId(holder.getPieceToUpdate().getItemId(), holder.getPieceToUpdate().getPoLineId(), requestContext);
    verify(pieceUpdateInventoryService).createItemRecord(holder.getPoLineToSave(), holdingId, requestContext);
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
    PieceUpdateHolder holder = new PieceUpdateHolder(pieceToUpdate, true).withPieceFromStorage(pieceFromStorage);
    holder.shallowCopy(new PieceUpdateHolder(purchaseOrder, poLine));

    doReturn(completedFuture(title)).when(titlesService).getTitleById(pieceToUpdate.getTitleId(), requestContext);
    doReturn(completedFuture(new JsonObject())).when(inventoryManager).getItemRecordById(pieceToUpdate.getItemId(), true, requestContext);
    doReturn(completedFuture(List.of(itemId))).when(inventoryManager)
      .createMissingPhysicalItems(holder.getPoLineToSave(), holdingId, 1, requestContext);
    doReturn(completedFuture(null)).when(inventoryManager).updateItemWithPoLineId(holder.getPieceToUpdate().getItemId(), holder.getPieceToUpdate().getPoLineId(), requestContext);
    doReturn(completedFuture(holding)).when(inventoryManager).getHoldingById(holder.getPieceFromStorage().getHoldingId(), true, requestContext);
    doReturn(completedFuture(new ArrayList())).when(inventoryManager).getItemsByHoldingId(holder.getPieceFromStorage().getHoldingId(), requestContext);
    doReturn(completedFuture(null)).when(pieceUpdateInventoryService).deleteHoldingById(holder.getPieceFromStorage().getHoldingId(), requestContext);
    doReturn(completedFuture(itemId)).when(pieceUpdateInventoryService).createItemRecord(holder.getPoLineToSave(), holdingId, requestContext);

    pieceUpdateFlowInventoryManager.processInventory(holder, requestContext).join();

    verify(titlesService).getTitleById(pieceToUpdate.getTitleId(), requestContext);
    verify(inventoryManager, times(0)).getOrCreateHoldingsRecord(eq(holder.getInstanceId()), any(Location.class), eq(requestContext));
    verify(pieceUpdateInventoryService).deleteHoldingById(holder.getPieceFromStorage().getHoldingId(), requestContext);
    verify(inventoryManager).updateItemWithPoLineId(holder.getPieceToUpdate().getItemId(), holder.getPieceToUpdate().getPoLineId(), requestContext);
    verify(pieceUpdateInventoryService).createItemRecord(holder.getPoLineToSave(), holdingId, requestContext);
  }

  @Test
  void shouldDeleteHoldingWhenPhysPieceUpdateForPackagePoLineWithCreateInventoryAllAndPieceContainsNewHoldin() {
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
    PieceUpdateHolder holder = new PieceUpdateHolder(pieceToUpdate, true).withPieceFromStorage(pieceFromStorage);
    holder.shallowCopy(new PieceUpdateHolder(purchaseOrder, poLine));

    doReturn(completedFuture(title)).when(titlesService).getTitleById(pieceToUpdate.getTitleId(), requestContext);

    doReturn(completedFuture(new JsonObject())).when(inventoryManager).getItemRecordById(pieceToUpdate.getItemId(), true, requestContext);
    doReturn(completedFuture(List.of(itemId))).when(inventoryManager)
      .createMissingPhysicalItems(holder.getPoLineToSave(), holdingId, 1, requestContext);
    doReturn(completedFuture(null)).when(inventoryManager).updateItemWithPoLineId(holder.getPieceToUpdate().getItemId(), holder.getPieceToUpdate().getPoLineId(), requestContext);
    doReturn(completedFuture(holding)).when(inventoryManager).getHoldingById(holder.getPieceFromStorage().getHoldingId(), true, requestContext);
    doReturn(completedFuture(new ArrayList())).when(inventoryManager).getItemsByHoldingId(holder.getPieceFromStorage().getHoldingId(), requestContext);
    doReturn(completedFuture(null)).when(pieceUpdateInventoryService).deleteHoldingById(holder.getPieceFromStorage().getHoldingId(), requestContext);
    doReturn(completedFuture(itemId)).when(pieceUpdateInventoryService).createItemRecord(holder.getPoLineToSave(), holdingId, requestContext);

    pieceUpdateFlowInventoryManager.processInventory(holder, requestContext).join();

    verify(titlesService).getTitleById(pieceToUpdate.getTitleId(), requestContext);
    verify(inventoryManager, times(0)).getOrCreateHoldingsRecord(eq(holder.getInstanceId()), any(Location.class), eq(requestContext));
    verify(pieceUpdateInventoryService).deleteHoldingById(holder.getPieceFromStorage().getHoldingId(), requestContext);
    verify(inventoryManager).updateItemWithPoLineId(holder.getPieceToUpdate().getItemId(), holder.getPieceToUpdate().getPoLineId(), requestContext);
    verify(pieceUpdateInventoryService).createItemRecord(holder.getPoLineToSave(), holdingId, requestContext);
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
    PieceUpdateHolder holder = new PieceUpdateHolder(pieceToUpdate, true).withPieceFromStorage(pieceFromStorage);
    holder.shallowCopy(new PieceUpdateHolder(purchaseOrder, poLine));

    doReturn(completedFuture(title)).when(titlesService).getTitleById(pieceToUpdate.getTitleId(), requestContext);
    doReturn(completedFuture(item)).when(inventoryManager).getItemRecordById(pieceToUpdate.getItemId(), true, requestContext);
    doReturn(completedFuture(null)).when(inventoryManager).updateItem(item, requestContext);

    doReturn(completedFuture(List.of(itemId))).when(inventoryManager)
      .createMissingPhysicalItems(holder.getPoLineToSave(), holdingId, 1, requestContext);
    doReturn(completedFuture(null)).when(inventoryManager).updateItemWithPoLineId(holder.getPieceToUpdate().getItemId(), holder.getPieceToUpdate().getPoLineId(), requestContext);
    doReturn(completedFuture(holding)).when(inventoryManager).getHoldingById(holder.getPieceFromStorage().getHoldingId(), true, requestContext);
    doReturn(completedFuture(new ArrayList())).when(inventoryManager).getItemsByHoldingId(holder.getPieceFromStorage().getHoldingId(), requestContext);
    doReturn(completedFuture(null)).when(pieceUpdateInventoryService).deleteHoldingById(holder.getPieceFromStorage().getHoldingId(), requestContext);

    pieceUpdateFlowInventoryManager.processInventory(holder, requestContext).join();

    assertEquals(holdingId, item.getString(ITEM_HOLDINGS_RECORD_ID));
    assertEquals(holder.getPoLineToSave().getId(), item.getString(ITEM_PURCHASE_ORDER_LINE_IDENTIFIER));
    verify(titlesService).getTitleById(pieceToUpdate.getTitleId(), requestContext);
    verify(inventoryManager, times(0)).getOrCreateHoldingsRecord(eq(holder.getInstanceId()), any(Location.class), eq(requestContext));
    verify(pieceUpdateInventoryService).deleteHoldingById(holder.getPieceFromStorage().getHoldingId(), requestContext);
    verify(inventoryManager).updateItemWithPoLineId(holder.getPieceToUpdate().getItemId(), holder.getPieceToUpdate().getPoLineId(), requestContext);
    verify(inventoryManager,times(0)).createMissingElectronicItems(holder.getPoLineToSave(), holdingId, 1, requestContext);
    verify(inventoryManager,times(0)).createMissingPhysicalItems(holder.getPoLineToSave(), holdingId, 1, requestContext);
    verify(inventoryManager).updateItem(item, requestContext);
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
    PieceUpdateHolder holder = new PieceUpdateHolder(pieceToUpdate, true).withPieceFromStorage(pieceFromStorage);
    holder.shallowCopy(new PieceUpdateHolder(purchaseOrder, poLine));

    doReturn(completedFuture(title)).when(titlesService).getTitleById(pieceToUpdate.getTitleId(), requestContext);
    doReturn(completedFuture(new JsonObject())).when(inventoryManager).getItemRecordById(pieceToUpdate.getItemId(), true, requestContext);
    doReturn(completedFuture(List.of(itemId))).when(inventoryManager)
                                  .createMissingElectronicItems(holder.getPoLineToSave(), holdingId, 1, requestContext);
    doReturn(completedFuture(null)).when(inventoryManager).updateItemWithPoLineId(holder.getPieceToUpdate().getItemId(), holder.getPieceToUpdate().getPoLineId(), requestContext);
    doReturn(completedFuture(holding)).when(inventoryManager).getHoldingById(holder.getPieceFromStorage().getHoldingId(), true, requestContext);
    doReturn(completedFuture(new ArrayList())).when(inventoryManager).getItemsByHoldingId(holder.getPieceFromStorage().getHoldingId(), requestContext);
    doReturn(completedFuture(null)).when(pieceUpdateInventoryService).deleteHoldingById(holder.getPieceFromStorage().getHoldingId(), requestContext);
    doReturn(completedFuture(itemId)).when(pieceUpdateInventoryService).createItemRecord(holder.getPoLineToSave(), holdingId, requestContext);

    pieceUpdateFlowInventoryManager.processInventory(holder, requestContext).join();

    verify(titlesService).getTitleById(pieceToUpdate.getTitleId(), requestContext);
    verify(inventoryManager, times(0)).getOrCreateHoldingsRecord(eq(holder.getInstanceId()), any(Location.class), eq(requestContext));
    verify(pieceUpdateInventoryService).deleteHoldingById(holder.getPieceFromStorage().getHoldingId(), requestContext);
    verify(inventoryManager).updateItemWithPoLineId(holder.getPieceToUpdate().getItemId(), holder.getPieceToUpdate().getPoLineId(), requestContext);
    verify(pieceUpdateInventoryService).createItemRecord(holder.getPoLineToSave(), holdingId, requestContext);
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
    PieceUpdateHolder holder = new PieceUpdateHolder(pieceToUpdate, true).withPieceFromStorage(pieceFromStorage);
    holder.shallowCopy(new PieceUpdateHolder(purchaseOrder, poLine));

    doReturn(completedFuture(title)).when(titlesService).getTitleById(pieceToUpdate.getTitleId(), requestContext);
    doReturn(completedFuture(new JsonObject())).when(inventoryManager).getItemRecordById(pieceToUpdate.getItemId(), true, requestContext);
    doReturn(completedFuture(List.of(itemId))).when(inventoryManager)
      .createMissingPhysicalItems(holder.getPoLineToSave(), holdingId, 1, requestContext);
    doReturn(completedFuture(null)).when(inventoryManager).updateItemWithPoLineId(holder.getPieceToUpdate().getItemId(), holder.getPieceToUpdate().getPoLineId(), requestContext);
    doReturn(completedFuture(holding)).when(inventoryManager).getHoldingById(holder.getPieceFromStorage().getHoldingId(), true, requestContext);
    doReturn(completedFuture(new ArrayList())).when(inventoryManager).getItemsByHoldingId(holder.getPieceFromStorage().getHoldingId(), requestContext);
    doReturn(completedFuture(null)).when(pieceUpdateInventoryService).deleteHoldingById(holder.getPieceFromStorage().getHoldingId(), requestContext);
    doReturn(completedFuture(itemId)).when(pieceUpdateInventoryService).createItemRecord(holder.getPoLineToSave(), holdingId, requestContext);

    pieceUpdateFlowInventoryManager.processInventory(holder, requestContext).join();

    verify(titlesService).getTitleById(pieceToUpdate.getTitleId(), requestContext);
    verify(inventoryManager, times(0)).getOrCreateHoldingsRecord(eq(holder.getInstanceId()), any(Location.class), eq(requestContext));
    verify(pieceUpdateInventoryService).deleteHoldingById(holder.getPieceFromStorage().getHoldingId(), requestContext);
    verify(inventoryManager).updateItemWithPoLineId(holder.getPieceToUpdate().getItemId(), holder.getPieceToUpdate().getPoLineId(), requestContext);
    verify(pieceUpdateInventoryService).createItemRecord(holder.getPoLineToSave(), holdingId, requestContext);
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
    PieceUpdateHolder holder = new PieceUpdateHolder(pieceToUpdate, true).withPieceFromStorage(pieceFromStorage);
    holder.shallowCopy(new PieceUpdateHolder(purchaseOrder, poLine));

    doReturn(completedFuture(new JsonObject())).when(inventoryManager).getItemRecordById(pieceToUpdate.getItemId(), true, requestContext);
    doReturn(completedFuture(List.of(itemId))).when(inventoryManager)
      .createMissingPhysicalItems(holder.getPoLineToSave(), holdingId, 1, requestContext);
    doReturn(completedFuture(null)).when(inventoryManager).updateItemWithPoLineId(holder.getPieceToUpdate().getItemId(), holder.getPieceToUpdate().getPoLineId(), requestContext);
    doReturn(completedFuture(holding)).when(inventoryManager).getHoldingById(holder.getPieceFromStorage().getHoldingId(), true, requestContext);
    doReturn(completedFuture(new ArrayList())).when(inventoryManager).getItemsByHoldingId(holder.getPieceFromStorage().getHoldingId(), requestContext);
    doReturn(completedFuture(null)).when(pieceUpdateInventoryService).deleteHoldingById(holder.getPieceFromStorage().getHoldingId(), requestContext);
    doReturn(completedFuture(itemId)).when(pieceUpdateInventoryService).createItemRecord(holder.getPoLineToSave(), holdingId, requestContext);

    pieceUpdateFlowInventoryManager.processInventory(holder, requestContext).join();

    verify(titlesService, times(0)).getTitleById(pieceToUpdate.getTitleId(), requestContext);
    verify(inventoryManager, times(0)).getOrCreateHoldingsRecord(eq(holder.getInstanceId()), any(Location.class), eq(requestContext));
    verify(pieceUpdateInventoryService).deleteHoldingById(holder.getPieceFromStorage().getHoldingId(), requestContext);
    verify(inventoryManager).updateItemWithPoLineId(holder.getPieceToUpdate().getItemId(), holder.getPieceToUpdate().getPoLineId(), requestContext);
    verify(pieceUpdateInventoryService).createItemRecord(holder.getPoLineToSave(), holdingId, requestContext);
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
    PieceUpdateHolder holder = new PieceUpdateHolder(pieceToUpdate, true).withPieceFromStorage(pieceFromStorage);
    holder.shallowCopy(new PieceUpdateHolder(purchaseOrder, poLine));

    doReturn(completedFuture(item)).when(inventoryManager).getItemRecordById(pieceToUpdate.getItemId(), true, requestContext);
    doReturn(completedFuture(null)).when(inventoryManager).updateItem(item, requestContext);

    doReturn(completedFuture(List.of(itemId))).when(inventoryManager)
      .createMissingPhysicalItems(holder.getPoLineToSave(), holdingId, 1, requestContext);
    doReturn(completedFuture(null)).when(inventoryManager).updateItemWithPoLineId(holder.getPieceToUpdate().getItemId(), holder.getPieceToUpdate().getPoLineId(), requestContext);
    doReturn(completedFuture(holding)).when(inventoryManager).getHoldingById(holder.getPieceFromStorage().getHoldingId(), true, requestContext);
    doReturn(completedFuture(new ArrayList())).when(inventoryManager).getItemsByHoldingId(holder.getPieceFromStorage().getHoldingId(), requestContext);
    doReturn(completedFuture(null)).when(pieceUpdateInventoryService).deleteHoldingById(holder.getPieceFromStorage().getHoldingId(), requestContext);

    pieceUpdateFlowInventoryManager.processInventory(holder, requestContext).join();

    assertEquals(holdingId, item.getString(ITEM_HOLDINGS_RECORD_ID));
    assertEquals(holder.getPoLineToSave().getId(), item.getString(ITEM_PURCHASE_ORDER_LINE_IDENTIFIER));
    verify(titlesService, times(0)).getTitleById(pieceToUpdate.getTitleId(), requestContext);
    verify(inventoryManager, times(0)).getOrCreateHoldingsRecord(eq(holder.getInstanceId()), any(Location.class), eq(requestContext));
    verify(pieceUpdateInventoryService).deleteHoldingById(holder.getPieceFromStorage().getHoldingId(), requestContext);
    verify(inventoryManager).updateItemWithPoLineId(holder.getPieceToUpdate().getItemId(), holder.getPieceToUpdate().getPoLineId(), requestContext);
    verify(pieceUpdateInventoryService, times(0)).createItemRecord(holder.getPoLineToSave(), holdingId, requestContext);
    verify(inventoryManager).updateItem(item, requestContext);
  }

  private static class ContextConfiguration {
    @Bean TitlesService titlesService() {
      return mock(TitlesService.class);
    }

    @Bean InventoryManager inventoryManager() {
      return mock(InventoryManager.class);
    }

    @Bean PieceUpdateInventoryService pieceUpdateInventoryService() {
      return mock(PieceUpdateInventoryService.class);
    }

    @Bean PieceUpdateFlowInventoryManager pieceUpdateFlowInventoryManager(TitlesService titlesService,
      PieceUpdateInventoryService pieceUpdateInventoryService, InventoryManager inventoryManager) {
      return new PieceUpdateFlowInventoryManager(titlesService, pieceUpdateInventoryService, inventoryManager);
    }
  }
}
