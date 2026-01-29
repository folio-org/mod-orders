package org.folio.service.orders.lines.update.instance;

import static io.vertx.core.Future.succeededFuture;
import static org.folio.TestConfig.clearServiceInteractions;
import static org.folio.TestUtils.getMockData;
import static org.folio.rest.core.exceptions.ErrorCodes.ITEM_UPDATE_FAILED;
import static org.folio.rest.impl.MockServer.HOLDINGS_OLD_NEW_PATH;
import static org.folio.service.inventory.InventoryItemManager.ID;
import static org.folio.service.inventory.InventoryItemManager.ITEM_HOLDINGS_RECORD_ID;
import static org.folio.service.inventory.InventoryItemManager.ITEM_STATUS;
import static org.folio.service.inventory.InventoryItemManager.ITEM_STATUS_NAME;
import static org.mockito.ArgumentMatchers.any;
import static org.mockito.ArgumentMatchers.anyString;
import static org.mockito.ArgumentMatchers.eq;
import static org.mockito.Mockito.doReturn;
import static org.mockito.Mockito.never;
import static org.mockito.Mockito.times;
import static org.mockito.Mockito.verify;
import static org.mockito.Mockito.when;

import java.io.IOException;
import java.util.ArrayList;
import java.util.List;
import java.util.UUID;
import java.util.stream.Collectors;

import org.folio.TestConstants;
import org.folio.models.ItemStatus;
import org.folio.models.orders.lines.update.OrderLineUpdateInstanceHolder;
import org.folio.rest.core.exceptions.ErrorCodes;
import org.folio.rest.core.exceptions.HttpException;
import org.folio.rest.core.models.RequestContext;
import org.folio.rest.jaxrs.model.Error;
import org.folio.rest.jaxrs.model.Location;
import org.folio.rest.jaxrs.model.Parameter;
import org.folio.rest.jaxrs.model.PatchOrderLineRequest;
import org.folio.rest.jaxrs.model.Physical;
import org.folio.rest.jaxrs.model.Piece;
import org.folio.rest.jaxrs.model.PoLine;
import org.folio.rest.jaxrs.model.ReplaceInstanceRef;
import org.folio.service.inventory.InventoryHoldingManager;
import org.folio.service.inventory.InventoryInstanceManager;
import org.folio.service.inventory.InventoryItemManager;
import org.folio.service.orders.PurchaseOrderLineService;
import org.folio.service.pieces.PieceStorageService;
import org.junit.jupiter.api.AfterEach;
import org.junit.jupiter.api.Assertions;
import org.junit.jupiter.api.BeforeEach;
import org.junit.jupiter.api.Test;
import org.junit.jupiter.api.extension.ExtendWith;
import org.mockito.InjectMocks;
import org.mockito.Mock;
import org.mockito.MockitoAnnotations;

import com.google.common.collect.Lists;

import io.vertx.core.Future;
import io.vertx.core.json.JsonObject;
import io.vertx.junit5.VertxExtension;
import io.vertx.junit5.VertxTestContext;

@ExtendWith(VertxExtension.class)
public class WithHoldingOrderLineUpdateInstanceStrategyTest {

  @InjectMocks
  private WithHoldingOrderLineUpdateInstanceStrategy withHoldingOrderLineUpdateInstanceStrategy;
  @Mock
  private InventoryInstanceManager inventoryInstanceManager;
  @Mock
  private InventoryItemManager inventoryItemManager;
  @Mock
  private InventoryHoldingManager inventoryHoldingManager;
  @Mock
  private PieceStorageService pieceStorageService;
  @Mock
  private PurchaseOrderLineService purchaseOrderLineService;
  @Mock
  private RequestContext requestContext;
  private AutoCloseable mockitoMocks;

  @BeforeEach
  void initMocks(){
    mockitoMocks = MockitoAnnotations.openMocks(this);
    doReturn(succeededFuture(Lists.newArrayList())).when(pieceStorageService).getPiecesByPoLineId(any(), any());
  }

  @AfterEach
  void resetMocks() throws Exception {
    clearServiceInteractions();
    mockitoMocks.close();
  }

  @Test
  public void updateInstanceForMoveHoldingOperation() throws IOException {
    String orderLineId = UUID.randomUUID().toString();
    String instanceId = UUID.randomUUID().toString();

    JsonObject holdingsCollection = new JsonObject(getMockData(HOLDINGS_OLD_NEW_PATH));

    List<JsonObject> holdings = holdingsCollection.getJsonArray("holdingsRecords").stream()
        .map(o -> ((JsonObject) o))
        .toList();

    List<String> holdingIds = holdings.stream().map(holding ->  holding.getString(ID)).toList();

    ArrayList<Location> locations = new ArrayList<>();
    locations.add(new Location()
        .withHoldingId(holdingIds.get(0))
        .withQuantity(1)
        .withQuantityPhysical(1));
    locations.add(new Location()
        .withHoldingId(holdingIds.get(1))
        .withQuantity(1)
        .withQuantityPhysical(1));

    PoLine poLine = new PoLine().
            withId(orderLineId).
            withOrderFormat(PoLine.OrderFormat.PHYSICAL_RESOURCE)
        .withPhysical(new Physical()
            .withCreateInventory(Physical.CreateInventory.INSTANCE_HOLDING))
        .withLocations(locations);

    PatchOrderLineRequest patchOrderLineRequest = new PatchOrderLineRequest();
    patchOrderLineRequest.withOperation(PatchOrderLineRequest.Operation.REPLACE_INSTANCE_REF)
        .withReplaceInstanceRef(new ReplaceInstanceRef()
            .withNewInstanceId(instanceId)
            .withHoldingsOperation(ReplaceInstanceRef.HoldingsOperation.MOVE)
            .withDeleteAbandonedHoldings(false));

    OrderLineUpdateInstanceHolder orderLineUpdateInstanceHolder = new OrderLineUpdateInstanceHolder()
        .withStoragePoLine(poLine).withPathOrderLineRequest(patchOrderLineRequest);

    doReturn(succeededFuture()).when(inventoryInstanceManager).createShadowInstanceIfNeeded(eq(instanceId), any(RequestContext.class));
    doReturn(succeededFuture(holdings)).when(inventoryHoldingManager).getHoldingsByIds(eq(holdingIds), eq(requestContext));
    doReturn(succeededFuture(null)).when(inventoryHoldingManager).updateInstanceForHoldingRecords(eq(holdings), eq(instanceId), eq(requestContext));

    withHoldingOrderLineUpdateInstanceStrategy.updateInstance(orderLineUpdateInstanceHolder, requestContext).result();

    verify(inventoryInstanceManager, times(1)).createShadowInstanceIfNeeded(eq(instanceId), any(RequestContext.class));
    verify(inventoryHoldingManager, times(1)).getHoldingsByIds(holdingIds, requestContext);
    verify(inventoryHoldingManager, times(1)).updateInstanceForHoldingRecords(holdings, instanceId, requestContext);
  }

  @Test
  public void updateInstanceWithNullReplaceInstanceRef() throws IOException {
    String orderLineId = UUID.randomUUID().toString();
    String instanceId = UUID.randomUUID().toString();

    JsonObject holdingsCollection = new JsonObject(getMockData(HOLDINGS_OLD_NEW_PATH));

    List<JsonObject> holdings = holdingsCollection.getJsonArray("holdingsRecords").stream()
      .map(o -> ((JsonObject) o))
      .toList();

    List<String> holdingIds = holdings.stream().map(holding ->  holding.getString(ID)).toList();

    ArrayList<Location> locations = new ArrayList<>();
    locations.add(new Location()
      .withHoldingId(holdingIds.get(0))
      .withQuantity(1)
      .withQuantityPhysical(1));
    locations.add(new Location()
      .withHoldingId(holdingIds.get(1))
      .withQuantity(1)
      .withQuantityPhysical(1));

    PoLine poLine = new PoLine().
        withId(orderLineId).
        withOrderFormat(PoLine.OrderFormat.PHYSICAL_RESOURCE)
      .withPhysical(new Physical()
        .withCreateInventory(Physical.CreateInventory.INSTANCE_HOLDING))
      .withLocations(locations);

    PatchOrderLineRequest patchOrderLineRequest = new PatchOrderLineRequest();
    patchOrderLineRequest.withOperation(PatchOrderLineRequest.Operation.REPLACE_INSTANCE_REF);

    OrderLineUpdateInstanceHolder orderLineUpdateInstanceHolder = new OrderLineUpdateInstanceHolder()
      .withStoragePoLine(poLine).withPathOrderLineRequest(patchOrderLineRequest);

    withHoldingOrderLineUpdateInstanceStrategy.updateInstance(orderLineUpdateInstanceHolder, requestContext).result();

    verify(inventoryHoldingManager, times(0)).getHoldingsByIds(holdingIds, requestContext);
    verify(inventoryHoldingManager, times(0)).updateInstanceForHoldingRecords(holdings, instanceId, requestContext);
  }

  @Test
  public void updateInstanceForFindOrCreateHoldingOperation() throws IOException {
    String orderLineId = UUID.randomUUID().toString();
    String instanceId = UUID.randomUUID().toString();

    JsonObject holdingsCollection = new JsonObject(getMockData(HOLDINGS_OLD_NEW_PATH));

    List<JsonObject> holdings = holdingsCollection.getJsonArray("holdingsRecords").stream()
        .map(o -> ((JsonObject) o))
        .toList();

    List<String> holdingIds = holdings.stream().map(holding ->  holding.getString(ID)).toList();

    ArrayList<Location> locations = new ArrayList<>();
    locations.add(new Location()
        .withHoldingId(holdingIds.get(0))
        .withQuantity(1)
        .withQuantityPhysical(1));
    locations.add(new Location()
        .withHoldingId(holdingIds.get(1))
        .withQuantity(1)
        .withQuantityPhysical(1));

    PoLine poLine = new PoLine().
            withId(orderLineId).
            withOrderFormat(PoLine.OrderFormat.PHYSICAL_RESOURCE)
        .withPhysical(new Physical()
            .withCreateInventory(Physical.CreateInventory.INSTANCE_HOLDING))
        .withLocations(locations);

    PatchOrderLineRequest patchOrderLineRequest = new PatchOrderLineRequest();
    patchOrderLineRequest.withOperation(PatchOrderLineRequest.Operation.REPLACE_INSTANCE_REF)
        .withReplaceInstanceRef(new ReplaceInstanceRef()
            .withNewInstanceId(instanceId)
            .withHoldingsOperation(ReplaceInstanceRef.HoldingsOperation.FIND_OR_CREATE)
            .withDeleteAbandonedHoldings(false));

    OrderLineUpdateInstanceHolder orderLineUpdateInstanceHolder = new OrderLineUpdateInstanceHolder()
        .withStoragePoLine(poLine)
        .withPathOrderLineRequest(patchOrderLineRequest);

    List<JsonObject> items = holdingIds.stream().map(holdingId -> {
      JsonObject item = new JsonObject().put(TestConstants.ID, UUID.randomUUID().toString());
      item.put(ITEM_STATUS, new JsonObject().put(ITEM_STATUS_NAME, ItemStatus.ON_ORDER.value()));
      item.put(ITEM_HOLDINGS_RECORD_ID, holdingId);
      return item;
    }).toList();

    doReturn(succeededFuture()).when(inventoryInstanceManager).createShadowInstanceIfNeeded(eq(instanceId), any(RequestContext.class));
    doReturn(succeededFuture(UUID.randomUUID().toString())).when(inventoryHoldingManager)
        .getOrCreateHoldingRecordByInstanceAndLocation(eq(instanceId), eq(locations.get(0)), eq(requestContext));
    doReturn(succeededFuture(UUID.randomUUID().toString())).when(inventoryHoldingManager)
        .getOrCreateHoldingRecordByInstanceAndLocation(eq(instanceId), eq(locations.get(1)), eq(requestContext));
    doReturn(succeededFuture(List.of(items.get(0)))).when(inventoryItemManager).getItemsByHoldingIdAndOrderLineId(eq(holdingIds.get(0)), eq(orderLineId), eq(requestContext));
    doReturn(succeededFuture(List.of(items.get(1)))).when(inventoryItemManager).getItemsByHoldingIdAndOrderLineId(eq(holdingIds.get(1)), eq(orderLineId), eq(requestContext));
    doReturn(succeededFuture(null)).when(inventoryItemManager).updateItem(any(JsonObject.class), eq(requestContext));

    withHoldingOrderLineUpdateInstanceStrategy.updateInstance(orderLineUpdateInstanceHolder, requestContext).result();

    verify(inventoryInstanceManager, times(1)).createShadowInstanceIfNeeded(eq(instanceId), any(RequestContext.class));
    verify(inventoryHoldingManager, times(1)).getOrCreateHoldingRecordByInstanceAndLocation(instanceId, locations.get(0), requestContext);
    verify(inventoryHoldingManager, never()).getOrCreateHoldingRecordByInstanceAndLocation(instanceId, locations.get(1), requestContext);
    verify(inventoryItemManager, times(1)).getItemsByHoldingIdAndOrderLineId(anyString(), anyString(), any(RequestContext.class));
    verify(inventoryItemManager, times(1)).batchUpdatePartialItems(any(), any(RequestContext.class));
  }

  @Test
  public void updateInstanceAndItemsForFindOrCreateHoldingOperation() throws IOException {
    String itemId = UUID.randomUUID().toString();
    String instanceId = UUID.randomUUID().toString();

    JsonObject holdingsCollection = new JsonObject(getMockData(HOLDINGS_OLD_NEW_PATH));

    JsonObject holding = holdingsCollection.getJsonArray("holdingsRecords").getJsonObject(0);

    String holdingId = holding.getString(ID);
    String orderLineId = holding.getString("purchaseOrderLineIdentifier");

    ArrayList<Location> locations = new ArrayList<>();
    locations.add(new Location()
      .withHoldingId(holdingId)
      .withQuantity(1)
      .withQuantityPhysical(1));
    PoLine poLine = new PoLine().
        withId(orderLineId).
        withOrderFormat(PoLine.OrderFormat.PHYSICAL_RESOURCE)
      .withPhysical(new Physical()
        .withCreateInventory(Physical.CreateInventory.INSTANCE_HOLDING_ITEM))
      .withLocations(locations);

    PatchOrderLineRequest patchOrderLineRequest = new PatchOrderLineRequest();
    patchOrderLineRequest.withOperation(PatchOrderLineRequest.Operation.REPLACE_INSTANCE_REF)
      .withReplaceInstanceRef(new ReplaceInstanceRef()
        .withNewInstanceId(instanceId)
        .withHoldingsOperation(ReplaceInstanceRef.HoldingsOperation.FIND_OR_CREATE)
        .withDeleteAbandonedHoldings(false));

    OrderLineUpdateInstanceHolder orderLineUpdateInstanceHolder = new OrderLineUpdateInstanceHolder()
      .withStoragePoLine(poLine)
      .withPathOrderLineRequest(patchOrderLineRequest);

    JsonObject item = new JsonObject().put(TestConstants.ID, itemId);
    item.put(ITEM_STATUS, new JsonObject().put(ITEM_STATUS_NAME, ItemStatus.ON_ORDER.value()));
    item.put(ITEM_HOLDINGS_RECORD_ID, holdingId);

    doReturn(succeededFuture()).when(inventoryInstanceManager).createShadowInstanceIfNeeded(eq(instanceId), any(RequestContext.class));
    doReturn(succeededFuture(UUID.randomUUID().toString())).when(inventoryHoldingManager)
      .getOrCreateHoldingRecordByInstanceAndLocation(eq(instanceId), eq(locations.getFirst()), eq(requestContext));
    doReturn(succeededFuture(List.of(item))).when(inventoryItemManager).getItemsByHoldingIdAndOrderLineId(eq(holdingId), eq(orderLineId), eq(requestContext));
    doReturn(succeededFuture(null)).when(inventoryItemManager).batchUpdatePartialItems(any(), eq(requestContext));

    withHoldingOrderLineUpdateInstanceStrategy.updateInstance(orderLineUpdateInstanceHolder, requestContext).result();

    verify(inventoryInstanceManager, times(1)).createShadowInstanceIfNeeded(eq(instanceId), any(RequestContext.class));
    verify(inventoryHoldingManager, times(1)).getOrCreateHoldingRecordByInstanceAndLocation(instanceId, locations.getFirst(), requestContext);
    verify(inventoryItemManager, times(1)).getItemsByHoldingIdAndOrderLineId(holdingId, orderLineId, requestContext);
    verify(inventoryItemManager, times(1)).batchUpdatePartialItems(any(), eq(requestContext));
  }

  @Test
  public void updateInstanceAndNotUpdateItemsForFindOrCreateHoldingOperationWhenHoldingsFound() throws IOException {
    String instanceId = UUID.randomUUID().toString();

    JsonObject holdingsCollection = new JsonObject(getMockData(HOLDINGS_OLD_NEW_PATH));

    JsonObject holding = holdingsCollection.getJsonArray("holdingsRecords").getJsonObject(0);

    String holdingId = holding.getString(ID);
    String orderLineId = holding.getString("purchaseOrderLineIdentifier");

    ArrayList<Location> locations = new ArrayList<>();
    locations.add(new Location()
      .withHoldingId(holdingId)
      .withQuantity(1)
      .withQuantityPhysical(1));
    PoLine poLine = new PoLine().
      withId(orderLineId).
      withOrderFormat(PoLine.OrderFormat.PHYSICAL_RESOURCE)
      .withPhysical(new Physical()
        .withCreateInventory(Physical.CreateInventory.INSTANCE_HOLDING_ITEM))
      .withLocations(locations);

    PatchOrderLineRequest patchOrderLineRequest = new PatchOrderLineRequest();
    patchOrderLineRequest.withOperation(PatchOrderLineRequest.Operation.REPLACE_INSTANCE_REF)
      .withReplaceInstanceRef(new ReplaceInstanceRef()
        .withNewInstanceId(instanceId)
        .withHoldingsOperation(ReplaceInstanceRef.HoldingsOperation.FIND_OR_CREATE)
        .withDeleteAbandonedHoldings(false));

    OrderLineUpdateInstanceHolder orderLineUpdateInstanceHolder = new OrderLineUpdateInstanceHolder()
      .withStoragePoLine(poLine)
      .withPathOrderLineRequest(patchOrderLineRequest);

    doReturn(succeededFuture()).when(inventoryInstanceManager).createShadowInstanceIfNeeded(eq(instanceId), any(RequestContext.class));
    doReturn(succeededFuture(holdingId)).when(inventoryHoldingManager)
      .getOrCreateHoldingRecordByInstanceAndLocation(eq(instanceId), eq(locations.getFirst()), eq(requestContext));

    withHoldingOrderLineUpdateInstanceStrategy.updateInstance(orderLineUpdateInstanceHolder, requestContext).result();

    verify(inventoryInstanceManager, times(1)).createShadowInstanceIfNeeded(eq(instanceId), any(RequestContext.class));
    verify(inventoryHoldingManager, times(1)).getOrCreateHoldingRecordByInstanceAndLocation(instanceId, locations.getFirst(), requestContext);
    verify(inventoryItemManager, never()).getItemsByHoldingIdAndOrderLineId(anyString(), anyString(), any(RequestContext.class));
    verify(inventoryItemManager, never()).updateItem(any(JsonObject.class), any(RequestContext.class));
  }

  @Test
  public void updateInstanceForFindOrCreateHoldingOperationAndDeleteAbandonedHoldings(VertxTestContext vertxTestContext) throws IOException {
    String instanceId = UUID.randomUUID().toString();

    List<JsonObject> holdings = new JsonObject(getMockData(HOLDINGS_OLD_NEW_PATH))
      .getJsonArray("holdingsRecords").stream()
      .map(o -> ((JsonObject) o))
      .collect(Collectors.toCollection(ArrayList::new));
    holdings.addLast(JsonObject.of(
        ID, UUID.randomUUID().toString(),
        "purchaseOrderLineIdentifier", UUID.randomUUID().toString()
    ));

    List<String> holdingIds = holdings.stream().map(holding ->  holding.getString(ID)).toList();
    String usedHoldingId = holdingIds.getLast();
    String orderLineId = holdings.getFirst().getString("purchaseOrderLineIdentifier");

    ArrayList<Location> locations = new ArrayList<>();
    locations.add(new Location()
        .withHoldingId(holdingIds.get(0))
        .withQuantity(1)
        .withQuantityPhysical(1));
    locations.add(new Location()
        .withHoldingId(holdingIds.get(1))
        .withQuantity(1)
        .withQuantityPhysical(1));
    locations.add(new Location()
        .withHoldingId(usedHoldingId)
        .withQuantity(1)
        .withQuantityPhysical(1));

    PoLine poLine = new PoLine().
            withId(orderLineId).
            withOrderFormat(PoLine.OrderFormat.PHYSICAL_RESOURCE)
        .withPhysical(new Physical()
            .withCreateInventory(Physical.CreateInventory.INSTANCE_HOLDING))
        .withLocations(locations);

    PatchOrderLineRequest patchOrderLineRequest = new PatchOrderLineRequest();
    patchOrderLineRequest.withOperation(PatchOrderLineRequest.Operation.REPLACE_INSTANCE_REF)
        .withReplaceInstanceRef(new ReplaceInstanceRef()
            .withNewInstanceId(instanceId)
            .withHoldingsOperation(ReplaceInstanceRef.HoldingsOperation.FIND_OR_CREATE)
            .withDeleteAbandonedHoldings(true));

    OrderLineUpdateInstanceHolder orderLineUpdateInstanceHolder = new OrderLineUpdateInstanceHolder()
        .withStoragePoLine(poLine)
        .withPathOrderLineRequest(patchOrderLineRequest);

    List<JsonObject> items = holdingIds.stream().filter(id -> !id.equals(usedHoldingId))
      .map(holdingId -> JsonObject.of(
        TestConstants.ID, UUID.randomUUID().toString(),
        ITEM_STATUS, new JsonObject().put(ITEM_STATUS_NAME, ItemStatus.ON_ORDER.value()),
        ITEM_HOLDINGS_RECORD_ID, holdingId))
      .toList();

    when(inventoryInstanceManager.createShadowInstanceIfNeeded(eq(instanceId), any(RequestContext.class))).thenReturn(succeededFuture());
    when(inventoryHoldingManager.getOrCreateHoldingRecordByInstanceAndLocation(eq(instanceId), eq(locations.get(0)), eq(requestContext))).thenReturn(succeededFuture(UUID.randomUUID().toString()));
    when(inventoryHoldingManager.getOrCreateHoldingRecordByInstanceAndLocation(eq(instanceId), eq(locations.get(1)), eq(requestContext))).thenReturn(succeededFuture(UUID.randomUUID().toString()));
    when(inventoryHoldingManager.getOrCreateHoldingRecordByInstanceAndLocation(eq(instanceId), eq(locations.get(2)), eq(requestContext))).thenReturn(succeededFuture(UUID.randomUUID().toString()));
    when(inventoryItemManager.getItemsByHoldingId(eq(holdingIds.get(0)), eq(requestContext))).thenReturn(succeededFuture(new ArrayList<>()));
    when(inventoryItemManager.getItemsByHoldingId(eq(holdingIds.get(1)), eq(requestContext))).thenReturn(succeededFuture(new ArrayList<>()));
    when(inventoryItemManager.getItemsByHoldingId(eq(usedHoldingId), eq(requestContext))).thenReturn(succeededFuture(new ArrayList<>()));
    when(inventoryItemManager.getItemsByHoldingIdAndOrderLineId(eq(holdingIds.get(0)), eq(orderLineId), any(RequestContext.class))).thenReturn(succeededFuture(List.of(items.get(0))));
    when(inventoryItemManager.getItemsByHoldingIdAndOrderLineId(eq(holdingIds.get(1)), eq(orderLineId), any(RequestContext.class))).thenReturn(succeededFuture(List.of(items.get(1))));
    when(inventoryItemManager.getItemsByHoldingIdAndOrderLineId(eq(usedHoldingId), eq(orderLineId), any(RequestContext.class))).thenReturn(succeededFuture(List.of()));
    when(inventoryHoldingManager.deleteHoldingById(eq(holdingIds.get(0)), eq(true), any(RequestContext.class))).thenReturn(succeededFuture(null));
    when(inventoryHoldingManager.deleteHoldingById(eq(holdingIds.get(1)), eq(true), any(RequestContext.class))).thenReturn(succeededFuture(null));
    when(inventoryItemManager.batchUpdatePartialItems(any(), eq(requestContext))).thenReturn(succeededFuture(null));
    when(pieceStorageService.getPiecesByHoldingIds(holdingIds, requestContext)).thenReturn(succeededFuture(List.of(new Piece().withHoldingId(usedHoldingId))));
    when(purchaseOrderLineService.getPoLinesByHoldingIds(holdingIds, requestContext)).thenReturn(succeededFuture(List.of(new PoLine().withLocations(List.of(new Location().withHoldingId(usedHoldingId))))));

    var future = withHoldingOrderLineUpdateInstanceStrategy.updateInstance(orderLineUpdateInstanceHolder, requestContext);

    vertxTestContext.assertComplete(future).onComplete(res-> {
      verify(inventoryInstanceManager, times(3)).createShadowInstanceIfNeeded(eq(instanceId), any(RequestContext.class));
      verify(inventoryHoldingManager, times(1)).getOrCreateHoldingRecordByInstanceAndLocation(instanceId, locations.get(0), requestContext);
      verify(inventoryHoldingManager, times(1)).getOrCreateHoldingRecordByInstanceAndLocation(instanceId, locations.get(1), requestContext);
      verify(inventoryHoldingManager, times(1)).getOrCreateHoldingRecordByInstanceAndLocation(instanceId, locations.get(2), requestContext);
      verify(inventoryHoldingManager, times(1)).deleteHoldingById(eq(holdingIds.get(0)), eq(true), any(RequestContext.class));
      verify(inventoryHoldingManager, times(1)).deleteHoldingById(eq(holdingIds.get(1)), eq(true), any(RequestContext.class));
      verify(inventoryHoldingManager, times(0)).deleteHoldingById(eq(usedHoldingId), eq(true), any(RequestContext.class));
      verify(inventoryItemManager, times(3)).getItemsByHoldingIdAndOrderLineId(anyString(), anyString(), any(RequestContext.class));
      verify(inventoryItemManager, times(3)).batchUpdatePartialItems(any(), any(RequestContext.class));
      vertxTestContext.completeNow();
    });
  }

  @Test
  public void updateInstanceForCreateHoldingOperation() throws IOException {
    String orderLineId = UUID.randomUUID().toString();
    String instanceId = UUID.randomUUID().toString();

    JsonObject holdingsCollection = new JsonObject(getMockData(HOLDINGS_OLD_NEW_PATH));

    List<JsonObject> holdings = holdingsCollection.getJsonArray("holdingsRecords").stream()
        .map(o -> ((JsonObject) o))
        .toList();

    List<String> holdingIds = holdings.stream().map(holding ->  holding.getString(ID)).toList();

    ArrayList<Location> locations = new ArrayList<>();
    locations.add(new Location()
        .withHoldingId(holdingIds.get(0))
        .withQuantity(1)
        .withQuantityPhysical(1));
    locations.add(new Location()
        .withHoldingId(holdingIds.get(1))
        .withQuantity(1)
        .withQuantityPhysical(1));

    PoLine poLine = new PoLine().
            withId(orderLineId).
            withOrderFormat(PoLine.OrderFormat.PHYSICAL_RESOURCE)
        .withPhysical(new Physical()
            .withCreateInventory(Physical.CreateInventory.INSTANCE_HOLDING))
        .withLocations(locations);

    List<JsonObject> items = holdingIds.stream().map(holdingId -> {
      JsonObject item = new JsonObject().put(TestConstants.ID, UUID.randomUUID().toString());
      item.put(ITEM_STATUS, new JsonObject().put(ITEM_STATUS_NAME, ItemStatus.ON_ORDER.value()));
      item.put(ITEM_HOLDINGS_RECORD_ID, holdingId);
      return item;
    }).toList();

    PatchOrderLineRequest patchOrderLineRequest = new PatchOrderLineRequest();
    patchOrderLineRequest.withOperation(PatchOrderLineRequest.Operation.REPLACE_INSTANCE_REF)
        .withReplaceInstanceRef(new ReplaceInstanceRef()
            .withNewInstanceId(instanceId)
            .withHoldingsOperation(ReplaceInstanceRef.HoldingsOperation.CREATE)
            .withDeleteAbandonedHoldings(false));

    OrderLineUpdateInstanceHolder orderLineUpdateInstanceHolder = new OrderLineUpdateInstanceHolder()
        .withStoragePoLine(poLine).withPathOrderLineRequest(patchOrderLineRequest);

    doReturn(succeededFuture()).when(inventoryInstanceManager).createShadowInstanceIfNeeded(eq(instanceId), any(RequestContext.class));
    doReturn(succeededFuture(UUID.randomUUID().toString())).when(inventoryHoldingManager)
        .createHolding(eq(instanceId), eq(locations.get(0)), eq(requestContext));
    doReturn(succeededFuture(UUID.randomUUID().toString())).when(inventoryHoldingManager)
        .createHolding(eq(instanceId), eq(locations.get(1)), eq(requestContext));
    doReturn(succeededFuture(List.of(items.get(0)))).when(inventoryItemManager).getItemsByHoldingIdAndOrderLineId(eq(holdingIds.get(0)), eq(orderLineId), eq(requestContext));
    doReturn(succeededFuture(List.of(items.get(1)))).when(inventoryItemManager).getItemsByHoldingIdAndOrderLineId(eq(holdingIds.get(1)), eq(orderLineId), eq(requestContext));
    doReturn(succeededFuture(null)).when(inventoryItemManager).batchUpdatePartialItems(any(), eq(requestContext));

    withHoldingOrderLineUpdateInstanceStrategy.updateInstance(orderLineUpdateInstanceHolder, requestContext).result();

    verify(inventoryInstanceManager, times(2)).createShadowInstanceIfNeeded(eq(instanceId), any(RequestContext.class));
    verify(inventoryHoldingManager, times(1)).createHolding(instanceId, locations.get(0), requestContext);
    verify(inventoryHoldingManager, times(1)).createHolding(instanceId, locations.get(1), requestContext);
    verify(inventoryItemManager, times(2)).getItemsByHoldingIdAndOrderLineId(anyString(), anyString(), any(RequestContext.class));
    verify(inventoryItemManager, times(2)).batchUpdatePartialItems(any(), any(RequestContext.class));
  }

  @Test
  void updateInstanceAndItemsForCreateHoldingOperation() throws Exception {
    String instanceId = UUID.randomUUID().toString();
    String itemId = UUID.randomUUID().toString();

    JsonObject holdingsCollection = new JsonObject(getMockData(HOLDINGS_OLD_NEW_PATH));

    JsonObject holding = holdingsCollection.getJsonArray("holdingsRecords").getJsonObject(0);

    String holdingId = holding.getString(ID);
    String orderLineId = holding.getString("purchaseOrderLineIdentifier");

    ArrayList<Location> locations = new ArrayList<>();
    locations.add(new Location()
        .withHoldingId(holdingId)
        .withQuantity(1)
        .withQuantityPhysical(1));

    JsonObject item = new JsonObject().put(TestConstants.ID, itemId);
    item.put(ITEM_STATUS, new JsonObject().put(ITEM_STATUS_NAME, ItemStatus.ON_ORDER.value()));
    item.put(ITEM_HOLDINGS_RECORD_ID, holdingId);

    PoLine poLine = new PoLine().
            withId(orderLineId).
            withOrderFormat(PoLine.OrderFormat.PHYSICAL_RESOURCE)
        .withPhysical(new Physical()
            .withCreateInventory(Physical.CreateInventory.INSTANCE_HOLDING_ITEM))
        .withLocations(locations);

    PatchOrderLineRequest patchOrderLineRequest = new PatchOrderLineRequest();
    patchOrderLineRequest.withOperation(PatchOrderLineRequest.Operation.REPLACE_INSTANCE_REF)
        .withReplaceInstanceRef(new ReplaceInstanceRef()
            .withNewInstanceId(instanceId)
            .withHoldingsOperation(ReplaceInstanceRef.HoldingsOperation.CREATE)
            .withDeleteAbandonedHoldings(false));

    OrderLineUpdateInstanceHolder orderLineUpdateInstanceHolder = new OrderLineUpdateInstanceHolder()
        .withStoragePoLine(poLine).withPathOrderLineRequest(patchOrderLineRequest);

    doReturn(succeededFuture()).when(inventoryInstanceManager).createShadowInstanceIfNeeded(eq(instanceId), any(RequestContext.class));
    doReturn(succeededFuture(UUID.randomUUID().toString())).when(inventoryHoldingManager)
        .createHolding(eq(instanceId), eq(locations.getFirst()), eq(requestContext));
    doReturn(succeededFuture(List.of(item))).when(inventoryItemManager).getItemsByHoldingIdAndOrderLineId(eq(holdingId), eq(orderLineId), eq(requestContext));
    doReturn(succeededFuture(null)).when(inventoryItemManager).batchUpdatePartialItems(any(), eq(requestContext));

    withHoldingOrderLineUpdateInstanceStrategy.updateInstance(orderLineUpdateInstanceHolder, requestContext).result();

    verify(inventoryInstanceManager, times(1)).createShadowInstanceIfNeeded(eq(instanceId), any(RequestContext.class));
    verify(inventoryHoldingManager, times(1)).createHolding(instanceId, locations.getFirst(), requestContext);
    verify(inventoryItemManager, times(1)).getItemsByHoldingIdAndOrderLineId(holdingId, orderLineId, requestContext);
    verify(inventoryItemManager, times(1)).batchUpdatePartialItems(any(), eq(requestContext));
  }

  @Test
  public void updateInstanceAndItemsWithItemsNotUpdatesCorrect(VertxTestContext vertxTestContext) throws IOException {
    String instanceId = UUID.randomUUID().toString();
    String itemId = UUID.randomUUID().toString();

    JsonObject holdingsCollection = new JsonObject(getMockData(HOLDINGS_OLD_NEW_PATH));

    JsonObject holding = holdingsCollection.getJsonArray("holdingsRecords").getJsonObject(0);

    String holdingId = holding.getString(ID);
    String orderLineId = holding.getString("purchaseOrderLineIdentifier");

    ArrayList<Location> locations = new ArrayList<>();
    locations.add(new Location()
        .withHoldingId(holdingId)
        .withQuantity(1)
        .withQuantityPhysical(1));

    JsonObject item = new JsonObject().put(TestConstants.ID, itemId);
    item.put(ITEM_STATUS, new JsonObject().put(ITEM_STATUS_NAME, ItemStatus.ON_ORDER.value()));
    item.put(ITEM_HOLDINGS_RECORD_ID, holdingId);

    PoLine poLine = new PoLine().
            withId(orderLineId).
            withOrderFormat(PoLine.OrderFormat.PHYSICAL_RESOURCE)
        .withPhysical(new Physical()
            .withCreateInventory(Physical.CreateInventory.INSTANCE_HOLDING_ITEM))
        .withLocations(locations);

    PatchOrderLineRequest patchOrderLineRequest = new PatchOrderLineRequest();
    patchOrderLineRequest.withOperation(PatchOrderLineRequest.Operation.REPLACE_INSTANCE_REF)
        .withReplaceInstanceRef(new ReplaceInstanceRef()
            .withNewInstanceId(instanceId)
            .withHoldingsOperation(ReplaceInstanceRef.HoldingsOperation.CREATE)
            .withDeleteAbandonedHoldings(false));

    OrderLineUpdateInstanceHolder orderLineUpdateInstanceHolder = new OrderLineUpdateInstanceHolder()
        .withStoragePoLine(poLine).withPathOrderLineRequest(patchOrderLineRequest);

    doReturn(succeededFuture()).when(inventoryInstanceManager).createShadowInstanceIfNeeded(eq(instanceId), any(RequestContext.class));
    doReturn(succeededFuture(UUID.randomUUID().toString())).when(inventoryHoldingManager)
        .createHolding(eq(instanceId), eq(locations.getFirst()), eq(requestContext));
    doReturn(succeededFuture(List.of(item))).when(inventoryItemManager).getItemsByHoldingIdAndOrderLineId(eq(holdingId), eq(orderLineId), eq(requestContext));
    doReturn(Future.failedFuture(new HttpException(500, ErrorCodes.GENERIC_ERROR_CODE))).when(inventoryItemManager).batchUpdatePartialItems(any(), eq(requestContext));

    vertxTestContext.assertFailure(withHoldingOrderLineUpdateInstanceStrategy.updateInstance(orderLineUpdateInstanceHolder, requestContext))
      .onComplete(res-> {
        HttpException actHttpException = (HttpException) res.cause();

        Error actError = actHttpException.getErrors().getErrors().getFirst();
        Parameter actParameter = actError.getParameters().getFirst();

        Assertions.assertEquals(ITEM_UPDATE_FAILED.getCode(), actError.getCode());
        Assertions.assertEquals(ITEM_UPDATE_FAILED.getDescription(), actError.getMessage());
        Assertions.assertEquals("itemId", actParameter.getKey());
        Assertions.assertEquals(itemId, actParameter.getValue());

        verify(inventoryInstanceManager, times(1)).createShadowInstanceIfNeeded(eq(instanceId), any(RequestContext.class));
        verify(inventoryHoldingManager, times(1)).createHolding(instanceId, locations.getFirst(), requestContext);
        verify(inventoryItemManager, times(1)).getItemsByHoldingIdAndOrderLineId(holdingId, orderLineId, requestContext);
        verify(inventoryItemManager, times(1)).batchUpdatePartialItems(any(), eq(requestContext));
        vertxTestContext.completeNow();
      });
  }

  @Test
  void testUpdateItemsWithMaterialTypeAndPermanentLoanType() throws IOException {
    // given
    var instanceId = UUID.randomUUID().toString();
    var itemId = UUID.randomUUID().toString();
    var materialTypeId = UUID.randomUUID().toString();
    var permanentLoanTypeId = UUID.randomUUID().toString();

    var holdingsCollection = new JsonObject(getMockData(HOLDINGS_OLD_NEW_PATH));
    var holding = holdingsCollection.getJsonArray("holdingsRecords").getJsonObject(0);
    var holdingId = holding.getString(ID);
    var orderLineId = holding.getString("purchaseOrderLineIdentifier");

    var locations = new ArrayList<Location>();
    locations.add(new Location()
      .withHoldingId(holdingId)
      .withQuantity(1)
      .withQuantityPhysical(1));

    var item = new JsonObject()
      .put(TestConstants.ID, itemId)
      .put(ITEM_STATUS, new JsonObject().put(ITEM_STATUS_NAME, ItemStatus.ON_ORDER.value()))
      .put(ITEM_HOLDINGS_RECORD_ID, holdingId)
      .put("materialType", new JsonObject().put(ID, materialTypeId))
      .put("permanentLoanType", new JsonObject().put(ID, permanentLoanTypeId))
      .put("_version", "1");

    var poLine = new PoLine()
      .withId(orderLineId)
      .withOrderFormat(PoLine.OrderFormat.PHYSICAL_RESOURCE)
      .withPhysical(new Physical().withCreateInventory(Physical.CreateInventory.INSTANCE_HOLDING_ITEM))
      .withLocations(locations);

    var patchOrderLineRequest = new PatchOrderLineRequest()
      .withOperation(PatchOrderLineRequest.Operation.REPLACE_INSTANCE_REF)
      .withReplaceInstanceRef(new ReplaceInstanceRef()
        .withNewInstanceId(instanceId)
        .withHoldingsOperation(ReplaceInstanceRef.HoldingsOperation.CREATE)
        .withDeleteAbandonedHoldings(false));

    var orderLineUpdateInstanceHolder = new OrderLineUpdateInstanceHolder()
      .withStoragePoLine(poLine).withPathOrderLineRequest(patchOrderLineRequest);

    doReturn(succeededFuture()).when(inventoryInstanceManager).createShadowInstanceIfNeeded(eq(instanceId), any(RequestContext.class));
    doReturn(succeededFuture(UUID.randomUUID().toString())).when(inventoryHoldingManager)
      .createHolding(eq(instanceId), eq(locations.getFirst()), eq(requestContext));
    doReturn(succeededFuture(List.of(item))).when(inventoryItemManager).getItemsByHoldingIdAndOrderLineId(eq(holdingId), eq(orderLineId), eq(requestContext));
    doReturn(succeededFuture(null)).when(inventoryItemManager).batchUpdatePartialItems(any(), eq(requestContext));

    // when
    withHoldingOrderLineUpdateInstanceStrategy.updateInstance(orderLineUpdateInstanceHolder, requestContext).result();

    // then
    verify(inventoryItemManager, times(1)).batchUpdatePartialItems(any(), eq(requestContext));
  }

  @Test
  void testUpdateItemsWithMaterialTypeOnly() throws IOException {
    // given
    var instanceId = UUID.randomUUID().toString();
    var itemId = UUID.randomUUID().toString();
    var materialTypeId = UUID.randomUUID().toString();

    var holdingsCollection = new JsonObject(getMockData(HOLDINGS_OLD_NEW_PATH));
    var holding = holdingsCollection.getJsonArray("holdingsRecords").getJsonObject(0);
    var holdingId = holding.getString(ID);
    var orderLineId = holding.getString("purchaseOrderLineIdentifier");

    var locations = new ArrayList<Location>();
    locations.add(new Location()
      .withHoldingId(holdingId)
      .withQuantity(1)
      .withQuantityPhysical(1));

    var item = new JsonObject()
      .put(TestConstants.ID, itemId)
      .put(ITEM_STATUS, new JsonObject().put(ITEM_STATUS_NAME, ItemStatus.ON_ORDER.value()))
      .put(ITEM_HOLDINGS_RECORD_ID, holdingId)
      .put("materialType", new JsonObject().put(ID, materialTypeId))
      .put("_version", "1");

    var poLine = new PoLine()
      .withId(orderLineId)
      .withOrderFormat(PoLine.OrderFormat.PHYSICAL_RESOURCE)
      .withPhysical(new Physical().withCreateInventory(Physical.CreateInventory.INSTANCE_HOLDING_ITEM))
      .withLocations(locations);

    var patchOrderLineRequest = new PatchOrderLineRequest()
      .withOperation(PatchOrderLineRequest.Operation.REPLACE_INSTANCE_REF)
      .withReplaceInstanceRef(new ReplaceInstanceRef()
        .withNewInstanceId(instanceId)
        .withHoldingsOperation(ReplaceInstanceRef.HoldingsOperation.CREATE)
        .withDeleteAbandonedHoldings(false));

    var orderLineUpdateInstanceHolder = new OrderLineUpdateInstanceHolder()
      .withStoragePoLine(poLine).withPathOrderLineRequest(patchOrderLineRequest);

    doReturn(succeededFuture()).when(inventoryInstanceManager).createShadowInstanceIfNeeded(eq(instanceId), any(RequestContext.class));
    doReturn(succeededFuture(UUID.randomUUID().toString())).when(inventoryHoldingManager)
      .createHolding(eq(instanceId), eq(locations.getFirst()), eq(requestContext));
    doReturn(succeededFuture(List.of(item))).when(inventoryItemManager).getItemsByHoldingIdAndOrderLineId(eq(holdingId), eq(orderLineId), eq(requestContext));
    doReturn(succeededFuture(null)).when(inventoryItemManager).batchUpdatePartialItems(any(), eq(requestContext));

    // when
    withHoldingOrderLineUpdateInstanceStrategy.updateInstance(orderLineUpdateInstanceHolder, requestContext).result();

    // then
    verify(inventoryItemManager, times(1)).batchUpdatePartialItems(any(), eq(requestContext));
  }

  @Test
  void testUpdateItemsWithPermanentLoanTypeOnly() throws IOException {
    // given
    var instanceId = UUID.randomUUID().toString();
    var itemId = UUID.randomUUID().toString();
    var permanentLoanTypeId = UUID.randomUUID().toString();

    var holdingsCollection = new JsonObject(getMockData(HOLDINGS_OLD_NEW_PATH));
    var holding = holdingsCollection.getJsonArray("holdingsRecords").getJsonObject(0);
    var holdingId = holding.getString(ID);
    var orderLineId = holding.getString("purchaseOrderLineIdentifier");

    var locations = new ArrayList<Location>();
    locations.add(new Location()
      .withHoldingId(holdingId)
      .withQuantity(1)
      .withQuantityPhysical(1));

    var item = new JsonObject()
      .put(TestConstants.ID, itemId)
      .put(ITEM_STATUS, new JsonObject().put(ITEM_STATUS_NAME, ItemStatus.ON_ORDER.value()))
      .put(ITEM_HOLDINGS_RECORD_ID, holdingId)
      .put("permanentLoanType", new JsonObject().put(ID, permanentLoanTypeId))
      .put("_version", "1");

    var poLine = new PoLine()
      .withId(orderLineId)
      .withOrderFormat(PoLine.OrderFormat.PHYSICAL_RESOURCE)
      .withPhysical(new Physical().withCreateInventory(Physical.CreateInventory.INSTANCE_HOLDING_ITEM))
      .withLocations(locations);

    var patchOrderLineRequest = new PatchOrderLineRequest()
      .withOperation(PatchOrderLineRequest.Operation.REPLACE_INSTANCE_REF)
      .withReplaceInstanceRef(new ReplaceInstanceRef()
        .withNewInstanceId(instanceId)
        .withHoldingsOperation(ReplaceInstanceRef.HoldingsOperation.CREATE)
        .withDeleteAbandonedHoldings(false));

    var orderLineUpdateInstanceHolder = new OrderLineUpdateInstanceHolder()
      .withStoragePoLine(poLine).withPathOrderLineRequest(patchOrderLineRequest);

    doReturn(succeededFuture()).when(inventoryInstanceManager).createShadowInstanceIfNeeded(eq(instanceId), any(RequestContext.class));
    doReturn(succeededFuture(UUID.randomUUID().toString())).when(inventoryHoldingManager)
      .createHolding(eq(instanceId), eq(locations.getFirst()), eq(requestContext));
    doReturn(succeededFuture(List.of(item))).when(inventoryItemManager).getItemsByHoldingIdAndOrderLineId(eq(holdingId), eq(orderLineId), eq(requestContext));
    doReturn(succeededFuture(null)).when(inventoryItemManager).batchUpdatePartialItems(any(), eq(requestContext));

    // when
    withHoldingOrderLineUpdateInstanceStrategy.updateInstance(orderLineUpdateInstanceHolder, requestContext).result();

    // then
    verify(inventoryItemManager, times(1)).batchUpdatePartialItems(any(), eq(requestContext));
  }

  @Test
  void testUpdateItemsWithoutMaterialTypeAndPermanentLoanType() throws IOException {
    // given
    var instanceId = UUID.randomUUID().toString();
    var itemId = UUID.randomUUID().toString();

    var holdingsCollection = new JsonObject(getMockData(HOLDINGS_OLD_NEW_PATH));
    var holding = holdingsCollection.getJsonArray("holdingsRecords").getJsonObject(0);
    var holdingId = holding.getString(ID);
    var orderLineId = holding.getString("purchaseOrderLineIdentifier");

    var locations = new ArrayList<Location>();
    locations.add(new Location()
      .withHoldingId(holdingId)
      .withQuantity(1)
      .withQuantityPhysical(1));

    var item = new JsonObject()
      .put(TestConstants.ID, itemId)
      .put(ITEM_STATUS, new JsonObject().put(ITEM_STATUS_NAME, ItemStatus.ON_ORDER.value()))
      .put(ITEM_HOLDINGS_RECORD_ID, holdingId)
      .put("_version", "1");

    var poLine = new PoLine()
      .withId(orderLineId)
      .withOrderFormat(PoLine.OrderFormat.PHYSICAL_RESOURCE)
      .withPhysical(new Physical().withCreateInventory(Physical.CreateInventory.INSTANCE_HOLDING_ITEM))
      .withLocations(locations);

    var patchOrderLineRequest = new PatchOrderLineRequest()
      .withOperation(PatchOrderLineRequest.Operation.REPLACE_INSTANCE_REF)
      .withReplaceInstanceRef(new ReplaceInstanceRef()
        .withNewInstanceId(instanceId)
        .withHoldingsOperation(ReplaceInstanceRef.HoldingsOperation.CREATE)
        .withDeleteAbandonedHoldings(false));

    var orderLineUpdateInstanceHolder = new OrderLineUpdateInstanceHolder()
      .withStoragePoLine(poLine).withPathOrderLineRequest(patchOrderLineRequest);

    doReturn(succeededFuture()).when(inventoryInstanceManager).createShadowInstanceIfNeeded(eq(instanceId), any(RequestContext.class));
    doReturn(succeededFuture(UUID.randomUUID().toString())).when(inventoryHoldingManager)
      .createHolding(eq(instanceId), eq(locations.getFirst()), eq(requestContext));
    doReturn(succeededFuture(List.of(item))).when(inventoryItemManager).getItemsByHoldingIdAndOrderLineId(eq(holdingId), eq(orderLineId), eq(requestContext));
    doReturn(succeededFuture(null)).when(inventoryItemManager).batchUpdatePartialItems(any(), eq(requestContext));

    // when
    withHoldingOrderLineUpdateInstanceStrategy.updateInstance(orderLineUpdateInstanceHolder, requestContext).result();

    // then
    verify(inventoryItemManager, times(1)).batchUpdatePartialItems(any(), eq(requestContext));
  }

  @Test
  void testUpdateItemsWithBlankMaterialTypeAndPermanentLoanType() throws IOException {
    // given
    var instanceId = UUID.randomUUID().toString();
    var itemId = UUID.randomUUID().toString();

    var holdingsCollection = new JsonObject(getMockData(HOLDINGS_OLD_NEW_PATH));
    var holding = holdingsCollection.getJsonArray("holdingsRecords").getJsonObject(0);
    var holdingId = holding.getString(ID);
    var orderLineId = holding.getString("purchaseOrderLineIdentifier");

    var locations = new ArrayList<Location>();
    locations.add(new Location()
      .withHoldingId(holdingId)
      .withQuantity(1)
      .withQuantityPhysical(1));

    var item = new JsonObject()
      .put(TestConstants.ID, itemId)
      .put(ITEM_STATUS, new JsonObject().put(ITEM_STATUS_NAME, ItemStatus.ON_ORDER.value()))
      .put(ITEM_HOLDINGS_RECORD_ID, holdingId)
      .put("materialType", "")
      .put("permanentLoanType", "")
      .put("_version", "1");

    var poLine = new PoLine()
      .withId(orderLineId)
      .withOrderFormat(PoLine.OrderFormat.PHYSICAL_RESOURCE)
      .withPhysical(new Physical().withCreateInventory(Physical.CreateInventory.INSTANCE_HOLDING_ITEM))
      .withLocations(locations);

    var patchOrderLineRequest = new PatchOrderLineRequest()
      .withOperation(PatchOrderLineRequest.Operation.REPLACE_INSTANCE_REF)
      .withReplaceInstanceRef(new ReplaceInstanceRef()
        .withNewInstanceId(instanceId)
        .withHoldingsOperation(ReplaceInstanceRef.HoldingsOperation.CREATE)
        .withDeleteAbandonedHoldings(false));

    var orderLineUpdateInstanceHolder = new OrderLineUpdateInstanceHolder()
      .withStoragePoLine(poLine).withPathOrderLineRequest(patchOrderLineRequest);

    doReturn(succeededFuture()).when(inventoryInstanceManager).createShadowInstanceIfNeeded(eq(instanceId), any(RequestContext.class));
    doReturn(succeededFuture(UUID.randomUUID().toString())).when(inventoryHoldingManager)
      .createHolding(eq(instanceId), eq(locations.getFirst()), eq(requestContext));
    doReturn(succeededFuture(List.of(item))).when(inventoryItemManager).getItemsByHoldingIdAndOrderLineId(eq(holdingId), eq(orderLineId), eq(requestContext));
    doReturn(succeededFuture(null)).when(inventoryItemManager).batchUpdatePartialItems(any(), eq(requestContext));

    // when
    withHoldingOrderLineUpdateInstanceStrategy.updateInstance(orderLineUpdateInstanceHolder, requestContext).result();

    // then
    verify(inventoryItemManager, times(1)).batchUpdatePartialItems(any(), eq(requestContext));
  }

  @Test
  void testUpdateItemsWithNonJsonObjectMaterialType() throws IOException {
    // given
    var instanceId = UUID.randomUUID().toString();
    var itemId = UUID.randomUUID().toString();

    var holdingsCollection = new JsonObject(getMockData(HOLDINGS_OLD_NEW_PATH));
    var holding = holdingsCollection.getJsonArray("holdingsRecords").getJsonObject(0);
    var holdingId = holding.getString(ID);
    var orderLineId = holding.getString("purchaseOrderLineIdentifier");

    var locations = new ArrayList<Location>();
    locations.add(new Location()
      .withHoldingId(holdingId)
      .withQuantity(1)
      .withQuantityPhysical(1));

    var item = new JsonObject()
      .put(TestConstants.ID, itemId)
      .put(ITEM_STATUS, new JsonObject().put(ITEM_STATUS_NAME, ItemStatus.ON_ORDER.value()))
      .put(ITEM_HOLDINGS_RECORD_ID, holdingId)
      .put("materialType", "not-a-json-object")
      .put("permanentLoanType", "not-a-json-object")
      .put("_version", "1");

    var poLine = new PoLine()
      .withId(orderLineId)
      .withOrderFormat(PoLine.OrderFormat.PHYSICAL_RESOURCE)
      .withPhysical(new Physical().withCreateInventory(Physical.CreateInventory.INSTANCE_HOLDING_ITEM))
      .withLocations(locations);

    var patchOrderLineRequest = new PatchOrderLineRequest()
      .withOperation(PatchOrderLineRequest.Operation.REPLACE_INSTANCE_REF)
      .withReplaceInstanceRef(new ReplaceInstanceRef()
        .withNewInstanceId(instanceId)
        .withHoldingsOperation(ReplaceInstanceRef.HoldingsOperation.CREATE)
        .withDeleteAbandonedHoldings(false));

    var orderLineUpdateInstanceHolder = new OrderLineUpdateInstanceHolder()
      .withStoragePoLine(poLine).withPathOrderLineRequest(patchOrderLineRequest);

    doReturn(succeededFuture()).when(inventoryInstanceManager).createShadowInstanceIfNeeded(eq(instanceId), any(RequestContext.class));
    doReturn(succeededFuture(UUID.randomUUID().toString())).when(inventoryHoldingManager)
      .createHolding(eq(instanceId), eq(locations.getFirst()), eq(requestContext));
    doReturn(succeededFuture(List.of(item))).when(inventoryItemManager).getItemsByHoldingIdAndOrderLineId(eq(holdingId), eq(orderLineId), eq(requestContext));
    doReturn(succeededFuture(null)).when(inventoryItemManager).batchUpdatePartialItems(any(), eq(requestContext));

    // when
    withHoldingOrderLineUpdateInstanceStrategy.updateInstance(orderLineUpdateInstanceHolder, requestContext).result();

    // then
    verify(inventoryItemManager, times(1)).batchUpdatePartialItems(any(), eq(requestContext));
  }
}
