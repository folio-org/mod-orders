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
import static org.mockito.ArgumentMatchers.anyInt;
import static org.mockito.ArgumentMatchers.anyString;
import static org.mockito.ArgumentMatchers.argThat;
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

import org.apache.commons.lang3.tuple.Pair;
import org.folio.TestConstants;
import org.folio.TestMate;
import org.folio.models.ItemStatus;
import org.folio.models.orders.lines.update.OrderLineUpdateInstanceHolder;
import org.folio.rest.core.exceptions.ErrorCodes;
import org.folio.rest.core.exceptions.HttpException;
import org.folio.rest.core.models.RequestContext;
import org.folio.rest.jaxrs.model.Error;
import org.folio.rest.jaxrs.model.Parameter;
import org.folio.rest.jaxrs.model.PatchOrderLineRequest;
import org.folio.rest.jaxrs.model.Physical;
import org.folio.rest.jaxrs.model.Piece;
import org.folio.rest.jaxrs.model.PoLine;
import org.folio.rest.jaxrs.model.ReplaceInstanceRef;
import org.folio.rest.jaxrs.model.acq.Location;
import org.folio.service.HoldingDeletionService;
import org.folio.service.batch.BatchTrackingService;
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
import java.util.Collections;
import java.util.Map;
import org.folio.okapi.common.XOkapiHeaders;
import static org.assertj.core.api.Assertions.assertThat;
import org.mockito.ArgumentCaptor;
import io.vertx.core.json.JsonArray;
import org.folio.rest.acq.model.StorageReplaceOrderLineHoldingRefs;

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
  private BatchTrackingService batchTrackingService;
  @Mock
  private HoldingDeletionService holdingDeletionService;
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
    doReturn(succeededFuture()).when(batchTrackingService).createBatchTrackingRecord(anyString(), anyInt(), eq(requestContext));

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
    doReturn(succeededFuture()).when(batchTrackingService).createBatchTrackingRecord(anyString(), anyInt(), eq(requestContext));
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
    when(inventoryHoldingManager.getHoldingById(eq(holdingIds.get(0)), eq(true), any(RequestContext.class))).thenReturn(succeededFuture(holdings.get(0).put("permanentLocationId", UUID.randomUUID().toString())));
    when(inventoryHoldingManager.getHoldingById(eq(holdingIds.get(1)), eq(true), any(RequestContext.class))).thenReturn(succeededFuture(holdings.get(1).put("permanentLocationId", UUID.randomUUID().toString())));
    when(inventoryHoldingManager.getHoldingById(eq(usedHoldingId), eq(true), any(RequestContext.class))).thenReturn(succeededFuture(holdings.get(2).put("permanentLocationId", UUID.randomUUID().toString())));
    when(inventoryItemManager.batchUpdatePartialItems(any(), eq(requestContext))).thenReturn(succeededFuture(null));
    when(pieceStorageService.getPiecesByHoldingIds(holdingIds, requestContext)).thenReturn(succeededFuture(List.of(new Piece().withHoldingId(usedHoldingId))));
    when(purchaseOrderLineService.getPoLinesByHoldingIds(holdingIds, requestContext)).thenReturn(succeededFuture(List.of(new PoLine().withLocations(List.of(new Location().withHoldingId(usedHoldingId))))));
    // getPiecesByHoldingId (singular) is called per-holding in deleteAbandonedHoldingsAndUpdateHolder to build excludePieceIds for HoldingDataExclusionConfig (4-arg constructor, PO_LINE_CHANGE_INSTANCE mode)
    when(pieceStorageService.getPiecesByHoldingId(eq(holdingIds.get(0)), any(RequestContext.class))).thenReturn(succeededFuture(List.of()));
    when(pieceStorageService.getPiecesByHoldingId(eq(holdingIds.get(1)), any(RequestContext.class))).thenReturn(succeededFuture(List.of()));
    when(pieceStorageService.getPiecesByHoldingId(eq(usedHoldingId), any(RequestContext.class))).thenReturn(succeededFuture(List.of(new Piece().withId(UUID.randomUUID().toString()).withHoldingId(usedHoldingId))));
    when(batchTrackingService.createBatchTrackingRecord(anyString(), anyInt(), eq(requestContext))).thenReturn(succeededFuture());
    // Mock HoldingDeletionService methods
    when(holdingDeletionService.getHoldingLinkedData(any(), any(), any(), any())).thenAnswer(invocation -> {
      JsonObject holding = invocation.getArgument(0);
      String holdingId = holding.getString("id");
      // Return deletable for holdings that are not used
      boolean isDeletable = !holdingId.equals(usedHoldingId);
      return succeededFuture(Pair.of(isDeletable, isDeletable ? holding : new JsonObject()));
    });
    when(holdingDeletionService.deleteHoldingIfPossible(any(), any())).thenAnswer(invocation -> {
      Pair<Boolean, JsonObject> deletableHoldings = invocation.getArgument(0);
      if (deletableHoldings.getKey() && !deletableHoldings.getValue().isEmpty()) {
        JsonObject holding = deletableHoldings.getValue();
        String holdingId = holding.getString("id");
        String permanentLocationId = holding.getString("permanentLocationId");
        return succeededFuture(Pair.of(holdingId, permanentLocationId));
      }
      return succeededFuture(null);
    });

    var future = withHoldingOrderLineUpdateInstanceStrategy.updateInstance(orderLineUpdateInstanceHolder, requestContext);

    vertxTestContext.assertComplete(future).onComplete(res-> {
      verify(inventoryInstanceManager, times(3)).createShadowInstanceIfNeeded(eq(instanceId), any(RequestContext.class));
      verify(inventoryHoldingManager, times(1)).getOrCreateHoldingRecordByInstanceAndLocation(instanceId, locations.get(0), requestContext);
      verify(inventoryHoldingManager, times(1)).getOrCreateHoldingRecordByInstanceAndLocation(instanceId, locations.get(1), requestContext);
      verify(inventoryHoldingManager, times(1)).getOrCreateHoldingRecordByInstanceAndLocation(instanceId, locations.get(2), requestContext);
      // Verify HoldingDeletionService methods are called (3 holdings total)
      verify(holdingDeletionService, times(3)).getHoldingLinkedData(any(), any(), any(), any());
      // Only 2 holdings should be deleted (the ones that are not used)
      verify(holdingDeletionService, times(2)).deleteHoldingIfPossible(argThat(Pair::getKey), any());
      // The used holding should not be deleted
      verify(holdingDeletionService, times(1)).deleteHoldingIfPossible(argThat(pair -> !pair.getKey()), any());
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
    doReturn(succeededFuture()).when(batchTrackingService).createBatchTrackingRecord(anyString(), anyInt(), eq(requestContext));
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
    doReturn(succeededFuture()).when(batchTrackingService).createBatchTrackingRecord(anyString(), anyInt(), eq(requestContext));
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
    doReturn(succeededFuture()).when(batchTrackingService).createBatchTrackingRecord(anyString(), anyInt(), eq(requestContext));
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
    doReturn(succeededFuture()).when(batchTrackingService).createBatchTrackingRecord(anyString(), anyInt(), eq(requestContext));
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
    doReturn(succeededFuture()).when(batchTrackingService).createBatchTrackingRecord(anyString(), anyInt(), eq(requestContext));
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
    doReturn(succeededFuture()).when(batchTrackingService).createBatchTrackingRecord(anyString(), anyInt(), eq(requestContext));
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
    doReturn(succeededFuture()).when(batchTrackingService).createBatchTrackingRecord(anyString(), anyInt(), eq(requestContext));
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
    doReturn(succeededFuture()).when(batchTrackingService).createBatchTrackingRecord(anyString(), anyInt(), eq(requestContext));

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
    doReturn(succeededFuture()).when(batchTrackingService).createBatchTrackingRecord(anyString(), anyInt(), eq(requestContext));
    doReturn(succeededFuture(null)).when(inventoryItemManager).batchUpdatePartialItems(any(), eq(requestContext));

    // when
    withHoldingOrderLineUpdateInstanceStrategy.updateInstance(orderLineUpdateInstanceHolder, requestContext).result();

    // then
    verify(inventoryItemManager, times(1)).batchUpdatePartialItems(any(), eq(requestContext));
  }

  @Test
  @TestMate(name = "TestMate-a23ac7ff68d7df16378fe40380351580")
  void processHoldingsShouldCreateShadowInstanceInTargetTenantBeforeHoldingOperations() {
    // Given
    String orderLineId = "5097457a-977a-42c9-9430-8456f913d8f1";
    String newInstanceId = "cd3288a4-898c-4347-a003-2d810ef70f03";
    String originalHoldingId = "12345678-1234-1234-1234-123456789012";
    String newHoldingId = "87654321-4321-4321-4321-210987654321";
    String targetTenantId = "member-tenant-1";
    Location location = new Location()
      .withHoldingId(originalHoldingId)
      .withTenantId(targetTenantId)
      .withQuantity(1)
      .withQuantityPhysical(1);
    PoLine poLine = new PoLine()
      .withId(orderLineId)
      .withLocations(List.of(location));
    ReplaceInstanceRef replaceInstanceRef = new ReplaceInstanceRef()
      .withNewInstanceId(newInstanceId)
      .withHoldingsOperation(ReplaceInstanceRef.HoldingsOperation.CREATE)
      .withDeleteAbandonedHoldings(false);
    PatchOrderLineRequest patchOrderLineRequest = new PatchOrderLineRequest()
      .withOperation(PatchOrderLineRequest.Operation.REPLACE_INSTANCE_REF)
      .withReplaceInstanceRef(replaceInstanceRef);
    OrderLineUpdateInstanceHolder holder = new OrderLineUpdateInstanceHolder()
      .withStoragePoLine(poLine)
      .withPathOrderLineRequest(patchOrderLineRequest);
    doReturn(succeededFuture(Collections.emptyList())).when(pieceStorageService).getPiecesByPoLineId(any(), any());
    doReturn(succeededFuture()).when(inventoryInstanceManager).createShadowInstanceIfNeeded(eq(newInstanceId), any(RequestContext.class));
    doReturn(succeededFuture(newHoldingId)).when(inventoryHoldingManager).createHolding(eq(newInstanceId), eq(location), any(RequestContext.class));
    doReturn(succeededFuture(Collections.emptyList())).when(inventoryItemManager).getItemsByHoldingIdAndOrderLineId(anyString(), anyString(), any(RequestContext.class));
    doReturn(succeededFuture()).when(batchTrackingService).createBatchTrackingRecord(anyString(), anyInt(), any(RequestContext.class));
    // When
    withHoldingOrderLineUpdateInstanceStrategy.processHoldings(holder, requestContext).result();
    // Then
    verify(inventoryInstanceManager, times(1)).createShadowInstanceIfNeeded(
      eq(newInstanceId),
      argThat(ctx -> targetTenantId.equals(ctx.getHeaders().get(XOkapiHeaders.TENANT)))
    );
    verify(inventoryHoldingManager, times(1)).createHolding(
      eq(newInstanceId),
      eq(location),
      argThat(ctx -> targetTenantId.equals(ctx.getHeaders().get(XOkapiHeaders.TENANT)))
    );
  }

  @Test
  @TestMate(name = "TestMate-e4aae478702bb711b8841cf677e35e99")
  void processHoldingsMoveShouldStripUnrecognizedFieldsFromHoldings() {
    // Given
    String orderLineId = UUID.fromString("5097457a-977a-42c9-9430-8456f913d8f1").toString();
    String newInstanceId = UUID.fromString("cd3288a4-898c-4347-a003-2d810ef70f03").toString();
    String holdingId = UUID.fromString("12345678-1234-1234-1234-123456789012").toString();
    Location location = new Location()
      .withHoldingId(holdingId)
      .withQuantity(1)
      .withQuantityPhysical(1);
    PoLine poLine = new PoLine()
      .withId(orderLineId)
      .withLocations(List.of(location));
    ReplaceInstanceRef replaceInstanceRef = new ReplaceInstanceRef()
      .withNewInstanceId(newInstanceId)
      .withHoldingsOperation(ReplaceInstanceRef.HoldingsOperation.MOVE)
      .withDeleteAbandonedHoldings(false);
    PatchOrderLineRequest patchOrderLineRequest = new PatchOrderLineRequest()
      .withOperation(PatchOrderLineRequest.Operation.REPLACE_INSTANCE_REF)
      .withReplaceInstanceRef(replaceInstanceRef);
    OrderLineUpdateInstanceHolder holder = new OrderLineUpdateInstanceHolder()
      .withStoragePoLine(poLine)
      .withPathOrderLineRequest(patchOrderLineRequest);
    JsonObject dirtyHolding = new JsonObject()
      .put(ID, holdingId)
      .put("instanceId", UUID.randomUUID().toString())
      .put("holdingsItems", new JsonArray().add(new JsonObject()))
      .put("bareHoldingsItems", new JsonArray().add(new JsonObject()));
    doReturn(succeededFuture(Collections.emptyList())).when(pieceStorageService).getPiecesByPoLineId(poLine, requestContext);
    doReturn(succeededFuture(List.of(dirtyHolding))).when(inventoryHoldingManager).getHoldingsByIds(eq(List.of(holdingId)), any(RequestContext.class));
    doReturn(succeededFuture()).when(inventoryInstanceManager).createShadowInstanceIfNeeded(eq(newInstanceId), any(RequestContext.class));
    doReturn(succeededFuture()).when(inventoryHoldingManager).updateInstanceForHoldingRecords(any(), eq(newInstanceId), any(RequestContext.class));
    // When
    withHoldingOrderLineUpdateInstanceStrategy.processHoldings(holder, requestContext).result();
    // Then
    @SuppressWarnings("unchecked")
    ArgumentCaptor<List<JsonObject>> holdingCaptor = ArgumentCaptor.forClass(List.class);
    verify(inventoryHoldingManager).updateInstanceForHoldingRecords(holdingCaptor.capture(), eq(newInstanceId), any(RequestContext.class));
    List<JsonObject> capturedHoldings = holdingCaptor.getValue();
    assertThat(capturedHoldings).hasSize(1);

    JsonObject cleanHolding = capturedHoldings.get(0);
    assertThat(cleanHolding.getString(ID)).isEqualTo(holdingId);
    assertThat(cleanHolding.containsKey("holdingsItems")).isFalse();
    assertThat(cleanHolding.containsKey("bareHoldingsItems")).isFalse();
  }

  @Test
  @TestMate(name = "TestMate-7953e0eba0a857901e3c19340404cf02")
  void processHoldingsShouldSkipAlreadyProcessedHoldings() {
    // Given
    String orderLineId = "5097457a-977a-42c9-9430-8456f913d8f1";
    String newInstanceId = "cd3288a4-898c-4347-a003-2d810ef70f03";
    String sharedHoldingId = "12345678-1234-1234-1234-123456789012";
    String newHoldingId = "87654321-4321-4321-4321-210987654321";
    Piece piece = new Piece()
      .withId(UUID.randomUUID().toString())
      .withPoLineId(orderLineId)
      .withHoldingId(sharedHoldingId);
    Location location = new Location()
      .withHoldingId(sharedHoldingId)
      .withQuantity(1)
      .withQuantityPhysical(1);
    PoLine poLine = new PoLine()
      .withId(orderLineId)
      .withLocations(List.of(location));
    ReplaceInstanceRef replaceInstanceRef = new ReplaceInstanceRef()
      .withNewInstanceId(newInstanceId)
      .withHoldingsOperation(ReplaceInstanceRef.HoldingsOperation.CREATE)
      .withDeleteAbandonedHoldings(false);
    PatchOrderLineRequest patchOrderLineRequest = new PatchOrderLineRequest()
      .withOperation(PatchOrderLineRequest.Operation.REPLACE_INSTANCE_REF)
      .withReplaceInstanceRef(replaceInstanceRef);
    OrderLineUpdateInstanceHolder holder = new OrderLineUpdateInstanceHolder()
      .withStoragePoLine(poLine)
      .withPathOrderLineRequest(patchOrderLineRequest);
    // Mocking setup
    // Ensure pieceStorageService returns the piece so that retrieveProcessableLocations finds the holdingId
    doReturn(succeededFuture(List.of(piece))).when(pieceStorageService).getPiecesByPoLineId(any(PoLine.class), any(RequestContext.class));
    doReturn(succeededFuture()).when(inventoryInstanceManager).createShadowInstanceIfNeeded(eq(newInstanceId), any(RequestContext.class));
    // Use any(Location.class) because retrieveProcessableLocations creates a new Location instance from the Piece
    doReturn(succeededFuture(newHoldingId)).when(inventoryHoldingManager).createHolding(eq(newInstanceId), any(Location.class), any(RequestContext.class));
    // Mock item-related calls to ensure the chain completes successfully
    doReturn(succeededFuture(Collections.emptyList())).when(inventoryItemManager).getItemsByHoldingIdAndOrderLineId(anyString(), anyString(), any(RequestContext.class));
    doReturn(succeededFuture()).when(batchTrackingService).createBatchTrackingRecord(anyString(), anyInt(), any(RequestContext.class));
    // Ensure batchUpdatePartialItems returns a succeeded future to avoid NPE on .otherwise() call in the strategy
    doReturn(succeededFuture()).when(inventoryItemManager).batchUpdatePartialItems(any(), any(RequestContext.class));
    // When
    Future<Void> result = withHoldingOrderLineUpdateInstanceStrategy.processHoldings(holder, requestContext);
    // Then
    assertThat(result.succeeded()).isTrue();
    // Verify that createHolding was called exactly once despite the holdingId being referenced in both Piece and PoLine locations
    verify(inventoryHoldingManager, times(1)).createHolding(eq(newInstanceId), any(Location.class), any(RequestContext.class));
    verify(inventoryInstanceManager, times(1)).createShadowInstanceIfNeeded(eq(newInstanceId), any(RequestContext.class));
    // Verify the state of the holder
    List<StorageReplaceOrderLineHoldingRefs> holdingRefs = holder.getStoragePatchOrderLineRequest().getReplaceInstanceRef().getHoldings();
    assertThat(holdingRefs).hasSize(1);
    assertThat(holdingRefs.getFirst().getFromHoldingId()).isEqualTo(sharedHoldingId);
    assertThat(holdingRefs.getFirst().getToHoldingId()).isEqualTo(newHoldingId);
  }

  @Test
  @TestMate(name = "TestMate-db2786eb40a9fb7340af1990aaec6cce")
  void processHoldingsShouldDeleteAbandonedHoldingsInCorrectTenantContext() {
    // Given
    String orderLineId = UUID.fromString("5097457a-977a-42c9-9430-8456f913d8f1").toString();
    String newInstanceId = UUID.fromString("cd3288a4-898c-4347-a003-2d810ef70f03").toString();
    String originalHoldingId = UUID.fromString("12345678-1234-1234-1234-123456789012").toString();
    String newHoldingId = UUID.fromString("87654321-4321-4321-4321-210987654321").toString();
    String targetTenantId = "tenant-x";
    Location location = new Location()
      .withHoldingId(originalHoldingId)
      .withTenantId(targetTenantId)
      .withQuantity(1)
      .withQuantityPhysical(1);
    PoLine poLine = new PoLine()
      .withId(orderLineId)
      .withLocations(List.of(location));
    ReplaceInstanceRef replaceInstanceRef = new ReplaceInstanceRef()
      .withNewInstanceId(newInstanceId)
      .withHoldingsOperation(ReplaceInstanceRef.HoldingsOperation.CREATE)
      .withDeleteAbandonedHoldings(true);
    PatchOrderLineRequest patchOrderLineRequest = new PatchOrderLineRequest()
      .withOperation(PatchOrderLineRequest.Operation.REPLACE_INSTANCE_REF)
      .withReplaceInstanceRef(replaceInstanceRef);
    OrderLineUpdateInstanceHolder holder = new OrderLineUpdateInstanceHolder()
      .withStoragePoLine(poLine)
      .withPathOrderLineRequest(patchOrderLineRequest);
    JsonObject holdingJsonObject = new JsonObject().put("id", originalHoldingId);
    doReturn(succeededFuture(Collections.emptyList())).when(pieceStorageService).getPiecesByPoLineId(any(), any());
    doReturn(succeededFuture()).when(inventoryInstanceManager).createShadowInstanceIfNeeded(eq(newInstanceId), any(RequestContext.class));
    doReturn(succeededFuture(newHoldingId)).when(inventoryHoldingManager).createHolding(eq(newInstanceId), eq(location), any(RequestContext.class));
    doReturn(succeededFuture(Collections.emptyList())).when(inventoryItemManager).getItemsByHoldingIdAndOrderLineId(anyString(), anyString(), any(RequestContext.class));
    doReturn(succeededFuture()).when(batchTrackingService).createBatchTrackingRecord(anyString(), anyInt(), any(RequestContext.class));
    doReturn(succeededFuture()).when(inventoryItemManager).batchUpdatePartialItems(any(), any(RequestContext.class));
    doReturn(succeededFuture(holdingJsonObject)).when(inventoryHoldingManager).getHoldingById(eq(originalHoldingId), eq(true), any(RequestContext.class));
    doReturn(succeededFuture(Collections.emptyList())).when(pieceStorageService).getPiecesByHoldingId(eq(originalHoldingId), any(RequestContext.class));
    doReturn(succeededFuture(Pair.of(true, holdingJsonObject))).when(holdingDeletionService).getHoldingLinkedData(any(), any(), any(), any());
    doReturn(succeededFuture(Pair.of(originalHoldingId, UUID.randomUUID().toString()))).when(holdingDeletionService).deleteHoldingIfPossible(any(), any());
    // When
    Future<Void> result = withHoldingOrderLineUpdateInstanceStrategy.processHoldings(holder, requestContext);
    // Then
    assertThat(result.succeeded()).isTrue();
    verify(inventoryHoldingManager, times(1)).getHoldingById(
      eq(originalHoldingId),
      eq(true),
      argThat(ctx -> targetTenantId.equals(ctx.getHeaders().get(XOkapiHeaders.TENANT)))
    );
    verify(holdingDeletionService, times(1)).getHoldingLinkedData(
      eq(holdingJsonObject),
      any(),
      eq(requestContext),
      argThat(ctx -> targetTenantId.equals(ctx.getHeaders().get(XOkapiHeaders.TENANT)))
    );
    verify(holdingDeletionService, times(1)).deleteHoldingIfPossible(
      argThat(pair -> pair.getKey().equals(true) && pair.getValue().equals(holdingJsonObject)),
      argThat(ctx -> targetTenantId.equals(ctx.getHeaders().get(XOkapiHeaders.TENANT)))
    );
  }

  @Test
  @TestMate(name = "TestMate-bc699760ddbb253d4ad4a996345719ef")
  void processHoldingsShouldUseDefaultTenantWhenLocationTenantIdIsNull() {
    // Given
    String orderLineId = UUID.fromString("5097457a-977a-42c9-9430-8456f913d8f1").toString();
    String newInstanceId = UUID.fromString("cd3288a4-898c-4347-a003-2d810ef70f03").toString();
    String originalHoldingId = UUID.fromString("12345678-1234-1234-1234-123456789012").toString();
    String newHoldingId = UUID.fromString("87654321-4321-4321-4321-210987654321").toString();
    String defaultTenantId = "diku";
    Location location = new Location()
      .withHoldingId(originalHoldingId)
      .withTenantId(null)
      .withQuantity(1)
      .withQuantityPhysical(1);
    PoLine poLine = new PoLine()
      .withId(orderLineId)
      .withLocations(List.of(location));
    ReplaceInstanceRef replaceInstanceRef = new ReplaceInstanceRef()
      .withNewInstanceId(newInstanceId)
      .withHoldingsOperation(ReplaceInstanceRef.HoldingsOperation.CREATE)
      .withDeleteAbandonedHoldings(false);
    PatchOrderLineRequest patchOrderLineRequest = new PatchOrderLineRequest()
      .withOperation(PatchOrderLineRequest.Operation.REPLACE_INSTANCE_REF)
      .withReplaceInstanceRef(replaceInstanceRef);
    OrderLineUpdateInstanceHolder holder = new OrderLineUpdateInstanceHolder()
      .withStoragePoLine(poLine)
      .withPathOrderLineRequest(patchOrderLineRequest);
    when(requestContext.getHeaders()).thenReturn(Map.of(XOkapiHeaders.TENANT, defaultTenantId));
    doReturn(succeededFuture(Collections.emptyList())).when(pieceStorageService).getPiecesByPoLineId(any(), any());
    doReturn(succeededFuture()).when(inventoryInstanceManager).createShadowInstanceIfNeeded(eq(newInstanceId), any(RequestContext.class));
    doReturn(succeededFuture(newHoldingId)).when(inventoryHoldingManager).createHolding(eq(newInstanceId), eq(location), any(RequestContext.class));
    doReturn(succeededFuture(Collections.emptyList())).when(inventoryItemManager).getItemsByHoldingIdAndOrderLineId(anyString(), anyString(), any(RequestContext.class));
    doReturn(succeededFuture()).when(batchTrackingService).createBatchTrackingRecord(anyString(), anyInt(), any(RequestContext.class));
    // Stubbing batchUpdatePartialItems to prevent NullPointerException when calling .otherwise() in the strategy
    doReturn(succeededFuture()).when(inventoryItemManager).batchUpdatePartialItems(any(), any(RequestContext.class));
    // When
    Future<Void> result = withHoldingOrderLineUpdateInstanceStrategy.processHoldings(holder, requestContext);
    // Then
    assertThat(result.succeeded()).isTrue();
    verify(inventoryInstanceManager, times(1)).createShadowInstanceIfNeeded(
      eq(newInstanceId),
      argThat(ctx -> defaultTenantId.equals(ctx.getHeaders().get(XOkapiHeaders.TENANT)))
    );
    verify(inventoryHoldingManager, times(1)).createHolding(
      eq(newInstanceId),
      eq(location),
      argThat(ctx -> defaultTenantId.equals(ctx.getHeaders().get(XOkapiHeaders.TENANT)))
    );
    List<StorageReplaceOrderLineHoldingRefs> holdingRefs = holder.getStoragePatchOrderLineRequest().getReplaceInstanceRef().getHoldings();
    assertThat(holdingRefs).hasSize(1);
    assertThat(holdingRefs.getFirst().getFromHoldingId()).isEqualTo(originalHoldingId);
    assertThat(holdingRefs.getFirst().getToHoldingId()).isEqualTo(newHoldingId);
  }
}
