package org.folio.service.orders.lines.update.instance;

import static io.vertx.core.Future.succeededFuture;
import static java.util.stream.Collectors.toList;
import static org.folio.TestConfig.clearServiceInteractions;
import static org.folio.TestUtils.getMockData;
import static org.folio.rest.core.exceptions.ErrorCodes.ITEM_UPDATE_FAILED;
import static org.folio.rest.impl.MockServer.HOLDINGS_OLD_NEW_PATH;
import static org.folio.service.inventory.InventoryManager.ID;
import static org.folio.service.inventory.InventoryManager.ITEM_HOLDINGS_RECORD_ID;
import static org.folio.service.inventory.InventoryManager.ITEM_STATUS;
import static org.folio.service.inventory.InventoryManager.ITEM_STATUS_NAME;
import static org.junit.jupiter.api.Assertions.assertThrows;
import static org.mockito.ArgumentMatchers.eq;
import static org.mockito.ArgumentMatchers.any;
import static org.mockito.ArgumentMatchers.anyString;
import static org.mockito.Mockito.doReturn;
import static org.mockito.Mockito.times;
import static org.mockito.Mockito.verify;
import static org.mockito.Mockito.never;

import java.io.IOException;
import java.util.ArrayList;
import java.util.List;
import java.util.UUID;
import java.util.concurrent.CompletableFuture;
import java.util.concurrent.CompletionException;

import com.google.common.collect.Lists;
import io.vertx.core.Future;
import io.vertx.junit5.VertxExtension;
import io.vertx.junit5.VertxTestContext;
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
import org.folio.rest.jaxrs.model.PoLine;
import org.folio.rest.jaxrs.model.ReplaceInstanceRef;
import org.folio.service.inventory.InventoryManager;
import org.folio.service.pieces.PieceStorageService;
import org.junit.jupiter.api.AfterEach;
import org.junit.jupiter.api.Assertions;
import org.junit.jupiter.api.BeforeEach;
import org.junit.jupiter.api.Test;
import org.junit.jupiter.api.extension.ExtendWith;
import org.mockito.InjectMocks;
import org.mockito.Mock;
import org.mockito.Mockito;
import org.mockito.MockitoAnnotations;

import io.vertx.core.json.JsonObject;


@ExtendWith(VertxExtension.class)
public class WithHoldingOrderLineUpdateInstanceStrategyTest {
  @InjectMocks
  private WithHoldingOrderLineUpdateInstanceStrategy withHoldingOrderLineUpdateInstanceStrategy;
  @Mock
  private InventoryManager inventoryManager;
  @Mock
  private PieceStorageService pieceStorageService;

  @Mock
  RequestContext requestContext;

  @BeforeEach
  void initMocks(){
    MockitoAnnotations.openMocks(this);
    doReturn(succeededFuture(Lists.newArrayList())).when(pieceStorageService).getPiecesByPoLineId(any(), any());
  }

  @AfterEach
  void resetMocks() {
    clearServiceInteractions();
    Mockito.reset(inventoryManager);
    Mockito.reset(pieceStorageService);
  }

  @Test
  public void updateInstanceForMoveHoldingOperation() throws IOException {
    String orderLineId = UUID.randomUUID().toString();
    String instanceId = UUID.randomUUID().toString();

    JsonObject holdingsCollection = new JsonObject(getMockData(HOLDINGS_OLD_NEW_PATH));

    List<JsonObject> holdings = holdingsCollection.getJsonArray("holdingsRecords").stream()
        .map(o -> ((JsonObject) o))
        .collect(toList());

    List<String> holdingIds = holdings.stream().map(holding ->  holding.getString(ID)).collect(toList());

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

    doReturn(succeededFuture(holdings)).when(inventoryManager).getHoldingsByIds(eq(holdingIds), eq(requestContext));
    doReturn(succeededFuture(null)).when(inventoryManager).updateInstanceForHoldingRecords(eq(holdings), eq(instanceId), eq(requestContext));

    withHoldingOrderLineUpdateInstanceStrategy.updateInstance(orderLineUpdateInstanceHolder, requestContext).result();

    verify(inventoryManager, times(1)).getHoldingsByIds(holdingIds, requestContext);
    verify(inventoryManager, times(1)).updateInstanceForHoldingRecords(holdings, instanceId, requestContext);
  }

  @Test
  public void updateInstanceWithNullReplaceInstanceRef() throws IOException {
    String orderLineId = UUID.randomUUID().toString();
    String instanceId = UUID.randomUUID().toString();

    JsonObject holdingsCollection = new JsonObject(getMockData(HOLDINGS_OLD_NEW_PATH));

    List<JsonObject> holdings = holdingsCollection.getJsonArray("holdingsRecords").stream()
      .map(o -> ((JsonObject) o))
      .collect(toList());

    List<String> holdingIds = holdings.stream().map(holding ->  holding.getString(ID)).collect(toList());

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

    verify(inventoryManager, times(0)).getHoldingsByIds(holdingIds, requestContext);
    verify(inventoryManager, times(0)).updateInstanceForHoldingRecords(holdings, instanceId, requestContext);
  }

  @Test
  public void updateInstanceForFindOrCreateHoldingOperation() throws IOException {
    String orderLineId = UUID.randomUUID().toString();
    String instanceId = UUID.randomUUID().toString();

    JsonObject holdingsCollection = new JsonObject(getMockData(HOLDINGS_OLD_NEW_PATH));

    List<JsonObject> holdings = holdingsCollection.getJsonArray("holdingsRecords").stream()
        .map(o -> ((JsonObject) o))
        .collect(toList());

    List<String> holdingIds = holdings.stream().map(holding ->  holding.getString(ID)).collect(toList());

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
    }).collect(toList());

    doReturn(succeededFuture(UUID.randomUUID().toString())).when(inventoryManager)
        .getOrCreateHoldingRecordByInstanceAndLocation(eq(instanceId), eq(locations.get(0)), eq(requestContext));
    doReturn(succeededFuture(UUID.randomUUID().toString())).when(inventoryManager)
        .getOrCreateHoldingRecordByInstanceAndLocation(eq(instanceId), eq(locations.get(1)), eq(requestContext));
    doReturn(succeededFuture(List.of(items.get(0)))).when(inventoryManager).getItemsByHoldingIdAndOrderLineId(eq(holdingIds.get(0)), eq(orderLineId), eq(requestContext));
    doReturn(succeededFuture(List.of(items.get(1)))).when(inventoryManager).getItemsByHoldingIdAndOrderLineId(eq(holdingIds.get(1)), eq(orderLineId), eq(requestContext));
    doReturn(succeededFuture(null)).when(inventoryManager).updateItem(any(JsonObject.class), eq(requestContext));

    withHoldingOrderLineUpdateInstanceStrategy.updateInstance(orderLineUpdateInstanceHolder, requestContext).result();

    verify(inventoryManager, times(1)).getOrCreateHoldingRecordByInstanceAndLocation(instanceId, locations.get(0), requestContext);
    verify(inventoryManager, times(1)).getOrCreateHoldingRecordByInstanceAndLocation(instanceId, locations.get(1), requestContext);
    verify(inventoryManager, times(2)).getItemsByHoldingIdAndOrderLineId(anyString(), anyString(), any(RequestContext.class));
    verify(inventoryManager, times(2)).updateItem(any(JsonObject.class), any(RequestContext.class));
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

    doReturn(succeededFuture(UUID.randomUUID().toString())).when(inventoryManager)
      .getOrCreateHoldingRecordByInstanceAndLocation(eq(instanceId), eq(locations.get(0)), eq(requestContext));
    doReturn(succeededFuture(List.of(item))).when(inventoryManager).getItemsByHoldingIdAndOrderLineId(eq(holdingId), eq(orderLineId), eq(requestContext));
    doReturn(succeededFuture(null)).when(inventoryManager).updateItem(eq(item), eq(requestContext));

    withHoldingOrderLineUpdateInstanceStrategy.updateInstance(orderLineUpdateInstanceHolder, requestContext).result();

    verify(inventoryManager, times(1)).getOrCreateHoldingRecordByInstanceAndLocation(instanceId, locations.get(0), requestContext);
    verify(inventoryManager, times(1)).getItemsByHoldingIdAndOrderLineId(holdingId, orderLineId, requestContext);
    verify(inventoryManager, times(1)).updateItem(item, requestContext);
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

    doReturn(succeededFuture(holdingId)).when(inventoryManager)
      .getOrCreateHoldingRecordByInstanceAndLocation(eq(instanceId), eq(locations.get(0)), eq(requestContext));

    withHoldingOrderLineUpdateInstanceStrategy.updateInstance(orderLineUpdateInstanceHolder, requestContext).result();

    verify(inventoryManager, times(1)).getOrCreateHoldingRecordByInstanceAndLocation(instanceId, locations.get(0), requestContext);
    verify(inventoryManager, never()).getItemsByHoldingIdAndOrderLineId(anyString(), anyString(), any(RequestContext.class));
    verify(inventoryManager, never()).updateItem(any(JsonObject.class), any(RequestContext.class));
  }

  @Test
  public void updateInstanceForFindOrCreateHoldingOperationAndDeleteAbandonedHoldings(VertxTestContext vertxTestContext) throws IOException {
    String instanceId = UUID.randomUUID().toString();

    JsonObject holdingsCollection = new JsonObject(getMockData(HOLDINGS_OLD_NEW_PATH));

    List<JsonObject> holdings = holdingsCollection.getJsonArray("holdingsRecords").stream()
        .map(o -> ((JsonObject) o))
        .collect(toList());

    List<String> holdingIds = holdings.stream().map(holding ->  holding.getString(ID)).collect(toList());
    String orderLineId = holdings.get(0).getString("purchaseOrderLineIdentifier");

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
            .withDeleteAbandonedHoldings(true));

    OrderLineUpdateInstanceHolder orderLineUpdateInstanceHolder = new OrderLineUpdateInstanceHolder()
        .withStoragePoLine(poLine)
        .withPathOrderLineRequest(patchOrderLineRequest);

    List<JsonObject> items = holdingIds.stream().map(holdingId -> {
      JsonObject item = new JsonObject().put(TestConstants.ID, UUID.randomUUID().toString());
      item.put(ITEM_STATUS, new JsonObject().put(ITEM_STATUS_NAME, ItemStatus.ON_ORDER.value()));
      item.put(ITEM_HOLDINGS_RECORD_ID, holdingId);
      return item;
    }).collect(toList());


    doReturn(succeededFuture(UUID.randomUUID().toString())).when(inventoryManager)
        .getOrCreateHoldingRecordByInstanceAndLocation(eq(instanceId), eq(locations.get(0)), eq(requestContext));
    doReturn(succeededFuture(UUID.randomUUID().toString())).when(inventoryManager)
        .getOrCreateHoldingRecordByInstanceAndLocation(eq(instanceId), eq(locations.get(1)), eq(requestContext));
    doReturn(succeededFuture(new ArrayList<JsonObject>())).when(inventoryManager).getItemsByHoldingId(eq(holdingIds.get(0)), eq(requestContext));
    doReturn(succeededFuture(new ArrayList<JsonObject>())).when(inventoryManager).getItemsByHoldingId(eq(holdingIds.get(1)), eq(requestContext));
    doReturn(succeededFuture()).when(inventoryManager).deleteHoldingById(eq(holdingIds.get(0)), eq(true), eq(requestContext));
    doReturn(succeededFuture()).when(inventoryManager).deleteHoldingById(eq(holdingIds.get(0)), eq(true), eq(requestContext));
    doReturn(succeededFuture(List.of(items.get(0)))).when(inventoryManager).getItemsByHoldingIdAndOrderLineId(eq(holdingIds.get(0)), eq(orderLineId), eq(requestContext));
    doReturn(succeededFuture(List.of(items.get(1)))).when(inventoryManager).getItemsByHoldingIdAndOrderLineId(eq(holdingIds.get(1)), eq(orderLineId), eq(requestContext));
    doReturn(succeededFuture()).when(inventoryManager).updateItem(any(JsonObject.class), eq(requestContext));

    var future = withHoldingOrderLineUpdateInstanceStrategy.updateInstance(orderLineUpdateInstanceHolder, requestContext);

    vertxTestContext.assertComplete(future).onComplete(res-> {
      verify(inventoryManager, times(1)).getOrCreateHoldingRecordByInstanceAndLocation(instanceId, locations.get(0), requestContext);
      verify(inventoryManager, times(1)).getOrCreateHoldingRecordByInstanceAndLocation(instanceId, locations.get(1), requestContext);
      verify(inventoryManager, times(1)).deleteHoldingById(eq(holdingIds.get(0)), eq(true), eq(requestContext));
      verify(inventoryManager, times(1)).deleteHoldingById(eq(holdingIds.get(1)), eq(true), eq(requestContext));
      verify(inventoryManager, times(2)).getItemsByHoldingIdAndOrderLineId(anyString(), anyString(), any(RequestContext.class));
      verify(inventoryManager, times(2)).updateItem(any(JsonObject.class), any(RequestContext.class));
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
        .collect(toList());

    List<String> holdingIds = holdings.stream().map(holding ->  holding.getString(ID)).collect(toList());

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
    }).collect(toList());

    PatchOrderLineRequest patchOrderLineRequest = new PatchOrderLineRequest();
    patchOrderLineRequest.withOperation(PatchOrderLineRequest.Operation.REPLACE_INSTANCE_REF)
        .withReplaceInstanceRef(new ReplaceInstanceRef()
            .withNewInstanceId(instanceId)
            .withHoldingsOperation(ReplaceInstanceRef.HoldingsOperation.CREATE)
            .withDeleteAbandonedHoldings(false));

    OrderLineUpdateInstanceHolder orderLineUpdateInstanceHolder = new OrderLineUpdateInstanceHolder()
        .withStoragePoLine(poLine).withPathOrderLineRequest(patchOrderLineRequest);

    doReturn(succeededFuture(UUID.randomUUID().toString())).when(inventoryManager)
        .createHolding(eq(instanceId), eq(locations.get(0)), eq(requestContext));
    doReturn(succeededFuture(UUID.randomUUID().toString())).when(inventoryManager)
        .createHolding(eq(instanceId), eq(locations.get(1)), eq(requestContext));
    doReturn(succeededFuture(List.of(items.get(0)))).when(inventoryManager).getItemsByHoldingIdAndOrderLineId(eq(holdingIds.get(0)), eq(orderLineId), eq(requestContext));
    doReturn(succeededFuture(List.of(items.get(1)))).when(inventoryManager).getItemsByHoldingIdAndOrderLineId(eq(holdingIds.get(1)), eq(orderLineId), eq(requestContext));
    doReturn(succeededFuture(null)).when(inventoryManager).updateItem(any(JsonObject.class), eq(requestContext));

    withHoldingOrderLineUpdateInstanceStrategy.updateInstance(orderLineUpdateInstanceHolder, requestContext).result();

    verify(inventoryManager, times(1)).createHolding(instanceId, locations.get(0), requestContext);
    verify(inventoryManager, times(1)).createHolding(instanceId, locations.get(1), requestContext);
    verify(inventoryManager, times(2)).getItemsByHoldingIdAndOrderLineId(anyString(), anyString(), any(RequestContext.class));
    verify(inventoryManager, times(2)).updateItem(any(JsonObject.class), any(RequestContext.class));
  }

  @Test
  public void updateInstanceAndItemsForCreateHoldingOperation() throws IOException {
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

    doReturn(succeededFuture(UUID.randomUUID().toString())).when(inventoryManager)
        .createHolding(eq(instanceId), eq(locations.get(0)), eq(requestContext));
    doReturn(succeededFuture(List.of(item))).when(inventoryManager).getItemsByHoldingIdAndOrderLineId(eq(holdingId), eq(orderLineId), eq(requestContext));
    doReturn(succeededFuture(null)).when(inventoryManager).updateItem(eq(item), eq(requestContext));

    withHoldingOrderLineUpdateInstanceStrategy.updateInstance(orderLineUpdateInstanceHolder, requestContext).result();

    verify(inventoryManager, times(1)).createHolding(instanceId, locations.get(0), requestContext);
    verify(inventoryManager, times(1)).getItemsByHoldingIdAndOrderLineId(holdingId, orderLineId, requestContext);
    verify(inventoryManager, times(1)).updateItem(item, requestContext);
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

    doReturn(succeededFuture(UUID.randomUUID().toString())).when(inventoryManager)
        .createHolding(eq(instanceId), eq(locations.get(0)), eq(requestContext));
    doReturn(succeededFuture(List.of(item))).when(inventoryManager).getItemsByHoldingIdAndOrderLineId(eq(holdingId), eq(orderLineId), eq(requestContext));
    doReturn(Future.failedFuture(new HttpException(500, ErrorCodes.GENERIC_ERROR_CODE))).when(inventoryManager).updateItem(eq(item), eq(requestContext));

    vertxTestContext.assertFailure(withHoldingOrderLineUpdateInstanceStrategy.updateInstance(orderLineUpdateInstanceHolder, requestContext))
      .onComplete(res-> {
        HttpException actHttpException = (HttpException) res.cause();

        Error actError = actHttpException.getErrors().getErrors().get(0);
        Parameter actParameter = actError.getParameters().get(0);

        Assertions.assertEquals(ITEM_UPDATE_FAILED.getCode(), actError.getCode());
        Assertions.assertEquals(ITEM_UPDATE_FAILED.getDescription(), actError.getMessage());
        Assertions.assertEquals("itemId", actParameter.getKey());
        Assertions.assertEquals(itemId, actParameter.getValue());

        verify(inventoryManager, times(1)).createHolding(instanceId, locations.get(0), requestContext);
        verify(inventoryManager, times(1)).getItemsByHoldingIdAndOrderLineId(holdingId, orderLineId, requestContext);
        verify(inventoryManager, times(1)).updateItem(item, requestContext);
        vertxTestContext.completeNow();
      });
  }

}
