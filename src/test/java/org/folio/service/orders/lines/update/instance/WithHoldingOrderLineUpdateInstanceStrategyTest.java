package org.folio.service.orders.lines.update.instance;

import static java.util.concurrent.CompletableFuture.completedFuture;
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

import java.io.IOException;
import java.util.ArrayList;
import java.util.List;
import java.util.UUID;
import java.util.concurrent.CompletableFuture;
import java.util.concurrent.CompletionException;

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
import org.junit.jupiter.api.AfterEach;
import org.junit.jupiter.api.Assertions;
import org.junit.jupiter.api.BeforeEach;
import org.junit.jupiter.api.Test;
import org.mockito.InjectMocks;
import org.mockito.Mock;
import org.mockito.Mockito;
import org.mockito.MockitoAnnotations;

import io.vertx.core.json.JsonObject;

public class WithHoldingOrderLineUpdateInstanceStrategyTest {
  @InjectMocks
  private WithHoldingOrderLineUpdateInstanceStrategy withHoldingOrderLineUpdateInstanceStrategy;
  @Mock
  private InventoryManager inventoryManager;

  @Mock
  RequestContext requestContext;

  @BeforeEach
  void initMocks(){
    MockitoAnnotations.openMocks(this);
  }

  @AfterEach
  void resetMocks() {
    clearServiceInteractions();
    Mockito.reset(inventoryManager);
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

    doReturn(completedFuture(holdings)).when(inventoryManager).getHoldingsByIds(eq(holdingIds), eq(requestContext));
    doReturn(completedFuture(null)).when(inventoryManager).updateInstanceForHoldingRecords(eq(holdings), eq(instanceId), eq(requestContext));

    withHoldingOrderLineUpdateInstanceStrategy.updateInstance(orderLineUpdateInstanceHolder, requestContext).join();

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

    withHoldingOrderLineUpdateInstanceStrategy.updateInstance(orderLineUpdateInstanceHolder, requestContext).join();

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


    doReturn(completedFuture(UUID.randomUUID().toString())).when(inventoryManager)
        .getOrCreateHoldingRecordByInstanceAndLocation(eq(instanceId), eq(locations.get(0)), eq(requestContext));
    doReturn(completedFuture(UUID.randomUUID().toString())).when(inventoryManager)
        .getOrCreateHoldingRecordByInstanceAndLocation(eq(instanceId), eq(locations.get(1)), eq(requestContext));
    doReturn(completedFuture(List.of(items.get(0)))).when(inventoryManager).getItemsByHoldingIdAndOrderLineId(eq(holdingIds.get(0)), eq(orderLineId), eq(requestContext));
    doReturn(completedFuture(List.of(items.get(1)))).when(inventoryManager).getItemsByHoldingIdAndOrderLineId(eq(holdingIds.get(1)), eq(orderLineId), eq(requestContext));
    doReturn(completedFuture(null)).when(inventoryManager).updateItem(any(JsonObject.class), eq(requestContext));

    withHoldingOrderLineUpdateInstanceStrategy.updateInstance(orderLineUpdateInstanceHolder, requestContext).join();

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

    doReturn(completedFuture(UUID.randomUUID().toString())).when(inventoryManager)
      .getOrCreateHoldingRecordByInstanceAndLocation(eq(instanceId), eq(locations.get(0)), eq(requestContext));
    doReturn(completedFuture(List.of(item))).when(inventoryManager).getItemsByHoldingIdAndOrderLineId(eq(holdingId), eq(orderLineId), eq(requestContext));
    doReturn(completedFuture(null)).when(inventoryManager).updateItem(eq(item), eq(requestContext));

    withHoldingOrderLineUpdateInstanceStrategy.updateInstance(orderLineUpdateInstanceHolder, requestContext).join();

    verify(inventoryManager, times(1)).getOrCreateHoldingRecordByInstanceAndLocation(instanceId, locations.get(0), requestContext);
    verify(inventoryManager, times(1)).getItemsByHoldingIdAndOrderLineId(holdingId, orderLineId, requestContext);
    verify(inventoryManager, times(1)).updateItem(item, requestContext);
  }

  @Test
  public void updateInstanceForFindOrCreateHoldingOperationAndDeleteAbandonedHoldings() throws IOException {
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


    doReturn(completedFuture(UUID.randomUUID().toString())).when(inventoryManager)
        .getOrCreateHoldingRecordByInstanceAndLocation(eq(instanceId), eq(locations.get(0)), eq(requestContext));
    doReturn(completedFuture(UUID.randomUUID().toString())).when(inventoryManager)
        .getOrCreateHoldingRecordByInstanceAndLocation(eq(instanceId), eq(locations.get(1)), eq(requestContext));
    doReturn(completedFuture(new ArrayList())).when(inventoryManager).getItemsByHoldingId(eq(holdingIds.get(0)), eq(requestContext));
    doReturn(completedFuture(new ArrayList())).when(inventoryManager).getItemsByHoldingId(eq(holdingIds.get(1)), eq(requestContext));
    doReturn(completedFuture(null)).when(inventoryManager).deleteHoldingById(eq(holdingIds.get(0)), eq(true), eq(requestContext));
    doReturn(completedFuture(null)).when(inventoryManager).deleteHoldingById(eq(holdingIds.get(0)), eq(true), eq(requestContext));
    doReturn(completedFuture(List.of(items.get(0)))).when(inventoryManager).getItemsByHoldingIdAndOrderLineId(eq(holdingIds.get(0)), eq(orderLineId), eq(requestContext));
    doReturn(completedFuture(List.of(items.get(1)))).when(inventoryManager).getItemsByHoldingIdAndOrderLineId(eq(holdingIds.get(1)), eq(orderLineId), eq(requestContext));
    doReturn(completedFuture(null)).when(inventoryManager).updateItem(any(JsonObject.class), eq(requestContext));

    withHoldingOrderLineUpdateInstanceStrategy.updateInstance(orderLineUpdateInstanceHolder, requestContext).join();

    verify(inventoryManager, times(1)).getOrCreateHoldingRecordByInstanceAndLocation(instanceId, locations.get(0), requestContext);
    verify(inventoryManager, times(1)).getOrCreateHoldingRecordByInstanceAndLocation(instanceId, locations.get(1), requestContext);
    verify(inventoryManager, times(1)).deleteHoldingById(eq(holdingIds.get(0)), eq(true), eq(requestContext));
    verify(inventoryManager, times(1)).deleteHoldingById(eq(holdingIds.get(1)), eq(true), eq(requestContext));
    verify(inventoryManager, times(2)).getItemsByHoldingIdAndOrderLineId(anyString(), anyString(), any(RequestContext.class));
    verify(inventoryManager, times(2)).updateItem(any(JsonObject.class), any(RequestContext.class));
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

    doReturn(completedFuture(UUID.randomUUID().toString())).when(inventoryManager)
        .createHolding(eq(instanceId), eq(locations.get(0)), eq(requestContext));
    doReturn(completedFuture(UUID.randomUUID().toString())).when(inventoryManager)
        .createHolding(eq(instanceId), eq(locations.get(1)), eq(requestContext));
    doReturn(completedFuture(List.of(items.get(0)))).when(inventoryManager).getItemsByHoldingIdAndOrderLineId(eq(holdingIds.get(0)), eq(orderLineId), eq(requestContext));
    doReturn(completedFuture(List.of(items.get(1)))).when(inventoryManager).getItemsByHoldingIdAndOrderLineId(eq(holdingIds.get(1)), eq(orderLineId), eq(requestContext));
    doReturn(completedFuture(null)).when(inventoryManager).updateItem(any(JsonObject.class), eq(requestContext));

    withHoldingOrderLineUpdateInstanceStrategy.updateInstance(orderLineUpdateInstanceHolder, requestContext).join();

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

    doReturn(completedFuture(UUID.randomUUID().toString())).when(inventoryManager)
        .createHolding(eq(instanceId), eq(locations.get(0)), eq(requestContext));
    doReturn(completedFuture(List.of(item))).when(inventoryManager).getItemsByHoldingIdAndOrderLineId(eq(holdingId), eq(orderLineId), eq(requestContext));
    doReturn(completedFuture(null)).when(inventoryManager).updateItem(eq(item), eq(requestContext));

    withHoldingOrderLineUpdateInstanceStrategy.updateInstance(orderLineUpdateInstanceHolder, requestContext).join();

    verify(inventoryManager, times(1)).createHolding(instanceId, locations.get(0), requestContext);
    verify(inventoryManager, times(1)).getItemsByHoldingIdAndOrderLineId(holdingId, orderLineId, requestContext);
    verify(inventoryManager, times(1)).updateItem(item, requestContext);
  }

  @Test
  public void updateInstanceAndItemsWithItemsNotUpdatesCorrect() throws IOException {
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

    doReturn(completedFuture(UUID.randomUUID().toString())).when(inventoryManager)
        .createHolding(eq(instanceId), eq(locations.get(0)), eq(requestContext));
    doReturn(completedFuture(List.of(item))).when(inventoryManager).getItemsByHoldingIdAndOrderLineId(eq(holdingId), eq(orderLineId), eq(requestContext));
    doReturn(CompletableFuture.failedFuture(new HttpException(500, ErrorCodes.GENERIC_ERROR_CODE))).when(inventoryManager).updateItem(eq(item), eq(requestContext));


    CompletionException thrown = assertThrows(
        CompletionException.class,
        () -> withHoldingOrderLineUpdateInstanceStrategy.updateInstance(orderLineUpdateInstanceHolder, requestContext).join(),      "Expected exception"
    );
    HttpException actHttpException = (HttpException) thrown.getCause();

    Error actError = actHttpException.getErrors().getErrors().get(0);
    Parameter actParameter = actError.getParameters().get(0);

    Assertions.assertEquals(ITEM_UPDATE_FAILED.getCode(), actError.getCode());
    Assertions.assertEquals(ITEM_UPDATE_FAILED.getDescription(), actError.getMessage());
    Assertions.assertEquals("itemId", actParameter.getKey());
    Assertions.assertEquals(itemId, actParameter.getValue());

    verify(inventoryManager, times(1)).createHolding(instanceId, locations.get(0), requestContext);
    verify(inventoryManager, times(1)).getItemsByHoldingIdAndOrderLineId(holdingId, orderLineId, requestContext);
    verify(inventoryManager, times(1)).updateItem(item, requestContext);
  }

}
