package org.folio.service.inventory;

import io.vertx.core.Future;
import io.vertx.core.json.JsonObject;
import org.folio.models.ItemStatus;
import org.folio.rest.core.models.RequestContext;
import org.folio.rest.jaxrs.model.Location;
import org.folio.rest.jaxrs.model.PoLine;
import org.junit.jupiter.api.BeforeEach;
import org.junit.jupiter.api.Test;
import org.mockito.InjectMocks;
import org.mockito.Mock;
import org.mockito.MockitoAnnotations;

import java.util.ArrayList;
import java.util.Collections;
import java.util.List;
import java.util.UUID;

import static org.junit.jupiter.api.Assertions.assertEquals;
import static org.mockito.ArgumentMatchers.any;
import static org.mockito.ArgumentMatchers.eq;
import static org.mockito.Mockito.*;

public class InventoryItemStatusSyncServiceTest {
  private static final String PO_LINE_ID = UUID.randomUUID().toString();
  @Mock
  private InventoryItemManager inventoryItemManager;
  @Mock
  private RequestContext requestContext;

  @InjectMocks
  private InventoryItemStatusSyncService itemStatusSyncService;

  @BeforeEach
  public void init() {
    MockitoAnnotations.openMocks(this);
  }

  @Test
  void testShouldNotUpdateItemStatusWhenOnOrderItemsNotFound() {
    //given
    when(inventoryItemManager.getItemsByPoLineIdsAndStatus(eq(List.of(PO_LINE_ID)), eq(ItemStatus.ON_ORDER.value()), any(RequestContext.class)))
      .thenReturn(Future.succeededFuture(List.of()));
    List<Location> locations = getLocations("tenant1");
    //when
    itemStatusSyncService.updateInventoryItemStatus(PO_LINE_ID, locations, ItemStatus.ORDER_CLOSED, requestContext);
    //then
    verify(inventoryItemManager, never()).updateItem(any(JsonObject.class), any(RequestContext.class));
  }

  @Test
  void testUpdateItemStatus() {
    //given
    JsonObject item = new JsonObject()
      .put(InventoryItemManager.ITEM_PURCHASE_ORDER_LINE_IDENTIFIER, PO_LINE_ID);
    when(inventoryItemManager.getItemsByPoLineIdsAndStatus(eq(List.of(PO_LINE_ID)), eq(ItemStatus.ON_ORDER.value()), any(RequestContext.class)))
      .thenReturn(Future.succeededFuture(List.of(item)));
    when(inventoryItemManager.updateItem(eq(item), any(RequestContext.class))).thenReturn(Future.succeededFuture());
    List<Location> locations = getLocations("tenant1");
    //when
    itemStatusSyncService.updateInventoryItemStatus(PO_LINE_ID, locations, ItemStatus.ORDER_CLOSED, requestContext);
    //then
    verify(inventoryItemManager, times(1)).updateItem(eq(item), any(RequestContext.class));
    assertEquals(ItemStatus.ORDER_CLOSED, ItemStatus.fromValue(item.getJsonObject("status").getString("name")));
  }

  @Test
  void testUpdateMultipleItemStatusesBasedOnLocations() {
    //given
    JsonObject item = new JsonObject()
      .put(InventoryItemManager.ITEM_PURCHASE_ORDER_LINE_IDENTIFIER, PO_LINE_ID);
    when(inventoryItemManager.getItemsByPoLineIdsAndStatus(eq(List.of(PO_LINE_ID)), eq(ItemStatus.ON_ORDER.value()), any(RequestContext.class)))
      .thenReturn(Future.succeededFuture(List.of(item)));
    when(inventoryItemManager.updateItem(eq(item), any(RequestContext.class))).thenReturn(Future.succeededFuture());
    List<Location> locations = getLocations("tenant1", "tenant2");
    //when
    itemStatusSyncService.updateInventoryItemStatus(PO_LINE_ID, locations, ItemStatus.ORDER_CLOSED, requestContext);
    //then
    verify(inventoryItemManager, times(2)).updateItem(eq(item), any(RequestContext.class));
    assertEquals(ItemStatus.ORDER_CLOSED, ItemStatus.fromValue(item.getJsonObject("status").getString("name")));
  }

  @Test
  void testUpdateItemStatusesWhenPoLinesEmpty() {
    //when
    itemStatusSyncService.updateItemStatusesInInventory(Collections.emptyList(), ItemStatus.ON_ORDER, ItemStatus.ORDER_CLOSED, requestContext);
    //then
    verify(inventoryItemManager, never()).updateItem(any(JsonObject.class), any(RequestContext.class));
  }

  @Test
  void testUpdateItemStatusesWhenItemsNotExist() {
    //given
    List<PoLine> poLines = List.of(new PoLine().withId(PO_LINE_ID).withLocations(List.of(new Location())));
    when(inventoryItemManager.getItemsByPoLineIdsAndStatus(eq(List.of(PO_LINE_ID)), eq(ItemStatus.ON_ORDER.value()), any(RequestContext.class)))
      .thenReturn(Future.succeededFuture(List.of()));
    //when
    itemStatusSyncService.updateItemStatusesInInventory(poLines, ItemStatus.ON_ORDER, ItemStatus.ORDER_CLOSED, requestContext);
    //then
    verify(inventoryItemManager, never()).updateItem(any(JsonObject.class), any(RequestContext.class));
  }

  @Test
  void testUpdateItemStatusesWhenLocationDoesNotHaveTenantId() {
    //given
    JsonObject item = new JsonObject()
      .put(InventoryItemManager.ITEM_PURCHASE_ORDER_LINE_IDENTIFIER, PO_LINE_ID);
    List<PoLine> poLines = List.of(new PoLine().withId(PO_LINE_ID).withLocations(List.of(new Location())));
    when(inventoryItemManager.getItemsByPoLineIdsAndStatus(eq(List.of(PO_LINE_ID)), eq(ItemStatus.ON_ORDER.value()), any(RequestContext.class)))
      .thenReturn(Future.succeededFuture(List.of(item)));
    when(inventoryItemManager.updateItem(eq(item), any(RequestContext.class))).thenReturn(Future.succeededFuture());
    //when
    itemStatusSyncService.updateItemStatusesInInventory(poLines, ItemStatus.ON_ORDER, ItemStatus.ORDER_CLOSED, requestContext);
    //then
    verify(inventoryItemManager, times(1)).updateItem(any(JsonObject.class), any(RequestContext.class));
    assertEquals(ItemStatus.ORDER_CLOSED, ItemStatus.fromValue(item.getJsonObject("status").getString("name")));
  }

  @Test
  void testUpdateItemStatusesInDifferentTenants() {
    //given
    JsonObject item = new JsonObject()
      .put(InventoryItemManager.ITEM_PURCHASE_ORDER_LINE_IDENTIFIER, PO_LINE_ID);
    List<PoLine> poLines = List.of(new PoLine()
      .withId(PO_LINE_ID)
      .withLocations(getLocations("tenant1", "tenant2")));
    when(inventoryItemManager.getItemsByPoLineIdsAndStatus(eq(List.of(PO_LINE_ID)), eq(ItemStatus.ON_ORDER.value()), any(RequestContext.class)))
      .thenReturn(Future.succeededFuture(List.of(item)));
    when(inventoryItemManager.updateItem(eq(item), any(RequestContext.class))).thenReturn(Future.succeededFuture());
    //when
    itemStatusSyncService.updateItemStatusesInInventory(poLines, ItemStatus.ON_ORDER, ItemStatus.IN_TRANSIT, requestContext);
    //then
    verify(inventoryItemManager, times(2)).updateItem(any(JsonObject.class), any(RequestContext.class));
    assertEquals(ItemStatus.IN_TRANSIT, ItemStatus.fromValue(item.getJsonObject("status").getString("name")));
  }

  private List<Location> getLocations(String... tenantIds) {
    if (tenantIds.length == 0) {
      return List.of();
    }
    List<Location> result = new ArrayList<>();
    for (String tenantId: tenantIds) {
      result.add(new Location().withTenantId(tenantId));
    }
    return result;
  }
}
