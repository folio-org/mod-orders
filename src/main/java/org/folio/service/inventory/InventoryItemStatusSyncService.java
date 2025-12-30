package org.folio.service.inventory;

import io.vertx.core.Future;
import io.vertx.core.json.JsonObject;
import one.util.streamex.StreamEx;
import org.apache.commons.collections4.CollectionUtils;
import org.apache.logging.log4j.LogManager;
import org.apache.logging.log4j.Logger;
import org.folio.completablefuture.VertxFutureRepeater;
import org.folio.models.ItemStatus;
import org.folio.orders.utils.PoLineCommonUtil;
import org.folio.orders.utils.RequestContextUtil;
import org.folio.rest.core.models.RequestContext;
import org.folio.rest.jaxrs.model.Location;
import org.folio.rest.jaxrs.model.PoLine;

import java.time.Instant;
import java.util.ArrayList;
import java.util.HashMap;
import java.util.List;
import java.util.Map;

import static org.folio.helper.BaseHelper.MAX_REPEAT_ON_FAILURE;
import static org.folio.rest.RestConstants.MAX_IDS_FOR_GET_RQ_15;

public class InventoryItemStatusSyncService {
  private static final Logger logger = LogManager.getLogger(InventoryItemStatusSyncService.class);

  private final InventoryItemManager inventoryItemManager;

  public InventoryItemStatusSyncService(InventoryItemManager inventoryItemManager) {
    this.inventoryItemManager = inventoryItemManager;
  }

  /**
   * Update Inventory item statuses that have an On order status for the provided order line.
   * For ECS based envs cross-tenant requests are possible to update Inventory item statuses in different tenants based on PoLine:Location:tenantId
   *
   * @param poLineId       the po line id used to find related items and update statuses
   * @param locations      the locations array
   * @param newStatus      the new status to update
   * @param requestContext initial request context
   * @return future with void result when all items processed
   */
  public Future<Void> updateInventoryItemStatus(String poLineId,
                                                List<Location> locations,
                                                ItemStatus newStatus,
                                                RequestContext requestContext) {
    return Future.all(PoLineCommonUtil.getTenantsFromLocations(locations).stream()
        .map(tenantId -> updateItemStatusForTenant(poLineId, newStatus, RequestContextUtil.createContextWithNewTenantId(requestContext, tenantId)))
        .toList())
      .mapEmpty();
  }

  private Future<Void> updateItemStatusForTenant(String poLineId, ItemStatus status, RequestContext requestContext) {
    return inventoryItemManager.getItemsByPoLineIdsAndStatus(List.of(poLineId), ItemStatus.ON_ORDER.value(), requestContext)
      .compose(items -> {
        List<JsonObject> itemRecords = items.stream()
          .filter(item -> poLineId.equals(item.getString(InventoryItemManager.ITEM_PURCHASE_ORDER_LINE_IDENTIFIER)))
          .map(item -> updateItemStatus(item, status))
          .toList();
        if (CollectionUtils.isNotEmpty(itemRecords)) {
          return inventoryItemManager.updateItemRecords(itemRecords, requestContext)
            .onFailure(e -> logger.error("Failed to update Inventory items status to: {}, po line id: {}", status.value(), poLineId, e))
            .mapEmpty();
        }
        return Future.succeededFuture(null);
      });
  }

  /**
   * Update Inventory item statuses for order lines.
   * For ECS based envs cross-tenant requests are possible to update Inventory item statuses in different tenants based on PoLine:Location:tenantId
   *
   * @param poLines        the po lines
   * @param currentStatus  the current status to find items
   * @param newStatus      the new status to update
   * @param requestContext the initial request context
   * @return future with void result when all items processed
   */
  public Future<Void> updateItemStatusesInInventory(List<PoLine> poLines,
                                                    ItemStatus currentStatus,
                                                    ItemStatus newStatus,
                                                    RequestContext requestContext) {
    if (CollectionUtils.isEmpty(poLines)) {
      return Future.succeededFuture();
    }
    Map<String, List<String>> tenantIdToPoLineIds = getTenantIdsToPoLineIdsMap(poLines);
    List<Future<Void>> futures = new ArrayList<>();
    for (Map.Entry<String, List<String>> entry : tenantIdToPoLineIds.entrySet()) {
      String tenantId = entry.getKey();
      List<String> poLineIds = entry.getValue();
      RequestContext updatedContext = RequestContextUtil.createContextWithNewTenantId(requestContext, tenantId);
      Future<Void> updateItemFeature = Future.join(StreamEx.ofSubLists(poLineIds, MAX_IDS_FOR_GET_RQ_15)
          .map(chunk -> VertxFutureRepeater.repeat(MAX_REPEAT_ON_FAILURE, () -> updateItemStatuses(chunk, currentStatus, newStatus, updatedContext)))
          .toList())
        .mapEmpty();
      futures.add(updateItemFeature);
    }
    return Future.all(futures).mapEmpty();
  }

  private Future<Void> updateItemStatuses(List<String> poLineIds,
                                          ItemStatus currentStatus,
                                          ItemStatus newStatus,
                                          RequestContext requestContext) {
    return inventoryItemManager.getItemsByPoLineIdsAndStatus(poLineIds, currentStatus.value(), requestContext)
      .map(items -> updateItemStatuses(items, newStatus))
      .compose(items -> updateItemsInInventory(items, requestContext))
      .onFailure(e -> logger.error("Failed to update Inventory item status to: {} for po line ids: {}", newStatus.value(), poLineIds));
  }

  private Map<String, List<String>> getTenantIdsToPoLineIdsMap(List<PoLine> poLines) {
    Map<String, List<String>> result = new HashMap<>();
    for (PoLine poLine : poLines) {
      for (Location location : poLine.getLocations()) {
        String tenantId = location.getTenantId();
        result.computeIfAbsent(tenantId, k -> new ArrayList<>()).add(poLine.getId());
      }
    }
    return result;
  }

  private List<JsonObject> updateItemStatuses(List<JsonObject> items, ItemStatus status) {
    items.forEach(item -> updateItemStatus(item, status));
    return items;
  }

  private JsonObject updateItemStatus(JsonObject item, ItemStatus itemStatus) {
    JsonObject status = new JsonObject();
    status.put("name", itemStatus.value());
    status.put("date", Instant.now());
    item.put("status", status);
    return item;
  }

  private Future<Void> updateItemsInInventory(List<JsonObject> items, RequestContext requestContext) {
    return Future.join(items.stream()
        .map(item -> inventoryItemManager.updateItem(item, requestContext))
        .toList())
      .mapEmpty();
  }

}
