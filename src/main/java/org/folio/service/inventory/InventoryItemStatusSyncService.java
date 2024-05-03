package org.folio.service.inventory;

import io.vertx.core.Future;
import io.vertx.core.json.JsonObject;
import one.util.streamex.StreamEx;
import org.apache.commons.collections4.CollectionUtils;
import org.folio.completablefuture.VertxFutureRepeater;
import org.folio.models.ItemStatus;
import org.folio.okapi.common.GenericCompositeFuture;
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
import java.util.Optional;

import static java.util.stream.Collectors.toList;
import static org.folio.helper.BaseHelper.MAX_REPEAT_ON_FAILURE;
import static org.folio.rest.RestConstants.MAX_IDS_FOR_GET_RQ_15;

public class InventoryItemStatusSyncService {
  private final InventoryItemManager inventoryItemManager;

  public InventoryItemStatusSyncService(InventoryItemManager inventoryItemManager) {
    this.inventoryItemManager = inventoryItemManager;
  }

  /**
   * Update Inventory item statuses that have an On order status for the provided order line.
   * For ECS based envs cross-tenant requests are possible to update Inventory item statuses in different tenants based on PoLine:Location:tenantId
   *
   * @param poLineId the po line id used to find related items and update statuses
   * @param locations the locations array
   * @param newStatus the new status to update
   * @param requestContext initial request context
   * @return future with void result when all items processed
   */
  public Future<Void> updateInventoryItemStatus(String poLineId,
                                                List<Location> locations,
                                                ItemStatus newStatus,
                                                RequestContext requestContext) {
    return GenericCompositeFuture.all(
      PoLineCommonUtil.getTenantsFromLocations(locations)
        .stream()
        .map(tenantId -> updateItemStatusForTenant(poLineId, newStatus, RequestContextUtil.createContextWithNewTenantId(requestContext, tenantId)))
        .toList()
    ).mapEmpty();
  }

  private Future<Void> updateItemStatusForTenant(String poLineId, ItemStatus status, RequestContext requestContext) {
    return inventoryItemManager.getItemsByPoLineIdsAndStatus(List.of(poLineId), ItemStatus.ON_ORDER.value(), requestContext)
      .compose(items -> {
        //Each poLine can have only one linked item
        Optional<JsonObject> poLineItem = items.stream()
          .filter(item -> poLineId.equals(item.getString(InventoryItemManager.ITEM_PURCHASE_ORDER_LINE_IDENTIFIER))).findFirst();
        if (poLineItem.isPresent()) {
          JsonObject updatedItem = updateItemStatus(poLineItem.get(), status);
          return inventoryItemManager.updateItem(updatedItem, requestContext);
        }
        return Future.succeededFuture(null);
      });
  }

  private JsonObject updateItemStatus(JsonObject poLineItem, ItemStatus itemStatus) {
    JsonObject status = new JsonObject();
    status.put("name", itemStatus);
    status.put("date", Instant.now());
    poLineItem.put("status", status);
    return poLineItem;
  }

  /**
   * Update Inventory item statuses for order lines.
   * For ECS based envs cross-tenant requests are possible to update Inventory item statuses in different tenants based on PoLine:Location:tenantId
   *
   * @param poLines the po lines
   * @param currentStatus the current status to find items
   * @param newStatus the new status to update
   * @param requestContext the initial request context
   * @return future with void result when all items processed
   */
  public Future<Void> updateItemStatusesInInventory(List<PoLine> poLines,
                                                    ItemStatus currentStatus,
                                                    ItemStatus newStatus,
                                                    RequestContext requestContext) {
    if (CollectionUtils.isEmpty(poLines)) {
      return io.vertx.core.Future.succeededFuture();
    }
    return GenericCompositeFuture.join(
        StreamEx.ofSubLists(poLines, MAX_IDS_FOR_GET_RQ_15)
          .map(chunk -> VertxFutureRepeater.repeat(MAX_REPEAT_ON_FAILURE, () -> updateItemStatuses(chunk, currentStatus, newStatus, requestContext)))
          .collect(toList()))
      .mapEmpty();
  }

  private Future<Void> updateItemStatuses(List<PoLine> poLines,
                                          ItemStatus currentStatus,
                                          ItemStatus newStatus,
                                          RequestContext requestContext) {
    Map<String, List<String>> tenantIdToPoLineIds = getTenantIdsToPoLineIdsMap(poLines);
    List<Future<Void>> futures = new ArrayList<>();
    for (Map.Entry<String, List<String>> entry : tenantIdToPoLineIds.entrySet()) {
      String tenantId = entry.getKey();
      List<String> poLineIds = entry.getValue();
      RequestContext updatedContext = RequestContextUtil.createContextWithNewTenantId(requestContext, tenantId);
      Future<Void> updateItemFeature = inventoryItemManager.getItemsByPoLineIdsAndStatus(poLineIds, currentStatus.value(), updatedContext)
        .map(items -> updateStatusName(items, newStatus))
        .compose(items -> updateItemsInInventory(items, requestContext));
      futures.add(updateItemFeature);
    }
    return GenericCompositeFuture.all(futures)
      .mapEmpty();
  }

  private Map<String, List<String>> getTenantIdsToPoLineIdsMap(List<PoLine> poLines) {
    Map<String, List<String>> result = new HashMap<>();
    for (PoLine poLine : poLines) {
      for (Location location : poLine.getLocations()) {
        String tenantId = location.getTenantId();
        if (!result.containsKey(tenantId)) {
          result.put(tenantId, new ArrayList<>());
        }
        result.get(tenantId).add(poLine.getId());
      }
    }
    return result;
  }

  private List<JsonObject> updateStatusName(List<JsonObject> items, ItemStatus status) {
    items.forEach(item -> item.getJsonObject("status").put("name", status.value()));
    return items;
  }

  private Future<Void> updateItemsInInventory(List<JsonObject> items, RequestContext requestContext) {
    return GenericCompositeFuture.join(items.stream()
        .map(item -> inventoryItemManager.updateItem(item, requestContext))
        .collect(toList()))
      .mapEmpty();
  }
}
