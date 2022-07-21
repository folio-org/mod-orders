package org.folio.service.orders.lines.update.instance;

import static java.util.stream.Collectors.toList;
import static org.folio.service.inventory.InventoryManager.ID;
import static org.folio.service.inventory.InventoryManager.ITEM_HOLDINGS_RECORD_ID;

import java.util.ArrayList;
import java.util.List;
import java.util.Objects;
import java.util.concurrent.CompletableFuture;

import org.apache.commons.collections4.CollectionUtils;
import org.apache.commons.lang3.ObjectUtils;
import org.folio.models.orders.lines.update.OrderLineUpdateInstanceHolder;
import org.folio.orders.utils.PoLineCommonUtil;
import org.folio.rest.acq.model.StoragePatchOrderLineRequest;
import org.folio.rest.core.exceptions.ErrorCodes;
import org.folio.rest.core.exceptions.HttpException;
import org.folio.rest.core.models.RequestContext;
import org.folio.rest.jaxrs.model.CompositePoLine;
import org.folio.rest.jaxrs.model.Error;
import org.folio.rest.jaxrs.model.Location;
import org.folio.rest.jaxrs.model.Parameter;
import org.folio.rest.jaxrs.model.ReplaceInstanceRef;
import org.folio.service.inventory.InventoryManager;

import io.vertx.core.json.JsonObject;


public class WithHoldingOrderLineUpdateInstanceStrategy extends BaseOrderLineUpdateInstanceStrategy {

  public WithHoldingOrderLineUpdateInstanceStrategy(InventoryManager inventoryManager) {
    super(inventoryManager);
  }

  protected CompletableFuture<Void> processHoldings(OrderLineUpdateInstanceHolder holder, RequestContext requestContext) {
    if (Objects.nonNull(holder.getPatchOrderLineRequest().getReplaceInstanceRef())) {
      ReplaceInstanceRef replaceInstanceRef = holder.getPatchOrderLineRequest().getReplaceInstanceRef();
      String newInstanceId = replaceInstanceRef.getNewInstanceId();

      holder.createStoragePatchOrderLineRequest(StoragePatchOrderLineRequest.PatchOrderLineOperationType.REPLACE_INSTANCE_REF, newInstanceId);

      switch (replaceInstanceRef.getHoldingsOperation()) {
        case MOVE:
          return moveHoldings(holder, newInstanceId, requestContext);
        case FIND_OR_CREATE:
          return findOrCreateHoldingsAndUpdateItems(holder, newInstanceId, requestContext)
              .thenAccept(v -> deleteAbandonedHoldings(replaceInstanceRef.getDeleteAbandonedHoldings(),
                  holder.getStoragePoLine(), requestContext));
        case CREATE:
          return createHoldingsAndUpdateItems(holder, newInstanceId, requestContext)
              .thenAccept(v -> deleteAbandonedHoldings(replaceInstanceRef.getDeleteAbandonedHoldings(),
                  holder.getStoragePoLine(), requestContext));
      case NONE:
        default:
          return CompletableFuture.completedFuture(null);
      }
    } else {
      return CompletableFuture.completedFuture(null);
    }
  }


  private CompletableFuture<Void> moveHoldings(OrderLineUpdateInstanceHolder holder, String newInstanceId, RequestContext requestContext) {
    List<String> holdingIds = holder.getStoragePoLine()
      .getLocations()
      .stream()
      .map(Location::getHoldingId)
      .filter(Objects::nonNull)
      .collect(toList());
    holdingIds.forEach(id -> holder.addHoldingRefsToStoragePatchOrderLineRequest(id, id));

    return inventoryManager.getHoldingsByIds(holdingIds, requestContext)
        .thenCompose(holdings -> inventoryManager.updateInstanceForHoldingRecords(holdings, newInstanceId, requestContext));
  }

  private CompletableFuture<Void> findOrCreateHoldingsAndUpdateItems(OrderLineUpdateInstanceHolder holder,
      String newInstanceId, RequestContext requestContext) {

    List<CompletableFuture<Void>> futures = new ArrayList<>();

    holder.getStoragePoLine().getLocations().forEach(location -> {
      String holdingId = location.getHoldingId();
      if (holdingId != null) {
        futures.add(inventoryManager.getOrCreateHoldingRecordByInstanceAndLocation(newInstanceId, location, requestContext )
              .thenCompose(newHoldingId -> {
                holder.addHoldingRefsToStoragePatchOrderLineRequest(holdingId, newHoldingId);
                CompositePoLine compositePoLine = PoLineCommonUtil.convertToCompositePoLine(holder.getStoragePoLine());
                if (ObjectUtils.notEqual(holdingId, newHoldingId)) {
                  return updateItemsHolding(holdingId, newHoldingId, compositePoLine.getId(), requestContext);
                } else {
                  return CompletableFuture.completedFuture(null);
                }
            })
        );
      }
    });

    return CompletableFuture.allOf(futures.toArray(new CompletableFuture[0]));
  }

  private CompletableFuture<Void> createHoldingsAndUpdateItems(OrderLineUpdateInstanceHolder holder,
      String newInstanceId, RequestContext requestContext) {

    List<CompletableFuture<Void>> futures = new ArrayList<>();

    holder.getStoragePoLine().getLocations().forEach(location -> {
      String holdingId = location.getHoldingId();
      if (holdingId != null) {
        futures.add(inventoryManager.createHolding(newInstanceId, location, requestContext)
            .thenCompose(newHoldingId -> {
                holder.addHoldingRefsToStoragePatchOrderLineRequest(holdingId, newHoldingId);
                CompositePoLine compositePoLine = PoLineCommonUtil.convertToCompositePoLine(holder.getStoragePoLine());
                return updateItemsHolding(holdingId, newHoldingId, compositePoLine.getId(), requestContext);
            })
        );
      }
    });

    return CompletableFuture.allOf(futures.toArray(new CompletableFuture[0]));
  }

  private CompletableFuture<Void> updateItemsHolding(String holdingId, String newHoldingId, String poLineId, RequestContext requestContext) {
    return inventoryManager.getItemsByHoldingIdAndOrderLineId(holdingId, poLineId, requestContext)
        .thenApply(items -> updateItemHoldingId(items, newHoldingId))
        .thenCompose(items -> updateItemsInInventory(items, requestContext));
  }

  private List<JsonObject> updateItemHoldingId(List<JsonObject> items, String holdingId) {
    items.forEach(item -> item.put(ITEM_HOLDINGS_RECORD_ID, holdingId));
    return items;
  }

  private CompletableFuture<Void> updateItemsInInventory(List<JsonObject> items, RequestContext requestContext) {
    List<Parameter> parameters = new ArrayList<>();
    return CompletableFuture.allOf(items.stream()
        .map(item -> inventoryManager.updateItem(item, requestContext)
            .exceptionally(ex -> {
              Parameter parameter = new Parameter().withKey("itemId").withValue(item.getString(ID));
              if (ex.getCause() instanceof HttpException){
                HttpException httpException = (HttpException) ex.getCause();
                parameter.withAdditionalProperty("originalError", httpException.getError().getMessage());
              }
              parameters.add(parameter);
              return null;
            }))
        .toArray(CompletableFuture[]::new))
        .thenAccept(v -> {
      if (CollectionUtils.isNotEmpty(parameters)) {
        Error error = ErrorCodes.ITEM_UPDATE_FAILED.toError().withParameters(parameters);
        throw new HttpException(500, error);
      }
    });
  }

}
