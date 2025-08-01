package org.folio.service.orders.lines.update.instance;

import io.vertx.core.Future;
import lombok.extern.log4j.Log4j2;
import org.apache.commons.lang3.BooleanUtils;
import org.apache.commons.lang3.StringUtils;
import org.folio.models.orders.lines.update.OrderLineUpdateInstanceHolder;
import org.folio.orders.utils.RequestContextUtil;
import org.folio.rest.core.models.RequestContext;
import org.folio.rest.jaxrs.model.Location;
import org.folio.service.inventory.InventoryHoldingManager;
import org.folio.service.inventory.InventoryInstanceManager;
import org.folio.service.inventory.InventoryItemManager;
import org.folio.service.orders.lines.update.OrderLineUpdateInstanceStrategy;

import java.util.Collections;
import java.util.List;

import static org.folio.orders.utils.HelperUtils.collectResultsOnSuccessNonNull;

@Log4j2
public abstract class BaseOrderLineUpdateInstanceStrategy implements OrderLineUpdateInstanceStrategy {

  InventoryInstanceManager inventoryInstanceManager;
  InventoryItemManager inventoryItemManager;
  InventoryHoldingManager inventoryHoldingManager;

  protected BaseOrderLineUpdateInstanceStrategy(InventoryInstanceManager inventoryInstanceManager,
                                                InventoryItemManager inventoryItemManager,
                                                InventoryHoldingManager inventoryHoldingManager) {
    this.inventoryInstanceManager = inventoryInstanceManager;
    this.inventoryItemManager = inventoryItemManager;
    this.inventoryHoldingManager = inventoryHoldingManager;
  }

  @Override
  public Future<Void> updateInstance(OrderLineUpdateInstanceHolder holder, RequestContext requestContext) {
    if (holder.getPatchOrderLineRequest() == null || holder.getStoragePoLine() == null) {
      return Future.succeededFuture();
    }
    return processHoldings(holder, requestContext);
  }

  Future<List<String>> deleteAbandonedHoldings(boolean isDeleteAbandonedHoldings, List<Location> locations, RequestContext requestContext) {
    if (!isDeleteAbandonedHoldings) {
      return Future.succeededFuture(Collections.emptyList());
    }
    var deleteHoldingFutures = locations.stream()
      .filter(location -> StringUtils.isNotEmpty(location.getHoldingId()))
      .map(location -> {
        var locationContext = RequestContextUtil.createContextWithNewTenantId(requestContext, location.getTenantId());
        return deleteHoldingWithoutItems(location.getHoldingId(), locationContext)
          .onSuccess(v -> log.info("deleteAbandonedHoldings:: operation succeeded for holdingId: {}", location.getHoldingId()))
          .onFailure(e -> log.error("Failed to delete abandoned holdings for holdingId: {}", location.getHoldingId(), e))
          .map(deleted -> BooleanUtils.isTrue(deleted) ? location.getHoldingId() : null);
      }).toList();
    return collectResultsOnSuccessNonNull(deleteHoldingFutures);
  }

  private Future<Boolean> deleteHoldingWithoutItems(String holdingId, RequestContext requestContext) {
    return inventoryItemManager.getItemsByHoldingId(holdingId, requestContext)
      .compose(items -> items.isEmpty()
        ? inventoryHoldingManager.deleteHoldingById(holdingId, true, requestContext).map(true)
        : Future.succeededFuture(false));
  }

  abstract Future<Void> processHoldings(OrderLineUpdateInstanceHolder holder, RequestContext requestContext);

}
