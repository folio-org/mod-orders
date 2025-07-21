package org.folio.service.orders.lines.update.instance;

import io.vertx.core.Future;
import org.apache.commons.lang3.StringUtils;
import org.apache.logging.log4j.LogManager;
import org.apache.logging.log4j.Logger;
import org.folio.models.orders.lines.update.OrderLineUpdateInstanceHolder;
import org.folio.orders.utils.RequestContextUtil;
import org.folio.rest.core.models.RequestContext;
import org.folio.rest.jaxrs.model.PoLine;
import org.folio.service.inventory.InventoryHoldingManager;
import org.folio.service.inventory.InventoryInstanceManager;
import org.folio.service.inventory.InventoryItemManager;
import org.folio.service.orders.lines.update.OrderLineUpdateInstanceStrategy;

import java.util.Collections;
import java.util.List;

import static org.folio.orders.utils.HelperUtils.collectResultsOnSuccessNonNull;

public abstract class BaseOrderLineUpdateInstanceStrategy implements OrderLineUpdateInstanceStrategy {

  private static final Logger logger = LogManager.getLogger(BaseOrderLineUpdateInstanceStrategy.class);

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

  Future<List<String>> deleteAbandonedHoldings(boolean isDeleteAbandonedHoldings, PoLine poLine, RequestContext requestContext) {
    if (!isDeleteAbandonedHoldings) {
      return Future.succeededFuture(Collections.emptyList());
    }
    var deleteHoldingFutures = poLine.getLocations().stream()
      .filter(location -> StringUtils.isNotEmpty(location.getHoldingId()))
      .map(location -> {
        var locationContext = RequestContextUtil.createContextWithNewTenantId(requestContext, location.getTenantId());
        return deleteHoldingWithoutItems(location.getHoldingId(), locationContext)
          .onSuccess(v -> logger.info("deleteAbandonedHoldings:: operation succeeded for holdingId: {}", location.getHoldingId()))
          .onFailure(e -> logger.error("Failed to delete abandoned holdings for holdingId: {}", location.getHoldingId(), e))
          .map(deleted -> deleted ? location.getHoldingId() : null);
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
