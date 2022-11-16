package org.folio.service.orders.lines.update.instance;

import java.util.ArrayList;
import java.util.List;
import java.util.Objects;

import org.apache.commons.lang3.StringUtils;
import org.folio.models.orders.lines.update.OrderLineUpdateInstanceHolder;
import org.folio.okapi.common.GenericCompositeFuture;
import org.folio.rest.core.models.RequestContext;
import org.folio.rest.jaxrs.model.PatchOrderLineRequest;
import org.folio.rest.jaxrs.model.PoLine;
import org.folio.service.inventory.InventoryManager;
import org.folio.service.orders.lines.update.OrderLineUpdateInstanceStrategy;

import io.vertx.core.Future;

public abstract class BaseOrderLineUpdateInstanceStrategy implements OrderLineUpdateInstanceStrategy {

  InventoryManager inventoryManager;

  protected BaseOrderLineUpdateInstanceStrategy(InventoryManager inventoryManager) {
    this.inventoryManager = inventoryManager;
  }

  @Override
  public Future<Void> updateInstance(OrderLineUpdateInstanceHolder holder, RequestContext requestContext) {
    if (Objects.nonNull(holder.getPatchOrderLineRequest()) && holder.getPatchOrderLineRequest()
      .getOperation()
      .value()
      .equals(PatchOrderLineRequest.Operation.REPLACE_INSTANCE_REF.value()) && Objects.nonNull(holder.getStoragePoLine())) {
      return processHoldings(holder, requestContext);
    }
    return Future.succeededFuture();
  }

  abstract Future<Void> processHoldings(OrderLineUpdateInstanceHolder holder, RequestContext requestContext);

  Future<Void> deleteAbandonedHoldings(boolean isDeleteAbandonedHoldings, PoLine poLine, RequestContext requestContext) {
    List<Future<Void>> futures = new ArrayList<>();
    if (isDeleteAbandonedHoldings) {
      poLine.getLocations().forEach(location -> {
        String holdingId = location.getHoldingId();
        if (!StringUtils.isEmpty(holdingId)) {
          futures.add(deleteHoldingWithoutItems(holdingId, requestContext));
        }
      });
    }
    return GenericCompositeFuture.all(futures)
      .mapEmpty();

  }

  private Future<Void> deleteHoldingWithoutItems(String holdingId, RequestContext requestContext) {
    return inventoryManager.getItemsByHoldingId(holdingId, requestContext)
      .compose(items -> {
        if (items.isEmpty()) {
          return inventoryManager.deleteHoldingById(holdingId, true, requestContext);
        } else {
          return Future.succeededFuture();
        }
      });
  }
}
