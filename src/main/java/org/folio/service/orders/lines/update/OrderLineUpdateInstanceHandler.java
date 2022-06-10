package org.folio.service.orders.lines.update;

import org.folio.models.orders.lines.update.OrderLineUpdateInstanceHolder;
import org.folio.rest.core.models.RequestContext;
import org.folio.rest.jaxrs.model.CreateInventoryType;
import org.folio.rest.jaxrs.model.PoLine;

import java.util.concurrent.CompletableFuture;

public class OrderLineUpdateInstanceHandler implements PatchOperationHandler{
  private final OrderLineUpdateInstanceStrategyResolver orderLineUpdateInstanceStrategyResolver;

  public OrderLineUpdateInstanceHandler(OrderLineUpdateInstanceStrategyResolver orderLineUpdateInstanceStrategyResolver) {
    this.orderLineUpdateInstanceStrategyResolver = orderLineUpdateInstanceStrategyResolver;
  }

  @Override public CompletableFuture<Void> handle(OrderLineUpdateInstanceHolder holder, RequestContext requestContext) {
    PoLine storagePoLine = holder.getStoragePoLine();

    switch (storagePoLine.getOrderFormat()) {
      case P_E_MIX:
        return orderLineUpdateInstanceStrategyResolver
            .resolve(CreateInventoryType.fromValue(storagePoLine.getPhysical().getCreateInventory().value()))
            .updateInstance(holder, requestContext)
            .thenCompose(v -> orderLineUpdateInstanceStrategyResolver
                .resolve(CreateInventoryType.fromValue(storagePoLine.getEresource().getCreateInventory().value()))
                .updateInstance(holder, requestContext));
      case ELECTRONIC_RESOURCE:
        return orderLineUpdateInstanceStrategyResolver
            .resolve(CreateInventoryType.fromValue(storagePoLine.getEresource().getCreateInventory().value()))
            .updateInstance(holder, requestContext);
      case OTHER:
      case PHYSICAL_RESOURCE:
        return orderLineUpdateInstanceStrategyResolver
            .resolve(CreateInventoryType.fromValue(storagePoLine.getPhysical().getCreateInventory().value()))
            .updateInstance(holder, requestContext);
      default:
        return CompletableFuture.completedFuture(null);
    }
  }
}
