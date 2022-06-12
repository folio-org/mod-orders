package org.folio.service.orders.lines.update.instance;

import java.util.ArrayList;
import java.util.List;
import java.util.Objects;
import java.util.concurrent.CompletableFuture;

import org.apache.commons.lang3.StringUtils;
import org.folio.models.orders.lines.update.OrderLineUpdateInstanceHolder;
import org.folio.rest.acq.model.StoragePatchOrderLineRequest;
import org.folio.rest.core.models.RequestContext;
import org.folio.rest.jaxrs.model.PatchOrderLineRequest;
import org.folio.rest.jaxrs.model.PoLine;
import org.folio.rest.jaxrs.model.ReplaceInstanceRef;
import org.folio.service.inventory.InventoryManager;
import org.folio.service.orders.lines.update.OrderLineUpdateInstanceStrategy;

public class WithoutHoldingOrderLineUpdateInstanceStrategy implements OrderLineUpdateInstanceStrategy {
  private final InventoryManager inventoryManager;

  public WithoutHoldingOrderLineUpdateInstanceStrategy(InventoryManager inventoryManager) {
    this.inventoryManager = inventoryManager;
  }

  @Override
  public CompletableFuture<Void> updateInstance(OrderLineUpdateInstanceHolder holder, RequestContext rqContext) {
    if (Objects.nonNull(holder.getPatchOrderLineRequest()) && holder.getPatchOrderLineRequest()
      .getOperation()
      .value()
      .equals(PatchOrderLineRequest.Operation.REPLACE_INSTANCE_REF.value()) && Objects.nonNull(holder.getStoragePoLine())) {
      return processHoldings(holder, rqContext);
    }
    return CompletableFuture.completedFuture(null);
  }

  private CompletableFuture<Void> processHoldings(OrderLineUpdateInstanceHolder holder, RequestContext requestContext) {
    if (Objects.nonNull(holder.getPatchOrderLineRequest().getReplaceInstanceRef())) {
      ReplaceInstanceRef replaceInstanceRef = holder.getPatchOrderLineRequest().getReplaceInstanceRef();
      String newInstanceId = replaceInstanceRef.getNewInstanceId();

      holder.createStoragePatchOrderLineRequest(StoragePatchOrderLineRequest.PatchOrderLineOperationType.REPLACE_INSTANCE_REF, newInstanceId);

      return deleteAbandonedHoldings(replaceInstanceRef.getDeleteAbandonedHoldings(), holder.getStoragePoLine(), requestContext);
    }
    return CompletableFuture.completedFuture(null);
  }

  private CompletableFuture<Void> deleteAbandonedHoldings(boolean isDeleteAbandonedHoldings, PoLine poLine, RequestContext requestContext) {
    List<CompletableFuture<Void>> futures = new ArrayList<>();
    if (isDeleteAbandonedHoldings) {
      poLine.getLocations().forEach(location -> {
          String holdingId = location.getHoldingId();
          if (!StringUtils.isEmpty(holdingId)) {
            futures.add(deleteHoldingWithoutItems(requestContext, holdingId));
          }
        });
    }
    return CompletableFuture.allOf(futures.toArray(CompletableFuture[]::new));

  }

  private CompletableFuture<Void> deleteHoldingWithoutItems(RequestContext requestContext, String holdingId) {
    return inventoryManager.getItemsByHoldingId(holdingId, requestContext)
      .thenCompose(items -> {
        if (items.isEmpty()) {
          return inventoryManager.deleteHoldingById(holdingId, true, requestContext);
        } else {
          return CompletableFuture.completedFuture(null);
        }
      });
  }
}
