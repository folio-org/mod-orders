package org.folio.service.orders.lines.update.instance;

import java.util.Objects;
import java.util.concurrent.CompletableFuture;

import org.folio.models.orders.lines.update.OrderLineUpdateInstanceHolder;
import org.folio.rest.acq.model.StoragePatchOrderLineRequest;
import org.folio.rest.core.models.RequestContext;
import org.folio.rest.jaxrs.model.ReplaceInstanceRef;
import org.folio.service.inventory.InventoryManager;

public class WithoutHoldingOrderLineUpdateInstanceStrategy extends BaseOrderLineUpdateInstanceStrategy {

  public WithoutHoldingOrderLineUpdateInstanceStrategy(InventoryManager inventoryManager) {
    super(inventoryManager);
  }

  CompletableFuture<Void> processHoldings(OrderLineUpdateInstanceHolder holder, RequestContext requestContext) {
    if (Objects.nonNull(holder.getPatchOrderLineRequest().getReplaceInstanceRef())) {
      ReplaceInstanceRef replaceInstanceRef = holder.getPatchOrderLineRequest().getReplaceInstanceRef();
      String newInstanceId = replaceInstanceRef.getNewInstanceId();

      holder.createStoragePatchOrderLineRequest(StoragePatchOrderLineRequest.PatchOrderLineOperationType.REPLACE_INSTANCE_REF, newInstanceId);

      return deleteAbandonedHoldings(replaceInstanceRef.getDeleteAbandonedHoldings(), holder.getStoragePoLine(), requestContext);
    }
    return CompletableFuture.completedFuture(null);
  }

}
