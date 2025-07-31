package org.folio.service.orders.lines.update.instance;

import io.vertx.core.Future;
import org.folio.models.orders.lines.update.OrderLineUpdateInstanceHolder;
import org.folio.rest.acq.model.StoragePatchOrderLineRequest;
import org.folio.rest.core.models.RequestContext;
import org.folio.rest.jaxrs.model.ReplaceInstanceRef;
import org.folio.service.inventory.InventoryHoldingManager;
import org.folio.service.inventory.InventoryInstanceManager;
import org.folio.service.inventory.InventoryItemManager;

public class WithoutHoldingOrderLineUpdateInstanceStrategy extends BaseOrderLineUpdateInstanceStrategy {

  public WithoutHoldingOrderLineUpdateInstanceStrategy(InventoryInstanceManager inventoryInstanceManager,
                                                       InventoryItemManager inventoryItemManager,
                                                       InventoryHoldingManager inventoryHoldingManager) {
    super(inventoryInstanceManager, inventoryItemManager, inventoryHoldingManager);
  }

  Future<Void> processHoldings(OrderLineUpdateInstanceHolder holder, RequestContext requestContext) {
    ReplaceInstanceRef replaceInstanceRef = holder.getPatchOrderLineRequest().getReplaceInstanceRef();
    if (replaceInstanceRef == null) {
      return Future.succeededFuture();
    }
    String newInstanceId = replaceInstanceRef.getNewInstanceId();

    holder.createStoragePatchOrderLineRequest(StoragePatchOrderLineRequest.PatchOrderLineOperationType.REPLACE_INSTANCE_REF, newInstanceId);

    return deleteAbandonedHoldings(replaceInstanceRef.getDeleteAbandonedHoldings(), holder.getStoragePoLine().getLocations(), requestContext)
      .mapEmpty();
  }

}
