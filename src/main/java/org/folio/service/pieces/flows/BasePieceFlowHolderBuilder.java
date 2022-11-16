package org.folio.service.pieces.flows;

import org.folio.models.pieces.BasePieceFlowHolder;
import org.folio.rest.core.models.RequestContext;
import org.folio.service.orders.PurchaseOrderLineService;
import org.folio.service.orders.PurchaseOrderStorageService;

import io.vertx.core.Future;

public class BasePieceFlowHolderBuilder {
  protected final PurchaseOrderStorageService purchaseOrderStorageService;
  protected final PurchaseOrderLineService purchaseOrderLineService;

  public BasePieceFlowHolderBuilder(PurchaseOrderStorageService purchaseOrderStorageService, PurchaseOrderLineService purchaseOrderLineService) {
    this.purchaseOrderStorageService = purchaseOrderStorageService;
    this.purchaseOrderLineService = purchaseOrderLineService;
  }

  public Future<Void> updateHolderWithOrderInformation(BasePieceFlowHolder holder, RequestContext requestContext) {
    return purchaseOrderLineService.getOrderLineById(holder.getOrderLineId(), requestContext)
      .compose(poLine -> purchaseOrderStorageService.getPurchaseOrderById(poLine.getPurchaseOrderId(), requestContext)
        .onSuccess(purchaseOrder -> holder.withOrderInformation(purchaseOrder, poLine)))
        .mapEmpty();

  }
}
