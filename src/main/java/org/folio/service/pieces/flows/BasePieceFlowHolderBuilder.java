package org.folio.service.pieces.flows;

import java.util.concurrent.CompletableFuture;

import org.folio.models.pieces.BasePieceFlowHolder;
import org.folio.rest.core.models.RequestContext;
import org.folio.service.orders.PurchaseOrderLineService;
import org.folio.service.orders.PurchaseOrderService;

public class BasePieceFlowHolderBuilder {
  protected final PurchaseOrderService purchaseOrderService;
  protected final PurchaseOrderLineService purchaseOrderLineService;

  public BasePieceFlowHolderBuilder(PurchaseOrderService purchaseOrderService, PurchaseOrderLineService purchaseOrderLineService) {
    this.purchaseOrderService = purchaseOrderService;
    this.purchaseOrderLineService = purchaseOrderLineService;
  }

  public CompletableFuture<Void> updateHolderWithOrderInformation(BasePieceFlowHolder holder, RequestContext requestContext) {
    return purchaseOrderLineService.getOrderLineById(holder.getOrderLineId(), requestContext)
      .thenCompose(poLine -> purchaseOrderService.getPurchaseOrderById(poLine.getPurchaseOrderId(), requestContext)
        .thenAccept(purchaseOrder -> holder.withOrderInformation(purchaseOrder, poLine))
      );
  }
}
