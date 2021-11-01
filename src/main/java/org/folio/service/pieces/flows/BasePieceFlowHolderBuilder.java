package org.folio.service.pieces.flows;

import java.util.concurrent.CompletableFuture;

import org.folio.models.pieces.BasePieceFlowHolder;
import org.folio.rest.core.models.RequestContext;
import org.folio.service.orders.PurchaseOrderLineService;
import org.folio.service.orders.PurchaseOrderStorageService;

public class BasePieceFlowHolderBuilder {
  protected final PurchaseOrderStorageService purchaseOrderStorageService;
  protected final PurchaseOrderLineService purchaseOrderLineService;

  public BasePieceFlowHolderBuilder(PurchaseOrderStorageService purchaseOrderStorageService, PurchaseOrderLineService purchaseOrderLineService) {
    this.purchaseOrderStorageService = purchaseOrderStorageService;
    this.purchaseOrderLineService = purchaseOrderLineService;
  }

  public CompletableFuture<Void> updateHolderWithOrderInformation(BasePieceFlowHolder holder, RequestContext requestContext) {
    return purchaseOrderLineService.getOrderLineById(holder.getOrderLineId(), requestContext)
      .thenCompose(poLine -> purchaseOrderStorageService.getPurchaseOrderById(poLine.getPurchaseOrderId(), requestContext)
        .thenAccept(purchaseOrder -> holder.withOrderInformation(purchaseOrder, poLine))
      );
  }
}
