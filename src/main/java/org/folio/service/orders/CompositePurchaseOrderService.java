package org.folio.service.orders;

import static org.folio.orders.utils.HelperUtils.convertToCompositePurchaseOrder;

import java.util.concurrent.CompletableFuture;

import org.folio.rest.core.models.RequestContext;
import org.folio.rest.jaxrs.model.CompositePurchaseOrder;

import io.vertx.core.json.JsonObject;

public class CompositePurchaseOrderService {

  private final PurchaseOrderService purchaseOrderService;
  private final PurchaseOrderLineService purchaseOrderLineService;

  public CompositePurchaseOrderService(PurchaseOrderService purchaseOrderService,
      PurchaseOrderLineService purchaseOrderLineService) {
    this.purchaseOrderService = purchaseOrderService;
    this.purchaseOrderLineService = purchaseOrderLineService;
  }

  public CompletableFuture<CompositePurchaseOrder> getCompositeOrderById(String orderId, RequestContext requestContext) {
    return purchaseOrderService.getPurchaseOrderById(orderId, requestContext)
      .thenCompose(purchaseOrder -> purchaseOrderLineService.getCompositePoLinesByOrderId( orderId, requestContext)
      .thenApply(poLines -> convertToCompositePurchaseOrder(JsonObject.mapFrom(purchaseOrder)).withCompositePoLines(poLines)));
  }
}
