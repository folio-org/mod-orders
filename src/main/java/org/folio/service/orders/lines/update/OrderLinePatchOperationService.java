package org.folio.service.orders.lines.update;

import org.folio.rest.core.models.RequestContext;
import org.folio.rest.jaxrs.model.PatchOrderLineRequest;
import org.folio.service.orders.PurchaseOrderLineService;

import java.util.concurrent.CompletableFuture;

public class OrderLinePatchOperationService {

  private final OrderLinePatchOperationHandlerResolver orderLinePatchOperationHandlerResolver;

  private final PurchaseOrderLineService purchaseOrderLineService;

  public OrderLinePatchOperationService(OrderLinePatchOperationHandlerResolver orderLinePatchOperationHandlerResolver,
      PurchaseOrderLineService purchaseOrderLineService) {
    this.orderLinePatchOperationHandlerResolver = orderLinePatchOperationHandlerResolver;
    this.purchaseOrderLineService = purchaseOrderLineService;
  }

  public CompletableFuture<Void> patch(String lineId, PatchOrderLineRequest request, RequestContext requestContext){
    CompletableFuture<Void> future = new CompletableFuture<>();

    purchaseOrderLineService.getOrderLineById(lineId, requestContext)
        .thenAccept(poLine ->     {
          OrderLineUpdateInstanceHolder orderLineUpdateInstanceHolder
            = new OrderLineUpdateInstanceHolder()
            .withPathOrderLineRequest(request)
            .withStoragePoLine(poLine);

          PatchOperationHandler patchOperationHandler = orderLinePatchOperationHandlerResolver.resolve(request.getOperation());
          patchOperationHandler.handle(orderLineUpdateInstanceHolder, requestContext)
              .thenAccept(v -> future.complete(null))
              .exceptionally(t -> {
                future.completeExceptionally(t);
                return null;
              });
        }).exceptionally(t -> {
          future.completeExceptionally(t);
          return null;
        });

    return future;
  }
}
