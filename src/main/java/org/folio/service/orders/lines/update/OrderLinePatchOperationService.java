package org.folio.service.orders.lines.update;

import org.folio.rest.core.models.RequestContext;
import org.folio.rest.jaxrs.model.PatchOrderLineRequest;
import org.folio.rest.jaxrs.model.PoLine;

import java.util.concurrent.CompletableFuture;

public class OrderLinePatchOperationService {

  private final OrderLinePatchOperationHandlerResolver orderLinePatchOperationHandlerResolver;

  public OrderLinePatchOperationService(OrderLinePatchOperationHandlerResolver orderLinePatchOperationHandlerResolver) {
    this.orderLinePatchOperationHandlerResolver = orderLinePatchOperationHandlerResolver;

  }

  public CompletableFuture<Void> patch(PoLine poLine, PatchOrderLineRequest request, RequestContext requestContext){
    OrderLineUpdateInstanceHolder orderLineUpdateInstanceHolder
        = new OrderLineUpdateInstanceHolder()
        .withPathOrderLineRequest(request)
        .withStoragePoLine(poLine);

    PatchOperationHandler patchOperationHandler = orderLinePatchOperationHandlerResolver.resolve(request.getOperation());
    return patchOperationHandler.handle(orderLineUpdateInstanceHolder, requestContext);
  }
}
