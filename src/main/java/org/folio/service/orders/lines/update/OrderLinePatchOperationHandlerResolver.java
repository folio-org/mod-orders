package org.folio.service.orders.lines.update;

import java.util.Map;

import org.folio.rest.jaxrs.model.PatchOrderLineRequest;

public class OrderLinePatchOperationHandlerResolver {
  private final Map<PatchOrderLineRequest.Operation, PatchOperationHandler> handlers;

  public OrderLinePatchOperationHandlerResolver(Map<PatchOrderLineRequest.Operation, PatchOperationHandler> handlers) {
    this.handlers = handlers;
  }

  public PatchOperationHandler resolve(PatchOrderLineRequest.Operation opType) {
    return this.handlers.get(opType);
  }
}
