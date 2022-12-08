package org.folio.service.orders.lines.update;

import org.folio.models.orders.lines.update.OrderLineUpdateInstanceHolder;
import org.folio.rest.core.models.RequestContext;

import io.vertx.core.Future;

public interface PatchOperationHandler {
  Future<Void> handle(OrderLineUpdateInstanceHolder holder, RequestContext requestContext);
}
