package org.folio.service.orders.lines.update;

import org.folio.models.orders.lines.update.OrderLineUpdateInstanceHolder;
import org.folio.rest.core.models.RequestContext;

import java.util.concurrent.CompletableFuture;

public interface PatchOperationHandler {
  CompletableFuture<Void> handle(OrderLineUpdateInstanceHolder holder, RequestContext requestContext);
}
