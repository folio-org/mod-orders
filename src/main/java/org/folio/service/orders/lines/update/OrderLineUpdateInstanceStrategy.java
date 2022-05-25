package org.folio.service.orders.lines.update;

import org.folio.rest.core.models.RequestContext;

import java.util.concurrent.CompletableFuture;

public interface OrderLineUpdateInstanceStrategy {
  CompletableFuture<Void> updateInstance(OrderLineUpdateInstanceHolder holder, RequestContext rqContext);
}
