package org.folio.service.orders.lines.update.instance;

import org.apache.commons.lang.NotImplementedException;
import org.folio.rest.core.models.RequestContext;
import org.folio.service.orders.lines.update.OrderLineUpdateInstanceHolder;
import org.folio.service.orders.lines.update.OrderLineUpdateInstanceStrategy;

import java.util.concurrent.CompletableFuture;

public class WithoutHoldingOrderLineUpdateInstanceStrategy implements OrderLineUpdateInstanceStrategy {

  @Override public CompletableFuture<Void> updateInstance(OrderLineUpdateInstanceHolder holder, RequestContext rqContext) {
    throw new NotImplementedException();
  }
}
