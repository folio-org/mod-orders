package org.folio.service.orders.lines.update.instance;

import org.folio.service.orders.lines.update.OrderLineUpdateInstanceStrategy;
import org.apache.commons.lang.NotImplementedException;
import org.folio.rest.core.models.RequestContext;
import org.folio.service.orders.lines.update.OrderLineUpdateInstanceHolder;

import java.util.concurrent.CompletableFuture;

public class WithHoldingOrderLineUpdateInstanceStrategy implements OrderLineUpdateInstanceStrategy {

  @Override
  public CompletableFuture<Void> updateInstance(OrderLineUpdateInstanceHolder holder, RequestContext rqContext) {
    throw new NotImplementedException();
  }
}
