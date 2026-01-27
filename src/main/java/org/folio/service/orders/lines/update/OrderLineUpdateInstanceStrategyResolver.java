package org.folio.service.orders.lines.update;

import java.util.Map;

import org.folio.rest.jaxrs.model.CreateInventoryType;

public class OrderLineUpdateInstanceStrategyResolver {

  private final Map<CreateInventoryType, OrderLineUpdateInstanceStrategy> strategies;

  public OrderLineUpdateInstanceStrategyResolver(Map<CreateInventoryType, OrderLineUpdateInstanceStrategy> strategies) {
    this.strategies = strategies;
  }

  public OrderLineUpdateInstanceStrategy resolve(CreateInventoryType opType) {
    return this.strategies.get(opType);
  }
}
