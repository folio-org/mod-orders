package org.folio.service.orders.lines.update;

import org.folio.rest.jaxrs.model.CreateInventoryType;

import java.util.Map;

public class OrderLineUpdateInstanceStrategyResolver {
  private final Map<CreateInventoryType, OrderLineUpdateInstanceStrategy> strategies;

  public OrderLineUpdateInstanceStrategyResolver(Map<CreateInventoryType, OrderLineUpdateInstanceStrategy> strategies) {
    this.strategies = strategies;
  }

  public OrderLineUpdateInstanceStrategy resolve(CreateInventoryType opType) {
    return this.strategies.get(opType);
  }
}
