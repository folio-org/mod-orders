package org.folio.service.pieces.flows.strategies;

import java.util.Map;

public class Resolver {

  private final Map<String, ProcessInventoryStrategy> strategy;

  public Resolver(Map<String, ProcessInventoryStrategy> strategy) {
    this.strategy = strategy;
  }

  public ProcessInventoryStrategy getHoldingAndItemStrategy(String key) {
    return this.strategy.get(key);
  }
}
