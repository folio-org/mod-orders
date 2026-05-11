package org.folio.service.pieces.flows;

public interface PoLineUpdateQuantityService<T> {
  boolean poLineUpdateCost(T holder);
  boolean poLineUpdateLocations(T holder);
}
