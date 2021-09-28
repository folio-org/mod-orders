package org.folio.service.pieces.flows;

import java.util.Map;
import java.util.Optional;

public class PieceFlowUpdatePoLineStrategyResolver {
  private final Map<PieceFlowUpdatePoLineKey, PieceFlowUpdatePoLineStrategy> strategies;

  public PieceFlowUpdatePoLineStrategyResolver(Map<PieceFlowUpdatePoLineKey, PieceFlowUpdatePoLineStrategy> strategies) {
    this.strategies = strategies;
  }

  public Optional<PieceFlowUpdatePoLineStrategy> resolve(PieceFlowUpdatePoLineKey key) {
    return Optional.ofNullable(strategies.get(key));
  }
}
