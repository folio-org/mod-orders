package org.folio.models;

import java.util.Set;

public record HoldingDataExclusionConfig(HoldingDataExclusionMode mode,
                                         Set<String> excludePoLinesIds,
                                         Set<String> pendingPoLineIds,
                                         Set<String> excludePieceIds) {

  // Must only be used by PURCHASE_ORDER_UNOPEN mode, because it is not concerned with Pieces
  public HoldingDataExclusionConfig(HoldingDataExclusionMode mode,
                                    Set<String> excludePoLinesIds,
                                    Set<String> pendingPoLineIds) {
    this(mode, excludePoLinesIds, pendingPoLineIds, Set.of());
  }
}
