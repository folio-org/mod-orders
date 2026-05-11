package org.folio.models;

import java.util.Set;

public record HoldingDataExclusionConfig(HoldingDataExclusionMode mode,
                                         Set<String> excludePoLinesIds,
                                         Set<String> pendingPoLineIds,
                                         Set<String> excludePieceIds) {

  public HoldingDataExclusionConfig(HoldingDataExclusionMode mode,
                             Set<String> excludePoLinesIds,
                             Set<String> pendingPoLineIds) {
    this(mode, excludePoLinesIds, pendingPoLineIds, Set.of());
  }
}
