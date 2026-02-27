package org.folio.models;

import org.folio.rest.jaxrs.model.Piece;

import java.util.List;
import java.util.Map;
import java.util.Set;

public record HoldingUpdateResult(Map<String, List<Piece>> pieces,
                                  Set<String> processedHoldingIds) {
}
