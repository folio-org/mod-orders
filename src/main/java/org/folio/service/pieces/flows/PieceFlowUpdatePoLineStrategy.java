package org.folio.service.pieces.flows;

import org.folio.rest.jaxrs.model.CompositePoLine;
import org.folio.rest.jaxrs.model.Piece;

public interface PieceFlowUpdatePoLineStrategy {
  void updateQuantity(int qty, Piece piece, CompositePoLine lineToSave);
}
