package org.folio.service.pieces;

import org.folio.rest.jaxrs.model.CompositePoLine;
import org.folio.rest.jaxrs.model.Piece;

public class PieceUtil {
  private PieceUtil(){

  }

  public boolean isUpdateInventoryNeeded(Piece.Format pieceFormat) {
    if (pieceFormat == Piece.Format.ELECTRONIC) {

    } else if (pieceFormat == Piece.Format.PHYSICAL) {

    }
    return false;
  }
}
