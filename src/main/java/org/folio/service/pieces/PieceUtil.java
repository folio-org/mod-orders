package org.folio.service.pieces;

import org.folio.rest.jaxrs.model.CompositePoLine;
import org.folio.rest.jaxrs.model.Eresource;
import org.folio.rest.jaxrs.model.Physical;
import org.folio.rest.jaxrs.model.Piece;

public class PieceUtil {
  private PieceUtil(){

  }

  public boolean isUpdateInventoryNeeded(Piece.Format pieceFormat, CompositePoLine compositePoLine) {
    if (pieceFormat == Piece.Format.ELECTRONIC) {

    } else if (pieceFormat == Piece.Format.PHYSICAL) {

    }
    return false;
  }
//
//  public static boolean isUpdateNotRequiredForEresource(CompositePoLine compPOL) {
//    return compPOL.getEresource() == null || compPOL.getEresource().getCreateInventory() == Eresource.CreateInventory.NONE;
//  }
//
//  public static boolean isUpdateNotRequiredForPhysical(CompositePoLine compPOL) {
//    return compPOL.getPhysical() == null || compPOL.getPhysical().getCreateInventory() == Physical.CreateInventory.NONE;
//  }
}
