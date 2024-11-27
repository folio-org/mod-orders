package org.folio.models;

import org.folio.rest.jaxrs.model.Piece;

import java.util.List;

public class ClaimingHolder {

  private List<Piece> pieces;

  public ClaimingHolder() {
  }

  public ClaimingHolder withPieces(List<Piece> pieces) {
    this.pieces = pieces;
    return this;
  }

  public List<Piece> getPieces() {
    return pieces;
  }
}
