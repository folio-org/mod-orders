package org.folio.models.pieces;

import org.folio.rest.jaxrs.model.Piece;

public class PieceDeletionHolder extends BasePieceFlowHolder {
  private Piece pieceToDelete;
  private boolean deleteHolding;

  public PieceDeletionHolder withDeleteHolding(boolean deleteHolding) {
    this.deleteHolding = deleteHolding;
    return this;
  }

  public boolean isDeleteHolding() {
    return deleteHolding;
  }

  public void setPieceToDelete(Piece pieceToDelete) {
    this.pieceToDelete = pieceToDelete;
  }

  public PieceDeletionHolder withPieceToDelete(Piece pieceToDelete) {
    this.pieceToDelete = pieceToDelete;
    return this;
  }

  public Piece getPieceToDelete() {
    return pieceToDelete;
  }

  @Override
  public String getOrderLineId() {
    return pieceToDelete.getPoLineId();
  }

  @Override
  public String getTitleId() {
    return pieceToDelete.getTitleId();
  }
}
