package org.folio.models.pieces;

import org.folio.rest.jaxrs.model.Piece;

public class PieceCreationHolder extends BasePieceFlowHolder {
  private Piece pieceToCreate;
  private boolean createItem;

  public PieceCreationHolder withCreateItem(boolean createItem) {
    this.createItem = createItem;
    return this;
  }

  public boolean isCreateItem() {
    return createItem;
  }

  public PieceCreationHolder withPieceToCreate(Piece pieceToCreate) {
    this.pieceToCreate = pieceToCreate;
    return this;
  }

  public Piece getPieceToCreate() {
    return pieceToCreate;
  }

  @Override
  public String getOrderLineId() {
    return pieceToCreate.getPoLineId();
  }

  @Override
  public String getTitleId() {
    return pieceToCreate.getTitleId();
  }
}
