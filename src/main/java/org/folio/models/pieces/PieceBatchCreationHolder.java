package org.folio.models.pieces;

import java.util.List;

import org.folio.rest.jaxrs.model.Piece;
import org.folio.rest.jaxrs.model.PieceCollection;

public class PieceBatchCreationHolder extends BasePieceFlowHolder {
  private List<Piece> piecesToCreate;
  private boolean createItem;

  public PieceBatchCreationHolder withCreateItem(boolean createItem) {
    this.createItem = createItem;
    return this;
  }

  public boolean isCreateItem() {
    return createItem;
  }

  public PieceBatchCreationHolder withPieceToCreate(List<Piece> piecesToCreate) {
    this.piecesToCreate = piecesToCreate;
    return this;
  }

  public PieceBatchCreationHolder withPieceToCreate(PieceCollection pieceCollection) {
    this.piecesToCreate = pieceCollection.getPieces();
    return this;
  }

  public List<Piece> getPiecesToCreate() {
    return piecesToCreate;
  }

  @Override
  public String getOrderLineId() {
    if (piecesToCreate.isEmpty()) {
      return null;
    }
    return piecesToCreate.get(0).getPoLineId();
  }

  @Override
  public String getTitleId() {
    if (piecesToCreate.isEmpty()) {
      return null;
    }
    return piecesToCreate.get(0).getTitleId();
  }
}
