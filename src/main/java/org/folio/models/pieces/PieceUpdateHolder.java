package org.folio.models.pieces;

import org.folio.rest.jaxrs.model.Piece;

public class PieceUpdateHolder extends BasePieceFlowHolder {
  private boolean createItem;
  private boolean deleteHolding;
  private String instanceId;
  private Piece pieceToUpdate;
  private Piece pieceFromStorage;

  public PieceUpdateHolder withPieceFromStorage(Piece pieceFromStorage) {
    this.pieceFromStorage = pieceFromStorage;
    return this;
  }

  public Piece getPieceFromStorage() {
    return pieceFromStorage;
  }

  public PieceUpdateHolder withPieceToUpdate(Piece pieceToUpdate) {
    this.pieceToUpdate = pieceToUpdate;
    return this;
  }

  public Piece getPieceToUpdate() {
    return pieceToUpdate;
  }

  public PieceUpdateHolder withDeleteHolding(boolean deleteHolding) {
    this.deleteHolding = deleteHolding;
    return this;
  }

  public boolean isDeleteHolding() {
    return deleteHolding;
  }

  public PieceUpdateHolder withCreateItem(boolean createItem) {
    this.createItem = createItem;
    return this;
  }

  public boolean isCreateItem() {
    return createItem;
  }

  public PieceUpdateHolder withInstanceId(String instanceId) {
    this.instanceId = instanceId;
    return this;
  }

  public String getInstanceId() {
    return this.instanceId;
  }

  @Override
  public String getOrderLineId() {
    return pieceToUpdate.getPoLineId();
  }

  @Override
  public String getTitleId() {
    return pieceToUpdate.getTitleId();
  }
}
