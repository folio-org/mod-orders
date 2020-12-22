package org.folio.models;

import org.folio.rest.acq.model.Piece;

import io.vertx.core.json.JsonObject;

public class PieceItemPair {
  private Piece piece;
  private JsonObject item;

  public Piece getPiece() {
    return piece;
  }

  public JsonObject getItem() {
    return item;
  }

  public PieceItemPair withPiece(Piece piece) {
    this.piece = piece;
    return this;
  }

  public PieceItemPair withItem(JsonObject item) {
    this.item = item;
    return this;
  }
}
