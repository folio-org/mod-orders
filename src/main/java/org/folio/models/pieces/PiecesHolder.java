package org.folio.models.pieces;

import io.vertx.core.json.JsonObject;
import org.apache.commons.lang3.tuple.Pair;
import org.folio.rest.jaxrs.model.CheckInPiece;
import org.folio.rest.jaxrs.model.CompositePoLine;
import org.folio.rest.jaxrs.model.CompositePurchaseOrder;
import org.folio.rest.jaxrs.model.Piece;

import java.util.ArrayList;
import java.util.HashMap;
import java.util.List;
import java.util.Map;

public class PiecesHolder {
  private Pair<CompositePurchaseOrder, CompositePoLine> purchaseOrderPoLinePair;
  private Map<String, List<Piece>> piecesFromStorage;
  private Map<String, List<PiecePoLineDto>> itemsToRecreate;

  public static class PiecePoLineDto {
    private final String poLineId;
    private final CompositePoLine compositePoLine;
    private final Piece pieceFromStorage;
    private final CheckInPiece checkInPiece;
    private final boolean recreateItem;

    public PiecePoLineDto(CompositePoLine compositePoLine, Piece pieceFromStorage, CheckInPiece checkInPiece) {
      this.poLineId = compositePoLine.getId();
      this.compositePoLine = compositePoLine;
      this.pieceFromStorage = pieceFromStorage;
      this.checkInPiece = checkInPiece;
      this.recreateItem = true;
    }

    public PiecePoLineDto(String poLineId, Piece pieceFromStorage) {
      this.poLineId = poLineId;
      this.compositePoLine = null;
      this.pieceFromStorage = pieceFromStorage;
      this.checkInPiece = null;
      this.recreateItem = false;
    }

    public String getPoLineId() {
      return poLineId;
    }

    public Piece getPieceFromStorage() {
      return pieceFromStorage;
    }

    public CompositePoLine getCompositePoLine() {
      return compositePoLine;
    }

    public CheckInPiece getCheckInPiece() {
      return checkInPiece;
    }

    public boolean isRecreateItem() {
      return this.recreateItem;
    }
  }

  public Pair<CompositePurchaseOrder, CompositePoLine> getPurchaseOrderPoLinePair() {
    return purchaseOrderPoLinePair;
  }

  public Map<String, List<Piece>> getPiecesFromStorage() {
    return this.piecesFromStorage;
  }

  public Map<String, List<PiecePoLineDto>> getItemsToRecreate() {
    return this.itemsToRecreate;
  }

  public PiecesHolder withPurchaseOrderPoLinePair(Pair<CompositePurchaseOrder, CompositePoLine> purchaseOrderPoLinePair) {
    this.purchaseOrderPoLinePair = purchaseOrderPoLinePair;
    return this;
  }

  public PiecesHolder withPiecesFromStorage(Map<String, List<Piece>> piecesFromStorage) {
    Map<String, List<Piece>> copy = new HashMap<>();
    for (Map.Entry<String, List<Piece>> entry : piecesFromStorage.entrySet()) {
      List<Piece> pieces = new ArrayList<>();
      for (Piece piece : entry.getValue()) {
        // create copies
        pieces.add(JsonObject.mapFrom(piece).mapTo(Piece.class));
      }
      copy.put(entry.getKey(), pieces);
    }

    this.piecesFromStorage = copy;
    return this;
  }

  public PiecesHolder withItemsToRecreate(Map<String, List<PiecePoLineDto>> itemsToRecreate) {
    this.itemsToRecreate = itemsToRecreate;
    return this;
  }
}
