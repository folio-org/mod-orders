package org.folio.models.pieces;

import io.vertx.core.json.JsonObject;
import org.apache.commons.lang3.tuple.Pair;
import org.folio.rest.jaxrs.model.CheckInPiece;
import org.folio.rest.jaxrs.model.PoLine;
import org.folio.rest.jaxrs.model.CompositePurchaseOrder;
import org.folio.rest.jaxrs.model.Piece;

import java.util.ArrayList;
import java.util.HashMap;
import java.util.HashSet;
import java.util.List;
import java.util.Map;
import java.util.Set;

public class PiecesHolder {

  private Pair<CompositePurchaseOrder, PoLine> purchaseOrderPoLinePair;
  private Map<String, List<Piece>> piecesFromStorage;
  private Map<String, List<PiecePoLineDto>> itemsToRecreate;
  private final Set<String> processedHoldingIds = new HashSet<>();

  public static class PiecePoLineDto {
    private final String poLineId;
    private final PoLine poLine;
    private final Piece pieceFromStorage;
    private final CheckInPiece checkInPiece;
    private final boolean recreateItem;

    public PiecePoLineDto(PoLine poLine, Piece pieceFromStorage, CheckInPiece checkInPiece) {
      this.poLineId = poLine.getId();
      this.poLine = poLine;
      this.pieceFromStorage = pieceFromStorage;
      this.checkInPiece = checkInPiece;
      this.recreateItem = true;
    }

    public PiecePoLineDto(String poLineId, Piece pieceFromStorage) {
      this.poLineId = poLineId;
      this.poLine = null;
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

    public PoLine getPoLine() {
      return poLine;
    }

    public CheckInPiece getCheckInPiece() {
      return checkInPiece;
    }

    public boolean isRecreateItem() {
      return this.recreateItem;
    }
  }

  public Pair<CompositePurchaseOrder, PoLine> getPurchaseOrderPoLinePair() {
    return purchaseOrderPoLinePair;
  }

  public Map<String, List<Piece>> getPiecesFromStorage() {
    return this.piecesFromStorage;
  }

  public Map<String, List<PiecePoLineDto>> getItemsToRecreate() {
    return this.itemsToRecreate;
  }

  public Set<String> getProcessedHoldingIds() {
    return processedHoldingIds;
  }

  public PiecesHolder withPurchaseOrderPoLinePair(Pair<CompositePurchaseOrder, PoLine> purchaseOrderPoLinePair) {
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
