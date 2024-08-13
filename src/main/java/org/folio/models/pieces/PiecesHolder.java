package org.folio.models.pieces;

import io.vertx.core.json.JsonObject;
import org.folio.rest.jaxrs.model.CheckInPiece;
import org.folio.rest.jaxrs.model.CompositePoLine;
import org.folio.rest.jaxrs.model.Piece;

import java.util.ArrayList;
import java.util.HashMap;
import java.util.List;
import java.util.Map;

public class PiecesHolder {
  private Map<String, List<Piece>> piecesFromStorage;
  private Map<String, PiecePoLineDto> itemsToRecreate;

  public static class PiecePoLineDto {
    private String poLineId;
    private CompositePoLine compositePoLine;
    private Piece pieceFromStorage;
    private CheckInPiece checkInPiece;

    public PiecePoLineDto(CompositePoLine compositePoLine, Piece pieceFromStorage, CheckInPiece checkInPiece) {
      this.poLineId = compositePoLine.getId();
      this.compositePoLine = compositePoLine;
      this.pieceFromStorage = pieceFromStorage;
      this.checkInPiece = checkInPiece;
    }

    public PiecePoLineDto(String poLineId, Piece pieceFromStorage) {
      this.poLineId = poLineId;
      this.pieceFromStorage = pieceFromStorage;
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
  }

  public Map<String, List<Piece>> getPiecesFromStorage() {
    return this.piecesFromStorage;
  }

  public Map<String, PiecePoLineDto> getItemsToRecreate() {
    return itemsToRecreate;
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

  public PiecesHolder withItemsToRecreate(Map<String, PiecePoLineDto> itemsToRecreate) {
    this.itemsToRecreate = itemsToRecreate;
    return this;
  }
}
