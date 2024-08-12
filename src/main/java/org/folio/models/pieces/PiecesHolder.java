package org.folio.models.pieces;

import io.vertx.core.json.JsonObject;
import org.folio.rest.jaxrs.model.Piece;

import java.util.ArrayList;
import java.util.HashMap;
import java.util.List;
import java.util.Map;

public class PiecesHolder {
  private Map<String, List<Piece>> piecesFromStorage;

  public Map<String, List<Piece>> getPiecesFromStorage() {
    return piecesFromStorage;
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
}
