package org.folio.models;

import io.vertx.core.json.JsonObject;
import org.apache.commons.lang3.StringUtils;
import org.folio.rest.jaxrs.model.Piece;
import org.folio.rest.jaxrs.model.PoLine;

import java.util.HashMap;
import java.util.List;
import java.util.Map;
import java.util.Objects;

public class HoldingDetailAggregator {

  private Map<String, List<PoLine>> poLinesByHoldingId = new HashMap<>();
  private Map<String, List<Piece>> piecesByHoldingId = new HashMap<>();
  private Map<String, List<JsonObject>> itemsByHoldingId = new HashMap<>();

  public Map<String, List<PoLine>> getPoLinesByHoldingId() {
    return poLinesByHoldingId;
  }

  public Map<String, List<Piece>> getPiecesByHoldingId() {
    return piecesByHoldingId;
  }

  public Map<String, List<JsonObject>> getItemsByHoldingId() {
    return itemsByHoldingId;
  }

  public String getPieceTenantIdByItemId(String itemId) {
    return piecesByHoldingId.values().stream()
      .flatMap(List::stream)
      .filter(Objects::nonNull)
      .filter(piece -> StringUtils.equals(itemId, piece.getItemId()))
      .map(Piece::getReceivingTenantId)
      .filter(Objects::nonNull)
      .findFirst()
      .orElse(null);
  }

  public void setPoLinesByHoldingId(Map<String, List<PoLine>> poLines) {
    this.poLinesByHoldingId = poLines;
  }

  public void setPiecesByHoldingId(Map<String, List<Piece>> pieces) {
    this.piecesByHoldingId = pieces;
  }

  public void setItemsByHoldingId(Map<String, List<JsonObject>> items) {
    this.itemsByHoldingId = items;
  }
}
