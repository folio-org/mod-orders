package org.folio.models;

import io.vertx.core.json.JsonObject;
import lombok.Data;
import org.apache.commons.lang3.StringUtils;
import org.folio.rest.jaxrs.model.Piece;
import org.folio.rest.jaxrs.model.PoLine;

import java.util.HashMap;
import java.util.List;
import java.util.Map;
import java.util.Objects;

@Data
public class HoldingDetailAggregator {

  private String tenant;
  private Map<String, List<PoLine>> poLinesByHoldingId = new HashMap<>();
  private Map<String, List<Piece>> piecesByHoldingId = new HashMap<>();
  private Map<String, List<JsonObject>> itemsByHoldingId = new HashMap<>();

  public String getPieceTenantIdByItemId(String itemId) {
    if (Objects.isNull(itemId)) {
      return null;
    }
    return piecesByHoldingId.values().stream()
      .flatMap(List::stream)
      .filter(Objects::nonNull)
      .filter(piece -> StringUtils.equals(itemId, piece.getItemId()))
      .map(Piece::getReceivingTenantId)
      .filter(Objects::nonNull)
      .findFirst()
      .orElse(null);
  }
}
