package org.folio.models;

import java.util.Collection;
import java.util.List;
import java.util.Map;

import lombok.Data;
import lombok.experimental.Accessors;
import org.folio.rest.jaxrs.model.Piece;
import org.folio.rest.jaxrs.model.PoLine;
import org.folio.rest.jaxrs.model.Title;

@Data
@Accessors(chain = true)
public class TitleHolder {

  private Title title;
  private PoLine poLine;
  private List<Piece> pieces;
  private Map<String, List<String>> holdingIdsToDeleteByTenant;
  private Map<String, List<String>> allHoldingIdsByTenant;
  private boolean isCentralEnabled;
  private boolean deleteHoldings;

  public TitleHolder(Title title) {
    this.title = title;
  }

  public List<String> getHoldingIdsToDelete() {
    return holdingIdsToDeleteByTenant.values().stream()
      .flatMap(Collection::stream)
      .distinct()
      .toList();
  }

  public List<String> getAllHoldings() {
    return allHoldingIdsByTenant.values().stream()
      .flatMap(Collection::stream)
      .distinct()
      .toList();
  }

  public String getTitleId() {
    return title.getId();
  }

  public String getPoLineId() {
    return poLine.getId();
  }

}
