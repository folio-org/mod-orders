package org.folio.models;

import java.util.Collection;
import java.util.List;
import java.util.Map;

import lombok.AllArgsConstructor;
import lombok.Builder;
import lombok.Data;
import lombok.With;
import org.folio.rest.jaxrs.model.PoLine;
import org.folio.rest.jaxrs.model.Title;

@Data
@Builder
@With
@AllArgsConstructor
public class TitleHolder {
  private Title title;
  private PoLine poLine;
  private Map<String, List<String>> holdingIdsToDeleteByTenant;
  private boolean isCentralEnabled;

  public TitleHolder(Title title) {
    this.title = title;
  }

  public List<String> getHoldingIdsToDelete() {
    return holdingIdsToDeleteByTenant.values().stream()
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
