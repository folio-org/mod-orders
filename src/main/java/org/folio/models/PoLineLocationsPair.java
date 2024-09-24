package org.folio.models;

import org.folio.rest.jaxrs.model.Location;
import org.folio.rest.jaxrs.model.PoLine;

import java.util.List;

public class PoLineLocationsPair {
  private final PoLine poLine;
  private final List<Location> locations;

  private PoLineLocationsPair(PoLine poLine, List<Location> locationIds) {
    this.poLine = poLine;
    this.locations = locationIds;
  }

  public static PoLineLocationsPair of(PoLine poLine, List<Location> locationIds) {
    return new PoLineLocationsPair(poLine, locationIds);
  }

  public PoLine getPoLine() {
    return poLine;
  }

  public List<Location> getLocations() {
    return locations;
  }

}
