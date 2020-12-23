package org.folio.orders.utils;

import java.util.ArrayList;
import java.util.List;
import java.util.Map;
import java.util.stream.Collectors;
import java.util.stream.IntStream;

import org.apache.commons.collections4.CollectionUtils;
import org.apache.commons.lang3.StringUtils;
import org.folio.models.PoLineUpdateHolder;
import org.folio.rest.acq.model.Piece;
import org.folio.rest.jaxrs.model.Location;
import static java.util.stream.Collectors.toList;
import static java.util.stream.Collectors.toMap;

public final class LocationUtil {
  private LocationUtil() {

  }

  public static Map<String, Integer> groupLocationQtyByLocationId(List<Location> locations) {
    return locations.stream().collect(Collectors.groupingBy(Location::getLocationId, Collectors.summingInt(Location::getQuantity)));
  }

  public static List<PoLineUpdateHolder> convertToOldNewLocationIdPair(List<Location> newLocations, List<Location> oldLocations) {

    Map<String, Integer> newLocationQty = LocationUtil.groupLocationQtyByLocationId(newLocations);
    Map<String, Integer> storageLocationQty = LocationUtil.groupLocationQtyByLocationId(oldLocations);
    int newLocationsQty = newLocations.stream().map(Location::getQuantity).mapToInt(Number::intValue).sum();
    int storeLocationsQty = oldLocations.stream().map(Location::getQuantity).mapToInt(Number::intValue).sum();
    List<PoLineUpdateHolder> poLineUpdateHolders = new ArrayList<>();
    if (newLocationsQty == storeLocationsQty && newLocationQty.size() == storageLocationQty.size()) {
      List<String> originNewLocationIds = newLocations.stream().map(Location::getLocationId).collect(toList());
      originNewLocationIds.retainAll(storageLocationQty.keySet());
      if (!originNewLocationIds.isEmpty()) {
        newLocationQty.entrySet().removeIf(e -> originNewLocationIds.contains(e.getKey()));
        storageLocationQty.entrySet().removeIf(e -> originNewLocationIds.contains(e.getKey()));
      }
      List<String> remNewLocationIds = new ArrayList<>(newLocationQty.keySet());
      List<String> remOldLocationIds = new ArrayList<>(storageLocationQty.keySet());
      IntStream.range(0, newLocationQty.size()).forEach(i -> {
        String newLocationId = remNewLocationIds.get(i);
        String oldLocationId = remOldLocationIds.get(i);
        PoLineUpdateHolder holder = new PoLineUpdateHolder().withNewLocationId(newLocationId).withOldLocationId(oldLocationId);
        poLineUpdateHolders.add(holder);
      });
    }
    return poLineUpdateHolders;
  }
}
