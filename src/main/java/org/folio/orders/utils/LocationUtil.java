package org.folio.orders.utils;

import java.util.ArrayDeque;
import java.util.ArrayList;
import java.util.Deque;
import java.util.List;
import java.util.Map;
import java.util.stream.Collectors;

import org.folio.models.PoLineUpdateHolder;
import org.folio.rest.jaxrs.model.Location;

public final class LocationUtil {
  private LocationUtil() {

  }

  public static Map<String, Integer> groupLocationQtyByLocationId(List<Location> locations) {
    return locations.stream().collect(Collectors.groupingBy(Location::getLocationId, Collectors.summingInt(Location::getQuantity)));
  }

  public static List<PoLineUpdateHolder> convertToOldNewLocationIdPair(List<Location> newLocations, List<Location> oldLocations) {
    Map<String, Integer> newLocationQty = LocationUtil.groupLocationQtyByLocationId(newLocations);
    Map<String, Integer> storageLocationQty = LocationUtil.groupLocationQtyByLocationId(oldLocations);
    int newQty = newLocations.stream().map(Location::getQuantity).mapToInt(Number::intValue).sum();
    int storeQty = oldLocations.stream().map(Location::getQuantity).mapToInt(Number::intValue).sum();
    List<PoLineUpdateHolder> poLineUpdateHolders = new ArrayList<>();
    if (newQty == storeQty) {
      Deque<String> oldLocationIds = new ArrayDeque<>(storageLocationQty.keySet());
      List<String> newLocationIds = new ArrayList<>(newLocationQty.keySet());
      String oldLocationId = oldLocationIds.poll();
      for (int i = 0; i < newLocationQty.values().size(); i++) {
        String newLocationId = newLocationIds.get(i);
        if (oldLocationId != null && !oldLocationId.equals(newLocationId)) {
          poLineUpdateHolders.add(new PoLineUpdateHolder().withOldLocationId(oldLocationId).withNewLocationId(newLocationId));
        }
        if (oldLocationIds.size() == 1) {
          oldLocationId = oldLocationIds.peek();
        } else {
          oldLocationId = oldLocationIds.poll();
        }
      }
    }
    return poLineUpdateHolders;
  }
}
