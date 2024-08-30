package org.folio.service.routinglists.validators;

import java.util.ArrayList;
import java.util.List;
import java.util.Optional;

import org.folio.rest.core.exceptions.ErrorCodes;
import org.folio.rest.jaxrs.model.Error;
import org.folio.rest.jaxrs.model.PoLine;
import org.folio.rest.jaxrs.model.RoutingList;
import org.folio.rest.jaxrs.model.RoutingListCollection;

public class RoutingListValidatorUtil {

  private RoutingListValidatorUtil() { }

  public static List<Error> validateRoutingList(RoutingList routingList, RoutingListCollection rListExisting, PoLine poLine) {
    List<ErrorCodes> errors = new ArrayList<>();
    if (poLine == null) {
      errors.add(ErrorCodes.PO_LINE_NOT_FOUND_FOR_ROUTING_LIST);
    } else if (!isPoLineFormatValid(poLine)) {
      errors.add(ErrorCodes.INVALID_ROUTING_LIST_FOR_PO_LINE_FORMAT);
    } else if (isRoutingListsLimitReached(routingList, rListExisting, poLine)) {
      errors.add(ErrorCodes.ROUTING_LIST_LIMIT_REACHED_FOR_PO_LINE);
    }
    return errors.stream().map(ErrorCodes::toError).toList();
  }

  private static boolean isPoLineFormatValid(PoLine poLine) {
    return poLine.getOrderFormat() == PoLine.OrderFormat.PHYSICAL_RESOURCE
      || poLine.getOrderFormat() == PoLine.OrderFormat.P_E_MIX;
  }

  private static boolean isRoutingListsLimitReached(RoutingList routingList, RoutingListCollection rListExisting, PoLine poLine) {
    return getQuantityPhysicalTotal(poLine) <= extractDifferentRoutingLists(routingList, rListExisting).size();
  }

  private static int getQuantityPhysicalTotal(PoLine poLine) {
    return poLine.getLocations()
      .stream()
      .mapToInt(loc -> Optional.ofNullable(loc.getQuantityPhysical()).orElse(0))
      .sum();
  }

  private static List<RoutingList> extractDifferentRoutingLists(RoutingList routingList, RoutingListCollection routingListCollection) {
    return routingListCollection.getRoutingLists()
      .stream()
      .filter(lst -> !lst.getId().equals(routingList.getId()))
      .toList();
  }

}
