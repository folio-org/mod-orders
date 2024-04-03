package org.folio.service.routinglists.validators;

import org.folio.rest.core.exceptions.ErrorCodes;
import org.folio.rest.jaxrs.model.Error;
import org.folio.rest.jaxrs.model.Location;
import org.folio.rest.jaxrs.model.PoLine;
import org.folio.rest.jaxrs.model.RoutingListCollection;

import java.util.ArrayList;
import java.util.List;

import static java.util.stream.Collectors.toList;

public class RoutingListValidatorUtil {

  public static List<Error> validateRoutingList(RoutingListCollection rListExisting, PoLine poLine) {
    List<ErrorCodes> errors = new ArrayList<>();
    if (!isPoLineFormatValid(poLine)) {
      errors.add(ErrorCodes.INVALID_ROUTING_LIST_FOR_PO_LINE_FORMAT);
    } else if (isRoutingListsLimitReached(rListExisting, poLine)) {
      errors.add(ErrorCodes.ROUTING_LIST_LIMIT_REACHED_FOR_PO_LINE);
    }
    return errors.stream().map(ErrorCodes::toError).collect(toList());
  }

  private static boolean isPoLineFormatValid(PoLine poLine) {
    return poLine.getOrderFormat() == PoLine.OrderFormat.PHYSICAL_RESOURCE
      || poLine.getOrderFormat() == PoLine.OrderFormat.P_E_MIX;
  }

  private static boolean isRoutingListsLimitReached(RoutingListCollection rListExisting, PoLine poLine) {
    return getQuantityPhysicalTotal(poLine) <= rListExisting.getTotalRecords();
  }

  private static int getQuantityPhysicalTotal(PoLine poLine) {
    return poLine.getLocations().stream().mapToInt(Location::getQuantityPhysical).sum();
  }

}
