package org.folio.orders.utils;

import static org.folio.orders.utils.HelperUtils.calculateTotalLocationQuantity;
import static org.folio.rest.jaxrs.model.CompositePoLine.OrderFormat.ELECTRONIC_RESOURCE;
import static org.folio.rest.jaxrs.model.CompositePoLine.OrderFormat.OTHER;
import static org.folio.rest.jaxrs.model.CompositePoLine.OrderFormat.P_E_MIX;
import static org.folio.rest.jaxrs.model.CompositePoLine.OrderFormat.PHYSICAL_RESOURCE;

import java.util.Collections;
import java.util.List;
import java.util.Map;
import java.util.Objects;
import java.util.Optional;
import java.util.stream.Collectors;

import org.apache.commons.collections4.CollectionUtils;
import org.apache.commons.lang3.ObjectUtils;
import org.folio.rest.jaxrs.model.CompositePoLine;
import org.folio.rest.jaxrs.model.Eresource;
import org.folio.rest.jaxrs.model.Location;
import org.folio.rest.jaxrs.model.Physical;
import org.folio.rest.jaxrs.model.PoLine;

import io.vertx.core.json.JsonObject;

public final class PoLineCommonUtil {
  public static final String DASH_SEPARATOR = "-";

  private PoLineCommonUtil() {

  }

  public static void sortPoLinesByPoLineNumber(List<CompositePoLine> poLines) {
    poLines.sort(PoLineCommonUtil::comparePoLinesByPoLineNumber);
  }

  public static boolean isReceiptNotRequired(CompositePoLine.ReceiptStatus receiptStatus) {
    return receiptStatus == CompositePoLine.ReceiptStatus.RECEIPT_NOT_REQUIRED;
  }

  public static boolean isInventoryUpdateNotRequired(CompositePoLine compPOL) {
    // in case of "Other" order format check Physical createInventory value only
    if (compPOL.getOrderFormat() == OTHER || compPOL.getOrderFormat() == PHYSICAL_RESOURCE) {
      return isUpdateNotRequiredForPhysical(compPOL);
    } else if (compPOL.getOrderFormat() == ELECTRONIC_RESOURCE) {
      return isUpdateNotRequiredForEresource(compPOL);
    } else {
      return isUpdateNotRequiredForPhysical(compPOL) && isUpdateNotRequiredForEresource(compPOL);
    }
  }

  public static boolean isUpdateNotRequiredForEresource(CompositePoLine compPOL) {
    return compPOL.getEresource() == null || compPOL.getEresource().getCreateInventory() == Eresource.CreateInventory.NONE;
  }

  public static boolean isUpdateNotRequiredForPhysical(CompositePoLine compPOL) {
    return compPOL.getPhysical() == null || compPOL.getPhysical().getCreateInventory() == Physical.CreateInventory.NONE;
  }

  public static boolean isHoldingsUpdateRequired(CompositePoLine compPOL) {
    return isHoldingUpdateRequiredForPhysical(compPOL) || isHoldingUpdateRequiredForEresource(compPOL);
  }

  public static boolean isHoldingUpdateRequiredForPhysical(CompositePoLine compPOL) {
    CompositePoLine.OrderFormat format = compPOL.getOrderFormat();
    if (!(format == PHYSICAL_RESOURCE || format == OTHER || format == P_E_MIX))
      return false;
    Physical physical = compPOL.getPhysical();
    return physical != null && (physical.getCreateInventory() == Physical.CreateInventory.INSTANCE_HOLDING
      || physical.getCreateInventory() == Physical.CreateInventory.INSTANCE_HOLDING_ITEM);
  }

  public static boolean isHoldingUpdateRequiredForEresource(CompositePoLine compPOL) {
    CompositePoLine.OrderFormat format = compPOL.getOrderFormat();
    if (!(format == ELECTRONIC_RESOURCE || format == P_E_MIX))
      return false;
    Eresource eresource = compPOL.getEresource();
    return eresource != null && (eresource.getCreateInventory() == Eresource.CreateInventory.INSTANCE_HOLDING
      || eresource.getCreateInventory() == Eresource.CreateInventory.INSTANCE_HOLDING_ITEM);
  }

  public static boolean isItemsUpdateRequired(CompositePoLine compPOL) {
    return isItemsUpdateRequiredForPhysical(compPOL) || isItemsUpdateRequiredForEresource(compPOL);
  }

  public static boolean isItemsUpdateRequiredForEresource(CompositePoLine compPOL) {
    CompositePoLine.OrderFormat format = compPOL.getOrderFormat();
    if (!(format == ELECTRONIC_RESOURCE || format == P_E_MIX))
      return false;
    if (compPOL.getCheckinItems() != null && compPOL.getCheckinItems()) {
      return false;
    }
    return Optional.ofNullable(compPOL.getEresource())
      .map(eresource -> eresource.getCreateInventory() == Eresource.CreateInventory.INSTANCE_HOLDING_ITEM)
      .orElse(false);
  }

  public static boolean isItemsUpdateRequiredForPhysical(CompositePoLine compPOL) {
    CompositePoLine.OrderFormat format = compPOL.getOrderFormat();
    if (!(format == PHYSICAL_RESOURCE || format == OTHER || format == P_E_MIX))
      return false;
    if (compPOL.getCheckinItems() != null && compPOL.getCheckinItems()) {
      return false;
    }
    return Optional.ofNullable(compPOL.getPhysical())
      .map(physical -> physical.getCreateInventory() == Physical.CreateInventory.INSTANCE_HOLDING_ITEM)
      .orElse(false);
  }

  public static boolean isOnlyInstanceUpdateRequired(CompositePoLine compPOL) {
    CompositePoLine.OrderFormat format = compPOL.getOrderFormat();
    boolean checkPhysical = format == PHYSICAL_RESOURCE || format == OTHER || format == P_E_MIX;
    boolean checkElectronic = format == ELECTRONIC_RESOURCE || format == P_E_MIX;

    boolean isPhysicalInstance = checkPhysical && Optional.ofNullable(compPOL.getPhysical())
      .map(physical -> physical.getCreateInventory() == Physical.CreateInventory.INSTANCE)
      .orElse(false);

    boolean isElectronicInstance = checkElectronic && Optional.ofNullable(compPOL.getEresource())
      .map(elec -> elec.getCreateInventory() == Eresource.CreateInventory.INSTANCE)
      .orElse(false);

    return isPhysicalInstance || isElectronicInstance;
  }

  public static boolean isHoldingCreationRequiredForLocation(CompositePoLine compPOL, Location location) {
    return (isHoldingUpdateRequiredForPhysical(compPOL) && ObjectUtils.defaultIfNull(location.getQuantityPhysical(), 0) > 0)
      || (isHoldingUpdateRequiredForEresource(compPOL) && ObjectUtils.defaultIfNull(location.getQuantityElectronic(), 0) > 0);
  }

  private static int comparePoLinesByPoLineNumber(CompositePoLine poLine1, CompositePoLine poLine2) {
    String n1 = poLine1.getPoLineNumber();
    String n2 = poLine2.getPoLineNumber();
    if (n1 == null || n2 == null)
      return 0;
    String poLineNumberSuffix1 = n1.split(DASH_SEPARATOR)[1];
    String poLineNumberSuffix2 = n2.split(DASH_SEPARATOR)[1];
    return Integer.parseInt(poLineNumberSuffix1) - Integer.parseInt(poLineNumberSuffix2);
  }

  /**
   * Group all PO Line's locations for which the holding should be created by location identifier
   * @param compPOL PO line with locations to group
   * @return map of grouped locations where key is location id and value is list of locations with the same id
   */
  public static Map<String, List<Location>> groupLocationsByLocationId(CompositePoLine compPOL) {
    if (CollectionUtils.isEmpty(compPOL.getLocations())) {
      return Collections.emptyMap();
    }

    return compPOL.getLocations()
                  .stream()
                  .filter(location -> Objects.nonNull(location.getLocationId()))
                  .filter(location -> !isHoldingCreationRequiredForLocation(compPOL, location))
                  .collect(Collectors.groupingBy(Location::getLocationId));
  }

  /**
   * Group all PO Line's locations for which the holding should be created by location identifier
   * @param compPOL PO line with locations to group
   * @return map of grouped locations where key is holding id and value is list of locations with the same id
   */
  public static Map<String, List<Location>> groupLocationsByHoldingId(CompositePoLine compPOL) {
    if (CollectionUtils.isEmpty(compPOL.getLocations())) {
      return Collections.emptyMap();
    }

    return compPOL.getLocations()
      .stream()
      .filter(location -> Objects.nonNull(location.getHoldingId()))
      .filter(location -> isHoldingCreationRequiredForLocation(compPOL, location))
      .collect(Collectors.groupingBy(Location::getHoldingId));
  }

  public static CompositePoLine convertToCompositePoLine(PoLine poLine) {
    poLine.setAlerts(null);
    poLine.setReportingCodes(null);
    JsonObject jsonLine = JsonObject.mapFrom(poLine);
    return jsonLine.mapTo(CompositePoLine.class);
  }

  public static void makePoLinesPending(List<CompositePoLine> compositePoLines) {
    compositePoLines.forEach(HelperUtils::makePoLinePending);
  }

  public static void updateLocationsQuantity(List<Location> locations) {
    locations.forEach(location -> location.setQuantity(calculateTotalLocationQuantity(location)));
  }
}
