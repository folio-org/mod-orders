package org.folio.orders.utils;

import static org.apache.commons.lang3.ObjectUtils.defaultIfNull;
import static org.folio.orders.utils.HelperUtils.calculateTotalLocationQuantity;
import static org.folio.rest.core.exceptions.ErrorCodes.PROHIBITED_FIELD_CHANGING;
import static org.folio.rest.core.exceptions.ErrorCodes.WRONG_ONGOING_NOT_SUBSCRIPTION_FIELDS_CHANGED;
import static org.folio.rest.core.exceptions.ErrorCodes.WRONG_ONGOING_SUBSCRIPTION_FIELDS_CHANGED;

import static org.folio.rest.jaxrs.model.PoLine.OrderFormat.ELECTRONIC_RESOURCE;
import static org.folio.rest.jaxrs.model.PoLine.OrderFormat.OTHER;
import static org.folio.rest.jaxrs.model.PoLine.OrderFormat.PHYSICAL_RESOURCE;
import static org.folio.rest.jaxrs.model.PoLine.OrderFormat.P_E_MIX;
import static org.folio.rest.jaxrs.model.PoLine.ReceiptStatus.ONGOING;

import java.util.ArrayList;
import java.util.HashSet;
import java.util.List;
import java.util.Map;
import java.util.Objects;
import java.util.Optional;
import java.util.Set;
import java.util.function.Function;
import java.util.stream.Collectors;

import one.util.streamex.StreamEx;
import org.apache.commons.collections4.CollectionUtils;
import org.apache.commons.lang3.ObjectUtils;
import org.folio.rest.core.exceptions.HttpException;
import org.folio.rest.core.exceptions.ErrorCodes;
import org.folio.rest.jaxrs.model.PoLine;
import org.folio.rest.jaxrs.model.CompositePurchaseOrder;
import org.folio.rest.jaxrs.model.Eresource;
import org.folio.rest.jaxrs.model.Error;
import org.folio.rest.jaxrs.model.Location;
import org.folio.rest.jaxrs.model.Parameter;
import org.folio.rest.jaxrs.model.Physical;
import org.folio.rest.tools.parser.JsonPathParser;

import io.vertx.core.json.JsonArray;
import io.vertx.core.json.JsonObject;

public final class PoLineCommonUtil {
  private static final String DASH_SEPARATOR = "-";
  private static final String PROTECTED_AND_MODIFIED_FIELDS = "protectedAndModifiedFields";

  private static final List<String> NOT_EDITABLE_ONGOING_SUBSCRIPTION_FIELDS = List.of(
    "reviewDate",
    "manualRenewal"
  );

  private static final List<String> NOT_EDITABLE_ONGOING_NOT_SUBSCRIPTION_FIELDS = List.of(
    "interval",
    "renewalDate",
    "reviewPeriod",
    "manualRenewal"
  );

  private PoLineCommonUtil() {
  }

  public static String buildPoLineNumber(String poNumber, String sequence) {
    return poNumber + DASH_SEPARATOR + sequence;
  }

  public static List<PoLine> sortPoLinesByPoLineNumber(List<PoLine> poLines) {
    poLines.sort(PoLineCommonUtil::comparePoLinesByPoLineNumber);
    return poLines;
  }

  private static int comparePoLinesByPoLineNumber(PoLine poLine1, PoLine poLine2) {
    String n1 = poLine1.getPoLineNumber();
    String n2 = poLine2.getPoLineNumber();
    if (n1 == null || n2 == null)
      return 0;
    String poLineNumberSuffix1 = n1.split(DASH_SEPARATOR)[1];
    String poLineNumberSuffix2 = n2.split(DASH_SEPARATOR)[1];
    return Integer.parseInt(poLineNumberSuffix1) - Integer.parseInt(poLineNumberSuffix2);
  }

  public static boolean isReceiptNotRequired(PoLine.ReceiptStatus receiptStatus) {
    return receiptStatus == PoLine.ReceiptStatus.RECEIPT_NOT_REQUIRED;
  }

  public static boolean isInventoryUpdateNotRequired(PoLine poLine) {
    // in case of "Other" order format check Physical createInventory value only
    if (poLine.getOrderFormat() == OTHER || poLine.getOrderFormat() == PHYSICAL_RESOURCE) {
      return isUpdateNotRequiredForPhysical(poLine);
    } else if (poLine.getOrderFormat() == ELECTRONIC_RESOURCE) {
      return isUpdateNotRequiredForEresource(poLine);
    } else {
      return isUpdateNotRequiredForPhysical(poLine) && isUpdateNotRequiredForEresource(poLine);
    }
  }

  private static boolean isUpdateNotRequiredForEresource(PoLine poLine) {
    return poLine.getEresource() == null || poLine.getEresource().getCreateInventory() == Eresource.CreateInventory.NONE;
  }

  private static boolean isUpdateNotRequiredForPhysical(PoLine poLine) {
    return poLine.getPhysical() == null || poLine.getPhysical().getCreateInventory() == Physical.CreateInventory.NONE;
  }

  public static boolean isHoldingsUpdateRequired(PoLine poLine) {
    return isHoldingUpdateRequiredForPhysical(poLine) || isHoldingUpdateRequiredForEresource(poLine);
  }

  public static boolean isHoldingUpdateRequiredForPhysical(PoLine poLine) {
    Physical physical = getPhysical(poLine);
    return physical != null && (physical.getCreateInventory() == Physical.CreateInventory.INSTANCE_HOLDING
      || physical.getCreateInventory() == Physical.CreateInventory.INSTANCE_HOLDING_ITEM);
  }

  public static boolean isHoldingUpdateRequiredForEresource(PoLine poLine) {
    Eresource eresource = getEresource(poLine);
    return eresource != null && (eresource.getCreateInventory() == Eresource.CreateInventory.INSTANCE_HOLDING
      || eresource.getCreateInventory() == Eresource.CreateInventory.INSTANCE_HOLDING_ITEM);
  }

  public static boolean isItemsUpdateRequired(PoLine poLine) {
    return isItemsUpdateRequiredForPhysical(poLine) || isItemsUpdateRequiredForEresource(poLine);
  }

  public static boolean isItemsUpdateRequiredForEresource(PoLine poLine) {
    if (poLine.getCheckinItems() != null && poLine.getCheckinItems()) {
      return false;
    }
    return Optional.ofNullable(getEresource(poLine))
      .map(eresource -> eresource.getCreateInventory() == Eresource.CreateInventory.INSTANCE_HOLDING_ITEM)
      .orElse(false);
  }

  public static boolean isItemsUpdateRequiredForPhysical(PoLine poLine) {
    if (poLine.getCheckinItems() != null && poLine.getCheckinItems()) {
      return false;
    }
    return Optional.ofNullable(getPhysical(poLine))
      .map(physical -> physical.getCreateInventory() == Physical.CreateInventory.INSTANCE_HOLDING_ITEM)
      .orElse(false);
  }

  public static Physical getPhysical(PoLine poLine) {
    PoLine.OrderFormat format = poLine.getOrderFormat();
    return format == PHYSICAL_RESOURCE || format == P_E_MIX || format == OTHER ? poLine.getPhysical() : null;
  }

  public static Eresource getEresource(PoLine poLine) {
    PoLine.OrderFormat format = poLine.getOrderFormat();
    return format == ELECTRONIC_RESOURCE || format == P_E_MIX ? poLine.getEresource() : null;
  }

  public static boolean isOnlyInstanceUpdateRequired(PoLine poLine) {
    PoLine.OrderFormat format = poLine.getOrderFormat();
    boolean checkPhysical = format == PHYSICAL_RESOURCE || format == OTHER || format == P_E_MIX;
    boolean checkElectronic = format == ELECTRONIC_RESOURCE || format == P_E_MIX;

    boolean isPhysicalInstance = checkPhysical && Optional.ofNullable(poLine.getPhysical())
      .map(physical -> physical.getCreateInventory() == Physical.CreateInventory.INSTANCE)
      .orElse(false);

    boolean isElectronicInstance = checkElectronic && Optional.ofNullable(poLine.getEresource())
      .map(elec -> elec.getCreateInventory() == Eresource.CreateInventory.INSTANCE)
      .orElse(false);

    return isPhysicalInstance || isElectronicInstance;
  }

  public static boolean isHoldingCreationRequiredForLocation(PoLine poLine, Location location) {
    return (isHoldingUpdateRequiredForPhysical(poLine) && ObjectUtils.defaultIfNull(location.getQuantityPhysical(), 0) > 0)
      || (isHoldingUpdateRequiredForEresource(poLine) && ObjectUtils.defaultIfNull(location.getQuantityElectronic(), 0) > 0);
  }

  /**
   * Group all PO Line's locations for which the holding should be created by location identifier
   * @param poLine PO line with locations to group
   * @return map of grouped locations where key is location id and value is list of locations with the same id
   */
  public static Map<String, List<Location>> groupLocationsByLocationId(PoLine poLine) {
    return extractLocationsForPoLineByLocationId(poLine)
      .collect(Collectors.groupingBy(Location::getLocationId));
  }

  /**
   * Map all PO Line's location to tenantIds for which the holding should be created by location identifier
   * @param poLine PO line with locations
   * @return map of locations and tenantIds where key is location id and value is the tenantId of the specified location
   */
  public static Map<String, String> mapLocationIdsToTenantIds(PoLine poLine) {
    return extractLocationsForPoLineByLocationId(poLine)
      .filter(location -> Objects.nonNull(location.getTenantId()))
      .collect(Collectors.toMap(Location::getLocationId, Location::getTenantId));
  }

  /**
   * Group all PO Line's locations for which the holding should be created by holding identifier
   * @param poLine PO line with locations to group
   * @return map of grouped locations where key is holding id and value is list of locations with the same id
   */
  public static Map<String, List<Location>> groupLocationsByHoldingId(PoLine poLine) {
    return extractLocationsForPoLineByHoldingId(poLine)
      .collect(Collectors.groupingBy(Location::getHoldingId));
  }

  /**
   * Map all PO Line's location to tenantIds for which the holding should be created by holding identifier
   * @param poLine PO line with locations
   * @return map of locations and tenantIds where key is holding id and value is the tenantId of the specified location
   */
  public static Map<String, String> mapHoldingIdsToTenantIds(PoLine poLine) {
    return extractLocationsForPoLineByHoldingId(poLine)
      .filter(location -> Objects.nonNull(location.getTenantId()))
      .collect(Collectors.toMap(Location::getHoldingId, Location::getTenantId));
  }

  private static StreamEx<Location> extractLocationsForPoLineByLocationId(PoLine poLine) {
    return extractLocationsForPoLine(poLine, Location::getLocationId)
      .filter(location -> !isHoldingCreationRequiredForLocation(poLine, location));
  }

  private static StreamEx<Location> extractLocationsForPoLineByHoldingId(PoLine poLine) {
    return extractLocationsForPoLine(poLine, Location::getHoldingId)
      .filter(location -> isHoldingCreationRequiredForLocation(poLine, location));
  }

  private static StreamEx<Location> extractLocationsForPoLine(PoLine poLine, Function<Location, String> fieldExtractor) {
    if (CollectionUtils.isEmpty(poLine.getLocations())) {
      return StreamEx.empty();
    }
    return StreamEx.of(poLine.getLocations())
      .filter(location -> Objects.nonNull(fieldExtractor.apply(location)));
  }

  public static List<String> getTenantsFromLocations(PoLine poLine) {
    return getTenantsFromLocations(poLine.getLocations());
  }

  public static List<String> getTenantsFromLocations(List<Location> locations) {
    return locations
      .stream()
      .map(Location::getTenantId)
      .distinct()
      .toList();
  }

  public static Set<Location> extractUnaffiliatedLocations(List<Location> locations, List<String> tenantIds) {
    return StreamEx.of(locations)
      .filter(location -> Objects.nonNull(location.getTenantId()))
      .filter(location -> !tenantIds.contains(location.getTenantId()))
      .toSet();
  }

  public static void updateLocationsQuantity(List<Location> locations) {
    locations.forEach(location -> location.setQuantity(calculateTotalLocationQuantity(location)));
  }

  public static int getPhysicalCostQuantity(PoLine poLine) {
    return defaultIfNull(poLine.getCost().getQuantityPhysical(), 0);
  }

  public static int getElectronicCostQuantity(PoLine poLine) {
    return defaultIfNull(poLine.getCost().getQuantityElectronic(), 0);
  }

  public static int getOverallCostQuantity(PoLine poLine) {
    return getElectronicCostQuantity(poLine) + getPhysicalCostQuantity(poLine);
  }

  public static JsonObject verifyProtectedFieldsChanged(List<String> protectedFields, JsonObject objectFromStorage,
    JsonObject requestObject) {
    Set<String> fields = new HashSet<>();
    JsonPathParser oldObject = new JsonPathParser(objectFromStorage);
    JsonPathParser newObject = new JsonPathParser(requestObject);
    for (String field : protectedFields) {
      if (oldObject.getValueAt(field) instanceof JsonArray || newObject.getValueAt(field) instanceof JsonArray) {
        var oldList = getArray(oldObject, field);
        var newList = getArray(newObject, field);
        if (oldList.size() != newList.size() || !CollectionUtils.containsAll(oldList, newList)) {
          fields.add(field);
        }
      } else if (ObjectUtils.notEqual(oldObject.getValueAt(field), newObject.getValueAt(field))) {
        fields.add(field);
      }
    }

    if (CollectionUtils.isNotEmpty(fields)) {
      Error error = PROHIBITED_FIELD_CHANGING.toError()
        .withAdditionalProperty(PROTECTED_AND_MODIFIED_FIELDS, fields);
      throw new HttpException(400, error);
    }

    return objectFromStorage;
  }

  private static List getArray(JsonPathParser parser, String field) {
    Object valueAt = parser.getValueAt(field);
    if (!(valueAt instanceof JsonArray jsonArray)) {
      return new ArrayList<>();
    }
    return jsonArray.getList();
  }

  public static void verifyOngoingFieldsChanged(JsonObject compPOFromStorageJson, CompositePurchaseOrder compPO) {
    JsonObject ongoingJson = compPOFromStorageJson.getJsonObject("ongoing");
    if (Objects.nonNull(ongoingJson) && Objects.nonNull(compPO.getOngoing())) {
      JsonPathParser oldObject = new JsonPathParser(ongoingJson);
      JsonPathParser newObject = new JsonPathParser(JsonObject.mapFrom(compPO.getOngoing()));
      if (Boolean.TRUE.equals(compPO.getOngoing().getIsSubscription())) {
        checkFieldsChanged(oldObject, newObject, NOT_EDITABLE_ONGOING_SUBSCRIPTION_FIELDS, WRONG_ONGOING_SUBSCRIPTION_FIELDS_CHANGED);
      } else {
        checkFieldsChanged(oldObject, newObject, NOT_EDITABLE_ONGOING_NOT_SUBSCRIPTION_FIELDS, WRONG_ONGOING_NOT_SUBSCRIPTION_FIELDS_CHANGED);
      }
    }
  }

  private static void checkFieldsChanged(JsonPathParser oldObject, JsonPathParser newObject, List<String> fields, ErrorCodes error) {
    List<Parameter> parameters = new ArrayList<>();
    for (String field: fields) {
      if (ObjectUtils.notEqual(oldObject.getValueAt(field), newObject.getValueAt(field))) {
        parameters.add(new Parameter().withKey(field).withValue(newObject.getValueAt(field).toString()));
      }
    }
    if (!parameters.isEmpty()) {
      throw new HttpException(400, error, parameters);
    }
  }

  public static boolean isCancelledOrOngoingStatus(PoLine poLine) {
    return poLine.getReceiptStatus() == PoLine.ReceiptStatus.CANCELLED || poLine.getReceiptStatus() == ONGOING;
  }
}
