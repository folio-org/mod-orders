package org.folio.orders.utils;

import static java.util.stream.Collectors.toList;
import static org.apache.commons.lang3.ObjectUtils.defaultIfNull;
import static org.folio.orders.utils.HelperUtils.calculateTotalLocationQuantity;
import static org.folio.orders.utils.ResourcePathResolver.ALERTS;
import static org.folio.orders.utils.ResourcePathResolver.REPORTING_CODES;
import static org.folio.rest.core.exceptions.ErrorCodes.PROHIBITED_FIELD_CHANGING;
import static org.folio.rest.core.exceptions.ErrorCodes.WRONG_ONGOING_NOT_SUBSCRIPTION_FIELDS_CHANGED;
import static org.folio.rest.core.exceptions.ErrorCodes.WRONG_ONGOING_SUBSCRIPTION_FIELDS_CHANGED;

import static org.folio.rest.jaxrs.model.CompositePoLine.OrderFormat.ELECTRONIC_RESOURCE;
import static org.folio.rest.jaxrs.model.CompositePoLine.OrderFormat.OTHER;
import static org.folio.rest.jaxrs.model.CompositePoLine.OrderFormat.PHYSICAL_RESOURCE;
import static org.folio.rest.jaxrs.model.CompositePoLine.OrderFormat.P_E_MIX;
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
import org.folio.rest.jaxrs.model.Alert;
import org.folio.rest.jaxrs.model.CompositePoLine;
import org.folio.rest.jaxrs.model.CompositePurchaseOrder;
import org.folio.rest.jaxrs.model.Eresource;
import org.folio.rest.jaxrs.model.Error;
import org.folio.rest.jaxrs.model.Location;
import org.folio.rest.jaxrs.model.Parameter;
import org.folio.rest.jaxrs.model.Physical;
import org.folio.rest.jaxrs.model.PoLine;
import org.folio.rest.jaxrs.model.ReportingCode;
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

  public static void sortPoLinesByPoLineNumber(List<CompositePoLine> poLines) {
    poLines.sort(PoLineCommonUtil::comparePoLinesByPoLineNumber);
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

  private static boolean isUpdateNotRequiredForEresource(CompositePoLine compPOL) {
    return compPOL.getEresource() == null || compPOL.getEresource().getCreateInventory() == Eresource.CreateInventory.NONE;
  }

  private static boolean isUpdateNotRequiredForPhysical(CompositePoLine compPOL) {
    return compPOL.getPhysical() == null || compPOL.getPhysical().getCreateInventory() == Physical.CreateInventory.NONE;
  }

  public static boolean isHoldingsUpdateRequired(CompositePoLine compPOL) {
    return isHoldingUpdateRequiredForPhysical(compPOL) || isHoldingUpdateRequiredForEresource(compPOL);
  }

  public static boolean isHoldingUpdateRequiredForPhysical(CompositePoLine compPOL) {
    Physical physical = getPhysical(compPOL);
    return physical != null && (physical.getCreateInventory() == Physical.CreateInventory.INSTANCE_HOLDING
      || physical.getCreateInventory() == Physical.CreateInventory.INSTANCE_HOLDING_ITEM);
  }

  public static boolean isHoldingUpdateRequiredForEresource(CompositePoLine compPOL) {
    Eresource eresource = getEresource(compPOL);
    return eresource != null && (eresource.getCreateInventory() == Eresource.CreateInventory.INSTANCE_HOLDING
      || eresource.getCreateInventory() == Eresource.CreateInventory.INSTANCE_HOLDING_ITEM);
  }

  public static boolean isItemsUpdateRequired(CompositePoLine compPOL) {
    return isItemsUpdateRequiredForPhysical(compPOL) || isItemsUpdateRequiredForEresource(compPOL);
  }

  public static boolean isItemsUpdateRequiredForEresource(CompositePoLine compPOL) {
    if (compPOL.getCheckinItems() != null && compPOL.getCheckinItems()) {
      return false;
    }
    return Optional.ofNullable(getEresource(compPOL))
      .map(eresource -> eresource.getCreateInventory() == Eresource.CreateInventory.INSTANCE_HOLDING_ITEM)
      .orElse(false);
  }

  public static boolean isItemsUpdateRequiredForPhysical(CompositePoLine compPOL) {
    if (compPOL.getCheckinItems() != null && compPOL.getCheckinItems()) {
      return false;
    }
    return Optional.ofNullable(getPhysical(compPOL))
      .map(physical -> physical.getCreateInventory() == Physical.CreateInventory.INSTANCE_HOLDING_ITEM)
      .orElse(false);
  }

  public static Physical getPhysical(PoLine poLine) {
    return getPhysical(convertToCompositePoLine(poLine));
  }

  public static Physical getPhysical(CompositePoLine compPOL) {
    CompositePoLine.OrderFormat format = compPOL.getOrderFormat();
    return format == PHYSICAL_RESOURCE || format == P_E_MIX || format == OTHER ? compPOL.getPhysical() : null;
  }

  public static Eresource getEresource(PoLine poLine) {
    return getEresource(convertToCompositePoLine(poLine));
  }

  public static Eresource getEresource(CompositePoLine compPOL) {
    CompositePoLine.OrderFormat format = compPOL.getOrderFormat();
    return format == ELECTRONIC_RESOURCE || format == P_E_MIX ? compPOL.getEresource() : null;
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

  /**
   * Group all PO Line's locations for which the holding should be created by location identifier
   * @param compPOL PO line with locations to group
   * @return map of grouped locations where key is location id and value is list of locations with the same id
   */
  public static Map<String, List<Location>> groupLocationsByLocationId(CompositePoLine compPOL) {
    return extractLocationsForPoLineByLocationId(compPOL)
      .collect(Collectors.groupingBy(Location::getLocationId));
  }

  /**
   * Map all PO Line's location to tenantIds for which the holding should be created by location identifier
   * @param compPOL PO line with locations
   * @return map of locations and tenantIds where key is location id and value is the tenantId of the specified location
   */
  public static Map<String, String> mapLocationIdsToTenantIds(CompositePoLine compPOL) {
    return extractLocationsForPoLineByLocationId(compPOL)
      .filter(location -> Objects.nonNull(location.getTenantId()))
      .collect(Collectors.toMap(Location::getLocationId, Location::getTenantId));
  }

  /**
   * Group all PO Line's locations for which the holding should be created by holding identifier
   * @param compPOL PO line with locations to group
   * @return map of grouped locations where key is holding id and value is list of locations with the same id
   */
  public static Map<String, List<Location>> groupLocationsByHoldingId(CompositePoLine compPOL) {
    return extractLocationsForPoLineByHoldingId(compPOL)
      .collect(Collectors.groupingBy(Location::getHoldingId));
  }

  /**
   * Map all PO Line's location to tenantIds for which the holding should be created by holding identifier
   * @param compPOL PO line with locations
   * @return map of locations and tenantIds where key is holding id and value is the tenantId of the specified location
   */
  public static Map<String, String> mapHoldingIdsToTenantIds(CompositePoLine compPOL) {
    return extractLocationsForPoLineByHoldingId(compPOL)
      .filter(location -> Objects.nonNull(location.getTenantId()))
      .collect(Collectors.toMap(Location::getHoldingId, Location::getTenantId));
  }

  private static StreamEx<Location> extractLocationsForPoLineByLocationId(CompositePoLine compPOL) {
    return extractLocationsForPoLine(compPOL, Location::getLocationId)
      .filter(location -> !isHoldingCreationRequiredForLocation(compPOL, location));
  }

  private static StreamEx<Location> extractLocationsForPoLineByHoldingId(CompositePoLine compPOL) {
    return extractLocationsForPoLine(compPOL, Location::getHoldingId)
      .filter(location -> isHoldingCreationRequiredForLocation(compPOL, location));
  }

  private static StreamEx<Location> extractLocationsForPoLine(CompositePoLine compPOL, Function<Location, String> fieldExtractor) {
    if (CollectionUtils.isEmpty(compPOL.getLocations())) {
      return StreamEx.empty();
    }
    return StreamEx.of(compPOL.getLocations())
      .filter(location -> Objects.nonNull(fieldExtractor.apply(location)));
  }

  public static List<String> getTenantsFromLocations(CompositePoLine poLine) {
    return getTenantsFromLocations(poLine.getLocations());
  }

  public static List<String> getTenantsFromLocations(List<Location> locations) {
    return locations
      .stream()
      .map(Location::getTenantId)
      .distinct()
      .toList();
  }

  public static CompositePoLine convertToCompositePoLine(PoLine poLine) {
    var poLineJson = JsonObject.mapFrom(poLine);
    poLineJson.remove(ALERTS);
    poLineJson.remove(REPORTING_CODES);
    return poLineJson.mapTo(CompositePoLine.class);
  }

  public static PoLine convertToPoLine(CompositePoLine compPoLine) {
    var pol = JsonObject.mapFrom(compPoLine);
    pol.remove(ALERTS);
    pol.remove(REPORTING_CODES);
    var poLine = pol.mapTo(PoLine.class);
    poLine.setAlerts(compPoLine.getAlerts().stream().map(Alert::getId).collect(toList()));
    poLine.setReportingCodes(compPoLine.getReportingCodes().stream().map(ReportingCode::getId).collect(toList()));
    return poLine;
  }

  public static List<PoLine> convertToPoLines(List<CompositePoLine> compositePoLines) {
    return compositePoLines.stream()
      .map(PoLineCommonUtil::convertToPoLine)
      .collect(toList());
  }

  public static void updateLocationsQuantity(List<Location> locations) {
    locations.forEach(location -> location.setQuantity(calculateTotalLocationQuantity(location)));
  }

  public static int getPhysicalCostQuantity(CompositePoLine compPOL) {
    return defaultIfNull(compPOL.getCost().getQuantityPhysical(), 0);
  }

  public static int getElectronicCostQuantity(CompositePoLine compPOL) {
    return defaultIfNull(compPOL.getCost().getQuantityElectronic(), 0);
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
