package org.folio.orders.utils;

import static org.folio.orders.utils.POLineFieldNames.ACQUISITION_METHOD;
import static org.folio.orders.utils.POLineFieldNames.CHECKIN_ITEMS;
import static org.folio.orders.utils.POLineFieldNames.COLLECTION;
import static org.folio.orders.utils.POLineFieldNames.CONTRIBUTORS;
import static org.folio.orders.utils.POLineFieldNames.DETAILS_PRODUCT_IDS;
import static org.folio.orders.utils.POLineFieldNames.DETAILS_SUBSCRIPTION_INTERVAL;
import static org.folio.orders.utils.POLineFieldNames.DONOR;
import static org.folio.orders.utils.POLineFieldNames.ERESOURCE_CREATE_INVENTORY;
import static org.folio.orders.utils.POLineFieldNames.ERESOURCE_LICENSE;
import static org.folio.orders.utils.POLineFieldNames.ERESOURCE_MATERIAL_TYPE;
import static org.folio.orders.utils.POLineFieldNames.ERESOURCE_TRIAL;
import static org.folio.orders.utils.POLineFieldNames.ERESOURCE_USER_LIMIT;
import static org.folio.orders.utils.POLineFieldNames.ORDER_FORMAT;
import static org.folio.orders.utils.POLineFieldNames.PHYSICAL_CREATE_INVENTORY;
import static org.folio.orders.utils.POLineFieldNames.PHYSICAL_MATERIAL_TYPE;
import static org.folio.orders.utils.POLineFieldNames.PHYSICAL_VOLUMES;
import static org.folio.orders.utils.POLineFieldNames.RUSH;
import static org.folio.orders.utils.POLineFieldNames.SELECTOR;
import static org.folio.orders.utils.POLineFieldNames.VENDORDETAIL_INSTRUCTION_TO_VENDOR;
import static org.folio.rest.jaxrs.model.CompositePurchaseOrder.WorkflowStatus.OPEN;
import static org.folio.rest.jaxrs.model.PoLine.OrderFormat.ELECTRONIC_RESOURCE;
import static org.folio.rest.jaxrs.model.PoLine.OrderFormat.OTHER;
import static org.folio.rest.jaxrs.model.PoLine.OrderFormat.PHYSICAL_RESOURCE;
import static org.folio.rest.jaxrs.model.PoLine.OrderFormat.P_E_MIX;

import java.util.Collections;
import java.util.EnumMap;
import java.util.List;
import java.util.Map;
import java.util.Optional;
import java.util.function.BiPredicate;

import lombok.experimental.UtilityClass;
import org.apache.commons.collections4.ListUtils;
import org.apache.commons.lang3.BooleanUtils;
import org.folio.rest.jaxrs.model.CompositePurchaseOrder;
import org.folio.rest.jaxrs.model.PoLine;

@UtilityClass
public class POLineProtectedFieldsUtil {

  private static final List<POLineFieldNames> COMMON_PROTECTED_FIELDS = List.of(
    ACQUISITION_METHOD, COLLECTION, CONTRIBUTORS, DONOR, DETAILS_PRODUCT_IDS, DETAILS_SUBSCRIPTION_INTERVAL,
    ORDER_FORMAT, RUSH, SELECTOR, VENDORDETAIL_INSTRUCTION_TO_VENDOR);

  private static final List<POLineFieldNames> ELEC_PROTECTED_FIELDS = List.of(
    ERESOURCE_CREATE_INVENTORY, ERESOURCE_TRIAL, ERESOURCE_LICENSE, ERESOURCE_MATERIAL_TYPE, ERESOURCE_USER_LIMIT);

  private static final List<POLineFieldNames> PHYS_PROTECTED_FIELDS = List.of(
    PHYSICAL_CREATE_INVENTORY, PHYSICAL_VOLUMES, PHYSICAL_MATERIAL_TYPE);

  private static final List<POLineFieldNames> PHYS_VS_ELEC_PROTECTED_FIELDS = ListUtils.union(ELEC_PROTECTED_FIELDS, PHYS_PROTECTED_FIELDS);

  private static final EnumMap<PoLine.OrderFormat, List<POLineFieldNames>> PROTECTED_FIELDS_MAP = new EnumMap<>(Map.of(
    P_E_MIX, ListUtils.union(COMMON_PROTECTED_FIELDS, PHYS_VS_ELEC_PROTECTED_FIELDS),
    ELECTRONIC_RESOURCE, ListUtils.union(COMMON_PROTECTED_FIELDS, ELEC_PROTECTED_FIELDS),
    PHYSICAL_RESOURCE, ListUtils.union(COMMON_PROTECTED_FIELDS, PHYS_PROTECTED_FIELDS),
    OTHER, ListUtils.union(COMMON_PROTECTED_FIELDS, PHYS_PROTECTED_FIELDS)
  ));

  private static final EnumMap<POLineFieldNames, BiPredicate<CompositePurchaseOrder, PoLine>> CONDITIONAL_PROTECTED_FIELDS = new EnumMap<>(Map.of(
    // Restrict check-in items only for open POs with check-in items enabled
    CHECKIN_ITEMS, (po, pol) -> po.getWorkflowStatus() == OPEN && BooleanUtils.isNotTrue(pol.getCheckinItems())
  ));

  private static List<String> getConditionalProtectedFields(CompositePurchaseOrder po, PoLine pol) {
    return CONDITIONAL_PROTECTED_FIELDS.entrySet().stream()
      .filter(entry -> entry.getValue().test(po, pol))
      .map(Map.Entry::getKey)
      .map(POLineFieldNames::getFieldName)
      .toList();
  }

  public static List<String> getFieldNames(CompositePurchaseOrder purchaseOrder, PoLine poLine) {
    return Optional.ofNullable(PROTECTED_FIELDS_MAP.get(poLine.getOrderFormat()))
      .map(fieldNames -> fieldNames.stream().map(POLineFieldNames::getFieldName).toList())
      .map(fieldNames -> ListUtils.union(fieldNames, getConditionalProtectedFields(purchaseOrder, poLine)))
      .orElse(Collections.emptyList());
  }

}
