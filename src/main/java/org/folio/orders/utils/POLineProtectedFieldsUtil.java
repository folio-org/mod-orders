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
import static org.folio.rest.jaxrs.model.PoLine.OrderFormat.ELECTRONIC_RESOURCE;
import static org.folio.rest.jaxrs.model.PoLine.OrderFormat.OTHER;
import static org.folio.rest.jaxrs.model.PoLine.OrderFormat.PHYSICAL_RESOURCE;
import static org.folio.rest.jaxrs.model.PoLine.OrderFormat.P_E_MIX;

import java.util.Collections;
import java.util.HashMap;
import java.util.List;
import java.util.Map;
import java.util.Optional;
import java.util.stream.Collectors;
import java.util.stream.Stream;

import org.apache.commons.collections4.ListUtils;

public class POLineProtectedFieldsUtil {
  private static final List<String> commonProtectedFieldNames = Stream.of(ACQUISITION_METHOD, CHECKIN_ITEMS,
      COLLECTION, CONTRIBUTORS, DONOR, DETAILS_PRODUCT_IDS, DETAILS_SUBSCRIPTION_INTERVAL,
      ORDER_FORMAT, RUSH, SELECTOR, VENDORDETAIL_INSTRUCTION_TO_VENDOR)
    .map(POLineFieldNames::getFieldName).collect(Collectors.toList());

  private static final List<String> elecProtectedFieldNames = Stream.of(ERESOURCE_CREATE_INVENTORY,
      ERESOURCE_TRIAL, ERESOURCE_LICENSE, ERESOURCE_MATERIAL_TYPE, ERESOURCE_USER_LIMIT)
    .map(POLineFieldNames::getFieldName).collect(Collectors.toList());

  private static final List<String> physProtectedFieldNames = Stream.of(PHYSICAL_CREATE_INVENTORY,
      PHYSICAL_VOLUMES, PHYSICAL_MATERIAL_TYPE)
    .map(POLineFieldNames::getFieldName).collect(Collectors.toList());

  private static final List<String> physVsElecProtectedFieldNames = ListUtils.union(elecProtectedFieldNames, physProtectedFieldNames);

  private static final Map<String, List<String>> protectedFieldsMap;
  static {
    protectedFieldsMap = new HashMap<>();
    protectedFieldsMap.put(P_E_MIX.value(), ListUtils.union(commonProtectedFieldNames, physVsElecProtectedFieldNames));
    protectedFieldsMap.put(ELECTRONIC_RESOURCE.value(), ListUtils.union(commonProtectedFieldNames, elecProtectedFieldNames));
    protectedFieldsMap.put(PHYSICAL_RESOURCE.value(), ListUtils.union(commonProtectedFieldNames, physProtectedFieldNames));
    protectedFieldsMap.put(OTHER.value(), ListUtils.union(commonProtectedFieldNames, physProtectedFieldNames));
  }

  private POLineProtectedFieldsUtil() {

  }

  public static List<String> getFieldNames(String orderFormat) {
    return Optional.ofNullable(protectedFieldsMap.get(orderFormat)).orElse(Collections.emptyList());
  }
}
