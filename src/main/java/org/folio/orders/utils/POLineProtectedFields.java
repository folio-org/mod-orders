package org.folio.orders.utils;

import static org.folio.helper.PurchaseOrderLineHelper.ERESOURCE;

import java.util.Arrays;
import java.util.List;
import java.util.stream.Collectors;

import org.folio.rest.jaxrs.model.CompositePoLine;

public enum POLineProtectedFields {

  ACQUISITION_METHOD("acquisitionMethod"),
  CHECKIN_ITEMS("checkinItems"),
  COLLECTION("collection"),
  CONTRIBUTORS("contributors"),
  DONOR("donor"),
  DETAILS_PRODUCT_IDS("details.productIds"),
  DETAILS_SUBSCRIPTION_INTERVAL("details.subscriptionInterval"),
  ERESOURCE_CREATE_INVENTORY("eresource.createInventory"),
  ERESOURCE_TRIAL("eresource.trial"),
  ERESOURCE_LICENSE("eresource.license"),
  ERESOURCE_MATERIAL_TYPE("eresource.materialType"),
  ERESOURCE_USER_LIMIT("eresource.userLimit"),
  ORDER_FORMAT("orderFormat"),
  PHYSICAL_CREATE_INVENTORY("physical.createInventory"),
  PHYSICAL_VOLUMES("physical.volumes"),
  PHYSICAL_MATERIAL_TYPE("physical.materialType"),
  REQUESTER("requester"),
  RUSH("rush"),
  SELECTOR("selector"),
  VENDORDETAIL_INSTRUCTION_TO_VENDOR("vendorDetail.instructions"),
  LAST_EDI_EXPORT_DATE("lastEDIExportDate");


  POLineProtectedFields(String field) {
    this.field = field;
  }

  public String getFieldName() {
    return field;
  }

  private String field;

  public static List<String> getFieldNames(String orderFormat) {
    if (!CompositePoLine.OrderFormat.ELECTRONIC_RESOURCE.value().equals(orderFormat) &&
                      !CompositePoLine.OrderFormat.P_E_MIX.value().equals(orderFormat) ) {
      return Arrays.stream(POLineProtectedFields.values())
                      .map(POLineProtectedFields::getFieldName)
                      .filter(protectedPath -> !protectedPath.contains(ERESOURCE))
                      .collect(Collectors.toList());
    }
    return Arrays.stream(POLineProtectedFields.values())
                 .map(POLineProtectedFields::getFieldName)
                 .collect(Collectors.toList());
  }
}
