package org.folio.orders.utils;

import java.util.Arrays;
import java.util.List;
import java.util.stream.Collectors;

public enum POLineProtectedFields {

  ACQUISITION_METHOD("acquisitionMethod"),
  CHECKIN_ITEMS("checkinItems"),
  COST_ADDITIONAL_COST("cost.additionalCost"),
  COST_CURRENCY("cost.currency"),
  COST_DISCOUNT("cost.discount"),
  COST_DISCOUNT_TYPE("cost.discountType"),
  COLLECTION("collection"),
  CONTRIBUTORS("contributors"),
  DONOR("donor"),
  DETAILS_PRODUCT_IDS("details.productIds"),
  DETAILS_SUBSCRIPTION_INTERVAL("details.subscriptionInterval"),
  DETAILS_SUBSCRIPTION_TO_DATE("details.subscriptionTo"),
  ERESOURCE_CREATE_INVENTORY("eresource.createInventory"),
  ERESOURCE_TRIAL("eresource.trial"),
  ERESOURCE_LICENSE("eresource.license"),
  ERESOURCE_MATERIAL_TYPE("eresource.materialType"),
  ERESOURCE_USER_LIMIT("eresource.userLimit"),
  ORDER_FORMAT("orderFormat"),
  PHYSICAL_CREATE_INVENTORY("physical.createInventory"),
  PHYSICAL_VOLUMES("physical.volumes"),
  PHYSICAL_MATERIAL_TYPE("physical.materialType"),
  PUBLICATION_DATE("publicationDate"),
  PUBLISHER("publisher"),
  REQUESTER("requester"),
  RUSH("rush"),
  SOURCE("source"),
  SELECTOR("selector"),
  TITLE_OR_PACKAGE("titleOrPackage"),
  VENDORDETAIL_INSTRUCTION_TO_VENDOR("vendorDetail.instructions"),
  VENDORDETAIL_VENDOR_ACCOUNT("vendorDetail.vendorAccount");


  POLineProtectedFields(String field) {
    this.field = field;
  }

  public String getFieldName() {
    return field;
  }

  private String field;

  public static List<String> getFieldNames() {
    return Arrays.stream(POLineProtectedFields.values()).map(POLineProtectedFields::getFieldName).collect(Collectors.toList());
  }
}
