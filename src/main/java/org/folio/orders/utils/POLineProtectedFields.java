package org.folio.orders.utils;

import java.util.Arrays;
import java.util.List;
import java.util.stream.Collectors;

public enum POLineProtectedFields {

  CHECKIN_ITEMS("checkinItems"),
  COST_LIST_UNIT_PRICE("cost.listUnitPrice"),
  COST_LIST_UNIT_PRICE_ELECTRONIC("cost.listUnitPriceElectronic"),
  COST_CURRENCY("cost.currency"),
  COST_DISCOUNT("cost.discount"),
  COST_DISCOUNT_TYPE("cost.discountType"),
  COST_QUANTITY_PHYSICAL("cost.quantityPhysical"),
  COST_QUANTITY_ELECTRONIC("cost.quantityElectronic"),
  COLLECTION("collection"),
  CONTRIBUTORS("contributors"),
  DONOR("donor"),
  DETAILS_PRODUCT_IDS("details.productIds"),
  DETAILS_SUBSCRIPTION_INTERVAL("details.subscriptionInterval"),
  DETAILS_SUBSCRIPTION_TO_DATE("details.subscriptionTo"),
  ERSOURCE_CREATE_INVENTORY("eresource.createInventory"),
  ERESOURCE_TRIAL("eresource.trial"),
  ERESOURCE_LICENSE("eresource.license"),
  ERESOURCE_MATERIAL_TYPE("eresource.materialType"),
  ERESOURCE_USER_LIMIT("eresource.userLimit"),
  FUND_DISTRIBUTION("fundDistribution"),
  LOCATION("locations") ,
  ORER_FORMAT("orderFormat"),
  PHYSICAL_VOLUMES("physical.volumes"),
  PHYSICAL_MATERIAL_TYPE("physical.materialType"),
  PUBLICATION_DATE("publicationDate"),
  PUBLISHER("publisher"),
  PO_LINE_NUMBER("poLineNumber"),
  RUSH("rush"),
  SOURCE("source"),
  SELECTOR("selector"),
  TITLE("title"),
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
