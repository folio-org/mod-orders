package org.folio.orders.utils;

import java.util.Arrays;
import java.util.List;
import java.util.stream.Collectors;

public enum POLineProtectedFields {

  CHECKIN_ITEMS("checkInItems"),
  COST("cost"),
  COLLECTION("collection"),
  CONTRIBUTORS("contributors"),
  CREATED_DATE("createdDate"),
  DONOR("donor"),
  DETAILS_PRODUCT_IDS("details.productIds"),
  DETAILS_SUBSCRIPTION_INTERVAL("details.subscriptionInterval"),
  DETAILS_SUBSCRIPTION_TO_DATE("details.subscriptionToDate"),
  DETAILS_EDITION("details.edition"),
  ERSOURCE_CREATE_INVENTORY("eresource.createInventory"),
  ERESOURCE_TRIAL("eresource.trial"),
  ERESOURCE_LICENSE("eresource.license"),
  ERESOURCE_MATERIAL_TYPE("eresource.material"),
  ERESOURCE_USER_LIMIT("eresource.userLimit"),
  FUND_DISTRIBUTION("fundDistribution"),
  LOCATION("location") ,
  ORER_FORMAT("orderFormat"),
  PHYSICAL_VOLUMES("physical.volumes"),
  PHYSICAL_MATERIAL_TYPE("physical.materialType"),
  PHYSICAL_ACQUISITION_METHOD("physical.acquistionMethod"),
  PUBLICATION_DATE("publicationDate"),
  PUBLISHER("publisher"),
  PO_LINE_NUMBER("poLineNumber"),
  RUSH("rush"),
  SOURCE("source"),
  REQUESTOR("requestor"),
  SELECTOR("selector"),
  TITLE("title"),
  VENDORDETAIL_VENDOR("vendorDetail.vendor"),
  VENDORDETAIL_INSTRUCTION_TO_VENDOR("vendorDetail.instructionToVendor"),
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
