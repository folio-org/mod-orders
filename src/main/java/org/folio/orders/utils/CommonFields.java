package org.folio.orders.utils;

public enum CommonFields {

  ID("id"),
  METADATA("metadata"),
  CREATED_DATE("createdDate"),
  TENANT_ID("tenantId"),
  COLLECTION_TOTAL("totalRecords");

  private final String value;

  CommonFields(String value) {
    this.value = value;
  }

  public String getValue() {
    return value;
  }

}
