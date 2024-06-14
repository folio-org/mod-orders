package org.folio.orders.utils;

public enum CommonFields {

  METADATA("metadata"),
  CREATED_DATE("createdDate");

  private final String value;

  CommonFields(String value) {
    this.value = value;
  }

  public String getValue() {
    return value;
  }

}
