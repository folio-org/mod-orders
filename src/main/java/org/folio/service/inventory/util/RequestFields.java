package org.folio.service.inventory.util;

public enum RequestFields {

  ITEM_ID("itemId"),
  REQUESTER_ID("requesterId"),
  STATUS("status"),
  DESTINATION_ITEM_ID("destinationItemId"),
  COLLECTION_RECORDS("requests"),
  COLLECTION_TOTAL("totalRecords");

  private final String value;

  RequestFields(String value) {
    this.value = value;
  }

  public String getValue() {
    return value;
  }

}
