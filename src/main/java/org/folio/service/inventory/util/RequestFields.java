package org.folio.service.inventory.util;

public enum RequestFields {

  ID_KEY("id"),
  ITEM_ID_KEY("itemId"),
  DESTINATION_KEY("destinationItemId"),
  COLLECTION_RECORDS("requests"),
  COLLECTION_TOTAL("totalRecords");

  private String value;

  RequestFields(String value) {
    this.value = value;
  }

  public String getValue() {
    return value;
  }

}
