package org.folio.models;

import com.fasterxml.jackson.annotation.JsonValue;

public enum ItemFields {
  ID("id"),
  HOLDINGS_RECORD_ID("holdingsRecordId"),
  BARCODE("barcode"),
  ACCESSION_NUMBER("accessionNumber"),
  ITEM_LEVEL_CALL_NUMBER("itemLevelCallNumber"),
  STATUS("status"),
  STATUS_DATE("date"),
  STATUS_NAME("name"),
  MATERIAL_TYPE_ID("materialTypeId"),
  MATERIAL_TYPE("materialType"),
  PERMANENT_LOAN_TYPE_ID("permanentLoanTypeId"),
  PURCHASE_ORDER_LINE_IDENTIFIER("purchaseOrderLineIdentifier"),
  EFFECTIVE_LOCATION("effectiveLocation"),
  ENUMERATION("enumeration"),
  CHRONOLOGY("chronology"),
  DISCOVERY_SUPPRESS("discoverySuppress");

  private final String value;

  ItemFields(String value) {
    this.value = value;
  }

  @Override
  public String toString() {
    return this.value;
  }

  @JsonValue
  public String value() {
    return this.value;
  }

}
