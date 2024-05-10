package org.folio.models;

import com.fasterxml.jackson.annotation.JsonCreator;
import com.fasterxml.jackson.annotation.JsonValue;

import java.util.HashMap;
import java.util.Map;

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


  public enum ItemStatus {

    IN_PROCESS("In process"),
    ON_ORDER("On order"),
    AVAILABLE("Available"),
    IN_TRANSIT("In transit"),
    UNDEFINED("Undefined"),
    ORDER_CLOSED("Order closed");
    private final String value;
    private static final Map<String, ItemFields.ItemStatus> CONSTANTS = new HashMap<>();

    static {
      for (ItemFields.ItemStatus c: values()) {
        CONSTANTS.put(c.value, c);
      }
    }

    ItemStatus(String value) {
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

    @JsonCreator
    public static ItemFields.ItemStatus fromValue(String value) {
      ItemFields.ItemStatus constant = CONSTANTS.get(value);
      if (constant == null) {
        throw new IllegalArgumentException(value);
      } else {
        return constant;
      }
    }
  }
}
