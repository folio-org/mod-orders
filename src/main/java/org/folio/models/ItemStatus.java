package org.folio.models;

import java.util.HashMap;
import java.util.Map;

import com.fasterxml.jackson.annotation.JsonCreator;
import com.fasterxml.jackson.annotation.JsonValue;


public enum ItemStatus {

  IN_PROCESS("In process"),
  ON_ORDER("On order"),
  AVAILABLE("Available"),
  IN_TRANSIT("In transit"),
  UNDEFINED("Undefined"),
  ORDER_CLOSED("Order closed");
  private final String value;
  private static final Map<String, ItemStatus> CONSTANTS = new HashMap<>();

  static {
    for (ItemStatus c: values()) {
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
  public static ItemStatus fromValue(String value) {
    ItemStatus constant = CONSTANTS.get(value);
    if (constant == null) {
      throw new IllegalArgumentException(value);
    } else {
      return constant;
    }
  }

}
