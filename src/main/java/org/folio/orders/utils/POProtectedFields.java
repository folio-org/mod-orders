package org.folio.orders.utils;

import java.util.Arrays;
import java.util.List;
import java.util.stream.Collectors;

public enum POProtectedFields {
  PO_NUMBER("poNumber"),
  MANUAL_PO("manualPo"),
  APPROVED("approved"),
  ORDER_TYPE("orderType"),
  RE_ENCUMBER("reEncumber"),
  TEMPLATE("template");

  POProtectedFields(String field) {
    this.field = field;
  }

  public String getFieldName() {
    return field;
  }

  private final String field;

  public static List<String> getFieldNames() {
    return Arrays.stream(POProtectedFields.values())
      .map(POProtectedFields::getFieldName)
      .collect(Collectors.toList());
  }
}
