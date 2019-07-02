package org.folio.orders.utils;

import java.util.Arrays;
import java.util.List;
import java.util.stream.Collectors;

public enum POProtectedFields {
  PO_NUMBER("poNumber"),
  REASON_FOR_CLOSURE("reasonForClosure"),
  MANUAL_PO("manualPo"),
  APPROVED("approved"),
  ORDER_TYPE("orderType"),
  RE_ENCUMBER("reEncumber"),
  BILL_TO("billTo"),
  RENEWAL("renewal"),
  VENDOR("vendor");

  POProtectedFields(String field) {
    this.field = field;
  }

  public String getFieldName() {
    return field;
  }

  private String field;

  public static List<String> getFieldNames() {
    return Arrays.stream(POProtectedFields.values())
      .map(POProtectedFields::getFieldName)
      .collect(Collectors.toList());
  }
}
