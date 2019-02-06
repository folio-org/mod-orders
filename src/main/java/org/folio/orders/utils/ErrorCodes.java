package org.folio.orders.utils;

public enum ErrorCodes {

  PO_NUMBER_ALREADY_EXISTS("poNumberNotUnique", "PO Number already exists");

  private final String code;
  private final String description;

  ErrorCodes(String code, String description) {
    this.code = code;
    this.description = description;
  }

  public String getDescription() {
    return description;
  }

  public String getCode() {
    return code;
  }

  @Override
  public String toString() {
    return code + ": " + description;
  }
}
