package org.folio.service.orders.utils;

import lombok.Getter;

@Getter
public enum PoLineFields {

  LOCATIONS("locations");

  private final String value;

  PoLineFields(String value) {
    this.value = value;
  }

}
