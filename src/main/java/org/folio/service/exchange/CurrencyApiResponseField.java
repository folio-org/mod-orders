package org.folio.service.exchange;

import lombok.AllArgsConstructor;
import lombok.Getter;

@Getter
@AllArgsConstructor
public enum CurrencyApiResponseField {
  DATA("data"),
  VALUE("value");

  private final String value;
}
