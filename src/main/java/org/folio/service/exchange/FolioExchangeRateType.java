package org.folio.service.exchange;

import lombok.AccessLevel;
import lombok.AllArgsConstructor;
import lombok.Getter;

import javax.money.convert.ExchangeRateProviderSupplier;

@Getter
@AllArgsConstructor
public enum FolioExchangeRateType implements ExchangeRateProviderSupplier {
  FOLIO_CURRENT("FOLIO_CURRENT", "Custom current exchange rate type for FOLIO"),
  FOLIO_HISTORY("FOLIO_HISTORY", "Custom history exchange rate type for FOLIO");

  @Getter(AccessLevel.NONE)
  private final String type;
  private final String description;

  @Override
  public String get() {
    return this.type;
  }
}
