package org.folio.models;

import org.folio.rest.acq.model.finance.ExchangeRate;

public class RolloverConversionHolder {

  private String poLineId;
  private ExchangeRate exchangeRate;
  private boolean customExchangeRate;

  public RolloverConversionHolder withPoLineId(String poLineId) {
    this.poLineId = poLineId;
    return this;
  }

  public RolloverConversionHolder withExchangeRate(ExchangeRate exchangeRate) {
    this.exchangeRate = exchangeRate;
    return this;
  }

  public RolloverConversionHolder withCustomExchangeRate(boolean customExchangeRate) {
    this.customExchangeRate = customExchangeRate;
    return this;
  }

  public String getPoLineId() {
    return poLineId;
  }

  public ExchangeRate getExchangeRate() {
    return exchangeRate;
  }

  public boolean isCustomExchangeRate() {
    return customExchangeRate;
  }
}
