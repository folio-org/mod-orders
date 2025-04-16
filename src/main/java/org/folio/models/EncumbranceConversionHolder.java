package org.folio.models;

import javax.money.convert.CurrencyConversion;
import java.util.List;

public class EncumbranceConversionHolder {

  private List<? extends EncumbranceRelationsHolder> encumbranceRelationsHolders;
  private CurrencyConversion conversion;
  private CurrencyConversion reverseConversion;

  public List<? extends EncumbranceRelationsHolder> getEncumbranceRelationsHolders() {
    return encumbranceRelationsHolders;
  }

  public CurrencyConversion getConversion() {
    return conversion;
  }

  public CurrencyConversion getReverseConversion() {
    return reverseConversion;
  }

  public EncumbranceConversionHolder withHolders(List<? extends EncumbranceRelationsHolder> holders) {
    this.encumbranceRelationsHolders = holders;
    return this;
  }

  public EncumbranceConversionHolder withConversion(CurrencyConversion conversion) {
    this.conversion = conversion;
    return this;
  }

  public EncumbranceConversionHolder withReverseConversion(CurrencyConversion reverseConversion) {
    this.reverseConversion = reverseConversion;
    return this;
  }
}
