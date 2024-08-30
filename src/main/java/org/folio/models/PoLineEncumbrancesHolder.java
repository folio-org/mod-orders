package org.folio.models;

import java.util.ArrayList;
import java.util.List;

import javax.money.convert.CurrencyConversion;

import org.folio.rest.acq.model.finance.Transaction;
import org.folio.rest.jaxrs.model.PoLine;

public class PoLineEncumbrancesHolder {
  private final PoLine poLine;
  private CurrencyConversion currencyConversion;
  private List<Transaction> encumbrances;
  private String systemCurrency;

  public PoLineEncumbrancesHolder(PoLine poLine) {
    this.poLine = poLine;
    this.encumbrances = new ArrayList<>();
  }

  public PoLineEncumbrancesHolder addEncumbrance(Transaction encumbrance) {
    this.encumbrances.add(encumbrance);
    return this;
  }

  public PoLineEncumbrancesHolder withEncumbrances(List<Transaction> encumbrances) {
    this.encumbrances = new ArrayList<>(encumbrances);
    return this;
  }

  public PoLineEncumbrancesHolder withCurrencyConversion(CurrencyConversion currencyConversion) {
    this.currencyConversion = currencyConversion;
    return this;
  }

  public PoLineEncumbrancesHolder withSystemCurrency(String systemCurrency) {
    this.systemCurrency = systemCurrency;
    return this;
  }


  public PoLine getPoLine() {
    return this.poLine;
  }

  public List<Transaction> getEncumbrances() {
    return this.encumbrances;
  }

  public CurrencyConversion getCurrencyConversion() {
    return currencyConversion;
  }

  // System currency is the reference currency that will
  // be used to create encumbrance transactions in the Finance app
  public String getSystemCurrency() {
    return systemCurrency;
  }
}
