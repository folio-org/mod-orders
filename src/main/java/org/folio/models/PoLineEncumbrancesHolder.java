package org.folio.models;

import java.util.ArrayList;
import java.util.List;

import org.folio.rest.acq.model.finance.Transaction;
import org.folio.rest.jaxrs.model.EncumbranceRollover;
import org.folio.rest.jaxrs.model.PoLine;

public class PoLineEncumbrancesHolder {
  private final PoLine poLine;
  private EncumbranceRollover.BasedOn baseOn;
  private List<Transaction> encumbrances;

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

  public PoLineEncumbrancesHolder withBasedOn(EncumbranceRollover.BasedOn baseOn) {
    this.baseOn = baseOn;
    return this;
  }

  public PoLine getPoLine() {
    return this.poLine;
  }

  public List<Transaction> getEncumbrances() {
    return this.encumbrances;
  }

  public EncumbranceRollover.BasedOn getBaseOn() {
    return this.baseOn;
  }
}
