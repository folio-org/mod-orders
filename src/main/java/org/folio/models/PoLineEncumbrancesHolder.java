package org.folio.models;

import java.util.ArrayList;
import java.util.List;

import org.folio.rest.acq.model.finance.Encumbrance;
import org.folio.rest.acq.model.finance.Transaction;
import org.folio.rest.jaxrs.model.PoLine;

public class PoLineEncumbrancesHolder {
  private final PoLine poLine;
  private String orderType;
  private List<Transaction> prevEncumbrances;
  private List<Transaction> currEncumbrances;

  public PoLineEncumbrancesHolder(PoLine poLine) {
    this.poLine = poLine;
    this.prevEncumbrances = new ArrayList<>();
    this.currEncumbrances = new ArrayList<>();
  }

  public PoLineEncumbrancesHolder addPrevEncumbrance(Transaction prevEncumbrance) {
    this.prevEncumbrances.add(prevEncumbrance);
    return this;
  }

  public PoLineEncumbrancesHolder withPrevEncumbrances(List<Transaction> prevEncumbrances) {
    this.prevEncumbrances = new ArrayList<>(prevEncumbrances);
    return this;
  }

  public PoLineEncumbrancesHolder addCurrEncumbrance(Transaction currEncumbrance) {
    this.currEncumbrances.add(currEncumbrance);
    return this;
  }

  public PoLineEncumbrancesHolder withCurrEncumbrances(List<Transaction> currEncumbrances) {
    this.currEncumbrances = new ArrayList<>(currEncumbrances);
    return this;
  }

  public PoLineEncumbrancesHolder withOrderType(String orderType) {
    this.orderType = orderType;
    return this;
  }

  public PoLine getPoLine() {
    return this.poLine;
  }

  public List<Transaction> getPrevEncumbrances() {
    return this.prevEncumbrances;
  }

  public List<Transaction> getCurrEncumbrances() {
    return this.currEncumbrances;
  }

  public String getOrderType() {
    return this.orderType;
  }
}
