package org.folio.models;

import java.util.Optional;

import javax.money.convert.CurrencyConversion;

import org.folio.rest.acq.model.finance.Budget;
import org.folio.rest.acq.model.finance.Transaction;
import org.folio.rest.jaxrs.model.CompositePoLine;
import org.folio.rest.jaxrs.model.CompositePurchaseOrder;
import org.folio.rest.jaxrs.model.FundDistribution;
import org.folio.rest.jaxrs.model.Ongoing;

public class EncumbranceRelationsHolder {

  private CompositePurchaseOrder purchaseOrder;
  private CompositePoLine poLine;
  private FundDistribution fundDistribution;
  private Transaction newEncumbrance;
  private Budget budget;
  private String currentFiscalYearId;
  private String currency;
  private CurrencyConversion poLineToFyConversion;
  private Transaction oldEncumbrance;
  private String ledgerId;
  private boolean restrictEncumbrance;

  public EncumbranceRelationsHolder withNewEncumbrance(Transaction transaction) {
    this.newEncumbrance = transaction;
    return this;
  }

  public Transaction getNewEncumbrance() {
    return newEncumbrance;
  }

  public CompositePoLine getPoLine() {
    return poLine;
  }

  public String getPoLineId() {
    return poLine.getId();
  }

  public String getOrderId() {
    return purchaseOrder.getId();
  }

  public CompositePurchaseOrder.OrderType getOrderType() {
    return purchaseOrder.getOrderType();
  }

  public boolean getReEncumber() {
    return purchaseOrder.getReEncumber();
  }

  public FundDistribution getFundDistribution() {
    return fundDistribution;
  }

  public CurrencyConversion getPoLineToFyConversion() {
    return poLineToFyConversion;
  }

  public EncumbranceRelationsHolder withPoLineToFyConversion(CurrencyConversion conversion) {
    this.poLineToFyConversion = conversion;
    return this;
  }

  public Transaction getOldEncumbrance() {
    return oldEncumbrance;
  }

  public EncumbranceRelationsHolder withOldEncumbrance(Transaction existingTransaction) {
    this.oldEncumbrance = existingTransaction;
    return this;
  }

  public EncumbranceRelationsHolder withPoLine(CompositePoLine poLine) {
    this.poLine = poLine;
    return this;
  }

  public EncumbranceRelationsHolder withFundDistribution(FundDistribution fundDistribution) {
    this.fundDistribution = fundDistribution;
    return this;
  }

  public CompositePurchaseOrder getPurchaseOrder() {
    return purchaseOrder;
  }

  public EncumbranceRelationsHolder withPurchaseOrder(CompositePurchaseOrder purchaseOrder) {
    this.purchaseOrder = purchaseOrder;
    return this;
  }

  public Ongoing getOngoing() {
    return purchaseOrder.getOngoing();
  }

  public String getFundId() {
    return fundDistribution.getFundId();
  }

  public String getExpenseClassId() {
    return fundDistribution.getExpenseClassId();
  }

  public EncumbranceRelationsHolder withBudget(Budget budget) {
    this.budget = budget;
    return this;
  }

  public Budget getBudget() {
    return budget;
  }

  public EncumbranceRelationsHolder withLedgerId(String ledgerId) {
    this.ledgerId = ledgerId;
    return this;
  }

  public String getLedgerId() {
    return ledgerId;
  }

  public EncumbranceRelationsHolder withRestrictEncumbrances(boolean restrictEncumbrance) {
    this.restrictEncumbrance = restrictEncumbrance;
    return this;
  }

  public boolean getRestrictEncumbrance() {
    return restrictEncumbrance;
  }

  public EncumbranceRelationsHolder withCurrentFiscalYearId(String fiscalYearId) {
    this.currentFiscalYearId = fiscalYearId;
    Optional.ofNullable(newEncumbrance).ifPresent(transaction -> transaction.setFiscalYearId(fiscalYearId));
    return this;
  }

  public EncumbranceRelationsHolder withCurrency(String currency) {
    this.currency = currency;
    Optional.ofNullable(newEncumbrance).ifPresent(transaction -> transaction.setCurrency(currency));
    return this;
  }

  public String getCurrency() {
    return currency;
  }

  public String getCurrentFiscalYearId() {
    return currentFiscalYearId;
  }
}
