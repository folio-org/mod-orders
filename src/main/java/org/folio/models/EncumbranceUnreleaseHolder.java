package org.folio.models;

import org.folio.rest.acq.model.finance.Encumbrance;
import org.folio.rest.acq.model.finance.Transaction;
import org.folio.rest.acq.model.invoice.InvoiceLine;

import java.util.List;

public class EncumbranceUnreleaseHolder {

  private List<Transaction> encumbrances;
  private List<Transaction> pendingPayments;
  private List<Transaction> payments;
  private List<InvoiceLine> invoiceLines;

  public EncumbranceUnreleaseHolder() {
  }

  public List<Transaction> getEncumbrances() {
    return encumbrances;
  }

  public List<String> getEncumbranceIds() {
    return this.encumbrances.stream()
      .map(Transaction::getId)
      .distinct()
      .toList();
  }

  public List<String> getPoLineIds() {
    return this.encumbrances.stream().map(Transaction::getEncumbrance)
      .map(Encumbrance::getSourcePoLineId)
      .distinct()
      .toList();
  }

  public List<Transaction> getPendingPayments() {
    return pendingPayments;
  }

  public List<Transaction> getPayments() {
    return payments;
  }

  public List<InvoiceLine> getInvoiceLines() {
    return invoiceLines;
  }

  public EncumbranceUnreleaseHolder withEncumbrances(List<Transaction> encumbrances) {
    this.encumbrances = encumbrances;
    return this;
  }

  public EncumbranceUnreleaseHolder withPendingPayments(List<Transaction> pendingPayments) {
    this.pendingPayments = pendingPayments;
    return this;
  }

  public EncumbranceUnreleaseHolder withPayments(List<Transaction> payments) {
    this.payments = payments;
    return this;
  }

  public EncumbranceUnreleaseHolder withInvoiceLines(List<InvoiceLine> invoiceLines) {
    this.invoiceLines = invoiceLines;
    return this;
  }
}
