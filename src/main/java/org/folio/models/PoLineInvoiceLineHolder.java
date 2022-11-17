package org.folio.models;

import org.folio.rest.acq.model.invoice.InvoiceLine;
import org.folio.rest.jaxrs.model.CompositePoLine;

import java.util.List;
import java.util.ArrayList;


public class PoLineInvoiceLineHolder {
  private CompositePoLine poLine;
  private List<InvoiceLine> invoiceLines;
  private List<InvoiceLine> openOrReviewedInvoiceLines;
  private List<InvoiceLine> currentYearPaidInvoiceLines;

  public PoLineInvoiceLineHolder(CompositePoLine poLine) {
    this.poLine = poLine;
    this.invoiceLines = new ArrayList<>();
    this.openOrReviewedInvoiceLines = new ArrayList<>();
    this.currentYearPaidInvoiceLines = new ArrayList<>();
  }

  public PoLineInvoiceLineHolder withInvoiceLines(List<InvoiceLine> invoiceLines) {
    this.invoiceLines = new ArrayList<>(invoiceLines);
    return this;
  }

  public PoLineInvoiceLineHolder withOpenOrReviewedInvoiceLines(List<InvoiceLine> openOrReviewedInvoiceLines) {
    this.openOrReviewedInvoiceLines = new ArrayList<>(openOrReviewedInvoiceLines);
    return this;
  }

  public PoLineInvoiceLineHolder withCurrentYearPaidInvoiceLines(List<InvoiceLine> currentYearPaidInvoiceLines) {
    this.currentYearPaidInvoiceLines = new ArrayList<>(currentYearPaidInvoiceLines);
    return this;
  }

  public CompositePoLine getPoLine() {
    return poLine;
  }

  public List<InvoiceLine> getInvoiceLines() {
    return invoiceLines;
  }

  public List<InvoiceLine> getOpenOrReviewedInvoiceLines() {
    return openOrReviewedInvoiceLines;
  }

  public List<InvoiceLine> getCurrentYearPaidInvoiceLines() {
    return currentYearPaidInvoiceLines;
  }

}
