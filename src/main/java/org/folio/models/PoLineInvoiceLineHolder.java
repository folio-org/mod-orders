package org.folio.models;

import java.util.ArrayList;
import java.util.List;

import org.folio.rest.acq.model.invoice.InvoiceLine;
import org.folio.rest.jaxrs.model.CompositePoLine;

import io.vertx.core.json.JsonObject;


public class PoLineInvoiceLineHolder {
  private CompositePoLine poLine;
  private JsonObject poLineFromStorage;
  private List<InvoiceLine> invoiceLines;
  private List<InvoiceLine> openOrReviewedInvoiceLines;
  private List<InvoiceLine> currentYearPaidInvoiceLines;

  public PoLineInvoiceLineHolder(CompositePoLine poLine, JsonObject poLineFromStorage) {
    this.poLine = poLine;
    this.poLineFromStorage = poLineFromStorage;
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

  public JsonObject getPoLineFromStorage() {
    return poLineFromStorage;
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
