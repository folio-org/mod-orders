package org.folio.models;

import java.util.ArrayList;
import java.util.List;

import org.folio.rest.acq.model.invoice.InvoiceLine;
import org.folio.rest.jaxrs.model.CompositePoLine;
import org.folio.rest.jaxrs.model.PoLine;

import io.vertx.core.json.JsonObject;


public class PoLineInvoiceLineHolder {
  private CompositePoLine poLineFromRequest;
  private PoLine poLineFromStorage;
  private List<InvoiceLine> invoiceLines;
  private List<InvoiceLine> openOrReviewedInvoiceLines;

  public PoLineInvoiceLineHolder(CompositePoLine poLineFromRequest, JsonObject poLineFromStorage) {
    this.poLineFromRequest = poLineFromRequest;
    this.poLineFromStorage = poLineFromStorage.mapTo(PoLine.class);
    this.invoiceLines = new ArrayList<>();
    this.openOrReviewedInvoiceLines = new ArrayList<>();
  }

  public PoLineInvoiceLineHolder withInvoiceLines(List<InvoiceLine> invoiceLines) {
    this.invoiceLines = new ArrayList<>(invoiceLines);
    return this;
  }

  public PoLineInvoiceLineHolder withOpenOrReviewedInvoiceLines(List<InvoiceLine> openOrReviewedInvoiceLines) {
    this.openOrReviewedInvoiceLines = new ArrayList<>(openOrReviewedInvoiceLines);
    return this;
  }

  public CompositePoLine getPoLineFromRequest() {
    return poLineFromRequest;
  }

  public PoLine getPoLineFromStorage() {
    return poLineFromStorage;
  }

  public List<InvoiceLine> getInvoiceLines() {
    return invoiceLines;
  }

  public List<InvoiceLine> getOpenOrReviewedInvoiceLines() {
    return openOrReviewedInvoiceLines;
  }

}
