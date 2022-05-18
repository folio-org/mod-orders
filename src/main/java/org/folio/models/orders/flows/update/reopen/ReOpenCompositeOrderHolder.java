package org.folio.models.orders.flows.update.reopen;

import java.util.List;

import org.folio.rest.acq.model.invoice.Invoice;
import org.folio.rest.acq.model.invoice.InvoiceLine;
import org.folio.rest.jaxrs.model.Piece;

public class ReOpenCompositeOrderHolder {
  private final String orderId;
  private List<Piece> orderLinePieces;
  private List<Invoice> orderInvoices;
  private List<InvoiceLine> orderLineInvoiceLines;

  public ReOpenCompositeOrderHolder(String orderId) {
    this.orderId = orderId;
  }

  public ReOpenCompositeOrderHolder withPieces(List<Piece> pieces) {
    this.orderLinePieces = pieces;
    return this;
  }

  public ReOpenCompositeOrderHolder withInvoiceLines(List<InvoiceLine> invoiceLines) {
    this.orderLineInvoiceLines = invoiceLines;
    return this;
  }

  public ReOpenCompositeOrderHolder withOrderInvoices(List<Invoice> invoices) {
    this.orderInvoices = invoices;
    return this;
  }

  public List<Piece> getOrderLinePieces() {
    return orderLinePieces;
  }

  public List<InvoiceLine> getOrderLineInvoiceLines() {
    return orderLineInvoiceLines;
  }

  public List<Invoice> getOrderInvoices() {
    return orderInvoices;
  }

  public String getOrderId() {
    return orderId;
  }
}
