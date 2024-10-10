package org.folio.orders.utils;

import org.folio.rest.acq.model.invoice.InvoiceLine;

import java.util.List;

public class InvoiceUtil {
  private InvoiceUtil() {}

  public static List<InvoiceLine> filterInvoiceLinesByStatuses(List<InvoiceLine> invoiceLines,
                                                               List<InvoiceLine.InvoiceLineStatus> invoiceLineStatuses) {
    return invoiceLines.stream()
      .filter(invoiceLine -> invoiceLineStatuses.contains(invoiceLine.getInvoiceLineStatus()))
      .toList();
  }
}
