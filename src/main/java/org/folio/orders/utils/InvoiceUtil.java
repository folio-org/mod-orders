package org.folio.orders.utils;

import org.folio.rest.acq.model.invoice.InvoiceLine;

import java.util.List;
import java.util.stream.Collectors;

public class InvoiceUtil {
  private InvoiceUtil() {}

  public static List<InvoiceLine> filterInvoiceLinesByStatuses(List<InvoiceLine> invoiceLines,
                                                               List<InvoiceLine.InvoiceLineStatus> invoiceLineStatuses) {
    return invoiceLines.stream()
      .filter(invoiceLine -> invoiceLineStatuses.contains(invoiceLine.getInvoiceLineStatus()))
      .collect(Collectors.toList());
  }
}
