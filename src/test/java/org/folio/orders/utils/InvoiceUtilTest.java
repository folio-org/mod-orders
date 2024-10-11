package org.folio.orders.utils;

import org.folio.CopilotGenerated;
import org.folio.rest.acq.model.invoice.InvoiceLine;
import org.folio.rest.acq.model.invoice.InvoiceLine.InvoiceLineStatus;
import org.junit.jupiter.api.Test;

import java.util.List;

import static org.junit.jupiter.api.Assertions.assertEquals;

@CopilotGenerated
public class InvoiceUtilTest {
  @Test
  void filterInvoiceLinesByStatuses_shouldReturnMatchingInvoiceLines() {
    InvoiceLine line1 = new InvoiceLine().withInvoiceLineStatus(InvoiceLineStatus.OPEN);
    InvoiceLine line2 = new InvoiceLine().withInvoiceLineStatus(InvoiceLineStatus.PAID);
    List<InvoiceLine> invoiceLines = List.of(line1, line2);
    List<InvoiceLineStatus> statuses = List.of(InvoiceLineStatus.OPEN);

    List<InvoiceLine> result = InvoiceUtil.filterInvoiceLinesByStatuses(invoiceLines, statuses);

    assertEquals(1, result.size());
    assertEquals(InvoiceLineStatus.OPEN, result.get(0).getInvoiceLineStatus());
  }

  @Test
  void filterInvoiceLinesByStatuses_shouldReturnEmptyList_whenNoMatchingStatuses() {
    InvoiceLine line1 = new InvoiceLine().withInvoiceLineStatus(InvoiceLineStatus.OPEN);
    InvoiceLine line2 = new InvoiceLine().withInvoiceLineStatus(InvoiceLineStatus.PAID);
    List<InvoiceLine> invoiceLines = List.of(line1, line2);
    List<InvoiceLineStatus> statuses = List.of(InvoiceLineStatus.CANCELLED);

    List<InvoiceLine> result = InvoiceUtil.filterInvoiceLinesByStatuses(invoiceLines, statuses);

    assertEquals(0, result.size());
  }

  @Test
  void filterInvoiceLinesByStatuses_shouldReturnAllInvoiceLines_whenAllStatusesMatch() {
    InvoiceLine line1 = new InvoiceLine().withInvoiceLineStatus(InvoiceLineStatus.OPEN);
    InvoiceLine line2 = new InvoiceLine().withInvoiceLineStatus(InvoiceLineStatus.PAID);
    List<InvoiceLine> invoiceLines = List.of(line1, line2);
    List<InvoiceLineStatus> statuses = List.of(InvoiceLineStatus.OPEN, InvoiceLineStatus.PAID);

    List<InvoiceLine> result = InvoiceUtil.filterInvoiceLinesByStatuses(invoiceLines, statuses);

    assertEquals(2, result.size());
  }

}
