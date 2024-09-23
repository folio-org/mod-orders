package org.folio.service.invoice;

import java.util.List;
import java.util.stream.Collectors;

import org.apache.commons.collections4.CollectionUtils;
import org.folio.models.PoLineInvoiceLineHolder;
import org.folio.rest.acq.model.invoice.InvoiceLine;
import org.folio.rest.core.models.RequestContext;
import org.folio.rest.jaxrs.model.CompositePoLine;
import org.folio.rest.jaxrs.model.PoLine;

import io.vertx.core.Future;

public class PoLineInvoiceLineHolderBuilder {
  private static final List<InvoiceLine.InvoiceLineStatus> EDITABLE_STATUSES = List.of(InvoiceLine.InvoiceLineStatus.OPEN, InvoiceLine.InvoiceLineStatus.REVIEWED);
  private final InvoiceLineService invoiceLineService;

  public PoLineInvoiceLineHolderBuilder(InvoiceLineService invoiceLineService) {
    this.invoiceLineService = invoiceLineService;
  }

  public Future<PoLineInvoiceLineHolder> buildHolder(CompositePoLine compOrderLine, PoLine poLineFromStorage, RequestContext requestContext) {
    var holder = new PoLineInvoiceLineHolder(compOrderLine, poLineFromStorage);
    return invoiceLineService.getInvoiceLinesByOrderLineId(compOrderLine.getId(), requestContext)
      .onSuccess(holder::withInvoiceLines)
      .compose(aResult -> CollectionUtils.isEmpty(holder.getInvoiceLines()) ? Future.succeededFuture(holder) :
        Future.succeededFuture(getOpenOrReviewedInvoiceLines(holder.getInvoiceLines()))
          .onSuccess(holder::withOpenOrReviewedInvoiceLines)
          .map(aVoid -> holder));
  }

  private static List<InvoiceLine> getOpenOrReviewedInvoiceLines(List<InvoiceLine> invoiceLines) {
    return filterInvoiceLinesByStatuses(invoiceLines, EDITABLE_STATUSES);
  }

  public static List<InvoiceLine> filterInvoiceLinesByStatuses(List<InvoiceLine> invoiceLines, List<InvoiceLine.InvoiceLineStatus> invoiceLineStatuses) {
    return invoiceLines.stream()
      .filter(invoiceLine -> invoiceLineStatuses.contains(invoiceLine.getInvoiceLineStatus()))
      .collect(Collectors.toList());
  }

}
