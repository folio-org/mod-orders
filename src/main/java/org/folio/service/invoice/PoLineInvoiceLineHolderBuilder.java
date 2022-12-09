package org.folio.service.invoice;

import java.util.HashMap;
import java.util.List;
import java.util.Map;
import java.util.Objects;
import java.util.Optional;
import java.util.stream.Collectors;

import org.apache.commons.collections4.CollectionUtils;
import org.apache.http.HttpStatus;
import org.folio.models.PoLineInvoiceLineHolder;
import org.folio.rest.acq.model.finance.Transaction;
import org.folio.rest.acq.model.invoice.InvoiceLine;
import org.folio.rest.core.exceptions.ErrorCodes;
import org.folio.rest.core.exceptions.HttpException;
import org.folio.rest.core.models.RequestContext;
import org.folio.rest.jaxrs.model.CompositePoLine;
import org.folio.rest.jaxrs.model.Error;
import org.folio.rest.jaxrs.model.FundDistribution;
import org.folio.service.finance.FiscalYearService;
import org.folio.service.finance.transaction.EncumbranceService;

import io.vertx.core.Future;
import io.vertx.core.json.JsonObject;

public class PoLineInvoiceLineHolderBuilder {
  private static final List<InvoiceLine.InvoiceLineStatus> EDITABLE_STATUSES = List.of(InvoiceLine.InvoiceLineStatus.OPEN, InvoiceLine.InvoiceLineStatus.REVIEWED);
  private static final List<InvoiceLine.InvoiceLineStatus> MOVABLE_STATUSES = List.of(InvoiceLine.InvoiceLineStatus.PAID, InvoiceLine.InvoiceLineStatus.CANCELLED);
  private final FiscalYearService fiscalYearService;
  private final InvoiceLineService invoiceLineService;
  private final EncumbranceService encumbranceService;

  public PoLineInvoiceLineHolderBuilder(FiscalYearService fiscalYearService, InvoiceLineService invoiceLineService,
                                        EncumbranceService encumbranceService) {
    this.fiscalYearService = fiscalYearService;
    this.invoiceLineService = invoiceLineService;
    this.encumbranceService = encumbranceService;
  }

  public Future<PoLineInvoiceLineHolder> buildHolder(CompositePoLine compOrderLine, JsonObject poLineFromStorage, RequestContext requestContext) {
    PoLineInvoiceLineHolder holder = new PoLineInvoiceLineHolder(compOrderLine, poLineFromStorage);
    return invoiceLineService.getInvoiceLinesByOrderLineId(compOrderLine.getId(), requestContext)
      .onSuccess(holder::withInvoiceLines)
      .compose(aResult -> CollectionUtils.isEmpty(holder.getInvoiceLines()) ? Future.succeededFuture(holder) :
        Future.succeededFuture(getOpenOrReviewedInvoiceLines(holder.getInvoiceLines()))
          .onSuccess(holder::withOpenOrReviewedInvoiceLines)
          .compose(aVoid -> validateAndRetrievePaidInvoiceLines(compOrderLine, holder.getInvoiceLines(), requestContext))
          .onSuccess(holder::withCurrentYearPaidInvoiceLines)
          .map(aVoid -> holder));
  }

  private Future<List<InvoiceLine>> validateAndRetrievePaidInvoiceLines(CompositePoLine compOrderLine, List<InvoiceLine> invoiceLines, RequestContext requestContext) {
    Optional<FundDistribution> optionalFundDistribution = compOrderLine.getFundDistribution().stream().findFirst();
    if (optionalFundDistribution.isPresent()) {
      String poLineFundId = optionalFundDistribution.get().getFundId();
      return getCurrentFiscalYearInvoiceLines(poLineFundId, invoiceLines, requestContext)
        .map(currentYearInvoiceLines -> {
          validateInvoiceLinesStatus(currentYearInvoiceLines);
          return getPaidOrCancelledInvoiceLines(currentYearInvoiceLines);
        });
    } else {
      return Future.succeededFuture(); //Should ask Dennis what we should do in case of POL doesn't have fund distribution (Delete the comment after receiving a response)
    }
  }

  private Future<List<InvoiceLine>> getCurrentFiscalYearInvoiceLines(String poLineFundId, List<InvoiceLine> invoiceLines, RequestContext requestContext) {
    Map<String, String> encumbranceIdsByInvoiceLineId = getEncumbranceIdsByInvoiceLines(invoiceLines);
    List<String> encumbranceIds = encumbranceIdsByInvoiceLineId.values().stream().filter(Objects::nonNull).collect(Collectors.toList());

    // One low-cost way to determine the fiscal year of an invoice line is to extract the fiscalYearId field from the encumbrance body.
    return encumbranceService.getEncumbrancesByIds(encumbranceIds, requestContext)
      // We need to determine the current fiscal year of CompositePOL to cut off invoices that belong to other fiscal years.
      .compose(encumbrances -> fiscalYearService.getCurrentFiscalYearByFundId(poLineFundId, requestContext)
        .map(fiscalYear -> {
        // The list contains all encumbranceIds belonging to the current fiscal year.
        List<String> currentFYEncumbranceIds = encumbrances
          .stream()
          .filter(encumbrance -> Objects.equals(encumbrance.getFiscalYearId(), fiscalYear.getId()))
          .map(Transaction::getId)
          .collect(Collectors.toList());

          return invoiceLines.stream()
            .filter(invoiceLine -> {
              String encumbranceId = encumbranceIdsByInvoiceLineId.get(invoiceLine.getId());
              // When encumbranceId is null encumbrance for the invoice line has not yet been created.
              // We can assign it to the current fiscal year.
              return currentFYEncumbranceIds.contains(encumbranceId) || Objects.isNull(encumbranceId);
            })
            .collect(Collectors.toList());
      }));
  }

  private void validateInvoiceLinesStatus(List<InvoiceLine> invoiceLines) {
    List<InvoiceLine> approvedInvoices = filterInvoiceLinesByStatuses(invoiceLines, List.of(InvoiceLine.InvoiceLineStatus.APPROVED));
    if (CollectionUtils.isNotEmpty(approvedInvoices)) {
      String invoiceLineIds = approvedInvoices.stream().map(InvoiceLine::getId).collect(Collectors.joining(", "));
      String message = String.format(ErrorCodes.PO_LINE_HAS_RELATED_APPROVED_INVOICE_ERROR.getDescription(), invoiceLineIds);
      throw new HttpException(HttpStatus.SC_FORBIDDEN, new Error().withCode(ErrorCodes.PO_LINE_HAS_RELATED_APPROVED_INVOICE_ERROR.getCode()).withMessage(message));
    }
  }

  private Map<String, String> getEncumbranceIdsByInvoiceLines(List<InvoiceLine> invoiceLines) {
    return invoiceLines.stream()
      .map(invoiceLine -> invoiceLine.getFundDistributions().stream().findFirst())
      .flatMap(Optional::stream)
      .collect(HashMap::new, (m, v) -> m.put(v.getInvoiceLineId(), v.getEncumbrance()), HashMap::putAll);
  }

  private List<InvoiceLine> getOpenOrReviewedInvoiceLines(List<InvoiceLine> invoiceLines) {
    return filterInvoiceLinesByStatuses(invoiceLines, EDITABLE_STATUSES);
  }

  private List<InvoiceLine> getPaidOrCancelledInvoiceLines(List<InvoiceLine> invoiceLines) {
    return filterInvoiceLinesByStatuses(invoiceLines, MOVABLE_STATUSES);
  }

  private List<InvoiceLine> filterInvoiceLinesByStatuses(List<InvoiceLine> invoiceLines, List<InvoiceLine.InvoiceLineStatus> invoiceLineStatuses) {
    return invoiceLines.stream()
      .filter(invoiceLine -> invoiceLineStatuses.contains(invoiceLine.getInvoiceLineStatus()))
      .collect(Collectors.toList());
  }

}
