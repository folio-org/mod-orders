package org.folio.service.orders;

import java.util.List;
import java.util.Map;
import java.util.function.Function;
import java.util.stream.Collectors;

import lombok.extern.log4j.Log4j2;
import one.util.streamex.StreamEx;
import org.folio.models.CompositeOrderRetrieveHolder;
import org.folio.rest.acq.model.invoice.Invoice;
import org.folio.rest.acq.model.invoice.InvoiceLine;
import org.folio.rest.core.models.RequestContext;
import org.folio.service.invoice.InvoiceLineService;
import org.folio.service.invoice.InvoiceService;
import org.javamoney.moneta.Money;
import org.javamoney.moneta.function.MonetaryOperators;

import io.vertx.core.Future;

import static org.folio.orders.utils.HelperUtils.collectResultsOnSuccess;

@Log4j2
public class InvoicesTotalFieldsPopulateService implements CompositeOrderDynamicDataPopulateService {

  private final InvoiceService invoiceService;
  private final InvoiceLineService invoiceLineService;

  public InvoicesTotalFieldsPopulateService(InvoiceService invoiceService, InvoiceLineService invoiceLineService) {
    this.invoiceService = invoiceService;
    this.invoiceLineService = invoiceLineService;
  }

  @Override
  public Future<CompositeOrderRetrieveHolder> populate(CompositeOrderRetrieveHolder holder, RequestContext requestContext) {
    if (holder.getFiscalYear() == null) {
      return Future.succeededFuture(holder.withTotalExpended(0d).withTotalCredited(0d).withTotalEncumbered(0d));
    }
    return invoiceService.getInvoicesByOrderId(holder.getOrderId(), requestContext)
      .compose(invoices -> getInvoiceLinesByInvoiceIds(invoices, requestContext)
        .map(invoiceLines -> groupInvoiceLinesByInvoices(invoices, invoiceLines))
        .map(invoiceToInvoiceLinesMap -> populateTotalFields(holder, invoiceToInvoiceLinesMap)))
      .onFailure(t -> log.error("Failed to populate order's '{}' total fields with fiscal year '{}' from invoice lines",
        holder.getOrderId(), holder.getFiscalYearId(), t));
  }

  private Future<List<InvoiceLine>> getInvoiceLinesByInvoiceIds(List<Invoice> invoices, RequestContext requestContext) {
    return collectResultsOnSuccess(invoices.stream()
      .map(invoice -> invoiceLineService.getInvoiceLinesByInvoiceId(invoice.getId(), requestContext))
      .toList())
      .map(invoiceLinesLists -> invoiceLinesLists.stream().flatMap(List::stream).toList());
  }

  private Map<Invoice, List<InvoiceLine>> groupInvoiceLinesByInvoices(List<Invoice> invoices, List<InvoiceLine> invoiceLines) {
    return invoices.stream()
      .collect(Collectors.toMap(invoice -> invoice, invoice -> invoiceLines.stream()
        .filter(invoiceLine -> invoiceLine.getInvoiceId().equals(invoice.getId()))
        .collect(Collectors.toList())));
  }

  private CompositeOrderRetrieveHolder populateTotalFields(CompositeOrderRetrieveHolder holder, Map<Invoice, List<InvoiceLine>> invoiceToInvoiceLinesMap) {
    return holder
      .withTotalEncumbered(getTotalAmountSum(invoiceToInvoiceLinesMap, Function.identity()))
      .withTotalExpended(getTotalAmountSum(invoiceToInvoiceLinesMap, total -> Double.max(total, 0)))
      .withTotalCredited(getTotalAmountSum(invoiceToInvoiceLinesMap, total -> Double.min(total, 0)));
  }

  private double getTotalAmountSum(Map<Invoice, List<InvoiceLine>> invoiceToInvoiceLinesMap, Function<Double, Double> amountMapper) {
    return StreamEx.of(invoiceToInvoiceLinesMap.entrySet())
      .mapToDouble(entry -> getTotalAmount(entry.getValue(), entry.getKey().getCurrency(), amountMapper))
      .sum();

  }

  private double getTotalAmount(List<InvoiceLine> invoiceLines, String currency, Function<Double, Double> amountMapper) {
    return StreamEx.of(invoiceLines)
      .map(invoiceLine -> Math.abs(amountMapper.apply(invoiceLine.getTotal())))
      .map(amount -> Money.of(amount, currency))
      .reduce(Money::add)
      .map(amount -> amount.with(MonetaryOperators.rounding()).getNumber().doubleValue())
      .orElse(0d);
  }

}
