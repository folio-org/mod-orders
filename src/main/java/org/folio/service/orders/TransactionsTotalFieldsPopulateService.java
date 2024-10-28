package org.folio.service.orders;

import java.util.List;
import java.util.Map;
import java.util.Optional;
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
public class TransactionsTotalFieldsPopulateService implements CompositeOrderDynamicDataPopulateService {

  private final InvoiceService invoiceService;
  private final InvoiceLineService invoiceLineService;

  public TransactionsTotalFieldsPopulateService(InvoiceService invoiceService, InvoiceLineService invoiceLineService) {
    this.invoiceService = invoiceService;
    this.invoiceLineService = invoiceLineService;
  }

  @Override
  public Future<CompositeOrderRetrieveHolder> populate(CompositeOrderRetrieveHolder holder, RequestContext requestContext) {
    return Optional.of(holder)
      .map(CompositeOrderRetrieveHolder::getFiscalYear)
      .map(v -> withTotalFields(holder, requestContext))
      .orElseGet(() -> Future.succeededFuture(holder.withTotalExpended(0d).withTotalCredited(0d).withTotalEncumbered(0d)))
      .onFailure(v -> log.error("Failed at {}.{}", this.getClass().getName(), Thread.currentThread().getStackTrace()[0]));
  }

  private Future<CompositeOrderRetrieveHolder> withTotalFields(CompositeOrderRetrieveHolder holder, RequestContext requestContext) {
    return invoiceService.getInvoicesByOrderId(holder.getOrderId(), requestContext)
      .compose(invoices -> getInvoiceLinesByInvoiceIds(invoices, requestContext)
        .map(invoiceLines -> groupInvoiceLinesByInvoices(invoices, invoiceLines))
        .map(invoiceToInvoiceLinesMap -> populateTotalFields(holder, invoiceToInvoiceLinesMap)));
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
      .withTotalEncumbered(getTotalAmountSum(invoiceToInvoiceLinesMap, Math::abs))
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
      .map(invoiceLine -> amountMapper.apply(invoiceLine.getTotal()))
      .map(amount -> Money.of(amount, currency))
      .reduce(Money::add)
      .map(amount -> amount.with(MonetaryOperators.rounding()).getNumber().doubleValue())
      .orElse(0d);
  }

}
