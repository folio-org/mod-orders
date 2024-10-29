package org.folio.service.orders;

import java.util.List;
import java.util.Map;
import java.util.function.Function;
import java.util.stream.Collectors;

import lombok.extern.log4j.Log4j2;
import one.util.streamex.StreamEx;
import org.apache.commons.lang3.StringUtils;
import org.folio.models.CompositeOrderRetrieveHolder;
import org.folio.rest.acq.model.finance.FiscalYear;
import org.folio.rest.acq.model.finance.Transaction;
import org.folio.rest.acq.model.invoice.Invoice;
import org.folio.rest.acq.model.invoice.InvoiceLine;
import org.folio.rest.core.models.RequestContext;
import org.folio.service.finance.transaction.TransactionService;
import org.folio.service.invoice.InvoiceLineService;
import org.folio.service.invoice.InvoiceService;
import org.javamoney.moneta.Money;
import org.javamoney.moneta.function.MonetaryOperators;

import io.vertx.core.Future;

import static org.folio.orders.utils.HelperUtils.collectResultsOnSuccess;

@Log4j2
public class CompositeOrderTotalFieldsPopulateService implements CompositeOrderDynamicDataPopulateService {

  private final TransactionService transactionService;
  private final InvoiceService invoiceService;
  private final InvoiceLineService invoiceLineService;

  public CompositeOrderTotalFieldsPopulateService(TransactionService transactionService,
                                                  InvoiceService invoiceService,
                                                  InvoiceLineService invoiceLineService) {
    this.transactionService = transactionService;
    this.invoiceService = invoiceService;
    this.invoiceLineService = invoiceLineService;
  }

  @Override
  public Future<CompositeOrderRetrieveHolder> populate(CompositeOrderRetrieveHolder holder, RequestContext requestContext) {
    if (holder.getFiscalYear() == null) {
      return Future.succeededFuture(holder.withTotalExpended(0d).withTotalCredited(0d).withTotalEncumbered(0d));
    }
    // We need to fetch invoices, invoice lines and transactions in order to calculate total fields
    // totalEncumbered = sum of transactions amounts
    // totalExpended = sum of invoice lines positive total values
    // totalCredited = absolute sum of invoice lines negative total values
    var query = "transactionType==Encumbrance AND encumbrance.sourcePurchaseOrderId==%s AND fiscalYearId==%s"
      .formatted(holder.getOrderId(), holder.getFiscalYearId());
    return invoiceService.getInvoicesByOrderId(holder.getOrderId(), requestContext)
      .compose(invoices -> getInvoiceLinesByInvoiceIds(invoices, holder.getFiscalYear(), requestContext)
        .map(invoiceLines -> groupInvoiceLinesByInvoices(invoices, invoiceLines))
        .compose(invoiceToInvoiceLinesMap -> transactionService.getTransactions(query, requestContext)
          .map(transactions -> populateTotalFields(holder, invoiceToInvoiceLinesMap, transactions))))
      .onFailure(t -> log.error("Failed to populate order's '{}' total fields with fiscal year '{}' from invoice lines",
        holder.getOrderId(), holder.getFiscalYearId(), t));
  }

  private Future<List<InvoiceLine>> getInvoiceLinesByInvoiceIds(List<Invoice> invoices, FiscalYear fiscalYear, RequestContext requestContext) {
    return collectResultsOnSuccess(invoices.stream()
      .filter(invoice -> StringUtils.equals(invoice.getFiscalYearId(), fiscalYear.getId()))
      .map(invoice -> invoiceLineService.getInvoiceLinesByInvoiceId(invoice.getId(), requestContext)).toList())
      .map(invoiceLinesLists -> invoiceLinesLists.stream().flatMap(List::stream).toList());
  }

  private Map<Invoice, List<InvoiceLine>> groupInvoiceLinesByInvoices(List<Invoice> invoices, List<InvoiceLine> invoiceLines) {
    return invoices.stream()
      .collect(Collectors.toMap(invoice -> invoice, invoice -> invoiceLines.stream()
        .filter(invoiceLine -> invoiceLine.getInvoiceId().equals(invoice.getId()))
        .collect(Collectors.toList())));
  }

  private CompositeOrderRetrieveHolder populateTotalFields(CompositeOrderRetrieveHolder holder, Map<Invoice, List<InvoiceLine>> invoiceToInvoiceLinesMap, List<Transaction> transactions) {
    return holder
      .withTotalEncumbered(getTotalAmountSum(transactions))
      .withTotalExpended(getTotalAmountSum(invoiceToInvoiceLinesMap, total -> Double.max(total, 0)))
      .withTotalCredited(getTotalAmountSum(invoiceToInvoiceLinesMap, total -> Double.min(total, 0)));
  }

  private double getTotalAmountSum(List<Transaction> transactions) {
    return getTotalAmount(transactions, Transaction::getCurrency, Transaction::getAmount);
  }

  private double getTotalAmountSum(Map<Invoice, List<InvoiceLine>> invoiceToInvoiceLinesMap, Function<Double, Double> amountMapper) {
    return StreamEx.of(invoiceToInvoiceLinesMap.entrySet())
      .mapToDouble(entry -> getTotalAmount(entry.getValue(),
        invoiceLine -> entry.getKey().getCurrency(),
        invoiceLine -> Math.abs(amountMapper.apply(invoiceLine.getTotal()))))
      .sum();
  }

  private <T> double getTotalAmount(List<T> entities, Function<T, String> currencyExtractor, Function<T, Double> amountExtractor) {
    return StreamEx.of(entities)
      .map(entity -> Money.of(amountExtractor.apply(entity), currencyExtractor.apply(entity)))
      .reduce(Money::add)
      .map(amount -> amount.with(MonetaryOperators.rounding()).getNumber().doubleValue())
      .orElse(0d);
  }

}