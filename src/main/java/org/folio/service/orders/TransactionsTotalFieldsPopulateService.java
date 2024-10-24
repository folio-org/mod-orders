package org.folio.service.orders;

import java.util.List;
import java.util.Optional;
import java.util.function.Function;

import lombok.extern.log4j.Log4j2;
import one.util.streamex.StreamEx;
import org.folio.models.CompositeOrderRetrieveHolder;
import org.folio.rest.acq.model.finance.Transaction;
import org.folio.rest.acq.model.invoice.Invoice;
import org.folio.rest.core.models.RequestContext;
import org.folio.service.finance.transaction.TransactionService;
import org.folio.service.invoice.InvoiceService;
import org.javamoney.moneta.Money;
import org.javamoney.moneta.function.MonetaryOperators;

import io.vertx.core.Future;

@Log4j2
public class TransactionsTotalFieldsPopulateService implements CompositeOrderDynamicDataPopulateService {

  private final TransactionService transactionService;
  private final InvoiceService invoiceService;

  public TransactionsTotalFieldsPopulateService(TransactionService transactionService, InvoiceService invoiceService) {
    this.transactionService = transactionService;
    this.invoiceService = invoiceService;
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
    var query = "transactionType==Encumbrance AND encumbrance.sourcePurchaseOrderId==%s AND fiscalYearId==%s"
      .formatted(holder.getOrderId(), holder.getFiscalYearId());
    return transactionService.getTransactions(query, requestContext)
      .map(transactions -> holder
        .withTotalEncumbered(getTransactionsTotal(transactions, Transaction::getAmount))
        .withTotalExpended(getTransactionsTotal(transactions, transaction -> transaction.getEncumbrance().getAmountExpended()))
        .withTotalCredited(getTransactionsTotal(transactions, transaction -> transaction.getEncumbrance().getAmountCredited())))
      .compose(v -> holder.getOrder().getTotalExpended() != 0
        ? Future.succeededFuture(holder)
        : invoiceService.getInvoicesByOrderId(holder.getOrderId(), requestContext)
            .map(invoices -> holder.withTotalExpended(getInvoicesTotal(invoices, holder.getFiscalYearId(), Invoice::getTotal))));
  }

  private double getTransactionsTotal(List<Transaction> transactions, Function<Transaction, Double> amountExtractor) {
    return getMonetaryAmount(StreamEx.of(transactions), Transaction::getCurrency, amountExtractor);
  }

  private double getInvoicesTotal(List<Invoice> invoices, String fiscalYearId, Function<Invoice, Double> amountExtractor) {
    var filteredInvoices = StreamEx.of(invoices)
      .filter(invoice -> invoice.getFiscalYearId().equals(fiscalYearId));
    return getMonetaryAmount(filteredInvoices, Invoice::getCurrency, amountExtractor);
  }

  private <T> double getMonetaryAmount(StreamEx<T> entities, Function<T, String> currencyExtractor, Function<T, Double> amountExtractor) {
    return entities
      .map(entity -> Money.of(amountExtractor.apply(entity), currencyExtractor.apply(entity)))
      .reduce(Money::add)
      .map(amount -> amount.with(MonetaryOperators.rounding())
        .getNumber().doubleValue())
      .orElse(0d);
  }

}
