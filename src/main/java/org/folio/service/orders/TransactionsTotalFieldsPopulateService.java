package org.folio.service.orders;

import java.util.List;
import java.util.Optional;
import java.util.concurrent.CompletableFuture;
import java.util.function.ToDoubleFunction;

import org.folio.models.CompositeOrderRetrieveHolder;
import org.folio.rest.acq.model.finance.Transaction;
import org.folio.rest.acq.model.finance.TransactionCollection;
import org.folio.rest.core.models.RequestContext;
import org.folio.service.finance.transaction.TransactionService;
import org.javamoney.moneta.Money;
import org.javamoney.moneta.function.MonetaryOperators;

public class TransactionsTotalFieldsPopulateService implements CompositeOrderDynamicDataPopulateService {

  private static final ToDoubleFunction<Transaction> GET_AMOUNT_EXPENDED_FUNCTION = transaction -> transaction.getEncumbrance()
    .getAmountExpended();
  private final TransactionService transactionService;

  public TransactionsTotalFieldsPopulateService(TransactionService transactionService) {
    this.transactionService = transactionService;
  }

  @Override
  public CompletableFuture<CompositeOrderRetrieveHolder> populate(CompositeOrderRetrieveHolder holder,
      RequestContext requestContext) {
    return Optional.of(holder)
      .map(CompositeOrderRetrieveHolder::getFiscalYear)
      .map(s -> withTotalFields(holder, requestContext))
      .orElseGet(() -> CompletableFuture.completedFuture(holder.withTotalExpended(0d)
        .withTotalEncumbered(0d)));
  }

  private CompletableFuture<CompositeOrderRetrieveHolder> withTotalFields(CompositeOrderRetrieveHolder holder,
      RequestContext requestContext) {
    return getCurrentEncumbrances(holder, requestContext).thenApply(transactions -> {
      holder.withTotalEncumbered(getTransactionsTotal(transactions, Transaction::getAmount));
      return holder.withTotalExpended(getTransactionsTotal(transactions, GET_AMOUNT_EXPENDED_FUNCTION));
    });
  }

  public CompletableFuture<List<Transaction>> getCurrentEncumbrances(CompositeOrderRetrieveHolder holder,
      RequestContext requestContext) {
    String query = String.format("transactionType==Encumbrance AND encumbrance.sourcePurchaseOrderId==%s AND fiscalYearId==%s",
        holder.getOrderId(), holder.getFiscalYearId());
    return transactionService.getTransactions(query, 0, Integer.MAX_VALUE, requestContext)
      .thenApply(TransactionCollection::getTransactions);
  }

  private double getTransactionsTotal(List<Transaction> transactions, ToDoubleFunction<Transaction> getAmount) {
    return transactions.stream()
      .map(transaction -> Money.of(getAmount.applyAsDouble(transaction), transaction.getCurrency()))
      .reduce(Money::add)
      .map(amount -> amount.with(MonetaryOperators.rounding())
        .getNumber()
        .doubleValue())
      .orElse(0d);
  }
}
