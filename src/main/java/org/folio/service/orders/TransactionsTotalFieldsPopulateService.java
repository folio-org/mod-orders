package org.folio.service.orders;

import java.util.List;
import java.util.Optional;
import java.util.function.ToDoubleFunction;

import org.apache.logging.log4j.LogManager;
import org.apache.logging.log4j.Logger;
import org.folio.models.CompositeOrderRetrieveHolder;
import org.folio.rest.acq.model.finance.Transaction;
import org.folio.rest.core.models.RequestContext;
import org.folio.service.finance.transaction.TransactionService;
import org.javamoney.moneta.Money;
import org.javamoney.moneta.function.MonetaryOperators;

import io.vertx.core.Future;

public class TransactionsTotalFieldsPopulateService implements CompositeOrderDynamicDataPopulateService {
  private static final Logger log = LogManager.getLogger(TransactionsTotalFieldsPopulateService.class);

  private final TransactionService transactionService;

  public TransactionsTotalFieldsPopulateService(TransactionService transactionService) {
    this.transactionService = transactionService;
  }

  @Override
  public Future<CompositeOrderRetrieveHolder> populate(CompositeOrderRetrieveHolder holder,
      RequestContext requestContext) {
    return Optional.of(holder)
      .map(CompositeOrderRetrieveHolder::getFiscalYear)
      .map(s -> withTotalFields(holder, requestContext))
      .orElseGet(() ->  Future.succeededFuture(holder.withTotalExpended(0d).withTotalCredited(0d).withTotalEncumbered(0d)))
      .onFailure(v -> log.error("Failed at {}.{}", this.getClass().getName(), Thread.currentThread().getStackTrace()[0]));
  }

  private Future<CompositeOrderRetrieveHolder> withTotalFields(CompositeOrderRetrieveHolder holder,
      RequestContext requestContext) {
    return getCurrentEncumbrances(holder, requestContext).map(transactions -> {
      holder.withTotalEncumbered(getTransactionsTotal(transactions, Transaction::getAmount));
      return holder
        .withTotalExpended(getTransactionsTotal(transactions, transaction -> transaction.getEncumbrance().getAmountExpended()))
        .withTotalCredited(getTransactionsTotal(transactions, transaction -> transaction.getEncumbrance().getAmountCredited()));
    });
  }

  public Future<List<Transaction>> getCurrentEncumbrances(CompositeOrderRetrieveHolder holder,
      RequestContext requestContext) {
    String query = String.format("transactionType==Encumbrance AND encumbrance.sourcePurchaseOrderId==%s AND fiscalYearId==%s",
        holder.getOrderId(), holder.getFiscalYearId());
    return transactionService.getTransactions(query, requestContext);
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
