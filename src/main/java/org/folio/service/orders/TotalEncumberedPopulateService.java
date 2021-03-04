package org.folio.service.orders;

import java.util.Optional;
import java.util.concurrent.CompletableFuture;

import org.folio.models.CompositeOrderRetrieveHolder;
import org.folio.orders.utils.HelperUtils;
import org.folio.rest.acq.model.finance.TransactionCollection;
import org.folio.rest.core.models.RequestContext;
import org.folio.service.finance.transaction.TransactionService;

public class TotalEncumberedPopulateService implements CompositeOrderDynamicDataPopulateService {

  private final TransactionService transactionService;

  public TotalEncumberedPopulateService(TransactionService transactionService) {
    this.transactionService = transactionService;
  }

  @Override
  public CompletableFuture<CompositeOrderRetrieveHolder> populate(CompositeOrderRetrieveHolder holder,
      RequestContext requestContext) {

    return Optional.of(holder)
      .map(CompositeOrderRetrieveHolder::getFiscalYear)
      .map(s -> withTotalEncumbered(holder, requestContext))
      .orElseGet(() -> CompletableFuture.completedFuture(holder.withTotalEncumbered(0d)));
  }

  public CompletableFuture<CompositeOrderRetrieveHolder> withTotalEncumbered(CompositeOrderRetrieveHolder holder,
      RequestContext requestContext) {
    String query = String.format("transactionType==Encumbrance AND encumbrance.sourcePurchaseOrderId==%s AND fiscalYearId==%s",
        holder.getOrderId(), holder.getFiscalYearId());
    return transactionService.getTransactions(query, 0, Integer.MAX_VALUE, requestContext)
      .thenApply(TransactionCollection::getTransactions)
      .thenApply(transactions -> holder.withTotalEncumbered(HelperUtils.getTransactionsTotal(transactions)));
  }

}
