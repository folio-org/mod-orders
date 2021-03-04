package org.folio.service.orders;

import org.folio.models.CompositeOrderRetrieveHolder;
import org.folio.orders.utils.HelperUtils;
import org.folio.rest.acq.model.finance.Transaction;
import org.folio.rest.core.models.RequestContext;
import org.folio.service.finance.transaction.TransactionService;

import java.util.List;
import java.util.Optional;
import java.util.concurrent.CompletableFuture;

import static java.util.stream.Collectors.toList;

public class TotalExpendedPopulateService implements CompositeOrderDynamicDataPopulateService {

    private final TransactionService transactionService;

    public TotalExpendedPopulateService(TransactionService transactionService) {
        this.transactionService = transactionService;
    }

    @Override
    public CompletableFuture<CompositeOrderRetrieveHolder> populate(CompositeOrderRetrieveHolder holder, RequestContext requestContext) {
        return Optional.of(holder)
                .map(CompositeOrderRetrieveHolder::getFiscalYear)
                .map(s -> withTotalExpended(holder, requestContext))
                .orElseGet(() -> CompletableFuture.completedFuture(holder.withTotalExpended(0d)));
    }

    private CompletableFuture<CompositeOrderRetrieveHolder> withTotalExpended(CompositeOrderRetrieveHolder holder, RequestContext requestContext) {
        List<String> encumbranceIds = holder.getCurrentEncumbrances().stream()
                .map(Transaction::getId)
                .collect(toList());
        return transactionService.getCurrentPaymentsByEncumbranceIds(encumbranceIds, holder.getFiscalYearId(), requestContext)
                .thenApply(transactions -> holder.withTotalExpended(HelperUtils.getTransactionsTotal(transactions)));
    }
}
