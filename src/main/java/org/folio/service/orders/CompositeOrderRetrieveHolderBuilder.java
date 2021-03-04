package org.folio.service.orders;

import org.folio.models.CompositeOrderRetrieveHolder;
import org.folio.orders.rest.exceptions.HttpException;
import org.folio.rest.acq.model.finance.TransactionCollection;
import org.folio.rest.core.models.RequestContext;
import org.folio.rest.jaxrs.model.FundDistribution;
import org.folio.service.finance.FiscalYearService;
import org.folio.service.finance.transaction.TransactionService;

import java.util.Objects;
import java.util.concurrent.CompletableFuture;
import java.util.concurrent.CompletionException;

public class CompositeOrderRetrieveHolderBuilder {
    private final FiscalYearService fiscalYearService;
    private final TransactionService transactionService;

    public CompositeOrderRetrieveHolderBuilder(FiscalYearService fiscalYearService, TransactionService transactionService) {
        this.fiscalYearService = fiscalYearService;
        this.transactionService = transactionService;
    }

    public CompletableFuture<CompositeOrderRetrieveHolder> withCurrentFiscalYear(CompositeOrderRetrieveHolder holder, RequestContext requestContext) {
        return holder.getOrder().getCompositePoLines().stream().flatMap(poLine -> poLine.getFundDistribution().stream())
                .map(FundDistribution::getFundId).findFirst()
                .map(fundId -> fiscalYearService.getCurrentFiscalYearByFundId(fundId, requestContext)
                        .thenApply(holder::withFiscalYear)
                        .exceptionally(t -> {
                            Throwable cause = Objects.nonNull(t.getCause()) ? t.getCause() : t;
                            if (cause instanceof HttpException && ((HttpException) cause).getCode() == 404) {
                                return holder;
                            }
                            throw new CompletionException(cause);
                        }))
                .orElseGet(() -> CompletableFuture.completedFuture(holder));
    }

    public CompletableFuture<CompositeOrderRetrieveHolder> withCurrentEncumbranceIds(CompositeOrderRetrieveHolder holder, RequestContext requestContext) {
        String query = String.format("transactionType==Encumbrance AND encumbrance.sourcePurchaseOrderId==%s AND fiscalYearId==%s",
                holder.getOrderId(), holder.getFiscalYearId());
        return transactionService.getTransactions(query, 0, Integer.MAX_VALUE, requestContext)
                .thenApply(TransactionCollection::getTransactions)
                .thenApply(holder::withCurrentEncumbrances);
    }
}
