package org.folio.service.finance;

import java.util.concurrent.CompletableFuture;

import org.folio.rest.core.models.RequestContext;
import org.folio.rest.jaxrs.model.CompositePurchaseOrder;

public class OpenToPendingEncumbranceStrategy implements EncumbranceWorkflowStrategy {

    private final EncumbranceService encumbranceService;
    private final TransactionSummariesService transactionSummariesService;

    public OpenToPendingEncumbranceStrategy(EncumbranceService encumbranceService, TransactionSummariesService transactionSummariesService) {
        this.encumbranceService = encumbranceService;
        this.transactionSummariesService = transactionSummariesService;
    }

    @Override
    public CompletableFuture<Void> processEncumbrances(CompositePurchaseOrder compPO, RequestContext requestContext) {
        return encumbranceService.getOrderEncumbrances(compPO.getId(), requestContext)
                .thenApply(encumbranceService::makeEncumbrancesPending)
                .thenCompose(encumbrances -> transactionSummariesService.updateOrderTransactionSummary(compPO.getId(), encumbrances.size(), requestContext)
                        .thenApply(v -> encumbrances))
                .thenCompose(transactions -> encumbranceService.updateTransactions(transactions, requestContext));
    }

    @Override
    public WorkflowStatusName getStrategyName() {
        return WorkflowStatusName.OPEN_TO_PENDING;
    }
}
