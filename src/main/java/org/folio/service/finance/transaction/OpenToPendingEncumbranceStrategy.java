package org.folio.service.finance.transaction;

import java.util.List;
import java.util.concurrent.CompletableFuture;

import org.folio.rest.acq.model.finance.Encumbrance;
import org.folio.rest.acq.model.finance.Transaction;
import org.folio.rest.core.models.RequestContext;
import org.folio.rest.jaxrs.model.CompositePurchaseOrder;
import org.folio.service.orders.OrderWorkflowType;

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
                .thenApply(this::makeEncumbrancesPending)
                .thenCompose(transactions -> transactionSummariesService.updateOrderTransactionSummary(compPO.getId(), transactions.size(), requestContext)
                    .thenApply(vVoid -> transactions))
                .thenCompose(transactions -> encumbranceService.updateEncumbrances(transactions, requestContext));
    }

    private List<Transaction> makeEncumbrancesPending(List<Transaction> encumbrances) {
        encumbrances.forEach(encumbrance -> {
            encumbrance.setAmount(0d);
            encumbrance.getEncumbrance().setInitialAmountEncumbered(0d);
            encumbrance.getEncumbrance().setStatus(Encumbrance.Status.PENDING);
            encumbrance.getEncumbrance().setOrderStatus(Encumbrance.OrderStatus.PENDING);
        });
        return encumbrances;
    }

    @Override
    public OrderWorkflowType getStrategyName() {
        return OrderWorkflowType.OPEN_TO_PENDING;
    }
}
