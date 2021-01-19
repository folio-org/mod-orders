package org.folio.service.finance;

import static org.folio.orders.utils.FundDistributionUtils.isFundDistributionsPresent;
import static org.folio.orders.utils.FundDistributionUtils.validateFundDistributionTotal;

import java.util.concurrent.CompletableFuture;

import org.folio.models.EncumbrancesProcessingHolder;
import org.folio.rest.core.models.RequestContext;
import org.folio.rest.jaxrs.model.CompositePurchaseOrder;

public class PendingToOpenEncumbranceStrategy implements EncumbranceWorkflowStrategy {

    private final EncumbranceService encumbranceService;
    private final TransactionSummariesService transactionSummariesService;

    public PendingToOpenEncumbranceStrategy(EncumbranceService encumbranceService, TransactionSummariesService transactionSummariesService) {
        this.encumbranceService = encumbranceService;
        this.transactionSummariesService = transactionSummariesService;
    }

    @Override
    public CompletableFuture<Void> processEncumbrances(CompositePurchaseOrder compPO, RequestContext requestContext) {
        EncumbrancesProcessingHolder holder = new EncumbrancesProcessingHolder();
        if (isFundDistributionsPresent(compPO.getCompositePoLines())) {
            validateFundDistributionTotal(compPO.getCompositePoLines());
            return encumbranceService.getPoLinesEncumbrances(compPO.getCompositePoLines(), requestContext)
                    .thenAccept(holder::withEncumbrancesFromStorage)
                    .thenCompose(v -> encumbranceService.buildNewEncumbrances(compPO, compPO.getCompositePoLines(), holder.getEncumbrancesFromStorage(),requestContext))
                    .thenAccept(holder::withEncumbrancesForCreate)
                    .thenCompose(v -> encumbranceService.buildEncumbrancesForUpdate(compPO.getCompositePoLines(), holder.getEncumbrancesFromStorage(), requestContext))
                    .thenAccept(holder::withEncumbrancesForUpdate)
                    .thenApply(v -> encumbranceService.findNeedReleaseEncumbrances(compPO.getCompositePoLines(), holder.getEncumbrancesFromStorage()))
                    .thenAccept(holder::withEncumbrancesForRelease)
                    .thenCompose(v -> transactionSummariesService.createOrUpdateOrderTransactionSummary(compPO.getId(), holder, requestContext))
                    .thenCompose(v -> encumbranceService.createOrUpdateEncumbrances(holder, requestContext));

        }
        return CompletableFuture.completedFuture(null);
    }

    @Override
    public WorkflowStatusName getStrategyName() {
      return WorkflowStatusName.PENDING_TO_OPEN;
    }
}
