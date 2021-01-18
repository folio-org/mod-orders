package org.folio.service.finance;

import org.folio.models.EncumbrancesProcessingHolder;
import org.folio.rest.core.models.RequestContext;
import org.folio.rest.jaxrs.model.CompositePurchaseOrder;

import java.util.concurrent.CompletableFuture;

import static org.folio.orders.utils.FundDistributionUtils.isFundDistributionsPresent;

public class OpenToCloseEncumbranceStrategy implements EncumbranceWorkflowStrategy {

  private final EncumbranceService encumbranceService;
  private final TransactionSummariesService transactionSummariesService;

  public OpenToCloseEncumbranceStrategy(EncumbranceService encumbranceService, TransactionSummariesService transactionSummariesService) {
    this.encumbranceService = encumbranceService;
    this.transactionSummariesService = transactionSummariesService;
  }

  @Override
  public CompletableFuture<Void> processEncumbrances(CompositePurchaseOrder compPO, RequestContext requestContext) {
    EncumbrancesProcessingHolder holder = new EncumbrancesProcessingHolder();
    if (isFundDistributionsPresent(compPO.getCompositePoLines())) {
      return encumbranceService.getOrderEncumbrances(compPO.getId(), requestContext)
        .thenAccept(holder::withEncumbrancesFromStorage)
        .thenApply(v -> holder.getEncumbrancesFromStorage())
        .thenAccept(holder::withEncumbrancesForRelease)
        .thenCompose(v -> transactionSummariesService.createOrUpdateOrderTransactionSummary(compPO.getId(), holder, requestContext))
        .thenCompose(v -> encumbranceService.createOrUpdateEncumbrances(holder, requestContext));
    }
    return CompletableFuture.completedFuture(null);

  }

  @Override
  public WorkflowStatusName getStrategyName() {
    return WorkflowStatusName.OPEN_TO_CLOSE;
  }
}
