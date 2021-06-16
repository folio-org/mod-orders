package org.folio.service.finance.transaction;

import org.folio.models.EncumbrancesProcessingHolder;
import org.folio.rest.core.models.RequestContext;
import org.folio.rest.jaxrs.model.CompositePurchaseOrder;
import org.folio.service.finance.WorkflowStatusName;

import java.util.concurrent.CompletableFuture;

import static org.folio.orders.utils.FundDistributionUtils.isFundDistributionsPresent;

public class OpenToCloseEncumbranceStrategy implements EncumbranceWorkflowStrategy {

  private final EncumbranceService encumbranceService;

  public OpenToCloseEncumbranceStrategy(EncumbranceService encumbranceService) {
    this.encumbranceService = encumbranceService;
  }

  @Override
  public CompletableFuture<Void> processEncumbrances(CompositePurchaseOrder compPO, RequestContext requestContext) {
    EncumbrancesProcessingHolder holder = new EncumbrancesProcessingHolder();
    if (isFundDistributionsPresent(compPO.getCompositePoLines())) {
      return encumbranceService.getOrderEncumbrances(compPO.getId(), requestContext)
        .thenCompose(transactions -> {
          if (transactions.isEmpty()) {
            return CompletableFuture.completedFuture(null);
          } else {
            holder.withEncumbrancesFromStorage(transactions);
            holder.withEncumbrancesForRelease(holder.getEncumbrancesFromStorage());
            return encumbranceService.createOrUpdateEncumbrances(holder, requestContext);
          }
        });
    }
    return CompletableFuture.completedFuture(null);

  }

  @Override
  public WorkflowStatusName getStrategyName() {
    return WorkflowStatusName.OPEN_TO_CLOSE;
  }
}
