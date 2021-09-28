package org.folio.service.finance.transaction;

import static org.folio.orders.utils.FundDistributionUtils.isFundDistributionsPresent;

import java.util.concurrent.CompletableFuture;

import org.folio.models.EncumbrancesProcessingHolder;
import org.folio.rest.core.models.RequestContext;
import org.folio.rest.jaxrs.model.CompositePurchaseOrder;
import org.folio.service.orders.OrderWorkflowType;

public class OpenToClosedEncumbranceStrategy implements EncumbranceWorkflowStrategy {

  private final EncumbranceService encumbranceService;
  private final EncumbranceRelationsHoldersBuilder encumbranceRelationsHoldersBuilder;

  public OpenToClosedEncumbranceStrategy(EncumbranceService encumbranceService, EncumbranceRelationsHoldersBuilder encumbranceRelationsHoldersBuilder) {
    this.encumbranceService = encumbranceService;
    this.encumbranceRelationsHoldersBuilder = encumbranceRelationsHoldersBuilder;
  }

  @Override
  public CompletableFuture<Void> processEncumbrances(CompositePurchaseOrder compPO, CompositePurchaseOrder poAndLinesFromStorage,
      RequestContext requestContext) {

    EncumbrancesProcessingHolder holder = new EncumbrancesProcessingHolder();
    if (isFundDistributionsPresent(compPO.getCompositePoLines())) {
      return encumbranceRelationsHoldersBuilder.retrieveMapFiscalYearsWithCompPOLines(compPO, poAndLinesFromStorage, requestContext)
        .thenCompose(mapCompPoLine -> encumbranceService.getEncumbrancesByPoLinesFromCurrentFy(mapCompPoLine, requestContext))
        .thenApply(transactions -> encumbranceRelationsHoldersBuilder.retrieveTransactionCollection(transactions))
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
  public OrderWorkflowType getStrategyName() {
    return OrderWorkflowType.OPEN_TO_CLOSED;
  }
}
