package org.folio.service.finance.transaction;

import static org.folio.orders.utils.FundDistributionUtils.isFundDistributionsPresent;
import static org.folio.rest.acq.model.finance.Encumbrance.OrderStatus.CLOSED;

import java.util.concurrent.CompletableFuture;

import org.folio.rest.acq.model.finance.Encumbrance;
import org.folio.rest.core.models.RequestContext;
import org.folio.rest.jaxrs.model.CompositePurchaseOrder;
import org.folio.service.orders.OrderWorkflowType;

public class OpenToClosedEncumbranceStrategy implements EncumbranceWorkflowStrategy {

  private final EncumbranceService encumbranceService;
  private final EncumbranceRelationsHoldersBuilder encumbranceRelationsHoldersBuilder;
  private final TransactionSummariesService transactionSummariesService;

  public OpenToClosedEncumbranceStrategy(EncumbranceService encumbranceService,
      EncumbranceRelationsHoldersBuilder encumbranceRelationsHoldersBuilder,
      TransactionSummariesService transactionSummariesService) {
    this.encumbranceService = encumbranceService;
    this.encumbranceRelationsHoldersBuilder = encumbranceRelationsHoldersBuilder;
    this.transactionSummariesService = transactionSummariesService;
  }

  @Override
  public CompletableFuture<Void> processEncumbrances(CompositePurchaseOrder compPO, CompositePurchaseOrder poAndLinesFromStorage,
      RequestContext requestContext) {

    if (isFundDistributionsPresent(compPO.getCompositePoLines())) {
      return encumbranceRelationsHoldersBuilder.retrieveMapFiscalYearsWithCompPOLines(compPO, poAndLinesFromStorage, requestContext)
        .thenCompose(mapCompPoLine -> encumbranceService.getEncumbrancesByPoLinesFromCurrentFy(mapCompPoLine, requestContext))
        .thenCompose(transactions -> {
          if (transactions.isEmpty()) {
            return CompletableFuture.completedFuture(null);
          } else {
            transactions.forEach(tr -> {
              tr.getEncumbrance().setOrderStatus(CLOSED);
              tr.getEncumbrance().setStatus(Encumbrance.Status.RELEASED);
            });
            return transactionSummariesService.updateOrderTransactionSummary(compPO.getId(), transactions.size(), requestContext)
              .thenCompose(v -> encumbranceService.updateEncumbrances(transactions, requestContext));
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
