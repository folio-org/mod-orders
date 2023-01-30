package org.folio.service.finance.transaction;

import static org.folio.orders.utils.FundDistributionUtils.isFundDistributionsPresent;
import static org.folio.rest.acq.model.finance.Encumbrance.OrderStatus.CLOSED;

import org.folio.rest.acq.model.finance.Encumbrance;
import org.folio.rest.core.models.RequestContext;
import org.folio.rest.jaxrs.model.CompositePurchaseOrder;
import org.folio.service.finance.transaction.summary.OrderTransactionSummariesService;
import org.folio.service.orders.OrderWorkflowType;

import io.vertx.core.Future;

public class OpenToClosedEncumbranceStrategy implements EncumbranceWorkflowStrategy {

  private final EncumbranceService encumbranceService;
  private final EncumbranceRelationsHoldersBuilder encumbranceRelationsHoldersBuilder;
  private final OrderTransactionSummariesService orderTransactionSummariesService;

  public OpenToClosedEncumbranceStrategy(EncumbranceService encumbranceService,
      EncumbranceRelationsHoldersBuilder encumbranceRelationsHoldersBuilder,
      OrderTransactionSummariesService orderTransactionSummariesService) {
    this.encumbranceService = encumbranceService;
    this.encumbranceRelationsHoldersBuilder = encumbranceRelationsHoldersBuilder;
    this.orderTransactionSummariesService = orderTransactionSummariesService;
  }

  @Override
  public Future<Void> processEncumbrances(CompositePurchaseOrder compPO, CompositePurchaseOrder poAndLinesFromStorage,
      RequestContext requestContext) {

    if (isFundDistributionsPresent(compPO.getCompositePoLines())) {
      return encumbranceRelationsHoldersBuilder.retrieveMapFiscalYearsWithCompPOLines(compPO, poAndLinesFromStorage, requestContext)
        .compose(mapCompPoLine -> encumbranceService.getEncumbrancesByPoLinesFromCurrentFy(mapCompPoLine, requestContext))
        .compose(transactions -> {
          if (transactions.isEmpty()) {
            return Future.succeededFuture();
          } else {
            transactions.forEach(tr -> {
              tr.getEncumbrance().setOrderStatus(CLOSED);
              tr.getEncumbrance().setStatus(Encumbrance.Status.RELEASED);
            });
            return orderTransactionSummariesService.updateTransactionSummary(compPO.getId(), transactions.size(), requestContext)
              .compose(v -> encumbranceService.updateEncumbrances(transactions, requestContext));
          }
        });
    }
    return Future.succeededFuture();
  }

  @Override
  public OrderWorkflowType getStrategyName() {
    return OrderWorkflowType.OPEN_TO_CLOSED;
  }
}
