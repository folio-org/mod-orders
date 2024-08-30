package org.folio.service.finance.transaction;

import java.util.List;

import org.folio.rest.acq.model.finance.Encumbrance;
import org.folio.rest.acq.model.finance.Transaction;
import org.folio.rest.core.models.RequestContext;
import org.folio.rest.jaxrs.model.CompositePurchaseOrder;
import org.folio.service.orders.OrderWorkflowType;

import io.vertx.core.Future;

public class OpenToPendingEncumbranceStrategy implements EncumbranceWorkflowStrategy {

  private final EncumbranceService encumbranceService;
  private final EncumbranceRelationsHoldersBuilder encumbranceRelationsHoldersBuilder;

  public OpenToPendingEncumbranceStrategy(EncumbranceService encumbranceService,
    EncumbranceRelationsHoldersBuilder encumbranceRelationsHoldersBuilder) {
    this.encumbranceService = encumbranceService;
    this.encumbranceRelationsHoldersBuilder = encumbranceRelationsHoldersBuilder;
  }

  @Override
  public Future<Void> processEncumbrances(CompositePurchaseOrder compPO, CompositePurchaseOrder poAndLinesFromStorage,
      RequestContext requestContext) {

    return getOrderEncumbrances(compPO, poAndLinesFromStorage, requestContext)
      .map(this::makeEncumbrancesPending)
      .compose(transactions -> encumbranceService.updateEncumbrances(transactions, requestContext));
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

  public Future<List<Transaction>> getOrderEncumbrances(CompositePurchaseOrder compPo,
      CompositePurchaseOrder poFromStorage, RequestContext requestContext) {

    return encumbranceRelationsHoldersBuilder.retrieveMapFiscalYearsWithCompPOLines(compPo, poFromStorage, requestContext)
      .compose(poLinesByCurrentFy -> encumbranceService.getEncumbrancesByPoLinesFromCurrentFy(poLinesByCurrentFy, requestContext));
  }
}
