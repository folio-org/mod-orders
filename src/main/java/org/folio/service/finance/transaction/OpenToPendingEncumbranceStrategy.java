package org.folio.service.finance.transaction;

import java.util.Collection;
import java.util.List;
import java.util.concurrent.CompletableFuture;
import java.util.stream.Collectors;

import org.folio.rest.acq.model.finance.Encumbrance;
import org.folio.rest.acq.model.finance.Transaction;
import org.folio.rest.core.models.RequestContext;
import org.folio.rest.jaxrs.model.CompositePurchaseOrder;
import org.folio.service.orders.OrderWorkflowType;

public class OpenToPendingEncumbranceStrategy implements EncumbranceWorkflowStrategy {

  private final EncumbranceService encumbranceService;
  private final TransactionSummariesService transactionSummariesService;
  private final EncumbranceRelationsHoldersBuilder encumbranceRelationsHoldersBuilder;

  public OpenToPendingEncumbranceStrategy(EncumbranceService encumbranceService,
      TransactionSummariesService transactionSummariesService,
      EncumbranceRelationsHoldersBuilder encumbranceRelationsHoldersBuilder) {
    this.encumbranceService = encumbranceService;
    this.transactionSummariesService = transactionSummariesService;
    this.encumbranceRelationsHoldersBuilder = encumbranceRelationsHoldersBuilder;
  }

    @Override
    public CompletableFuture<Void> processEncumbrances(CompositePurchaseOrder compPO, CompositePurchaseOrder poAndLinesFromStorage,
        RequestContext requestContext) {

      return getOrderEncumbrances(compPO, poAndLinesFromStorage, requestContext)
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

  public CompletableFuture<List<Transaction>> getOrderEncumbrances(CompositePurchaseOrder compPo,
                                                                   CompositePurchaseOrder poFromStorage, RequestContext requestContext) {

    return encumbranceRelationsHoldersBuilder.retrieveMapFiscalYearsWithCompPOLines(compPo, poFromStorage, requestContext)
      .thenCompose(poLinesByCurrentFy -> encumbranceService.getEncumbrancesByPoLinesFromCurrentFy(poLinesByCurrentFy, requestContext))
      .thenApply(trs -> trs.stream().flatMap(Collection::stream).collect(Collectors.toList()));
  }
}
