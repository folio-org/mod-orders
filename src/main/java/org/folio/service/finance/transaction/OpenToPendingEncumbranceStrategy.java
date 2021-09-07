package org.folio.service.finance.transaction;

import java.util.List;
import java.util.concurrent.CompletableFuture;

import org.folio.models.EncumbranceRelationsHolder;
import org.folio.rest.acq.model.finance.Encumbrance;
import org.folio.rest.acq.model.finance.Transaction;
import org.folio.rest.core.models.RequestContext;
import org.folio.rest.jaxrs.model.CompositePurchaseOrder;
import org.folio.service.orders.OrderWorkflowType;

public class OpenToPendingEncumbranceStrategy extends BaseEncumbranceWorkflowStrategy {

  private final EncumbranceService encumbranceService;
  private final TransactionSummariesService transactionSummariesService;
  private final EncumbranceRelationsHoldersBuilder encumbranceRelationsHoldersBuilder;

  public OpenToPendingEncumbranceStrategy(EncumbranceService encumbranceService,
      TransactionSummariesService transactionSummariesService,
      EncumbranceRelationsHoldersBuilder encumbranceRelationsHoldersBuilder) {
    super(encumbranceService);
    this.encumbranceService = encumbranceService;
    this.transactionSummariesService = transactionSummariesService;
    this.encumbranceRelationsHoldersBuilder = encumbranceRelationsHoldersBuilder;
  }

  @Override
  public CompletableFuture<Void> processEncumbrances(CompositePurchaseOrder compPO, CompositePurchaseOrder poAndLinesFromStorage,
        RequestContext requestContext) {

    return prepareEncumbranceRelationsHolder(compPO, poAndLinesFromStorage, requestContext)
              .thenCompose(encumbranceRelationsHolders -> getOrderEncumbrancesForCurrentFy(encumbranceRelationsHolders, requestContext))
              .thenApply(this::makeEncumbrancesPending)
              .thenCompose(transactions ->
                    transactionSummariesService.updateOrderTransactionSummary(compPO.getId(), transactions.size(), requestContext)
                                               .thenApply(vVoid -> transactions))
              .thenCompose(transactions -> encumbranceService.updateEncumbrances(transactions, requestContext));
  }

  @Override
  public OrderWorkflowType getStrategyName() {
    return OrderWorkflowType.OPEN_TO_PENDING;
  }

  public CompletableFuture<List<EncumbranceRelationsHolder>> prepareEncumbranceRelationsHolder(CompositePurchaseOrder compPO,
      CompositePurchaseOrder poFromStorage, RequestContext requestContext) {
    List<EncumbranceRelationsHolder> encumbranceRelationsHolders = encumbranceRelationsHoldersBuilder.buildBaseHolders(compPO);
    return encumbranceRelationsHoldersBuilder.withBudgets(encumbranceRelationsHolders, requestContext)
      .thenCompose(holders -> encumbranceRelationsHoldersBuilder.withLedgersData(holders, requestContext))
      .thenCompose(holders -> encumbranceRelationsHoldersBuilder.withFiscalYearData(holders, requestContext))
      .thenCompose(holders -> encumbranceRelationsHoldersBuilder.withConversion(holders, requestContext))
      .thenCompose(holders -> encumbranceRelationsHoldersBuilder.withExistingTransactions(holders, poFromStorage, requestContext));
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
}
