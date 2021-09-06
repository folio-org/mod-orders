package org.folio.service.finance.transaction;

import static java.util.stream.Collectors.toList;

import java.util.List;
import java.util.Objects;
import java.util.concurrent.CompletableFuture;

import org.apache.logging.log4j.LogManager;
import org.apache.logging.log4j.Logger;
import org.folio.models.EncumbranceRelationsHolder;
import org.folio.models.EncumbrancesProcessingHolder;
import org.folio.rest.acq.model.finance.Transaction;
import org.folio.rest.core.models.RequestContext;
import org.folio.rest.jaxrs.model.CompositePurchaseOrder;
import org.folio.service.FundsDistributionService;
import org.folio.service.finance.budget.BudgetRestrictionService;
import org.folio.service.orders.OrderWorkflowType;

public class ReceivingEncumbranceStrategy implements EncumbranceWorkflowStrategy {
  private static final Logger LOG = LogManager.getLogger(ReceivingEncumbranceStrategy.class);

  private final EncumbranceService encumbranceService;
  private final FundsDistributionService fundsDistributionService;
  private final BudgetRestrictionService budgetRestrictionService;
  private final EncumbranceRelationsHoldersBuilder encumbranceRelationsHoldersBuilder;
  private final TransactionSummariesService transactionSummariesService;

  public ReceivingEncumbranceStrategy(EncumbranceService encumbranceService,
                                            FundsDistributionService fundsDistributionService,
                                            BudgetRestrictionService budgetRestrictionService,
                                            EncumbranceRelationsHoldersBuilder encumbranceRelationsHoldersBuilder,
                                            TransactionSummariesService transactionSummariesService) {
      this.encumbranceService = encumbranceService;
      this.fundsDistributionService = fundsDistributionService;
      this.budgetRestrictionService = budgetRestrictionService;
      this.encumbranceRelationsHoldersBuilder = encumbranceRelationsHoldersBuilder;
      this.transactionSummariesService = transactionSummariesService;
    }

  @Override
  public CompletableFuture<Void> processEncumbrances(CompositePurchaseOrder compPO, CompositePurchaseOrder poAndLinesFromStorage,
    RequestContext requestContext) {
    List<EncumbranceRelationsHolder> encumbranceRelationsHolders = encumbranceRelationsHoldersBuilder.buildBaseHolders(compPO);
    return prepareEncumbranceRelationsHolder(encumbranceRelationsHolders, poAndLinesFromStorage, requestContext)
      .thenApply(fundsDistributionService::distributeFunds)
      .thenAccept(budgetRestrictionService::checkEncumbranceRestrictions)
      .thenApply(aVoid -> distributeHoldersByOperation(encumbranceRelationsHolders))
      .thenCompose(holder -> encumbranceService.createOrUpdateEncumbrances(holder, requestContext))
      .thenAccept(holders -> LOG.debug("End processing encumbrances for piece add/delete"));
  }

  public CompletableFuture<List<EncumbranceRelationsHolder>> prepareEncumbranceRelationsHolder(List<EncumbranceRelationsHolder> encumbranceRelationsHolders,
                  CompositePurchaseOrder poFromStorage, RequestContext requestContext) {
    return encumbranceRelationsHoldersBuilder.withBudgets(encumbranceRelationsHolders, requestContext)
      .thenCompose(holders -> encumbranceRelationsHoldersBuilder.withLedgersData(holders, requestContext))
      .thenCompose(holders -> encumbranceRelationsHoldersBuilder.withFiscalYearData(holders, requestContext))
      .thenCompose(holders -> encumbranceRelationsHoldersBuilder.withConversion(holders, requestContext))
      .thenCompose(holders -> encumbranceRelationsHoldersBuilder.withExistingTransactions(holders, poFromStorage, requestContext));
  }

  @Override
  public OrderWorkflowType getStrategyName() {
    return OrderWorkflowType.RECEIVING_PIECE_ADD_OR_DELETE;
  }

  private EncumbrancesProcessingHolder distributeHoldersByOperation(
    List<EncumbranceRelationsHolder> encumbranceRelationsHolders) {
    EncumbrancesProcessingHolder holder = new EncumbrancesProcessingHolder();
    holder.withEncumbrancesForCreate(getToBeCreatedHolders(encumbranceRelationsHolders));
    holder.withEncumbrancesForUpdate(getToBeUpdatedHolders(encumbranceRelationsHolders));
    holder.withEncumbrancesForDelete(getTransactionsToDelete(encumbranceRelationsHolders));
    holder.withEncumbrancesFromStorage(encumbranceRelationsHolders.stream()
      .map(EncumbranceRelationsHolder::getOldEncumbrance)
      .filter(Objects::nonNull)
      .collect(toList()));
    return holder;
  }

  private List<Transaction> getTransactionsToDelete(List<EncumbranceRelationsHolder> encumbranceRelationsHolders) {
    return encumbranceRelationsHolders.stream()
      .filter(holder -> Objects.isNull(holder.getNewEncumbrance()))
      .map(EncumbranceRelationsHolder::getOldEncumbrance)
      .collect(toList());
  }

  private List<EncumbranceRelationsHolder> getToBeUpdatedHolders(List<EncumbranceRelationsHolder> encumbranceRelationsHolders) {
    return encumbranceRelationsHolders.stream()
      .filter(holder -> Objects.nonNull(holder.getOldEncumbrance()))
      .filter(holder -> Objects.nonNull(holder.getNewEncumbrance()))
      .filter(this::isTransactionUpdated)
      .collect(toList());
  }

  private boolean isTransactionUpdated(EncumbranceRelationsHolder holder) {
    double amountBeforeUpdate = holder.getOldEncumbrance()
      .getAmount();
    double updatedAmount = holder.getNewEncumbrance()
      .getAmount();
    double initialAmountBeforeUpdate = holder.getOldEncumbrance()
      .getEncumbrance()
      .getInitialAmountEncumbered();
    double updatedInitialAmount = holder.getNewEncumbrance()
      .getEncumbrance()
      .getInitialAmountEncumbered();

    return Double.compare(amountBeforeUpdate, updatedAmount) != 0
      || (Double.compare(initialAmountBeforeUpdate, updatedInitialAmount) != 0);
  }

  private List<EncumbranceRelationsHolder> getToBeCreatedHolders(List<EncumbranceRelationsHolder> encumbranceRelationsHolders) {
    return encumbranceRelationsHolders.stream()
      .filter(holder -> Objects.isNull(holder.getOldEncumbrance()))
      .collect(toList());
  }
}
