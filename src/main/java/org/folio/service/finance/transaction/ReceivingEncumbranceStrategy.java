package org.folio.service.finance.transaction;

import java.util.List;
import java.util.concurrent.CompletableFuture;

import org.apache.logging.log4j.LogManager;
import org.apache.logging.log4j.Logger;
import org.folio.models.EncumbranceRelationsHolder;
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
  private final TransactionSummariesService transactionSummariesService;
  private final EncumbranceRelationsHoldersBuilder encumbranceRelationsHoldersBuilder;
  private final EncumbrancesProcessingHolderBuilder encumbrancesProcessingHolderBuilder;

  public ReceivingEncumbranceStrategy(EncumbranceService encumbranceService, FundsDistributionService fundsDistributionService,
    BudgetRestrictionService budgetRestrictionService, EncumbranceRelationsHoldersBuilder encumbranceRelationsHoldersBuilder,
    TransactionSummariesService transactionSummariesService,
    EncumbrancesProcessingHolderBuilder encumbrancesProcessingHolderBuilder) {
      this.encumbranceService = encumbranceService;
      this.fundsDistributionService = fundsDistributionService;
      this.budgetRestrictionService = budgetRestrictionService;
      this.encumbranceRelationsHoldersBuilder = encumbranceRelationsHoldersBuilder;
      this.transactionSummariesService = transactionSummariesService;
      this.encumbrancesProcessingHolderBuilder = encumbrancesProcessingHolderBuilder;
  }

  @Override
  public CompletableFuture<Void> processEncumbrances(CompositePurchaseOrder compPO, CompositePurchaseOrder poAndLinesFromStorage,
    RequestContext requestContext) {
    List<EncumbranceRelationsHolder> encumbranceRelationsHolders = encumbranceRelationsHoldersBuilder.buildBaseHolders(compPO);
    return prepareEncumbranceRelationsHolder(encumbranceRelationsHolders, poAndLinesFromStorage, requestContext)
      .thenApply(fundsDistributionService::distributeFunds)
      .thenAccept(budgetRestrictionService::checkEncumbranceRestrictions)
      .thenApply(aVoid -> encumbrancesProcessingHolderBuilder.distributeHoldersByOperation(encumbranceRelationsHolders))
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

}
