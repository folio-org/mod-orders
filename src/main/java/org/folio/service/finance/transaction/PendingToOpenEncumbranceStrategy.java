package org.folio.service.finance.transaction;

import static org.folio.orders.utils.FundDistributionUtils.validateFundDistributionTotal;

import java.util.List;
import java.util.concurrent.CompletableFuture;

import org.folio.models.EncumbranceRelationsHolder;
import org.folio.rest.core.models.RequestContext;
import org.folio.rest.jaxrs.model.CompositePurchaseOrder;
import org.folio.service.FundsDistributionService;
import org.folio.service.finance.budget.BudgetRestrictionService;
import org.folio.service.orders.OrderWorkflowType;

public class PendingToOpenEncumbranceStrategy implements EncumbranceWorkflowStrategy {

  private final EncumbranceService encumbranceService;
  private final FundsDistributionService fundsDistributionService;
  private final BudgetRestrictionService budgetRestrictionService;
  private final EncumbranceRelationsHoldersBuilder encumbranceRelationsHoldersBuilder;
  private final EncumbrancesProcessingHolderBuilder encumbrancesProcessingHolderBuilder;

  public PendingToOpenEncumbranceStrategy(EncumbranceService encumbranceService, FundsDistributionService fundsDistributionService,
      BudgetRestrictionService budgetRestrictionService, EncumbranceRelationsHoldersBuilder encumbranceRelationsHoldersBuilder,
      EncumbrancesProcessingHolderBuilder encumbrancesProcessingHolderBuilder) {
    this.encumbranceService = encumbranceService;
    this.fundsDistributionService = fundsDistributionService;
    this.budgetRestrictionService = budgetRestrictionService;
    this.encumbranceRelationsHoldersBuilder = encumbranceRelationsHoldersBuilder;
    this.encumbrancesProcessingHolderBuilder = encumbrancesProcessingHolderBuilder;
  }

  @Override
  public CompletableFuture<List<EncumbranceRelationsHolder>> prepareProcessEncumbrancesAndValidate(
      CompositePurchaseOrder compPO, CompositePurchaseOrder poAndLinesFromStorage, RequestContext requestContext) {
    validateFundDistributionTotal(compPO.getCompositePoLines());
    List<EncumbranceRelationsHolder> encumbranceRelationsHolders = encumbranceRelationsHoldersBuilder.buildBaseHolders(compPO);
    return encumbranceRelationsHoldersBuilder.withBudgets(encumbranceRelationsHolders, requestContext)
      .thenCompose(holders -> encumbranceRelationsHoldersBuilder.withLedgersData(holders, requestContext))
      .thenCompose(holders -> encumbranceRelationsHoldersBuilder.withFiscalYearData(holders, requestContext))
      .thenCompose(holders -> encumbranceRelationsHoldersBuilder.withConversion(holders, requestContext))
      .thenCompose(holders -> encumbranceRelationsHoldersBuilder.withExistingTransactions(holders, poAndLinesFromStorage, requestContext))
      .thenApply(fundsDistributionService::distributeFunds)
      .thenAccept(budgetRestrictionService::checkEncumbranceRestrictions)
      .thenApply(v -> encumbranceRelationsHolders);
  }

  @Override
  public CompletableFuture<Void> processEncumbrances(CompositePurchaseOrder compPO,
      CompositePurchaseOrder poAndLinesFromStorage, RequestContext requestContext) {
    return prepareProcessEncumbrancesAndValidate(compPO, poAndLinesFromStorage, requestContext)
      .thenApply(encumbrancesProcessingHolderBuilder::distributeHoldersByOperation)
      .thenCompose(holder -> encumbranceService.createOrUpdateEncumbrances(holder, requestContext));
  }

  @Override
  public OrderWorkflowType getStrategyName() {
    return OrderWorkflowType.PENDING_TO_OPEN;
  }
}
