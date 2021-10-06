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
  private List<EncumbranceRelationsHolder> encumbranceRelationsHolders;
  private boolean prepared;

  public PendingToOpenEncumbranceStrategy(EncumbranceService encumbranceService, FundsDistributionService fundsDistributionService,
      BudgetRestrictionService budgetRestrictionService, EncumbranceRelationsHoldersBuilder encumbranceRelationsHoldersBuilder,
      EncumbrancesProcessingHolderBuilder encumbrancesProcessingHolderBuilder) {
    this.encumbranceService = encumbranceService;
    this.fundsDistributionService = fundsDistributionService;
    this.budgetRestrictionService = budgetRestrictionService;
    this.encumbranceRelationsHoldersBuilder = encumbranceRelationsHoldersBuilder;
    this.encumbrancesProcessingHolderBuilder = encumbrancesProcessingHolderBuilder;
    prepared = false;
  }

  @Override
  public CompletableFuture<Void> prepareProcessEncumbrancesAndValidate(CompositePurchaseOrder compPO,
      CompositePurchaseOrder poAndLinesFromStorage, RequestContext requestContext) {
    validateFundDistributionTotal(compPO.getCompositePoLines());
    encumbranceRelationsHolders = encumbranceRelationsHoldersBuilder.buildBaseHolders(compPO);
    return encumbranceRelationsHoldersBuilder.withBudgets(encumbranceRelationsHolders, requestContext)
      .thenCompose(holders -> encumbranceRelationsHoldersBuilder.withLedgersData(holders, requestContext))
      .thenCompose(holders -> encumbranceRelationsHoldersBuilder.withFiscalYearData(holders, requestContext))
      .thenCompose(holders -> encumbranceRelationsHoldersBuilder.withConversion(holders, requestContext))
      .thenCompose(holders -> encumbranceRelationsHoldersBuilder.withExistingTransactions(holders, poAndLinesFromStorage, requestContext))
      .thenApply(fundsDistributionService::distributeFunds)
      .thenAccept(budgetRestrictionService::checkEncumbranceRestrictions)
      .thenAccept(v -> {
        prepared = true;
      });
  }

  @Override
  public CompletableFuture<Void> processEncumbrances(CompositePurchaseOrder compPO, CompositePurchaseOrder poAndLinesFromStorage,
      RequestContext requestContext) {
    CompletableFuture<Void> f;
    if (prepared)
      f = CompletableFuture.completedFuture(null);
    else
      f = prepareProcessEncumbrancesAndValidate(compPO, poAndLinesFromStorage, requestContext);
    return f.thenApply(v -> encumbrancesProcessingHolderBuilder.distributeHoldersByOperation(encumbranceRelationsHolders))
      .thenCompose(holder -> encumbranceService.createOrUpdateEncumbrances(holder, requestContext))
      .whenComplete((v, t) -> {
        prepared = false;
      });
  }

  @Override
  public OrderWorkflowType getStrategyName() {
    return OrderWorkflowType.PENDING_TO_OPEN;
  }
}
