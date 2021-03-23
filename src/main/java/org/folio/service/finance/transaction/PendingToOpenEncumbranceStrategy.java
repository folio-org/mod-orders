package org.folio.service.finance.transaction;

import static java.util.stream.Collectors.toList;
import static org.folio.orders.utils.FundDistributionUtils.isFundDistributionsPresent;
import static org.folio.orders.utils.FundDistributionUtils.validateFundDistributionTotal;

import java.util.List;
import java.util.Objects;
import java.util.concurrent.CompletableFuture;

import org.folio.models.EncumbranceRelationsHolder;
import org.folio.models.EncumbrancesProcessingHolder;
import org.folio.rest.acq.model.finance.Transaction;
import org.folio.rest.core.models.RequestContext;
import org.folio.rest.jaxrs.model.CompositePurchaseOrder;
import org.folio.service.FundsDistributionService;
import org.folio.service.finance.WorkflowStatusName;
import org.folio.service.finance.budget.BudgetRestrictionService;

public class PendingToOpenEncumbranceStrategy implements EncumbranceWorkflowStrategy {

    private final EncumbranceService encumbranceService;
    private final FundsDistributionService fundsDistributionService;
    private final BudgetRestrictionService budgetRestrictionService;
    private final EncumbranceRelationsHoldersBuilder encumbranceRelationsHoldersBuilder;

    public PendingToOpenEncumbranceStrategy(EncumbranceService encumbranceService,
                                            FundsDistributionService fundsDistributionService,
                                            BudgetRestrictionService budgetRestrictionService,
                                            EncumbranceRelationsHoldersBuilder encumbranceRelationsHoldersBuilder) {
        this.encumbranceService = encumbranceService;
        this.fundsDistributionService = fundsDistributionService;
        this.budgetRestrictionService = budgetRestrictionService;
        this.encumbranceRelationsHoldersBuilder = encumbranceRelationsHoldersBuilder;
    }

    @Override
    public CompletableFuture<Void> processEncumbrances(CompositePurchaseOrder compPO, RequestContext requestContext) {

        if (isFundDistributionsPresent(compPO.getCompositePoLines())) {
            validateFundDistributionTotal(compPO.getCompositePoLines());
            List<EncumbranceRelationsHolder> encumbranceRelationsHolders = encumbranceRelationsHoldersBuilder
              .buildBaseHolders(compPO);
            return encumbranceRelationsHoldersBuilder.withBudgets(encumbranceRelationsHolders, requestContext)
              .thenCompose(holders -> encumbranceRelationsHoldersBuilder.withLedgersData(holders, requestContext))
              .thenCompose(holders -> encumbranceRelationsHoldersBuilder.withFiscalYearData(holders, requestContext))
              .thenCompose(holders -> encumbranceRelationsHoldersBuilder.withConversion(holders, requestContext))
              .thenCompose(holders -> encumbranceRelationsHoldersBuilder.withExistingTransactions(holders, requestContext))
              .thenApply(fundsDistributionService::distributeFunds)
              .thenAccept(budgetRestrictionService::checkEncumbranceRestrictions)
              .thenApply(aVoid -> distributeHoldersByOperation(encumbranceRelationsHolders))
              .thenCompose(holder -> encumbranceService.createOrUpdateEncumbrances(holder, requestContext));
        }
        return CompletableFuture.completedFuture(null);
    }

    private EncumbrancesProcessingHolder distributeHoldersByOperation(
        List<EncumbranceRelationsHolder> encumbranceRelationsHolders) {
      EncumbrancesProcessingHolder holder = new EncumbrancesProcessingHolder();
      holder.withEncumbrancesForCreate(getToBeCreatedHolders(encumbranceRelationsHolders));
      holder.withEncumbrancesForUpdate(getToBeUpdatedHolders(encumbranceRelationsHolders));
      holder.withEncumbrancesForRelease(getToBeReleasedHolders(encumbranceRelationsHolders));
      holder.withEncumbrancesFromStorage(encumbranceRelationsHolders.stream()
                                             .map(EncumbranceRelationsHolder::getOldEncumbrance)
                                             .filter(Objects::nonNull)
                                             .collect(toList()));
      return holder;
    }

    private List<Transaction> getToBeReleasedHolders(List<EncumbranceRelationsHolder> encumbranceRelationsHolders) {
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

    @Override
    public WorkflowStatusName getStrategyName() {
      return WorkflowStatusName.PENDING_TO_OPEN;
    }
}
