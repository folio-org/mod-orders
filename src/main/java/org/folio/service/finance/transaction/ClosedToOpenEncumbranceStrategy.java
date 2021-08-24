package org.folio.service.finance.transaction;

import org.apache.logging.log4j.LogManager;
import org.apache.logging.log4j.Logger;
import org.folio.HttpStatus;
import org.folio.models.EncumbranceRelationsHolder;
import org.folio.models.EncumbrancesProcessingHolder;
import org.folio.orders.rest.exceptions.HttpException;
import org.folio.rest.core.models.RequestContext;
import org.folio.rest.jaxrs.model.CompositePurchaseOrder;
import org.folio.service.FundsDistributionService;
import org.folio.service.finance.budget.BudgetRestrictionService;
import org.folio.service.orders.OrderWorkflowType;

import java.util.List;
import java.util.concurrent.CompletableFuture;
import java.util.stream.Collectors;

import static org.folio.orders.utils.FundDistributionUtils.isFundDistributionsPresent;

public class ClosedToOpenEncumbranceStrategy implements EncumbranceWorkflowStrategy {

  private static final String PROCESS_ENCUMBRANCES_ERROR = "Error when processing encumbrances to reopen an order";
  private static final Logger logger = LogManager.getLogger();
  private final EncumbranceService encumbranceService;
  private final FundsDistributionService fundsDistributionService;
  private final BudgetRestrictionService budgetRestrictionService;
  private final EncumbranceRelationsHoldersBuilder encumbranceRelationsHoldersBuilder;

  public ClosedToOpenEncumbranceStrategy(EncumbranceService encumbranceService,
      FundsDistributionService fundsDistributionService,
      BudgetRestrictionService budgetRestrictionService,
      EncumbranceRelationsHoldersBuilder encumbranceRelationsHoldersBuilder) {
    this.encumbranceService = encumbranceService;
    this.fundsDistributionService = fundsDistributionService;
    this.budgetRestrictionService = budgetRestrictionService;
    this.encumbranceRelationsHoldersBuilder = encumbranceRelationsHoldersBuilder;
  }

  @Override
  public CompletableFuture<Void> processEncumbrances(CompositePurchaseOrder compPO, CompositePurchaseOrder poAndLinesFromStorage,
      RequestContext requestContext) {

    if (isFundDistributionsPresent(compPO.getCompositePoLines())) {
      // get the encumbrances to unrelease
      return encumbranceService.getOrderEncumbrancesToUnrelease(compPO, requestContext)
        .thenCompose(transactions -> {
          // stop if nothing needs to be done
          if (transactions.isEmpty())
            return CompletableFuture.completedFuture(null);
          // check encumbrance restrictions as in PendingToOpenEncumbranceStrategy
          // (except we use a different list of polines/transactions)
          List<EncumbranceRelationsHolder> encumbranceRelationsHolders = encumbranceRelationsHoldersBuilder
            .buildBaseHolders(compPO);
          // only keep holders with a matching selected transaction
          encumbranceRelationsHolders = encumbranceRelationsHolders.stream()
            .filter(h -> transactions.stream()
              .anyMatch(t -> t.getEncumbrance().getSourcePoLineId().equals(h.getPoLineId())))
            .collect(Collectors.toList());
          return encumbranceRelationsHoldersBuilder.withBudgets(encumbranceRelationsHolders, requestContext)
            .thenCompose(holders -> encumbranceRelationsHoldersBuilder.withLedgersData(holders, requestContext))
            .thenCompose(holders -> encumbranceRelationsHoldersBuilder.withFiscalYearData(holders, requestContext))
            .thenCompose(holders -> encumbranceRelationsHoldersBuilder.withConversion(holders, requestContext))
            // use given transactions (withKnownTransactions) instead of retrieving them (withExistingTransactions)
            .thenApply(holders -> encumbranceRelationsHoldersBuilder.withKnownTransactions(holders, transactions))
            .thenApply(fundsDistributionService::distributeFunds)
            .thenAccept(budgetRestrictionService::checkEncumbranceRestrictions)
            .thenCompose(aVoid -> {
              // unrelease the encumbrances
              EncumbrancesProcessingHolder holder = new EncumbrancesProcessingHolder();
              holder.withEncumbrancesFromStorage(transactions);
              holder.withEncumbrancesForUnrelease(transactions);
              return encumbranceService.createOrUpdateEncumbrances(holder, requestContext);
            });
        })
        .exceptionally(t -> {
          logger.error(PROCESS_ENCUMBRANCES_ERROR, t);
          throw new HttpException(HttpStatus.HTTP_INTERNAL_SERVER_ERROR.toInt(), PROCESS_ENCUMBRANCES_ERROR +
            (t.getMessage() != null ? ": " + t.getMessage() : ""));
        });
    }
    return CompletableFuture.completedFuture(null);
  }

  @Override
  public OrderWorkflowType getStrategyName() {
    return OrderWorkflowType.CLOSED_TO_OPEN;
  }
}
