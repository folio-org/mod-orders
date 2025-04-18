package org.folio.service.finance.transaction;

import static java.util.stream.Collectors.toList;
import static org.folio.orders.utils.FundDistributionUtils.isFundDistributionsPresent;

import java.util.List;
import java.util.stream.Collectors;

import org.apache.logging.log4j.LogManager;
import org.apache.logging.log4j.Logger;
import org.folio.HttpStatus;
import org.folio.models.EncumbranceRelationsHolder;
import org.folio.models.EncumbrancesProcessingHolder;
import org.folio.rest.core.exceptions.HttpException;
import org.folio.rest.core.models.RequestContext;
import org.folio.rest.jaxrs.model.CompositePurchaseOrder;
import org.folio.service.FundsDistributionService;
import org.folio.service.finance.budget.BudgetRestrictionService;
import org.folio.service.orders.OrderWorkflowType;

import io.vertx.core.Future;

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
  public Future<Void> processEncumbrances(CompositePurchaseOrder compPO, CompositePurchaseOrder poAndLinesFromStorage,
      RequestContext requestContext) {

    if (isFundDistributionsPresent(compPO.getPoLines())) {
      // get the encumbrances to unrelease
      return encumbranceRelationsHoldersBuilder.retrieveMapFiscalYearsWithPoLines(compPO, poAndLinesFromStorage, requestContext)
        .compose(mapFiscalYearIdsWithPoLines -> encumbranceService.getOrderEncumbrancesToUnrelease(compPO, mapFiscalYearIdsWithPoLines, requestContext))
        .compose(transactions -> {
          // stop if nothing needs to be done

          if (transactions.isEmpty() && compPO.getPoLines().stream().noneMatch(
              pol -> pol.getFundDistribution().stream().anyMatch(f -> f.getEncumbrance() == null))) {
            return Future.succeededFuture();
          }

          // check encumbrance restrictions as in PendingToOpenEncumbranceStrategy
          // (except we use a different list of polines/transactions)
          List<EncumbranceRelationsHolder> holders = encumbranceRelationsHoldersBuilder
            .buildBaseHolders(compPO)
            // only keep holders with a missing encumbrance or a matching selected transaction
            .stream()
            .filter(h -> h.getFundDistribution().getEncumbrance() == null || transactions.stream()
              .anyMatch(t -> t.getEncumbrance().getSourcePoLineId().equals(h.getPoLineId())))
            .collect(Collectors.toList());
          return encumbranceRelationsHoldersBuilder.withFinances(holders, requestContext)
            // use given transactions (withKnownTransactions) instead of retrieving them (withExistingTransactions)
            .map(v -> {
              encumbranceRelationsHoldersBuilder.withKnownTransactions(holders, transactions);
              return holders;
            })
            .map(fundsDistributionService::distributeFunds)
            .map(dataHolders -> {
              budgetRestrictionService.checkEncumbranceRestrictions(dataHolders);
              return null;
            })
            .compose(v -> {
              // create missing encumbrances and unrelease existing ones
              EncumbrancesProcessingHolder holder = new EncumbrancesProcessingHolder();
              List<EncumbranceRelationsHolder> toBeCreatedHolders = holders.stream()
                .filter(h -> h.getOldEncumbrance() == null)
                .collect(toList());
              holder.withEncumbrancesForCreate(toBeCreatedHolders);
              holder.withEncumbrancesForUnrelease(transactions);
              return encumbranceService.createOrUpdateEncumbrances(holder, requestContext);
            });
        })
         .recover(t -> {
          logger.error(PROCESS_ENCUMBRANCES_ERROR, t);
          throw new HttpException(HttpStatus.HTTP_INTERNAL_SERVER_ERROR.toInt(), PROCESS_ENCUMBRANCES_ERROR +
            (t.getMessage() != null ? ": " + t.getMessage() : ""));
        });
    }
    return Future.succeededFuture();
  }

  @Override
  public OrderWorkflowType getStrategyName() {
    return OrderWorkflowType.CLOSED_TO_OPEN;
  }
}
