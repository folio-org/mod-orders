package org.folio.service.finance.transaction;

import java.util.List;

import org.apache.logging.log4j.LogManager;
import org.apache.logging.log4j.Logger;
import org.folio.models.EncumbranceRelationsHolder;
import org.folio.rest.core.models.RequestContext;
import org.folio.rest.jaxrs.model.CompositePurchaseOrder;
import org.folio.service.FundsDistributionService;
import org.folio.service.finance.budget.BudgetRestrictionService;
import org.folio.service.orders.OrderWorkflowType;

import io.vertx.core.Future;

public class ReceivingEncumbranceStrategy implements EncumbranceWorkflowStrategy {
  private static final Logger LOG = LogManager.getLogger(ReceivingEncumbranceStrategy.class);

  private final EncumbranceService encumbranceService;
  private final FundsDistributionService fundsDistributionService;
  private final BudgetRestrictionService budgetRestrictionService;
  private final EncumbranceRelationsHoldersBuilder encumbranceRelationsHoldersBuilder;
  private final EncumbrancesProcessingHolderBuilder encumbrancesProcessingHolderBuilder;

  public ReceivingEncumbranceStrategy(EncumbranceService encumbranceService, FundsDistributionService fundsDistributionService,
    BudgetRestrictionService budgetRestrictionService, EncumbranceRelationsHoldersBuilder encumbranceRelationsHoldersBuilder,
    EncumbrancesProcessingHolderBuilder encumbrancesProcessingHolderBuilder) {
      this.encumbranceService = encumbranceService;
      this.fundsDistributionService = fundsDistributionService;
      this.budgetRestrictionService = budgetRestrictionService;
      this.encumbranceRelationsHoldersBuilder = encumbranceRelationsHoldersBuilder;
      this.encumbrancesProcessingHolderBuilder = encumbrancesProcessingHolderBuilder;
  }

  @Override
  public Future<Void> processEncumbrances(CompositePurchaseOrder compPO, CompositePurchaseOrder poAndLinesFromStorage,
                                                        RequestContext requestContext) {
    List<EncumbranceRelationsHolder> encumbranceRelationsHolders = encumbranceRelationsHoldersBuilder.buildBaseHolders(compPO);
    return prepareEncumbranceRelationsHolder(encumbranceRelationsHolders, poAndLinesFromStorage, requestContext)
      .map(fundsDistributionService::distributeFunds)
      .map(dataHolders -> {
        budgetRestrictionService.checkEncumbranceRestrictions(dataHolders);
        return null;
      })
      .map(aVoid -> encumbrancesProcessingHolderBuilder.distributeHoldersByOperation(encumbranceRelationsHolders))
      .compose(holder -> encumbranceService.createOrUpdateEncumbrances(holder, requestContext))
      .onSuccess(holders -> LOG.debug("End processing encumbrances for piece add/delete for order id: {}", compPO.getId()))
      .onFailure(t -> LOG.error("Failed to process encumbrances for piece add/delete for order id: {}", compPO.getId(), t));
  }

  public Future<List<EncumbranceRelationsHolder>> prepareEncumbranceRelationsHolder(List<EncumbranceRelationsHolder> holders,
      CompositePurchaseOrder poFromStorage, RequestContext requestContext) {
    return encumbranceRelationsHoldersBuilder.withFinances(holders, requestContext)
      .compose(v -> encumbranceRelationsHoldersBuilder.withExistingTransactions(holders, poFromStorage, requestContext));
  }

  @Override
  public OrderWorkflowType getStrategyName() {
    return OrderWorkflowType.RECEIVING_PIECE_ADD_OR_DELETE;
  }

}
