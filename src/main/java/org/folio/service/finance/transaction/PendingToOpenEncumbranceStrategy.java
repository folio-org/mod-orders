package org.folio.service.finance.transaction;

import static org.folio.orders.utils.FundDistributionUtils.validateFundDistributionTotal;

import java.util.List;

import org.folio.models.EncumbranceRelationsHolder;
import org.folio.rest.core.models.RequestContext;
import org.folio.rest.jaxrs.model.CompositePurchaseOrder;
import org.folio.service.FundsDistributionService;
import org.folio.service.finance.budget.BudgetRestrictionService;
import org.folio.service.invoice.POLInvoiceLineRelationService;
import org.folio.service.orders.OrderWorkflowType;

import io.vertx.core.Future;

public class PendingToOpenEncumbranceStrategy implements EncumbranceWorkflowStrategy {

  private final EncumbranceService encumbranceService;
  private final FundsDistributionService fundsDistributionService;
  private final BudgetRestrictionService budgetRestrictionService;
  private final EncumbranceRelationsHoldersBuilder encumbranceRelationsHoldersBuilder;
  private final EncumbrancesProcessingHolderBuilder encumbrancesProcessingHolderBuilder;
  private final POLInvoiceLineRelationService polInvoiceLineRelationService;

  public PendingToOpenEncumbranceStrategy(EncumbranceService encumbranceService, FundsDistributionService fundsDistributionService,
                                          BudgetRestrictionService budgetRestrictionService, EncumbranceRelationsHoldersBuilder encumbranceRelationsHoldersBuilder,
                                          EncumbrancesProcessingHolderBuilder encumbrancesProcessingHolderBuilder, POLInvoiceLineRelationService polInvoiceLineRelationService) {
    this.encumbranceService = encumbranceService;
    this.fundsDistributionService = fundsDistributionService;
    this.budgetRestrictionService = budgetRestrictionService;
    this.encumbranceRelationsHoldersBuilder = encumbranceRelationsHoldersBuilder;
    this.encumbrancesProcessingHolderBuilder = encumbrancesProcessingHolderBuilder;
    this.polInvoiceLineRelationService = polInvoiceLineRelationService;
  }

  @Override
  public Future<List<EncumbranceRelationsHolder>> prepareProcessEncumbrancesAndValidate(CompositePurchaseOrder compPO,
                                                                                        CompositePurchaseOrder poAndLinesFromStorage,
                                                                                        RequestContext requestContext) {
    validateFundDistributionTotal(compPO.getCompositePoLines());
    List<EncumbranceRelationsHolder> holders = encumbranceRelationsHoldersBuilder.buildBaseHolders(compPO);

    return encumbranceRelationsHoldersBuilder.withFinances(holders, requestContext)
      .compose(v -> encumbranceRelationsHoldersBuilder.withExistingTransactions(holders, poAndLinesFromStorage, requestContext))
      .map(v -> fundsDistributionService.distributeFunds(holders))
      .map(dataHolders -> {
        budgetRestrictionService.checkEncumbranceRestrictions(dataHolders);
        return null;
      })
      .map(v -> holders);
  }

  @Override
  public Future<Void> processEncumbrances(CompositePurchaseOrder compPO, CompositePurchaseOrder poAndLinesFromStorage,
                                          RequestContext requestContext) {
    return prepareProcessEncumbrancesAndValidate(compPO, poAndLinesFromStorage, requestContext)
      .map(encumbrancesProcessingHolderBuilder::distributeHoldersByOperation)
      .compose(holder -> polInvoiceLineRelationService.manageInvoiceRelation(holder, requestContext))
      .compose(holder -> encumbranceService.createOrUpdateEncumbrances(holder, requestContext));
  }

  @Override
  public OrderWorkflowType getStrategyName() {
    return OrderWorkflowType.PENDING_TO_OPEN;
  }
}
