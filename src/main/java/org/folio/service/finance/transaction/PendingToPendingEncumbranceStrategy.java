package org.folio.service.finance.transaction;

import static org.folio.orders.utils.FundDistributionUtils.validateFundDistributionTotal;

import java.util.List;
import java.util.Objects;

import org.folio.models.EncumbranceRelationsHolder;
import org.folio.models.EncumbrancesProcessingHolder;
import org.folio.rest.acq.model.finance.Transaction;
import org.folio.rest.core.models.RequestContext;
import org.folio.rest.jaxrs.model.CompositePurchaseOrder;
import org.folio.service.orders.OrderWorkflowType;

import io.vertx.core.Future;

public class PendingToPendingEncumbranceStrategy implements EncumbranceWorkflowStrategy {

  private final EncumbranceService encumbranceService;
  private final EncumbranceRelationsHoldersBuilder encumbranceRelationsHoldersBuilder;

  public PendingToPendingEncumbranceStrategy(EncumbranceService encumbranceService,
    EncumbranceRelationsHoldersBuilder encumbranceRelationsHoldersBuilder) {
    this.encumbranceService = encumbranceService;
    this.encumbranceRelationsHoldersBuilder = encumbranceRelationsHoldersBuilder;
  }

  @Override
  public Future<Void> processEncumbrances(CompositePurchaseOrder compPO, CompositePurchaseOrder poAndLinesFromStorage,
      RequestContext requestContext) {

    validateFundDistributionTotal(compPO.getCompositePoLines());
    List<EncumbranceRelationsHolder> encumbranceRelationsHolders = encumbranceRelationsHoldersBuilder.buildBaseHolders(compPO);
    return encumbranceRelationsHoldersBuilder.withExistingTransactions(encumbranceRelationsHolders, poAndLinesFromStorage, requestContext)
      .map(aVoid -> distributeHoldersByOperation(encumbranceRelationsHolders))
      .compose(holder -> encumbranceService.createOrUpdateEncumbrances(holder, requestContext));
  }

  @Override
  public OrderWorkflowType getStrategyName() {
    return OrderWorkflowType.PENDING_TO_PENDING;
  }

  private EncumbrancesProcessingHolder distributeHoldersByOperation(List<EncumbranceRelationsHolder> encumbranceRelationsHolders) {
    EncumbrancesProcessingHolder holder = new EncumbrancesProcessingHolder();
    List<EncumbranceRelationsHolder> toUpdate = getTransactionsToUpdate(encumbranceRelationsHolders);
    List<EncumbranceRelationsHolder> toDelete = getTransactionsToDelete(encumbranceRelationsHolders);
    List<Transaction> toRelease = toDelete.stream().map(EncumbranceRelationsHolder::getOldEncumbrance).toList();
    holder.withEncumbrancesForUpdate(toUpdate);
    holder.withEncumbrancesForRelease(toRelease);
    holder.withEncumbrancesForDelete(toDelete);
    return holder;
  }

  private List<EncumbranceRelationsHolder> getTransactionsToDelete(List<EncumbranceRelationsHolder> encumbranceRelationsHolders) {
    return encumbranceRelationsHolders.stream()
      .filter(holder -> Objects.isNull(holder.getNewEncumbrance()))
      .toList();
  }

  private List<EncumbranceRelationsHolder> getTransactionsToUpdate(List<EncumbranceRelationsHolder> encumbranceRelationsHolders) {
    // Return a list of holders with transactions that need to have an expense class update.
    // We want to avoid loading all financial data like in pending->open, so we copy some fields
    // from the old encumbrance (amount is 0 with pending orders).
    List<EncumbranceRelationsHolder> toUpdate = encumbranceRelationsHolders.stream()
      .filter(holder -> holder.getNewEncumbrance() != null && holder.getOldEncumbrance() != null &&
        !Objects.equals(holder.getNewEncumbrance().getExpenseClassId(), holder.getOldEncumbrance().getExpenseClassId()))
      .toList();
    toUpdate.forEach(holder -> {
      Transaction oldEncumbrance = holder.getOldEncumbrance();
      Transaction newEncumbrance = holder.getNewEncumbrance();
      newEncumbrance.withFiscalYearId(oldEncumbrance.getFiscalYearId())
        .withCurrency(oldEncumbrance.getCurrency())
        .withAmount(oldEncumbrance.getAmount())
        .getEncumbrance().withInitialAmountEncumbered(oldEncumbrance.getEncumbrance().getInitialAmountEncumbered());
    });
    return toUpdate;
  }

}
