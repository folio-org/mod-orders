package org.folio.service.finance.transaction;

import org.folio.models.EncumbranceRelationsHolder;
import org.folio.models.EncumbrancesProcessingHolder;
import org.folio.rest.acq.model.finance.Transaction;
import org.folio.rest.core.models.RequestContext;
import org.folio.rest.jaxrs.model.CompositePurchaseOrder;
import org.folio.service.orders.OrderWorkflowType;

import java.util.List;
import java.util.Objects;
import java.util.concurrent.CompletableFuture;

import static java.util.stream.Collectors.toList;
import static org.folio.orders.utils.FundDistributionUtils.validateFundDistributionTotal;

public class PendingToPendingEncumbranceStrategy implements EncumbranceWorkflowStrategy {

  private final EncumbranceService encumbranceService;
  private final EncumbranceRelationsHoldersBuilder encumbranceRelationsHoldersBuilder;

  public PendingToPendingEncumbranceStrategy(EncumbranceService encumbranceService,
    EncumbranceRelationsHoldersBuilder encumbranceRelationsHoldersBuilder) {
    this.encumbranceService = encumbranceService;
    this.encumbranceRelationsHoldersBuilder = encumbranceRelationsHoldersBuilder;
  }

  @Override
  public CompletableFuture<Void> processEncumbrances(CompositePurchaseOrder compPO, CompositePurchaseOrder poAndLinesFromStorage,
      RequestContext requestContext) {

    validateFundDistributionTotal(compPO.getCompositePoLines());
    List<EncumbranceRelationsHolder> encumbranceRelationsHolders = encumbranceRelationsHoldersBuilder.buildBaseHolders(compPO);
    return encumbranceRelationsHoldersBuilder.withExistingTransactions(encumbranceRelationsHolders, poAndLinesFromStorage, requestContext)
      .thenApply(aVoid -> distributeHoldersByOperation(encumbranceRelationsHolders))
      .thenCompose(holder -> encumbranceService.createOrUpdateEncumbrances(holder, requestContext));
  }

  @Override
  public OrderWorkflowType getStrategyName() {
    return OrderWorkflowType.PENDING_TO_PENDING;
  }

  private EncumbrancesProcessingHolder distributeHoldersByOperation(List<EncumbranceRelationsHolder> encumbranceRelationsHolders) {
    EncumbrancesProcessingHolder holder = new EncumbrancesProcessingHolder();
    holder.withEncumbrancesFromStorage(encumbranceRelationsHolders.stream()
      .map(EncumbranceRelationsHolder::getOldEncumbrance)
      .filter(Objects::nonNull)
      .collect(toList()));
    holder.withEncumbrancesForRelease(getTransactionsToDelete(encumbranceRelationsHolders));
    holder.withEncumbrancesForDelete(getTransactionsToDelete(encumbranceRelationsHolders));
    return holder;
  }

  private List<Transaction> getTransactionsToDelete(List<EncumbranceRelationsHolder> encumbranceRelationsHolders) {
    return encumbranceRelationsHolders.stream()
      .filter(holder -> Objects.isNull(holder.getNewEncumbrance()))
      .map(EncumbranceRelationsHolder::getOldEncumbrance)
      .collect(toList());
  }

}
