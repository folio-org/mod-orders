package org.folio.service.finance.transaction;

import static java.util.stream.Collectors.toList;

import java.util.List;
import java.util.Objects;

import org.folio.models.EncumbranceRelationsHolder;
import org.folio.models.EncumbrancesProcessingHolder;
import org.folio.rest.acq.model.finance.Transaction;

public class EncumbrancesProcessingHolderBuilder {

  protected EncumbrancesProcessingHolder distributeHoldersByOperation(
                List<EncumbranceRelationsHolder> encumbranceRelationsHolders) {
    EncumbrancesProcessingHolder holder = new EncumbrancesProcessingHolder();
    holder.withEncumbrancesForCreate(getToBeCreatedHolders(encumbranceRelationsHolders));
    holder.withEncumbrancesForUpdate(getToBeUpdatedHolders(encumbranceRelationsHolders));
    List<EncumbranceRelationsHolder> toDelete = getTransactionsToDelete(encumbranceRelationsHolders);
    holder.withEncumbrancesForDelete(toDelete);
    // also release transaction before delete
    List<Transaction> toRelease = toDelete.stream().map(EncumbranceRelationsHolder::getOldEncumbrance).collect(toList());
    holder.withEncumbrancesForRelease(toRelease);
    holder.withEncumbrancesFromStorage(encumbranceRelationsHolders.stream()
      .map(EncumbranceRelationsHolder::getOldEncumbrance)
      .filter(Objects::nonNull)
      .collect(toList()));
    return holder;
  }

  private List<EncumbranceRelationsHolder> getToBeUpdatedHolders(List<EncumbranceRelationsHolder> encumbranceRelationsHolders) {
    return encumbranceRelationsHolders.stream()
      .filter(holder -> Objects.nonNull(holder.getOldEncumbrance()))
      .filter(holder -> Objects.nonNull(holder.getNewEncumbrance()))
      .filter(this::isTransactionUpdated)
      .collect(toList());
  }

  private boolean isTransactionUpdated(EncumbranceRelationsHolder holder) {
    double amountBeforeUpdate = holder.getOldEncumbrance().getAmount();
    double updatedAmount = holder.getNewEncumbrance().getAmount();
    double initialAmountBeforeUpdate = holder.getOldEncumbrance()
      .getEncumbrance().getInitialAmountEncumbered();
    double updatedInitialAmount = holder.getNewEncumbrance()
      .getEncumbrance().getInitialAmountEncumbered();

    return Double.compare(amountBeforeUpdate, updatedAmount) != 0
      || (Double.compare(initialAmountBeforeUpdate, updatedInitialAmount) != 0);
  }

  private List<EncumbranceRelationsHolder> getToBeCreatedHolders(List<EncumbranceRelationsHolder> encumbranceRelationsHolders) {
    return encumbranceRelationsHolders.stream()
      .filter(holder -> Objects.isNull(holder.getOldEncumbrance()))
      .collect(toList());
  }

  private List<EncumbranceRelationsHolder> getTransactionsToDelete(List<EncumbranceRelationsHolder> encumbranceRelationsHolders) {
    return encumbranceRelationsHolders.stream()
      .filter(holder -> Objects.isNull(holder.getNewEncumbrance()))
      .collect(toList());
  }
}
