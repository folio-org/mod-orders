package org.folio.service.finance.transaction;

import org.folio.models.EncumbranceRelationsHolder;
import org.folio.models.EncumbrancesProcessingHolder;
import org.folio.rest.acq.model.finance.Transaction;

import java.util.List;
import java.util.Objects;

import static java.util.stream.Collectors.toList;

public class EncumbrancesProcessingHolderBuilder {

  protected EncumbrancesProcessingHolder distributeHoldersByOperation(
                List<EncumbranceRelationsHolder> encumbranceRelationsHolders) {
    EncumbrancesProcessingHolder holder = new EncumbrancesProcessingHolder();
    holder.withEncumbrancesForCreate(getToBeCreatedHolders(encumbranceRelationsHolders));
    holder.withEncumbrancesForUpdate(getToBeUpdatedHolders(encumbranceRelationsHolders));
    holder.withEncumbrancesForDelete(getTransactionsToDelete(encumbranceRelationsHolders));
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

  private List<Transaction> getTransactionsToDelete(List<EncumbranceRelationsHolder> encumbranceRelationsHolders) {
    return encumbranceRelationsHolders.stream()
      .filter(holder -> Objects.isNull(holder.getNewEncumbrance()))
      .map(EncumbranceRelationsHolder::getOldEncumbrance)
      .collect(toList());
  }
}
