package org.folio.models;

import java.util.ArrayList;
import java.util.List;

import org.folio.rest.acq.model.finance.Transaction;

public class EncumbrancesProcessingHolder {
  private List<Transaction> encumbrancesForRelease;
  private List<EncumbranceRelationsHolder> encumbrancesForCreate;
  private List<EncumbranceRelationsHolder> encumbrancesForDelete;
  private List<EncumbranceRelationsHolder> encumbrancesForUpdate;
  private List<Transaction> encumbrancesForUnrelease;
  private List<Transaction> pendingPaymentsToUpdate;

  public EncumbrancesProcessingHolder() {
    this.encumbrancesForCreate = new ArrayList<>();
    this.encumbrancesForDelete = new ArrayList<>();
    this.encumbrancesForUpdate = new ArrayList<>();
    this.encumbrancesForRelease = new ArrayList<>();
    this.encumbrancesForUnrelease = new ArrayList<>();
    this.pendingPaymentsToUpdate = new ArrayList<>();
  }

  public EncumbrancesProcessingHolder addEncumbrancesForCreate(EncumbranceRelationsHolder encumbranceForCreate) {
    this.encumbrancesForCreate.add(encumbranceForCreate);
    return this;
  }

  public EncumbrancesProcessingHolder addEncumbranceForUpdate(EncumbranceRelationsHolder encumbranceForUpdate) {
    this.encumbrancesForUpdate.add(encumbranceForUpdate);
    return this;
  }

  public EncumbrancesProcessingHolder addEncumbranceForRelease(Transaction transaction) {
    this.encumbrancesForRelease.add(transaction);
    return this;
  }

  public EncumbrancesProcessingHolder withEncumbrancesForCreate(List<EncumbranceRelationsHolder> encumbrancesForCreate) {
    this.encumbrancesForCreate = new ArrayList<>(encumbrancesForCreate);
    return this;
  }

  public EncumbrancesProcessingHolder withEncumbrancesForDelete(List<EncumbranceRelationsHolder> encumbrancesForDelete) {
    this.encumbrancesForDelete = new ArrayList<>(encumbrancesForDelete);
    return this;
  }

  public EncumbrancesProcessingHolder withEncumbrancesForUpdate(List<EncumbranceRelationsHolder> encumbrancesForUpdate) {
    this.encumbrancesForUpdate = new ArrayList<>(encumbrancesForUpdate);
    return this;
  }

  public EncumbrancesProcessingHolder withEncumbrancesForRelease(List<Transaction> encumbrancesForRelease) {
    this.encumbrancesForRelease = new ArrayList<>(encumbrancesForRelease);
    return this;
  }

  public EncumbrancesProcessingHolder withEncumbrancesForUnrelease(List<Transaction> encumbrancesForUnrelease) {
    this.encumbrancesForUnrelease = new ArrayList<>(encumbrancesForUnrelease);
    return this;
  }

  public EncumbrancesProcessingHolder withPendingPaymentsToUpdate(List<Transaction> pendingPayments) {
    this.pendingPaymentsToUpdate = new ArrayList<>(pendingPayments);
    return this;
  }

  public List<EncumbranceRelationsHolder> getEncumbrancesForCreate() {
    return encumbrancesForCreate;
  }

  public List<EncumbranceRelationsHolder> getEncumbrancesForDelete() {
    return encumbrancesForDelete;
  }

  public List<EncumbranceRelationsHolder> getEncumbrancesForUpdate() {
    return encumbrancesForUpdate;
  }

  public List<Transaction> getEncumbrancesForRelease() {
    return encumbrancesForRelease;
  }

  public List<Transaction> getEncumbrancesForUnrelease() {
    return encumbrancesForUnrelease;
  }

  public List<Transaction> getPendingPaymentsToUpdate() {
    return pendingPaymentsToUpdate;
  }

  public boolean isEmpty() {
    return encumbrancesForCreate.size() + encumbrancesForUpdate.size()
        + encumbrancesForRelease.size() + encumbrancesForUnrelease.size()
        + encumbrancesForDelete.size() + pendingPaymentsToUpdate.size() == 0;
  }
}
