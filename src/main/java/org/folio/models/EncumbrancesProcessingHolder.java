package org.folio.models;

import java.util.ArrayList;
import java.util.List;

import org.folio.rest.acq.model.finance.Transaction;

import com.google.common.collect.ImmutableList;

public class EncumbrancesProcessingHolder {
  private List<Transaction> encumbrancesFromStorage;
  private List<Transaction> encumbrancesForRelease;
  private List<EncumbranceRelationsHolder> encumbrancesForCreate;
  private List<EncumbranceRelationsHolder> encumbrancesForUpdate;

  public EncumbrancesProcessingHolder() {
    this.encumbrancesFromStorage = new ArrayList<>();
    this.encumbrancesForCreate = new ArrayList<>();
    this.encumbrancesForUpdate = new ArrayList<>();
    this.encumbrancesForRelease = new ArrayList<>();
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

  public EncumbrancesProcessingHolder withEncumbrancesForUpdate(List<EncumbranceRelationsHolder> encumbrancesForUpdate) {
    this.encumbrancesForUpdate = new ArrayList<>(encumbrancesForUpdate);
    return this;
  }

  public EncumbrancesProcessingHolder withEncumbrancesForRelease(List<Transaction> encumbrancesForRelease) {
    this.encumbrancesForRelease = new ArrayList<>(encumbrancesForRelease);
    return this;
  }

  public EncumbrancesProcessingHolder withEncumbrancesFromStorage(List<Transaction> encumbrancesFromStorage) {
    this.encumbrancesFromStorage = new ArrayList<>(encumbrancesFromStorage);
    return this;
  }

  public List<EncumbranceRelationsHolder> getEncumbrancesForCreate() {
    return encumbrancesForCreate;
  }

  public List<EncumbranceRelationsHolder> getEncumbrancesForUpdate() {
    return encumbrancesForUpdate;
  }

  public List<Transaction> getEncumbrancesForRelease() {
    return encumbrancesForRelease;
  }

  public int getAllEncumbrancesQuantity(){
      return encumbrancesForCreate.size() + encumbrancesForRelease.size() + encumbrancesForUpdate.size();
  }

  public List<Transaction> getEncumbrancesFromStorage() {
    return ImmutableList.copyOf(encumbrancesFromStorage);
  }
}
