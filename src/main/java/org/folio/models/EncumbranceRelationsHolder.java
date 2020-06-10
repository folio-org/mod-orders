package org.folio.models;

import org.folio.rest.acq.model.finance.Transaction;

public class EncumbranceRelationsHolder {
  private Transaction transaction;
  private PoLineFundHolder poLineFundHolder;

  public EncumbranceRelationsHolder() {

  }

  public EncumbranceRelationsHolder(Transaction transaction, PoLineFundHolder poLineFundHolder) {
    this.transaction = transaction;
    this.poLineFundHolder = poLineFundHolder;
  }

  public EncumbranceRelationsHolder withTransaction(Transaction transaction) {
    this.transaction = transaction;
    return this;
  }

  public EncumbranceRelationsHolder withPoLineFund(PoLineFundHolder poLineFundHolder) {
    this.poLineFundHolder = poLineFundHolder;
    return this;
  }

  public Transaction getTransaction() {
    return transaction;
  }

  public PoLineFundHolder getPoLineFundHolder() {
    return poLineFundHolder;
  }
}
