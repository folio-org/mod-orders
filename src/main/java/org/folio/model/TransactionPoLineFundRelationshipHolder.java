package org.folio.model;

import org.folio.rest.acq.model.finance.Transaction;
import org.folio.rest.jaxrs.model.CompositePoLine;
import org.folio.rest.jaxrs.model.FundDistribution;

public class TransactionPoLineFundRelationshipHolder {
  private Transaction transaction;
  private CompositePoLine poLine;
  private FundDistribution fundDistribution;

  public TransactionPoLineFundRelationshipHolder() {

  }

  public TransactionPoLineFundRelationshipHolder(Transaction transaction, CompositePoLine poLine, FundDistribution fundDistribution) {
    this.transaction = transaction;
    this.poLine = poLine;
    this.fundDistribution = fundDistribution;
  }

  public TransactionPoLineFundRelationshipHolder withTransaction(Transaction transaction) {
    this.transaction = transaction;
    return this;
  }

  public TransactionPoLineFundRelationshipHolder withPoLine(CompositePoLine poLine) {
    this.poLine = poLine;
    return this;
  }

  public TransactionPoLineFundRelationshipHolder withFundDistribution(FundDistribution fundDistribution) {
    this.fundDistribution = fundDistribution;
    return this;
  }

  public Transaction getTransaction() {
    return transaction;
  }

  public CompositePoLine getPoLine() {
    return poLine;
  }

  public FundDistribution getFundDistribution() {
    return fundDistribution;
  }
}
