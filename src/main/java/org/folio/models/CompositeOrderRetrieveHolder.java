package org.folio.models;

import lombok.Data;
import org.folio.rest.jaxrs.model.CompositePurchaseOrder;

@Data
public class CompositeOrderRetrieveHolder {
    private CompositePurchaseOrder order;
    private String fiscalYearId;
    private String fiscalYearCurrency;

    public CompositeOrderRetrieveHolder(CompositePurchaseOrder order) {
        this.order = order;
    }

    public CompositeOrderRetrieveHolder withFiscalYearId(String fiscalYearId) {
        this.fiscalYearId = fiscalYearId;
        return this;
    }

    public CompositeOrderRetrieveHolder withFiscalYearCurrency(String currency) {
      this.fiscalYearCurrency = currency;
      return this;
    }

    public String getOrderId() {
        return order.getId();
    }

    public CompositeOrderRetrieveHolder withTotalEncumbered(double transactionsTotal) {
        order.setTotalEncumbered(transactionsTotal);
        return this;
    }

    public CompositeOrderRetrieveHolder withTotalExpended(double transactionsTotal) {
        order.setTotalExpended(transactionsTotal);
        return this;
    }

    public CompositeOrderRetrieveHolder withTotalCredited(double transactionsTotal) {
        order.withTotalCredited(transactionsTotal);
        return this;
    }

    public CompositeOrderRetrieveHolder withNeedReEncumber(boolean needReEncumber) {
        order.setNeedReEncumber(needReEncumber);
        return this;
    }

}
