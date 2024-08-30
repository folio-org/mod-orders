package org.folio.models;

import java.util.Optional;

import org.folio.rest.acq.model.finance.FiscalYear;
import org.folio.rest.jaxrs.model.CompositePurchaseOrder;

public class CompositeOrderRetrieveHolder {
    private CompositePurchaseOrder order;
    private FiscalYear fiscalYear;

    public CompositeOrderRetrieveHolder(CompositePurchaseOrder order) {
        this.order = order;
    }

    public FiscalYear getFiscalYear() {
        return fiscalYear;
    }

    public CompositeOrderRetrieveHolder withFiscalYear(FiscalYear fiscalYear) {
        this.fiscalYear = fiscalYear;
        return this;
    }

    public CompositePurchaseOrder getOrder() {
        return order;
    }

    public String getOrderId() {
        return order.getId();
    }

    public String getFiscalYearId() {
        return Optional.ofNullable(fiscalYear).map(FiscalYear::getId).orElse(null);
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
