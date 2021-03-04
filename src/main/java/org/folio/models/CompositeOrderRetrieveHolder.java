package org.folio.models;

import org.folio.rest.acq.model.finance.FiscalYear;
import org.folio.rest.acq.model.finance.Transaction;
import org.folio.rest.jaxrs.model.CompositePurchaseOrder;

import java.util.ArrayList;
import java.util.List;
import java.util.Optional;

public class CompositeOrderRetrieveHolder {
    private CompositePurchaseOrder order;
    private FiscalYear fiscalYear;
    private List<Transaction> currentEncumbrances = new ArrayList<>();

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

    public CompositeOrderRetrieveHolder withNeedReEncumber(boolean needReEncumber) {
        order.setNeedReEncumber(needReEncumber);
        return this;
    }

    public List<Transaction> getCurrentEncumbrances() {
        return currentEncumbrances;
    }

    public CompositeOrderRetrieveHolder withCurrentEncumbrances(List<Transaction> currentEncumbrances) {
        this.currentEncumbrances = currentEncumbrances;
        return this;
    }
}
