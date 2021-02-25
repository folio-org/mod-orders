package org.folio.models;

import org.folio.rest.acq.model.finance.Budget;
import org.folio.rest.acq.model.finance.FiscalYear;
import org.folio.rest.acq.model.finance.Fund;
import org.folio.rest.acq.model.finance.Ledger;
import org.folio.rest.acq.model.finance.Transaction;
import org.folio.rest.jaxrs.model.CompositePoLine;
import org.folio.rest.jaxrs.model.CompositePurchaseOrder;
import org.folio.rest.jaxrs.model.EncumbranceRollover;
import org.folio.rest.jaxrs.model.FundDistribution;
import org.folio.rest.jaxrs.model.LedgerFiscalYearRollover;

import javax.money.convert.CurrencyConversion;

public class ReEncumbranceHolder {

    private CompositePurchaseOrder purchaseOrder;
    private CompositePoLine poLine;
    private FundDistribution fundDistribution;
    private LedgerFiscalYearRollover rollover;
    private Fund fund;
    private Budget budget;
    private Ledger ledger;
    private EncumbranceRollover encumbranceRollover;
    private Transaction fromFYEncumbrance;
    private Transaction toFYEncumbrance;
    private FiscalYear currentFiscalYear;
    private CurrencyConversion poLineToFyConversion;


    public CompositePurchaseOrder getPurchaseOrder() {
        return purchaseOrder;
    }

    public CompositePoLine getPoLine() {
        return poLine;
    }

    public FundDistribution getFundDistribution() {
        return fundDistribution;
    }

    public String getFundId() {
        return fundDistribution.getFundId();
    }

    public LedgerFiscalYearRollover getRollover() {
        return rollover;
    }

    public Transaction getFromFYEncumbrance() {
        return fromFYEncumbrance;
    }

    public Transaction getToFYEncumbrance() {
        return toFYEncumbrance;
    }

    public ReEncumbranceHolder withPurchaseOrder(CompositePurchaseOrder purchaseOrder) {
        this.purchaseOrder = purchaseOrder;
        return this;
    }

    public ReEncumbranceHolder withPoLine(CompositePoLine poLine) {
        this.poLine = poLine;
        return this;
    }


    public ReEncumbranceHolder withRollover(LedgerFiscalYearRollover rollover) {
        this.rollover = rollover;
        return this;
    }

    public ReEncumbranceHolder withFundDistribution(FundDistribution fundDistribution) {
        this.fundDistribution = fundDistribution;
        return this;
    }

    public ReEncumbranceHolder withFromFYEncumbrance(Transaction transaction) {
        this.fromFYEncumbrance = transaction;
        return this;
    }

    public ReEncumbranceHolder withToFYEncumbrance(Transaction transaction) {
        this.toFYEncumbrance = transaction;
        return this;
    }

    public CurrencyConversion getPoLineToFyConversion() {
        return poLineToFyConversion;
    }

    public boolean getRestrictEncumbrance() {
        return Boolean.TRUE.equals(ledger.getRestrictEncumbrance());
    }

    public ReEncumbranceHolder withPoLineToFyConversion(CurrencyConversion poLineToFyConversion) {
        this.poLineToFyConversion = poLineToFyConversion;
        return this;
    }

    public Fund getFund() {
        return fund;
    }

    public ReEncumbranceHolder withFund(Fund fund) {
        this.fund = fund;
        return this;
    }

    public Budget getBudget() {
        return budget;
    }

    public ReEncumbranceHolder withBudget(Budget budget) {
        this.budget = budget;
        return this;
    }

    public Ledger getLedger() {
        return ledger;
    }

    public ReEncumbranceHolder withLedger(Ledger ledger) {
        this.ledger = ledger;
        return this;
    }

    public FiscalYear getCurrentFiscalYear() {
        return currentFiscalYear;
    }

    public ReEncumbranceHolder withCurrentFiscalYear(FiscalYear currentFiscalYear) {
        this.currentFiscalYear = currentFiscalYear;
        return this;
    }

    public EncumbranceRollover getEncumbranceRollover() {
        return encumbranceRollover;
    }

    public ReEncumbranceHolder withEncumbranceRollover(EncumbranceRollover encumbranceRollover) {
        this.encumbranceRollover = encumbranceRollover;
        return this;
    }
}
