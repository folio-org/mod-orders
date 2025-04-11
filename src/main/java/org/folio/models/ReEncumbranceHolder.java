package org.folio.models;

import javax.money.convert.CurrencyConversion;

import org.folio.rest.acq.model.finance.Budget;
import org.folio.rest.acq.model.finance.Transaction;
import org.folio.rest.jaxrs.model.PoLine;
import org.folio.rest.jaxrs.model.CompositePurchaseOrder;
import org.folio.rest.jaxrs.model.EncumbranceRollover;
import org.folio.rest.jaxrs.model.FundDistribution;
import org.folio.rest.jaxrs.model.LedgerFiscalYearRollover;

public class ReEncumbranceHolder extends EncumbranceRelationsHolder {

    private LedgerFiscalYearRollover rollover;
    private EncumbranceRollover encumbranceRollover;
    private Transaction previousFyEncumbrance;
    private CurrencyConversion fyToPoLineConversion;

    @Override
    public ReEncumbranceHolder withPurchaseOrder(CompositePurchaseOrder purchaseOrder) {
        super.withPurchaseOrder(purchaseOrder);
        return this;
    }

    @Override
    public ReEncumbranceHolder withPoLine(PoLine poLine) {
        return (ReEncumbranceHolder) super.withPoLine(poLine);
    }

    @Override
    public ReEncumbranceHolder withFundDistribution(FundDistribution fundDistribution) {
        super.withFundDistribution(fundDistribution);
        return this;
    }

    @Override
    public ReEncumbranceHolder withBudget(Budget budget) {
        super.withBudget(budget);
        return this;
    }

    public LedgerFiscalYearRollover getRollover() {
        return rollover;
    }



    public ReEncumbranceHolder withRollover(LedgerFiscalYearRollover rollover) {
        this.rollover = rollover;
        return this;
    }

    @Override
    public ReEncumbranceHolder withOldEncumbrance(Transaction transaction) {
        super.withOldEncumbrance(transaction);
        return this;
    }

    @Override
    public ReEncumbranceHolder withNewEncumbrance(Transaction transaction) {
        super.withNewEncumbrance(transaction);
        return this;
    }

    @Override
    public ReEncumbranceHolder withPoLineToFyConversion(CurrencyConversion poLineToFyConversion) {
        super.withPoLineToFyConversion(poLineToFyConversion);
        return this;
    }

    @Override
    public ReEncumbranceHolder withLedgerId(String ledgerId) {
        super.withLedgerId(ledgerId);
        return this;
    }

    @Override
    public ReEncumbranceHolder withRestrictEncumbrances(boolean restrictEncumbrance) {
        super.withRestrictEncumbrances(restrictEncumbrance);
        return this;
    }

    @Override
    public ReEncumbranceHolder withCurrentFiscalYearId(String fiscalYearId) {
        super.withCurrentFiscalYearId(fiscalYearId);
        return this;
    }

    @Override
    public ReEncumbranceHolder withCurrency(String currency) {
        super.withCurrency(currency);
        return this;
    }

    public EncumbranceRollover getEncumbranceRollover() {
        return encumbranceRollover;
    }

    public ReEncumbranceHolder withEncumbranceRollover(EncumbranceRollover encumbranceRollover) {
        this.encumbranceRollover = encumbranceRollover;
        return this;
    }

    public ReEncumbranceHolder withFyToPoLineConversion(CurrencyConversion fyToPoLineConversion) {
        this.fyToPoLineConversion = fyToPoLineConversion;
        return this;
    }

    public CurrencyConversion getFyToPoLineConversion() {
        return fyToPoLineConversion;
    }

    public Transaction getPreviousFyEncumbrance() {
        return previousFyEncumbrance;
    }

    public ReEncumbranceHolder withPreviousFyEncumbrance(Transaction previousFyEncumbrance) {
        this.previousFyEncumbrance = previousFyEncumbrance;
        return this;
    }
}
