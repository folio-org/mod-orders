package org.folio.service.finance;

import org.junit.jupiter.api.BeforeEach;
import org.junit.jupiter.api.Test;
import org.mockito.InjectMocks;
import org.mockito.MockitoAnnotations;

import javax.money.CurrencyUnit;
import javax.money.Monetary;
import javax.money.MonetaryAmount;
import java.util.UUID;

import static org.hamcrest.MatcherAssert.assertThat;
import static org.hamcrest.Matchers.is;

import org.folio.rest.acq.model.finance.Budget;

public class BudgetRestrictionServiceTest {

  @InjectMocks
  private BudgetRestrictionService restrictionService;

  private final String fundId = UUID.randomUUID().toString();
  private final String fiscalYearId = UUID.randomUUID().toString();
  private Budget budget;
  private final CurrencyUnit currency = Monetary.getCurrency("USD");

  @BeforeEach
  public void initMocks(){
    MockitoAnnotations.openMocks(this);

    budget = new Budget()
      .withFiscalYearId(fiscalYearId)
      .withAwaitingPayment(0d)
      .withAllocated(100d)
      .withAvailable(100d)
      .withEncumbered(0d)
      .withUnavailable(0d)
      .withFundId(fundId);
  }

  @Test
  void testGetBudgetRemainingAmountForEncumbrance() {
    budget.withNetTransfers(20d)
      .withAllowableEncumbrance(110d)
      .withEncumbered(10d)
      .withAwaitingPayment(11d)
      .withExpenditures(90d)
      .withAvailable(21d) // should not be used
      .withUnavailable(22d); // should not be used
    MonetaryAmount amount = restrictionService.getBudgetRemainingAmountForEncumbrance(budget, currency);
    assertThat(amount.getNumber().doubleValue(), is(21d));
  }

}
