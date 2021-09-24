package org.folio.service.finance.budget;

import static org.folio.rest.core.exceptions.ErrorCodes.FUND_CANNOT_BE_PAID;
import static org.junit.jupiter.api.Assertions.assertDoesNotThrow;
import static org.junit.jupiter.api.Assertions.assertEquals;
import static org.junit.jupiter.api.Assertions.assertThrows;

import java.util.ArrayList;
import java.util.Collections;
import java.util.List;
import java.util.UUID;

import org.folio.models.EncumbranceRelationsHolder;
import org.folio.rest.core.exceptions.HttpException;
import org.folio.rest.acq.model.finance.Budget;
import org.folio.rest.acq.model.finance.Fund;
import org.folio.rest.acq.model.finance.Ledger;
import org.folio.rest.acq.model.finance.Transaction;
import org.folio.rest.jaxrs.model.Error;
import org.junit.jupiter.api.BeforeEach;
import org.junit.jupiter.api.Test;
import org.mockito.InjectMocks;
import org.mockito.MockitoAnnotations;

public class BudgetRestrictionServiceTest {

  @InjectMocks
  private BudgetRestrictionService restrictionService;

  @BeforeEach
  public void initMocks(){
    MockitoAnnotations.openMocks(this);
  }


  @Test
  void checkEnoughMoneyInBudgetShouldThrowFundCannotBePaidIfTransactionsAmountDifferenceGreaterThanBudgetRemainingAmount() {

    String fiscalYearId = UUID.randomUUID().toString();
    String fundId = UUID.randomUUID().toString();
    String budgetId = UUID.randomUUID().toString();

    Transaction existingTransaction = new Transaction()
        .withTransactionType(Transaction.TransactionType.ENCUMBRANCE)
        .withAmount(50d)
        .withFiscalYearId(fiscalYearId)
        .withFromFundId(fundId)
        .withCurrency("USD");

    Transaction newTransaction = new Transaction()
        .withTransactionType(Transaction.TransactionType.ENCUMBRANCE)
        .withAmount(60d)
        .withFiscalYearId(fiscalYearId)
        .withFromFundId(fundId)
        .withCurrency("USD");

    Budget budget = new Budget()
        .withId(budgetId)
        .withFiscalYearId(fiscalYearId)
        .withFundId(fundId)
        .withAllocated(59d)
        .withTotalFunding(59d)
        .withAvailable(9d)
        .withUnavailable(50d)
        .withAwaitingPayment(50D)
        .withAllowableEncumbrance(100d);

    List<EncumbranceRelationsHolder> holders = new ArrayList<>();
    EncumbranceRelationsHolder holder = new EncumbranceRelationsHolder().withOldEncumbrance(existingTransaction)
        .withNewEncumbrance(newTransaction)
        .withBudget(budget)
        .withRestrictEncumbrances(true)
        .withCurrentFiscalYearId(fiscalYearId)
        .withCurrency("USD");
    holders.add(holder);

    HttpException httpException = assertThrows(HttpException.class, () -> restrictionService.checkEncumbranceRestrictions(holders));

    assertEquals(422, httpException.getCode());
    Error error = httpException.getError();
    assertEquals(FUND_CANNOT_BE_PAID.getCode(), error.getCode());
    assertEquals(Collections.singletonList(budgetId).toString(), error.getParameters().get(0).getValue());

  }

  @Test
  void checkEnoughMoneyInBudgetShouldPassIfTransactionsAmountDifferenceLessThanBudgetRemainingAmount() {

    String fiscalYearId = UUID.randomUUID().toString();
    String fundId = UUID.randomUUID().toString();
    String ledgerId = UUID.randomUUID().toString();
    String budgetId = UUID.randomUUID().toString();

    Transaction existingTransaction = new Transaction()
        .withTransactionType(Transaction.TransactionType.ENCUMBRANCE)
        .withAmount(50d)
        .withFiscalYearId(fiscalYearId)
        .withFromFundId(fundId)
        .withCurrency("USD");

    Transaction newTransaction = new Transaction()
        .withTransactionType(Transaction.TransactionType.ENCUMBRANCE)
        .withAmount(60d)
        .withFiscalYearId(fiscalYearId)
        .withFromFundId(fundId)
        .withCurrency("USD");

    Budget budget = new Budget()
        .withId(budgetId)
        .withFiscalYearId(fiscalYearId)
        .withFundId(fundId)
        .withAllocated(59d)
        .withAvailable(9d)
        .withTotalFunding(59d)
        .withUnavailable(50d)
        .withAwaitingPayment(50D)
        .withAllowableEncumbrance(150d);

    Fund fund = new Fund()
        .withId(fundId)
        .withLedgerId(ledgerId);

    Ledger ledger = new Ledger()
        .withId(ledgerId)
        .withRestrictEncumbrance(true);

    List<EncumbranceRelationsHolder> holders = new ArrayList<>();
    EncumbranceRelationsHolder holder = new EncumbranceRelationsHolder().withOldEncumbrance(existingTransaction)
        .withNewEncumbrance(newTransaction)
        .withBudget(budget)
        .withLedgerId(fund.getLedgerId())
        .withRestrictEncumbrances(ledger.getRestrictExpenditures())
        .withCurrentFiscalYearId(fiscalYearId)
        .withCurrency("USD");
    holders.add(holder);

    assertDoesNotThrow(() -> restrictionService.checkEncumbranceRestrictions(holders));

  }

  @Test
  void checkEnoughMoneyInBudgetShouldPassIfTransactionsAmountDifferenceGreaterThanBudgetRemainingAmountAndBudgetAllowableExpenditureIsNull() {

    String fiscalYearId = UUID.randomUUID().toString();
    String fundId = UUID.randomUUID().toString();
    String budgetId = UUID.randomUUID().toString();

    Transaction existingTransaction = new Transaction()
        .withTransactionType(Transaction.TransactionType.ENCUMBRANCE)
        .withAmount(50d)
        .withFiscalYearId(fiscalYearId)
        .withFromFundId(fundId)
        .withCurrency("USD");

    Transaction newTransaction = new Transaction()
        .withTransactionType(Transaction.TransactionType.ENCUMBRANCE)
        .withAmount(60d)
        .withFiscalYearId(fiscalYearId)
        .withFromFundId(fundId)
        .withCurrency("USD");

    Budget budget = new Budget()
        .withId(budgetId)
        .withFiscalYearId(fiscalYearId)
        .withFundId(fundId)
        .withAllocated(59d)
        .withTotalFunding(59d)
        .withAvailable(0d)
        .withUnavailable(50d)
        .withAwaitingPayment(50D)
        .withAllowableEncumbrance(null);


    List<EncumbranceRelationsHolder> holders = new ArrayList<>();

    EncumbranceRelationsHolder holder = new EncumbranceRelationsHolder()
        .withOldEncumbrance(existingTransaction)
        .withNewEncumbrance(newTransaction)
        .withBudget(budget)
        .withRestrictEncumbrances(true)
        .withCurrentFiscalYearId(fiscalYearId)
        .withCurrency("USD");
    holders.add(holder);

    assertDoesNotThrow(() -> restrictionService.checkEncumbranceRestrictions(holders));

  }

  @Test
  void checkEnoughMoneyInBudgetShouldPassIfTransactionsAmountDifferenceGreaterThanBudgetRemainingAmountAndRestrictEncumbranceIsFalse() {

    String fiscalYearId = UUID.randomUUID().toString();
    String fundId = UUID.randomUUID().toString();
    String budgetId = UUID.randomUUID().toString();

    Transaction existingTransaction = new Transaction()
        .withTransactionType(Transaction.TransactionType.ENCUMBRANCE)
        .withAmount(50d)
        .withFiscalYearId(fiscalYearId)
        .withFromFundId(fundId)
        .withCurrency("USD");

    Transaction newTransaction = new Transaction()
        .withTransactionType(Transaction.TransactionType.ENCUMBRANCE)
        .withAmount(60d)
        .withFiscalYearId(fiscalYearId)
        .withFromFundId(fundId)
        .withCurrency("USD");

    Budget budget = new Budget()
        .withId(budgetId)
        .withFiscalYearId(fiscalYearId)
        .withFundId(fundId)
        .withAllocated(59d)
        .withTotalFunding(59d)
        .withAvailable(0d)
        .withUnavailable(50d)
        .withAwaitingPayment(50d)
        .withAllowableEncumbrance(100d);

    List<EncumbranceRelationsHolder> holders = new ArrayList<>();

    EncumbranceRelationsHolder holder = new EncumbranceRelationsHolder()
        .withOldEncumbrance(existingTransaction)
        .withNewEncumbrance(newTransaction)
        .withBudget(budget)
        .withRestrictEncumbrances(false)
        .withCurrentFiscalYearId(fiscalYearId)
        .withCurrency("USD");
    holders.add(holder);

    assertDoesNotThrow(() -> restrictionService.checkEncumbranceRestrictions(holders));
  }

  @Test
  void checkEnoughMoneyInBudgetShouldPassIfTransactionsAmountGreaterThanBudgetRemainingAmountAndBudgetRestricted() {

    String fiscalYearId = UUID.randomUUID().toString();
    String fundId = UUID.randomUUID().toString();
    String budgetId = UUID.randomUUID().toString();


    Transaction newTransaction = new Transaction()
        .withTransactionType(Transaction.TransactionType.ENCUMBRANCE)
        .withAmount(10d)
        .withFiscalYearId(fiscalYearId)
        .withFromFundId(fundId)
        .withCurrency("USD");

    Budget budget = new Budget()
        .withId(budgetId)
        .withFiscalYearId(fiscalYearId)
        .withFundId(fundId)
        .withAllocated(59d)
        .withTotalFunding(59d)
        .withAvailable(0d)
        .withUnavailable(50d)
        .withAwaitingPayment(50d)
        .withAllowableEncumbrance(100d);

    List<EncumbranceRelationsHolder> holders = new ArrayList<>();

    EncumbranceRelationsHolder holder = new EncumbranceRelationsHolder()
        .withOldEncumbrance(null)
        .withNewEncumbrance(newTransaction)
        .withBudget(budget)
        .withRestrictEncumbrances(true)
        .withCurrentFiscalYearId(fiscalYearId)
        .withCurrency("USD");
    holders.add(holder);

    HttpException httpException = assertThrows(HttpException.class, () -> restrictionService.checkEncumbranceRestrictions(holders));

    assertEquals(422, httpException.getCode());
    Error error = httpException.getError();
    assertEquals(FUND_CANNOT_BE_PAID.getCode(), error.getCode());
    assertEquals(Collections.singletonList(budgetId).toString(), error.getParameters().get(0).getValue());
  }



}
