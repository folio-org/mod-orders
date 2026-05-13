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
import org.folio.rest.acq.model.finance.Budget;
import org.folio.rest.acq.model.finance.Fund;
import org.folio.rest.acq.model.finance.Ledger;
import org.folio.rest.acq.model.finance.Transaction;
import org.folio.rest.core.exceptions.HttpException;
import org.folio.rest.jaxrs.model.Error;
import org.folio.rest.jaxrs.model.FundDistribution;
import org.junit.jupiter.api.BeforeEach;
import org.junit.jupiter.api.Test;
import org.mockito.InjectMocks;
import org.mockito.MockitoAnnotations;
import static org.folio.orders.utils.ResourcePathResolver.FUNDS;
import static org.junit.jupiter.api.Assertions.assertTrue;

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
    String fundCode = "TEST-FUND";

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
    FundDistribution fundDistribution = new FundDistribution()
        .withFundId(fundId)
        .withCode(fundCode);
    EncumbranceRelationsHolder holder = new EncumbranceRelationsHolder().withOldEncumbrance(existingTransaction)
        .withNewEncumbrance(newTransaction)
        .withBudget(budget)
        .withRestrictEncumbrances(true)
        .withCurrentFiscalYearId(fiscalYearId)
        .withCurrency("USD")
        .withFundDistribution(fundDistribution);
    holders.add(holder);

    HttpException httpException = assertThrows(HttpException.class, () -> restrictionService.checkEncumbranceRestrictions(holders));

    assertEquals(422, httpException.getCode());
    Error error = httpException.getError();
    assertEquals(FUND_CANNOT_BE_PAID.getCode(), error.getCode());
    assertEquals(Collections.singletonList(fundCode).toString(), error.getParameters().get(0).getValue());

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
    String fundCode = "TEST-FUND";


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

    FundDistribution fundDistribution = new FundDistribution()
        .withFundId(fundId)
        .withCode(fundCode);
    EncumbranceRelationsHolder holder = new EncumbranceRelationsHolder()
        .withOldEncumbrance(null)
        .withNewEncumbrance(newTransaction)
        .withBudget(budget)
        .withRestrictEncumbrances(true)
        .withCurrentFiscalYearId(fiscalYearId)
        .withCurrency("USD")
        .withFundDistribution(fundDistribution);
    holders.add(holder);

    HttpException httpException = assertThrows(HttpException.class, () -> restrictionService.checkEncumbranceRestrictions(holders));

    assertEquals(422, httpException.getCode());
    Error error = httpException.getError();
    assertEquals(FUND_CANNOT_BE_PAID.getCode(), error.getCode());
    assertEquals(Collections.singletonList(fundCode).toString(), error.getParameters().get(0).getValue());
  }

    @Test
  void testCheckEncumbranceRestrictionsShouldAggregateMultipleHoldersForSameBudget() {
    // TestMate-725ce559a4ccd7bbfeaf07ed6600b3f4
    // Given
    String fiscalYearId = UUID.randomUUID().toString();
    String fundId = UUID.randomUUID().toString();
    String budgetId = UUID.randomUUID().toString();
    String fundCode = "AGGREGATE-FUND";
    String currency = "USD";
    Budget budget = new Budget()
      .withId(budgetId)
      .withFiscalYearId(fiscalYearId)
      .withFundId(fundId)
      .withTotalFunding(100.0)
      .withAllowableEncumbrance(100.0)
      .withUnavailable(85.0);
    FundDistribution fundDistribution = new FundDistribution()
      .withFundId(fundId)
      .withCode(fundCode);
    Transaction oldTransaction1 = new Transaction()
      .withAmount(20.0)
      .withCurrency(currency);
    Transaction newTransaction1 = new Transaction()
      .withAmount(30.0)
      .withCurrency(currency);
    EncumbranceRelationsHolder holder1 = new EncumbranceRelationsHolder()
      .withOldEncumbrance(oldTransaction1)
      .withNewEncumbrance(newTransaction1)
      .withBudget(budget)
      .withRestrictEncumbrances(true)
      .withCurrentFiscalYearId(fiscalYearId)
      .withCurrency(currency)
      .withFundDistribution(fundDistribution);
    Transaction oldTransaction2 = new Transaction()
      .withAmount(15.0)
      .withCurrency(currency);
    Transaction newTransaction2 = new Transaction()
      .withAmount(25.0)
      .withCurrency(currency);
    EncumbranceRelationsHolder holder2 = new EncumbranceRelationsHolder()
      .withOldEncumbrance(oldTransaction2)
      .withNewEncumbrance(newTransaction2)
      .withBudget(budget)
      .withRestrictEncumbrances(true)
      .withCurrentFiscalYearId(fiscalYearId)
      .withCurrency(currency)
      .withFundDistribution(fundDistribution);
    List<EncumbranceRelationsHolder> holders = List.of(holder1, holder2);
    // When
    HttpException httpException = assertThrows(HttpException.class, () -> restrictionService.checkEncumbranceRestrictions(holders));
    // Then
    assertEquals(422, httpException.getCode());
    Error error = httpException.getError();
    assertEquals(FUND_CANNOT_BE_PAID.getCode(), error.getCode());
    assertEquals(Collections.singletonList(fundCode).toString(), error.getParameters().get(0).getValue());
    assertEquals(FUNDS, error.getParameters().get(0).getKey());
  }

    @Test
  void testCheckEncumbranceRestrictionsShouldHandleNegativeNetDifference() {
    // TestMate-3546561e5f4f97a05b9beebbdfd2f0a7
    // Given
    String fiscalYearId = UUID.randomUUID().toString();
    String fundId = UUID.randomUUID().toString();
    String budgetId = UUID.randomUUID().toString();
    String fundCode = "TEST-FUND";
    String currency = "USD";
    Budget budget = new Budget()
      .withId(budgetId)
      .withFiscalYearId(fiscalYearId)
      .withFundId(fundId)
      .withTotalFunding(100.0)
      .withAllowableEncumbrance(100.0)
      .withUnavailable(110.0);
    Transaction oldTransaction = new Transaction()
      .withAmount(50.0)
      .withCurrency(currency);
    Transaction newTransaction = new Transaction()
      .withAmount(40.0)
      .withCurrency(currency);
    FundDistribution fundDistribution = new FundDistribution()
      .withFundId(fundId)
      .withCode(fundCode);
    EncumbranceRelationsHolder holder = new EncumbranceRelationsHolder()
      .withOldEncumbrance(oldTransaction)
      .withNewEncumbrance(newTransaction)
      .withBudget(budget)
      .withRestrictEncumbrances(true)
      .withCurrentFiscalYearId(fiscalYearId)
      .withCurrency(currency)
      .withFundDistribution(fundDistribution);
    List<EncumbranceRelationsHolder> holders = List.of(holder);
    // When
    // Act is calling the method under test, net difference is -10.0 (40.0 - 50.0)
    // Then
    assertDoesNotThrow(() -> restrictionService.checkEncumbranceRestrictions(holders));
  }

    @Test
  void testCheckEncumbranceRestrictionsShouldThrowExceptionWithMultipleFailedFunds() {
    // TestMate-b898e68d498da14bb9265ccf5822bea6
    // Given
    String currency = "USD";
    String fiscalYearId = UUID.randomUUID().toString();
    String fundIdA = UUID.randomUUID().toString();
    String fundCodeA = "CODE-A";
    Budget budgetA = new Budget()
      .withId(UUID.randomUUID().toString())
      .withFundId(fundIdA)
      .withFiscalYearId(fiscalYearId)
      .withTotalFunding(100.0)
      .withAllowableEncumbrance(100.0)
      .withUnavailable(95.0);
    Transaction newTransactionA = new Transaction()
      .withAmount(10.0)
      .withCurrency(currency);
    FundDistribution fundDistributionA = new FundDistribution()
      .withFundId(fundIdA)
      .withCode(fundCodeA);
    EncumbranceRelationsHolder holderA = new EncumbranceRelationsHolder()
      .withOldEncumbrance(null)
      .withNewEncumbrance(newTransactionA)
      .withBudget(budgetA)
      .withRestrictEncumbrances(true)
      .withCurrentFiscalYearId(fiscalYearId)
      .withCurrency(currency)
      .withFundDistribution(fundDistributionA);
    String fundIdB = UUID.randomUUID().toString();
    String fundCodeB = "CODE-B";
    Budget budgetB = new Budget()
      .withId(UUID.randomUUID().toString())
      .withFundId(fundIdB)
      .withFiscalYearId(fiscalYearId)
      .withTotalFunding(200.0)
      .withAllowableEncumbrance(90.0)
      .withUnavailable(175.0);
    Transaction newTransactionB = new Transaction()
      .withAmount(15.0)
      .withCurrency(currency);
    FundDistribution fundDistributionB = new FundDistribution()
      .withFundId(fundIdB)
      .withCode(fundCodeB);
    EncumbranceRelationsHolder holderB = new EncumbranceRelationsHolder()
      .withOldEncumbrance(null)
      .withNewEncumbrance(newTransactionB)
      .withBudget(budgetB)
      .withRestrictEncumbrances(true)
      .withCurrentFiscalYearId(fiscalYearId)
      .withCurrency(currency)
      .withFundDistribution(fundDistributionB);
    List<EncumbranceRelationsHolder> holders = List.of(holderA, holderB);
    // When
    HttpException httpException = assertThrows(HttpException.class, () -> restrictionService.checkEncumbranceRestrictions(holders));
    // Then
    assertEquals(422, httpException.getCode());
    Error error = httpException.getError();
    assertEquals(FUND_CANNOT_BE_PAID.getCode(), error.getCode());
    assertEquals(1, error.getParameters().size());
    assertEquals(FUNDS, error.getParameters().get(0).getKey());
    String errorValue = error.getParameters().get(0).getValue();
    assertTrue(errorValue.contains(fundCodeA));
    assertTrue(errorValue.contains(fundCodeB));
  }

    @Test
  void testCheckEncumbranceRestrictionsShouldHandleEmptyDataHolders() {
    // TestMate-225bdf74cf02a90adfc62dbe73bbab17
    // Given
    List<EncumbranceRelationsHolder> holders = Collections.emptyList();
    // When
    // Then
    assertDoesNotThrow(() -> restrictionService.checkEncumbranceRestrictions(holders));
  }

    @Test
  void testCheckEncumbranceRestrictionsShouldIgnoreHoldersWithNullFundDistributionFields() {
    // TestMate-1189f6cb51ec35f27c07d0d82f869e8f
    // Given
    String fiscalYearId = UUID.randomUUID().toString();
    String currency = "USD";
    String budgetId = UUID.randomUUID().toString();
    String fundIdA = UUID.randomUUID().toString();
    String fundCodeA = "CODE-A";
    // Budget with 0 remaining capacity: (100 * 1.0) - 100 = 0
    // The budget must have a fundId because the service uses budget.getFundId() to report failures
    Budget budget = new Budget()
      .withId(budgetId)
      .withFiscalYearId(fiscalYearId)
      .withFundId(fundIdA)
      .withTotalFunding(100.0)
      .withAllowableEncumbrance(100.0)
      .withUnavailable(100.0);
    // Holder A: Valid failure trigger
    FundDistribution fdA = new FundDistribution()
      .withFundId(fundIdA)
      .withCode(fundCodeA);
    Transaction newTransactionA = new Transaction()
      .withAmount(10.0)
      .withCurrency(currency);
    EncumbranceRelationsHolder holderA = new EncumbranceRelationsHolder()
      .withNewEncumbrance(newTransactionA)
      .withBudget(budget)
      .withRestrictEncumbrances(true)
      .withCurrentFiscalYearId(fiscalYearId)
      .withCurrency(currency)
      .withFundDistribution(fdA);
    // Holder B: Target for filtering (null code)
    // This holder's fundId is different from the budget's fundId
    String fundIdB = UUID.randomUUID().toString();
    FundDistribution fdB = new FundDistribution()
      .withFundId(fundIdB)
      .withCode(null);
    Transaction newTransactionB = new Transaction()
      .withAmount(5.0)
      .withCurrency(currency);
    EncumbranceRelationsHolder holderB = new EncumbranceRelationsHolder()
      .withNewEncumbrance(newTransactionB)
      .withBudget(budget)
      .withRestrictEncumbrances(true)
      .withCurrentFiscalYearId(fiscalYearId)
      .withCurrency(currency)
      .withFundDistribution(fdB);
    List<EncumbranceRelationsHolder> holders = List.of(holderA, holderB);
    // When
    HttpException httpException = assertThrows(HttpException.class, () -> restrictionService.checkEncumbranceRestrictions(holders));
    // Then
    assertEquals(422, httpException.getCode());
    Error error = httpException.getError();
    assertEquals(FUND_CANNOT_BE_PAID.getCode(), error.getCode());
    assertEquals(1, error.getParameters().size());
    assertEquals(FUNDS, error.getParameters().get(0).getKey());
    String errorValue = error.getParameters().get(0).getValue();
    // The service reports the fund code associated with the budget's fundId (fundIdA -> CODE-A)
    assertTrue(errorValue.contains(fundCodeA));
    // To verify that the logic didn't crash and correctly filtered out the null code:
    // We create a second budget that also fails, but whose fundId corresponds to holderB (which has a null code).
    // Because fd.getCode() != null filters out holderB, fundHoldersMap.get(fundIdB) will return null.
    Budget budgetB = new Budget()
      .withId(UUID.randomUUID().toString())
      .withFiscalYearId(fiscalYearId)
      .withFundId(fundIdB)
      .withTotalFunding(100.0)
      .withAllowableEncumbrance(100.0)
      .withUnavailable(100.0);
    holderB.withBudget(budgetB);
    HttpException httpException2 = assertThrows(HttpException.class, () -> restrictionService.checkEncumbranceRestrictions(List.of(holderA, holderB)));
    String errorValue2 = httpException2.getError().getParameters().get(0).getValue();
    assertTrue(errorValue2.contains(fundCodeA));
    // fundHoldersMap.get(fundIdB) returns null, which becomes "null" in the stringified list representation
    assertTrue(errorValue2.contains("null"));
  }



}
