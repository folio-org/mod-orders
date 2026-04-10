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
  void checkEncumbranceRestrictionsShouldSumMultipleHoldersForSameBudget() {
    // TestMate-35e3f012dbf2ff98bed9c7d4cc467b36
    // Given
    String fiscalYearId = UUID.fromString("11111111-1111-1111-1111-111111111111").toString();
    String fundId = UUID.fromString("22222222-2222-2222-2222-222222222222").toString();
    String budgetId = UUID.fromString("33333333-3333-3333-3333-333333333333").toString();
    String fundCode = "FUND-A";
    String currency = "USD";
    Budget budget = new Budget()
        .withId(budgetId)
        .withFiscalYearId(fiscalYearId)
        .withFundId(fundId)
        .withTotalFunding(100.0)
        .withUnavailable(90.0)
        .withAllowableEncumbrance(100.0);
    FundDistribution fundDistribution = new FundDistribution()
        .withFundId(fundId)
        .withCode(fundCode);
    Transaction oldEncumbrance1 = new Transaction()
        .withAmount(10.0)
        .withCurrency(currency);
    Transaction newEncumbrance1 = new Transaction()
        .withAmount(15.0)
        .withCurrency(currency);
    EncumbranceRelationsHolder holder1 = new EncumbranceRelationsHolder()
        .withOldEncumbrance(oldEncumbrance1)
        .withNewEncumbrance(newEncumbrance1)
        .withBudget(budget)
        .withRestrictEncumbrances(true)
        .withCurrentFiscalYearId(fiscalYearId)
        .withCurrency(currency)
        .withFundDistribution(fundDistribution);
    Transaction oldEncumbrance2 = new Transaction()
        .withAmount(20.0)
        .withCurrency(currency);
    Transaction newEncumbrance2 = new Transaction()
        .withAmount(26.0)
        .withCurrency(currency);
    EncumbranceRelationsHolder holder2 = new EncumbranceRelationsHolder()
        .withOldEncumbrance(oldEncumbrance2)
        .withNewEncumbrance(newEncumbrance2)
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
    assertEquals(FUNDS, error.getParameters().get(0).getKey());
    assertEquals(List.of(fundCode).toString(), error.getParameters().get(0).getValue());
  }

    @Test
  void checkEncumbranceRestrictionsShouldPassWhenIncreaseExactlyEqualsRemaining() {
    // TestMate-d88df637e897346d3f7a6d7a6a8926df
    // Given
    String fiscalYearId = UUID.fromString("11111111-1111-1111-1111-111111111111").toString();
    String fundId = UUID.fromString("22222222-2222-2222-2222-222222222222").toString();
    String budgetId = UUID.fromString("33333333-3333-3333-3333-333333333333").toString();
    String fundCode = "TEST-FUND";
    String currency = "USD";
    Budget budget = new Budget()
        .withId(budgetId)
        .withFiscalYearId(fiscalYearId)
        .withFundId(fundId)
        .withTotalFunding(100.0)
        .withUnavailable(50.0)
        .withAllowableEncumbrance(100.0);
    Transaction newTransaction = new Transaction()
        .withTransactionType(Transaction.TransactionType.ENCUMBRANCE)
        .withAmount(50.0)
        .withFiscalYearId(fiscalYearId)
        .withFromFundId(fundId)
        .withCurrency(currency);
    FundDistribution fundDistribution = new FundDistribution()
        .withFundId(fundId)
        .withCode(fundCode);
    EncumbranceRelationsHolder holder = new EncumbranceRelationsHolder()
        .withOldEncumbrance(null)
        .withNewEncumbrance(newTransaction)
        .withBudget(budget)
        .withRestrictEncumbrances(true)
        .withCurrentFiscalYearId(fiscalYearId)
        .withCurrency(currency)
        .withFundDistribution(fundDistribution);
    List<EncumbranceRelationsHolder> holders = List.of(holder);
    // When & Then
    assertDoesNotThrow(() -> restrictionService.checkEncumbranceRestrictions(holders));
  }

    @Test
  void checkEncumbranceRestrictionsShouldHandleNegativeDifference() {
    // TestMate-6123ab1e9ebf160a488ee5690b69ab82
    // Given
    String fiscalYearId = UUID.fromString("11111111-1111-1111-1111-111111111111").toString();
    String fundId = UUID.fromString("22222222-2222-2222-2222-222222222222").toString();
    String budgetId = UUID.fromString("33333333-3333-3333-3333-333333333333").toString();
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
    // When & Then
    assertDoesNotThrow(() -> restrictionService.checkEncumbranceRestrictions(holders));
  }

    @Test
  void checkEncumbranceRestrictionsShouldThrowExceptionWithMultipleFailedFundCodes() {
    // TestMate-856a6c51a055de7bd83767db2ca303d1
    // Given
    String fiscalYearId = UUID.fromString("11111111-1111-1111-1111-111111111111").toString();
    String currency = "USD";
    // Failure 1: Fund A / Budget A
    String fundIdA = UUID.fromString("22222222-2222-2222-2222-222222222222").toString();
    String budgetIdA = UUID.fromString("33333333-3333-3333-3333-333333333333").toString();
    String fundCodeA = "F1";
    
    Budget budgetA = new Budget()
        .withId(budgetIdA)
        .withFiscalYearId(fiscalYearId)
        .withFundId(fundIdA)
        .withTotalFunding(100.0)
        .withUnavailable(95.0)
        .withAllowableEncumbrance(100.0); // Limit: (100 * 1.0) - 95 = 5.0
    Transaction oldEncumbranceA = new Transaction().withAmount(0.0).withCurrency(currency);
    Transaction newEncumbranceA = new Transaction().withAmount(10.0).withCurrency(currency); // Change: 10.0 > 5.0
    
    EncumbranceRelationsHolder holderA = new EncumbranceRelationsHolder()
        .withOldEncumbrance(oldEncumbranceA)
        .withNewEncumbrance(newEncumbranceA)
        .withBudget(budgetA)
        .withRestrictEncumbrances(true)
        .withCurrentFiscalYearId(fiscalYearId)
        .withCurrency(currency)
        .withFundDistribution(new FundDistribution().withFundId(fundIdA).withCode(fundCodeA));
    // Failure 2: Fund B / Budget B
    String fundIdB = UUID.fromString("44444444-4444-4444-4444-444444444444").toString();
    String budgetIdB = UUID.fromString("55555555-5555-5555-5555-555555555555").toString();
    String fundCodeB = "F2";
    Budget budgetB = new Budget()
        .withId(budgetIdB)
        .withFiscalYearId(fiscalYearId)
        .withFundId(fundIdB)
        .withTotalFunding(200.0)
        .withUnavailable(95.0)
        .withAllowableEncumbrance(50.0); // Limit: (200 * 0.5) - 95 = 5.0
    Transaction oldEncumbranceB = new Transaction().withAmount(10.0).withCurrency(currency);
    Transaction newEncumbranceB = new Transaction().withAmount(20.0).withCurrency(currency); // Change: 10.0 > 5.0
    EncumbranceRelationsHolder holderB = new EncumbranceRelationsHolder()
        .withOldEncumbrance(oldEncumbranceB)
        .withNewEncumbrance(newEncumbranceB)
        .withBudget(budgetB)
        .withRestrictEncumbrances(true)
        .withCurrentFiscalYearId(fiscalYearId)
        .withCurrency(currency)
        .withFundDistribution(new FundDistribution().withFundId(fundIdB).withCode(fundCodeB));
    List<EncumbranceRelationsHolder> holders = List.of(holderA, holderB);
    // When
    HttpException httpException = assertThrows(HttpException.class, () -> restrictionService.checkEncumbranceRestrictions(holders));
    // Then
    assertEquals(422, httpException.getCode());
    Error error = httpException.getError();
    assertEquals(FUND_CANNOT_BE_PAID.getCode(), error.getCode());
    assertEquals(FUNDS, error.getParameters().get(0).getKey());
    
    String actualValue = error.getParameters().get(0).getValue();
    assertTrue(actualValue.contains(fundCodeA));
    assertTrue(actualValue.contains(fundCodeB));
  }

    @Test
  void checkEncumbranceRestrictionsShouldHandleEmptyInput() {
    // TestMate-fed27df19019f801aadd8c3467f8bac5
    // Given
    List<EncumbranceRelationsHolder> holders = Collections.emptyList();
    // When & Then
    assertDoesNotThrow(() -> restrictionService.checkEncumbranceRestrictions(holders));
  }



}
