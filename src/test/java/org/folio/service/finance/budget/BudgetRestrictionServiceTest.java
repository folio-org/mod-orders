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
import org.folio.rest.jaxrs.model.Parameter;

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
  void testCheckEncumbranceRestrictionsShouldAggregateMultipleHoldersPerBudget() {
    // TestMate-1f9626f1628e19fbfeb5395f8c4dddec
    // Given
    String fiscalYearId = "5d2aa19b-f13e-4d40-83f5-626a57c5040e";
    String fundId = "1b6d3338-186e-4e35-9e75-1b886b0da53e";
    String budgetId = "6a2f876b-213e-469b-9023-235307bc9780";
    String fundCode = "TEST-FUND";
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
    Transaction oldTransaction1 = new Transaction()
      .withAmount(5.0)
      .withCurrency(currency);
    Transaction newTransaction1 = new Transaction()
      .withAmount(10.0)
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
      .withAmount(10.0)
      .withCurrency(currency);
    Transaction newTransaction2 = new Transaction()
      .withAmount(20.0)
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
    assertEquals(FUNDS, error.getParameters().get(0).getKey());
    assertEquals(List.of(fundCode).toString(), error.getParameters().get(0).getValue());
  }

    @Test
  void testCheckEncumbranceRestrictionsShouldHandleMultipleFailedFundsInOneError() {
    // TestMate-efc1b4fec1598fb614f98407ab39c378
    // Given
    String fiscalYearId = UUID.randomUUID().toString();
    String currency = "USD";
    String fundIdA = "fund-id-a";
    String fundCodeA = "FUND-A";
    Budget budgetA = new Budget()
      .withId(UUID.randomUUID().toString())
      .withFundId(fundIdA)
      .withFiscalYearId(fiscalYearId)
      .withTotalFunding(100.0)
      .withUnavailable(100.0)
      .withAllowableEncumbrance(100.0);
    FundDistribution fdA = new FundDistribution()
      .withFundId(fundIdA)
      .withCode(fundCodeA);
    Transaction newTransactionA = new Transaction()
      .withAmount(10.0)
      .withCurrency(currency);
    EncumbranceRelationsHolder holderA = new EncumbranceRelationsHolder()
      .withNewEncumbrance(newTransactionA)
      .withOldEncumbrance(null)
      .withBudget(budgetA)
      .withFundDistribution(fdA)
      .withRestrictEncumbrances(true)
      .withCurrency(currency);
    String fundIdB = "fund-id-b";
    String fundCodeB = "FUND-B";
    Budget budgetB = new Budget()
      .withId(UUID.randomUUID().toString())
      .withFundId(fundIdB)
      .withFiscalYearId(fiscalYearId)
      .withTotalFunding(200.0)
      .withUnavailable(200.0)
      .withAllowableEncumbrance(100.0);
    FundDistribution fdB = new FundDistribution()
      .withFundId(fundIdB)
      .withCode(fundCodeB);
    Transaction newTransactionB = new Transaction()
      .withAmount(20.0)
      .withCurrency(currency);
    EncumbranceRelationsHolder holderB = new EncumbranceRelationsHolder()
      .withNewEncumbrance(newTransactionB)
      .withOldEncumbrance(null)
      .withBudget(budgetB)
      .withFundDistribution(fdB)
      .withRestrictEncumbrances(true)
      .withCurrency(currency);
    List<EncumbranceRelationsHolder> holders = List.of(holderA, holderB);
    // When
    HttpException httpException = assertThrows(HttpException.class, () -> restrictionService.checkEncumbranceRestrictions(holders));
    // Then
    assertEquals(422, httpException.getCode());
    Error error = httpException.getError();
    assertEquals(FUND_CANNOT_BE_PAID.getCode(), error.getCode());
    assertEquals(FUNDS, error.getParameters().get(0).getKey());
    // The order of fund codes in the error message is non-deterministic because the implementation
    // uses a HashMap-based grouping. We parse the string back to a list and sort to verify contents.
    String actualValue = error.getParameters().get(0).getValue();
    String content = actualValue.substring(1, actualValue.length() - 1);
    List<String> actualCodes = new ArrayList<>(List.of(content.split(", ")));
    Collections.sort(actualCodes);
    List<String> expectedCodes = new ArrayList<>(List.of(fundCodeA, fundCodeB));
    Collections.sort(expectedCodes);
    assertEquals(expectedCodes, actualCodes);
  }

    @Test
  void testCheckEncumbranceRestrictionsShouldFilterOutHoldersWithMissingFundDistributionDetails() {
    // TestMate-fd4660f18a960e605f48b259ab865fa2
    // Given
    String fiscalYearId = UUID.fromString("5d2aa19b-f13e-4d40-83f5-626a57c5040e").toString();
    String fundId = UUID.fromString("1b6d3338-186e-4e35-9e75-1b886b0da53e").toString();
    String budgetId = UUID.fromString("6a2f876b-213e-469b-9023-235307bc9780").toString();
    String currency = "USD";
    Budget budget = new Budget()
      .withId(budgetId)
      .withFiscalYearId(fiscalYearId)
      .withFundId(fundId)
      .withTotalFunding(100.0)
      .withUnavailable(100.0)
      .withAllowableEncumbrance(100.0);
    FundDistribution fundDistribution = new FundDistribution()
      .withFundId(fundId)
      .withCode(null);
    Transaction newTransaction = new Transaction()
      .withAmount(10.0)
      .withCurrency(currency)
      .withFromFundId(fundId)
      .withFiscalYearId(fiscalYearId);
    EncumbranceRelationsHolder holder = new EncumbranceRelationsHolder()
      .withNewEncumbrance(newTransaction)
      .withOldEncumbrance(null)
      .withBudget(budget)
      .withRestrictEncumbrances(true)
      .withFundDistribution(fundDistribution)
      .withCurrency(currency)
      .withCurrentFiscalYearId(fiscalYearId);
    List<EncumbranceRelationsHolder> holders = List.of(holder);
    // When
    HttpException httpException = assertThrows(HttpException.class, () -> restrictionService.checkEncumbranceRestrictions(holders));
    // Then
    assertEquals(422, httpException.getCode());
    Error error = httpException.getError();
    assertEquals(FUND_CANNOT_BE_PAID.getCode(), error.getCode());
    Parameter parameter = error.getParameters().stream()
      .filter(p -> FUNDS.equals(p.getKey()))
      .findFirst()
      .orElseThrow();
    assertEquals("[null]", parameter.getValue());
  }



}
