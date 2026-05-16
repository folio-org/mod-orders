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
import static org.junit.jupiter.api.Assertions.assertTrue;
import static org.folio.orders.utils.ResourcePathResolver.FUNDS;

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
  void testCheckEncumbranceRestrictionsWhenMultipleHoldersForSameBudgetShouldAggregateDelta() {
    // TestMate-bebd8ec21437793380dfff9c1024848e
    // Given
    String fiscalYearId = "50917631-1555-46f0-9289-e9324b745426";
    String fundId = "4351333c-3363-4886-90e8-07f79435f37d";
    String budgetId = "67980556-340d-4050-985b-24905327244f";
    String fundCode = "AGGREGATE-FUND";
    String currency = "USD";
    Budget budget = new Budget()
      .withId(budgetId)
      .withFundId(fundId)
      .withFiscalYearId(fiscalYearId)
      .withTotalFunding(100.0)
      .withUnavailable(90.0)
      .withAllowableEncumbrance(100.0);
    FundDistribution fundDistribution = new FundDistribution()
      .withFundId(fundId)
      .withCode(fundCode);
    Transaction oldEncumbrance1 = new Transaction()
      .withAmount(10.0)
      .withCurrency(currency)
      .withFromFundId(fundId);
    Transaction newEncumbrance1 = new Transaction()
      .withAmount(20.0)
      .withCurrency(currency)
      .withFromFundId(fundId);
    EncumbranceRelationsHolder holder1 = new EncumbranceRelationsHolder()
      .withOldEncumbrance(oldEncumbrance1)
      .withNewEncumbrance(newEncumbrance1)
      .withBudget(budget)
      .withFundDistribution(fundDistribution)
      .withRestrictEncumbrances(true)
      .withCurrentFiscalYearId(fiscalYearId)
      .withCurrency(currency);
    Transaction oldEncumbrance2 = new Transaction()
      .withAmount(5.0)
      .withCurrency(currency)
      .withFromFundId(fundId);
    Transaction newEncumbrance2 = new Transaction()
      .withAmount(15.0)
      .withCurrency(currency)
      .withFromFundId(fundId);
    EncumbranceRelationsHolder holder2 = new EncumbranceRelationsHolder()
      .withOldEncumbrance(oldEncumbrance2)
      .withNewEncumbrance(newEncumbrance2)
      .withBudget(budget)
      .withFundDistribution(fundDistribution)
      .withRestrictEncumbrances(true)
      .withCurrentFiscalYearId(fiscalYearId)
      .withCurrency(currency);
    List<EncumbranceRelationsHolder> holders = List.of(holder1, holder2);
    // When
    HttpException httpException = assertThrows(HttpException.class, () -> restrictionService.checkEncumbranceRestrictions(holders));
    // Then
    assertEquals(422, httpException.getCode());
    Error error = httpException.getError();
    assertEquals(FUND_CANNOT_BE_PAID.getCode(), error.getCode());
    assertEquals("finance.funds", error.getParameters().get(0).getKey());
    assertEquals(Collections.singletonList(fundCode).toString(), error.getParameters().get(0).getValue());
  }

    @Test
  void testCheckEncumbranceRestrictionsWhenNewEncumbranceIsLowerThanOldShouldAlwaysPass() {
    // TestMate-1d5f2217efb726988004a01b765727eb
    // Given
    String fiscalYearId = UUID.fromString("6976696b-439d-4737-9208-a92705299092").toString();
    String fundId = UUID.fromString("1607590d-2771-419b-a63e-63796f6e5653").toString();
    String budgetId = UUID.fromString("06f89033-0669-42f8-953b-f458e0a39591").toString();
    String fundCode = "RELEASE-FUND";
    String currency = "USD";
    Budget budget = new Budget()
      .withId(budgetId)
      .withFundId(fundId)
      .withFiscalYearId(fiscalYearId)
      .withTotalFunding(100.0)
      .withUnavailable(100.0)
      .withAllowableEncumbrance(100.0);
    Transaction oldTransaction = new Transaction()
      .withTransactionType(Transaction.TransactionType.ENCUMBRANCE)
      .withAmount(50.0)
      .withCurrency(currency)
      .withFromFundId(fundId);
    Transaction newTransaction = new Transaction()
      .withTransactionType(Transaction.TransactionType.ENCUMBRANCE)
      .withAmount(40.0)
      .withCurrency(currency)
      .withFromFundId(fundId);
    FundDistribution fundDistribution = new FundDistribution()
      .withFundId(fundId)
      .withCode(fundCode);
    EncumbranceRelationsHolder holder = new EncumbranceRelationsHolder()
      .withOldEncumbrance(oldTransaction)
      .withNewEncumbrance(newTransaction)
      .withBudget(budget)
      .withFundDistribution(fundDistribution)
      .withRestrictEncumbrances(true)
      .withCurrentFiscalYearId(fiscalYearId)
      .withCurrency(currency);
    List<EncumbranceRelationsHolder> holders = List.of(holder);
    // When / Then
    assertDoesNotThrow(() -> restrictionService.checkEncumbranceRestrictions(holders));
  }

    @Test
  void testCheckEncumbranceRestrictionsWhenMultipleBudgetsFailShouldIncludeAllFundCodesInError() {
    // TestMate-9e9d4146cd11da260717296f59139599
    // Given
    String fiscalYearId = "50917631-1555-46f0-9289-e9324b745426";
    String currency = "USD";
    String fundId1 = "4351333c-3363-4886-90e8-07f79435f37d";
    String fundCode1 = "CODE1";
    Budget budget1 = new Budget()
      .withId(UUID.randomUUID().toString())
      .withFundId(fundId1)
      .withFiscalYearId(fiscalYearId)
      .withTotalFunding(100.0)
      .withUnavailable(100.0)
      .withAllowableEncumbrance(100.0);
    FundDistribution fundDistribution1 = new FundDistribution()
      .withFundId(fundId1)
      .withCode(fundCode1);
    Transaction newEncumbrance1 = new Transaction()
      .withAmount(10.0)
      .withCurrency(currency)
      .withFromFundId(fundId1);
    EncumbranceRelationsHolder holder1 = new EncumbranceRelationsHolder()
      .withNewEncumbrance(newEncumbrance1)
      .withBudget(budget1)
      .withFundDistribution(fundDistribution1)
      .withRestrictEncumbrances(true)
      .withCurrentFiscalYearId(fiscalYearId)
      .withCurrency(currency);
    String fundId2 = "1607590d-2771-419b-a63e-63796f6e5653";
    String fundCode2 = "CODE2";
    Budget budget2 = new Budget()
      .withId(UUID.randomUUID().toString())
      .withFundId(fundId2)
      .withFiscalYearId(fiscalYearId)
      .withTotalFunding(200.0)
      .withUnavailable(200.0)
      .withAllowableEncumbrance(100.0);
    FundDistribution fundDistribution2 = new FundDistribution()
      .withFundId(fundId2)
      .withCode(fundCode2);
    Transaction newEncumbrance2 = new Transaction()
      .withAmount(20.0)
      .withCurrency(currency)
      .withFromFundId(fundId2);
    EncumbranceRelationsHolder holder2 = new EncumbranceRelationsHolder()
      .withNewEncumbrance(newEncumbrance2)
      .withBudget(budget2)
      .withFundDistribution(fundDistribution2)
      .withRestrictEncumbrances(true)
      .withCurrentFiscalYearId(fiscalYearId)
      .withCurrency(currency);
    List<EncumbranceRelationsHolder> holders = List.of(holder1, holder2);
    // When
    HttpException httpException = assertThrows(HttpException.class, () -> restrictionService.checkEncumbranceRestrictions(holders));
    // Then
    assertEquals(422, httpException.getCode());
    Error error = httpException.getError();
    assertEquals(FUND_CANNOT_BE_PAID.getCode(), error.getCode());
    assertEquals("finance.funds", error.getParameters().get(0).getKey());
    String actualFundCodes = error.getParameters().get(0).getValue();
    assertTrue(actualFundCodes.contains(fundCode1));
    assertTrue(actualFundCodes.contains(fundCode2));
  }

    @Test
  void testCheckEncumbranceRestrictionsWhenFundDistributionHasNullFieldsShouldFilterOut() {
    // TestMate-1843ee6ca5dea18ec9846da94e6835d3
    // Given
    String fiscalYearId = UUID.randomUUID().toString();
    String fundId = UUID.randomUUID().toString();
    String budgetId = UUID.randomUUID().toString();
    String currency = "USD";
    FundDistribution incompleteFundDistribution = new FundDistribution()
      .withFundId(fundId)
      .withCode(null);
    Transaction newTransaction = new Transaction()
      .withTransactionType(Transaction.TransactionType.ENCUMBRANCE)
      .withAmount(10.0)
      .withCurrency(currency)
      .withFromFundId(fundId);
    Budget failingBudget = new Budget()
      .withId(budgetId)
      .withFundId(fundId)
      .withFiscalYearId(fiscalYearId)
      .withTotalFunding(100.0)
      .withUnavailable(100.0)
      .withAllowableEncumbrance(100.0);
    EncumbranceRelationsHolder holder = new EncumbranceRelationsHolder()
      .withNewEncumbrance(newTransaction)
      .withOldEncumbrance(null)
      .withBudget(failingBudget)
      .withFundDistribution(incompleteFundDistribution)
      .withRestrictEncumbrances(true)
      .withCurrentFiscalYearId(fiscalYearId)
      .withCurrency(currency);
    List<EncumbranceRelationsHolder> holders = List.of(holder);
    // When
    HttpException httpException = assertThrows(HttpException.class, () -> restrictionService.checkEncumbranceRestrictions(holders));
    // Then
    assertEquals(422, httpException.getCode());
    Error error = httpException.getError();
    assertEquals(FUND_CANNOT_BE_PAID.getCode(), error.getCode());
    assertEquals(FUNDS, error.getParameters().get(0).getKey());
    assertEquals("[null]", error.getParameters().get(0).getValue());
  }

    @Test
  void testCheckEncumbranceRestrictionsWhenDataHoldersIsEmptyShouldPass() {
    // TestMate-bb4b0bec5a3c0dc7cf0b85ba668b3935
    // Given
    List<EncumbranceRelationsHolder> holders = Collections.emptyList();
    // When / Then
    assertDoesNotThrow(() -> restrictionService.checkEncumbranceRestrictions(holders));
  }



}
