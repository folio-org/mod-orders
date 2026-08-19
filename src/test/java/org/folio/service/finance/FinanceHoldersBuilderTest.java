package org.folio.service.finance;

import io.vertx.core.Future;
import io.vertx.core.Vertx;
import io.vertx.junit5.VertxExtension;
import io.vertx.junit5.VertxTestContext;
import org.apache.commons.lang.StringUtils;
import org.folio.TestMate;
import org.folio.models.EncumbranceRelationsHolder;
import org.folio.models.ReEncumbranceHolder;
import org.folio.rest.acq.model.finance.Budget;
import org.folio.rest.acq.model.finance.ExchangeRate;
import org.folio.rest.acq.model.finance.FiscalYear;
import org.folio.rest.acq.model.finance.Fund;
import org.folio.rest.acq.model.finance.Ledger;
import org.folio.rest.acq.model.finance.Transaction;
import org.folio.rest.core.exceptions.HttpException;
import org.folio.rest.core.models.RequestContext;
import org.folio.rest.jaxrs.model.PoLine;
import org.folio.rest.jaxrs.model.CompositePurchaseOrder;
import org.folio.rest.jaxrs.model.Cost;
import org.folio.rest.jaxrs.model.Error;
import org.folio.rest.jaxrs.model.FundDistribution;
import org.folio.service.exchange.CacheableExchangeRateService;
import org.folio.service.finance.budget.BudgetService;
import org.hamcrest.Matchers;
import org.junit.jupiter.api.AfterEach;
import org.junit.jupiter.api.BeforeEach;
import org.junit.jupiter.api.Test;
import org.junit.jupiter.api.extension.ExtendWith;
import org.mockito.InjectMocks;
import org.mockito.Mock;
import org.mockito.MockitoAnnotations;

import java.util.Collections;
import java.util.List;
import java.util.UUID;

import static org.folio.rest.core.exceptions.ErrorCodes.BUDGET_NOT_FOUND_FOR_FISCAL_YEAR;
import static org.folio.rest.core.exceptions.ErrorCodes.MULTIPLE_FISCAL_YEARS;
import static org.hamcrest.MatcherAssert.assertThat;
import static org.hamcrest.Matchers.allOf;
import static org.hamcrest.Matchers.empty;
import static org.hamcrest.Matchers.everyItem;
import static org.hamcrest.Matchers.hasProperty;
import static org.hamcrest.core.Is.is;
import static org.junit.jupiter.api.Assertions.assertEquals;
import static org.junit.jupiter.api.Assertions.assertSame;
import static org.junit.jupiter.api.Assertions.assertTrue;
import static org.mockito.ArgumentMatchers.any;
import static org.mockito.ArgumentMatchers.anyCollection;
import static org.mockito.ArgumentMatchers.anyString;
import static org.mockito.ArgumentMatchers.eq;
import static org.mockito.Mockito.mock;
import static org.mockito.Mockito.never;
import static org.mockito.Mockito.verify;
import static org.mockito.Mockito.when;
import static org.hamcrest.Matchers.contains;
import static org.hamcrest.Matchers.hasSize;
import static org.mockito.Mockito.times;
import org.mockito.ArgumentCaptor;
import static io.vertx.core.Future.failedFuture;
import static io.vertx.core.Future.succeededFuture;
import static org.hamcrest.Matchers.nullValue;
import static org.mockito.ArgumentMatchers.isNull;
import org.folio.models.EncumbranceConversionHolder;
import static org.hamcrest.Matchers.containsInAnyOrder;

@ExtendWith(VertxExtension.class)
public class FinanceHoldersBuilderTest {

  @InjectMocks
  private FinanceHoldersBuilder financeHoldersBuilder;
  @Mock
  private BudgetService budgetService;
  @Mock
  private FundService fundService;
  @Mock
  private LedgerService ledgerService;
  @Mock
  private FiscalYearService fiscalYearService;
  @Mock
  private CacheableExchangeRateService cacheableExchangeRateService;
  @Mock
  private RequestContext requestContext;

  private AutoCloseable mockitoMocks;
  private EncumbranceRelationsHolder holder1;
  private EncumbranceRelationsHolder holder2;
  private EncumbranceRelationsHolder holder3;

  @BeforeEach
  public void initMocks() {
    mockitoMocks = MockitoAnnotations.openMocks(this);

    FundDistribution distribution1 = new FundDistribution().withFundId(UUID.randomUUID().toString()).withCode("FUND1");

    // Source/base/from currency
    PoLine line1 = new PoLine().withId(UUID.randomUUID().toString())
      .withPoLineNumber("1")
      .withCost(new Cost().withCurrency("USD"))
      .withFundDistribution(Collections.singletonList(distribution1));

    FundDistribution distribution2 = new FundDistribution().withFundId(UUID.randomUUID().toString()).withCode("FUND2");

    PoLine line2 = new PoLine().withId(UUID.randomUUID().toString())
      .withPoLineNumber("2")
      .withCost(new Cost().withCurrency("USD"))
      .withFundDistribution(Collections.singletonList(distribution2));

    FundDistribution distribution3 = new FundDistribution().withFundId(UUID.randomUUID().toString());

    PoLine line3 = new PoLine().withId(UUID.randomUUID().toString())
      .withCost(new Cost().withCurrency("EUR"))
      .withFundDistribution(Collections.singletonList(distribution3));

    Transaction newEncumbrance1 = new Transaction();
    Transaction newEncumbrance2 = new Transaction();
    Transaction newEncumbrance3 = new Transaction();

    holder1 = new EncumbranceRelationsHolder()
      .withPoLine(line1)
      .withFundDistribution(distribution1)
      .withNewEncumbrance(newEncumbrance1);
    holder2 = new EncumbranceRelationsHolder()
      .withPoLine(line2)
      .withFundDistribution(distribution2)
      .withNewEncumbrance(newEncumbrance2);
    holder3 = new EncumbranceRelationsHolder()
      .withPoLine(line3)
      .withFundDistribution(distribution3)
      .withNewEncumbrance(newEncumbrance3);
  }

  @AfterEach
  public void resetMocks() throws Exception {
    if (mockitoMocks != null) {
      mockitoMocks.close();
    }
  }

  @Test
  void shouldPopulateHoldersWithFinanceStructures(VertxTestContext vertxTestContext) {
    // Given
    Ledger ledger1 = new Ledger().withId(UUID.randomUUID().toString()).withRestrictEncumbrance(true);
    Ledger ledger2 = new Ledger().withId(UUID.randomUUID().toString()).withRestrictEncumbrance(true);
    Ledger ledger3 = new Ledger().withId(UUID.randomUUID().toString()).withRestrictEncumbrance(false);

    Fund fund1 = new Fund().withId(holder1.getFundId()).withLedgerId(ledger1.getId());
    Fund fund2 = new Fund().withId(holder2.getFundId()).withLedgerId(ledger2.getId());
    Fund fund3 = new Fund().withId(holder3.getFundId()).withLedgerId(ledger3.getId());

    Budget budget1 = new Budget().withId(UUID.randomUUID().toString()).withFundId(holder1.getFundId());
    Budget budget2 = new Budget().withId(UUID.randomUUID().toString()).withFundId(holder2.getFundId());
    Budget budget3 = new Budget().withId(UUID.randomUUID().toString()).withFundId(holder3.getFundId());

    String fiscalYearId = UUID.randomUUID().toString();
    // Destination/term/to currency
    FiscalYear fiscalYear = new FiscalYear().withId(fiscalYearId).withCurrency("RUB");

    List<EncumbranceRelationsHolder> holders = List.of(holder1, holder2, holder3);

    when(fundService.getAllFunds(anyCollection(), any()))
      .thenReturn(Future.succeededFuture(List.of(fund1, fund2, fund3)));
    when(ledgerService.getLedgersByIds(anyCollection(), any()))
      .thenReturn(Future.succeededFuture(List.of(ledger2, ledger1, ledger3)));
    when(fiscalYearService.getCurrentFiscalYear(anyString(), any()))
      .thenReturn(Future.succeededFuture(fiscalYear));
    when(budgetService.getBudgetsByQuery(anyString(), any()))
      .thenReturn(Future.succeededFuture(List.of(budget1, budget2, budget3)));
    when(cacheableExchangeRateService.getExchangeRate(any(), any(), any(), eq(requestContext)))
      .thenReturn(Future.succeededFuture(mock(ExchangeRate.class)));
    when(requestContext.getContext())
      .thenReturn(Vertx.vertx().getOrCreateContext());

    // When
    var future = financeHoldersBuilder.withFinances(holders, requestContext);

    // Then
    vertxTestContext.assertComplete(future)
      .onComplete(result -> {
        assertEquals(ledger1.getId(), holder1.getLedgerId());
        assertEquals(ledger1.getRestrictEncumbrance(), holder1.getRestrictEncumbrance());
        assertEquals(ledger2.getId(), holder2.getLedgerId());
        assertEquals(ledger2.getRestrictEncumbrance(), holder2.getRestrictEncumbrance());
        assertEquals(ledger3.getId(), holder3.getLedgerId());
        assertEquals(ledger3.getRestrictEncumbrance(), holder3.getRestrictEncumbrance());

        assertEquals(budget1, holder1.getBudget());
        assertEquals(budget2, holder2.getBudget());
        assertEquals(budget3, holder3.getBudget());

        assertThat(holders, everyItem(hasProperty("newEncumbrance", allOf(
          hasProperty("fiscalYearId", is(fiscalYear.getId())),
          hasProperty("currency", is(fiscalYear.getCurrency()))
        ))));

        // Assertion is changed to validate against a real provider object with a proper toString() implementation
        assertTrue(StringUtils.contains(holder1.getPoLineToFyConversion().toString(), "baseCurrency=USD"));
        assertTrue(StringUtils.contains(holder1.getPoLineToFyConversion().toString(), "termCurrency=RUB"));

        assertTrue(StringUtils.contains(holder2.getPoLineToFyConversion().toString(), "baseCurrency=USD"));
        assertTrue(StringUtils.contains(holder2.getPoLineToFyConversion().toString(), "termCurrency=RUB"));

        // The same currency is used so the currency conversion object is reused
        assertSame(holder1.getPoLineToFyConversion(), holder2.getPoLineToFyConversion());

        assertTrue(StringUtils.contains(holder3.getPoLineToFyConversion().toString(), "baseCurrency=EUR"));
        assertTrue(StringUtils.contains(holder3.getPoLineToFyConversion().toString(), "termCurrency=RUB"));

        vertxTestContext.completeNow();
      });
  }

  @Test
  void shouldNotRetrieveAnythingIfHoldersAreEmpty() {
    List<EncumbranceRelationsHolder> holders = Collections.emptyList();

    Future<Void> f = financeHoldersBuilder.withFinances(holders, requestContext);

    assertTrue(f.succeeded());
    assertThat(holders, Matchers.is(empty()));
    verify(fundService, never()).getAllFunds(anyCollection(), any());
    verify(ledgerService, never()).getLedgersByIds(anyCollection(), any());
    verify(fiscalYearService, never()).getCurrentFiscalYear(anyString(), any());
    verify(budgetService, never()).getBudgetsByQuery(anyString(), any());
    verify(cacheableExchangeRateService, never()).getExchangeRate(any(), any(), any(), any());
  }

  @Test
  void shouldNotFailIfHoldersHaveNoFund() {
    EncumbranceRelationsHolder encumbranceRelationsHolder1 = new EncumbranceRelationsHolder().withFundDistribution(new FundDistribution());
    EncumbranceRelationsHolder encumbranceRelationsHolder2 = new EncumbranceRelationsHolder().withFundDistribution(new FundDistribution());
    List<EncumbranceRelationsHolder> holders = List.of(encumbranceRelationsHolder1, encumbranceRelationsHolder2);

    Future<Void> f = financeHoldersBuilder.withFinances(holders, requestContext);

    assertTrue(f.succeeded());
  }

  @Test
  void shouldThrowExceptionWhenBudgetNotFoundForBudget() {
    // Note: when the funds are using multiple fiscal years, some budgets will not be found using the first fiscal year
    // The same error can also happen if a budget is missing or inactive.
    // Given
    FiscalYear fiscalYear1 = new FiscalYear().withId(UUID.randomUUID().toString()).withCode("FY1");

    Ledger ledger1 = new Ledger().withId(UUID.randomUUID().toString()).withRestrictEncumbrance(true);
    Ledger ledger2 = new Ledger().withId(UUID.randomUUID().toString()).withRestrictEncumbrance(true);

    Fund fund1 = new Fund().withId(holder1.getFundId()).withLedgerId(ledger1.getId());
    Fund fund2 = new Fund().withId(holder2.getFundId()).withLedgerId(ledger2.getId());

    Budget budget1 = new Budget()
      .withId(UUID.randomUUID().toString())
      .withFundId(fund1.getId())
      .withFiscalYearId(fiscalYear1.getId());

    List<EncumbranceRelationsHolder> holders = List.of(holder1, holder2);

    when(fundService.getAllFunds(anyCollection(), any()))
      .thenReturn(Future.succeededFuture(List.of(fund1, fund2)));
    when(ledgerService.getLedgersByIds(anyCollection(), any()))
      .thenReturn(Future.succeededFuture(List.of(ledger1, ledger2)));
    when(fiscalYearService.getCurrentFiscalYear(anyString(), any()))
      .thenReturn(Future.succeededFuture(fiscalYear1));
    when(budgetService.getBudgetsByQuery(anyString(), any()))
      .thenReturn(Future.succeededFuture(List.of(budget1)));

    // When
    Future<Void> f = financeHoldersBuilder.withFinances(holders, requestContext);

    // Then
    HttpException httpException = (HttpException) f.cause();
    assertEquals(422, httpException.getCode());
    Error error = httpException.getError();
    assertEquals(BUDGET_NOT_FOUND_FOR_FISCAL_YEAR.getCode(), error.getCode());
    assertEquals(List.of(fund2.getId()).toString(), error.getParameters().get(0).getValue());
    assertEquals("[FUND2]", error.getParameters().get(1).getValue());
    assertEquals(List.of(holder2.getPoLineId()).toString(), error.getParameters().get(2).getValue());
    assertEquals("[2]", error.getParameters().get(3).getValue());
    assertEquals(fiscalYear1.getId(), error.getParameters().get(4).getValue());
    assertEquals(fiscalYear1.getCode(), error.getParameters().get(5).getValue());
  }

  @Test
  void shouldThrowExceptionWithMultipleFiscalYears() {
    var purchaseOrderId = UUID.randomUUID().toString();
    var fiscalYear1 = new FiscalYear().withId(UUID.randomUUID().toString()).withCode("FY1");
    var fiscalYear2 = new FiscalYear().withId(UUID.randomUUID().toString()).withCode("FY1");

    var ledger1 = new Ledger().withId(UUID.randomUUID().toString())
      .withRestrictEncumbrance(true).withFiscalYearOneId(fiscalYear1.getId());
    var ledger2 = new Ledger().withId(UUID.randomUUID().toString())
      .withRestrictEncumbrance(true).withFiscalYearOneId(fiscalYear2.getId());

    var fund1 = new Fund().withId(holder1.getFundId()).withLedgerId(ledger1.getId());
    var fund2 = new Fund().withId(holder2.getFundId()).withLedgerId(ledger2.getId());

    var budget1 = new Budget()
      .withId(UUID.randomUUID().toString())
      .withFundId(fund1.getId())
      .withFiscalYearId(fiscalYear1.getId());

    var holders = List.of(holder1, holder2);
    holders.getFirst().withPurchaseOrder(new CompositePurchaseOrder().withId(purchaseOrderId));

    when(fundService.getAllFunds(anyCollection(), any()))
      .thenReturn(Future.succeededFuture(List.of(fund1, fund2)));
    when(ledgerService.getLedgersByIds(anyCollection(), any()))
      .thenReturn(Future.succeededFuture(List.of(ledger1, ledger2)));
    when(fiscalYearService.getCurrentFiscalYear(eq(ledger1.getId()), any()))
      .thenReturn(Future.succeededFuture(fiscalYear1));
    when(fiscalYearService.getCurrentFiscalYear(eq(ledger2.getId()), any()))
      .thenReturn(Future.succeededFuture(fiscalYear2));
    when(budgetService.getBudgetsByQuery(anyString(), any()))
      .thenReturn(Future.succeededFuture(List.of(budget1)));

    // When
    Future<Void> f = financeHoldersBuilder.withFinances(holders, requestContext);

    // Then
    HttpException httpException = (HttpException) f.cause();
    assertEquals(422, httpException.getCode());
    Error error = httpException.getError();
    assertEquals(MULTIPLE_FISCAL_YEARS.getCode(), error.getCode());
    assertEquals(List.of(fiscalYear1.getId(), fiscalYear2.getId()).toString(), error.getParameters().get(0).getValue());
    assertEquals(purchaseOrderId, error.getParameters().get(1).getValue());
  }

  @Test
  void shouldNotRetrieveFundsIfReEncumbranceHoldersIsEmpty() {
    List<ReEncumbranceHolder> holders = Collections.emptyList();

    financeHoldersBuilder.getLedgerIds(holders, requestContext).result();

    assertThat(holders, Matchers.is(empty()));
    verify(fundService, never()).getAllFunds(anyCollection(), any());
  }

  @Test
  @TestMate(name = "TestMate-d3ea99a44af1eb10c06c87c554583a4d")
  void testGetLedgerIdsShouldHandleDuplicateFundIdsEfficiently() {
    // Given
    String sharedFundId = UUID.fromString("00000000-0000-0000-0000-000000000001").toString();
    String sharedLedgerId = UUID.fromString("ffffffff-ffff-ffff-ffff-ffffffffffff").toString();
    FundDistribution distribution1 = new FundDistribution().withFundId(sharedFundId);
    PoLine line1 = new PoLine().withId(UUID.randomUUID().toString()).withFundDistribution(List.of(distribution1));
    EncumbranceRelationsHolder holderA = new EncumbranceRelationsHolder()
      .withPoLine(line1)
      .withFundDistribution(distribution1);
    FundDistribution distribution2 = new FundDistribution().withFundId(sharedFundId);
    PoLine line2 = new PoLine().withId(UUID.randomUUID().toString()).withFundDistribution(List.of(distribution2));
    EncumbranceRelationsHolder holderB = new EncumbranceRelationsHolder()
      .withPoLine(line2)
      .withFundDistribution(distribution2);
    List<EncumbranceRelationsHolder> holders = List.of(holderA, holderB);
    Fund fund = new Fund().withId(sharedFundId).withLedgerId(sharedLedgerId);
    when(fundService.getAllFunds(any(), eq(requestContext)))
      .thenReturn(Future.succeededFuture(List.of(fund)));

    // When
    List<String> resultLedgerIds = financeHoldersBuilder.getLedgerIds(holders, requestContext).result();

    // Then
    assertThat(resultLedgerIds, hasSize(1));
    assertThat(resultLedgerIds, contains(sharedLedgerId));
    @SuppressWarnings("unchecked")
    ArgumentCaptor<List<String>> fundIdsCaptor = ArgumentCaptor.forClass(List.class);
    verify(fundService, times(1)).getAllFunds(fundIdsCaptor.capture(), eq(requestContext));
    List<String> capturedFundIds = fundIdsCaptor.getValue();
    assertThat(capturedFundIds, hasSize(1));
    assertEquals(sharedFundId, capturedFundIds.getFirst());
    assertEquals(sharedLedgerId, holderA.getLedgerId());
    assertEquals(sharedLedgerId, holderB.getLedgerId());
  }

  @Test
  @TestMate(name = "TestMate-3692795f08922f068f89b7dc871d7bc1")
  void testGetLedgerIdsShouldPropagateFailureFromFundService(VertxTestContext vertxTestContext) {
    // Given
    HttpException expectedException = new HttpException(500, "Service Unavailable");
    when(fundService.getAllFunds(anyCollection(), eq(requestContext)))
      .thenReturn(failedFuture(expectedException));

    // When
    Future<List<String>> future = financeHoldersBuilder.getLedgerIds(List.of(holder1), requestContext);

    // Then
    vertxTestContext.assertFailure(future)
      .onComplete(result -> {
        HttpException actualException = (HttpException) result.cause();
        assertEquals(expectedException.getCode(), actualException.getCode());
        assertEquals(expectedException.getMessage(), actualException.getMessage());
        verify(fundService, times(1)).getAllFunds(anyCollection(), eq(requestContext));
        vertxTestContext.completeNow();
      });
  }

  @Test
  @TestMate(name = "TestMate-9f7bf3acef982a80bc2412f8e658c55f")
  void testGetLedgerIdsShouldHandleMixedNullAndValidFundIds() {
    // Given
    String validFundId = "00000000-0000-0000-0000-000000000001";
    String validLedgerId = "ffffffff-ffff-ffff-ffff-ffffffffffff";
    EncumbranceRelationsHolder holderWithFund = new EncumbranceRelationsHolder()
      .withFundDistribution(new FundDistribution().withFundId(validFundId));
    EncumbranceRelationsHolder holderWithNullFund = new EncumbranceRelationsHolder()
      .withFundDistribution(new FundDistribution().withFundId(null));
    List<EncumbranceRelationsHolder> holders = List.of(holderWithFund, holderWithNullFund);
    Fund fund = new Fund().withId(validFundId).withLedgerId(validLedgerId);
    when(fundService.getAllFunds(anyCollection(), any()))
      .thenReturn(succeededFuture(List.of(fund)));

    // When
    List<String> resultLedgerIds = financeHoldersBuilder.getLedgerIds(holders, requestContext).result();

    // Then
    assertThat(resultLedgerIds, hasSize(1));
    assertThat(resultLedgerIds, contains(validLedgerId));

    @SuppressWarnings("unchecked")
    ArgumentCaptor<List<String>> fundIdsCaptor = ArgumentCaptor.forClass(List.class);
    verify(fundService, times(1)).getAllFunds(fundIdsCaptor.capture(), eq(requestContext));
    List<String> capturedFundIds = fundIdsCaptor.getValue();
    assertThat(capturedFundIds, hasSize(1));
    assertEquals(validFundId, capturedFundIds.getFirst());
    assertThat(holderWithFund.getLedgerId(), is(validLedgerId));
    assertThat(holderWithNullFund.getLedgerId(), nullValue());
  }

    @Test
  void testGetExchangeRatesPerCurrencyHolderShouldFilterOutHoldersWithoutPoLines(VertxTestContext vertxTestContext) {
    // TestMate-889f7f4ac3125f03846c04247965fca4
    // Given
    String fyCurrency = "EUR";
    String poLineCurrency = "USD";
    PoLine poLine = new PoLine().withCost(new Cost().withCurrency(poLineCurrency));
    EncumbranceRelationsHolder validHolder = new EncumbranceRelationsHolder()
      .withPoLine(poLine)
      .withCurrency(fyCurrency);
    EncumbranceRelationsHolder invalidHolder = new EncumbranceRelationsHolder()
      .withPoLine(null)
      .withCurrency(fyCurrency);
    List<EncumbranceRelationsHolder> holders = List.of(validHolder, invalidHolder);
    ExchangeRate exchangeRate = mock(ExchangeRate.class);
    when(exchangeRate.getExchangeRate()).thenReturn(1.0);
    when(exchangeRate.getOperationMode()).thenReturn(ExchangeRate.OperationMode.MULTIPLY);
    when(cacheableExchangeRateService.getExchangeRate(eq(poLineCurrency), eq(fyCurrency), isNull(), eq(requestContext)))
      .thenReturn(succeededFuture(exchangeRate));
    // When
    Future<List<EncumbranceConversionHolder>> future = financeHoldersBuilder.getExchangeRatesPerCurrencyHolder(holders, requestContext);
    // Then
    vertxTestContext.assertComplete(future)
      .onComplete(result -> {
        assertTrue(result.succeeded());
        List<EncumbranceConversionHolder> conversionHolders = result.result();
        assertThat(conversionHolders, hasSize(1));
        EncumbranceConversionHolder conversionHolder = conversionHolders.get(0);
        assertThat(conversionHolder.getEncumbranceRelationsHolders(), hasSize(1));
        assertThat(conversionHolder.getEncumbranceRelationsHolders(), contains(validHolder));
        verify(cacheableExchangeRateService, times(1))
          .getExchangeRate(eq(poLineCurrency), eq(fyCurrency), any(), eq(requestContext));
        vertxTestContext.completeNow();
      });
  }

    @Test
  void testGetExchangeRatesPerCurrencyHolderShouldGroupHoldersByCurrencyAndRetrieveRates(VertxTestContext vertxTestContext) {
    // TestMate-d4e825ce318718d4d1f7fdb9ca9d51ae
    // Given
    String fyCurrency = "RUB";
    String usd = "USD";
    String gbp = "GBP";
    holder1.withCurrency(fyCurrency);
    holder2.withCurrency(fyCurrency);
    holder3.withCurrency(fyCurrency);
    holder3.getPoLine().getCost().setCurrency(gbp);
    List<EncumbranceRelationsHolder> holders = List.of(holder1, holder2, holder3);
    ExchangeRate usdRate = mock(ExchangeRate.class);
    when(usdRate.getExchangeRate()).thenReturn(75.0);
    when(usdRate.getOperationMode()).thenReturn(ExchangeRate.OperationMode.MULTIPLY);
    ExchangeRate gbpRate = mock(ExchangeRate.class);
    when(gbpRate.getExchangeRate()).thenReturn(95.0);
    when(gbpRate.getOperationMode()).thenReturn(ExchangeRate.OperationMode.MULTIPLY);
    when(cacheableExchangeRateService.getExchangeRate(eq(usd), eq(fyCurrency), any(), eq(requestContext)))
      .thenReturn(succeededFuture(usdRate));
    when(cacheableExchangeRateService.getExchangeRate(eq(gbp), eq(fyCurrency), any(), eq(requestContext)))
      .thenReturn(succeededFuture(gbpRate));
    when(requestContext.getContext())
      .thenReturn(Vertx.vertx().getOrCreateContext());
    // When
    Future<List<EncumbranceConversionHolder>> future = financeHoldersBuilder.getExchangeRatesPerCurrencyHolder(holders, requestContext);
    // Then
    vertxTestContext.assertComplete(future)
      .onComplete(result -> {
        List<EncumbranceConversionHolder> conversionHolders = result.result();
        assertThat(conversionHolders, hasSize(2));
        EncumbranceConversionHolder usdConversion = conversionHolders.stream()
          .filter(h -> h.getEncumbranceRelationsHolders().stream().anyMatch(erh -> erh.getPoLine().getCost().getCurrency().equals(usd)))
          .findFirst()
          .orElseThrow();
        assertThat(usdConversion.getEncumbranceRelationsHolders(), hasSize(2));
        assertThat(usdConversion.getEncumbranceRelationsHolders(), containsInAnyOrder(holder1, holder2));
        EncumbranceConversionHolder gbpConversion = conversionHolders.stream()
          .filter(h -> h.getEncumbranceRelationsHolders().stream().anyMatch(erh -> erh.getPoLine().getCost().getCurrency().equals(gbp)))
          .findFirst()
          .orElseThrow();
        assertThat(gbpConversion.getEncumbranceRelationsHolders(), hasSize(1));
        assertEquals(holder3, gbpConversion.getEncumbranceRelationsHolders().get(0));
        verify(cacheableExchangeRateService, times(1)).getExchangeRate(eq(usd), eq(fyCurrency), any(), eq(requestContext));
        verify(cacheableExchangeRateService, times(1)).getExchangeRate(eq(gbp), eq(fyCurrency), any(), eq(requestContext));
        vertxTestContext.completeNow();
      });
  }

    @Test
  void testGetExchangeRatesPerCurrencyHolderShouldUseFirstAvailableFixedExchangeRateInGroup(VertxTestContext vertxTestContext) {
    // TestMate-06227643b9a22e84d54690ebdcd6c34e
    // Given
    String poLineCurrency = "USD";
    String fyCurrency = "EUR";
    Double firstManualRate = 1.5;
    Double ignoredManualRate = 1.8;
    PoLine line1 = new PoLine().withId("00000000-0000-0000-0000-000000000001")
      .withCost(new Cost().withCurrency(poLineCurrency).withExchangeRate(null))
      .withFundDistribution(List.of(new FundDistribution().withFundId(UUID.randomUUID().toString())));
    PoLine line2 = new PoLine().withId("00000000-0000-0000-0000-000000000002")
      .withCost(new Cost().withCurrency(poLineCurrency).withExchangeRate(firstManualRate))
      .withFundDistribution(List.of(new FundDistribution().withFundId(UUID.randomUUID().toString())));
    PoLine line3 = new PoLine().withId("00000000-0000-0000-0000-000000000003")
      .withCost(new Cost().withCurrency(poLineCurrency).withExchangeRate(ignoredManualRate))
      .withFundDistribution(List.of(new FundDistribution().withFundId(UUID.randomUUID().toString())));
    EncumbranceRelationsHolder holderA = new EncumbranceRelationsHolder()
      .withPoLine(line1)
      .withCurrency(fyCurrency);
    EncumbranceRelationsHolder holderB = new EncumbranceRelationsHolder()
      .withPoLine(line2)
      .withCurrency(fyCurrency);
    EncumbranceRelationsHolder holderC = new EncumbranceRelationsHolder()
      .withPoLine(line3)
      .withCurrency(fyCurrency);
    List<EncumbranceRelationsHolder> holders = List.of(holderA, holderB, holderC);
    ExchangeRate exchangeRate = mock(ExchangeRate.class);
    when(exchangeRate.getExchangeRate()).thenReturn(firstManualRate);
    when(exchangeRate.getOperationMode()).thenReturn(ExchangeRate.OperationMode.MULTIPLY);
    when(cacheableExchangeRateService.getExchangeRate(eq(poLineCurrency), eq(fyCurrency), eq(firstManualRate), eq(requestContext)))
      .thenReturn(succeededFuture(exchangeRate));
    when(requestContext.getContext())
      .thenReturn(Vertx.vertx().getOrCreateContext());
    // When
    Future<List<EncumbranceConversionHolder>> future = financeHoldersBuilder.getExchangeRatesPerCurrencyHolder(holders, requestContext);
    // Then
    vertxTestContext.assertComplete(future)
      .onComplete(result -> {
        assertTrue(result.succeeded());
        List<EncumbranceConversionHolder> conversionHolders = result.result();
        
        assertThat(conversionHolders, hasSize(1));
        EncumbranceConversionHolder conversionHolder = conversionHolders.get(0);
        
        assertThat(conversionHolder.getEncumbranceRelationsHolders(), hasSize(3));
        assertThat(conversionHolder.getEncumbranceRelationsHolders(), containsInAnyOrder(holderA, holderB, holderC));
        
        verify(cacheableExchangeRateService, times(1))
          .getExchangeRate(eq(poLineCurrency), eq(fyCurrency), eq(firstManualRate), eq(requestContext));
        
        vertxTestContext.completeNow();
      });
  }

    @Test
  void testGetExchangeRatesPerCurrencyHolderShouldPropagateFailureFromExchangeRateService(VertxTestContext vertxTestContext) {
    // TestMate-7de4e182d028e1b0a04a3c79fb5a6fb2
    // Given
    String fyCurrency = "RUB";
    String poLineCurrency = holder1.getPoLine().getCost().getCurrency();
    holder1.withCurrency(fyCurrency);
    HttpException expectedException = new HttpException(500, "API Error");
    when(cacheableExchangeRateService.getExchangeRate(eq(poLineCurrency), eq(fyCurrency), any(), eq(requestContext)))
      .thenReturn(failedFuture(expectedException));
    // When
    Future<List<EncumbranceConversionHolder>> future = financeHoldersBuilder.getExchangeRatesPerCurrencyHolder(List.of(holder1), requestContext);
    // Then
    vertxTestContext.assertFailure(future)
      .onComplete(result -> {
        HttpException actualException = (HttpException) result.cause();
        assertEquals(expectedException.getCode(), actualException.getCode());
        assertEquals("API Error", actualException.getError().getMessage());
        verify(cacheableExchangeRateService, times(1))
          .getExchangeRate(eq(poLineCurrency), eq(fyCurrency), any(), eq(requestContext));
        vertxTestContext.completeNow();
      });
  }
}
