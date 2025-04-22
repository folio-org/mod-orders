package org.folio.service.finance;

import io.vertx.core.Future;
import io.vertx.core.Vertx;
import io.vertx.junit5.VertxExtension;
import io.vertx.junit5.VertxTestContext;
import org.apache.commons.lang.StringUtils;
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
    verify(fundService, never()).getFunds(anyCollection(), any());
  }
}
