package org.folio.service.finance;

import io.vertx.core.Future;
import io.vertx.core.Vertx;
import io.vertx.junit5.VertxExtension;
import io.vertx.junit5.VertxTestContext;
import org.folio.models.EncumbranceRelationsHolder;
import org.folio.models.ReEncumbranceHolder;
import org.folio.rest.acq.model.finance.Budget;
import org.folio.rest.acq.model.finance.FiscalYear;
import org.folio.rest.acq.model.finance.Fund;
import org.folio.rest.acq.model.finance.Ledger;
import org.folio.rest.acq.model.finance.Transaction;
import org.folio.rest.core.exceptions.HttpException;
import org.folio.rest.core.models.RequestContext;
import org.folio.rest.jaxrs.model.CompositePoLine;
import org.folio.rest.jaxrs.model.Cost;
import org.folio.rest.jaxrs.model.Error;
import org.folio.rest.jaxrs.model.FundDistribution;
import org.folio.service.exchange.ExchangeRateProviderResolver;
import org.folio.service.exchange.ManualCurrencyConversion;
import org.folio.service.exchange.ManualExchangeRateProvider;
import org.folio.service.finance.budget.BudgetService;
import org.hamcrest.Matchers;
import org.junit.jupiter.api.AfterEach;
import org.junit.jupiter.api.BeforeEach;
import org.junit.jupiter.api.Test;
import org.junit.jupiter.api.extension.ExtendWith;
import org.mockito.InjectMocks;
import org.mockito.Mock;
import org.mockito.MockitoAnnotations;

import javax.money.convert.ConversionQuery;
import javax.money.convert.ExchangeRateProvider;
import java.util.Collections;
import java.util.List;
import java.util.UUID;

import static org.folio.rest.core.exceptions.ErrorCodes.BUDGET_NOT_FOUND_FOR_FISCAL_YEAR;
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
  private ExchangeRateProviderResolver exchangeRateProviderResolver;
  @Mock
  private FiscalYearService fiscalYearService;
  @Mock
  private RequestContext requestContext;

  private AutoCloseable mockitoMocks;
  private EncumbranceRelationsHolder holder1;
  private EncumbranceRelationsHolder holder2;
  private EncumbranceRelationsHolder holder3;

  @BeforeEach
  public void initMocks(){
    mockitoMocks = MockitoAnnotations.openMocks(this);

    FundDistribution distribution1 = new FundDistribution().withFundId(UUID.randomUUID().toString());

    CompositePoLine line1 = new CompositePoLine().withId(UUID.randomUUID().toString())
      .withCost(new Cost().withCurrency("USD"))
      .withFundDistribution(Collections.singletonList(distribution1));

    FundDistribution distribution2 = new FundDistribution().withFundId(UUID.randomUUID().toString());

    CompositePoLine line2 = new CompositePoLine().withId(UUID.randomUUID().toString())
      .withCost(new Cost().withCurrency("USD"))
      .withFundDistribution(Collections.singletonList(distribution2));

    FundDistribution distribution3 = new FundDistribution().withFundId(UUID.randomUUID().toString());

    CompositePoLine line3 = new CompositePoLine().withId(UUID.randomUUID().toString())
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
    mockitoMocks.close();
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
    ExchangeRateProvider exchangeRateProvider = mock(ManualExchangeRateProvider.class);
    when(exchangeRateProviderResolver.resolve(any(), any()))
      .thenReturn(exchangeRateProvider);
    when(exchangeRateProvider.getCurrencyConversion(any(ConversionQuery.class)))
      .thenAnswer(invocation -> {
        ConversionQuery conversionQuery = invocation.getArgument(0);
        return mock(ManualCurrencyConversion.class, conversionQuery.getBaseCurrency().getCurrencyCode());
      });
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

        assertEquals("USD", holder1.getPoLineToFyConversion().toString());
        assertEquals("USD", holder2.getPoLineToFyConversion().toString());
        assertSame(holder1.getPoLineToFyConversion(), holder2.getPoLineToFyConversion());
        assertEquals("EUR", holder3.getPoLineToFyConversion().toString());

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
    verify(exchangeRateProviderResolver, never()).resolve(any(), any());
  }

  @Test
  void shouldNotFailIfHoldersHaveNoFund() {
    EncumbranceRelationsHolder holder1 = new EncumbranceRelationsHolder().withFundDistribution(new FundDistribution());
    EncumbranceRelationsHolder holder2 = new EncumbranceRelationsHolder().withFundDistribution(new FundDistribution());
    List<EncumbranceRelationsHolder> holders = List.of(holder1, holder2);

    Future<Void> f = financeHoldersBuilder.withFinances(holders, requestContext);

    assertTrue(f.succeeded());
  }

  @Test
  void shouldThrowExceptionWithMultipleFiscalYears() {
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
    HttpException httpException = (HttpException)f.cause();
    assertEquals(422, httpException.getCode());
    Error error = httpException.getError();
    assertEquals(BUDGET_NOT_FOUND_FOR_FISCAL_YEAR.getCode(), error.getCode());
    assertEquals(fund2.getId(), error.getParameters().get(0).getValue());
    assertEquals(fiscalYear1.getId(), error.getParameters().get(1).getValue());
    assertEquals(fiscalYear1.getCode(), error.getParameters().get(2).getValue());
  }

  @Test
  void shouldNotRetrieveFundsIfReEncumbranceHoldersIsEmpty() {
    List<ReEncumbranceHolder> holders = Collections.emptyList();

    financeHoldersBuilder.getLedgerIds(holders, requestContext).result();

    assertThat(holders, Matchers.is(empty()));
    verify(fundService, never()).getFunds(anyCollection(), any());
  }

}
