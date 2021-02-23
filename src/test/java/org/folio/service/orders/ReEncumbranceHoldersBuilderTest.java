package org.folio.service.orders;

import static org.hamcrest.MatcherAssert.assertThat;
import static org.hamcrest.Matchers.allOf;
import static org.hamcrest.Matchers.empty;
import static org.hamcrest.Matchers.everyItem;
import static org.hamcrest.Matchers.hasItem;
import static org.hamcrest.Matchers.hasProperty;
import static org.hamcrest.Matchers.hasSize;
import static org.hamcrest.Matchers.is;
import static org.hamcrest.Matchers.isOneOf;
import static org.hamcrest.Matchers.nullValue;
import static org.junit.jupiter.api.Assertions.assertEquals;
import static org.junit.jupiter.api.Assertions.assertTrue;
import static org.mockito.ArgumentMatchers.any;
import static org.mockito.ArgumentMatchers.anyCollection;
import static org.mockito.ArgumentMatchers.anyInt;
import static org.mockito.ArgumentMatchers.anyList;
import static org.mockito.ArgumentMatchers.anyString;
import static org.mockito.ArgumentMatchers.contains;
import static org.mockito.Mockito.never;
import static org.mockito.Mockito.verify;
import static org.mockito.Mockito.when;

import java.util.Arrays;
import java.util.Collections;
import java.util.List;
import java.util.UUID;
import java.util.concurrent.CompletableFuture;

import javax.money.convert.ConversionQuery;
import javax.money.convert.ConversionQueryBuilder;
import javax.money.convert.CurrencyConversion;

import org.folio.models.ReEncumbranceHolder;
import org.folio.rest.acq.model.finance.Budget;
import org.folio.rest.acq.model.finance.Encumbrance;
import org.folio.rest.acq.model.finance.FiscalYear;
import org.folio.rest.acq.model.finance.Fund;
import org.folio.rest.acq.model.finance.Ledger;
import org.folio.rest.acq.model.finance.Transaction;
import org.folio.rest.acq.model.finance.TransactionCollection;
import org.folio.rest.core.models.RequestContext;
import org.folio.rest.jaxrs.model.CompositePoLine;
import org.folio.rest.jaxrs.model.CompositePurchaseOrder;
import org.folio.rest.jaxrs.model.Cost;
import org.folio.rest.jaxrs.model.EncumbranceRollover;
import org.folio.rest.jaxrs.model.FundDistribution;
import org.folio.rest.jaxrs.model.LedgerFiscalYearRollover;
import org.folio.rest.jaxrs.model.Ongoing;
import org.folio.service.exchange.ExchangeRateProviderResolver;
import org.folio.service.exchange.ManualCurrencyConversion;
import org.folio.service.exchange.ManualExchangeRateProvider;
import org.folio.service.finance.BudgetService;
import org.folio.service.finance.FiscalYearService;
import org.folio.service.finance.FundService;
import org.folio.service.finance.LedgerService;
import org.folio.service.finance.RolloverRetrieveService;
import org.folio.service.finance.TransactionService;
import org.junit.jupiter.api.BeforeEach;
import org.junit.jupiter.api.Test;
import org.mockito.InjectMocks;
import org.mockito.Mock;
import org.mockito.MockitoAnnotations;
import org.mockito.Spy;

import io.vertx.core.Vertx;

public class ReEncumbranceHoldersBuilderTest {

  @InjectMocks
  private ReEncumbranceHoldersBuilder reEncumbranceHoldersBuilder;

  @Mock
  private FundService fundService;
  @Mock
  private LedgerService ledgerService;
  @Mock
  private FiscalYearService fiscalYearService;
  @Mock
  private BudgetService budgetService;
  @Mock
  private RolloverRetrieveService rolloverRetrieveService;
  @Mock
  private ExchangeRateProviderResolver exchangeRateProviderResolver;
  @Spy
  private ManualExchangeRateProvider exchangeRateProvider;
  @Mock
  private ManualCurrencyConversion currencyConversion;
  @Mock
  private TransactionService transactionService;

  @Mock
  private RequestContext requestContext;

  @BeforeEach
  public void initMocks() {
    MockitoAnnotations.openMocks(this);
  }

  @Test
  void shouldCreateNewReEncumbranceHoldersForEveryFundDistributions() {
    FundDistribution fundDistribution1 = new FundDistribution().withFundId(UUID.randomUUID().toString());
    FundDistribution fundDistribution2 = new FundDistribution().withFundId(UUID.randomUUID().toString());
    CompositePoLine compositePoLine1 = new CompositePoLine().withId(UUID.randomUUID().toString())
            .withFundDistribution(Arrays.asList(fundDistribution1, fundDistribution2));
    CompositePoLine compositePoLine2 = new CompositePoLine().withId(UUID.randomUUID().toString());
    CompositePurchaseOrder compPO = new CompositePurchaseOrder().withId(UUID.randomUUID().toString())
            .withCompositePoLines(Arrays.asList(compositePoLine1, compositePoLine2));

    List<ReEncumbranceHolder> resultHolders = reEncumbranceHoldersBuilder.buildReEncumbranceHoldersWithOrdersData(compPO);
    assertThat(resultHolders, hasSize(2));
    assertThat(resultHolders, everyItem(allOf(
      hasProperty("purchaseOrder", is(compPO)),
      hasProperty("poLine", is(compositePoLine1)),
      hasProperty("fundDistribution", isOneOf(fundDistribution1, fundDistribution2))
    )));
  }

  @Test
  void shouldPopulateEachReEncumbranceHolderWithCorrespondingFundIfAllFundsFound() {
    FundDistribution fundDistribution1 = new FundDistribution().withFundId(UUID.randomUUID().toString());
    FundDistribution fundDistribution2 = new FundDistribution().withFundId(UUID.randomUUID().toString());
    ReEncumbranceHolder holder1 = new ReEncumbranceHolder().withFundDistribution(fundDistribution1);
    ReEncumbranceHolder holder2 = new ReEncumbranceHolder().withFundDistribution(fundDistribution2);
    List<ReEncumbranceHolder> holders = Arrays.asList(holder1, holder2);

    Fund fund1 = new Fund().withId(fundDistribution1.getFundId());
    Fund fund2 = new Fund().withId(fundDistribution2.getFundId());

    when(fundService.getFunds(anyCollection(), any())).thenReturn(CompletableFuture.completedFuture(Arrays.asList(fund2, fund1)));


    List<ReEncumbranceHolder> resultHolders = reEncumbranceHoldersBuilder.withFunds(holders, requestContext).join();

    resultHolders.forEach(holder -> assertEquals(holder.getFundDistribution().getFundId(), holder.getFund().getId()));

  }

  @Test
  void shouldPopulateReEncumbranceHolderFundWithNullIfCorrespondingFundsFound() {
    FundDistribution fundDistribution1 = new FundDistribution().withFundId(UUID.randomUUID().toString());
    FundDistribution fundDistribution2 = new FundDistribution().withFundId(UUID.randomUUID().toString());
    ReEncumbranceHolder holder1 = new ReEncumbranceHolder().withFundDistribution(fundDistribution1);
    ReEncumbranceHolder holder2 = new ReEncumbranceHolder().withFundDistribution(fundDistribution2);
    List<ReEncumbranceHolder> holders = Arrays.asList(holder1, holder2);

    Fund fund1 = new Fund().withId(fundDistribution1.getFundId());

    when(fundService.getFunds(anyCollection(), any())).thenReturn(CompletableFuture.completedFuture(Collections.singletonList(fund1)));


    List<ReEncumbranceHolder> resultHolders = reEncumbranceHoldersBuilder.withFunds(holders, requestContext).join();

    assertThat(resultHolders, hasItem(allOf(hasProperty("fund", is(fund1)), hasProperty("fundDistribution", is(fundDistribution1)))));
    assertThat(resultHolders, hasItem(allOf(hasProperty("fund", nullValue()), hasProperty("fundDistribution", is(fundDistribution2)))));


  }

  @Test
  void shouldNotRetrieveFundsIfReEncumbranceHoldersIsEmpty() {

    List<ReEncumbranceHolder> holders = Collections.emptyList();

    List<ReEncumbranceHolder> resultHolders = reEncumbranceHoldersBuilder.withFunds(holders, requestContext).join();
    assertThat(resultHolders, is(empty()));
    verify(fundService, never()).getFunds(anyCollection(), any());

  }

  @Test
  void shouldPopulateEachReEncumbranceHolderContainingFundWithCorrespondingLedger() {

    Fund fund1 = new Fund().withId(UUID.randomUUID().toString()).withLedgerId(UUID.randomUUID().toString());
    Fund fund2 = new Fund().withId(UUID.randomUUID().toString()).withLedgerId(UUID.randomUUID().toString());

    ReEncumbranceHolder holder1 = new ReEncumbranceHolder().withFund(fund1);
    ReEncumbranceHolder holder2 = new ReEncumbranceHolder().withFund(fund2);
    List<ReEncumbranceHolder> holders = Arrays.asList(holder1, holder2);

    Ledger ledger1 = new Ledger().withId(fund1.getLedgerId());
    Ledger ledger2 = new Ledger().withId(fund2.getLedgerId());


    when(ledgerService.getLedgersByIds(anyCollection(), any())).thenReturn(CompletableFuture.completedFuture(Arrays.asList(ledger1, ledger2)));


    List<ReEncumbranceHolder> resultHolders = reEncumbranceHoldersBuilder.withLedgers(holders, requestContext).join();

    resultHolders.forEach(holder -> assertEquals(holder.getFund().getLedgerId(), holder.getLedger().getId()));

  }

  @Test
  void shouldNotRetrieveLedgersIfReEncumbranceHoldersIsEmpty() {

    List<ReEncumbranceHolder> holders = Collections.emptyList();

    List<ReEncumbranceHolder> resultHolders = reEncumbranceHoldersBuilder.withLedgers(holders, requestContext).join();
    assertThat(resultHolders, is(empty()));
    verify(ledgerService, never()).getLedgersByIds(anyCollection(), any());

  }

  @Test
  void shouldNotPopulateLedgerForReEncumbranceHoldersWithoutFund() {
    Fund fund1 = new Fund().withId(UUID.randomUUID().toString()).withLedgerId(UUID.randomUUID().toString());

    ReEncumbranceHolder holder1 = new ReEncumbranceHolder().withFund(fund1);
    ReEncumbranceHolder holder2 = new ReEncumbranceHolder();
    List<ReEncumbranceHolder> holders = Arrays.asList(holder1, holder2);

    Ledger ledger1 = new Ledger().withId(fund1.getLedgerId());

    when(ledgerService.getLedgersByIds(anyCollection(), any())).thenReturn(CompletableFuture.completedFuture(Collections.singletonList(ledger1)));

    List<ReEncumbranceHolder> resultHolders = reEncumbranceHoldersBuilder.withLedgers(holders, requestContext).join();

    assertThat(resultHolders, hasItem(hasProperty("ledger", nullValue())));
    assertThat(resultHolders, hasItem(hasProperty("ledger", is(ledger1))));
  }

  @Test
  void shouldPopulateEachReEncumbranceHolderWithFiscalYearIfFundExist() {

    Fund fund1 = new Fund().withId(UUID.randomUUID().toString()).withLedgerId(UUID.randomUUID().toString());
    Fund fund2 = new Fund().withId(UUID.randomUUID().toString()).withLedgerId(UUID.randomUUID().toString());

    ReEncumbranceHolder holder1 = new ReEncumbranceHolder().withFund(fund1);
    ReEncumbranceHolder holder2 = new ReEncumbranceHolder().withFund(fund2);
    List<ReEncumbranceHolder> holders = Arrays.asList(holder1, holder2);

    FiscalYear fiscalYear = new FiscalYear().withId(UUID.randomUUID().toString());

    when(fiscalYearService.getCurrentFiscalYear(anyString(), any())).thenReturn(CompletableFuture.completedFuture(fiscalYear));

    List<ReEncumbranceHolder> resultHolders = reEncumbranceHoldersBuilder.withCurrentFiscalYear(holders, requestContext).join();

    assertThat(resultHolders, everyItem(hasProperty("currentFiscalYear", is(fiscalYear))));

  }

  @Test
  void shouldNotRetrieveCurrentFiscalYearIfReEncumbranceHoldersIsEmpty() {

    List<ReEncumbranceHolder> holders = Collections.emptyList();

    List<ReEncumbranceHolder> resultHolders = reEncumbranceHoldersBuilder.withCurrentFiscalYear(holders, requestContext).join();
    assertThat(resultHolders, is(empty()));
    verify(fiscalYearService, never()).getCurrentFiscalYear(anyString(), any());

  }

  @Test
  void shouldPopulateCurrentFiscalYearForReEncumbranceHoldersIfAnyHolderHasFund() {
    Fund fund2 = new Fund().withId(UUID.randomUUID().toString()).withLedgerId(UUID.randomUUID().toString());
    Fund fund3 = new Fund().withId(UUID.randomUUID().toString()).withLedgerId(UUID.randomUUID().toString());

    ReEncumbranceHolder holder1 = new ReEncumbranceHolder();
    ReEncumbranceHolder holder2 = new ReEncumbranceHolder().withFund(fund2);
    ReEncumbranceHolder holder3 = new ReEncumbranceHolder().withFund(fund3);
    List<ReEncumbranceHolder> holders = Arrays.asList(holder1, holder2, holder3);

    FiscalYear fiscalYear = new FiscalYear().withId(UUID.randomUUID().toString());

    when(fiscalYearService.getCurrentFiscalYear(anyString(), any())).thenReturn(CompletableFuture.completedFuture(fiscalYear));

    List<ReEncumbranceHolder> resultHolders = reEncumbranceHoldersBuilder.withCurrentFiscalYear(holders, requestContext).join();

    assertThat(resultHolders, everyItem(hasProperty("currentFiscalYear", is(fiscalYear))));
  }

  @Test
  void shouldNotPopulateCurrentFiscalYearForReEncumbranceHoldersIfNoneHolderHasFund() {

    ReEncumbranceHolder holder1 = new ReEncumbranceHolder();
    ReEncumbranceHolder holder2 = new ReEncumbranceHolder();
    ReEncumbranceHolder holder3 = new ReEncumbranceHolder();
    List<ReEncumbranceHolder> holders = Arrays.asList(holder1, holder2, holder3);

    List<ReEncumbranceHolder> resultHolders = reEncumbranceHoldersBuilder.withCurrentFiscalYear(holders, requestContext).join();

    assertThat(resultHolders, everyItem(hasProperty("currentFiscalYear", nullValue())));
    verify(fiscalYearService, never()).getCurrentFiscalYear(anyString(), any());
  }

  @Test
  void shouldPopulateReEncumbranceHoldersWithCorrespondingBudgetsForEveryHolderWithFund() {
    Fund fund1 = new Fund().withId(UUID.randomUUID().toString()).withLedgerId(UUID.randomUUID().toString());
    Fund fund2 = new Fund().withId(UUID.randomUUID().toString()).withLedgerId(UUID.randomUUID().toString());

    ReEncumbranceHolder holder1 = new ReEncumbranceHolder().withFund(fund1);
    ReEncumbranceHolder holder2 = new ReEncumbranceHolder().withFund(fund2);
    ReEncumbranceHolder holder3 = new ReEncumbranceHolder();
    List<ReEncumbranceHolder> holders = Arrays.asList(holder1, holder2, holder3);

    Budget budget1 = new Budget().withId(UUID.randomUUID().toString()).withFundId(fund1.getId());
    Budget budget2 = new Budget().withId(UUID.randomUUID().toString()).withFundId(fund2.getId());

    when(budgetService.fetchBudgetsByFundIds(anyList(), any())).thenReturn(CompletableFuture.completedFuture(Arrays.asList(budget1, budget2)));

    List<ReEncumbranceHolder> resultHolders = reEncumbranceHoldersBuilder.withBudgets(holders, requestContext).join();

    assertThat(resultHolders, hasItem(allOf(hasProperty("budget", is(budget1)), hasProperty("fund", is(fund1)))));
    assertThat(resultHolders, hasItem(allOf(hasProperty("budget", is(budget2)), hasProperty("fund", is(fund2)))));
    assertThat(resultHolders, hasItem(allOf(hasProperty("budget", nullValue()), hasProperty("fund", nullValue()))));
  }

  @Test
  void shouldNotRetrieveActiveBudgetIfReEncumbranceHoldersIsEmpty() {

    List<ReEncumbranceHolder> holders = Collections.emptyList();

    List<ReEncumbranceHolder> resultHolders = reEncumbranceHoldersBuilder.withBudgets(holders, requestContext).join();
    assertThat(resultHolders, is(empty()));
    verify(budgetService, never()).fetchBudgetsByFundIds(anyList(), any());

  }

  @Test
  void shouldPopulateReEncumbranceHoldersWithCorrespondingRollover() {

    String ledgerId1 = UUID.randomUUID().toString();
    String ledgerId2 = UUID.randomUUID().toString();

    Fund fund1 = new Fund().withId(UUID.randomUUID().toString()).withLedgerId(ledgerId1);
    Fund fund2 = new Fund().withId(UUID.randomUUID().toString()).withLedgerId(ledgerId2);
    Fund fund3 = new Fund().withId(UUID.randomUUID().toString()).withLedgerId(ledgerId2);
    Fund fund4 = new Fund().withId(UUID.randomUUID().toString()).withLedgerId(UUID.randomUUID().toString());

    FiscalYear currentFiscalYear = new FiscalYear().withId(UUID.randomUUID().toString());

    ReEncumbranceHolder holder1 = new ReEncumbranceHolder().withFund(fund1).withCurrentFiscalYear(currentFiscalYear);
    ReEncumbranceHolder holder2 = new ReEncumbranceHolder().withFund(fund2).withCurrentFiscalYear(currentFiscalYear);
    ReEncumbranceHolder holder3 = new ReEncumbranceHolder().withFund(fund3).withCurrentFiscalYear(currentFiscalYear);
    ReEncumbranceHolder holder4 = new ReEncumbranceHolder().withFund(fund4).withCurrentFiscalYear(currentFiscalYear);
    List<ReEncumbranceHolder> holders = Arrays.asList(holder1, holder2, holder3, holder4);

    LedgerFiscalYearRollover rollover1 = new LedgerFiscalYearRollover().withLedgerId(ledgerId1);
    LedgerFiscalYearRollover rollover2 = new LedgerFiscalYearRollover().withLedgerId(ledgerId2);


    when(rolloverRetrieveService.getLedgerFyRollovers(anyString(), anyList(), any())).thenReturn(CompletableFuture.completedFuture(Arrays.asList(rollover1, rollover2)));

    List<ReEncumbranceHolder> resultHolders = reEncumbranceHoldersBuilder.withRollovers(holders, requestContext).join();

    assertThat(resultHolders, hasItem(allOf(hasProperty("rollover", is(rollover1)), hasProperty("fund", is(fund1)))));
    assertThat(resultHolders, hasItem(allOf(hasProperty("rollover", is(rollover2)), hasProperty("fund", is(fund2)))));
    assertThat(resultHolders, hasItem(allOf(hasProperty("rollover", is(rollover2)), hasProperty("fund", is(fund3)))));
    assertThat(resultHolders, hasItem(allOf(hasProperty("rollover", nullValue()), hasProperty("fund", is(fund4)))));
  }

  @Test
  void shouldNotRetrieveRolloversIfReEncumbranceHoldersCurrentFiscalYearNull() {

    String ledgerId1 = UUID.randomUUID().toString();
    String ledgerId2 = UUID.randomUUID().toString();

    Fund fund1 = new Fund().withId(UUID.randomUUID().toString()).withLedgerId(ledgerId1);
    Fund fund2 = new Fund().withId(UUID.randomUUID().toString()).withLedgerId(ledgerId2);
    Fund fund3 = new Fund().withId(UUID.randomUUID().toString()).withLedgerId(ledgerId2);
    Fund fund4 = new Fund().withId(UUID.randomUUID().toString()).withLedgerId(UUID.randomUUID().toString());


    ReEncumbranceHolder holder1 = new ReEncumbranceHolder().withFund(fund1);
    ReEncumbranceHolder holder2 = new ReEncumbranceHolder().withFund(fund2);
    ReEncumbranceHolder holder3 = new ReEncumbranceHolder().withFund(fund3);
    ReEncumbranceHolder holder4 = new ReEncumbranceHolder().withFund(fund4);
    List<ReEncumbranceHolder> holders = Arrays.asList(holder1, holder2, holder3, holder4);

    List<ReEncumbranceHolder> resultHolders = reEncumbranceHoldersBuilder.withRollovers(holders, requestContext).join();

    assertThat(resultHolders, hasItem(allOf(hasProperty("rollover", nullValue()), hasProperty("fund", is(fund1)))));
    assertThat(resultHolders, hasItem(allOf(hasProperty("rollover", nullValue()), hasProperty("fund", is(fund2)))));
    assertThat(resultHolders, hasItem(allOf(hasProperty("rollover", nullValue()), hasProperty("fund", is(fund3)))));
    assertThat(resultHolders, hasItem(allOf(hasProperty("rollover", nullValue()), hasProperty("fund", is(fund4)))));

    verify(rolloverRetrieveService, never()).getLedgerFyRollovers(anyString(), anyList(), any());
  }

  @Test
  void shouldNotRetrieveRolloversIfReEncumbranceHoldersCurrentFundNull() {

    ReEncumbranceHolder holder1 = new ReEncumbranceHolder();


    List<ReEncumbranceHolder> holders = Collections.singletonList(holder1);

    List<ReEncumbranceHolder> resultHolders = reEncumbranceHoldersBuilder.withRollovers(holders, requestContext).join();

    assertThat(resultHolders, hasItem(allOf(hasProperty("rollover", nullValue()), hasProperty("fund"))));
    assertThat(resultHolders, hasItem(allOf(hasProperty("rollover", nullValue()), hasProperty("fund"))));


    verify(rolloverRetrieveService, never()).getLedgerFyRollovers(anyString(), anyList(), any());
  }

  @Test
  void shouldPopulateReEncumbranceHoldersWithConversionWhenWhenHoldersContainsCurrentFiscalYear() {

    FiscalYear fiscalYear = new FiscalYear().withCurrency("USD");
    CompositePoLine line1 = new CompositePoLine().withCost(new Cost().withCurrency("EUR"));
    CompositePoLine line2 = new CompositePoLine().withCost(new Cost().withCurrency("EUR"));
    double exchangeEurToUsdRate = 1.1d;
    double exchangeUsdToEurRate = 0.909d;

    ReEncumbranceHolder holder1 = new ReEncumbranceHolder().withCurrentFiscalYear(fiscalYear).withPoLine(line1);
    ReEncumbranceHolder holder2 = new ReEncumbranceHolder().withCurrentFiscalYear(fiscalYear).withPoLine(line2);

    ConversionQuery conversionPoLineToFyQuery = ConversionQueryBuilder.of()
      .setBaseCurrency("EUR").setTermCurrency("USD").set(ExchangeRateProviderResolver.RATE_KEY, exchangeEurToUsdRate).build();
    ConversionQuery conversionFyToPoLineQuery = ConversionQueryBuilder.of()
      .setBaseCurrency("USD").setTermCurrency("EUR").set(ExchangeRateProviderResolver.RATE_KEY, exchangeUsdToEurRate).build();

    CurrencyConversion poLineToFyConversion = exchangeRateProvider.getCurrencyConversion(conversionPoLineToFyQuery);
    CurrencyConversion poFyToPoLineConversion = exchangeRateProvider.getCurrencyConversion(conversionFyToPoLineQuery);

    List<ReEncumbranceHolder> holders = Arrays.asList(holder1, holder2);
    when(exchangeRateProviderResolver.resolve(any(), any())).thenReturn(exchangeRateProvider);
    when(exchangeRateProviderResolver.resolve(conversionPoLineToFyQuery, requestContext)).thenReturn(exchangeRateProvider);
    when(exchangeRateProviderResolver.resolve(conversionFyToPoLineQuery, requestContext)).thenReturn(exchangeRateProvider);
    when(exchangeRateProvider.getCurrencyConversion(conversionPoLineToFyQuery)).thenReturn(poLineToFyConversion);
    when(exchangeRateProvider.getCurrencyConversion(conversionFyToPoLineQuery)).thenReturn(poFyToPoLineConversion);

    when(requestContext.getContext()).thenReturn(Vertx.vertx().getOrCreateContext());

    List<ReEncumbranceHolder> resultHolders = reEncumbranceHoldersBuilder.withConversions(holders, requestContext).join();

    assertEquals(resultHolders.get(0).getPoLineToFyConversion().getCurrency(), poLineToFyConversion.getCurrency());
    assertEquals(resultHolders.get(1).getPoLineToFyConversion().getCurrency(), poLineToFyConversion.getCurrency());
    assertEquals(resultHolders.get(0).getFyToPoLineConversion().getCurrency(), poFyToPoLineConversion.getCurrency());
    assertEquals(resultHolders.get(1).getFyToPoLineConversion().getCurrency(), poFyToPoLineConversion.getCurrency());
  }

  @Test
  void shouldNotResolveExchangeRateProviderWhenReEncumbranceHoldersEmpty() {

    List<ReEncumbranceHolder> holders = Collections.emptyList();

    List<ReEncumbranceHolder> resultHolders = reEncumbranceHoldersBuilder.withConversions(holders, requestContext).join();

    assertThat(resultHolders, is(empty()));
    verify(exchangeRateProviderResolver, never()).resolve(any(), any());

  }

  @Test
  void shouldNotResolveExchangeRateProviderWhenReEncumbranceHoldersNotContainsCurrentFiscalYear() {

    List<ReEncumbranceHolder> holders = Arrays.asList(new ReEncumbranceHolder(), new ReEncumbranceHolder());

    List<ReEncumbranceHolder> resultHolders = reEncumbranceHoldersBuilder.withConversions(holders, requestContext).join();

    assertEquals(resultHolders, holders);
    verify(exchangeRateProviderResolver, never()).resolve(any(), any());

  }

  @Test
  void shouldPopulateReEncumbranceHoldersWithOneTimeEncumbranceRolloverWhenOrderTypeOneTime() {


    EncumbranceRollover oneTimeEncumbranceRollover = new EncumbranceRollover()
            .withBasedOn(EncumbranceRollover.BasedOn.REMAINING)
            .withOrderType(EncumbranceRollover.OrderType.ONE_TIME)
            .withIncreaseBy(10d);

    EncumbranceRollover ongoingEncumbranceRollover = new EncumbranceRollover()
            .withBasedOn(EncumbranceRollover.BasedOn.EXPENDED)
            .withOrderType(EncumbranceRollover.OrderType.ONGOING)
            .withIncreaseBy(10d);

    EncumbranceRollover subscriptionEncumbranceRollover = new EncumbranceRollover()
            .withBasedOn(EncumbranceRollover.BasedOn.REMAINING)
            .withOrderType(EncumbranceRollover.OrderType.ONGOING_SUBSCRIPTION)
            .withIncreaseBy(10d);


    CompositePurchaseOrder compPO = new CompositePurchaseOrder().withOrderType(CompositePurchaseOrder.OrderType.ONE_TIME);
    LedgerFiscalYearRollover rollover = new LedgerFiscalYearRollover().withEncumbrancesRollover(Arrays.asList(oneTimeEncumbranceRollover, ongoingEncumbranceRollover, subscriptionEncumbranceRollover));

    ReEncumbranceHolder holder1 = new ReEncumbranceHolder()
            .withPurchaseOrder(compPO)
            .withRollover(rollover);
    ReEncumbranceHolder holder2 = new ReEncumbranceHolder()
            .withPurchaseOrder(compPO)
            .withRollover(rollover);

    List<ReEncumbranceHolder> holders = Arrays.asList(holder1, holder2);
    List<ReEncumbranceHolder> resultHolders = reEncumbranceHoldersBuilder.withEncumbranceRollover(holders);

    assertThat(resultHolders, everyItem(hasProperty("encumbranceRollover", is(oneTimeEncumbranceRollover))));
  }

  @Test
  void shouldPopulateReEncumbranceHoldersWithOngoingEncumbranceRolloverWhenOrderTypeOngoingAndSubscriptionFalse() {

    EncumbranceRollover oneTimeEncumbranceRollover = new EncumbranceRollover()
            .withBasedOn(EncumbranceRollover.BasedOn.REMAINING)
            .withOrderType(EncumbranceRollover.OrderType.ONE_TIME)
            .withIncreaseBy(10d);

    EncumbranceRollover ongoingEncumbranceRollover = new EncumbranceRollover()
            .withBasedOn(EncumbranceRollover.BasedOn.EXPENDED)
            .withOrderType(EncumbranceRollover.OrderType.ONGOING)
            .withIncreaseBy(10d);

    EncumbranceRollover subscriptionEncumbranceRollover = new EncumbranceRollover()
            .withBasedOn(EncumbranceRollover.BasedOn.REMAINING)
            .withOrderType(EncumbranceRollover.OrderType.ONGOING_SUBSCRIPTION)
            .withIncreaseBy(10d);

    CompositePurchaseOrder compPO = new CompositePurchaseOrder().withOrderType(CompositePurchaseOrder.OrderType.ONE_TIME);
    LedgerFiscalYearRollover rollover = new LedgerFiscalYearRollover().withEncumbrancesRollover(Arrays.asList(oneTimeEncumbranceRollover, ongoingEncumbranceRollover, subscriptionEncumbranceRollover));

    ReEncumbranceHolder holder1 = new ReEncumbranceHolder()
            .withPurchaseOrder(compPO)
            .withRollover(rollover);
    ReEncumbranceHolder holder2 = new ReEncumbranceHolder()
            .withPurchaseOrder(compPO)
            .withRollover(rollover);

    List<ReEncumbranceHolder> holders = Arrays.asList(holder1, holder2);
    List<ReEncumbranceHolder> resultHolders = reEncumbranceHoldersBuilder.withEncumbranceRollover(holders);

    assertThat(resultHolders, everyItem(hasProperty("encumbranceRollover", is(oneTimeEncumbranceRollover))));
  }

  @Test
  void shouldPopulateReEncumbranceHoldersWithOngoingSubscriptionEncumbranceRolloverWhenOrderTypeOngoingAndSubscriptionTrue() {

    EncumbranceRollover oneTimeEncumbranceRollover = new EncumbranceRollover()
            .withBasedOn(EncumbranceRollover.BasedOn.REMAINING)
            .withOrderType(EncumbranceRollover.OrderType.ONE_TIME)
            .withIncreaseBy(10d);

    EncumbranceRollover ongoingEncumbranceRollover = new EncumbranceRollover()
            .withBasedOn(EncumbranceRollover.BasedOn.EXPENDED)
            .withOrderType(EncumbranceRollover.OrderType.ONGOING)
            .withIncreaseBy(10d);

    EncumbranceRollover subscriptionEncumbranceRollover = new EncumbranceRollover()
            .withBasedOn(EncumbranceRollover.BasedOn.REMAINING)
            .withOrderType(EncumbranceRollover.OrderType.ONGOING_SUBSCRIPTION)
            .withIncreaseBy(10d);

    CompositePurchaseOrder compPO = new CompositePurchaseOrder().withOrderType(CompositePurchaseOrder.OrderType.ONGOING).withOngoing(new Ongoing().withIsSubscription(true));
    LedgerFiscalYearRollover rollover = new LedgerFiscalYearRollover().withEncumbrancesRollover(Arrays.asList(oneTimeEncumbranceRollover, ongoingEncumbranceRollover, subscriptionEncumbranceRollover));

    ReEncumbranceHolder holder1 = new ReEncumbranceHolder()
            .withPurchaseOrder(compPO)
            .withRollover(rollover);
    ReEncumbranceHolder holder2 = new ReEncumbranceHolder()
            .withPurchaseOrder(compPO)
            .withRollover(rollover);

    List<ReEncumbranceHolder> holders = Arrays.asList(holder1, holder2);
    List<ReEncumbranceHolder> resultHolders = reEncumbranceHoldersBuilder.withEncumbranceRollover(holders);

    assertThat(resultHolders, everyItem(hasProperty("encumbranceRollover", is(subscriptionEncumbranceRollover))));
  }

  @Test
  void shouldNotPopulateReEncumbranceHoldersWithEncumbranceRolloverWhenEncumbranceRolloversNotContainsItemWithOrderType() {

    EncumbranceRollover oneTimeEncumbranceRollover = new EncumbranceRollover()
            .withBasedOn(EncumbranceRollover.BasedOn.REMAINING)
            .withOrderType(EncumbranceRollover.OrderType.ONE_TIME)
            .withIncreaseBy(10d);

    EncumbranceRollover ongoingEncumbranceRollover = new EncumbranceRollover()
            .withBasedOn(EncumbranceRollover.BasedOn.EXPENDED)
            .withOrderType(EncumbranceRollover.OrderType.ONGOING)
            .withIncreaseBy(10d);


    CompositePurchaseOrder compPO = new CompositePurchaseOrder().withOrderType(CompositePurchaseOrder.OrderType.ONGOING).withOngoing(new Ongoing().withIsSubscription(true));
    LedgerFiscalYearRollover rollover = new LedgerFiscalYearRollover().withEncumbrancesRollover(Arrays.asList(oneTimeEncumbranceRollover, ongoingEncumbranceRollover));

    ReEncumbranceHolder holder1 = new ReEncumbranceHolder()
            .withPurchaseOrder(compPO)
            .withRollover(rollover);
    ReEncumbranceHolder holder2 = new ReEncumbranceHolder()
            .withPurchaseOrder(compPO)
            .withRollover(rollover);

    List<ReEncumbranceHolder> holders = Arrays.asList(holder1, holder2);
    List<ReEncumbranceHolder> resultHolders = reEncumbranceHoldersBuilder.withEncumbranceRollover(holders);

    assertThat(resultHolders, everyItem(hasProperty("encumbranceRollover", nullValue())));
  }

  @Test
  void shouldPopulateReEncumbranceHoldersWithFromFyEncumbrances() {
    String fromFyId = UUID.randomUUID().toString();
    String toFyId = UUID.randomUUID().toString();
    String orderId = UUID.randomUUID().toString();
    String fund1Id = UUID.randomUUID().toString();
    String fund2Id = UUID.randomUUID().toString();

    EncumbranceRollover encumbranceRollover = new EncumbranceRollover()
            .withBasedOn(EncumbranceRollover.BasedOn.REMAINING)
            .withOrderType(EncumbranceRollover.OrderType.ONE_TIME)
            .withIncreaseBy(10d);

    LedgerFiscalYearRollover rollover = new LedgerFiscalYearRollover()
            .withFromFiscalYearId(fromFyId)
            .withToFiscalYearId(toFyId)
            .withEncumbrancesRollover(Collections.singletonList(encumbranceRollover));

    CompositePurchaseOrder compPO = new CompositePurchaseOrder().withId(orderId).withOrderType(CompositePurchaseOrder.OrderType.ONE_TIME);

    Transaction fromEncumbrance1 = new Transaction().withEncumbrance(new Encumbrance().withSourcePurchaseOrderId(orderId))
            .withAmount(10d)
            .withFiscalYearId(fromFyId)
            .withFromFundId(fund1Id)
            .withId(UUID.randomUUID().toString())
            .withCurrency("USD");

    Transaction fromEncumbrance2 = new Transaction().withEncumbrance(new Encumbrance()
              .withSourcePurchaseOrderId(orderId))
            .withAmount(100d)
            .withFiscalYearId(fromFyId)
            .withFromFundId(fund2Id)
            .withId(UUID.randomUUID().toString())
            .withCurrency("USD");

    FundDistribution fundDistribution1 = new FundDistribution()
            .withFundId(fund1Id)
            .withEncumbrance(fromEncumbrance1.getId());

    FundDistribution fundDistribution2 = new FundDistribution()
            .withFundId(fund2Id)
            .withEncumbrance(fromEncumbrance2.getId());

    CompositePoLine line1 = new CompositePoLine().withId(UUID.randomUUID().toString())
            .withFundDistribution(Collections.singletonList(fundDistribution1));

    CompositePoLine line2 = new CompositePoLine().withId(UUID.randomUUID().toString())
            .withFundDistribution(Collections.singletonList(fundDistribution2));

    Transaction toEncumbrance1 = new Transaction().withEncumbrance(new Encumbrance()
              .withSourcePurchaseOrderId(orderId).withSourcePoLineId(line1.getId()))
            .withId(UUID.randomUUID().toString())
            .withFiscalYearId(toFyId)
            .withFromFundId(fund1Id)
            .withCurrency("USD");

    ReEncumbranceHolder holder1 = new ReEncumbranceHolder()
            .withPurchaseOrder(compPO)
            .withRollover(rollover)
            .withPoLine(line1)
            .withFundDistribution(fundDistribution1)
            .withEncumbranceRollover(encumbranceRollover);
    ReEncumbranceHolder holder2 = new ReEncumbranceHolder()
            .withPurchaseOrder(compPO)
            .withRollover(rollover)
            .withPoLine(line2)
            .withFundDistribution(fundDistribution2)
            .withEncumbranceRollover(encumbranceRollover);

    List<ReEncumbranceHolder> holders = Arrays.asList(holder1, holder2);

    TransactionCollection fromTransactionCollection = new TransactionCollection().withTransactions(Arrays.asList(fromEncumbrance1, fromEncumbrance2)).withTotalRecords(2);
    TransactionCollection toTransactionCollection = new TransactionCollection().withTransactions(Collections.singletonList(toEncumbrance1)).withTotalRecords(1);

    when(transactionService.getTransactions(contains(fromFyId), anyInt(), anyInt(), any())).thenReturn(CompletableFuture.completedFuture(fromTransactionCollection));
    when(transactionService.getTransactions(contains(toFyId), anyInt(), anyInt(), any())).thenReturn(CompletableFuture.completedFuture(toTransactionCollection));

    List<ReEncumbranceHolder> resultHolders = reEncumbranceHoldersBuilder.withFromEncumbrances(holders, requestContext).join();

    assertThat(resultHolders, hasItem(allOf(hasProperty("fromFYEncumbrance", is(fromEncumbrance1)))));
    assertThat(resultHolders, hasItem(allOf(hasProperty("fromFYEncumbrance", is(fromEncumbrance2)))));

  }

  @Test
  void shouldPopulateReEncumbranceHoldersWithFromFyEncumbrancesWhenBasedOnExpended() {
    String fromFyId = UUID.randomUUID().toString();
    String toFyId = UUID.randomUUID().toString();
    String orderId = UUID.randomUUID().toString();
    String fund1Id = UUID.randomUUID().toString();
    String fund2Id = UUID.randomUUID().toString();

    EncumbranceRollover encumbranceRollover = new EncumbranceRollover()
            .withBasedOn(EncumbranceRollover.BasedOn.EXPENDED)
            .withOrderType(EncumbranceRollover.OrderType.ONE_TIME)
            .withIncreaseBy(0d);

    LedgerFiscalYearRollover rollover = new LedgerFiscalYearRollover()
            .withFromFiscalYearId(fromFyId)
            .withToFiscalYearId(toFyId)
            .withEncumbrancesRollover(Collections.singletonList(encumbranceRollover));

    CompositePurchaseOrder compPO = new CompositePurchaseOrder().withId(orderId).withOrderType(CompositePurchaseOrder.OrderType.ONE_TIME);

    Transaction fromEncumbrance1 = new Transaction().withEncumbrance(new Encumbrance()
              .withSourcePurchaseOrderId(orderId)
              .withAmountExpended(20d))
            .withAmount(10d)
            .withFiscalYearId(fromFyId)
            .withFromFundId(fund1Id)
            .withId(UUID.randomUUID().toString())
            .withCurrency("USD");

    Transaction fromEncumbrance2 = new Transaction().withEncumbrance(new Encumbrance()
              .withSourcePurchaseOrderId(orderId)
              .withAmountExpended(10d))
            .withAmount(100d)
            .withFiscalYearId(fromFyId)
            .withFromFundId(fund2Id)
            .withId(UUID.randomUUID().toString())
            .withCurrency("USD");

    FundDistribution fundDistribution1 = new FundDistribution()
            .withFundId(fund1Id)
            .withEncumbrance(fromEncumbrance1.getId());

    FundDistribution fundDistribution2 = new FundDistribution()
            .withFundId(fund2Id)
            .withEncumbrance(fromEncumbrance2.getId());

    CompositePoLine line1 = new CompositePoLine().withId(UUID.randomUUID().toString())
            .withFundDistribution(Collections.singletonList(fundDistribution1));

    CompositePoLine line2 = new CompositePoLine().withId(UUID.randomUUID().toString())
            .withFundDistribution(Collections.singletonList(fundDistribution2));


    ReEncumbranceHolder holder1 = new ReEncumbranceHolder()
            .withPurchaseOrder(compPO)
            .withRollover(rollover)
            .withPoLine(line1)
            .withFundDistribution(fundDistribution1)
            .withEncumbranceRollover(encumbranceRollover);
    ReEncumbranceHolder holder2 = new ReEncumbranceHolder()
            .withPurchaseOrder(compPO)
            .withRollover(rollover)
            .withPoLine(line2)
            .withFundDistribution(fundDistribution2)
            .withEncumbranceRollover(encumbranceRollover);

    List<ReEncumbranceHolder> holders = Arrays.asList(holder1, holder2);

    TransactionCollection fromTransactionCollection = new TransactionCollection().withTransactions(Arrays.asList(fromEncumbrance1, fromEncumbrance2)).withTotalRecords(2);
    TransactionCollection toTransactionCollection = new TransactionCollection().withTransactions(Collections.emptyList()).withTotalRecords(0);

    when(transactionService.getTransactions(contains(fromFyId), anyInt(), anyInt(), any())).thenReturn(CompletableFuture.completedFuture(fromTransactionCollection));
    when(transactionService.getTransactions(contains(toFyId), anyInt(), anyInt(), any())).thenReturn(CompletableFuture.completedFuture(toTransactionCollection));

    List<ReEncumbranceHolder> resultHolders = reEncumbranceHoldersBuilder.withFromEncumbrances(holders, requestContext).join();

    assertThat(resultHolders, hasItem(allOf(hasProperty("fromFYEncumbrance", is(fromEncumbrance1)))));

    assertThat(resultHolders, hasItem(allOf(hasProperty("fromFYEncumbrance", is(fromEncumbrance2)))));
  }


  @Test
  void shouldPopulateReEncumbranceHoldersWithToFyEncumbrancesWhenBasedOnExpended() {
    String fromFyId = UUID.randomUUID().toString();
    String toFyId = UUID.randomUUID().toString();
    String orderId = UUID.randomUUID().toString();
    String fund1Id = UUID.randomUUID().toString();
    String fund2Id = UUID.randomUUID().toString();
    double exchangeEurToUsdRate = 1.1d;
    double exchangeUsdToEurRate = 0.909d;

    EncumbranceRollover encumbranceRollover = new EncumbranceRollover()
                  .withBasedOn(EncumbranceRollover.BasedOn.EXPENDED)
                  .withOrderType(EncumbranceRollover.OrderType.ONE_TIME)
                  .withIncreaseBy(10d);

    LedgerFiscalYearRollover rollover = new LedgerFiscalYearRollover()
            .withFromFiscalYearId(fromFyId)
            .withToFiscalYearId(toFyId)
            .withEncumbrancesRollover(Collections.singletonList(encumbranceRollover));

    CompositePurchaseOrder compPO = new CompositePurchaseOrder().withId(orderId).withOrderType(CompositePurchaseOrder.OrderType.ONE_TIME);
    Cost cost1 = new Cost().withListUnitPrice(120d).withQuantityPhysical(1)
      .withExchangeRate(exchangeEurToUsdRate).withCurrency("EUR").withPoLineEstimatedPrice(120d).withFyroAdjustmentAmount(-35.3d);

    Transaction fromEncumbrance1 = new Transaction().withEncumbrance(new Encumbrance()
              .withSourcePurchaseOrderId(orderId)
              .withInitialAmountEncumbered(60d)
              .withAmountExpended(40d))
              .withAmount(20d)
              .withFiscalYearId(fromFyId)
              .withFromFundId(fund1Id)
              .withId(UUID.randomUUID().toString())
              .withCurrency("USD");

    Transaction fromEncumbrance2 = new Transaction().withEncumbrance(new Encumbrance()
              .withSourcePurchaseOrderId(orderId)
              .withInitialAmountEncumbered(60d)
              .withAmountExpended(30d))
              .withAmount(30d)
              .withFiscalYearId(fromFyId)
              .withFromFundId(fund2Id)
              .withId(UUID.randomUUID().toString())
              .withCurrency("USD");

    FundDistribution fundDistribution1 = new FundDistribution()
      .withDistributionType(FundDistribution.DistributionType.PERCENTAGE)
      .withValue(50d)
      .withEncumbrance(fromEncumbrance1.getId())
      .withFundId(fund1Id);

    FundDistribution fundDistribution2 = new FundDistribution()
      .withDistributionType(FundDistribution.DistributionType.AMOUNT)
      .withValue(42.35d)
      .withEncumbrance(fromEncumbrance2.getId())
      .withFundId(fund2Id);

    CompositePoLine line1 = new CompositePoLine().withId(UUID.randomUUID().toString())
      .withCost(cost1)
      .withFundDistribution(List.of(fundDistribution1, fundDistribution2));

    ConversionQuery conversionPoLineToFyQuery = ConversionQueryBuilder.of()
      .setBaseCurrency("EUR").setTermCurrency("USD").set(ExchangeRateProviderResolver.RATE_KEY, exchangeEurToUsdRate).build();
    ConversionQuery conversionFyToPoLineQuery = ConversionQueryBuilder.of()
      .setBaseCurrency("USD").setTermCurrency("EUR").set(ExchangeRateProviderResolver.RATE_KEY, exchangeUsdToEurRate).build();

     TransactionCollection toTransactionCollection = new TransactionCollection().withTransactions(Collections.emptyList()).withTotalRecords(0);

    ReEncumbranceHolder holder1 = new ReEncumbranceHolder()
            .withPurchaseOrder(compPO)
            .withRollover(rollover)
            .withPoLine(line1)
            .withFundDistribution(fundDistribution1)
            .withEncumbranceRollover(encumbranceRollover)
            .withFromFYEncumbrance(fromEncumbrance1)
            .withPoLineToFyConversion(exchangeRateProvider.getCurrencyConversion(conversionPoLineToFyQuery))
            .withFyToPoLineConversion(exchangeRateProvider.getCurrencyConversion(conversionFyToPoLineQuery));
    ReEncumbranceHolder holder2 = new ReEncumbranceHolder()
            .withPurchaseOrder(compPO)
            .withRollover(rollover)
            .withPoLine(line1)
            .withFundDistribution(fundDistribution2)
            .withEncumbranceRollover(encumbranceRollover)
            .withFromFYEncumbrance(fromEncumbrance2)
            .withPoLineToFyConversion(exchangeRateProvider.getCurrencyConversion(conversionPoLineToFyQuery))
            .withFyToPoLineConversion(exchangeRateProvider.getCurrencyConversion(conversionFyToPoLineQuery));

    List<ReEncumbranceHolder> holders = Arrays.asList(holder1, holder2);

    when(transactionService.getTransactions(contains(toFyId), anyInt(), anyInt(), any())).thenReturn(CompletableFuture.completedFuture(toTransactionCollection));
    when(exchangeRateProviderResolver.resolve(conversionPoLineToFyQuery, requestContext)).thenReturn(exchangeRateProvider);
    when(exchangeRateProviderResolver.resolve(conversionFyToPoLineQuery, requestContext)).thenReturn(exchangeRateProvider);

    List<ReEncumbranceHolder> resultHolders = reEncumbranceHoldersBuilder.withToEncumbrances(holders, requestContext).join();

    assertThat(resultHolders, hasItem(allOf(hasProperty("toFYEncumbrance", allOf(
            hasProperty("id", nullValue()),
            hasProperty("currency", is(fromEncumbrance1.getCurrency())),
            hasProperty("fromFundId", is(fromEncumbrance1.getFromFundId())),
            hasProperty("amount", is(46.585d)),
            hasProperty("fiscalYearId", is(toFyId))
    )))));

    assertThat(resultHolders, hasItem(allOf(hasProperty("toFYEncumbrance", allOf(
                    hasProperty("id", nullValue()),
                    hasProperty("currency", is(fromEncumbrance2.getCurrency())),
                    hasProperty("fromFundId", is(fromEncumbrance2.getFromFundId())),
                    hasProperty("amount", is(46.585d)),
                    hasProperty("fiscalYearId", is(toFyId))
            )))));
  }
}
