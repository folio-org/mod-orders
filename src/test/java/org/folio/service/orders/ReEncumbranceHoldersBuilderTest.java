package org.folio.service.orders;

import static org.hamcrest.MatcherAssert.assertThat;
import static org.hamcrest.Matchers.allOf;
import static org.hamcrest.Matchers.everyItem;
import static org.hamcrest.Matchers.hasItem;
import static org.hamcrest.Matchers.hasProperty;
import static org.hamcrest.Matchers.hasSize;
import static org.hamcrest.Matchers.is;
import static org.hamcrest.Matchers.nullValue;
import static org.hamcrest.Matchers.oneOf;
import static org.junit.jupiter.api.Assertions.assertEquals;
import static org.mockito.ArgumentMatchers.any;
import static org.mockito.ArgumentMatchers.anyList;
import static org.mockito.ArgumentMatchers.anyString;
import static org.mockito.ArgumentMatchers.contains;
import static org.mockito.Mockito.mock;
import static org.mockito.Mockito.never;
import static org.mockito.Mockito.verify;
import static org.mockito.Mockito.when;
import static org.mockito.Mockito.withSettings;

import java.util.Arrays;
import java.util.Collections;
import java.util.List;
import java.util.UUID;

import javax.money.Monetary;
import javax.money.MonetaryAmount;
import javax.money.convert.ConversionQuery;
import javax.money.convert.CurrencyConversion;
import javax.money.convert.ExchangeRate;

import org.folio.models.ReEncumbranceHolder;
import org.folio.rest.acq.model.finance.Encumbrance;
import org.folio.rest.acq.model.finance.FiscalYear;
import org.folio.rest.acq.model.finance.Fund;
import org.folio.rest.acq.model.finance.Transaction;
import org.folio.rest.core.models.RequestContext;
import org.folio.rest.jaxrs.model.CompositePoLine;
import org.folio.rest.jaxrs.model.CompositePurchaseOrder;
import org.folio.rest.jaxrs.model.Cost;
import org.folio.rest.jaxrs.model.EncumbranceRollover;
import org.folio.rest.jaxrs.model.FundDistribution;
import org.folio.rest.jaxrs.model.LedgerFiscalYearRollover;
import org.folio.rest.jaxrs.model.Ongoing;
import org.folio.service.FundsDistributionService;
import org.folio.service.exchange.ExchangeRateProviderResolver;
import org.folio.service.exchange.ManualCurrencyConversion;
import org.folio.service.exchange.ManualExchangeRateProvider;
import org.folio.service.finance.rollover.LedgerRolloverService;
import org.folio.service.finance.transaction.TransactionService;
import org.javamoney.moneta.Money;
import org.javamoney.moneta.spi.DefaultNumberValue;
import org.junit.jupiter.api.AfterEach;
import org.junit.jupiter.api.BeforeEach;
import org.junit.jupiter.api.Test;
import org.junit.jupiter.api.extension.ExtendWith;
import org.mockito.InjectMocks;
import org.mockito.Mock;
import org.mockito.MockitoAnnotations;

import io.vertx.core.Future;
import io.vertx.core.Vertx;
import io.vertx.junit5.VertxExtension;
import io.vertx.junit5.VertxTestContext;
import org.mockito.Spy;


@ExtendWith(VertxExtension.class)
public class ReEncumbranceHoldersBuilderTest {

  @InjectMocks
  private ReEncumbranceHoldersBuilder reEncumbranceHoldersBuilder;

  @Mock
  private LedgerRolloverService ledgerRolloverService;
  @Mock
  private ExchangeRateProviderResolver exchangeRateProviderResolver;
  @Mock
  private ManualExchangeRateProvider exchangeRateProvider;
  @Mock
  private TransactionService transactionService;
  @Spy
  private FundsDistributionService fundsDistributionService;
  private AutoCloseable mockitoMocks;

  @Mock
  private RequestContext requestContext;

  @BeforeEach
  public void initMocks() {
    mockitoMocks = MockitoAnnotations.openMocks(this);
  }

  @AfterEach
  public void resetMocks() throws Exception {
    mockitoMocks.close();
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
      hasProperty("fundDistribution", is(oneOf(fundDistribution1, fundDistribution2)))
    )));
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

    ReEncumbranceHolder holder1 = new ReEncumbranceHolder().withLedgerId(fund1.getLedgerId()).withCurrentFiscalYearId(currentFiscalYear.getId());
    ReEncumbranceHolder holder2 = new ReEncumbranceHolder().withLedgerId(fund2.getLedgerId()).withCurrentFiscalYearId(currentFiscalYear.getId());
    ReEncumbranceHolder holder3 = new ReEncumbranceHolder().withLedgerId(fund3.getLedgerId()).withCurrentFiscalYearId(currentFiscalYear.getId());
    ReEncumbranceHolder holder4 = new ReEncumbranceHolder().withLedgerId(fund4.getLedgerId()).withCurrentFiscalYearId(currentFiscalYear.getId());
    List<ReEncumbranceHolder> holders = Arrays.asList(holder1, holder2, holder3, holder4);

    LedgerFiscalYearRollover rollover1 = new LedgerFiscalYearRollover().withLedgerId(ledgerId1);
    LedgerFiscalYearRollover rollover2 = new LedgerFiscalYearRollover().withLedgerId(ledgerId2);


    when(ledgerRolloverService.getLedgerFyRollovers(anyString(), anyList(), any())).thenReturn(Future.succeededFuture(Arrays.asList(rollover1, rollover2)));

    List<ReEncumbranceHolder> resultHolders = reEncumbranceHoldersBuilder.withRollovers(holders, requestContext).result();

    assertThat(resultHolders, hasItem(allOf(hasProperty("rollover", is(rollover1)), hasProperty("ledgerId", is(fund1.getLedgerId())))));
    assertThat(resultHolders, hasItem(allOf(hasProperty("rollover", is(rollover2)), hasProperty("ledgerId", is(fund2.getLedgerId())))));
    assertThat(resultHolders, hasItem(allOf(hasProperty("rollover", is(rollover2)), hasProperty("ledgerId", is(fund3.getLedgerId())))));
    assertThat(resultHolders, hasItem(allOf(hasProperty("rollover", nullValue()), hasProperty("ledgerId", is(fund4.getLedgerId())))));
  }

  @Test
  void shouldNotRetrieveRolloversIfReEncumbranceHoldersCurrentFiscalYearNull() {

    String ledgerId1 = UUID.randomUUID().toString();
    String ledgerId2 = UUID.randomUUID().toString();

    Fund fund1 = new Fund().withId(UUID.randomUUID().toString()).withLedgerId(ledgerId1);
    Fund fund2 = new Fund().withId(UUID.randomUUID().toString()).withLedgerId(ledgerId2);
    Fund fund3 = new Fund().withId(UUID.randomUUID().toString()).withLedgerId(ledgerId2);
    Fund fund4 = new Fund().withId(UUID.randomUUID().toString()).withLedgerId(UUID.randomUUID().toString());


    ReEncumbranceHolder holder1 = new ReEncumbranceHolder().withLedgerId(fund1.getLedgerId());
    ReEncumbranceHolder holder2 = new ReEncumbranceHolder().withLedgerId(fund2.getLedgerId());
    ReEncumbranceHolder holder3 = new ReEncumbranceHolder().withLedgerId(fund3.getLedgerId());
    ReEncumbranceHolder holder4 = new ReEncumbranceHolder().withLedgerId(fund4.getLedgerId());
    List<ReEncumbranceHolder> holders = Arrays.asList(holder1, holder2, holder3, holder4);

    List<ReEncumbranceHolder> resultHolders = reEncumbranceHoldersBuilder.withRollovers(holders, requestContext).result();

    assertThat(resultHolders, hasItem(allOf(hasProperty("rollover", nullValue()), hasProperty("ledgerId", is(fund1.getLedgerId())))));
    assertThat(resultHolders, hasItem(allOf(hasProperty("rollover", nullValue()), hasProperty("ledgerId", is(fund2.getLedgerId())))));
    assertThat(resultHolders, hasItem(allOf(hasProperty("rollover", nullValue()), hasProperty("ledgerId", is(fund3.getLedgerId())))));
    assertThat(resultHolders, hasItem(allOf(hasProperty("rollover", nullValue()), hasProperty("ledgerId", is(fund4.getLedgerId())))));

    verify(ledgerRolloverService, never()).getLedgerFyRollovers(anyString(), anyList(), any());
  }

  @Test
  void shouldNotRetrieveRolloversIfReEncumbranceHoldersCurrentFundNull() {

    ReEncumbranceHolder holder1 = new ReEncumbranceHolder();


    List<ReEncumbranceHolder> holders = Collections.singletonList(holder1);

    List<ReEncumbranceHolder> resultHolders = reEncumbranceHoldersBuilder.withRollovers(holders, requestContext).result();

    assertThat(resultHolders, hasItem(allOf(hasProperty("rollover", nullValue()), hasProperty("ledgerId"))));
    assertThat(resultHolders, hasItem(allOf(hasProperty("rollover", nullValue()), hasProperty("ledgerId"))));


    verify(ledgerRolloverService, never()).getLedgerFyRollovers(anyString(), anyList(), any());
  }

  @Test
  void shouldPopulateReEncumbranceHoldersWithConversionWhenHoldersContainsCurrency(VertxTestContext vertxTestContext) {

    FiscalYear fiscalYear = new FiscalYear().withCurrency("USD");
    CompositePoLine line1 = new CompositePoLine().withCost(new Cost().withCurrency("EUR"));
    CompositePoLine line2 = new CompositePoLine().withCost(new Cost().withCurrency("EUR"));
    double exchangeEurToUsdRate = 1.1d;

    ReEncumbranceHolder holder1 = new ReEncumbranceHolder()
        .withCurrency(fiscalYear.getCurrency()).withPoLine(line1);
    ReEncumbranceHolder holder2 = new ReEncumbranceHolder()
        .withCurrency(fiscalYear.getCurrency()).withPoLine(line2);

    CurrencyConversion poLineToFyConversion = mock(ManualCurrencyConversion.class, withSettings().name("poLineToFyConversion"));
    CurrencyConversion poFyToPoLineConversion = mock(ManualCurrencyConversion.class, withSettings().name("poFyToPoLineConversion"));
    ExchangeRate exchangeRate = mock(ExchangeRate.class);

    List<ReEncumbranceHolder> holders = Arrays.asList(holder1, holder2);
    when(exchangeRateProviderResolver.resolve(any(), any())).thenReturn(exchangeRateProvider);
    when(exchangeRateProvider.getCurrencyConversion(any(ConversionQuery.class))).thenReturn(poLineToFyConversion, poFyToPoLineConversion);

    when(exchangeRate.getFactor()).thenReturn(new DefaultNumberValue(exchangeEurToUsdRate));
    when(poLineToFyConversion.getCurrency()).thenReturn(Monetary.getCurrency("EUR"));
    when(poLineToFyConversion.getExchangeRate(any())).thenReturn(exchangeRate);
    when(poFyToPoLineConversion.getCurrency()).thenReturn(Monetary.getCurrency("USD"));
    when(requestContext.getContext()).thenReturn(Vertx.vertx().getOrCreateContext());

    var future = reEncumbranceHoldersBuilder.withConversion(holders, requestContext);
    vertxTestContext.assertComplete(future)
      .onComplete(result -> {
        assertEquals(holders.get(0).getPoLineToFyConversion().getCurrency(), poLineToFyConversion.getCurrency());
        assertEquals(holders.get(1).getPoLineToFyConversion().getCurrency(), poLineToFyConversion.getCurrency());
        assertEquals(holders.get(0).getFyToPoLineConversion().getCurrency(), poFyToPoLineConversion.getCurrency());
        assertEquals(holders.get(1).getFyToPoLineConversion().getCurrency(), poFyToPoLineConversion.getCurrency());
        vertxTestContext.completeNow();
      });

  }

  @Test
  void shouldNotResolveExchangeRateProviderWhenReEncumbranceHoldersNotContainsCurrentFiscalYear() {

    List<ReEncumbranceHolder> holders = Arrays.asList(new ReEncumbranceHolder(), new ReEncumbranceHolder());

    reEncumbranceHoldersBuilder.withConversion(holders, requestContext).result();

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

    List<Transaction> fromTransactionList = Arrays.asList(fromEncumbrance1, fromEncumbrance2);
    List<Transaction> toTransactionList = Collections.singletonList(toEncumbrance1);

    when(transactionService.getTransactions(contains(fromFyId), any())).thenReturn(Future.succeededFuture(fromTransactionList));
    when(transactionService.getTransactions(contains(toFyId), any())).thenReturn(Future.succeededFuture(toTransactionList));

    List<ReEncumbranceHolder> resultHolders = reEncumbranceHoldersBuilder.withPreviousFyEncumbrances(holders, requestContext).result();

    assertThat(resultHolders, hasItem(hasProperty("previousFyEncumbrance", is(fromEncumbrance1))));
    assertThat(resultHolders, hasItem(hasProperty("previousFyEncumbrance", is(fromEncumbrance2))));

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

    List<Transaction> fromTransactionList = Arrays.asList(fromEncumbrance1, fromEncumbrance2);
    List<Transaction> toTransactionList = Collections.emptyList();

    when(transactionService.getTransactions(contains(fromFyId), any())).thenReturn(Future.succeededFuture(fromTransactionList));
    when(transactionService.getTransactions(contains(toFyId), any())).thenReturn(Future.succeededFuture(toTransactionList));

    List<ReEncumbranceHolder> resultHolders = reEncumbranceHoldersBuilder.withPreviousFyEncumbrances(holders, requestContext).result();

    assertThat(resultHolders, hasItem(hasProperty("previousFyEncumbrance", is(fromEncumbrance1))));

    assertThat(resultHolders, hasItem(hasProperty("previousFyEncumbrance", is(fromEncumbrance2))));
  }


  @Test
  void shouldPopulateReEncumbranceHoldersWithToFyEncumbrancesWhenBasedOnExpended() {
    String fromFyId = UUID.randomUUID().toString();
    String toFyId = UUID.randomUUID().toString();
    String orderId = UUID.randomUUID().toString();
    String fund1Id = UUID.randomUUID().toString();
    String fund2Id = UUID.randomUUID().toString();
    double exchangeEurToUsdRate = 1.1d;

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

    ManualCurrencyConversion poLineToFyConversion = mock(ManualCurrencyConversion.class, withSettings().name("poLineToFyConversion"));
    ManualCurrencyConversion poFyToPoLineConversion = mock(ManualCurrencyConversion.class, withSettings().name("poFyToPoLineConversion"));

    List<Transaction> toTransactionList = Collections.emptyList();

    ReEncumbranceHolder holder1 = new ReEncumbranceHolder()
            .withPurchaseOrder(compPO)
            .withRollover(rollover)
            .withPoLine(line1)
            .withFundDistribution(fundDistribution1)
            .withEncumbranceRollover(encumbranceRollover)
            .withPreviousFyEncumbrance(fromEncumbrance1)
            .withPoLineToFyConversion(poLineToFyConversion)
            .withFyToPoLineConversion(poFyToPoLineConversion)
            .withCurrency("USD");
    ReEncumbranceHolder holder2 = new ReEncumbranceHolder()
            .withPurchaseOrder(compPO)
            .withRollover(rollover)
            .withPoLine(line1)
            .withFundDistribution(fundDistribution2)
            .withEncumbranceRollover(encumbranceRollover)
            .withPreviousFyEncumbrance(fromEncumbrance2)
            .withPoLineToFyConversion(poLineToFyConversion)
            .withFyToPoLineConversion(poFyToPoLineConversion)
            .withCurrency("USD");

    List<ReEncumbranceHolder> holders = Arrays.asList(holder1, holder2);

    when(transactionService.getTransactions(contains(toFyId), any())).thenReturn(Future.succeededFuture(toTransactionList));

    when(poLineToFyConversion.with(any())).thenReturn(poLineToFyConversion);
    when(poLineToFyConversion.apply(any(MonetaryAmount.class))).thenAnswer(invocation -> {
      MonetaryAmount amount = invocation.getArgument(0);
      return Money.of(amount.getNumber(), "USD").multiply(exchangeEurToUsdRate);
    } );
    when(poFyToPoLineConversion.getCurrency()).thenReturn(Monetary.getCurrency("USD"));
    when(exchangeRateProviderResolver.resolve(any(), any())).thenReturn(exchangeRateProvider);

    List<ReEncumbranceHolder> resultHolders = reEncumbranceHoldersBuilder.withToEncumbrances(holders, requestContext).result();

    assertThat(resultHolders, hasItem(hasProperty("newEncumbrance", allOf(
        hasProperty("id", nullValue()),
        hasProperty("currency", is(fromEncumbrance1.getCurrency())),
        hasProperty("fromFundId", is(fromEncumbrance1.getFromFundId())),
        hasProperty("amount", is(46.59d)),
        hasProperty("fiscalYearId", is(toFyId))
    ))));

    assertThat(resultHolders, hasItem(hasProperty("newEncumbrance", allOf(
        hasProperty("id", nullValue()),
        hasProperty("currency", is(fromEncumbrance2.getCurrency())),
        hasProperty("fromFundId", is(fromEncumbrance2.getFromFundId())),
        hasProperty("amount", is(46.58d)),
        hasProperty("fiscalYearId", is(toFyId))
    ))));
  }
}
