package org.folio.service.finance.transaction;

import static java.util.Collections.emptyList;
import static java.util.Collections.singletonList;
import static org.folio.rest.acq.model.finance.Encumbrance.OrderStatus.OPEN;
import static org.folio.rest.acq.model.finance.Encumbrance.OrderType.ONGOING;
import static org.folio.rest.acq.model.finance.Encumbrance.Status.UNRELEASED;
import static org.folio.rest.acq.model.finance.Transaction.Source.PO_LINE;
import static org.folio.rest.impl.MockServer.BASE_MOCK_DATA_PATH;
import static org.hamcrest.MatcherAssert.assertThat;
import static org.hamcrest.Matchers.allOf;
import static org.hamcrest.Matchers.empty;
import static org.hamcrest.Matchers.everyItem;
import static org.hamcrest.Matchers.hasItem;
import static org.hamcrest.Matchers.hasProperty;
import static org.hamcrest.Matchers.hasSize;
import static org.hamcrest.Matchers.nullValue;
import static org.hamcrest.core.Is.is;
import static org.junit.jupiter.api.Assertions.assertEquals;
import static org.junit.jupiter.api.Assertions.assertNull;
import static org.junit.jupiter.api.Assertions.assertSame;
import static org.mockito.ArgumentMatchers.any;
import static org.mockito.ArgumentMatchers.anyCollection;
import static org.mockito.ArgumentMatchers.anyList;
import static org.mockito.ArgumentMatchers.anyString;
import static org.mockito.Mockito.mock;
import static org.mockito.Mockito.when;

import java.util.ArrayList;
import java.util.Collections;
import java.util.List;
import java.util.UUID;
import java.util.concurrent.CompletableFuture;

import javax.money.convert.ConversionQuery;
import javax.money.convert.ExchangeRateProvider;

import org.folio.models.EncumbranceRelationsHolder;
import org.folio.rest.acq.model.finance.Budget;
import org.folio.rest.acq.model.finance.Encumbrance;
import org.folio.rest.acq.model.finance.FiscalYear;
import org.folio.rest.acq.model.finance.Fund;
import org.folio.rest.acq.model.finance.Ledger;
import org.folio.rest.acq.model.finance.Transaction;
import org.folio.rest.core.models.RequestContext;
import org.folio.rest.jaxrs.model.CompositePoLine;
import org.folio.rest.jaxrs.model.CompositePurchaseOrder;
import org.folio.rest.jaxrs.model.Cost;
import org.folio.rest.jaxrs.model.FundDistribution;
import org.folio.rest.jaxrs.model.Ongoing;
import org.folio.service.exchange.ExchangeRateProviderResolver;
import org.folio.service.exchange.ManualCurrencyConversion;
import org.folio.service.exchange.ManualExchangeRateProvider;
import org.folio.service.finance.FiscalYearService;
import org.folio.service.finance.FundService;
import org.folio.service.finance.LedgerService;
import org.folio.service.finance.budget.BudgetService;
import org.junit.jupiter.api.BeforeEach;
import org.junit.jupiter.api.Test;
import org.mockito.InjectMocks;
import org.mockito.Mock;
import org.mockito.MockitoAnnotations;

import io.vertx.core.Vertx;

public class EncumbranceRelationsHoldersBuilderTest {

  private static final String ORDER_ID = "1ab7ef6a-d1d4-4a4f-90a2-882aed18af14";
  private static final String ORDER_PATH = BASE_MOCK_DATA_PATH + "compositeOrders/" + ORDER_ID + ".json";

  @InjectMocks
  private EncumbranceRelationsHoldersBuilder encumbranceRelationsHoldersBuilder;

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
  private EncumbranceService encumbranceService;
  @Mock
  private RequestContext requestContextMock;

  CompositePurchaseOrder order;
  CompositePoLine line1;
  CompositePoLine line2;
  CompositePoLine line3;
  FundDistribution distribution1;
  FundDistribution distribution2;
  FundDistribution distribution3;
  Transaction newEncumbrance1;
  Transaction newEncumbrance2;
  Transaction newEncumbrance3;
  EncumbranceRelationsHolder holder1;
  EncumbranceRelationsHolder holder2;
  EncumbranceRelationsHolder holder3;

  @BeforeEach
  public void initMocks(){
    MockitoAnnotations.openMocks(this);
    order = new CompositePurchaseOrder().withId(UUID.randomUUID().toString())
        .withReEncumber(true)
        .withOrderType(CompositePurchaseOrder.OrderType.ONGOING)
        .withOngoing(new Ongoing().withIsSubscription(false));

    distribution1 = new FundDistribution().withFundId(UUID.randomUUID().toString())
        .withDistributionType(FundDistribution.DistributionType.PERCENTAGE)
        .withValue(100d)
        .withEncumbrance(UUID.randomUUID().toString());

    line1 = new CompositePoLine().withId(UUID.randomUUID().toString())
        .withCost(new Cost().withCurrency("USD").withListUnitPrice(68d).withQuantityPhysical(1))
        .withPurchaseOrderId(order.getId())
        .withFundDistribution(Collections.singletonList(distribution1));

    distribution2 = new FundDistribution().withFundId(UUID.randomUUID().toString())
        .withDistributionType(FundDistribution.DistributionType.PERCENTAGE)
        .withValue(100d)
        .withEncumbrance(UUID.randomUUID().toString());

    line2 = new CompositePoLine().withId(UUID.randomUUID().toString())
        .withCost(new Cost().withCurrency("USD").withListUnitPrice(34.95).withQuantityPhysical(1))
        .withPurchaseOrderId(order.getId())
        .withFundDistribution(Collections.singletonList(distribution2));

    distribution3 = new FundDistribution().withFundId(UUID.randomUUID().toString())
        .withDistributionType(FundDistribution.DistributionType.PERCENTAGE)
        .withExpenseClassId(UUID.randomUUID().toString())
        .withValue(100d)
        .withEncumbrance(UUID.randomUUID().toString());

    line3 = new CompositePoLine().withId(UUID.randomUUID().toString())
        .withCost(new Cost().withCurrency("EUR").withListUnitPrice(24.99).withQuantityPhysical(1))
        .withPurchaseOrderId(order.getId())
        .withFundDistribution(Collections.singletonList(distribution3));

    order.setCompositePoLines(List.of(line1, line2, line3));

    newEncumbrance1 = new Transaction()
        .withFromFundId(distribution1.getFundId())
        .withSource(PO_LINE)
        .withEncumbrance(
            new Encumbrance()
              .withOrderType(ONGOING)
              .withReEncumber(true)
              .withOrderStatus(OPEN)
              .withStatus(UNRELEASED)
              .withSubscription(false)
              .withSourcePurchaseOrderId(order.getId())
              .withSourcePoLineId(line1.getId())
        );

    newEncumbrance2 = new Transaction()
        .withFromFundId(distribution2.getFundId())
        .withSource(PO_LINE)
        .withEncumbrance(
            new Encumbrance()
                .withOrderType(ONGOING)
                .withReEncumber(true)
                .withOrderStatus(OPEN)
                .withStatus(UNRELEASED)
                .withSubscription(false)
                .withSourcePurchaseOrderId(order.getId())
                .withSourcePoLineId(line2.getId())
        );

    newEncumbrance3 = new Transaction()
        .withFromFundId(distribution3.getFundId())
        .withSource(PO_LINE)
        .withExpenseClassId(distribution3.getExpenseClassId())
        .withEncumbrance(
            new Encumbrance()
                .withOrderType(ONGOING)
                .withReEncumber(true)
                .withOrderStatus(OPEN)
                .withStatus(UNRELEASED)
                .withSubscription(false)
                .withSourcePurchaseOrderId(order.getId())
                .withSourcePoLineId(line3.getId())
        );

    holder1 = new EncumbranceRelationsHolder().withPurchaseOrder(order).withPoLine(line1)
        .withFundDistribution(distribution1).withNewEncumbrance(newEncumbrance1);
    holder2 = new EncumbranceRelationsHolder().withPurchaseOrder(order).withPoLine(line2)
        .withFundDistribution(distribution2).withNewEncumbrance(newEncumbrance2);
    holder3 = new EncumbranceRelationsHolder().withPurchaseOrder(order).withPoLine(line3)
        .withFundDistribution(distribution3).withNewEncumbrance(newEncumbrance3);

  }

  @Test
  void testShouldReturnHoldersForEveryFundDistribution() {
    //When
    List<EncumbranceRelationsHolder> resultHolders = encumbranceRelationsHoldersBuilder.buildBaseHolders(order);

    //Then
    assertEquals(3, resultHolders.size());
    assertThat(resultHolders, everyItem(hasProperty("newEncumbrance",  allOf(
        hasProperty("source", is(PO_LINE)),
        hasProperty("encumbrance", allOf(
            hasProperty("subscription", is(false)),
            hasProperty("status", is(UNRELEASED)),
            hasProperty("orderType", is(ONGOING)),
            hasProperty("orderStatus", is(OPEN)),
            hasProperty("reEncumber", is(true)),
            hasProperty("sourcePurchaseOrderId", is(order.getId()))
        ))
    ))));
    assertThat(resultHolders, hasItem(hasProperty("newEncumbrance", allOf(
        hasProperty("fromFundId", is(distribution1.getFundId())),
        hasProperty("expenseClassId", nullValue()),
        hasProperty("encumbrance", hasProperty("sourcePoLineId", is(line1.getId())))
    ))));
    assertThat(resultHolders, hasItem(hasProperty("newEncumbrance", allOf(
        hasProperty("fromFundId", is(distribution2.getFundId())),
        hasProperty("expenseClassId", nullValue()),
        hasProperty("encumbrance", hasProperty("sourcePoLineId", is(line2.getId())))
    ))));
    assertThat(resultHolders, hasItem(hasProperty("newEncumbrance", allOf(
        hasProperty("fromFundId", is(distribution3.getFundId())),
        hasProperty("expenseClassId", is(distribution3.getExpenseClassId())),
        hasProperty("encumbrance", hasProperty("sourcePoLineId", is(line3.getId())))
    ))));
    assertThat(resultHolders, hasItem(hasProperty("fundDistribution", is(distribution1))));
    assertThat(resultHolders, hasItem(hasProperty("fundDistribution", is(distribution2))));
    assertThat(resultHolders, hasItem(hasProperty("fundDistribution", is(distribution3))));
    assertThat(resultHolders, everyItem(hasProperty("purchaseOrder", is(order))));
    assertThat(resultHolders, hasItem(hasProperty("poLine", is(line1))));
    assertThat(resultHolders, hasItem(hasProperty("poLine", is(line2))));
    assertThat(resultHolders, hasItem(hasProperty("poLine", is(line3))));
  }

  @Test
  void testShouldReturnEmptyCollectionWhenOrdersNotContainsFundDistribution() {
    //given
    order.getCompositePoLines().forEach(poLine -> poLine.setFundDistribution(emptyList()));
    //When
    List<EncumbranceRelationsHolder> resultHolders = encumbranceRelationsHoldersBuilder.buildBaseHolders(order);
    //Then
    assertThat(resultHolders, empty());
  }

  @Test
  void testShouldPopulateHoldersWithOldEncumbranceIfMatchesTransactionFromStorage() {
    //given
    Transaction encumbranceFromStorage = new Transaction()
        .withId(UUID.randomUUID().toString())
        .withFromFundId(distribution1.getFundId())
        .withSource(PO_LINE)
        .withAmount(38d)
        .withFiscalYearId(UUID.randomUUID().toString())
        .withEncumbrance(new Encumbrance()
                             .withSubscription(false)
                             .withReEncumber(true)
                             .withOrderType(ONGOING)
                             .withSourcePurchaseOrderId(order.getId())
                             .withSourcePoLineId(line1.getId())
                             .withInitialAmountEncumbered(68d)
                             .withAmountExpended(10d)
                             .withAmountAwaitingPayment(20d)
            );

    List<EncumbranceRelationsHolder> holders = new ArrayList<>();
    holders.add(holder1);
    holders.add(holder2);

    when(encumbranceService.getEncumbrancesByIds(anyList(), any()))
      .thenReturn(CompletableFuture.succeededFuture(singletonList(encumbranceFromStorage)));
    //When
    List<EncumbranceRelationsHolder> resultHolders = encumbranceRelationsHoldersBuilder
        .withExistingTransactions(holders, order, requestContextMock).result();
    //Then
    assertThat(resultHolders, hasSize(2));
    assertEquals(encumbranceFromStorage, holder1.getOldEncumbrance());
    assertEquals(encumbranceFromStorage.getId(), holder1.getNewEncumbrance().getId());
    assertEquals(encumbranceFromStorage.getEncumbrance().getAmountExpended(),
                 holder1.getNewEncumbrance().getEncumbrance().getAmountExpended());
    assertEquals(encumbranceFromStorage.getEncumbrance().getAmountAwaitingPayment(),
                 holder1.getNewEncumbrance().getEncumbrance().getAmountAwaitingPayment());
    assertNull(holder2.getOldEncumbrance());

  }

  @Test
  void testShouldCreateNewHoldersForTransactionsFromStorageThatNotMatchProvidedHolders() {
    //given
    Transaction encumbranceFromStorage1 = new Transaction()
        .withId(UUID.randomUUID().toString())
        .withFromFundId(distribution1.getFundId())
        .withSource(PO_LINE)
        .withAmount(38d)
        .withExpenseClassId(UUID.randomUUID().toString())
        .withFiscalYearId(UUID.randomUUID().toString())
        .withEncumbrance(new Encumbrance()
                             .withSubscription(false)
                             .withReEncumber(true)
                             .withOrderType(ONGOING)
                             .withSourcePurchaseOrderId(order.getId())
                             .withSourcePoLineId(line1.getId())
                             .withInitialAmountEncumbered(68d)
                             .withAmountExpended(10d)
                             .withAmountAwaitingPayment(20d)
        );

    Transaction encumbranceFromStorage2 = new Transaction()
        .withId(UUID.randomUUID().toString())
        .withFromFundId(UUID.randomUUID().toString())
        .withSource(PO_LINE)
        .withAmount(38d)
        .withFiscalYearId(UUID.randomUUID().toString())
        .withEncumbrance(new Encumbrance()
                             .withSubscription(false)
                             .withReEncumber(true)
                             .withOrderType(ONGOING)
                             .withSourcePurchaseOrderId(order.getId())
                             .withSourcePoLineId(line1.getId())
                             .withInitialAmountEncumbered(68d)
                             .withAmountExpended(10d)
                             .withAmountAwaitingPayment(20d)
        );

    String fiscalYearId = UUID.randomUUID().toString();
    holder1.withCurrentFiscalYearId(fiscalYearId);
    holder2.withCurrentFiscalYearId(fiscalYearId);
    holder3.withCurrentFiscalYearId(fiscalYearId);
    List<EncumbranceRelationsHolder> holders = new ArrayList<>();
    holders.add(holder1);
    holders.add(holder2);
    holders.add(holder3);

    when(encumbranceService.getEncumbrancesByIds(anyList(), any()))
      .thenReturn(CompletableFuture.succeededFuture(List.of(encumbranceFromStorage1, encumbranceFromStorage2)));

    //When
    List<EncumbranceRelationsHolder> resultHolders = encumbranceRelationsHoldersBuilder
      .withExistingTransactions(holders, order, requestContextMock).result();
    //Then
    assertThat(resultHolders, hasSize(5));
    assertNull(holder1.getOldEncumbrance());
    assertNull(holder2.getOldEncumbrance());
    assertNull(holder3.getOldEncumbrance());

    assertThat(resultHolders, hasItem( hasProperty("oldEncumbrance", is(encumbranceFromStorage1))));
    assertThat(resultHolders, hasItem( hasProperty("oldEncumbrance", is(encumbranceFromStorage2))));
  }

  @Test
  void testShouldPopulateHoldersWithCorrespondingBudgets() {
    //given
    Budget budget1 = new Budget().withId(UUID.randomUUID().toString()).withFundId(holder1.getFundId());
    Budget budget2 = new Budget().withId(UUID.randomUUID().toString()).withFundId(holder2.getFundId());
    Budget budget3 = new Budget().withId(UUID.randomUUID().toString()).withFundId(holder3.getFundId());

    List<EncumbranceRelationsHolder> holders = new ArrayList<>();
    holders.add(holder1);
    holders.add(holder2);
    holders.add(holder3);

    when(budgetService.getBudgets(anyCollection(), any()))
        .thenReturn(CompletableFuture.succeededFuture(List.of(budget1, budget2, budget3)));

    //When
    encumbranceRelationsHoldersBuilder.withBudgets(holders, requestContextMock).result();
    //Then
    assertEquals(budget1, holder1.getBudget());
    assertEquals(budget2, holder2.getBudget());
    assertEquals(budget3, holder3.getBudget());
  }

  @Test
  void testShouldPopulateHoldersWithCorrespondingLedgerData() {
    //given

    Fund fund1 = new Fund().withId(holder1.getFundId()).withLedgerId(UUID.randomUUID().toString());
    Fund fund2 = new Fund().withId(holder2.getFundId()).withLedgerId(UUID.randomUUID().toString());
    Fund fund3 = new Fund().withId(holder3.getFundId()).withLedgerId(UUID.randomUUID().toString());

    Ledger ledger1 = new Ledger().withId(fund1.getLedgerId()).withRestrictEncumbrance(true);
    Ledger ledger2 = new Ledger().withId(fund2.getLedgerId()).withRestrictEncumbrance(true);
    Ledger ledger3 = new Ledger().withId(fund3.getLedgerId()).withRestrictEncumbrance(false);

    List<EncumbranceRelationsHolder> holders = new ArrayList<>();
    holders.add(holder1);
    holders.add(holder2);
    holders.add(holder3);

    when(fundService.getAllFunds(anyCollection(), any())).thenReturn(CompletableFuture.succeededFuture(List.of(fund1, fund2, fund3)));
    when(ledgerService.getLedgersByIds(anyCollection(), any())).thenReturn(CompletableFuture.succeededFuture(List.of(ledger2, ledger1, ledger3)));

    //When
    encumbranceRelationsHoldersBuilder.withLedgersData(holders, requestContextMock).result();
    //Then
    assertEquals(ledger1.getId(), holder1.getLedgerId());
    assertEquals(ledger1.getRestrictEncumbrance(), holder1.getRestrictEncumbrance());
    assertEquals(ledger2.getId(), holder2.getLedgerId());
    assertEquals(ledger2.getRestrictEncumbrance(), holder2.getRestrictEncumbrance());
    assertEquals(ledger3.getId(), holder3.getLedgerId());
    assertEquals(ledger3.getRestrictEncumbrance(), holder3.getRestrictEncumbrance());
  }

  @Test
  void testShouldPopulateHoldersWithFiscalYearData() {
    //given
    String fiscalYearId = UUID.randomUUID().toString();
    FiscalYear fiscalYear = new FiscalYear().withId(fiscalYearId).withCurrency("RUB");

    Budget budget1 = new Budget().withId(UUID.randomUUID().toString()).withFundId(holder1.getFundId()).withFiscalYearId(fiscalYearId);
    Budget budget2 = new Budget().withId(UUID.randomUUID().toString()).withFundId(holder2.getFundId()).withFiscalYearId(fiscalYearId);
    Budget budget3 = new Budget().withId(UUID.randomUUID().toString()).withFundId(holder3.getFundId()).withFiscalYearId(fiscalYearId);

    List<EncumbranceRelationsHolder> holders = new ArrayList<>();
    holders.add(holder1.withBudget(budget1));
    holders.add(holder2.withBudget(budget2));
    holders.add(holder3.withBudget(budget3));
   when(fiscalYearService.getFiscalYearById(anyString(), any())).thenReturn(CompletableFuture.succeededFuture(fiscalYear));
    //When
    List<EncumbranceRelationsHolder> resultHolders = encumbranceRelationsHoldersBuilder.withFiscalYearData(holders, requestContextMock).result();
    //Then
    assertThat(resultHolders, everyItem(hasProperty("newEncumbrance", allOf(
        hasProperty("fiscalYearId", is(fiscalYear.getId())),
        hasProperty("currency", is(fiscalYear.getCurrency()))
    ))));
  }

  @Test
  void testShouldPopulatePoLineToFiscalYearCurrencyConversion() {
    //given
    String currency = "RUB";

    List<EncumbranceRelationsHolder> holders = new ArrayList<>();
    holders.add(holder1.withCurrency(currency));
    holders.add(holder2.withCurrency(currency));
    holders.add(holder3.withCurrency(currency));
    ExchangeRateProvider exchangeRateProvider = mock(ManualExchangeRateProvider.class);
    when(exchangeRateProviderResolver.resolve(any(), any())).thenReturn(exchangeRateProvider);
    when(exchangeRateProvider.getCurrencyConversion(any(ConversionQuery.class))).thenAnswer(invocation -> {
      ConversionQuery conversionQuery = invocation.getArgument(0);
      return mock(ManualCurrencyConversion.class, conversionQuery.getBaseCurrency().getCurrencyCode());
    });
    when(requestContextMock.getContext()).thenReturn(Vertx.vertx().getOrCreateContext());
    //When
    encumbranceRelationsHoldersBuilder.withConversion(holders, requestContextMock).result();
    //Then
    assertEquals("USD", holder1.getPoLineToFyConversion().toString());
    assertEquals("USD", holder2.getPoLineToFyConversion().toString());
    assertSame(holder1.getPoLineToFyConversion(), holder2.getPoLineToFyConversion());
    assertEquals("EUR", holder3.getPoLineToFyConversion().toString());
  }

}
