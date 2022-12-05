package org.folio.service.orders;

import static java.util.Collections.singletonList;
import static io.vertx.core.Future.succeededFuture;
import static org.folio.rest.jaxrs.model.PurchaseOrder.OrderType.ONE_TIME;
import static org.folio.service.exchange.ExchangeRateProviderResolver.RATE_KEY;
import static org.hamcrest.MatcherAssert.assertThat;
import static org.hamcrest.Matchers.equalTo;
import static org.junit.jupiter.api.Assertions.assertFalse;
import static org.mockito.ArgumentMatchers.any;
import static org.mockito.ArgumentMatchers.anyInt;
import static org.mockito.ArgumentMatchers.anyList;
import static org.mockito.ArgumentMatchers.anyString;
import static org.mockito.ArgumentMatchers.argThat;
import static org.mockito.ArgumentMatchers.eq;
import static org.mockito.Mockito.doReturn;
import static org.mockito.Mockito.mock;
import static org.mockito.Mockito.times;
import static org.mockito.Mockito.verify;
import static org.mockito.Mockito.when;

import java.math.BigDecimal;
import java.math.RoundingMode;
import java.util.List;
import java.util.UUID;

import javax.money.Monetary;
import javax.money.convert.ConversionContext;
import javax.money.convert.ConversionQuery;
import javax.money.convert.ConversionQueryBuilder;
import javax.money.convert.ExchangeRate;
import javax.money.convert.ExchangeRateProvider;

import io.vertx.core.Future;
import org.folio.rest.acq.model.finance.Encumbrance;
import org.folio.rest.acq.model.finance.Fund;
import org.folio.rest.acq.model.finance.Transaction;
import org.folio.rest.core.models.RequestContext;
import org.folio.rest.jaxrs.model.Cost;
import org.folio.rest.jaxrs.model.EncumbranceRollover;
import org.folio.rest.jaxrs.model.FundDistribution;
import org.folio.rest.jaxrs.model.FundDistribution.DistributionType;
import org.folio.rest.jaxrs.model.LedgerFiscalYearRollover;
import org.folio.rest.jaxrs.model.PoLine;
import org.folio.rest.jaxrs.model.PurchaseOrder;
import org.folio.rest.jaxrs.model.PurchaseOrderCollection;
import org.folio.service.configuration.ConfigurationEntriesService;
import org.folio.service.exchange.ExchangeRateProviderResolver;
import org.folio.service.exchange.ManualCurrencyConversion;
import org.folio.service.exchange.ManualExchangeRateProvider;
import org.folio.service.finance.FundService;
import org.folio.service.finance.transaction.TransactionService;
import org.javamoney.moneta.spi.DefaultNumberValue;
import org.junit.jupiter.api.BeforeEach;
import org.junit.jupiter.api.DisplayName;
import org.junit.jupiter.api.Test;
import org.mockito.InjectMocks;
import org.mockito.Mock;
import org.mockito.Mockito;
import org.mockito.MockitoAnnotations;

public class OrderRolloverServiceTest {

  @InjectMocks
  private OrderRolloverService orderRolloverService;

  @Mock
  private FundService fundService;
  @Mock
  private TransactionService transactionService;
  @Mock
  private PurchaseOrderStorageService purchaseOrderStorageService;
  @Mock
  private PurchaseOrderLineService purchaseOrderLineService;
  @Mock
  private ConfigurationEntriesService configurationEntriesService;
  @Mock
  private ExchangeRateProviderResolver exchangeRateProviderResolver;
  @Mock
  private RequestContext requestContext;
  private String systemCurrency = "USD";

  @BeforeEach
  public void initMocks() {
    MockitoAnnotations.openMocks(this);
  }


  @Test
  @DisplayName("Should update order lines cost And Encumbrance Links where Pol Currency equals systemCurrency")
  void shouldUpdateOrderLinesCostAndEncumbranceLinksAndPolCurrencyVsSystemCurrencyTheSame() {
    String fromFiscalYearId = UUID.randomUUID().toString();
    String ledgerId = UUID.randomUUID().toString();
    String toFiscalYearId = UUID.randomUUID().toString();
    String fundId1 = UUID.randomUUID().toString();
    String fundId2 = UUID.randomUUID().toString();
    String fundId3 = UUID.randomUUID().toString();
    String orderId1 = UUID.randomUUID().toString();
    String orderId2 = UUID.randomUUID().toString();
    String orderId3 = UUID.randomUUID().toString();
    String poLineId1 = UUID.randomUUID().toString();
    String poLineId2 = UUID.randomUUID().toString();
    String poLineId3 = UUID.randomUUID().toString();
    String prevEncumbrId1 = UUID.randomUUID().toString();
    String prevEncumbrId2 = UUID.randomUUID().toString();
    String prevEncumbrId3 = UUID.randomUUID().toString();
    String currEncumbrId1 = UUID.randomUUID().toString();
    String currEncumbrId2 = UUID.randomUUID().toString();
    String currEncumbrId3 = UUID.randomUUID().toString();
    String expClassId2 = UUID.randomUUID().toString();
    String expClassId3 = UUID.randomUUID().toString();

    EncumbranceRollover ongoingEncumbranceBasedOnExpended = new EncumbranceRollover()
      .withOrderType(EncumbranceRollover.OrderType.ONGOING).withBasedOn(EncumbranceRollover.BasedOn.EXPENDED);
    EncumbranceRollover oneTimeEncumbrance = new EncumbranceRollover()
      .withOrderType(EncumbranceRollover.OrderType.ONE_TIME).withBasedOn(EncumbranceRollover.BasedOn.REMAINING);
    EncumbranceRollover ongoingEncumbranceBasedOnInitialAmount = new EncumbranceRollover()
      .withOrderType(EncumbranceRollover.OrderType.ONGOING).withBasedOn(EncumbranceRollover.BasedOn.INITIAL_AMOUNT);

    LedgerFiscalYearRollover ledgerFiscalYearRollover = new LedgerFiscalYearRollover()
      .withId(UUID.randomUUID().toString())
      .withFromFiscalYearId(fromFiscalYearId)
      .withLedgerId(ledgerId)
      .withToFiscalYearId(toFiscalYearId)
      .withEncumbrancesRollover(List.of(ongoingEncumbranceBasedOnExpended, oneTimeEncumbrance, ongoingEncumbranceBasedOnInitialAmount));

    List<Fund> funds = List.of(new Fund().withId(fundId1).withLedgerId(ledgerId), new Fund().withId(fundId2).withLedgerId(ledgerId),
      new Fund().withId(fundId3).withLedgerId(ledgerId));

    PurchaseOrder purchaseOrder1 = new PurchaseOrder().withId(orderId1).withWorkflowStatus(PurchaseOrder.WorkflowStatus.OPEN);
    PurchaseOrder purchaseOrder2 = new PurchaseOrder().withId(orderId2).withWorkflowStatus(PurchaseOrder.WorkflowStatus.OPEN);
    PurchaseOrder purchaseOrder3 = new PurchaseOrder().withId(orderId3).withWorkflowStatus(PurchaseOrder.WorkflowStatus.OPEN);

    List<PurchaseOrder> orders = List.of(purchaseOrder1, purchaseOrder2, purchaseOrder3);
    PurchaseOrderCollection purchaseOrderCollection = new PurchaseOrderCollection().withPurchaseOrders(orders).withTotalRecords(3);

    FundDistribution fundDistributionOneTime = new FundDistribution().withFundId(fundId1).withValue(100d)
      .withEncumbrance(prevEncumbrId1);
    FundDistribution fundDistributionOngoing2 = new FundDistribution().withFundId(fundId2).withValue(100d)
      .withEncumbrance(prevEncumbrId2).withExpenseClassId(expClassId2);
    FundDistribution fundDistributionOngoing3 = new FundDistribution().withFundId(fundId3).withValue(100d)
      .withEncumbrance(prevEncumbrId3).withExpenseClassId(expClassId3);

    Cost costOneTime = new Cost().withListUnitPrice(100d).withQuantityPhysical(1).withCurrency("USD").withPoLineEstimatedPrice(100d);
    PoLine poLineOneTime = new PoLine().withId(poLineId1).withPurchaseOrderId(orderId1).withCost(costOneTime)
      .withFundDistribution(List.of(fundDistributionOneTime));

    Cost costOngoing2 = new Cost().withListUnitPrice(100d).withQuantityPhysical(1).withCurrency("USD").withPoLineEstimatedPrice(100d);
    PoLine poLineOngoing2 = new PoLine().withId(poLineId2).withPurchaseOrderId(orderId2).withCost(costOngoing2)
      .withFundDistribution(List.of(fundDistributionOngoing2));

    Cost costOngoing3 = new Cost().withListUnitPrice(100d).withQuantityPhysical(1).withCurrency("USD").withPoLineEstimatedPrice(100d);
    PoLine poLineOngoing3 = new PoLine().withId(poLineId3).withPurchaseOrderId(orderId3).withCost(costOngoing3)
      .withFundDistribution(List.of(fundDistributionOngoing3));

    List<PoLine> poLines = List.of(poLineOneTime, poLineOngoing2, poLineOngoing3);

    doReturn(succeededFuture(funds)).when(fundService).getFundsByLedgerId(ledgerId, requestContext);
    doReturn(succeededFuture(purchaseOrderCollection)).when(purchaseOrderStorageService)
      .getPurchaseOrders(anyString(), anyInt(), anyInt(), any());
    doReturn(succeededFuture(poLines)).when(purchaseOrderLineService).getOrderLines(anyString(), anyInt(), anyInt(), any());
    doReturn(succeededFuture(null)).when(purchaseOrderLineService).saveOrderLines(eq(poLines), any());

    Encumbrance encumbranceOneTime = new Encumbrance().withSourcePurchaseOrderId(orderId1).withSourcePoLineId(poLineId1)
      .withOrderType(Encumbrance.OrderType.ONE_TIME).withInitialAmountEncumbered(60d);
    Transaction transactionOneTime = new Transaction().withId(currEncumbrId1).withFromFundId(fundId1)
      .withEncumbrance(encumbranceOneTime);
    Encumbrance encumbranceOngoing2 = new Encumbrance().withSourcePurchaseOrderId(orderId2).withSourcePoLineId(poLineId2)
      .withOrderType(Encumbrance.OrderType.ONGOING).withInitialAmountEncumbered(90d);
    Transaction transactionOngoing2 = new Transaction().withId(currEncumbrId2).withFromFundId(fundId2)
      .withEncumbrance(encumbranceOngoing2).withExpenseClassId(expClassId2);
    Encumbrance encumbranceOngoing3 = new Encumbrance().withSourcePurchaseOrderId(orderId3).withSourcePoLineId(poLineId3)
      .withOrderType(Encumbrance.OrderType.ONGOING).withInitialAmountEncumbered(95d);
    Transaction transactionOngoing3 = new Transaction().withId(currEncumbrId3).withFromFundId(fundId3)
      .withEncumbrance(encumbranceOngoing3).withExpenseClassId(expClassId3);

    List<Transaction> encumbrances = List.of(transactionOneTime, transactionOngoing2, transactionOngoing3);
    doReturn(succeededFuture(encumbrances)).when(transactionService).getTransactions(anyString(), any());

    double exchangeEurToUsdRate = 1.0d;
    doReturn(succeededFuture(systemCurrency)).when(configurationEntriesService).getSystemCurrency(requestContext);
    String polCurrency = systemCurrency;
    ConversionQuery actQuery = ConversionQueryBuilder.of().setBaseCurrency(polCurrency).setTermCurrency(systemCurrency).set(RATE_KEY, exchangeEurToUsdRate).build();
    ExchangeRateProvider exchangeRateProvider = Mockito.mock(ManualExchangeRateProvider.class);
    ManualCurrencyConversion manualCurrencyConversion = new ManualCurrencyConversion(actQuery, exchangeRateProvider, ConversionContext.of());
    ExchangeRate exchangeRate = mock(ExchangeRate.class);

    doReturn(exchangeRateProvider).when(exchangeRateProviderResolver).resolve(any(ConversionQuery.class), eq(requestContext));
    doReturn(manualCurrencyConversion).when(exchangeRateProvider).getCurrencyConversion(any(ConversionQuery.class));
    doReturn(exchangeRate).when(exchangeRateProvider).getExchangeRate(any(ConversionQuery.class));
    when(exchangeRate.getContext()).thenReturn(ConversionContext.of());
    when(exchangeRate.getCurrency()).thenReturn(Monetary.getCurrency(systemCurrency));
    when(exchangeRate.getBaseCurrency()).thenReturn(Monetary.getCurrency(polCurrency));
    when(exchangeRate.getFactor()).thenReturn(new DefaultNumberValue(exchangeEurToUsdRate));

    Future<Void> future = orderRolloverService.rollover(ledgerFiscalYearRollover, requestContext);
    future.result();
    assertFalse(future.failed());

    assertThat(fundDistributionOneTime.getEncumbrance(), equalTo(currEncumbrId1));
    assertThat(fundDistributionOngoing2.getEncumbrance(), equalTo(currEncumbrId2));
    assertThat(fundDistributionOngoing3.getEncumbrance(), equalTo(currEncumbrId3));

    assertThat(costOneTime.getPoLineEstimatedPrice(), equalTo(60d));
    assertThat(costOngoing2.getPoLineEstimatedPrice(), equalTo(90d));
    assertThat(costOngoing3.getPoLineEstimatedPrice(), equalTo(95d));

    assertThat(costOneTime.getFyroAdjustmentAmount(), equalTo(-40d));
    assertThat(costOngoing2.getFyroAdjustmentAmount(), equalTo(-10d));
    assertThat(costOngoing3.getFyroAdjustmentAmount(), equalTo(-5d));
  }

  @Test
  @DisplayName("Should update FundDistributions amounts based on updatedEstimatedPrice")
  void shouldUpdateFundDistributionsAmountsBasedOnUpdatedEstimatedPrice() {
    String fromFiscalYearId = UUID.randomUUID().toString();
    String ledgerId = UUID.randomUUID().toString();
    String toFiscalYearId = UUID.randomUUID().toString();
    String fundId1 = UUID.randomUUID().toString();
    String fundId2 = UUID.randomUUID().toString();
    String orderId1 = UUID.randomUUID().toString();
    String poLineId1 = UUID.randomUUID().toString();
    String prevEncumbrId1 = UUID.randomUUID().toString();
    String prevEncumbrId2 = UUID.randomUUID().toString();
    String currEncumbrId1 = UUID.randomUUID().toString();
    String expClassId2 = UUID.randomUUID().toString();

    EncumbranceRollover ongoingEncumbranceBasedOnExpended = new EncumbranceRollover()
      .withOrderType(EncumbranceRollover.OrderType.ONGOING).withBasedOn(EncumbranceRollover.BasedOn.EXPENDED);
    EncumbranceRollover oneTimeEncumbrance = new EncumbranceRollover()
      .withOrderType(EncumbranceRollover.OrderType.ONE_TIME).withBasedOn(EncumbranceRollover.BasedOn.REMAINING);
    EncumbranceRollover ongoingEncumbranceBasedOnInitialAmount = new EncumbranceRollover()
      .withOrderType(EncumbranceRollover.OrderType.ONGOING).withBasedOn(EncumbranceRollover.BasedOn.INITIAL_AMOUNT);

    LedgerFiscalYearRollover ledgerFiscalYearRollover = new LedgerFiscalYearRollover()
      .withId(UUID.randomUUID().toString())
      .withFromFiscalYearId(fromFiscalYearId)
      .withLedgerId(ledgerId)
      .withToFiscalYearId(toFiscalYearId)
      .withEncumbrancesRollover(List.of(ongoingEncumbranceBasedOnExpended, oneTimeEncumbrance, ongoingEncumbranceBasedOnInitialAmount));

    List<Fund> funds = List.of(new Fund().withId(fundId1).withLedgerId(ledgerId), new Fund().withId(fundId2).withLedgerId(ledgerId));

    PurchaseOrder purchaseOrder1 = new PurchaseOrder().withId(orderId1).withWorkflowStatus(PurchaseOrder.WorkflowStatus.OPEN);
    List<PurchaseOrder> orders = List.of(purchaseOrder1);
    PurchaseOrderCollection purchaseOrderCollection = new PurchaseOrderCollection().withPurchaseOrders(orders).withTotalRecords(1);

    FundDistribution fundDistributionOneTime = new FundDistribution()
      .withFundId(fundId1)
      .withValue(50d)
      .withDistributionType(DistributionType.PERCENTAGE)
      .withEncumbrance(prevEncumbrId1);
    FundDistribution fundDistributionOngoing = new FundDistribution()
      .withFundId(fundId2)
      .withValue(297.5d)
      .withDistributionType(DistributionType.AMOUNT)
      .withEncumbrance(prevEncumbrId2)
      .withExpenseClassId(expClassId2);

    Cost costOneTime = new Cost().withListUnitPrice(595d).withQuantityPhysical(1).withCurrency("USD").withPoLineEstimatedPrice(595d);
    PoLine poLineOneTime = new PoLine().withId(poLineId1).withPurchaseOrderId(orderId1).withCost(costOneTime)
      .withFundDistribution(List.of(fundDistributionOneTime, fundDistributionOngoing));
    List<PoLine> poLines = List.of(poLineOneTime);

    doReturn(succeededFuture(funds)).when(fundService).getFundsByLedgerId(ledgerId, requestContext);
    doReturn(succeededFuture(purchaseOrderCollection)).when(purchaseOrderStorageService)
      .getPurchaseOrders(anyString(), anyInt(), anyInt(), any());
    doReturn(succeededFuture(poLines)).when(purchaseOrderLineService).getOrderLines(anyString(), anyInt(), anyInt(), any());
    doReturn(succeededFuture(null)).when(purchaseOrderLineService).saveOrderLines(eq(poLines), any());

    Encumbrance encumbranceOneTime = new Encumbrance().withSourcePurchaseOrderId(orderId1).withSourcePoLineId(poLineId1)
      .withOrderType(Encumbrance.OrderType.ONE_TIME).withInitialAmountEncumbered(580d);
    Transaction transactionOneTime = new Transaction().withId(currEncumbrId1).withFromFundId(fundId1)
      .withEncumbrance(encumbranceOneTime);
    List<Transaction> encumbrances = List.of(transactionOneTime);
    doReturn(succeededFuture(encumbrances)).when(transactionService).getTransactions(anyString(), any());

    double exchangeEurToUsdRate = 1.0d;
    doReturn(succeededFuture(systemCurrency)).when(configurationEntriesService).getSystemCurrency(requestContext);
    String polCurrency = systemCurrency;
    ConversionQuery actQuery = ConversionQueryBuilder.of().setBaseCurrency(polCurrency).setTermCurrency(systemCurrency).set(RATE_KEY, exchangeEurToUsdRate).build();
    ExchangeRateProvider exchangeRateProvider = Mockito.mock(ManualExchangeRateProvider.class);
    ManualCurrencyConversion manualCurrencyConversion = new ManualCurrencyConversion(actQuery, exchangeRateProvider, ConversionContext.of());
    ExchangeRate exchangeRate = mock(ExchangeRate.class);

    doReturn(exchangeRateProvider).when(exchangeRateProviderResolver).resolve(any(ConversionQuery.class), eq(requestContext));
    doReturn(manualCurrencyConversion).when(exchangeRateProvider).getCurrencyConversion(any(ConversionQuery.class));
    doReturn(exchangeRate).when(exchangeRateProvider).getExchangeRate(any(ConversionQuery.class));
    when(exchangeRate.getContext()).thenReturn(ConversionContext.of());
    when(exchangeRate.getCurrency()).thenReturn(Monetary.getCurrency(systemCurrency));
    when(exchangeRate.getBaseCurrency()).thenReturn(Monetary.getCurrency(polCurrency));
    when(exchangeRate.getFactor()).thenReturn(new DefaultNumberValue(exchangeEurToUsdRate));

    Future<Void> future = orderRolloverService.rollover(ledgerFiscalYearRollover, requestContext);
    future.result();
    assertFalse(future.failed());

    assertThat(fundDistributionOneTime.getEncumbrance(), equalTo(currEncumbrId1));

    assertThat(costOneTime.getPoLineEstimatedPrice(), equalTo(580d));
    assertThat(costOneTime.getFyroAdjustmentAmount(), equalTo(-15d));
  }

  @Test
  @DisplayName("Should update order lines cost And Encumbrance Links where Pol Currency and systemCurrency are different")
  void shouldUpdateOrderLinesCostAndEncumbranceLinksWithExchangeRateAndPolCurrencyVsSystemCurrencyAreDifferent() {
    String fromFiscalYearId = UUID.randomUUID().toString();
    String ledgerId = UUID.randomUUID().toString();
    String toFiscalYearId = UUID.randomUUID().toString();
    String fundId1 = UUID.randomUUID().toString();
    String fundId2 = UUID.randomUUID().toString();
    String fundId3 = UUID.randomUUID().toString();
    String orderId1 = UUID.randomUUID().toString();
    String orderId2 = UUID.randomUUID().toString();
    String orderId3 = UUID.randomUUID().toString();
    String poLineId1 = UUID.randomUUID().toString();
    String poLineId2 = UUID.randomUUID().toString();
    String poLineId3 = UUID.randomUUID().toString();
    String prevEncumbrId1 = UUID.randomUUID().toString();
    String prevEncumbrId2 = UUID.randomUUID().toString();
    String prevEncumbrId3 = UUID.randomUUID().toString();
    String currEncumbrId1 = UUID.randomUUID().toString();
    String currEncumbrId2 = UUID.randomUUID().toString();
    String currEncumbrId3 = UUID.randomUUID().toString();
    String expClassId2 = UUID.randomUUID().toString();
    String expClassId3 = UUID.randomUUID().toString();

    EncumbranceRollover ongoingEncumbranceBasedOnExpended = new EncumbranceRollover()
      .withOrderType(EncumbranceRollover.OrderType.ONGOING).withBasedOn(EncumbranceRollover.BasedOn.EXPENDED);
    EncumbranceRollover oneTimeEncumbrance = new EncumbranceRollover()
      .withOrderType(EncumbranceRollover.OrderType.ONE_TIME).withBasedOn(EncumbranceRollover.BasedOn.REMAINING);
    EncumbranceRollover ongoingEncumbranceBasedOnInitialAmount = new EncumbranceRollover()
      .withOrderType(EncumbranceRollover.OrderType.ONGOING).withBasedOn(EncumbranceRollover.BasedOn.INITIAL_AMOUNT);

    LedgerFiscalYearRollover ledgerFiscalYearRollover = new LedgerFiscalYearRollover()
      .withId(UUID.randomUUID().toString())
      .withFromFiscalYearId(fromFiscalYearId)
      .withLedgerId(ledgerId)
      .withToFiscalYearId(toFiscalYearId)
      .withEncumbrancesRollover(List.of(ongoingEncumbranceBasedOnExpended, oneTimeEncumbrance, ongoingEncumbranceBasedOnInitialAmount));

    List<Fund> funds = List.of(new Fund().withId(fundId1).withLedgerId(ledgerId), new Fund().withId(fundId2).withLedgerId(ledgerId),
      new Fund().withId(fundId3).withLedgerId(ledgerId));

    PurchaseOrder purchaseOrder1 = new PurchaseOrder().withId(orderId1).withWorkflowStatus(PurchaseOrder.WorkflowStatus.OPEN);
    PurchaseOrder purchaseOrder2 = new PurchaseOrder().withId(orderId2).withWorkflowStatus(PurchaseOrder.WorkflowStatus.OPEN);
    PurchaseOrder purchaseOrder3 = new PurchaseOrder().withId(orderId3).withWorkflowStatus(PurchaseOrder.WorkflowStatus.OPEN);

    List<PurchaseOrder> orders = List.of(purchaseOrder1, purchaseOrder2, purchaseOrder3);
    PurchaseOrderCollection purchaseOrderCollection = new PurchaseOrderCollection().withPurchaseOrders(orders).withTotalRecords(3);

    FundDistribution fundDistributionOneTime = new FundDistribution().withFundId(fundId1).withValue(100d)
      .withEncumbrance(prevEncumbrId1);
    FundDistribution fundDistributionOngoing2 = new FundDistribution().withFundId(fundId2).withValue(100d)
      .withEncumbrance(prevEncumbrId2).withExpenseClassId(expClassId2);
    FundDistribution fundDistributionOngoing3 = new FundDistribution().withFundId(fundId3).withValue(100d)
      .withEncumbrance(prevEncumbrId3).withExpenseClassId(expClassId3);

    String polCurrency = "EUR";
    Cost costOneTime = new Cost().withListUnitPrice(24.99d).withQuantityPhysical(1).withCurrency(polCurrency).withPoLineEstimatedPrice(24.99d);
    PoLine poLineOneTime = new PoLine().withId(poLineId1).withPurchaseOrderId(orderId1).withCost(costOneTime)
      .withFundDistribution(List.of(fundDistributionOneTime));

    Cost costOngoing2 = new Cost().withListUnitPrice(24.99d).withQuantityPhysical(1).withCurrency(polCurrency).withPoLineEstimatedPrice(24.99d);
    PoLine poLineOngoing2 = new PoLine().withId(poLineId2).withPurchaseOrderId(orderId2).withCost(costOngoing2)
      .withFundDistribution(List.of(fundDistributionOngoing2));

    Cost costOngoing3 = new Cost().withListUnitPrice(24.99d).withQuantityPhysical(1).withCurrency(polCurrency).withPoLineEstimatedPrice(24.99d);
    PoLine poLineOngoing3 = new PoLine().withId(poLineId3).withPurchaseOrderId(orderId3).withCost(costOngoing3)
      .withFundDistribution(List.of(fundDistributionOngoing3));
    List<PoLine> poLines = List.of(poLineOneTime, poLineOngoing2, poLineOngoing3);

    doReturn(succeededFuture(funds)).when(fundService).getFundsByLedgerId(ledgerId, requestContext);
    doReturn(succeededFuture(purchaseOrderCollection)).when(purchaseOrderStorageService)
      .getPurchaseOrders(anyString(), anyInt(), anyInt(), any());
    doReturn(succeededFuture(poLines)).when(purchaseOrderLineService).getOrderLines(anyString(), anyInt(), anyInt(), any());
    doReturn(succeededFuture(null)).when(purchaseOrderLineService).saveOrderLines(eq(poLines), any());

    Encumbrance encumbranceOneTime = new Encumbrance().withSourcePurchaseOrderId(orderId1).withSourcePoLineId(poLineId1)
      .withOrderType(Encumbrance.OrderType.ONE_TIME).withInitialAmountEncumbered(30.16d);
    Transaction transactionOneTime = new Transaction().withId(currEncumbrId1).withFromFundId(fundId1)
      .withEncumbrance(encumbranceOneTime);
    Encumbrance encumbranceOngoing2 = new Encumbrance().withSourcePurchaseOrderId(orderId2).withSourcePoLineId(poLineId2)
      .withOrderType(Encumbrance.OrderType.ONGOING).withInitialAmountEncumbered(30.16d);
    Transaction transactionOngoing2 = new Transaction().withId(currEncumbrId2).withFromFundId(fundId2)
      .withEncumbrance(encumbranceOngoing2).withExpenseClassId(expClassId2);
    Encumbrance encumbranceOngoing3 = new Encumbrance().withSourcePurchaseOrderId(orderId3).withSourcePoLineId(poLineId3)
      .withOrderType(Encumbrance.OrderType.ONGOING).withInitialAmountEncumbered(30.16d);
    Transaction transactionOngoing3 = new Transaction().withId(currEncumbrId3).withFromFundId(fundId3)
      .withEncumbrance(encumbranceOngoing3).withExpenseClassId(expClassId3);

    List<Transaction> encumbrances = List.of(transactionOneTime, transactionOngoing2, transactionOngoing3);
    doReturn(succeededFuture(encumbrances)).when(transactionService).getTransactions(anyString(), any());

    double exchangeEurToUsdRate = 0.82858d;
    doReturn(succeededFuture(systemCurrency)).when(configurationEntriesService).getSystemCurrency(requestContext);
    ConversionQuery actQuery = ConversionQueryBuilder.of().setBaseCurrency(polCurrency).setTermCurrency(systemCurrency).set(RATE_KEY, exchangeEurToUsdRate).build();
    ExchangeRateProvider exchangeRateProvider = Mockito.mock(ManualExchangeRateProvider.class);
    ManualCurrencyConversion manualCurrencyConversion = new ManualCurrencyConversion(actQuery, exchangeRateProvider, ConversionContext.of());
    ExchangeRate exchangeRate = mock(ExchangeRate.class);

    doReturn(exchangeRateProvider).when(exchangeRateProviderResolver).resolve(any(ConversionQuery.class), eq(requestContext));
    doReturn(manualCurrencyConversion).when(exchangeRateProvider).getCurrencyConversion(any(ConversionQuery.class));
    doReturn(exchangeRate).when(exchangeRateProvider).getExchangeRate(any(ConversionQuery.class));
    when(exchangeRate.getContext()).thenReturn(ConversionContext.of());
    when(exchangeRate.getCurrency()).thenReturn(Monetary.getCurrency(systemCurrency));
    when(exchangeRate.getBaseCurrency()).thenReturn(Monetary.getCurrency(polCurrency));
    when(exchangeRate.getFactor()).thenReturn(new DefaultNumberValue(exchangeEurToUsdRate));

    Future<Void> future = orderRolloverService.rollover(ledgerFiscalYearRollover, requestContext);
    future.result();
    assertFalse(future.failed());

    assertThat(fundDistributionOneTime.getEncumbrance(), equalTo(currEncumbrId1));
    assertThat(fundDistributionOngoing2.getEncumbrance(), equalTo(currEncumbrId2));
    assertThat(fundDistributionOngoing3.getEncumbrance(), equalTo(currEncumbrId3));

    assertThat(new BigDecimal(costOneTime.getPoLineEstimatedPrice()).setScale(2, RoundingMode.HALF_EVEN),
                equalTo(new BigDecimal(24.99d).setScale(2, RoundingMode.HALF_EVEN)));
    assertThat(new BigDecimal(costOngoing2.getPoLineEstimatedPrice()).setScale(2, RoundingMode.HALF_EVEN),
                equalTo(new BigDecimal(24.99d).setScale(2, RoundingMode.HALF_EVEN)));
    assertThat(new BigDecimal(costOngoing3.getPoLineEstimatedPrice()).setScale(2, RoundingMode.HALF_EVEN),
                equalTo(new BigDecimal(24.99d).setScale(2, RoundingMode.HALF_EVEN)));

    assertThat(costOneTime.getFyroAdjustmentAmount(), equalTo(0.0d));
    assertThat(costOngoing2.getFyroAdjustmentAmount(), equalTo(0.0d));
    assertThat(costOngoing3.getFyroAdjustmentAmount(), equalTo(0.0d));
  }

  @Test
  @DisplayName("Should remove encumbrance links and encumbrances if needed for closed orders")
  void shouldRemoveEncumbranceLinksAndEncumbrancesIfNeededForClosedOrders() {
    String fromFiscalYearId = UUID.randomUUID().toString();
    String ledgerId = UUID.randomUUID().toString();
    String toFiscalYearId = UUID.randomUUID().toString();
    String fundId = UUID.randomUUID().toString();
    String orderId = UUID.randomUUID().toString();
    String poLineId = UUID.randomUUID().toString();
    String prevEncumbrId = UUID.randomUUID().toString();
    String currEncumbrId = UUID.randomUUID().toString();

    EncumbranceRollover oneTimeEncumbrance = new EncumbranceRollover()
      .withOrderType(EncumbranceRollover.OrderType.ONE_TIME)
      .withBasedOn(EncumbranceRollover.BasedOn.REMAINING);

    LedgerFiscalYearRollover ledgerFiscalYearRollover = new LedgerFiscalYearRollover()
      .withId(UUID.randomUUID().toString())
      .withFromFiscalYearId(fromFiscalYearId)
      .withLedgerId(ledgerId)
      .withToFiscalYearId(toFiscalYearId)
      .withEncumbrancesRollover(singletonList(oneTimeEncumbrance));

    List<Fund> funds = singletonList(new Fund().withId(fundId).withLedgerId(ledgerId));

    PurchaseOrder purchaseOrder = new PurchaseOrder()
      .withId(orderId)
      .withWorkflowStatus(PurchaseOrder.WorkflowStatus.CLOSED)
      .withOrderType(ONE_TIME);
    List<PurchaseOrder> orders = singletonList(purchaseOrder);
    PurchaseOrderCollection purchaseOrderCollection = new PurchaseOrderCollection().withPurchaseOrders(orders).withTotalRecords(1);

    FundDistribution fundDistribution = new FundDistribution()
      .withFundId(fundId)
      .withValue(100d)
      .withDistributionType(DistributionType.PERCENTAGE)
      .withEncumbrance(prevEncumbrId);

    Cost cost = new Cost()
      .withListUnitPrice(595d)
      .withQuantityPhysical(1)
      .withCurrency("USD")
      .withPoLineEstimatedPrice(595d);
    PoLine poLine = new PoLine()
      .withId(poLineId)
      .withPurchaseOrderId(orderId)
      .withCost(cost)
      .withFundDistribution(singletonList(fundDistribution));
    List<PoLine> poLines = singletonList(poLine);

    doReturn(succeededFuture(funds)).when(fundService).getFundsByLedgerId(ledgerId, requestContext);
    doReturn(succeededFuture(purchaseOrderCollection)).when(purchaseOrderStorageService)
      .getPurchaseOrders(anyString(), anyInt(), anyInt(), any());
    doReturn(succeededFuture(poLines)).when(purchaseOrderLineService).getOrderLines(anyString(), anyInt(), anyInt(), any());
    doReturn(succeededFuture(null)).when(purchaseOrderLineService).saveOrderLines(eq(poLines), any());

    doReturn(succeededFuture(systemCurrency)).when(configurationEntriesService).getSystemCurrency(requestContext);

    Encumbrance encumbrance = new Encumbrance()
      .withSourcePurchaseOrderId(orderId)
      .withSourcePoLineId(poLineId)
      .withOrderType(Encumbrance.OrderType.ONE_TIME)
      .withInitialAmountEncumbered(580d);
    Transaction transaction = new Transaction()
      .withId(currEncumbrId)
      .withFromFundId(fundId)
      .withEncumbrance(encumbrance);
    List<Transaction> encumbrances = singletonList(transaction);
    doReturn(succeededFuture(encumbrances)).when(transactionService).getTransactions(anyString(), any());

    doReturn(succeededFuture(null)).when(transactionService).deleteTransactions(anyList(), any());

    Future<Void> future = orderRolloverService.rollover(ledgerFiscalYearRollover, requestContext);
    future.result();
    assertFalse(future.failed());

    assertThat(fundDistribution.getEncumbrance(), equalTo(null));
    verify(transactionService, times(1)).deleteTransactions(
      argThat(transactions -> transactions.size() == 1 && currEncumbrId.equals(transactions.get(0).getId())), any());
    verify(purchaseOrderLineService, times(1)).saveOrderLines(eq(poLines), any());
  }

}
