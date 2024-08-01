package org.folio.service.orders;

import static io.vertx.core.Future.failedFuture;
import static io.vertx.core.Future.succeededFuture;
import static java.lang.Thread.sleep;
import static java.util.Collections.singletonList;
import static org.folio.TestConfig.mockPort;
import static org.folio.TestConstants.X_OKAPI_TOKEN;
import static org.folio.TestConstants.X_OKAPI_USER_ID;
import static org.folio.rest.RestConstants.OKAPI_URL;
import static org.folio.rest.impl.PurchaseOrdersApiTest.X_OKAPI_TENANT;
import static org.folio.service.exchange.ExchangeRateProviderResolver.RATE_KEY;
import static org.hamcrest.MatcherAssert.assertThat;
import static org.hamcrest.Matchers.equalTo;
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
import java.util.ArrayList;
import java.util.HashMap;
import java.util.List;
import java.util.Map;
import java.util.UUID;

import javax.money.Monetary;
import javax.money.convert.ConversionContext;
import javax.money.convert.ConversionQuery;
import javax.money.convert.ConversionQueryBuilder;
import javax.money.convert.ExchangeRate;

import org.folio.rest.acq.model.finance.Encumbrance;
import org.folio.rest.acq.model.finance.Fund;
import org.folio.rest.acq.model.finance.LedgerFiscalYearRolloverError;
import org.folio.rest.acq.model.finance.LedgerFiscalYearRolloverErrorCollection;
import org.folio.rest.acq.model.finance.LedgerFiscalYearRolloverProgress;
import org.folio.rest.acq.model.finance.Transaction;
import org.folio.rest.core.models.RequestContext;
import org.folio.rest.jaxrs.model.Cost;
import org.folio.rest.jaxrs.model.EncumbranceRollover;
import org.folio.rest.jaxrs.model.FundDistribution;
import org.folio.rest.jaxrs.model.FundDistribution.DistributionType;
import org.folio.rest.jaxrs.model.LedgerFiscalYearRollover;
import org.folio.rest.jaxrs.model.PoLine;
import org.folio.rest.jaxrs.model.PoLineCollection;
import org.folio.rest.jaxrs.model.RolloverStatus;
import org.folio.service.caches.ConfigurationEntriesCache;
import org.folio.service.exchange.ExchangeRateProviderResolver;
import org.folio.service.exchange.ManualCurrencyConversion;
import org.folio.service.exchange.ManualExchangeRateProvider;
import org.folio.service.finance.FundService;
import org.folio.service.finance.rollover.LedgerRolloverErrorService;
import org.folio.service.finance.rollover.LedgerRolloverProgressService;
import org.folio.service.finance.transaction.TransactionService;
import org.javamoney.moneta.spi.DefaultNumberValue;
import org.junit.jupiter.api.BeforeEach;
import org.junit.jupiter.api.DisplayName;
import org.junit.jupiter.api.Test;
import org.junit.jupiter.api.extension.ExtendWith;
import org.mockito.ArgumentCaptor;
import org.mockito.InjectMocks;
import org.mockito.Captor;
import org.mockito.Mock;
import org.mockito.Mockito;
import org.mockito.MockitoAnnotations;

import io.vertx.core.Future;
import io.vertx.core.Vertx;
import io.vertx.junit5.VertxExtension;
import io.vertx.junit5.VertxTestContext;

@ExtendWith(VertxExtension.class)
public class OrderRolloverServiceTest {

  @InjectMocks
  private OrderRolloverService orderRolloverService;

  @Mock
  private FundService fundService;
  @Mock
  private TransactionService transactionService;
  @Mock
  private PurchaseOrderLineService purchaseOrderLineService;
  @Mock
  private ConfigurationEntriesCache configurationEntriesCache;
  @Mock
  private ExchangeRateProviderResolver exchangeRateProviderResolver;
  @Mock
  private LedgerRolloverProgressService ledgerRolloverProgressService;
  @Mock
  private LedgerRolloverErrorService ledgerRolloverErrorService;

  @Captor
  private ArgumentCaptor<List<PoLine>> argumentCaptor;

  private RequestContext requestContext;

  private final String systemCurrency = "USD";

  @BeforeEach
  public void initMocks() {
    MockitoAnnotations.openMocks(this);
    Map<String, String> okapiHeadersMock = new HashMap<>();
    okapiHeadersMock.put(OKAPI_URL, "http://localhost:" + mockPort);
    okapiHeadersMock.put(X_OKAPI_TOKEN.getName(), X_OKAPI_TOKEN.getValue());
    okapiHeadersMock.put(X_OKAPI_TENANT.getName(), X_OKAPI_TENANT.getValue());
    okapiHeadersMock.put(X_OKAPI_USER_ID.getName(), X_OKAPI_USER_ID.getValue());
    requestContext = new RequestContext(Vertx.vertx().getOrCreateContext(), okapiHeadersMock);
  }

  @Test
  @DisplayName("Should start preview rollover and check that start rollover was invoked")
  void shouldStartPreviewRolloverAndCheckThatStartRolloverWasInvoked(VertxTestContext vertxTestContext) {
    String fromFiscalYearId = UUID.randomUUID().toString();
    String ledgerId = UUID.randomUUID().toString();
    String toFiscalYearId = UUID.randomUUID().toString();

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

    LedgerFiscalYearRolloverProgress progress = new LedgerFiscalYearRolloverProgress().withId(UUID.randomUUID().toString())
      .withLedgerRolloverId(ledgerFiscalYearRollover.getId()).withOverallRolloverStatus(RolloverStatus.IN_PROGRESS)
      .withBudgetsClosingRolloverStatus(RolloverStatus.SUCCESS).withFinancialRolloverStatus(RolloverStatus.SUCCESS)
      .withOrdersRolloverStatus(RolloverStatus.IN_PROGRESS);
    LedgerFiscalYearRolloverErrorCollection errorCollection = new LedgerFiscalYearRolloverErrorCollection().withTotalRecords(0);

    doReturn(succeededFuture(progress)).when(ledgerRolloverProgressService)
      .getRolloversProgressByRolloverId(ledgerFiscalYearRollover.getId(), requestContext);
    doReturn(succeededFuture(errorCollection)).when(ledgerRolloverErrorService)
      .getRolloverErrorsByRolloverId(ledgerFiscalYearRollover.getId(), requestContext);
    doReturn(succeededFuture()).when(ledgerRolloverProgressService)
      .updateRolloverProgress(progress.withOrdersRolloverStatus(RolloverStatus.SUCCESS), requestContext);

    OrderRolloverService spy = Mockito.spy(orderRolloverService);
    doReturn(succeededFuture()).when(spy).startRollover(ledgerFiscalYearRollover, progress, requestContext);

    Future<Void> future = spy.rollover(ledgerFiscalYearRollover, requestContext);
    vertxTestContext.assertComplete(future)
      .onSuccess(result -> {
        verify(spy, times(1)).startRollover(ledgerFiscalYearRollover, progress, requestContext);
        verify(ledgerRolloverProgressService, times(1)).updateRolloverProgress(progress, requestContext);
        verify(ledgerRolloverProgressService, times(2)).getRolloversProgressByRolloverId(ledgerFiscalYearRollover.getId(), requestContext);
        vertxTestContext.completeNow();
      })
      .onFailure(vertxTestContext::failNow);
  }

  @Test
  @DisplayName("Should update order lines cost And Encumbrance Links where Pol Currency equals systemCurrency")
  void shouldUpdateOrderLinesCostAndEncumbranceLinksAndPolCurrencyVsSystemCurrencyTheSame(VertxTestContext vertxTestContext) {
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

    FundDistribution fundDistributionOneTime = new FundDistribution().withFundId(fundId1).withValue(100d)
      .withEncumbrance(prevEncumbrId1);
    FundDistribution fundDistributionOngoing2 = new FundDistribution().withFundId(fundId2).withValue(100d)
      .withEncumbrance(prevEncumbrId2).withExpenseClassId(expClassId2);
    FundDistribution fundDistributionOngoing3 = new FundDistribution().withFundId(fundId3).withValue(100d)
      .withEncumbrance(prevEncumbrId3).withExpenseClassId(expClassId3);

    Cost costOneTime = new Cost().withListUnitPrice(100d).withQuantityPhysical(1).withCurrency(systemCurrency).withPoLineEstimatedPrice(100d);
    PoLine poLineOneTime = new PoLine().withId(poLineId1).withPurchaseOrderId(orderId1).withCost(costOneTime)
      .withFundDistribution(List.of(fundDistributionOneTime));

    Cost costOngoing2 = new Cost().withListUnitPrice(100d).withQuantityPhysical(1).withCurrency(systemCurrency).withPoLineEstimatedPrice(100d);
    PoLine poLineOngoing2 = new PoLine().withId(poLineId2).withPurchaseOrderId(orderId2).withCost(costOngoing2)
      .withFundDistribution(List.of(fundDistributionOngoing2));

    Cost costOngoing3 = new Cost().withListUnitPrice(100d).withQuantityPhysical(1).withCurrency(systemCurrency).withPoLineEstimatedPrice(100d);
    PoLine poLineOngoing3 = new PoLine().withId(poLineId3).withPurchaseOrderId(orderId3).withCost(costOngoing3)
      .withFundDistribution(List.of(fundDistributionOngoing3));

    List<PoLine> poLines = List.of(poLineOneTime, poLineOngoing2, poLineOngoing3);
    PoLineCollection poLineCollection = new PoLineCollection()
      .withPoLines(poLines)
      .withTotalRecords(poLines.size());
    PoLineCollection emptyPoLineCollection = new PoLineCollection()
      .withPoLines(new ArrayList<>())
      .withTotalRecords(0);

    doReturn(succeededFuture(funds)).when(fundService).getFundsByLedgerId(ledgerId, requestContext);
    doReturn(succeededFuture(poLineCollection), succeededFuture(emptyPoLineCollection))
      .when(purchaseOrderLineService).getOrderLineCollection(anyString(), anyInt(), anyInt(), any());
    doReturn(succeededFuture(poLines)).when(purchaseOrderLineService).getOrderLines(anyString(), anyInt(), anyInt(), any());
    doReturn(succeededFuture()).when(purchaseOrderLineService).saveOrderLines(anyList(), any());

    LedgerFiscalYearRolloverProgress progress = new LedgerFiscalYearRolloverProgress().withId(UUID.randomUUID().toString())
      .withLedgerRolloverId(ledgerFiscalYearRollover.getId()).withOverallRolloverStatus(RolloverStatus.IN_PROGRESS)
      .withBudgetsClosingRolloverStatus(RolloverStatus.SUCCESS).withFinancialRolloverStatus(RolloverStatus.SUCCESS)
      .withOrdersRolloverStatus(RolloverStatus.IN_PROGRESS);
    LedgerFiscalYearRolloverErrorCollection errorCollection = new LedgerFiscalYearRolloverErrorCollection().withTotalRecords(0);

    doReturn(succeededFuture(progress)).when(ledgerRolloverProgressService).getRolloversProgressByRolloverId(ledgerFiscalYearRollover.getId(), requestContext);
    doReturn(succeededFuture(errorCollection)).when(ledgerRolloverErrorService).getRolloverErrorsByRolloverId(ledgerFiscalYearRollover.getId(), requestContext);
    doReturn(succeededFuture()).when(ledgerRolloverProgressService).updateRolloverProgress(progress.withOrdersRolloverStatus(RolloverStatus.SUCCESS), requestContext);

    Encumbrance encumbranceOneTime = new Encumbrance().withSourcePurchaseOrderId(orderId1).withSourcePoLineId(poLineId1)
      .withOrderType(Encumbrance.OrderType.ONE_TIME).withInitialAmountEncumbered(60d);
    Transaction transactionOneTime = new Transaction().withId(currEncumbrId1).withFromFundId(fundId1)
      .withEncumbrance(encumbranceOneTime).withCurrency(systemCurrency);
    Encumbrance encumbranceOngoing2 = new Encumbrance().withSourcePurchaseOrderId(orderId2).withSourcePoLineId(poLineId2)
      .withOrderType(Encumbrance.OrderType.ONGOING).withInitialAmountEncumbered(90d);
    Transaction transactionOngoing2 = new Transaction().withId(currEncumbrId2).withFromFundId(fundId2)
      .withEncumbrance(encumbranceOngoing2).withExpenseClassId(expClassId2).withCurrency(systemCurrency);
    Encumbrance encumbranceOngoing3 = new Encumbrance().withSourcePurchaseOrderId(orderId3).withSourcePoLineId(poLineId3)
      .withOrderType(Encumbrance.OrderType.ONGOING).withInitialAmountEncumbered(95d);
    Transaction transactionOngoing3 = new Transaction().withId(currEncumbrId3).withFromFundId(fundId3)
      .withEncumbrance(encumbranceOngoing3).withExpenseClassId(expClassId3).withCurrency(systemCurrency);

    List<Transaction> encumbrances = List.of(transactionOneTime, transactionOngoing2, transactionOngoing3);
    doReturn(succeededFuture(encumbrances)).when(transactionService).getTransactions(anyString(), any());

    double exchangeEurToUsdRate = 1.0d;
    doReturn(succeededFuture(systemCurrency)).when(configurationEntriesCache).getSystemCurrency(requestContext);
    String polCurrency = systemCurrency;
    ConversionQuery actQuery = ConversionQueryBuilder.of().setBaseCurrency(polCurrency).setTermCurrency(systemCurrency).set(RATE_KEY, exchangeEurToUsdRate).build();
    ManualExchangeRateProvider exchangeRateProvider = Mockito.mock(ManualExchangeRateProvider.class);
    ManualCurrencyConversion manualCurrencyConversion = new ManualCurrencyConversion(actQuery, exchangeRateProvider, ConversionContext.of(), ManualExchangeRateProvider.OperationMode.DIVIDE);
    ExchangeRate exchangeRate = mock(ExchangeRate.class);

    doReturn(exchangeRateProvider).when(exchangeRateProviderResolver).resolve(any(ConversionQuery.class), eq(requestContext), any(ManualExchangeRateProvider.OperationMode.class));
    doReturn(manualCurrencyConversion).when(exchangeRateProvider).getCurrencyConversion(any(ConversionQuery.class));
    doReturn(exchangeRate).when(exchangeRateProvider).getExchangeRate(any(ConversionQuery.class));
    when(exchangeRate.getContext()).thenReturn(ConversionContext.of());
    when(exchangeRate.getCurrency()).thenReturn(Monetary.getCurrency(systemCurrency));
    when(exchangeRate.getBaseCurrency()).thenReturn(Monetary.getCurrency(polCurrency));
    when(exchangeRate.getFactor()).thenReturn(new DefaultNumberValue(exchangeEurToUsdRate));

    Future<Void> future = orderRolloverService.startRollover(ledgerFiscalYearRollover, progress, requestContext);
    vertxTestContext.assertComplete(future)
      .onSuccess(result -> {
        assertThat(fundDistributionOneTime.getEncumbrance(), equalTo(currEncumbrId1));
        assertThat(fundDistributionOngoing2.getEncumbrance(), equalTo(currEncumbrId2));
        assertThat(fundDistributionOngoing3.getEncumbrance(), equalTo(currEncumbrId3));

        assertThat(costOneTime.getPoLineEstimatedPrice(), equalTo(60d));
        assertThat(costOngoing2.getPoLineEstimatedPrice(), equalTo(90d));
        assertThat(costOngoing3.getPoLineEstimatedPrice(), equalTo(95d));

        assertThat(costOneTime.getFyroAdjustmentAmount(), equalTo(-40d));
        assertThat(costOngoing2.getFyroAdjustmentAmount(), equalTo(-10d));
        assertThat(costOngoing3.getFyroAdjustmentAmount(), equalTo(-5d));
        vertxTestContext.completeNow();
      })
      .onFailure(vertxTestContext::failNow);
  }

  @Test
  @DisplayName("Should update FundDistributions amounts based on updatedEstimatedPrice")
  void shouldUpdateFundDistributionsAmountsBasedOnUpdatedEstimatedPrice(VertxTestContext vertxTestContext) {
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

    Cost costOneTime = new Cost().withListUnitPrice(595d).withQuantityPhysical(1).withCurrency(systemCurrency).withPoLineEstimatedPrice(595d);
    PoLine poLineOneTime = new PoLine().withId(poLineId1).withPurchaseOrderId(orderId1).withCost(costOneTime)
      .withFundDistribution(List.of(fundDistributionOneTime, fundDistributionOngoing));

    List<PoLine> poLines = List.of(poLineOneTime);
    PoLineCollection poLineCollection = new PoLineCollection()
      .withPoLines(poLines)
      .withTotalRecords(poLines.size());
    PoLineCollection emptyPoLineCollection = new PoLineCollection()
      .withPoLines(new ArrayList<>())
      .withTotalRecords(0);

    doReturn(succeededFuture(funds)).when(fundService).getFundsByLedgerId(ledgerId, requestContext);
    doReturn(succeededFuture(poLineCollection), succeededFuture(emptyPoLineCollection))
      .when(purchaseOrderLineService).getOrderLineCollection(anyString(), anyInt(), anyInt(), any());
    doReturn(succeededFuture(poLines)).when(purchaseOrderLineService).getOrderLines(anyString(), anyInt(), anyInt(), any());
    doReturn(succeededFuture()).when(purchaseOrderLineService).saveOrderLines(anyList(), any());

    LedgerFiscalYearRolloverProgress progress = new LedgerFiscalYearRolloverProgress().withId(UUID.randomUUID().toString())
      .withLedgerRolloverId(ledgerFiscalYearRollover.getId()).withOverallRolloverStatus(RolloverStatus.IN_PROGRESS)
      .withBudgetsClosingRolloverStatus(RolloverStatus.SUCCESS).withFinancialRolloverStatus(RolloverStatus.SUCCESS)
      .withOrdersRolloverStatus(RolloverStatus.IN_PROGRESS);
    LedgerFiscalYearRolloverErrorCollection errorCollection = new LedgerFiscalYearRolloverErrorCollection().withTotalRecords(0);

    doReturn(succeededFuture(progress)).when(ledgerRolloverProgressService).getRolloversProgressByRolloverId(ledgerFiscalYearRollover.getId(), requestContext);
    doReturn(succeededFuture(errorCollection)).when(ledgerRolloverErrorService).getRolloverErrorsByRolloverId(ledgerFiscalYearRollover.getId(), requestContext);
    doReturn(succeededFuture()).when(ledgerRolloverProgressService).updateRolloverProgress(progress.withOrdersRolloverStatus(RolloverStatus.SUCCESS), requestContext);

    Encumbrance encumbranceOneTime = new Encumbrance().withSourcePurchaseOrderId(orderId1).withSourcePoLineId(poLineId1)
      .withOrderType(Encumbrance.OrderType.ONE_TIME).withInitialAmountEncumbered(580d);
    Transaction transactionOneTime = new Transaction().withId(currEncumbrId1).withFromFundId(fundId1)
      .withEncumbrance(encumbranceOneTime).withCurrency(systemCurrency);
    List<Transaction> encumbrances = List.of(transactionOneTime);
    doReturn(succeededFuture(encumbrances)).when(transactionService).getTransactions(anyString(), any());

    double exchangeEurToUsdRate = 1.0d;
    doReturn(succeededFuture(systemCurrency)).when(configurationEntriesCache).getSystemCurrency(requestContext);
    String polCurrency = systemCurrency;
    ConversionQuery actQuery = ConversionQueryBuilder.of().setBaseCurrency(polCurrency).setTermCurrency(systemCurrency).set(RATE_KEY, exchangeEurToUsdRate).build();
    ManualExchangeRateProvider exchangeRateProvider = Mockito.mock(ManualExchangeRateProvider.class);
    ManualCurrencyConversion manualCurrencyConversion = new ManualCurrencyConversion(actQuery, exchangeRateProvider, ConversionContext.of(), ManualExchangeRateProvider.OperationMode.DIVIDE);
    ExchangeRate exchangeRate = mock(ExchangeRate.class);

    doReturn(exchangeRateProvider).when(exchangeRateProviderResolver).resolve(any(ConversionQuery.class), eq(requestContext), any(ManualExchangeRateProvider.OperationMode.class));
    doReturn(manualCurrencyConversion).when(exchangeRateProvider).getCurrencyConversion(any(ConversionQuery.class));
    doReturn(exchangeRate).when(exchangeRateProvider).getExchangeRate(any(ConversionQuery.class));
    when(exchangeRate.getContext()).thenReturn(ConversionContext.of());
    when(exchangeRate.getCurrency()).thenReturn(Monetary.getCurrency(systemCurrency));
    when(exchangeRate.getBaseCurrency()).thenReturn(Monetary.getCurrency(polCurrency));
    when(exchangeRate.getFactor()).thenReturn(new DefaultNumberValue(exchangeEurToUsdRate));

    Future<Void> future = orderRolloverService.startRollover(ledgerFiscalYearRollover, progress, requestContext);
    vertxTestContext.assertComplete(future)
      .onSuccess(result -> {
        assertThat(fundDistributionOneTime.getEncumbrance(), equalTo(currEncumbrId1));
        assertThat(costOneTime.getPoLineEstimatedPrice(), equalTo(580d));
        assertThat(costOneTime.getFyroAdjustmentAmount(), equalTo(-15d));
        vertxTestContext.completeNow();
      })
      .onFailure(vertxTestContext::failNow);
  }

  @Test
  @DisplayName("Should update order lines cost And Encumbrance Links where Pol Currency and systemCurrency are different")
  void shouldUpdateOrderLinesCostAndEncumbranceLinksWithExchangeRateAndPolCurrencyVsSystemCurrencyAreDifferent(VertxTestContext vertxTestContext) {
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
    PoLineCollection poLineCollection = new PoLineCollection()
      .withPoLines(poLines)
      .withTotalRecords(poLines.size());
    PoLineCollection emptyPoLineCollection = new PoLineCollection()
      .withPoLines(new ArrayList<>())
      .withTotalRecords(0);

    doReturn(succeededFuture(funds)).when(fundService).getFundsByLedgerId(ledgerId, requestContext);
    doReturn(succeededFuture(poLineCollection), succeededFuture(emptyPoLineCollection))
      .when(purchaseOrderLineService).getOrderLineCollection(anyString(), anyInt(), anyInt(), any());
    doReturn(succeededFuture(poLines)).when(purchaseOrderLineService).getOrderLines(anyString(), anyInt(), anyInt(), any());
    doReturn(succeededFuture()).when(purchaseOrderLineService).saveOrderLines(anyList(), any());

    LedgerFiscalYearRolloverProgress progress = new LedgerFiscalYearRolloverProgress().withId(UUID.randomUUID().toString())
      .withLedgerRolloverId(ledgerFiscalYearRollover.getId()).withOverallRolloverStatus(RolloverStatus.IN_PROGRESS)
      .withBudgetsClosingRolloverStatus(RolloverStatus.SUCCESS).withFinancialRolloverStatus(RolloverStatus.SUCCESS)
      .withOrdersRolloverStatus(RolloverStatus.IN_PROGRESS);
    LedgerFiscalYearRolloverErrorCollection errorCollection = new LedgerFiscalYearRolloverErrorCollection().withTotalRecords(0);

    doReturn(succeededFuture(progress)).when(ledgerRolloverProgressService).getRolloversProgressByRolloverId(ledgerFiscalYearRollover.getId(), requestContext);
    doReturn(succeededFuture(errorCollection)).when(ledgerRolloverErrorService).getRolloverErrorsByRolloverId(ledgerFiscalYearRollover.getId(), requestContext);
    doReturn(succeededFuture()).when(ledgerRolloverProgressService).updateRolloverProgress(progress.withOrdersRolloverStatus(RolloverStatus.SUCCESS), requestContext);

    Encumbrance encumbranceOneTime = new Encumbrance().withSourcePurchaseOrderId(orderId1).withSourcePoLineId(poLineId1)
      .withOrderType(Encumbrance.OrderType.ONE_TIME).withInitialAmountEncumbered(30.16d);
    Transaction transactionOneTime = new Transaction().withId(currEncumbrId1).withFromFundId(fundId1)
      .withEncumbrance(encumbranceOneTime).withCurrency(systemCurrency);
    Encumbrance encumbranceOngoing2 = new Encumbrance().withSourcePurchaseOrderId(orderId2).withSourcePoLineId(poLineId2)
      .withOrderType(Encumbrance.OrderType.ONGOING).withInitialAmountEncumbered(30.16d);
    Transaction transactionOngoing2 = new Transaction().withId(currEncumbrId2).withFromFundId(fundId2)
      .withEncumbrance(encumbranceOngoing2).withExpenseClassId(expClassId2).withCurrency(systemCurrency);
    Encumbrance encumbranceOngoing3 = new Encumbrance().withSourcePurchaseOrderId(orderId3).withSourcePoLineId(poLineId3)
      .withOrderType(Encumbrance.OrderType.ONGOING).withInitialAmountEncumbered(30.16d);
    Transaction transactionOngoing3 = new Transaction().withId(currEncumbrId3).withFromFundId(fundId3)
      .withEncumbrance(encumbranceOngoing3).withExpenseClassId(expClassId3).withCurrency(systemCurrency);

    List<Transaction> encumbrances = List.of(transactionOneTime, transactionOngoing2, transactionOngoing3);
    doReturn(succeededFuture(encumbrances)).when(transactionService).getTransactions(anyString(), any());

    double exchangeEurToUsdRate = 0.82858d;
    doReturn(succeededFuture(systemCurrency)).when(configurationEntriesCache).getSystemCurrency(requestContext);
    ConversionQuery actQuery = ConversionQueryBuilder.of().setBaseCurrency(polCurrency).setTermCurrency(systemCurrency).set(RATE_KEY, exchangeEurToUsdRate).build();
    ManualExchangeRateProvider exchangeRateProvider = Mockito.mock(ManualExchangeRateProvider.class);
    ManualCurrencyConversion manualCurrencyConversion = new ManualCurrencyConversion(actQuery, exchangeRateProvider, ConversionContext.of(), ManualExchangeRateProvider.OperationMode.DIVIDE);
    ExchangeRate exchangeRate = mock(ExchangeRate.class);

    doReturn(exchangeRateProvider).when(exchangeRateProviderResolver).resolve(any(ConversionQuery.class), eq(requestContext), any(ManualExchangeRateProvider.OperationMode.class));
    doReturn(manualCurrencyConversion).when(exchangeRateProvider).getCurrencyConversion(any(ConversionQuery.class));
    doReturn(exchangeRate).when(exchangeRateProvider).getExchangeRate(any(ConversionQuery.class));
    when(exchangeRate.getContext()).thenReturn(ConversionContext.of());
    when(exchangeRate.getCurrency()).thenReturn(Monetary.getCurrency(systemCurrency));
    when(exchangeRate.getBaseCurrency()).thenReturn(Monetary.getCurrency(polCurrency));
    when(exchangeRate.getFactor()).thenReturn(new DefaultNumberValue(exchangeEurToUsdRate));

    Future<Void> future = orderRolloverService.startRollover(ledgerFiscalYearRollover, progress, requestContext);
    vertxTestContext.assertComplete(future)
      .onSuccess(result -> {
        assertThat(fundDistributionOneTime.getEncumbrance(), equalTo(currEncumbrId1));
        assertThat(fundDistributionOngoing2.getEncumbrance(), equalTo(currEncumbrId2));
        assertThat(fundDistributionOngoing3.getEncumbrance(), equalTo(currEncumbrId3));

        assertThat(BigDecimal.valueOf(costOneTime.getPoLineEstimatedPrice()), equalTo(new BigDecimal("30.16")));
        assertThat(BigDecimal.valueOf(costOngoing2.getPoLineEstimatedPrice()), equalTo(new BigDecimal("30.16")));
        assertThat(BigDecimal.valueOf(costOngoing3.getPoLineEstimatedPrice()), equalTo(new BigDecimal("30.16")));

        assertThat(costOneTime.getFyroAdjustmentAmount(), equalTo(5.17d));
        assertThat(costOngoing2.getFyroAdjustmentAmount(), equalTo(5.17d));
        assertThat(costOngoing3.getFyroAdjustmentAmount(), equalTo(5.17d));
        vertxTestContext.completeNow();
      })
      .onFailure(vertxTestContext::failNow);



  }

  @Test
  @DisplayName("Should remove encumbrance links and encumbrances if needed for closed orders")
  void shouldRemoveEncumbranceLinksAndEncumbrancesIfNeededForClosedOrders(VertxTestContext vertxTestContext)
    throws InterruptedException {
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
    PoLineCollection poLineCollection = new PoLineCollection()
      .withPoLines(poLines)
      .withTotalRecords(poLines.size());
    PoLineCollection emptyPoLineCollection = new PoLineCollection()
      .withPoLines(new ArrayList<>())
      .withTotalRecords(0);

    LedgerFiscalYearRolloverProgress progress = new LedgerFiscalYearRolloverProgress().withId(UUID.randomUUID().toString())
      .withLedgerRolloverId(ledgerFiscalYearRollover.getId()).withOverallRolloverStatus(RolloverStatus.IN_PROGRESS)
      .withBudgetsClosingRolloverStatus(RolloverStatus.SUCCESS).withFinancialRolloverStatus(RolloverStatus.SUCCESS)
      .withOrdersRolloverStatus(RolloverStatus.IN_PROGRESS);
    LedgerFiscalYearRolloverErrorCollection errorCollection = new LedgerFiscalYearRolloverErrorCollection().withTotalRecords(0);

    doReturn(succeededFuture(progress)).when(ledgerRolloverProgressService).getRolloversProgressByRolloverId(ledgerFiscalYearRollover.getId(), requestContext);
    doReturn(succeededFuture(errorCollection)).when(ledgerRolloverErrorService).getRolloverErrorsByRolloverId(ledgerFiscalYearRollover.getId(), requestContext);
    doReturn(succeededFuture()).when(ledgerRolloverProgressService).updateRolloverProgress(progress.withOrdersRolloverStatus(RolloverStatus.SUCCESS), requestContext);
    doReturn(succeededFuture(funds)).when(fundService).getFundsByLedgerId(ledgerId, requestContext);

    doReturn(succeededFuture(emptyPoLineCollection), succeededFuture(poLineCollection))
      .when(purchaseOrderLineService).getOrderLineCollection(anyString(), anyInt(), anyInt(), any(RequestContext.class));
    doReturn(succeededFuture(poLineCollection.getPoLines()))
      .when(purchaseOrderLineService).getOrderLines(anyString(), anyInt(), anyInt(), any(RequestContext.class));

    doReturn(succeededFuture()).when(purchaseOrderLineService).saveOrderLines(anyList(), any());

    doReturn(succeededFuture(systemCurrency)).when(configurationEntriesCache).getSystemCurrency(requestContext);

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

    doReturn(succeededFuture(null)).when(transactionService).batchDelete(anyList(), any());

    Future<Void> future = orderRolloverService.startRollover(ledgerFiscalYearRollover, progress, requestContext);
    sleep(3000);
    vertxTestContext.assertComplete(future)
      .onSuccess(result -> {
        verify(transactionService, times(1)).batchDelete(
          argThat(transactionIds -> transactionIds.size() == 1 && currEncumbrId.equals(transactionIds.get(0))), any());

        verify(purchaseOrderLineService).saveOrderLines(argumentCaptor.capture(), any(RequestContext.class));
        assertThat(argumentCaptor.getAllValues().get(0).get(0).getFundDistribution().get(0).getEncumbrance(), equalTo(null));

        vertxTestContext.completeNow();
      })
      .onFailure(vertxTestContext::failNow);
  }

  @Test
  @DisplayName("Should fail when retrieve exchange rate provider and handle rollover error")
  void shouldFailWhenRetrieveExchangeRateProviderAndHandleRolloverError(VertxTestContext vertxTestContext) {
    String fromFiscalYearId = UUID.randomUUID().toString();
    String ledgerId = UUID.randomUUID().toString();
    String toFiscalYearId = UUID.randomUUID().toString();
    String fundId1 = UUID.randomUUID().toString();
    String fundId2 = UUID.randomUUID().toString();
    String fundId3 = UUID.randomUUID().toString();

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

    LedgerFiscalYearRolloverProgress progress = new LedgerFiscalYearRolloverProgress().withId(UUID.randomUUID().toString())
      .withLedgerRolloverId(ledgerFiscalYearRollover.getId()).withOverallRolloverStatus(RolloverStatus.IN_PROGRESS)
      .withBudgetsClosingRolloverStatus(RolloverStatus.SUCCESS).withFinancialRolloverStatus(RolloverStatus.SUCCESS)
      .withOrdersRolloverStatus(RolloverStatus.IN_PROGRESS);
    LedgerFiscalYearRolloverErrorCollection errorCollection = new LedgerFiscalYearRolloverErrorCollection().withTotalRecords(0);

    doReturn(succeededFuture(funds)).when(fundService).getFundsByLedgerId(ledgerId, requestContext);
    doReturn(succeededFuture(progress)).when(ledgerRolloverProgressService).getRolloversProgressByRolloverId(ledgerFiscalYearRollover.getId(), requestContext);
    doReturn(succeededFuture(errorCollection)).when(ledgerRolloverErrorService).getRolloverErrorsByRolloverId(ledgerFiscalYearRollover.getId(), requestContext);
    doReturn(succeededFuture()).when(ledgerRolloverProgressService).updateRolloverProgress(progress.withOrdersRolloverStatus(RolloverStatus.SUCCESS), requestContext);
    doReturn(succeededFuture(new LedgerFiscalYearRolloverError())).when(ledgerRolloverErrorService)
      .saveRolloverError(anyString(), any(Throwable.class), any(LedgerFiscalYearRolloverError.ErrorType.class), anyString(), eq(requestContext));
    doReturn(failedFuture("Error loading system currency from cache")).when(configurationEntriesCache).getSystemCurrency(requestContext);

    Future<Void> future = orderRolloverService.startRollover(ledgerFiscalYearRollover, progress, requestContext);
    vertxTestContext.assertFailure(future)
      .onComplete(result -> {
        verify(ledgerRolloverErrorService, times(1)).saveRolloverError(anyString(),
          any(Throwable.class), any(LedgerFiscalYearRolloverError.ErrorType.class), anyString(), eq(requestContext));
        verify(ledgerRolloverProgressService, times(1)).updateRolloverProgress(progress, requestContext);
        vertxTestContext.completeNow();
      });
  }

}
