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
import static org.hamcrest.MatcherAssert.assertThat;
import static org.hamcrest.Matchers.equalTo;
import static org.mockito.ArgumentMatchers.any;
import static org.mockito.ArgumentMatchers.anyInt;
import static org.mockito.ArgumentMatchers.anyList;
import static org.mockito.ArgumentMatchers.anyString;
import static org.mockito.ArgumentMatchers.argThat;
import static org.mockito.ArgumentMatchers.eq;
import static org.mockito.Mockito.doReturn;
import static org.mockito.Mockito.times;
import static org.mockito.Mockito.verify;

import java.math.BigDecimal;
import java.util.ArrayList;
import java.util.HashMap;
import java.util.List;
import java.util.Map;
import java.util.UUID;

import java.util.stream.Stream;

import org.folio.models.PoLineEncumbrancesHolder;
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
import org.folio.service.finance.FundService;
import org.folio.service.finance.rollover.LedgerRolloverErrorService;
import org.folio.service.finance.rollover.LedgerRolloverProgressService;
import org.folio.service.finance.transaction.TransactionService;
import org.folio.utils.CurrencyConversionMockHelper;
import org.junit.jupiter.api.Assertions;
import org.junit.jupiter.api.BeforeEach;
import org.junit.jupiter.api.DisplayName;
import org.junit.jupiter.api.Test;
import org.junit.jupiter.api.extension.ExtendWith;
import org.junit.jupiter.params.ParameterizedTest;
import org.junit.jupiter.params.provider.Arguments;
import org.junit.jupiter.params.provider.MethodSource;
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

    Double fundAmount = 100d;
    Double nopExchangeRate = 1.0d;

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

    FundDistribution fundDistributionOneTime = new FundDistribution().withFundId(fundId1).withValue(fundAmount)
      .withEncumbrance(prevEncumbrId1);
    FundDistribution fundDistributionOngoing2 = new FundDistribution().withFundId(fundId2).withValue(fundAmount)
      .withEncumbrance(prevEncumbrId2).withExpenseClassId(expClassId2);
    FundDistribution fundDistributionOngoing3 = new FundDistribution().withFundId(fundId3).withValue(fundAmount)
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

    var conversionHelper = new CurrencyConversionMockHelper(configurationEntriesCache, exchangeRateProviderResolver, requestContext);
    conversionHelper.mockExchangeRateProviderResolver(CurrencyConversionMockHelper.ExchangeRateProviderMode.FINANCE_API, systemCurrency, systemCurrency, nopExchangeRate);

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

    Double nopExchangeRate = 1.0d;

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

    var conversionHelper = new CurrencyConversionMockHelper(configurationEntriesCache, exchangeRateProviderResolver, requestContext);
    conversionHelper.mockExchangeRateProviderResolver(CurrencyConversionMockHelper.ExchangeRateProviderMode.FINANCE_API, systemCurrency, systemCurrency, nopExchangeRate);

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

    String polCurrency = "AUD";
    Double poLineAmount = 10.00d;
    Double transactionAmount = 6.51d;
    Double exchangeAudToUsdRate = 1.536d;
    Double fundAmount = 100d;
    Double rolloverAdjustmentAmount = 0.0d;

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

    FundDistribution fundDistributionOneTime = new FundDistribution().withFundId(fundId1).withValue(fundAmount)
      .withEncumbrance(prevEncumbrId1);
    FundDistribution fundDistributionOngoing2 = new FundDistribution().withFundId(fundId2).withValue(fundAmount)
      .withEncumbrance(prevEncumbrId2).withExpenseClassId(expClassId2);
    FundDistribution fundDistributionOngoing3 = new FundDistribution().withFundId(fundId3).withValue(fundAmount)
      .withEncumbrance(prevEncumbrId3).withExpenseClassId(expClassId3);

    Cost costOneTime = new Cost().withListUnitPrice(poLineAmount).withQuantityPhysical(1).withCurrency(polCurrency).withPoLineEstimatedPrice(poLineAmount);
    PoLine poLineOneTime = new PoLine().withId(poLineId1).withPurchaseOrderId(orderId1).withCost(costOneTime)
      .withFundDistribution(List.of(fundDistributionOneTime));

    Cost costOngoing2 = new Cost().withListUnitPrice(poLineAmount).withQuantityPhysical(1).withCurrency(polCurrency).withPoLineEstimatedPrice(poLineAmount);
    PoLine poLineOngoing2 = new PoLine().withId(poLineId2).withPurchaseOrderId(orderId2).withCost(costOngoing2)
      .withFundDistribution(List.of(fundDistributionOngoing2));

    Cost costOngoing3 = new Cost().withListUnitPrice(poLineAmount).withQuantityPhysical(1).withCurrency(polCurrency).withPoLineEstimatedPrice(poLineAmount);
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
      .withOrderType(Encumbrance.OrderType.ONE_TIME).withInitialAmountEncumbered(transactionAmount);
    Transaction transactionOneTime = new Transaction().withId(currEncumbrId1).withFromFundId(fundId1)
      .withEncumbrance(encumbranceOneTime);
    Encumbrance encumbranceOngoing2 = new Encumbrance().withSourcePurchaseOrderId(orderId2).withSourcePoLineId(poLineId2)
      .withOrderType(Encumbrance.OrderType.ONGOING).withInitialAmountEncumbered(transactionAmount);
    Transaction transactionOngoing2 = new Transaction().withId(currEncumbrId2).withFromFundId(fundId2)
      .withEncumbrance(encumbranceOngoing2).withExpenseClassId(expClassId2);
    Encumbrance encumbranceOngoing3 = new Encumbrance().withSourcePurchaseOrderId(orderId3).withSourcePoLineId(poLineId3)
      .withOrderType(Encumbrance.OrderType.ONGOING).withInitialAmountEncumbered(transactionAmount);
    Transaction transactionOngoing3 = new Transaction().withId(currEncumbrId3).withFromFundId(fundId3)
      .withEncumbrance(encumbranceOngoing3).withExpenseClassId(expClassId3);

    List<Transaction> encumbrances = List.of(transactionOneTime, transactionOngoing2, transactionOngoing3);
    doReturn(succeededFuture(encumbrances)).when(transactionService).getTransactions(anyString(), any());

    var conversionHelper = new CurrencyConversionMockHelper(configurationEntriesCache, exchangeRateProviderResolver, requestContext);
    conversionHelper.mockExchangeRateProviderResolver(CurrencyConversionMockHelper.ExchangeRateProviderMode.FINANCE_API, systemCurrency, polCurrency, exchangeAudToUsdRate);

    Future<Void> future = orderRolloverService.startRollover(ledgerFiscalYearRollover, progress, requestContext);
    vertxTestContext.assertComplete(future)
      .onSuccess(result -> {
        assertThat(fundDistributionOneTime.getEncumbrance(), equalTo(currEncumbrId1));
        assertThat(fundDistributionOngoing2.getEncumbrance(), equalTo(currEncumbrId2));
        assertThat(fundDistributionOngoing3.getEncumbrance(), equalTo(currEncumbrId3));

        assertThat(BigDecimal.valueOf(costOneTime.getPoLineEstimatedPrice()), equalTo(BigDecimal.valueOf(poLineAmount)));
        assertThat(BigDecimal.valueOf(costOngoing2.getPoLineEstimatedPrice()), equalTo(BigDecimal.valueOf(poLineAmount)));
        assertThat(BigDecimal.valueOf(costOngoing3.getPoLineEstimatedPrice()), equalTo(BigDecimal.valueOf(poLineAmount)));

        assertThat(costOneTime.getFyroAdjustmentAmount(), equalTo(rolloverAdjustmentAmount));
        assertThat(costOngoing2.getFyroAdjustmentAmount(), equalTo(rolloverAdjustmentAmount));
        assertThat(costOngoing3.getFyroAdjustmentAmount(), equalTo(rolloverAdjustmentAmount));
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

  private static Stream<Arguments> shouldConvertTotalAmountUsingCorrectExchangeRateProviderArgs() {
    // FINANCE_API uses multiplication and 2 ECB/IMF exchanges rates, e.g. USD->AUD using 1.536 and AUD->USD using 0.651
    // MANUAL uses division and 1 exchange rate (the initial exchange on the POL used to create an Encumbrance Transaction)
    return Stream.of(
      Arguments.of("USD", "USD", 10d, 10d, 10d, 1d, 100d, CurrencyConversionMockHelper.ExchangeRateProviderMode.FINANCE_API),
      Arguments.of("USD", "AUD", 10d, 6.51d, 10d, 1.536d, 100d, CurrencyConversionMockHelper.ExchangeRateProviderMode.FINANCE_API),
      Arguments.of("AUD", "USD", 6.51d, 10d, 6.51d, 0.651d, 100d, CurrencyConversionMockHelper.ExchangeRateProviderMode.FINANCE_API),
      Arguments.of("USD", "UZS", 10d, 9d, 10d, 0.9d, 100d, CurrencyConversionMockHelper.ExchangeRateProviderMode.MANUAL)
    );
  }

  @ParameterizedTest
  @MethodSource("shouldConvertTotalAmountUsingCorrectExchangeRateProviderArgs")
  @DisplayName("Should convert total amount using correct exchange rate provider")
  void shouldConvertTotalAmountUsingCorrectExchangeRateProviderMode(String fromCurrency, String toCurrency, Double poLineEstimatedPrice, Double initialAmountEncumbered,
                                                                    Double totalAmount, Double exchangeRateAmount, Double fundAllocation,
                                                                    CurrencyConversionMockHelper.ExchangeRateProviderMode exchangeRateProviderMode) {
    var purchaseOrderId = UUID.randomUUID().toString();
    var poLineId = UUID.randomUUID().toString();
    var transactionId = UUID.randomUUID().toString();
    var encumbranceId = UUID.randomUUID().toString();
    var fundId = UUID.randomUUID().toString();

    var fundDistribution = new FundDistribution()
      .withFundId(fundId)
      .withValue(fundAllocation)
      .withDistributionType(DistributionType.PERCENTAGE);
    var cost = new Cost()
      .withCurrency(toCurrency)
      .withPoLineEstimatedPrice(poLineEstimatedPrice);
    if (exchangeRateProviderMode == CurrencyConversionMockHelper.ExchangeRateProviderMode.MANUAL) {
      cost.setExchangeRate(exchangeRateAmount);
    }

    var poLine = new PoLine()
      .withId(encumbranceId)
      .withId(poLineId)
      .withPurchaseOrderId(purchaseOrderId)
      .withCost(cost)
      .withFundDistribution(singletonList(fundDistribution));

    var encumbrance = new Encumbrance()
      .withSourcePurchaseOrderId(purchaseOrderId)
      .withSourcePoLineId(poLineId)
      .withOrderType(Encumbrance.OrderType.ONE_TIME)
      .withInitialAmountEncumbered(initialAmountEncumbered);
    var transaction = new Transaction()
      .withId(transactionId)
      .withFromFundId(fundId)
      .withCurrency(fromCurrency)
      .withEncumbrance(encumbrance);

    var conversionHelper = new CurrencyConversionMockHelper(configurationEntriesCache, exchangeRateProviderResolver, requestContext);
    conversionHelper.mockExchangeRateProviderResolver(exchangeRateProviderMode, fromCurrency, toCurrency, exchangeRateAmount);

    var currencyConversion = orderRolloverService.retrieveCurrencyConversion(fromCurrency, poLine.getCost(), requestContext);

    var holder = new PoLineEncumbrancesHolder(poLine)
      .withEncumbrances(singletonList(transaction))
      .withCurrencyConversion(currencyConversion)
      .withSystemCurrency(fromCurrency);

    var totalAmountAfterConversion = orderRolloverService.calculateTotalInitialAmountEncumbered(holder);

    Assertions.assertEquals(BigDecimal.valueOf(totalAmount), totalAmountAfterConversion);
  }
}
