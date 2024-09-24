package org.folio.service.orders;

import static io.vertx.core.Future.succeededFuture;
import static org.folio.TestUtils.getMockAsJson;
import static org.folio.helper.PurchaseOrderHelperTest.ORDER_PATH;
import static org.folio.rest.core.exceptions.ErrorCodes.ROLLOVER_NOT_COMPLETED;
import static org.folio.rest.core.exceptions.ErrorCodes.ENCUMBRANCES_FOR_RE_ENCUMBER_NOT_FOUND;
import static org.folio.rest.jaxrs.model.RolloverStatus.ERROR;
import static org.folio.rest.jaxrs.model.RolloverStatus.IN_PROGRESS;
import static org.folio.rest.jaxrs.model.RolloverStatus.NOT_STARTED;
import static org.folio.rest.jaxrs.model.RolloverStatus.SUCCESS;
import static org.hamcrest.MatcherAssert.assertThat;
import static org.hamcrest.Matchers.containsString;
import static org.hamcrest.Matchers.hasSize;
import static org.hamcrest.Matchers.instanceOf;
import static org.junit.jupiter.api.Assertions.assertEquals;
import static org.junit.jupiter.api.Assertions.assertFalse;
import static org.junit.jupiter.api.Assertions.assertNotNull;
import static org.junit.jupiter.api.Assertions.assertTrue;
import static org.mockito.ArgumentMatchers.any;
import static org.mockito.ArgumentMatchers.anyList;
import static org.mockito.ArgumentMatchers.anyString;
import static org.mockito.ArgumentMatchers.eq;
import static org.mockito.Mockito.doNothing;
import static org.mockito.Mockito.doReturn;
import static org.mockito.Mockito.when;

import java.util.Arrays;
import java.util.Collections;
import java.util.List;
import java.util.UUID;

import javax.money.convert.ConversionQuery;
import javax.money.convert.ConversionQueryBuilder;

import org.folio.models.CompositeOrderRetrieveHolder;
import org.folio.models.ReEncumbranceHolder;
import org.folio.rest.acq.model.finance.Budget;
import org.folio.rest.acq.model.finance.Encumbrance;
import org.folio.rest.acq.model.finance.Ledger;
import org.folio.rest.acq.model.finance.LedgerFiscalYearRolloverError;
import org.folio.rest.acq.model.finance.LedgerFiscalYearRolloverErrorCollection;
import org.folio.rest.acq.model.finance.LedgerFiscalYearRolloverProgress;
import org.folio.rest.acq.model.finance.Transaction;
import org.folio.rest.core.exceptions.HttpException;
import org.folio.rest.core.models.RequestContext;
import org.folio.rest.jaxrs.model.CompositePoLine;
import org.folio.rest.jaxrs.model.CompositePurchaseOrder;
import org.folio.rest.jaxrs.model.Cost;
import org.folio.rest.jaxrs.model.EncumbranceRollover;
import org.folio.rest.jaxrs.model.FundDistribution;
import org.folio.rest.jaxrs.model.LedgerFiscalYearRollover;
import org.folio.rest.jaxrs.model.LedgerFiscalYearRolloverCollection;
import org.folio.rest.jaxrs.model.Parameter;
import org.folio.service.exchange.ExchangeRateProviderResolver;
import org.folio.service.exchange.ManualExchangeRateProvider;
import org.folio.service.finance.budget.BudgetRestrictionService;
import org.folio.service.finance.rollover.LedgerRolloverErrorService;
import org.folio.service.finance.rollover.LedgerRolloverProgressService;
import org.folio.service.finance.rollover.LedgerRolloverService;
import org.folio.service.finance.transaction.TransactionService;
import org.junit.jupiter.api.AfterEach;
import org.junit.jupiter.api.BeforeEach;
import org.junit.jupiter.api.Test;
import org.junit.jupiter.api.extension.ExtendWith;
import org.mockito.AdditionalMatchers;
import org.mockito.InjectMocks;
import org.mockito.Mock;
import org.mockito.MockitoAnnotations;
import org.mockito.Spy;

import io.vertx.core.Future;
import io.vertx.junit5.VertxExtension;
import io.vertx.junit5.VertxTestContext;

@ExtendWith(VertxExtension.class)
public class OrderReEncumberServiceTest {

  @InjectMocks
  private OrderReEncumberService orderReEncumberService;

  @Mock
  private PurchaseOrderStorageService purchaseOrderStorageService;
  @Mock
  private LedgerRolloverService ledgerRolloverService;
  @Mock
  private LedgerRolloverErrorService ledgerRolloverErrorService;
  @Mock
  private LedgerRolloverProgressService ledgerRolloverProgressService;
  @Mock
  private PurchaseOrderLineService purchaseOrderLineService;
  @Mock
  private BudgetRestrictionService budgetRestrictionService;
  @Spy
  private ManualExchangeRateProvider exchangeRateProvider;
  @Mock
  private ExchangeRateProviderResolver exchangeRateProviderResolver;
  @Mock
  private TransactionService transactionService;
  @Mock
  private ReEncumbranceHoldersBuilder spyReEncumbranceHoldersBuilder;

  @Mock
  private RequestContext requestContext;

  private AutoCloseable mockitoMocks;
  private LedgerFiscalYearRolloverProgress notStarted;
  private LedgerFiscalYearRolloverProgress inProgress;
  private LedgerFiscalYearRolloverProgress success;
  private LedgerFiscalYearRolloverProgress error;

  @BeforeEach
  public void initMocks(){
    mockitoMocks = MockitoAnnotations.openMocks(this);
    notStarted = new LedgerFiscalYearRolloverProgress().withOverallRolloverStatus(NOT_STARTED);
    inProgress = new LedgerFiscalYearRolloverProgress().withOverallRolloverStatus(IN_PROGRESS);
    success = new LedgerFiscalYearRolloverProgress().withOverallRolloverStatus(SUCCESS);
    error = new LedgerFiscalYearRolloverProgress().withOverallRolloverStatus(ERROR);
  }

  @AfterEach
  public void resetMocks() throws Exception {
    mockitoMocks.close();
  }


  @Test
  void testPopulateNeedReEncumberField() {
    // given
    CompositePurchaseOrder order = getMockAsJson(ORDER_PATH).mapTo(CompositePurchaseOrder.class);
    String ledgerId = UUID.randomUUID().toString();

    LedgerFiscalYearRollover rollover = new LedgerFiscalYearRollover().withId(UUID.randomUUID().toString())
        .withLedgerId(ledgerId);

    LedgerFiscalYearRolloverErrorCollection ledgerFiscalYearRolloverErrors = new LedgerFiscalYearRolloverErrorCollection()
        .withLedgerFiscalYearRolloverErrors(Collections.singletonList(new LedgerFiscalYearRolloverError()));

    CompositePoLine line = order.getCompositePoLines().get(0);
    FundDistribution fundDistribution1 = line.getFundDistribution().get(0);
    String fiscalYearId = UUID.randomUUID().toString();

    ReEncumbranceHolder holder1 = new ReEncumbranceHolder().withPurchaseOrder(order)
        .withPoLine(line)
        .withFundDistribution(fundDistribution1)
        .withRollover(rollover)
        .withLedgerId(ledgerId)
        .withCurrentFiscalYearId(fiscalYearId);

    FundDistribution fundDistribution2 = line.getFundDistribution().get(1);
    ReEncumbranceHolder holder2 = new ReEncumbranceHolder().withPurchaseOrder(order)
        .withPoLine(order.getCompositePoLines().get(0))
        .withFundDistribution(fundDistribution2)
        .withRollover(rollover)
        .withLedgerId(ledgerId)
        .withCurrentFiscalYearId(fiscalYearId);

    FundDistribution fundDistribution3 = line.getFundDistribution().get(1);
    ReEncumbranceHolder holder3 = new ReEncumbranceHolder().withPurchaseOrder(order)
        .withPoLine(order.getCompositePoLines().get(0))
        .withFundDistribution(fundDistribution3)
        .withRollover(rollover)
        .withLedgerId(ledgerId)
        .withCurrentFiscalYearId(fiscalYearId);

    List<LedgerFiscalYearRolloverProgress> progresses = Collections.singletonList(success);

    List<ReEncumbranceHolder> holders = List.of(holder1, holder2, holder3);

    // LedgerFiscalYearRolloverErrorCollection is not empty. Expected "needReEncumber" = true
    doReturn(holders).when(spyReEncumbranceHoldersBuilder).buildReEncumbranceHoldersWithOrdersData(any());
    doReturn(succeededFuture(holders)).when(spyReEncumbranceHoldersBuilder).getLedgerIds(any(), any());
    doReturn(succeededFuture(holders)).when(spyReEncumbranceHoldersBuilder).withRollovers(any(), any());
    doReturn(succeededFuture(progresses)).when(ledgerRolloverProgressService).getRolloversProgress(anyString(), any());
    doReturn(succeededFuture(ledgerFiscalYearRolloverErrors)).when(ledgerRolloverErrorService).getLedgerFyRolloverErrors(any(), any());
    CompositeOrderRetrieveHolder holder = new CompositeOrderRetrieveHolder(order);
    CompositePurchaseOrder compOrder = orderReEncumberService.populate(holder, requestContext).result().getOrder();
    assertTrue(compOrder.getNeedReEncumber());

    // LedgerFiscalYearRolloverErrorCollection is empty. Expected "needReEncumber" = false
    doReturn(succeededFuture(new LedgerFiscalYearRolloverErrorCollection()))
        .when(ledgerRolloverErrorService).getLedgerFyRolloverErrors(any(), any());
    compOrder = orderReEncumberService.populate(holder, requestContext).result().getOrder();
    assertFalse(compOrder.getNeedReEncumber());

    // LedgerFyRollover not exists. Expected "needReEncumber" = false
    doReturn(succeededFuture(new LedgerFiscalYearRolloverCollection())).when(ledgerRolloverService).getLedgerFyRollovers(anyString(), anyString(), any());
    compOrder = orderReEncumberService.populate(holder, requestContext).result().getOrder();
    assertFalse(compOrder.getNeedReEncumber());

  }

  @Test
  void shouldThrowHttpExceptionWithRolloverNotCompletedCodeWhenAtLeastOneRolloverHasProgressNotStartedOrInProgress(VertxTestContext vertxTestContext) {
    String orderId = UUID.randomUUID().toString();

    String fundId1 = UUID.randomUUID().toString();
    String fundId2 = UUID.randomUUID().toString();
    String fundId3 = UUID.randomUUID().toString();

    String ledgerId1 = UUID.randomUUID().toString();
    String ledgerId2 = UUID.randomUUID().toString();
    String ledgerId3 = UUID.randomUUID().toString();

    String rolloverId1 = UUID.randomUUID().toString();
    String rolloverId2 = UUID.randomUUID().toString();
    String rolloverId3 = UUID.randomUUID().toString();


    ReEncumbranceHolder holder1 = new ReEncumbranceHolder()
        .withLedgerId(ledgerId1)
        .withRollover(new LedgerFiscalYearRollover().withId(rolloverId1).withLedgerId(ledgerId1))
        .withFundDistribution(new FundDistribution().withFundId(fundId1));
    ReEncumbranceHolder holder2 = new ReEncumbranceHolder()
        .withLedgerId(ledgerId2)
        .withRollover(new LedgerFiscalYearRollover().withId(rolloverId2).withLedgerId(ledgerId2))
        .withFundDistribution(new FundDistribution().withFundId(fundId2));
    ReEncumbranceHolder holder3 = new ReEncumbranceHolder()
        .withLedgerId(ledgerId3)
        .withRollover(new LedgerFiscalYearRollover().withId(rolloverId3).withLedgerId(ledgerId3))
        .withFundDistribution(new FundDistribution().withFundId(fundId3));

    List<ReEncumbranceHolder> holders = List.of(holder1, holder2, holder3);

    when(purchaseOrderStorageService.getCompositeOrderById(anyString(), any())).thenReturn(succeededFuture(new CompositePurchaseOrder()));
    when(spyReEncumbranceHoldersBuilder.buildReEncumbranceHoldersWithOrdersData(any())).thenReturn(holders);
    when(spyReEncumbranceHoldersBuilder.withFinances(any(), any())).thenReturn(succeededFuture(null));
    when(spyReEncumbranceHoldersBuilder.withRollovers(any(), any())).thenReturn(succeededFuture(holders));
    when(spyReEncumbranceHoldersBuilder.withEncumbranceRollover(any())).thenReturn(holders);
    when(ledgerRolloverProgressService.getRolloversProgress(eq(rolloverId1), any()))
        .thenReturn(succeededFuture(Collections.singletonList(notStarted)));
    when(ledgerRolloverProgressService.getRolloversProgress(eq(rolloverId2), any()))
        .thenReturn(succeededFuture(Collections.singletonList(inProgress)));
    when(ledgerRolloverProgressService.getRolloversProgress(eq(rolloverId3), any()))
        .thenReturn(succeededFuture(Collections.singletonList(success)));

    var future = orderReEncumberService.reEncumber(orderId, requestContext);
    vertxTestContext.assertFailure(future)
      .onComplete(completionException ->{
      assertNotNull(completionException.cause());
      assertThat(completionException.cause(), instanceOf(HttpException.class));
      HttpException e = (HttpException) completionException.cause();
      assertEquals(ROLLOVER_NOT_COMPLETED.getCode(), e.getError().getCode());
      assertThat(e.getError().getParameters(), hasSize(1));
      Parameter parameter = e.getError().getParameters().get(0);
      assertThat(parameter.getValue(), containsString(ledgerId1));
      assertThat(parameter.getValue(), containsString(ledgerId2));
      vertxTestContext.completeNow();
    });

  }

  @Test
  void shouldThrowHttpExceptionWithRolloverNotCompletedCodeWhenAtLeastOneRolloverIsMissing(VertxTestContext vertxTestContext) {
    String orderId = UUID.randomUUID().toString();

    String fundId1 = UUID.randomUUID().toString();
    String fundId2 = UUID.randomUUID().toString();
    String fundId3 = UUID.randomUUID().toString();

    String ledgerId1 = UUID.randomUUID().toString();
    String ledgerId2 = UUID.randomUUID().toString();
    String ledgerId3 = UUID.randomUUID().toString();

    String rolloverId1 = UUID.randomUUID().toString();
    String rolloverId2 = UUID.randomUUID().toString();

    ReEncumbranceHolder holder1 = new ReEncumbranceHolder()
        .withLedgerId(ledgerId1)
        .withRollover(new LedgerFiscalYearRollover().withId(rolloverId1).withLedgerId(ledgerId1))
        .withFundDistribution(new FundDistribution().withFundId(fundId1));
    ReEncumbranceHolder holder2 = new ReEncumbranceHolder()
        .withLedgerId(ledgerId2)
        .withRollover(new LedgerFiscalYearRollover().withId(rolloverId2).withLedgerId(ledgerId2))
        .withFundDistribution(new FundDistribution().withFundId(fundId2));
    ReEncumbranceHolder holder3 = new ReEncumbranceHolder()
        .withLedgerId(ledgerId3)
        .withFundDistribution(new FundDistribution().withFundId(fundId3));

    List<ReEncumbranceHolder> holders = List.of(holder1, holder2, holder3);

    when(purchaseOrderStorageService.getCompositeOrderById(anyString(), any())).thenReturn(succeededFuture(new CompositePurchaseOrder()));
    when(spyReEncumbranceHoldersBuilder.buildReEncumbranceHoldersWithOrdersData(any())).thenReturn(holders);
    when(spyReEncumbranceHoldersBuilder.withFinances(any(), any())).thenReturn(succeededFuture(null));
    when(spyReEncumbranceHoldersBuilder.withRollovers(any(), any())).thenReturn(succeededFuture(holders));
    when(spyReEncumbranceHoldersBuilder.withEncumbranceRollover(any())).thenReturn(holders);
    when(ledgerRolloverProgressService.getRolloversProgress(eq(rolloverId1), any())).thenReturn(succeededFuture(Collections.singletonList(success)));
    when(ledgerRolloverProgressService.getRolloversProgress(eq(rolloverId2), any())).thenReturn(succeededFuture(Collections.singletonList(error)));

    var future = orderReEncumberService.reEncumber(orderId, requestContext);
    vertxTestContext.assertFailure(future)
      .onComplete(completionException ->{
        assertNotNull(completionException.cause());
        assertThat(completionException.cause(), instanceOf(HttpException.class));
        HttpException e = (HttpException) completionException.cause();
        assertEquals(ROLLOVER_NOT_COMPLETED.getCode(), e.getError().getCode());
        assertEquals(e.getError().getParameters(), Collections.singletonList(new Parameter().withKey("ledgerIds").withValue(ledgerId3)));
        vertxTestContext.completeNow();
      });

  }

  @Test
  void shouldThrowHttpExceptionWithRolloverNotCompletedCodeWhenAtLeastOneRolloverHasNoProgress(VertxTestContext vertxTestContext) {
    String orderId = UUID.randomUUID().toString();

    String fundId1 = UUID.randomUUID().toString();
    String fundId2 = UUID.randomUUID().toString();
    String fundId3 = UUID.randomUUID().toString();

    String ledgerId1 = UUID.randomUUID().toString();
    String ledgerId2 = UUID.randomUUID().toString();
    String ledgerId3 = UUID.randomUUID().toString();

    String rolloverId1 = UUID.randomUUID().toString();
    String rolloverId2 = UUID.randomUUID().toString();
    String rolloverId3 = UUID.randomUUID().toString();

    ReEncumbranceHolder holder1 = new ReEncumbranceHolder()
        .withLedgerId(ledgerId1)
        .withRollover(new LedgerFiscalYearRollover().withId(rolloverId1).withLedgerId(ledgerId1))
        .withFundDistribution(new FundDistribution().withFundId(fundId1));

    ReEncumbranceHolder holder2 = new ReEncumbranceHolder()
        .withLedgerId(ledgerId2)
        .withRollover(new LedgerFiscalYearRollover().withId(rolloverId2).withLedgerId(ledgerId2))
        .withFundDistribution(new FundDistribution().withFundId(fundId2));

    ReEncumbranceHolder holder3 = new ReEncumbranceHolder()
        .withLedgerId(ledgerId3)
        .withRollover(new LedgerFiscalYearRollover().withId(rolloverId3).withLedgerId(ledgerId3))
        .withFundDistribution(new FundDistribution().withFundId(fundId3));

    List<ReEncumbranceHolder> holders = List.of(holder1, holder2, holder3);

    when(purchaseOrderStorageService.getCompositeOrderById(anyString(), any())).thenReturn(succeededFuture(new CompositePurchaseOrder()));
    when(spyReEncumbranceHoldersBuilder.buildReEncumbranceHoldersWithOrdersData(any())).thenReturn(holders);
    when(spyReEncumbranceHoldersBuilder.withFinances(any(), any())).thenReturn(succeededFuture(null));
    when(spyReEncumbranceHoldersBuilder.withRollovers(any(), any())).thenReturn(succeededFuture(holders));
    when(spyReEncumbranceHoldersBuilder.withEncumbranceRollover(any())).thenReturn(holders);
    when(ledgerRolloverProgressService.getRolloversProgress(eq(rolloverId3), any()))
        .thenReturn(succeededFuture(Collections.singletonList(error)));
    when(ledgerRolloverProgressService.getRolloversProgress(AdditionalMatchers.not(eq(rolloverId3)), any())).thenReturn(succeededFuture(Collections.emptyList()));

    var future = orderReEncumberService.reEncumber(orderId, requestContext);
    vertxTestContext.assertFailure(future)
      .onComplete(completionException -> {
        assertNotNull(completionException.cause());
        assertThat(completionException.cause(), instanceOf(HttpException.class));
        HttpException e = (HttpException) completionException.cause();
        assertEquals(ROLLOVER_NOT_COMPLETED.getCode(), e.getError().getCode());
        assertEquals(e.getError().getParameters(), Collections.singletonList(new Parameter().withKey("ledgerIds").withValue(ledgerId1 + ", " + ledgerId2)));
        vertxTestContext.completeNow();
      });
  }

  @Test
  void reEncumberWithEmptyHoldersShouldFailed(VertxTestContext vertxTestContext) {

    String orderId = UUID.randomUUID().toString();

    when(purchaseOrderStorageService.getCompositeOrderById(eq(orderId), eq(requestContext)))
        .thenReturn(succeededFuture(new CompositePurchaseOrder().withId(orderId)));
    when(spyReEncumbranceHoldersBuilder.buildReEncumbranceHoldersWithOrdersData(any())).thenReturn(Collections.emptyList());
    when(spyReEncumbranceHoldersBuilder.withFinances(any(), any())).thenReturn(succeededFuture(null));
    when(spyReEncumbranceHoldersBuilder.withRollovers(any(), any())).thenReturn(succeededFuture(Collections.emptyList()));
    when(spyReEncumbranceHoldersBuilder.withEncumbranceRollover(any())).thenReturn(Collections.emptyList());
    when(spyReEncumbranceHoldersBuilder.withPreviousFyEncumbrances(any(), any())).thenReturn(succeededFuture(Collections.emptyList()));
    when(spyReEncumbranceHoldersBuilder.withToEncumbrances(any(), any())).thenReturn(succeededFuture(Collections.emptyList()));
    doNothing().when(budgetRestrictionService).checkEncumbranceRestrictions(anyList());
    when(ledgerRolloverErrorService.getLedgerFyRolloverErrors(anyString(), any()))
        .thenReturn(succeededFuture(new LedgerFiscalYearRolloverErrorCollection()));
    when(ledgerRolloverErrorService.deleteRolloverErrors(anyList(), any())).thenReturn(succeededFuture(null));
    when(purchaseOrderLineService.saveOrderLinesWithoutSearchLocationsUpdate(anyList(), any())).thenReturn(succeededFuture(null));
    ConversionQuery conversionQuery = ConversionQueryBuilder.of().setBaseCurrency("USD").setTermCurrency("USD").build();
    when(exchangeRateProviderResolver.resolve(conversionQuery, requestContext)).thenReturn(exchangeRateProvider);

    Future<Void> future = orderReEncumberService.reEncumber(orderId, requestContext);

    vertxTestContext.assertFailure(future)
      .onComplete(throwable -> {
        assertTrue(throwable.failed());
        HttpException httpException = (HttpException) throwable.cause();
        assertEquals(409, httpException.getCode());
        assertEquals(ENCUMBRANCES_FOR_RE_ENCUMBER_NOT_FOUND.toError(), httpException.getError());
        vertxTestContext.completeNow();
      });
  }

  @Test
  void shouldFailReEncumberHoldersWithEmptyEncumbranceRollover(VertxTestContext vertxTestContext) {

    CompositePoLine line= new CompositePoLine().withCost(new Cost().withCurrency("USD").withListUnitPrice(1d));
    String orderId = UUID.randomUUID().toString();

    ReEncumbranceHolder holder1 = new ReEncumbranceHolder()
        .withPoLine(line)
        .withLedgerId(UUID.randomUUID().toString());
    ReEncumbranceHolder holder2 = new ReEncumbranceHolder().withPoLine(line)
        .withLedgerId(UUID.randomUUID().toString());

    List<ReEncumbranceHolder> holders = List.of(holder1, holder2);

    when(purchaseOrderStorageService.getCompositeOrderById(eq(orderId), eq(requestContext)))
        .thenReturn(succeededFuture(new CompositePurchaseOrder().withId(orderId)));
    when(spyReEncumbranceHoldersBuilder.buildReEncumbranceHoldersWithOrdersData(any())).thenReturn(holders);
    when(spyReEncumbranceHoldersBuilder.withFinances(any(), any())).thenReturn(succeededFuture(null));
    when(spyReEncumbranceHoldersBuilder.withRollovers(any(), any())).thenReturn(succeededFuture(holders));
    when(spyReEncumbranceHoldersBuilder.withEncumbranceRollover(any())).thenReturn(holders);
    when(spyReEncumbranceHoldersBuilder.withPreviousFyEncumbrances(any(), any()))
        .thenAnswer(invocation -> succeededFuture(invocation.getArgument(0)));
    when(spyReEncumbranceHoldersBuilder.withToEncumbrances(any(), any()))
        .thenAnswer(invocation -> succeededFuture(invocation.getArgument(0)));
    doNothing().when(budgetRestrictionService).checkEncumbranceRestrictions(anyList());
    when(ledgerRolloverErrorService.getLedgerFyRolloverErrors(anyString(), any()))
        .thenReturn(succeededFuture(new LedgerFiscalYearRolloverErrorCollection()));
    when(ledgerRolloverErrorService.deleteRolloverErrors(anyList(), any())).thenReturn(succeededFuture(null));
    when(purchaseOrderLineService.saveOrderLinesWithoutSearchLocationsUpdate(anyList(), any())).thenReturn(succeededFuture(null));

    List<Transaction> toTransactionList = Collections.emptyList();
    when(transactionService.getTransactions(anyString(), eq(requestContext)))
        .thenReturn(succeededFuture(toTransactionList));
    when(transactionService.batchCreate(anyList(), eq(requestContext)))
      .thenReturn(succeededFuture());

    Future<Void> future = orderReEncumberService.reEncumber(orderId, requestContext);

    vertxTestContext.assertFailure(future)
      .onComplete(throwable -> {
        assertTrue(throwable.failed());
        HttpException httpException = (HttpException) throwable.cause();
        assertEquals(409, httpException.getCode());
        assertEquals(ENCUMBRANCES_FOR_RE_ENCUMBER_NOT_FOUND.toError(), httpException.getError());
        vertxTestContext.completeNow();
      });
  }

  @Test
  void shouldVerifyFYROAdjustmentAndFundDistributionValuesWhenMixedFunds(VertxTestContext vertxTestContext) {
    String ledgerId = UUID.randomUUID().toString();
    String fromFyId = UUID.randomUUID().toString();
    String toFyId = UUID.randomUUID().toString();
    String orderId = UUID.randomUUID().toString();
    String fund1Id = UUID.randomUUID().toString();
    String fund2Id = UUID.randomUUID().toString();
    String rolloverId = UUID.randomUUID().toString();
    Ledger notRestrictedLedger = new Ledger().withId(ledgerId).withRestrictEncumbrance(false);
    double exchangeRate = 1.1d;

    Budget notRestrictedBudget1 = new Budget().withId(UUID.randomUUID().toString()).withFundId(fund1Id);

    EncumbranceRollover encumbranceRollover = new EncumbranceRollover()
        .withBasedOn(EncumbranceRollover.BasedOn.EXPENDED)
        .withOrderType(EncumbranceRollover.OrderType.ONE_TIME)
        .withIncreaseBy(10d);

    LedgerFiscalYearRollover rollover = new LedgerFiscalYearRollover()
        .withId(rolloverId)
        .withLedgerId(ledgerId)
        .withFromFiscalYearId(fromFyId)
        .withToFiscalYearId(toFyId)
        .withEncumbrancesRollover(Collections.singletonList(encumbranceRollover));

    CompositePurchaseOrder compPO = new CompositePurchaseOrder().withId(orderId).withOrderType(CompositePurchaseOrder.OrderType.ONE_TIME);

    Cost cost1 = new Cost().withListUnitPrice(120d).withQuantityPhysical(1)
                 .withExchangeRate(exchangeRate).withCurrency("EUR").withPoLineEstimatedPrice(120d);

    Transaction fromEncumbrance1 = new Transaction().withEncumbrance(new Encumbrance()
        .withSourcePurchaseOrderId(orderId)
        .withInitialAmountEncumbered(60d)
        .withAmountExpended(48d)
        .withAmountCredited(8d))
        .withAmount(20d)
        .withFiscalYearId(fromFyId)
        .withFromFundId(fund1Id)
        .withId(UUID.randomUUID().toString())
        .withCurrency("USD");

    Transaction fromEncumbrance2 = new Transaction().withEncumbrance(new Encumbrance()
        .withSourcePurchaseOrderId(orderId)
        .withInitialAmountEncumbered(60d)
        .withAmountExpended(38d)
        .withAmountCredited(8d))
        .withAmount(30d)
        .withFiscalYearId(fromFyId)
        .withFromFundId(fund2Id)
        .withId(UUID.randomUUID().toString())
        .withCurrency("USD");

    Transaction toEncumbrance1 = new Transaction().withEncumbrance(new Encumbrance()
        .withStatus(Encumbrance.Status.UNRELEASED)
        .withAmountAwaitingPayment(0d)
        .withSourcePurchaseOrderId(orderId)
        .withInitialAmountEncumbered(60d)
        .withAmountExpended(0d)
        .withAmountCredited(0d))
        .withAmount(40d)
        .withFiscalYearId(fromFyId)
        .withFromFundId(fund1Id)
        .withId(UUID.randomUUID().toString())
        .withCurrency("USD");

    Transaction toEncumbrance2 = new Transaction().withEncumbrance(new Encumbrance()
        .withStatus(Encumbrance.Status.UNRELEASED)
        .withAmountAwaitingPayment(0d)
        .withSourcePurchaseOrderId(orderId)
        .withInitialAmountEncumbered(60d)
        .withAmountExpended(0d)
        .withAmountCredited(0d))
        .withAmount(30d)
        .withFiscalYearId(fromFyId)
        .withFromFundId(fund2Id)
        .withId(UUID.randomUUID().toString())
        .withCurrency("USD");


    List<Transaction> toTransactionList = Collections.emptyList();

    FundDistribution fundDistribution1 = new FundDistribution()
        .withDistributionType(FundDistribution.DistributionType.PERCENTAGE)
        .withValue(50d)
        .withEncumbrance(fromEncumbrance1.getId())
        .withFundId(fund1Id);

    FundDistribution fundDistribution2 = new FundDistribution()
        .withDistributionType(FundDistribution.DistributionType.AMOUNT)
        .withValue(60d)
        .withEncumbrance(fromEncumbrance2.getId())
        .withFundId(fund2Id);

     CompositePoLine line1 = new CompositePoLine().withId(UUID.randomUUID().toString())
        .withCost(cost1)
        .withFundDistribution(List.of(fundDistribution1, fundDistribution2));

    ConversionQuery conversionPoLineToFyQuery = ConversionQueryBuilder.of()
        .setBaseCurrency("EUR").setTermCurrency("USD").set(ExchangeRateProviderResolver.RATE_KEY, exchangeRate).build();
    ConversionQuery conversionFyToPoLineQuery = ConversionQueryBuilder.of()
        .setBaseCurrency("USD").setTermCurrency("EUR").set(ExchangeRateProviderResolver.RATE_KEY, exchangeRate).build();

    ReEncumbranceHolder holder1 = new ReEncumbranceHolder()
        .withPurchaseOrder(compPO)
        .withRollover(rollover)
        .withPoLine(line1)
        .withFundDistribution(fundDistribution1)
        .withLedgerId(ledgerId)
        .withRestrictEncumbrances(notRestrictedLedger.getRestrictEncumbrance())
        .withBudget(notRestrictedBudget1)
        .withEncumbranceRollover(encumbranceRollover)
        .withPreviousFyEncumbrance(fromEncumbrance1)
        .withNewEncumbrance(toEncumbrance1)
        .withPoLineToFyConversion(exchangeRateProvider.getCurrencyConversion(conversionPoLineToFyQuery))
        .withFyToPoLineConversion(exchangeRateProvider.getCurrencyConversion(conversionFyToPoLineQuery));


    ReEncumbranceHolder holder2 = new ReEncumbranceHolder()
        .withPurchaseOrder(compPO)
        .withRollover(rollover)
        .withPoLine(line1)
        .withFundDistribution(fundDistribution2)
        .withLedgerId(ledgerId)
        .withRestrictEncumbrances(notRestrictedLedger.getRestrictEncumbrance())
        .withBudget(notRestrictedBudget1)
        .withEncumbranceRollover(encumbranceRollover)
        .withNewEncumbrance(toEncumbrance2)
        .withPreviousFyEncumbrance(fromEncumbrance2)
        .withPoLineToFyConversion(exchangeRateProvider.getCurrencyConversion(conversionPoLineToFyQuery))
        .withFyToPoLineConversion(exchangeRateProvider.getCurrencyConversion(conversionFyToPoLineQuery));

    List<ReEncumbranceHolder> holders = Arrays.asList(holder1, holder2);
    when(purchaseOrderStorageService.getCompositeOrderById(anyString(), eq(requestContext)))
        .thenAnswer(invocation -> succeededFuture(new CompositePurchaseOrder().withId(invocation.getArgument(0))));
    doReturn(holders).when(spyReEncumbranceHoldersBuilder).buildReEncumbranceHoldersWithOrdersData(any());
    doReturn(succeededFuture(null)).when(spyReEncumbranceHoldersBuilder).withFinances(any(), any());
    doReturn(succeededFuture(holders)).when(spyReEncumbranceHoldersBuilder).withRollovers(any(), any());
    doReturn(holders).when(spyReEncumbranceHoldersBuilder).withEncumbranceRollover(any());
    doReturn(succeededFuture(holders)).when(spyReEncumbranceHoldersBuilder).withPreviousFyEncumbrances(any(), any());
    doReturn(succeededFuture(holders)).when(spyReEncumbranceHoldersBuilder).withToEncumbrances(any(), any());
    doNothing().when(budgetRestrictionService).checkEncumbranceRestrictions(anyList());
    when(ledgerRolloverErrorService.getLedgerFyRolloverErrors(anyString(), any()))
        .thenReturn(succeededFuture(new LedgerFiscalYearRolloverErrorCollection()));
    when(ledgerRolloverErrorService.deleteRolloverErrors(anyList(), any())).thenReturn(succeededFuture(null));
    when(purchaseOrderLineService.saveOrderLinesWithoutSearchLocationsUpdate(anyList(), any())).thenReturn(succeededFuture(null));

    when(ledgerRolloverProgressService.getRolloversProgress(eq(rolloverId), any()))
        .thenReturn(succeededFuture(Collections.singletonList(success)));

    when(exchangeRateProviderResolver.resolve(conversionPoLineToFyQuery, requestContext)).thenReturn(exchangeRateProvider);
    when(exchangeRateProviderResolver.resolve(conversionFyToPoLineQuery, requestContext)).thenReturn(exchangeRateProvider);
    when(transactionService.getTransactions(anyString(), eq(requestContext)))
        .thenReturn(succeededFuture(toTransactionList));
    when(transactionService.batchCreate(anyList(), eq(requestContext)))
      .thenReturn(succeededFuture());

    //When
    Future<Void> future = orderReEncumberService.reEncumber(UUID.randomUUID().toString(), requestContext);
    vertxTestContext.assertComplete(future)
      .onSuccess(result -> {
        assertEquals(-35.3d, line1.getCost().getFyroAdjustmentAmount());
        assertEquals(50d, line1.getFundDistribution().get(0).getValue());
        assertEquals(42.35d, line1.getFundDistribution().get(1).getValue());
        vertxTestContext.completeNow();
      })
      .onFailure(vertxTestContext::failNow);
  }
}
