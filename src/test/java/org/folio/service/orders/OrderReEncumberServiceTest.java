package org.folio.service.orders;

import static java.util.concurrent.CompletableFuture.completedFuture;
import static org.folio.TestUtils.getMockAsJson;
import static org.folio.helper.PurchaseOrderHelperTest.ORDER_PATH;
import static org.folio.orders.utils.ErrorCodes.FUNDS_NOT_FOUND;
import static org.folio.orders.utils.ErrorCodes.ROLLOVER_NOT_COMPLETED;
import static org.folio.rest.acq.model.finance.LedgerFiscalYearRolloverProgress.RolloverStatus.ERROR;
import static org.folio.rest.acq.model.finance.LedgerFiscalYearRolloverProgress.RolloverStatus.IN_PROGRESS;
import static org.folio.rest.acq.model.finance.LedgerFiscalYearRolloverProgress.RolloverStatus.NOT_STARTED;
import static org.folio.rest.acq.model.finance.LedgerFiscalYearRolloverProgress.RolloverStatus.SUCCESS;
import static org.hamcrest.MatcherAssert.assertThat;
import static org.hamcrest.Matchers.containsString;
import static org.hamcrest.Matchers.everyItem;
import static org.hamcrest.Matchers.hasProperty;
import static org.hamcrest.Matchers.hasSize;
import static org.hamcrest.Matchers.instanceOf;
import static org.hamcrest.Matchers.notNullValue;
import static org.junit.jupiter.api.Assertions.assertEquals;
import static org.junit.jupiter.api.Assertions.assertFalse;
import static org.junit.jupiter.api.Assertions.assertNotNull;
import static org.junit.jupiter.api.Assertions.assertThrows;
import static org.junit.jupiter.api.Assertions.assertTrue;
import static org.mockito.ArgumentMatchers.any;
import static org.mockito.ArgumentMatchers.anyList;
import static org.mockito.ArgumentMatchers.anyMap;
import static org.mockito.ArgumentMatchers.anyString;
import static org.mockito.ArgumentMatchers.eq;
import static org.mockito.Mockito.clearInvocations;
import static org.mockito.Mockito.doNothing;
import static org.mockito.Mockito.doReturn;
import static org.mockito.Mockito.verify;
import static org.mockito.Mockito.when;

import java.util.Arrays;
import java.util.Collections;
import java.util.List;
import java.util.Map;
import java.util.UUID;
import java.util.concurrent.CompletableFuture;
import java.util.concurrent.CompletionException;

import org.folio.models.ReEncumbranceHolder;
import org.folio.orders.rest.exceptions.HttpException;
import org.folio.rest.acq.model.finance.Budget;
import org.folio.rest.acq.model.finance.FiscalYear;
import org.folio.rest.acq.model.finance.Fund;
import org.folio.rest.acq.model.finance.Ledger;
import org.folio.rest.acq.model.finance.LedgerFiscalYearRolloverError;
import org.folio.rest.acq.model.finance.LedgerFiscalYearRolloverErrorCollection;
import org.folio.rest.acq.model.finance.LedgerFiscalYearRolloverProgress;
import org.folio.rest.acq.model.finance.Transaction;
import org.folio.rest.core.models.RequestContext;
import org.folio.rest.jaxrs.model.CompositePoLine;
import org.folio.rest.jaxrs.model.CompositePurchaseOrder;
import org.folio.rest.jaxrs.model.Cost;
import org.folio.rest.jaxrs.model.EncumbranceRollover;
import org.folio.rest.jaxrs.model.FundDistribution;
import org.folio.rest.jaxrs.model.LedgerFiscalYearRollover;
import org.folio.rest.jaxrs.model.LedgerFiscalYearRolloverCollection;
import org.folio.rest.jaxrs.model.Parameter;
import org.folio.service.finance.BudgetRestrictionService;
import org.folio.service.finance.RolloverErrorService;
import org.folio.service.finance.RolloverRetrieveService;
import org.junit.jupiter.api.BeforeEach;
import org.junit.jupiter.api.Test;
import org.mockito.AdditionalMatchers;
import org.mockito.ArgumentCaptor;
import org.mockito.ArgumentMatcher;
import org.mockito.InjectMocks;
import org.mockito.Mock;
import org.mockito.MockitoAnnotations;

public class OrderReEncumberServiceTest {

    @InjectMocks
    private OrderReEncumberService orderReEncumberService;

    @Mock
    private CompositePurchaseOrderService compositePurchaseOrderService;
    @Mock
    private ReEncumbranceHoldersBuilder reEncumbranceHoldersBuilder;
    @Mock
    private RolloverErrorService rolloverErrorService;
    @Mock
    private RolloverRetrieveService rolloverRetrieveService;
    @Mock
    private PurchaseOrderLineService purchaseOrderLineService;
    @Mock
    private BudgetRestrictionService budgetRestrictionService;

    @Mock
    private RequestContext requestContext;

    private LedgerFiscalYearRolloverProgress notStarted;
    private LedgerFiscalYearRolloverProgress inProgress;
    private LedgerFiscalYearRolloverProgress success;
    private LedgerFiscalYearRolloverProgress error;

    @BeforeEach
    public void initMocks(){
        MockitoAnnotations.openMocks(this);
        notStarted = new LedgerFiscalYearRolloverProgress().withOverallRolloverStatus(NOT_STARTED);
        inProgress = new LedgerFiscalYearRolloverProgress().withOverallRolloverStatus(IN_PROGRESS);
        success = new LedgerFiscalYearRolloverProgress().withOverallRolloverStatus(SUCCESS);
        error = new LedgerFiscalYearRolloverProgress().withOverallRolloverStatus(ERROR);
    }


    @Test
    void testPopulateNeedReEncumberField() {
        // given
        CompositePurchaseOrder order = getMockAsJson(ORDER_PATH).mapTo(CompositePurchaseOrder.class);
        String ledgerId = UUID.randomUUID().toString();

        Fund sampleFund = new Fund()
                .withLedgerId(UUID.randomUUID().toString())
                .withFundStatus(Fund.FundStatus.ACTIVE)
                .withCode(UUID.randomUUID().toString());

        LedgerFiscalYearRollover rollover = new LedgerFiscalYearRollover().withId(UUID.randomUUID().toString())
                .withLedgerId(ledgerId);

        LedgerFiscalYearRolloverErrorCollection ledgerFiscalYearRolloverErrors = new LedgerFiscalYearRolloverErrorCollection()
                .withLedgerFiscalYearRolloverErrors(Collections.singletonList(new LedgerFiscalYearRolloverError()));

        CompositePoLine line = order.getCompositePoLines().get(0);
        FundDistribution fundDistribution1 = line.getFundDistribution().get(0);
        FiscalYear fiscalYear = new FiscalYear();

        ReEncumbranceHolder holder1 = new ReEncumbranceHolder().withPurchaseOrder(order)
                .withPoLine(line)
                .withFundDistribution(fundDistribution1)
                .withRollover(rollover)
                .withFund(new Fund()
                        .withId(fundDistribution1.getFundId())
                        .withLedgerId(ledgerId))
                .withCurrentFiscalYear(fiscalYear);

        FundDistribution fundDistribution2 = line.getFundDistribution().get(1);
        ReEncumbranceHolder holder2 = new ReEncumbranceHolder().withPurchaseOrder(order)
                .withPoLine(order.getCompositePoLines().get(0))
                .withFundDistribution(fundDistribution2)
                .withRollover(rollover)
                .withFund(new Fund()
                        .withId(fundDistribution2.getFundId())
                        .withLedgerId(ledgerId))
                .withCurrentFiscalYear(fiscalYear);

        FundDistribution fundDistribution3 = line.getFundDistribution().get(1);
        ReEncumbranceHolder holder3 = new ReEncumbranceHolder().withPurchaseOrder(order)
                .withPoLine(order.getCompositePoLines().get(0))
                .withFundDistribution(fundDistribution3)
                .withRollover(rollover)
                .withFund(new Fund()
                        .withId(fundDistribution3.getFundId())
                        .withLedgerId(ledgerId))
                .withCurrentFiscalYear(fiscalYear);

        List<LedgerFiscalYearRolloverProgress> progresses = Collections.singletonList(success);

        List<ReEncumbranceHolder> holders = List.of(holder1, holder2, holder3);

        // LedgerFiscalYearRolloverErrorCollection is not empty. Expected "needReEncumber" = true
        doReturn(holders).when(reEncumbranceHoldersBuilder).buildReEncumbranceHoldersWithOrdersData(any());
        doReturn(completedFuture(holders)).when(reEncumbranceHoldersBuilder).withFunds(any(), any());
        doReturn(completedFuture(holders)).when(reEncumbranceHoldersBuilder).withCurrentFiscalYear(any(), any());
        doReturn(completedFuture(holders)).when(reEncumbranceHoldersBuilder).withRollovers(any(), any());
        doReturn(completedFuture(progresses)).when(rolloverRetrieveService).getRolloversProgress(anyString(), any());
        doReturn(completedFuture(ledgerFiscalYearRolloverErrors)).when(rolloverErrorService).getLedgerFyRolloverErrors(any(), any());

        CompositePurchaseOrder compOrder = orderReEncumberService.populateNeedReEncumberFlag(order, requestContext).join();
        assertTrue(compOrder.getNeedReEncumber());

        // LedgerFiscalYearRolloverErrorCollection is empty. Expected "needReEncumber" = false
        doReturn(completedFuture(new LedgerFiscalYearRolloverErrorCollection()))
                .when(rolloverErrorService).getLedgerFyRolloverErrors(any(), any());
        compOrder = orderReEncumberService.populateNeedReEncumberFlag(order, requestContext).join();
        assertFalse(compOrder.getNeedReEncumber());

        // LedgerFyRollover not exists. Expected "needReEncumber" = false
        doReturn(completedFuture(new LedgerFiscalYearRolloverCollection())).when(rolloverRetrieveService).getLedgerFyRollovers(anyString(), anyString(), any());
        compOrder = orderReEncumberService.populateNeedReEncumberFlag(order, requestContext).join();
        assertFalse(compOrder.getNeedReEncumber());

    }

    @Test
    void shouldThrowHttpExceptionWithRolloverNotCompletedCodeWhenAtLeastOneRolloverHasProgressNotStartedOrInProgress() {
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
                .withFund(new Fund().withId(fundId1).withLedgerId(ledgerId1))
                .withRollover(new LedgerFiscalYearRollover().withId(rolloverId1).withLedgerId(ledgerId1))
                .withFundDistribution(new FundDistribution().withFundId(fundId1));
        ReEncumbranceHolder holder2 = new ReEncumbranceHolder()
                .withFund(new Fund().withId(fundId2).withLedgerId(ledgerId2))
                .withRollover(new LedgerFiscalYearRollover().withId(rolloverId2).withLedgerId(ledgerId2))
                .withFundDistribution(new FundDistribution().withFundId(fundId2));
        ReEncumbranceHolder holder3 = new ReEncumbranceHolder()
                .withFund(new Fund().withId(fundId3).withLedgerId(ledgerId3))
                .withRollover(new LedgerFiscalYearRollover().withId(rolloverId3).withLedgerId(ledgerId3))
                .withFundDistribution(new FundDistribution().withFundId(fundId3));

        List<ReEncumbranceHolder> holders = List.of(holder1, holder2, holder3);

        when(compositePurchaseOrderService.getCompositeOrderById(anyString(), any())).thenReturn(CompletableFuture.completedFuture(new CompositePurchaseOrder()));
        when(reEncumbranceHoldersBuilder.buildReEncumbranceHoldersWithOrdersData(any())).thenReturn(holders);
        when(reEncumbranceHoldersBuilder.withFunds(any(), any())).thenReturn(CompletableFuture.completedFuture(holders));
        when(reEncumbranceHoldersBuilder.withLedgers(any(), any())).thenReturn(CompletableFuture.completedFuture(holders));
        when(reEncumbranceHoldersBuilder.withCurrentFiscalYear(any(), any())).thenReturn(CompletableFuture.completedFuture(holders));
        when(reEncumbranceHoldersBuilder.withRollovers(any(), any())).thenReturn(CompletableFuture.completedFuture(holders));
        when(reEncumbranceHoldersBuilder.withEncumbranceRollover(any())).thenReturn(holders);
        when(rolloverRetrieveService.getRolloversProgress(eq(rolloverId1), any()))
                .thenReturn(CompletableFuture.completedFuture(Collections.singletonList(notStarted)));
        when(rolloverRetrieveService.getRolloversProgress(eq(rolloverId2), any()))
                .thenReturn(CompletableFuture.completedFuture(Collections.singletonList(inProgress)));
        when(rolloverRetrieveService.getRolloversProgress(eq(rolloverId3), any()))
                .thenReturn(CompletableFuture.completedFuture(Collections.singletonList(success)));

        CompletionException completionException = assertThrows(CompletionException.class, () -> orderReEncumberService.reEncumber(orderId, requestContext).join());

        assertNotNull(completionException.getCause());
        assertThat(completionException.getCause(), instanceOf(HttpException.class));
        HttpException e = (HttpException) completionException.getCause();
        assertEquals(ROLLOVER_NOT_COMPLETED.getCode(), e.getError().getCode());
        assertThat(e.getError().getParameters(), hasSize(1));
        Parameter parameter = e.getError().getParameters().get(0);
        assertThat(parameter.getValue(), containsString(ledgerId1));
        assertThat(parameter.getValue(), containsString(ledgerId2));

    }

    @Test
    void shouldThrowHttpExceptionWithRolloverNotCompletedCodeWhenAtLeastOneRolloverIsMissing() {
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
                .withFund(new Fund().withId(fundId1).withLedgerId(ledgerId1))
                .withRollover(new LedgerFiscalYearRollover().withId(rolloverId1).withLedgerId(ledgerId1))
                .withFundDistribution(new FundDistribution().withFundId(fundId1));
        ReEncumbranceHolder holder2 = new ReEncumbranceHolder()
                .withFund(new Fund().withId(fundId2).withLedgerId(ledgerId2))
                .withRollover(new LedgerFiscalYearRollover().withId(rolloverId2).withLedgerId(ledgerId2))
                .withFundDistribution(new FundDistribution().withFundId(fundId2));
        ReEncumbranceHolder holder3 = new ReEncumbranceHolder()
                .withFund(new Fund().withId(fundId3).withLedgerId(ledgerId3))
                .withFundDistribution(new FundDistribution().withFundId(fundId3));

        List<ReEncumbranceHolder> holders = List.of(holder1, holder2, holder3);

        when(compositePurchaseOrderService.getCompositeOrderById(anyString(), any())).thenReturn(CompletableFuture.completedFuture(new CompositePurchaseOrder()));
        when(reEncumbranceHoldersBuilder.buildReEncumbranceHoldersWithOrdersData(any())).thenReturn(holders);
        when(reEncumbranceHoldersBuilder.withFunds(any(), any())).thenReturn(CompletableFuture.completedFuture(holders));
        when(reEncumbranceHoldersBuilder.withLedgers(any(), any())).thenReturn(CompletableFuture.completedFuture(holders));
        when(reEncumbranceHoldersBuilder.withCurrentFiscalYear(any(), any())).thenReturn(CompletableFuture.completedFuture(holders));
        when(reEncumbranceHoldersBuilder.withRollovers(any(), any())).thenReturn(CompletableFuture.completedFuture(holders));
        when(reEncumbranceHoldersBuilder.withEncumbranceRollover(any())).thenReturn(holders);
        when(rolloverRetrieveService.getRolloversProgress(eq(rolloverId1), any())).thenReturn(CompletableFuture.completedFuture(Collections.singletonList(success)));
        when(rolloverRetrieveService.getRolloversProgress(eq(rolloverId2), any())).thenReturn(CompletableFuture.completedFuture(Collections.singletonList(error)));

        CompletionException completionException = assertThrows(CompletionException.class, () -> orderReEncumberService.reEncumber(orderId, requestContext).join());

        assertNotNull(completionException.getCause());
        assertThat(completionException.getCause(), instanceOf(HttpException.class));
        HttpException e = (HttpException) completionException.getCause();
        assertEquals(ROLLOVER_NOT_COMPLETED.getCode(), e.getError().getCode());
        assertEquals(e.getError().getParameters(), Collections.singletonList(new Parameter().withKey("ledgerIds").withValue(ledgerId3)));

    }

    @Test
    void shouldThrowHttpExceptionWithRolloverNotCompletedCodeWhenAtLeastOneRolloverHasNoProgress() {
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
                .withFund(new Fund().withId(fundId1).withLedgerId(ledgerId1))
                .withRollover(new LedgerFiscalYearRollover().withId(rolloverId1).withLedgerId(ledgerId1))
                .withFundDistribution(new FundDistribution().withFundId(fundId1));

        ReEncumbranceHolder holder2 = new ReEncumbranceHolder()
                .withFund(new Fund().withId(fundId2).withLedgerId(ledgerId2))
                .withRollover(new LedgerFiscalYearRollover().withId(rolloverId2).withLedgerId(ledgerId2))
                .withFundDistribution(new FundDistribution().withFundId(fundId2));

        ReEncumbranceHolder holder3 = new ReEncumbranceHolder()
                .withFund(new Fund().withId(fundId3).withLedgerId(ledgerId3))
                .withRollover(new LedgerFiscalYearRollover().withId(rolloverId3).withLedgerId(ledgerId3))
                .withFundDistribution(new FundDistribution().withFundId(fundId3));

        List<ReEncumbranceHolder> holders = List.of(holder1, holder2, holder3);

        when(compositePurchaseOrderService.getCompositeOrderById(anyString(), any())).thenReturn(CompletableFuture.completedFuture(new CompositePurchaseOrder()));
        when(reEncumbranceHoldersBuilder.buildReEncumbranceHoldersWithOrdersData(any())).thenReturn(holders);
        when(reEncumbranceHoldersBuilder.withFunds(any(), any())).thenReturn(CompletableFuture.completedFuture(holders));
        when(reEncumbranceHoldersBuilder.withLedgers(any(), any())).thenReturn(CompletableFuture.completedFuture(holders));
        when(reEncumbranceHoldersBuilder.withCurrentFiscalYear(any(), any())).thenReturn(CompletableFuture.completedFuture(holders));
        when(reEncumbranceHoldersBuilder.withRollovers(any(), any())).thenReturn(CompletableFuture.completedFuture(holders));
        when(reEncumbranceHoldersBuilder.withEncumbranceRollover(any())).thenReturn(holders);
        when(rolloverRetrieveService.getRolloversProgress(eq(rolloverId3), any()))
                .thenReturn(CompletableFuture.completedFuture(Collections.singletonList(error)));
        when(rolloverRetrieveService.getRolloversProgress(AdditionalMatchers.not(eq(rolloverId3)), any())).thenReturn(CompletableFuture.completedFuture(Collections.emptyList()));

        CompletionException completionException = assertThrows(CompletionException.class, () -> orderReEncumberService.reEncumber(orderId, requestContext).join());

        assertNotNull(completionException.getCause());
        assertThat(completionException.getCause(), instanceOf(HttpException.class));
        HttpException e = (HttpException) completionException.getCause();
        assertEquals(ROLLOVER_NOT_COMPLETED.getCode(), e.getError().getCode());
        assertEquals(e.getError().getParameters(), Collections.singletonList(new Parameter().withKey("ledgerIds").withValue(ledgerId1 + ", " + ledgerId2)));

    }

    @Test
    void shouldThrowHttpExceptionWithFundsNotFoundCodeWhenAtLeastOneFundIsMissing() {
        String orderId = UUID.randomUUID().toString();

        String fundId1 = UUID.randomUUID().toString();
        String fundId2 = UUID.randomUUID().toString();

        ReEncumbranceHolder holder1 = new ReEncumbranceHolder()
                .withFundDistribution(new FundDistribution().withFundId(fundId1))
                .withFund(new Fund().withId(fundId1));
        ReEncumbranceHolder holder2 = new ReEncumbranceHolder()
                .withFundDistribution(new FundDistribution().withFundId(fundId2));

        List<ReEncumbranceHolder> holders = List.of(holder1, holder2);

        when(compositePurchaseOrderService.getCompositeOrderById(anyString(), any())).thenReturn(CompletableFuture.completedFuture(new CompositePurchaseOrder()));
        when(reEncumbranceHoldersBuilder.buildReEncumbranceHoldersWithOrdersData(any())).thenReturn(holders);
        when(reEncumbranceHoldersBuilder.withFunds(any(), any())).thenReturn(CompletableFuture.completedFuture(holders));

        CompletionException completionException = assertThrows(CompletionException.class, () -> orderReEncumberService.reEncumber(orderId, requestContext).join());

        assertNotNull(completionException.getCause());
        assertThat(completionException.getCause(), instanceOf(HttpException.class));
        HttpException e = (HttpException) completionException.getCause();
        assertEquals(FUNDS_NOT_FOUND.getCode(), e.getError().getCode());
        assertEquals(e.getError().getParameters(), Collections.singletonList(new Parameter().withKey("fund").withValue(fundId2)));

    }

    @Test
    void reEncumberWithEmptyHoldersShouldCompleteSuccessfully() {

        String orderId = UUID.randomUUID().toString();

        when(compositePurchaseOrderService.getCompositeOrderById(eq(orderId), eq(requestContext)))
                .thenReturn(CompletableFuture.completedFuture(new CompositePurchaseOrder().withId(orderId)));
        when(reEncumbranceHoldersBuilder.buildReEncumbranceHoldersWithOrdersData(any())).thenReturn(Collections.emptyList());
        when(reEncumbranceHoldersBuilder.withFunds(any(), any())).thenReturn(CompletableFuture.completedFuture(Collections.emptyList()));
        when(reEncumbranceHoldersBuilder.withLedgers(any(), any())).thenReturn(CompletableFuture.completedFuture(Collections.emptyList()));
        when(reEncumbranceHoldersBuilder.withCurrentFiscalYear(any(), any())).thenReturn(CompletableFuture.completedFuture(Collections.emptyList()));
        when(reEncumbranceHoldersBuilder.withRollovers(any(), any())).thenReturn(CompletableFuture.completedFuture(Collections.emptyList()));
        when(reEncumbranceHoldersBuilder.withEncumbranceRollover(any())).thenReturn(Collections.emptyList());
        when(reEncumbranceHoldersBuilder.withBudgets(any(), any())).thenReturn(CompletableFuture.completedFuture(Collections.emptyList()));
        when(reEncumbranceHoldersBuilder.withConversion(any(), any())).thenReturn(CompletableFuture.completedFuture(Collections.emptyList()));
        when(reEncumbranceHoldersBuilder.withEncumbrances(any(), any())).thenReturn(CompletableFuture.completedFuture(Collections.emptyList()));
        doNothing().when(budgetRestrictionService).checkEnoughMoneyInBudgets(anyMap());
        when(rolloverErrorService.getLedgerFyRolloverErrors(anyString(), any()))
                .thenReturn(CompletableFuture.completedFuture(new LedgerFiscalYearRolloverErrorCollection()));
        when(rolloverErrorService.deleteRolloverErrors(anyList(), any())).thenReturn(CompletableFuture.completedFuture(null));
        when(purchaseOrderLineService.updateOrderLines(anyList(), any())).thenReturn(CompletableFuture.completedFuture(null));

        CompletableFuture<Void> future = orderReEncumberService.reEncumber(orderId, requestContext);

        future.join();
        assertFalse(future.isCompletedExceptionally());
    }

    @Test
    void shouldFilterReEncumberHoldersWithEmptyEncumbranceRollover() {

        CompositePoLine line= new CompositePoLine().withCost(new Cost().withCurrency("USD").withListUnitPrice(1d));
        String orderId = UUID.randomUUID().toString();

        ReEncumbranceHolder holder1 = new ReEncumbranceHolder()
                .withPoLine(line)
                .withFund(new Fund());
        ReEncumbranceHolder holder2 = new ReEncumbranceHolder().withPoLine(line)
                .withFund(new Fund());

        List<ReEncumbranceHolder> holders = List.of(holder1, holder2);

        when(compositePurchaseOrderService.getCompositeOrderById(eq(orderId), eq(requestContext)))
                .thenReturn(CompletableFuture.completedFuture(new CompositePurchaseOrder().withId(orderId)));
        when(reEncumbranceHoldersBuilder.buildReEncumbranceHoldersWithOrdersData(any())).thenReturn(holders);
        when(reEncumbranceHoldersBuilder.withFunds(any(), any())).thenReturn(CompletableFuture.completedFuture(holders));
        when(reEncumbranceHoldersBuilder.withLedgers(any(), any())).thenReturn(CompletableFuture.completedFuture(holders));
        when(reEncumbranceHoldersBuilder.withCurrentFiscalYear(any(), any())).thenReturn(CompletableFuture.completedFuture(holders));
        when(reEncumbranceHoldersBuilder.withRollovers(any(), any())).thenReturn(CompletableFuture.completedFuture(holders));
        when(reEncumbranceHoldersBuilder.withEncumbranceRollover(any())).thenReturn(holders);
        when(reEncumbranceHoldersBuilder.withBudgets(any(), any())).thenReturn(CompletableFuture.completedFuture(holders));
        when(reEncumbranceHoldersBuilder.withConversion(any(), any())).thenReturn(CompletableFuture.completedFuture(holders));
        when(reEncumbranceHoldersBuilder.withEncumbrances(any(), any()))
                .thenAnswer(invocation -> CompletableFuture.completedFuture(invocation.getArgument(0)));
        doNothing().when(budgetRestrictionService).checkEnoughMoneyInBudgets(anyMap());
        when(rolloverErrorService.getLedgerFyRolloverErrors(anyString(), any()))
                .thenReturn(CompletableFuture.completedFuture(new LedgerFiscalYearRolloverErrorCollection()));
        when(rolloverErrorService.deleteRolloverErrors(anyList(), any())).thenReturn(CompletableFuture.completedFuture(null));
        when(purchaseOrderLineService.updateOrderLines(anyList(), any())).thenReturn(CompletableFuture.completedFuture(null));

        CompletableFuture<Void> future = orderReEncumberService.reEncumber(orderId, requestContext);

        future.join();
        assertFalse(future.isCompletedExceptionally());

        ArgumentCaptor<List<ReEncumbranceHolder>> argumentCaptor = ArgumentCaptor.forClass(List.class);
        verify(reEncumbranceHoldersBuilder).withEncumbrances(argumentCaptor.capture(), any());
        List<ReEncumbranceHolder> argumentHolders = argumentCaptor.getValue();
        assertThat(argumentHolders, hasSize(0));
    }

    @Test
    void shouldVerifyOnlyBudgetsRelatedToLedgersWithRestrictEncumbranceTrue() {

        CompositePoLine line = new CompositePoLine().withCost(new Cost().withListUnitPrice(10d));

        Ledger restrictedLedger = new Ledger().withRestrictEncumbrance(true);
        Ledger notRestrictedLedger = new Ledger().withRestrictEncumbrance(false);

        Fund restrictedFund = new Fund();
        Fund notRestrictedFund = new Fund();

        Budget restrictedBudget = new Budget().withId(UUID.randomUUID().toString());
        Budget notRestrictedBudget = new Budget().withId(UUID.randomUUID().toString());

        Transaction transaction1 = new Transaction();
        Transaction transaction2 = new Transaction();
        Transaction transaction3 = new Transaction();

        ReEncumbranceHolder holder1 = new ReEncumbranceHolder()
                .withPoLine(line)
                .withFundDistribution(new FundDistribution())
                .withFund(restrictedFund)
                .withLedger(restrictedLedger)
                .withBudget(restrictedBudget)
                .withToFYEncumbrance(transaction1);
        ReEncumbranceHolder holder2 = new ReEncumbranceHolder()
                .withPoLine(line)
                .withFundDistribution(new FundDistribution())
                .withFund(restrictedFund)
                .withLedger(restrictedLedger)
                .withBudget(restrictedBudget)
                .withToFYEncumbrance(transaction2);
        ReEncumbranceHolder holder3 = new ReEncumbranceHolder()
                .withPoLine(line)
                .withFundDistribution(new FundDistribution())
                .withFund(notRestrictedFund)
                .withLedger(notRestrictedLedger)
                .withBudget(notRestrictedBudget)
                .withToFYEncumbrance(transaction3);

        List<ReEncumbranceHolder> holders = List.of(holder1, holder2, holder3);

        when(compositePurchaseOrderService.getCompositeOrderById(anyString(), eq(requestContext)))
                .thenAnswer(invocation -> CompletableFuture.completedFuture(new CompositePurchaseOrder().withId(invocation.getArgument(0))));
        when(reEncumbranceHoldersBuilder.buildReEncumbranceHoldersWithOrdersData(any())).thenReturn(holders);
        when(reEncumbranceHoldersBuilder.withFunds(any(), any())).thenReturn(CompletableFuture.completedFuture(holders));
        when(reEncumbranceHoldersBuilder.withLedgers(any(), any())).thenReturn(CompletableFuture.completedFuture(holders));
        when(reEncumbranceHoldersBuilder.withCurrentFiscalYear(any(), any())).thenReturn(CompletableFuture.completedFuture(holders));
        when(reEncumbranceHoldersBuilder.withRollovers(any(), any())).thenReturn(CompletableFuture.completedFuture(holders));
        when(reEncumbranceHoldersBuilder.withEncumbranceRollover(any())).thenReturn(holders);
        when(reEncumbranceHoldersBuilder.withBudgets(any(), any())).thenReturn(CompletableFuture.completedFuture(holders));
        when(reEncumbranceHoldersBuilder.withConversion(any(), any())).thenReturn(CompletableFuture.completedFuture(holders));
        when(reEncumbranceHoldersBuilder.withEncumbrances(any(), any())).thenReturn(CompletableFuture.completedFuture(holders));
        doNothing().when(budgetRestrictionService).checkEnoughMoneyInBudgets(anyMap());
        when(rolloverErrorService.getLedgerFyRolloverErrors(anyString(), any()))
                .thenReturn(CompletableFuture.completedFuture(new LedgerFiscalYearRolloverErrorCollection()));
        when(rolloverErrorService.deleteRolloverErrors(anyList(), any())).thenReturn(completedFuture(null));
        when(purchaseOrderLineService.updateOrderLines(anyList(), any())).thenReturn(completedFuture(null));

        CompletableFuture<Void> future = orderReEncumberService.reEncumber(UUID.randomUUID().toString(), requestContext);
        future.join();
        assertFalse(future.isCompletedExceptionally());

        ArgumentCaptor<Map<Budget, List<Transaction>>> argumentCaptor = ArgumentCaptor.forClass(Map.class);
        verify(budgetRestrictionService).checkEnoughMoneyInBudgets(argumentCaptor.capture());
        Map<Budget, List<Transaction>> verifiedBudgetTransactionMap = argumentCaptor.getValue();

        assertThat(verifiedBudgetTransactionMap.entrySet(), hasSize(1));
        assertTrue(verifiedBudgetTransactionMap.containsKey(restrictedBudget));
        assertThat(verifiedBudgetTransactionMap.get(restrictedBudget), hasSize(2));

    }
}
