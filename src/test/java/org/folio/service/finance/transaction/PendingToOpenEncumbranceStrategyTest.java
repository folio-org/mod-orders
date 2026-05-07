package org.folio.service.finance.transaction;

import static io.vertx.core.Future.succeededFuture;
import static java.lang.Boolean.TRUE;
import static java.util.Collections.singletonList;
import static javax.money.Monetary.getDefaultRounding;
import static org.folio.TestConstants.COMP_ORDER_MOCK_DATA_PATH;
import static org.folio.TestConstants.PO_WFD_ID_OPEN_STATUS;
import static org.folio.TestUtils.getMockAsJson;
import static org.folio.rest.acq.model.finance.Transaction.TransactionType.ENCUMBRANCE;
import static org.folio.rest.acq.model.finance.Transaction.TransactionType.PENDING_PAYMENT;
import static org.folio.rest.core.exceptions.ErrorCodes.DELETE_WITH_EXPENDED_AMOUNT;
import static org.junit.jupiter.api.Assertions.assertEquals;
import static org.junit.jupiter.api.Assertions.assertNull;
import static org.junit.jupiter.api.Assertions.assertTrue;
import static org.mockito.ArgumentMatchers.any;
import static org.mockito.ArgumentMatchers.anyCollection;
import static org.mockito.ArgumentMatchers.anyList;
import static org.mockito.ArgumentMatchers.anyString;
import static org.mockito.ArgumentMatchers.nullable;
import static org.mockito.ArgumentMatchers.argThat;
import static org.mockito.ArgumentMatchers.eq;
import static org.mockito.Mockito.doReturn;
import static org.mockito.Mockito.times;
import static org.mockito.Mockito.verify;

import java.util.List;
import java.util.UUID;

import io.vertx.junit5.VertxExtension;
import io.vertx.junit5.VertxTestContext;
import org.folio.CopilotGenerated;
import org.folio.models.EncumbranceRelationsHolder;
import org.folio.rest.acq.model.finance.AwaitingPayment;
import org.folio.rest.acq.model.finance.Budget;
import org.folio.rest.acq.model.finance.Encumbrance;
import org.folio.rest.acq.model.finance.ExchangeRate;
import org.folio.rest.acq.model.finance.FiscalYear;
import org.folio.rest.acq.model.finance.Fund;
import org.folio.rest.acq.model.finance.Ledger;
import org.folio.rest.acq.model.finance.Metadata;
import org.folio.rest.acq.model.finance.Transaction;
import org.folio.rest.acq.model.invoice.Invoice;
import org.folio.rest.acq.model.invoice.InvoiceLine;
import org.folio.rest.core.exceptions.HttpException;
import org.folio.rest.core.models.RequestContext;
import org.folio.rest.jaxrs.model.CompositePurchaseOrder;
import org.folio.rest.jaxrs.model.FundDistribution;
import org.folio.rest.jaxrs.model.Parameter;
import org.folio.rest.jaxrs.model.PoLine;
import org.folio.service.FundsDistributionService;
import org.folio.service.exchange.CacheableExchangeRateService;
import org.folio.service.finance.FiscalYearService;
import org.folio.service.finance.FundService;
import org.folio.service.finance.LedgerService;
import org.folio.service.finance.budget.BudgetRestrictionService;
import org.folio.service.finance.budget.BudgetService;
import org.folio.service.invoice.InvoiceLineService;
import org.folio.service.invoice.InvoiceService;
import org.folio.service.invoice.POLInvoiceLineRelationService;
import org.folio.service.orders.OrderInvoiceRelationService;
import org.junit.jupiter.api.BeforeEach;
import org.junit.jupiter.api.Test;
import org.junit.jupiter.api.extension.ExtendWith;
import org.mockito.ArgumentCaptor;
import org.mockito.Captor;
import org.mockito.Mock;
import org.mockito.junit.jupiter.MockitoExtension;

import io.vertx.core.Future;
import io.vertx.core.json.JsonObject;

@ExtendWith(MockitoExtension.class)
@ExtendWith(VertxExtension.class)
public class PendingToOpenEncumbranceStrategyTest {

  public static final String ORDER_PATH = COMP_ORDER_MOCK_DATA_PATH + PO_WFD_ID_OPEN_STATUS + ".json";

  private PendingToOpenEncumbranceStrategy pendingToOpenEncumbranceStrategy;
  @Mock
  private InvoiceService invoiceService;
  @Mock
  private InvoiceLineService invoiceLineService;
  @Mock
  private TransactionService transactionService;
  @Mock
  private OrderInvoiceRelationService orderInvoiceRelationService;
  @Mock
  private FundService fundService;
  @Mock
  private FiscalYearService fiscalYearService;
  @Mock
  private ExchangeRate exchangeRate;
  @Mock
  private BudgetService budgetService;
  @Mock
  private LedgerService ledgerService;
  @Mock
  private CacheableExchangeRateService cacheableExchangeRateService;
  @Mock
  private RequestContext requestContext;
  @Captor
  ArgumentCaptor<List<Transaction>> transactionListCaptor;
  @Captor
  ArgumentCaptor<List<String>> idListCaptor;

  @BeforeEach
  void init() {
    EncumbranceService encumbranceService = new EncumbranceService(transactionService,
      invoiceLineService, orderInvoiceRelationService, fiscalYearService);
    FundsDistributionService fundsDistributionService = new FundsDistributionService();
    BudgetRestrictionService budgetRestrictionService = new BudgetRestrictionService();
    EncumbranceRelationsHoldersBuilder encumbranceRelationsHoldersBuilder = new EncumbranceRelationsHoldersBuilder(
      encumbranceService, fundService, fiscalYearService, budgetService, ledgerService, cacheableExchangeRateService);
    EncumbrancesProcessingHolderBuilder encumbrancesProcessingHolderBuilder = new EncumbrancesProcessingHolderBuilder();
    PendingPaymentService pendingPaymentService = new PendingPaymentService(transactionService);
    POLInvoiceLineRelationService polInvoiceLineRelationService = new POLInvoiceLineRelationService(invoiceService, invoiceLineService,
      pendingPaymentService);
    pendingToOpenEncumbranceStrategy = new PendingToOpenEncumbranceStrategy(encumbranceService,
      fundsDistributionService, budgetRestrictionService, encumbranceRelationsHoldersBuilder,
      encumbrancesProcessingHolderBuilder, polInvoiceLineRelationService);
  }

  @Test
  void testUpdatingAReleasedEncumbranceWithACancelledInvoice(VertxTestContext vertxTestContext) {
    // Given
    CompositePurchaseOrder order = getMockAsJson(ORDER_PATH).mapTo(CompositePurchaseOrder.class);
    PoLine poLine = order.getPoLines().getFirst();
    FundDistribution fd1 = poLine.getFundDistribution().getFirst();
    String fundId1 = fd1.getFundId();
    String fundId2 = "1b6d3338-186e-4e35-9e75-1b886b0da53e";
    String encumbranceId = fd1.getEncumbrance();
    String invoiceId = UUID.randomUUID().toString();
    String invoiceLineId = UUID.randomUUID().toString();

    CompositePurchaseOrder orderFromStorage = JsonObject.mapFrom(order).mapTo(CompositePurchaseOrder.class);
    fd1.setValue(50d);
    FundDistribution fd2 = new FundDistribution()
      .withDistributionType(FundDistribution.DistributionType.PERCENTAGE)
      .withValue(50d)
      .withCode("GENRL")
      .withFundId("1b6d3338-186e-4e35-9e75-1b886b0da53e");
    poLine.getFundDistribution().add(fd2);

    Encumbrance encumbrance = new Encumbrance()
      .withSourcePurchaseOrderId(order.getId())
      .withSourcePoLineId(poLine.getId())
      .withOrderType(Encumbrance.OrderType.fromValue(order.getOrderType().value()))
      .withInitialAmountEncumbered(10d)
      .withOrderStatus(Encumbrance.OrderStatus.OPEN)
      .withStatus(Encumbrance.Status.RELEASED);
    Transaction released = new Transaction()
      .withTransactionType(ENCUMBRANCE)
      .withAmount(0d)
      .withId(encumbranceId)
      .withFromFundId(fundId1)
      .withEncumbrance(encumbrance)
      .withMetadata(new Metadata());
    Transaction unreleased = JsonObject.mapFrom(released).mapTo(Transaction.class);
    unreleased.getEncumbrance().setStatus(Encumbrance.Status.UNRELEASED);

    String fiscalYearId = UUID.randomUUID().toString();
    Budget budget1 = new Budget().withId(UUID.randomUUID().toString())
      .withFundId(fundId1)
      .withFiscalYearId(fiscalYearId);
    Budget budget2 = new Budget().withId(UUID.randomUUID().toString())
      .withFundId(fundId2)
      .withFiscalYearId(fiscalYearId);
    Fund fund1 = new Fund().withId(fundId1).withLedgerId(UUID.randomUUID().toString());
    Fund fund2 = new Fund().withId(fundId2).withLedgerId(UUID.randomUUID().toString());
    Ledger ledger = new Ledger().withId(fund1.getLedgerId()).withRestrictEncumbrance(true);
    FiscalYear fiscalYear = new FiscalYear().withId(fiscalYearId).withCurrency("USD");

    doReturn(Future.succeededFuture(List.of(fund1, fund2)))
      .when(fundService).getAllFunds(anyCollection(), any());
    doReturn(Future.succeededFuture(List.of(ledger)))
      .when(ledgerService).getLedgersByIds(anyCollection(), any());
    doReturn(Future.succeededFuture(fiscalYear))
      .when(fiscalYearService).getCurrentFiscalYear(anyString(), any());
    doReturn(Future.succeededFuture(List.of(budget1, budget2)))
      .when(budgetService).getBudgetsByQuery(anyString(), any());

    Invoice invoice = new Invoice()
      .withId(invoiceId)
      .withFiscalYearId(fiscalYearId);
    InvoiceLine invoiceLine = new InvoiceLine()
      .withId(invoiceLineId)
      .withInvoiceLineStatus(InvoiceLine.InvoiceLineStatus.CANCELLED);

    doReturn(Future.succeededFuture(exchangeRate))
      .when(cacheableExchangeRateService).getExchangeRate(any(), any(), any(), eq(requestContext));
    doReturn(succeededFuture(List.of(invoice)))
      .when(invoiceService).getInvoicesByOrderId(nullable(String.class), eq(requestContext));
    doReturn(succeededFuture(List.of(invoiceLine)))
      .when(invoiceLineService).getInvoiceLinesByOrderLineIds(anyList(), eq(requestContext));
    doReturn(succeededFuture(singletonList(released)))
      .when(transactionService).getTransactionsByIds(argThat(list -> list.size() == 1), eq(requestContext));
    doReturn(succeededFuture(null))
      .when(transactionService).batchAllOrNothing(any(), any(), any(), any(), eq(requestContext));
    getDefaultRounding();

    // When
    Future<Void> future = pendingToOpenEncumbranceStrategy.processEncumbrances(order, orderFromStorage, requestContext);

    // Then
    vertxTestContext.assertComplete(future)
      .onSuccess(result -> vertxTestContext.verify(() -> {
        verify(transactionService, times(1))
          .batchAllOrNothing(any(), transactionListCaptor.capture(), any(), any(), eq(requestContext));
        verify(transactionService, times(1))
          .getTransactionsByIds(anyList(), eq(requestContext));
        List<List<Transaction>> transactionLists = transactionListCaptor.getAllValues();
        assertEquals(1, transactionLists.size());
        assertEquals(1, transactionLists.getFirst().size());
        assertEquals(Encumbrance.Status.RELEASED, transactionLists.getFirst().getFirst().getEncumbrance().getStatus());
        vertxTestContext.completeNow();
      }))
      .onFailure(vertxTestContext::failNow);
  }

  @Test
  void testDeletingAnEncumbranceWithAPendingPayment(VertxTestContext vertxTestContext) {
    // Given
    CompositePurchaseOrder order = getMockAsJson(ORDER_PATH).mapTo(CompositePurchaseOrder.class);
    PoLine poLine = order.getPoLines().getFirst();
    FundDistribution fd = poLine.getFundDistribution().getFirst();
    String fundId = fd.getFundId();
    String encumbranceId = fd.getEncumbrance();
    String invoiceLineId = UUID.randomUUID().toString();
    String invoiceId = UUID.randomUUID().toString();
    String pendingPaymentId = UUID.randomUUID().toString();

    CompositePurchaseOrder orderFromStorage = JsonObject.mapFrom(order).mapTo(CompositePurchaseOrder.class);
    poLine.getFundDistribution().removeFirst();

    Encumbrance encumbrance = new Encumbrance()
      .withSourcePurchaseOrderId(order.getId())
      .withSourcePoLineId(poLine.getId())
      .withOrderType(Encumbrance.OrderType.fromValue(order.getOrderType().value()))
      .withInitialAmountEncumbered(10d)
      .withOrderStatus(Encumbrance.OrderStatus.OPEN)
      .withStatus(Encumbrance.Status.RELEASED);
    Transaction released = new Transaction()
      .withTransactionType(ENCUMBRANCE)
      .withAmount(0d)
      .withId(encumbranceId)
      .withFromFundId(fundId)
      .withEncumbrance(encumbrance)
      .withMetadata(new Metadata());
    Transaction unreleased = JsonObject.mapFrom(released).mapTo(Transaction.class);
    unreleased.getEncumbrance().setStatus(Encumbrance.Status.UNRELEASED);

    String fiscalYearId = UUID.randomUUID().toString();
    Invoice invoice = new Invoice()
      .withId(invoiceId)
      .withFiscalYearId(fiscalYearId);
    InvoiceLine invoiceLine = new InvoiceLine()
      .withId(invoiceLineId)
      .withInvoiceLineStatus(InvoiceLine.InvoiceLineStatus.CANCELLED);
    List<InvoiceLine> invoiceLines = List.of(invoiceLine);

    Transaction pendingPayment = new Transaction()
      .withId(pendingPaymentId)
      .withTransactionType(PENDING_PAYMENT)
      .withSourceInvoiceId(invoiceId)
      .withSourceInvoiceLineId(invoiceLineId)
      .withFromFundId(fundId)
      .withFiscalYearId(fiscalYearId)
      .withCurrency("USD")
      .withInvoiceCancelled(true)
      .withAwaitingPayment(new AwaitingPayment()
        .withEncumbranceId(encumbranceId));
    List<Transaction> pendingPayments = List.of(pendingPayment);

    doReturn(succeededFuture(List.of(invoice)))
      .when(invoiceService).getInvoicesByOrderId(nullable(String.class), eq(requestContext));
    doReturn(succeededFuture(invoiceLines))
      .doReturn(succeededFuture(invoiceLines))
      .when(invoiceLineService).getInvoiceLinesByOrderLineIds(anyList(), eq(requestContext));
    String expectedQuery = "awaitingPayment.encumbranceId==(" + encumbranceId + ")";
    doReturn(succeededFuture(pendingPayments))
      .when(transactionService).getTransactions(expectedQuery, requestContext);
    doReturn(succeededFuture(singletonList(released)))
      .when(transactionService).getTransactionsByIds(argThat(list -> list.size() == 1), eq(requestContext));
    doReturn(succeededFuture(null))
      .when(transactionService).batchAllOrNothing(any(), any(), any(), any(), eq(requestContext));
    doReturn(succeededFuture(TRUE))
      .when(orderInvoiceRelationService).isOrderLinkedToAnInvoice(order.getId(), requestContext);
    doReturn(succeededFuture(null))
      .when(invoiceLineService).removeEncumbranceLinks(anyList(), anyList(), eq(requestContext));
    getDefaultRounding();

    // When
    Future<Void> future = pendingToOpenEncumbranceStrategy.processEncumbrances(order, orderFromStorage, requestContext);

    // Then
    vertxTestContext.assertComplete(future)
      .onSuccess(result -> vertxTestContext.verify(() -> {
        verify(transactionService, times(1))
          .batchAllOrNothing(any(), transactionListCaptor.capture(), idListCaptor.capture(), any(), eq(requestContext));
        verify(transactionService, times(1))
          .getTransactionsByIds(anyList(), eq(requestContext));
        List<List<Transaction>> transactionLists = transactionListCaptor.getAllValues();
        List<List<String>> idLists = idListCaptor.getAllValues();
        assertEquals(1, transactionLists.size());
        assertEquals(1, idLists.size());
        assertEquals(2, transactionLists.getFirst().size());
        assertEquals(1, idLists.getFirst().size());
        assertEquals(Encumbrance.Status.RELEASED, transactionLists.getFirst().getFirst().getEncumbrance().getStatus());
        Transaction updatedEncumbrance = transactionLists.getFirst().getFirst();
        assertEquals(ENCUMBRANCE, updatedEncumbrance.getTransactionType());
        assertEquals(encumbranceId, updatedEncumbrance.getId());
        String deletedId = idLists.getFirst().getFirst();
        assertEquals(encumbranceId, deletedId);
        Transaction updatedPendingPayment = transactionLists.getFirst().get(1);
        assertEquals(PENDING_PAYMENT, updatedPendingPayment.getTransactionType());
        assertEquals(pendingPaymentId, updatedPendingPayment.getId());
        assertNull(updatedPendingPayment.getAwaitingPayment().getEncumbranceId());
        vertxTestContext.completeNow();
      }))
      .onFailure(vertxTestContext::failNow);
  }

  @Test
  void testDeletingAnEncumbranceWithExpendedAmountGreaterThanZero(VertxTestContext vertxTestContext) {
    // Given
    CompositePurchaseOrder order = getMockAsJson(ORDER_PATH).mapTo(CompositePurchaseOrder.class);
    PoLine poLine = order.getPoLines().getFirst();
    FundDistribution fd = poLine.getFundDistribution().getFirst();
    String fundId = fd.getFundId();
    String encumbranceId = fd.getEncumbrance();

    CompositePurchaseOrder orderFromStorage = JsonObject.mapFrom(order).mapTo(CompositePurchaseOrder.class);
    poLine.getFundDistribution().removeFirst();

    Encumbrance encumbrance = new Encumbrance()
      .withSourcePurchaseOrderId(order.getId())
      .withSourcePoLineId(poLine.getId())
      .withOrderType(Encumbrance.OrderType.fromValue(order.getOrderType().value()))
      .withInitialAmountEncumbered(10d)
      .withAmountExpended(18d)
      .withAmountCredited(8d)
      .withOrderStatus(Encumbrance.OrderStatus.OPEN)
      .withStatus(Encumbrance.Status.RELEASED);
    Transaction released = new Transaction()
      .withTransactionType(ENCUMBRANCE)
      .withAmount(0d)
      .withId(encumbranceId)
      .withFromFundId(fundId)
      .withEncumbrance(encumbrance)
      .withMetadata(new Metadata());

    doReturn(succeededFuture(null))
      .when(invoiceService).getInvoicesByOrderId(nullable(String.class), eq(requestContext));
    doReturn(succeededFuture(List.of()))
      .when(invoiceLineService).getInvoiceLinesByOrderLineIds(anyList(), eq(requestContext));
    doReturn(succeededFuture(singletonList(released)))
      .when(transactionService).getTransactionsByIds(argThat(list -> list.size() == 1), eq(requestContext));

    // When
    Future<Void> future = pendingToOpenEncumbranceStrategy.processEncumbrances(order, orderFromStorage, requestContext);

    // Then
    vertxTestContext.assertFailure(future)
      .onComplete(result -> {
        assertTrue(result.failed());
        HttpException exception = (HttpException) result.cause();
        assertEquals(422, exception.getCode());
        List<Parameter> expectedParameters = List.of(
          new Parameter().withKey("id").withValue(encumbranceId)
        );
        assertEquals(DELETE_WITH_EXPENDED_AMOUNT.toError().withParameters(expectedParameters), exception.getError());
        vertxTestContext.completeNow();
      });
  }

  @Test
  @CopilotGenerated(model = "Claude 3.5 Sonnet")
  void testPrepareProcessEncumbrancesAndValidateShouldSetFiscalYearId(VertxTestContext vertxTestContext) {
    // Given
    var order = getMockAsJson(ORDER_PATH).mapTo(CompositePurchaseOrder.class);
    var poLine = order.getPoLines().getFirst();
    var fd = poLine.getFundDistribution().getFirst();
    var fundId = fd.getFundId();
    var fiscalYearId = UUID.randomUUID().toString();
    var encumbranceId = fd.getEncumbrance();

    var orderFromStorage = JsonObject.mapFrom(order).mapTo(CompositePurchaseOrder.class);

    var fund = new Fund().withId(fundId)
      .withLedgerId(UUID.randomUUID().toString());
    var ledger = new Ledger().withId(fund.getLedgerId())
      .withRestrictEncumbrance(true);
    var fiscalYear = new FiscalYear().withId(fiscalYearId)
      .withCurrency("USD");
    var budget = new Budget().withId(UUID.randomUUID().toString())
      .withFundId(fundId)
      .withFiscalYearId(fiscalYearId);

    // Create a sample transaction with all required fields
    var encumbrance = new Encumbrance()
      .withSourcePurchaseOrderId(order.getId())
      .withSourcePoLineId(poLine.getId())
      .withStatus(Encumbrance.Status.UNRELEASED)
      .withAmountExpended(0.0)
      .withAmountAwaitingPayment(0.0)
      .withInitialAmountEncumbered(100.0)
      .withOrderType(Encumbrance.OrderType.fromValue(order.getOrderType().value()));

    var transaction = new Transaction()
      .withId(encumbranceId)
      .withTransactionType(Transaction.TransactionType.ENCUMBRANCE)
      .withFromFundId(fundId)
      .withAmount(100.0)
      .withCurrency("USD")
      .withFiscalYearId(fiscalYearId)
      .withSource(Transaction.Source.PO_LINE)
      .withEncumbrance(encumbrance)
      .withMetadata(new Metadata());

    // Setup mocks
    doReturn(Future.succeededFuture(List.of(fund)))
      .when(fundService).getAllFunds(anyCollection(), any());
    doReturn(Future.succeededFuture(List.of(ledger)))
      .when(ledgerService).getLedgersByIds(anyCollection(), any());
    doReturn(Future.succeededFuture(fiscalYear))
      .when(fiscalYearService).getCurrentFiscalYear(anyString(), any());
    doReturn(Future.succeededFuture(List.of(budget)))
      .when(budgetService).getBudgetsByQuery(anyString(), any());
    doReturn(Future.succeededFuture(exchangeRate))
      .when(cacheableExchangeRateService).getExchangeRate(any(), any(), any(), eq(requestContext));
    doReturn(Future.succeededFuture(List.of(transaction)))
      .when(transactionService).getTransactionsByIds(anyList(), eq(requestContext));

    // When
    Future<List<EncumbranceRelationsHolder>> future = pendingToOpenEncumbranceStrategy
      .prepareProcessEncumbrancesAndValidate(order, orderFromStorage, requestContext);

    // Then
    vertxTestContext.assertComplete(future)
      .onSuccess(holders -> vertxTestContext.verify(() -> {
        assertEquals(fiscalYearId, order.getFiscalYearId());
        assertEquals(1, holders.size());
        var holder = holders.getFirst();
        assertEquals(fiscalYearId, holder.getCurrentFiscalYearId());
        vertxTestContext.completeNow();
      }))
      .onFailure(vertxTestContext::failNow);
  }
}
