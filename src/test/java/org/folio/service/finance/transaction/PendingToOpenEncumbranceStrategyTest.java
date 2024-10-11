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
  import static org.mockito.ArgumentMatchers.argThat;
  import static org.mockito.ArgumentMatchers.eq;
  import static org.mockito.Mockito.doAnswer;
  import static org.mockito.Mockito.doReturn;
  import static org.mockito.Mockito.times;
  import static org.mockito.Mockito.verify;
  import static org.mockito.Mockito.when;

  import java.util.List;
  import java.util.UUID;

  import io.vertx.core.Vertx;
  import io.vertx.junit5.VertxExtension;
  import io.vertx.junit5.VertxTestContext;
  import org.folio.rest.acq.model.finance.AwaitingPayment;
  import org.folio.rest.acq.model.finance.Budget;
  import org.folio.rest.acq.model.finance.Encumbrance;
  import org.folio.rest.acq.model.finance.FiscalYear;
  import org.folio.rest.acq.model.finance.Fund;
  import org.folio.rest.acq.model.finance.Ledger;
  import org.folio.rest.acq.model.finance.Metadata;
  import org.folio.rest.acq.model.finance.Transaction;
  import org.folio.rest.acq.model.invoice.InvoiceLine;
  import org.folio.rest.core.exceptions.HttpException;
  import org.folio.rest.core.models.RequestContext;
  import org.folio.rest.jaxrs.model.CompositePoLine;
  import org.folio.rest.jaxrs.model.CompositePurchaseOrder;
  import org.folio.rest.jaxrs.model.FundDistribution;
  import org.folio.rest.jaxrs.model.Parameter;
  import org.folio.service.FundsDistributionService;
  import org.folio.service.exchange.ExchangeRateProviderResolver;
  import org.folio.service.exchange.ManualCurrencyConversion;
  import org.folio.service.finance.FiscalYearService;
  import org.folio.service.finance.FundService;
  import org.folio.service.finance.LedgerService;
  import org.folio.service.finance.budget.BudgetRestrictionService;
  import org.folio.service.finance.budget.BudgetService;
  import org.folio.service.invoice.InvoiceLineService;
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

  import javax.money.MonetaryAmount;
  import javax.money.convert.ConversionQuery;
  import javax.money.convert.ExchangeRateProvider;

@ExtendWith(MockitoExtension.class)
@ExtendWith(VertxExtension.class)
public class PendingToOpenEncumbranceStrategyTest {
  public static final String ORDER_PATH = COMP_ORDER_MOCK_DATA_PATH + PO_WFD_ID_OPEN_STATUS + ".json";

  private PendingToOpenEncumbranceStrategy pendingToOpenEncumbranceStrategy;
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
  private ExchangeRateProviderResolver exchangeRateProviderResolver;
  @Mock
  private ExchangeRateProvider exchangeRateProvider;
  @Mock
  private ManualCurrencyConversion currencyConversion;
  @Mock
  private BudgetService budgetService;
  @Mock
  private LedgerService ledgerService;
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
      encumbranceService, fundService, fiscalYearService, exchangeRateProviderResolver, budgetService, ledgerService);
    EncumbrancesProcessingHolderBuilder encumbrancesProcessingHolderBuilder = new EncumbrancesProcessingHolderBuilder();
    PendingPaymentService pendingPaymentService = new PendingPaymentService(transactionService);
    POLInvoiceLineRelationService polInvoiceLineRelationService = new POLInvoiceLineRelationService(invoiceLineService,
      pendingPaymentService);
    pendingToOpenEncumbranceStrategy = new PendingToOpenEncumbranceStrategy(encumbranceService,
      fundsDistributionService, budgetRestrictionService, encumbranceRelationsHoldersBuilder,
      encumbrancesProcessingHolderBuilder, polInvoiceLineRelationService);
  }

  @Test
  void testUpdatingAReleasedEncumbranceWithACancelledInvoice(VertxTestContext vertxTestContext) {
    // Given
    CompositePurchaseOrder order = getMockAsJson(ORDER_PATH).mapTo(CompositePurchaseOrder.class);
    CompositePoLine poLine = order.getCompositePoLines().get(0);
    FundDistribution fd1 = poLine.getFundDistribution().get(0);
    String fundId1 = fd1.getFundId();
    String fundId2 = "1b6d3338-186e-4e35-9e75-1b886b0da53e";
    String encumbranceId = fd1.getEncumbrance();
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

    InvoiceLine invoiceLine = new InvoiceLine()
      .withId(invoiceLineId)
      .withInvoiceLineStatus(InvoiceLine.InvoiceLineStatus.CANCELLED);

    doReturn(Vertx.vertx().getOrCreateContext())
      .when(requestContext).getContext();
    doReturn(exchangeRateProvider)
      .when(exchangeRateProviderResolver).resolve(any(), eq(requestContext));
    doReturn(currencyConversion)
      .when(exchangeRateProvider).getCurrencyConversion(any(ConversionQuery.class));
    doAnswer(invocation -> invocation.getArgument(0))
      .when(currencyConversion).apply(any(MonetaryAmount.class));
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
        assertEquals(1, transactionLists.get(0).size());
        assertEquals(Encumbrance.Status.RELEASED, transactionLists.get(0).get(0).getEncumbrance().getStatus());
        vertxTestContext.completeNow();
      }))
      .onFailure(vertxTestContext::failNow);
  }

  @Test
  void testDeletingAnEncumbranceWithAPendingPayment(VertxTestContext vertxTestContext) {
    // Given
    CompositePurchaseOrder order = getMockAsJson(ORDER_PATH).mapTo(CompositePurchaseOrder.class);
    CompositePoLine poLine = order.getCompositePoLines().get(0);
    FundDistribution fd = poLine.getFundDistribution().get(0);
    String fundId = fd.getFundId();
    String encumbranceId = fd.getEncumbrance();
    String invoiceLineId = UUID.randomUUID().toString();
    String invoiceId = UUID.randomUUID().toString();
    String pendingPaymentId = UUID.randomUUID().toString();

    CompositePurchaseOrder orderFromStorage = JsonObject.mapFrom(order).mapTo(CompositePurchaseOrder.class);
    poLine.getFundDistribution().remove(0);

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

    doReturn(succeededFuture(invoiceLines))
      .doReturn(succeededFuture(invoiceLines))
      .when(invoiceLineService).getInvoiceLinesByOrderLineIds(anyList(), eq(requestContext));
    String expectedQuery = "awaitingPayment.encumbranceId==(" + encumbranceId + ")";
    doReturn(succeededFuture(pendingPayments))
      .when(transactionService).getTransactions(eq(expectedQuery), eq(requestContext));
    doReturn(succeededFuture(singletonList(released)))
      .when(transactionService).getTransactionsByIds(argThat(list -> list.size() == 1), eq(requestContext));
    doReturn(succeededFuture(null))
      .when(transactionService).batchAllOrNothing(any(), any(), any(), any(), eq(requestContext));
    doReturn(succeededFuture(TRUE))
      .when(orderInvoiceRelationService).isOrderLinkedToAnInvoice(eq(order.getId()), eq(requestContext));
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
        assertEquals(2, transactionLists.get(0).size());
        assertEquals(1, idLists.get(0).size());
        assertEquals(Encumbrance.Status.RELEASED, transactionLists.get(0).get(0).getEncumbrance().getStatus());
        Transaction updatedEncumbrance = transactionLists.get(0).get(0);
        assertEquals(ENCUMBRANCE, updatedEncumbrance.getTransactionType());
        assertEquals(encumbranceId, updatedEncumbrance.getId());
        String deletedId = idLists.get(0).get(0);
        assertEquals(encumbranceId, deletedId);
        Transaction updatedPendingPayment = transactionLists.get(0).get(1);
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
    CompositePoLine poLine = order.getCompositePoLines().get(0);
    FundDistribution fd = poLine.getFundDistribution().get(0);
    String fundId = fd.getFundId();
    String encumbranceId = fd.getEncumbrance();

    CompositePurchaseOrder orderFromStorage = JsonObject.mapFrom(order).mapTo(CompositePurchaseOrder.class);
    poLine.getFundDistribution().remove(0);

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
}
