package org.folio.service.finance.transaction;

  import static io.vertx.core.Future.succeededFuture;
  import static java.util.Collections.singletonList;
  import static javax.money.Monetary.getDefaultRounding;
  import static org.folio.TestConstants.COMP_ORDER_MOCK_DATA_PATH;
  import static org.folio.TestConstants.PO_WFD_ID_OPEN_STATUS;
  import static org.folio.TestUtils.getMockAsJson;
  import static org.junit.jupiter.api.Assertions.assertEquals;
  import static org.mockito.ArgumentMatchers.any;
  import static org.mockito.ArgumentMatchers.anyInt;
  import static org.mockito.ArgumentMatchers.anyList;
  import static org.mockito.ArgumentMatchers.anyString;
  import static org.mockito.ArgumentMatchers.argThat;
  import static org.mockito.ArgumentMatchers.eq;
  import static org.mockito.Mockito.doAnswer;
  import static org.mockito.Mockito.doCallRealMethod;
  import static org.mockito.Mockito.doReturn;
  import static org.mockito.Mockito.times;
  import static org.mockito.Mockito.verify;

  import java.util.List;
  import java.util.UUID;

  import io.vertx.core.Vertx;
  import io.vertx.junit5.VertxExtension;
  import io.vertx.junit5.VertxTestContext;
  import org.folio.rest.acq.model.finance.Budget;
  import org.folio.rest.acq.model.finance.Encumbrance;
  import org.folio.rest.acq.model.finance.FiscalYear;
  import org.folio.rest.acq.model.finance.Fund;
  import org.folio.rest.acq.model.finance.Ledger;
  import org.folio.rest.acq.model.finance.OrderTransactionSummary;
  import org.folio.rest.acq.model.finance.Transaction;
  import org.folio.rest.core.models.RequestContext;
  import org.folio.rest.jaxrs.model.CompositePoLine;
  import org.folio.rest.jaxrs.model.CompositePurchaseOrder;
  import org.folio.rest.jaxrs.model.FundDistribution;
  import org.folio.service.FundsDistributionService;
  import org.folio.service.exchange.ExchangeRateProviderResolver;
  import org.folio.service.exchange.ManualCurrencyConversion;
  import org.folio.service.finance.FiscalYearService;
  import org.folio.service.finance.FundService;
  import org.folio.service.finance.LedgerService;
  import org.folio.service.finance.budget.BudgetRestrictionService;
  import org.folio.service.finance.budget.BudgetService;
  import org.folio.service.finance.transaction.summary.OrderTransactionSummariesService;
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
  private OrderTransactionSummariesService orderTransactionSummariesService;
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
  POLInvoiceLineRelationService polInvoiceLineRelationService;
  @Mock
  private RequestContext requestContext;
  @Captor
  ArgumentCaptor<List<Transaction>> transactionListCaptor;

  @BeforeEach
  void init() {
    EncumbranceService encumbranceService = new EncumbranceService(transactionService, orderTransactionSummariesService,
      invoiceLineService, orderInvoiceRelationService, fiscalYearService);

    FundsDistributionService fundsDistributionService = new FundsDistributionService();
    BudgetRestrictionService budgetRestrictionService = new BudgetRestrictionService();
    EncumbranceRelationsHoldersBuilder encumbranceRelationsHoldersBuilder = new EncumbranceRelationsHoldersBuilder(
      encumbranceService, fundService, fiscalYearService, exchangeRateProviderResolver, budgetService, ledgerService);
    EncumbrancesProcessingHolderBuilder encumbrancesProcessingHolderBuilder = new EncumbrancesProcessingHolderBuilder();

    pendingToOpenEncumbranceStrategy = new PendingToOpenEncumbranceStrategy(encumbranceService,
      fundsDistributionService, budgetRestrictionService, encumbranceRelationsHoldersBuilder,
      encumbrancesProcessingHolderBuilder, polInvoiceLineRelationService);

    doReturn(Vertx.vertx().getOrCreateContext())
      .when(requestContext).getContext();

    doReturn(exchangeRateProvider)
      .when(exchangeRateProviderResolver).resolve(any(), eq(requestContext));
    doReturn(currencyConversion)
      .when(exchangeRateProvider).getCurrencyConversion(any(ConversionQuery.class));
    doAnswer(invocation -> invocation.getArgument(0))
      .when(currencyConversion).apply(any(MonetaryAmount.class));
  }

  @Test
  void testUnreleaseBeforeAndReleaseAfterWhenUpdatingAReleasedEncumbrance(VertxTestContext vertxTestContext) {
    // Given
    CompositePurchaseOrder order = getMockAsJson(ORDER_PATH).mapTo(CompositePurchaseOrder.class);
    CompositePoLine poLine = order.getCompositePoLines().get(0);
    FundDistribution fd1 = poLine.getFundDistribution().get(0);
    String fundId1 = fd1.getFundId();
    String fundId2 = "1b6d3338-186e-4e35-9e75-1b886b0da53e";

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
      .withAmount(0d)
      .withId(fd1.getEncumbrance())
      .withFromFundId(fd1.getFundId())
      .withEncumbrance(encumbrance);
    Transaction unreleased = JsonObject.mapFrom(released).mapTo(Transaction.class);
    unreleased.getEncumbrance().setStatus(Encumbrance.Status.UNRELEASED);

    String fiscalYearId = UUID.randomUUID().toString();
    Budget budget1 = new Budget().withId(UUID.randomUUID().toString())
      .withFundId(fundId1)
      .withFiscalYearId(fiscalYearId);
    Budget budget2 = new Budget().withId(UUID.randomUUID().toString())
      .withFundId(fundId2)
      .withFiscalYearId(fiscalYearId);
    doReturn(succeededFuture(List.of(budget1, budget2)))
      .when(budgetService).getBudgets(anyList(), eq(requestContext));

    Fund fund1 = new Fund().withId(fundId1).withLedgerId(UUID.randomUUID().toString());
    Fund fund2 = new Fund().withId(fundId2).withLedgerId(UUID.randomUUID().toString());
    doReturn(succeededFuture(List.of(fund1, fund2)))
      .when(fundService).getAllFunds(anyList(), eq(requestContext));

    Ledger ledger = new Ledger().withId(fund1.getLedgerId()).withRestrictEncumbrance(true);
    doReturn(succeededFuture(List.of(ledger)))
      .when(ledgerService).getLedgersByIds(anyList(), eq(requestContext));

    FiscalYear fiscalYear = new FiscalYear().withId(fiscalYearId).withCurrency("USD");
    doReturn(succeededFuture(fiscalYear))
      .when(fiscalYearService).getFiscalYearById(anyString(), eq(requestContext));

    doAnswer(i -> succeededFuture(i.getArguments()[0]))
      .when(polInvoiceLineRelationService).manageInvoiceRelation(any(), eq(requestContext));

    doReturn(succeededFuture(singletonList(released)))
      .doReturn(succeededFuture(singletonList(unreleased)))
      .doReturn(succeededFuture(singletonList(unreleased)))
      .doThrow(new RuntimeException("Too many invocations of getTransactionsByIds()"))
      .when(transactionService).getTransactionsByIds(argThat(list -> list.size() == 1), eq(requestContext));
    doReturn(succeededFuture(null))
      .when(transactionService).updateTransactions(anyList(), eq(requestContext));
    doAnswer(i -> succeededFuture(i.getArguments()[0]))
      .when(transactionService).createTransaction(any(Transaction.class), eq(requestContext));
    OrderTransactionSummary orderTransactionSummary = new OrderTransactionSummary()
      .withId(UUID.randomUUID().toString())
      .withNumTransactions(123);
    doReturn(succeededFuture(orderTransactionSummary))
      .when(orderTransactionSummariesService).getTransactionSummary(anyString(), eq(requestContext));
    doCallRealMethod()
      .when(orderTransactionSummariesService).updateTransactionSummary(anyString(), anyInt(), eq(requestContext));
    doCallRealMethod()
      .when(orderTransactionSummariesService).updateOrCreateTransactionSummary(anyString(), anyInt(), eq(requestContext));
    doCallRealMethod()
      .when(orderTransactionSummariesService).createOrUpdateOrderTransactionSummary(any(), eq(requestContext));
    doReturn(succeededFuture(null))
      .when(orderTransactionSummariesService).updateTransactionSummary(any(), eq(requestContext));
    getDefaultRounding();

    // When
    Future<Void> future = pendingToOpenEncumbranceStrategy.processEncumbrances(order, orderFromStorage, requestContext);

    // Then
    vertxTestContext.assertComplete(future)
      .onSuccess(result -> vertxTestContext.verify(() -> {
        verify(transactionService, times(3))
          .updateTransactions(transactionListCaptor.capture(), eq(requestContext));
        List<List<Transaction>> transactionLists = transactionListCaptor.getAllValues();
        assertEquals(3, transactionLists.size());
        assertEquals(1, transactionLists.get(0).size());
        assertEquals(Encumbrance.Status.UNRELEASED, transactionLists.get(0).get(0).getEncumbrance().getStatus());
        assertEquals(1, transactionLists.get(1).size());
        assertEquals(Encumbrance.Status.UNRELEASED, transactionLists.get(1).get(0).getEncumbrance().getStatus());
        assertEquals(1, transactionLists.get(2).size());
        assertEquals(Encumbrance.Status.RELEASED, transactionLists.get(2).get(0).getEncumbrance().getStatus());
        vertxTestContext.completeNow();
      }))
      .onFailure(vertxTestContext::failNow);
  }
}
