package org.folio.service.finance.transaction;

import static io.vertx.core.Future.succeededFuture;
import static org.folio.TestConstants.COMP_ORDER_MOCK_DATA_PATH;
import static org.folio.TestUtils.getMockAsJson;
import static org.folio.rest.acq.model.finance.Transaction.TransactionType.PENDING_PAYMENT;
import static org.folio.rest.core.exceptions.ErrorCodes.DELETE_WITH_EXPENDED_AMOUNT;
import static org.junit.jupiter.api.Assertions.assertEquals;
import static org.junit.jupiter.api.Assertions.assertNull;
import static org.junit.jupiter.api.Assertions.assertTrue;
import static org.mockito.ArgumentMatchers.any;
import static org.mockito.ArgumentMatchers.anyList;
import static org.mockito.ArgumentMatchers.eq;
import static org.mockito.Mockito.doReturn;
import static org.mockito.Mockito.times;
import static org.mockito.Mockito.verify;

import java.util.List;
import java.util.UUID;

import io.vertx.junit5.VertxExtension;
import io.vertx.junit5.VertxTestContext;
import org.folio.models.EncumbranceRelationsHolder;
import org.folio.models.EncumbrancesProcessingHolder;
import org.folio.rest.acq.model.finance.AwaitingPayment;
import org.folio.rest.acq.model.finance.Encumbrance;
import org.folio.rest.acq.model.finance.Metadata;
import org.folio.rest.acq.model.finance.Transaction;
import org.folio.rest.core.exceptions.HttpException;
import org.folio.rest.core.models.RequestContext;
import org.folio.rest.jaxrs.model.CompositePoLine;
import org.folio.rest.jaxrs.model.CompositePurchaseOrder;
import org.folio.rest.jaxrs.model.FundDistribution;
import org.folio.rest.jaxrs.model.Parameter;
import org.folio.service.exchange.CacheableExchangeRateService;
import org.folio.service.finance.FiscalYearService;
import org.folio.service.finance.FundService;
import org.folio.service.finance.LedgerService;
import org.folio.service.finance.budget.BudgetService;
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
public class PendingToPendingEncumbranceStrategyTest {

  public static final String PENDING_ORDER_2_FD_ID = "22f0d3dd-267d-405c-9917-29395b8507d8";
  public static final String PENDING_ORDER_1_FD_ID = "b3ada928-f5e0-4fc8-9233-e1c16d9c5287";
  public static final String ORDER_2_FD_PATH = COMP_ORDER_MOCK_DATA_PATH + PENDING_ORDER_2_FD_ID + ".json";
  public static final String ORDER_1_FD_PATH = COMP_ORDER_MOCK_DATA_PATH + PENDING_ORDER_1_FD_ID + ".json";

  private PendingToPendingEncumbranceStrategy pendingToPendingEncumbranceStrategy;

  @Mock
  private EncumbranceService encumbranceService;
  @Mock
  private FundService fundService;
  @Mock
  private FiscalYearService fiscalYearService;
  @Mock
  private BudgetService budgetService;
  @Mock
  private LedgerService ledgerService;
  @Mock
  private TransactionService transactionService;
  @Mock
  private CacheableExchangeRateService cacheableExchangeRateService;
  @Mock
  private RequestContext requestContext;
  @Captor
  ArgumentCaptor<EncumbrancesProcessingHolder> holderCaptor;

  @BeforeEach
  void init() {
    EncumbranceRelationsHoldersBuilder encumbranceRelationsHoldersBuilder = new EncumbranceRelationsHoldersBuilder(
      encumbranceService, fundService, fiscalYearService, budgetService, ledgerService, cacheableExchangeRateService);
    PendingPaymentService pendingPaymentService = new PendingPaymentService(transactionService);
    pendingToPendingEncumbranceStrategy = new PendingToPendingEncumbranceStrategy(encumbranceService,
      encumbranceRelationsHoldersBuilder, pendingPaymentService);
  }

  @Test
  void testChangingExpenseClass(VertxTestContext vertxTestContext) {
    // Given
    String newExpenseClassId = "d9328814-eba1-4083-98ca-026eb269a4a8";
    CompositePurchaseOrder order = getMockAsJson(ORDER_2_FD_PATH).mapTo(CompositePurchaseOrder.class);
    CompositePurchaseOrder orderFromStorage = JsonObject.mapFrom(order).mapTo(CompositePurchaseOrder.class);
    CompositePoLine poLine = order.getCompositePoLines().getFirst();
    FundDistribution fd1 = poLine.getFundDistribution().getFirst();
    FundDistribution fd2 = poLine.getFundDistribution().get(1);

    String oldExpenseClassId = fd1.getExpenseClassId();
    fd1.setExpenseClassId(newExpenseClassId);

    Transaction encumbrance1 = new Transaction()
      .withTransactionType(Transaction.TransactionType.ENCUMBRANCE)
      .withAmount(0d)
      .withId(fd1.getEncumbrance())
      .withFromFundId(fd1.getFundId())
      .withExpenseClassId(oldExpenseClassId)
      .withSource(Transaction.Source.PO_LINE)
      .withEncumbrance(new Encumbrance()
        .withSourcePurchaseOrderId(order.getId())
        .withSourcePoLineId(poLine.getId())
        .withOrderType(Encumbrance.OrderType.fromValue(order.getOrderType().value()))
        .withInitialAmountEncumbered(2d)
        .withOrderStatus(Encumbrance.OrderStatus.PENDING)
        .withStatus(Encumbrance.Status.PENDING))
      .withMetadata(new Metadata());
    Transaction encumbrance2 = new Transaction()
      .withTransactionType(Transaction.TransactionType.ENCUMBRANCE)
      .withAmount(0d)
      .withId(fd2.getEncumbrance())
      .withFromFundId(fd2.getFundId())
      .withExpenseClassId(fd2.getExpenseClassId())
      .withSource(Transaction.Source.PO_LINE)
      .withEncumbrance(new Encumbrance()
        .withSourcePurchaseOrderId(order.getId())
        .withSourcePoLineId(poLine.getId())
        .withOrderType(Encumbrance.OrderType.fromValue(order.getOrderType().value()))
        .withInitialAmountEncumbered(2d)
        .withOrderStatus(Encumbrance.OrderStatus.PENDING)
        .withStatus(Encumbrance.Status.PENDING))
      .withMetadata(new Metadata());
    List<Transaction> encumbrances = List.of(encumbrance1, encumbrance2);

    doReturn(succeededFuture(encumbrances))
      .when(encumbranceService).getEncumbrancesByIds(anyList(), eq(requestContext));
    doReturn(succeededFuture())
      .when(encumbranceService).createOrUpdateEncumbrances(any(EncumbrancesProcessingHolder.class), eq(requestContext));

    // When
    Future<Void> future = pendingToPendingEncumbranceStrategy.processEncumbrances(order, orderFromStorage, requestContext);

    // Then
    vertxTestContext.assertComplete(future)
      .onSuccess(result -> vertxTestContext.verify(() -> {
        verify(encumbranceService, times(1))
          .createOrUpdateEncumbrances(holderCaptor.capture(), eq(requestContext));
        List<EncumbrancesProcessingHolder> holders = holderCaptor.getAllValues();
        assertEquals(1, holders.size());
        EncumbrancesProcessingHolder holder = holders.getFirst();
        assertEquals(0, holder.getEncumbrancesForCreate().size());
        assertEquals(1, holder.getEncumbrancesForUpdate().size());
        assertEquals(0, holder.getEncumbrancesForDelete().size());
        assertEquals(0, holder.getEncumbrancesForRelease().size());
        assertEquals(0, holder.getEncumbrancesForUnrelease().size());
        EncumbranceRelationsHolder updateHolder = holder.getEncumbrancesForUpdate().getFirst();
        Transaction updatedTransaction = updateHolder.getNewEncumbrance();
        assertEquals(newExpenseClassId, updatedTransaction.getExpenseClassId());
        assertEquals(encumbrance1, updatedTransaction.withExpenseClassId(oldExpenseClassId));
        vertxTestContext.completeNow();
      }))
      .onFailure(vertxTestContext::failNow);
  }

  @Test
  void testDeletingAnEncumbranceWithAPendingPayment(VertxTestContext vertxTestContext) {
    // Given
    CompositePurchaseOrder order = getMockAsJson(ORDER_1_FD_PATH).mapTo(CompositePurchaseOrder.class);
    CompositePurchaseOrder orderFromStorage = JsonObject.mapFrom(order).mapTo(CompositePurchaseOrder.class);
    CompositePoLine poLine = order.getCompositePoLines().getFirst();
    FundDistribution fd = poLine.getFundDistribution().getFirst();
    String encumbranceId = fd.getEncumbrance();
    String fundId = fd.getFundId();
    String invoiceId = UUID.randomUUID().toString();
    String invoiceLineId = UUID.randomUUID().toString();
    String fiscalYearId = UUID.randomUUID().toString();
    String pendingPaymentId = UUID.randomUUID().toString();

    poLine.getFundDistribution().removeFirst();

    Transaction encumbrance = new Transaction()
      .withTransactionType(Transaction.TransactionType.ENCUMBRANCE)
      .withAmount(0d)
      .withId(encumbranceId)
      .withFromFundId(fundId)
      .withExpenseClassId(fd.getExpenseClassId())
      .withSource(Transaction.Source.PO_LINE)
      .withEncumbrance(new Encumbrance()
        .withSourcePurchaseOrderId(order.getId())
        .withSourcePoLineId(poLine.getId())
        .withOrderType(Encumbrance.OrderType.fromValue(order.getOrderType().value()))
        .withInitialAmountEncumbered(4d)
        .withOrderStatus(Encumbrance.OrderStatus.PENDING)
        .withStatus(Encumbrance.Status.PENDING))
      .withMetadata(new Metadata());
    List<Transaction> encumbrances = List.of(encumbrance);

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

    doReturn(succeededFuture(encumbrances))
      .when(encumbranceService).getEncumbrancesByIds(anyList(), eq(requestContext));
    String expectedQuery = "awaitingPayment.encumbranceId==(" + encumbranceId + ")";
    doReturn(succeededFuture(pendingPayments))
      .when(transactionService).getTransactions(eq(expectedQuery), eq(requestContext));
    doReturn(succeededFuture())
      .when(encumbranceService).createOrUpdateEncumbrances(any(EncumbrancesProcessingHolder.class), eq(requestContext));

    // When
    Future<Void> future = pendingToPendingEncumbranceStrategy.processEncumbrances(order, orderFromStorage, requestContext);

    // Then
    vertxTestContext.assertComplete(future)
      .onSuccess(result -> vertxTestContext.verify(() -> {
        verify(encumbranceService, times(1))
          .createOrUpdateEncumbrances(holderCaptor.capture(), eq(requestContext));
        List<EncumbrancesProcessingHolder> holders = holderCaptor.getAllValues();
        assertEquals(1, holders.size());
        EncumbrancesProcessingHolder holder = holders.getFirst();
        assertEquals(0, holder.getEncumbrancesForCreate().size());
        assertEquals(0, holder.getEncumbrancesForUpdate().size());
        assertEquals(1, holder.getEncumbrancesForDelete().size());
        assertEquals(0, holder.getEncumbrancesForRelease().size());
        assertEquals(0, holder.getEncumbrancesForUnrelease().size());
        assertEquals(1, holder.getPendingPaymentsToUpdate().size());
        String deletedId = holder.getEncumbrancesForDelete().getFirst().getOldEncumbrance().getId();
        assertEquals(encumbranceId, deletedId);
        Transaction updatedPendingPayment = holder.getPendingPaymentsToUpdate().getFirst();
        assertEquals(pendingPaymentId, updatedPendingPayment.getId());
        assertNull(updatedPendingPayment.getAwaitingPayment().getEncumbranceId());
        vertxTestContext.completeNow();
      }))
      .onFailure(vertxTestContext::failNow);
  }

  @Test
  void testDeletingAnEncumbranceWithExpendedAmountGreaterThanZero(VertxTestContext vertxTestContext) {
    // Given
    CompositePurchaseOrder order = getMockAsJson(ORDER_1_FD_PATH).mapTo(CompositePurchaseOrder.class);
    CompositePurchaseOrder orderFromStorage = JsonObject.mapFrom(order).mapTo(CompositePurchaseOrder.class);
    CompositePoLine poLine = order.getCompositePoLines().getFirst();
    FundDistribution fd = poLine.getFundDistribution().getFirst();
    String encumbranceId = fd.getEncumbrance();
    String fundId = fd.getFundId();

    poLine.getFundDistribution().removeFirst();

    Transaction encumbrance = new Transaction()
      .withTransactionType(Transaction.TransactionType.ENCUMBRANCE)
      .withAmount(0d)
      .withId(encumbranceId)
      .withFromFundId(fundId)
      .withExpenseClassId(fd.getExpenseClassId())
      .withSource(Transaction.Source.PO_LINE)
      .withEncumbrance(new Encumbrance()
        .withSourcePurchaseOrderId(order.getId())
        .withSourcePoLineId(poLine.getId())
        .withOrderType(Encumbrance.OrderType.fromValue(order.getOrderType().value()))
        .withInitialAmountEncumbered(4d)
        .withAmountExpended(4d)
        .withOrderStatus(Encumbrance.OrderStatus.PENDING)
        .withStatus(Encumbrance.Status.RELEASED))
      .withMetadata(new Metadata());
    List<Transaction> encumbrances = List.of(encumbrance);

    doReturn(succeededFuture(encumbrances))
      .when(encumbranceService).getEncumbrancesByIds(anyList(), eq(requestContext));

    // When
    Future<Void> future = pendingToPendingEncumbranceStrategy.processEncumbrances(order, orderFromStorage, requestContext);

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
