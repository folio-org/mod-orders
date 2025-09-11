package org.folio.service.orders;

import static org.junit.jupiter.api.Assertions.assertEquals;
import static org.junit.jupiter.api.Assertions.assertInstanceOf;
import static org.junit.jupiter.api.Assertions.assertTrue;
import static org.mockito.ArgumentMatchers.any;
import static org.mockito.ArgumentMatchers.anyCollection;
import static org.mockito.ArgumentMatchers.anyString;
import static org.mockito.Mockito.when;

import java.util.Collections;
import java.util.List;

import io.vertx.junit5.VertxExtension;
import org.folio.CopilotGenerated;
import org.folio.rest.acq.model.finance.FiscalYear;
import org.folio.rest.acq.model.finance.Transaction;
import org.folio.rest.jaxrs.model.FiscalYearsHolder;
import org.folio.rest.core.models.RequestContext;
import org.folio.rest.jaxrs.model.CompositePurchaseOrder;
import org.folio.rest.jaxrs.model.FundDistribution;
import org.folio.rest.jaxrs.model.PoLine;
import org.folio.service.finance.FiscalYearService;
import org.folio.service.finance.transaction.TransactionService;
import org.junit.jupiter.api.AfterEach;
import org.junit.jupiter.api.BeforeEach;
import org.junit.jupiter.api.Test;
import org.junit.jupiter.api.extension.ExtendWith;
import org.mockito.InjectMocks;
import org.mockito.Mock;

import io.vertx.core.Future;
import org.mockito.MockitoAnnotations;

@ExtendWith(VertxExtension.class)
@CopilotGenerated(model = "Claude Sonnet 4")
public class OrderFiscalYearServiceTest {

  @Mock
  private TransactionService transactionService;
  @Mock
  private FiscalYearService fiscalYearService;
  @Mock
  private PurchaseOrderStorageService purchaseOrderStorageService;
  @Mock
  private RequestContext requestContext;
  private AutoCloseable openMocks;

  @InjectMocks
  private OrderFiscalYearService orderFiscalYearService;

  private static final String ORDER_ID = "test-order-id";
  private static final String FISCAL_YEAR_ID_1 = "fiscal-year-1";
  private static final String FISCAL_YEAR_ID_2 = "fiscal-year-2";
  private static final String FISCAL_YEAR_ID_3 = "fiscal-year-3";
  private static final String CURRENT_FISCAL_YEAR_ID = "current-fiscal-year";
  private static final String FUND_ID_1 = "fund-1";
  private static final String FUND_ID_2 = "fund-2";

  @BeforeEach
  void setUp() {
    openMocks = MockitoAnnotations.openMocks(this);
    orderFiscalYearService = new OrderFiscalYearService(transactionService, fiscalYearService, purchaseOrderStorageService);
  }

  @AfterEach
  public void closeMocks() throws Exception {
    if (openMocks != null) {
      openMocks.close();
    }
  }

  @Test
  void testGetAvailableFiscalYears_OrderWithFundMatches_ReturnsCurrentAndPreviousFiscalYears() {
    // Given - Order has fund distributions, transactions match some funds
    CompositePurchaseOrder order = createOrderWithFunds(List.of(FUND_ID_1, FUND_ID_2));

    Transaction transaction1 = new Transaction().withFiscalYearId(FISCAL_YEAR_ID_1).withFromFundId(FUND_ID_1);
    Transaction transaction2 = new Transaction().withFiscalYearId(FISCAL_YEAR_ID_2).withFromFundId(FUND_ID_2);
    Transaction transaction3 = new Transaction().withFiscalYearId(FISCAL_YEAR_ID_3).withFromFundId("other-fund");
    List<Transaction> transactions = List.of(transaction1, transaction2, transaction3);

    FiscalYear fy1 = new FiscalYear().withId(FISCAL_YEAR_ID_1).withName("FY2024");
    FiscalYear fy2 = new FiscalYear().withId(FISCAL_YEAR_ID_2).withName("FY2023");
    FiscalYear fy3 = new FiscalYear().withId(FISCAL_YEAR_ID_3).withName("FY2022");
    List<FiscalYear> fiscalYears = List.of(fy1, fy2, fy3);

    mockBasicServices(order, transactions);
    mockFiscalYearServices(fiscalYears, null, null);

    // When
    Future<FiscalYearsHolder> result = orderFiscalYearService.getAvailableFiscalYears(ORDER_ID, requestContext);

    // Then
    assertTrue(result.succeeded());
    FiscalYearsHolder holder = result.result();

    // Current should contain fiscal years from transactions matching order funds (sorted desc by name)
    assertEquals(2, holder.getCurrent().size());
    assertEquals("FY2024", holder.getCurrent().get(0).getName()); // FY2024 > FY2023
    assertEquals("FY2023", holder.getCurrent().get(1).getName());

    // Previous should contain other fiscal years (sorted desc by name)
    assertEquals(1, holder.getPrevious().size());
    assertEquals("FY2022", holder.getPrevious().getFirst().getName());
  }

  @Test
  void testGetAvailableFiscalYears_OrderWithoutFundMatches_UsesCurrentFiscalYear() {
    // Given - Order has funds but no transactions match them, so use current FY fallback
    CompositePurchaseOrder order = createOrderWithFunds(List.of(FUND_ID_1));

    Transaction transaction1 = new Transaction().withFiscalYearId(FISCAL_YEAR_ID_1).withFromFundId("other-fund");
    Transaction transaction2 = new Transaction().withFiscalYearId(FISCAL_YEAR_ID_2).withFromFundId("other-fund-2");
    List<Transaction> transactions = List.of(transaction1, transaction2);

    FiscalYear fy1 = new FiscalYear().withId(FISCAL_YEAR_ID_1).withName("FY2023");
    FiscalYear fy2 = new FiscalYear().withId(FISCAL_YEAR_ID_2).withName("FY2022");
    FiscalYear currentFy = new FiscalYear().withId(CURRENT_FISCAL_YEAR_ID).withName("FY2024");
    List<FiscalYear> fiscalYears = List.of(fy1, fy2);

    mockBasicServices(order, transactions);
    mockFiscalYearServices(fiscalYears, CURRENT_FISCAL_YEAR_ID, currentFy);

    // When
    Future<FiscalYearsHolder> result = orderFiscalYearService.getAvailableFiscalYears(ORDER_ID, requestContext);

    // Then
    assertTrue(result.succeeded());
    FiscalYearsHolder holder = result.result();

    // Current should contain the current fiscal year that was added
    assertEquals(1, holder.getCurrent().size());
    assertEquals("FY2024", holder.getCurrent().get(0).getName());

    // Previous should contain all fiscal years from transactions
    assertEquals(2, holder.getPrevious().size());
    assertEquals("FY2023", holder.getPrevious().get(0).getName()); // FY2023 > FY2022
    assertEquals("FY2022", holder.getPrevious().get(1).getName());
  }

  @Test
  void testGetAvailableFiscalYears_EmptyOrder_ReturnsEmptyHolder() {
    // Given - Order has no fund distributions
    CompositePurchaseOrder order = createEmptyOrder();

    Transaction transaction1 = new Transaction().withFiscalYearId(FISCAL_YEAR_ID_1).withFromFundId("some-fund");
    List<Transaction> transactions = List.of(transaction1);

    FiscalYear fy1 = new FiscalYear().withId(FISCAL_YEAR_ID_1).withName("FY2023");
    FiscalYear currentFy = new FiscalYear().withId(CURRENT_FISCAL_YEAR_ID).withName("FY2024");
    List<FiscalYear> fiscalYears = List.of(fy1);

    mockBasicServices(order, transactions);
    mockFiscalYearServices(fiscalYears, CURRENT_FISCAL_YEAR_ID, currentFy);

    // When
    Future<FiscalYearsHolder> result = orderFiscalYearService.getAvailableFiscalYears(ORDER_ID, requestContext);

    // Then
    assertTrue(result.succeeded());
    FiscalYearsHolder holder = result.result();

    // Current should contain the added current fiscal year
    assertEquals(1, holder.getCurrent().size());
    assertEquals("FY2024", holder.getCurrent().get(0).getName());

    // Previous should contain fiscal years from transactions
    assertEquals(1, holder.getPrevious().size());
    assertEquals("FY2023", holder.getPrevious().get(0).getName());
  }

  @Test
  void testGetAvailableFiscalYears_EmptyTransactionList_ReturnsEmptyHolder() {
    // Given - No transactions exist
    CompositePurchaseOrder order = createOrderWithFunds(List.of(FUND_ID_1));

    mockBasicServices(order, Collections.emptyList());

    // When
    Future<FiscalYearsHolder> result = orderFiscalYearService.getAvailableFiscalYears(ORDER_ID, requestContext);

    // Then
    assertTrue(result.succeeded());
    FiscalYearsHolder holder = result.result();
    assertTrue(holder.getCurrent().isEmpty());
    assertTrue(holder.getPrevious().isEmpty());
  }

  @Test
  void testGetAvailableFiscalYears_CurrentFiscalYearFallback_WithCurrentFYInTransactions() {
    // Given - No fund matches, but current FY exists in transactions
    CompositePurchaseOrder order = createOrderWithFunds(List.of(FUND_ID_1));

    Transaction transaction1 = new Transaction().withFiscalYearId(FISCAL_YEAR_ID_1).withFromFundId("other-fund");
    Transaction transaction2 = new Transaction().withFiscalYearId(CURRENT_FISCAL_YEAR_ID).withFromFundId("another-fund");
    List<Transaction> transactions = List.of(transaction1, transaction2);

    FiscalYear fy1 = new FiscalYear().withId(FISCAL_YEAR_ID_1).withName("FY2022");
    FiscalYear currentFy = new FiscalYear().withId(CURRENT_FISCAL_YEAR_ID).withName("FY2024");
    List<FiscalYear> fiscalYears = List.of(fy1, currentFy);

    mockBasicServices(order, transactions);
    mockFiscalYearServices(fiscalYears, CURRENT_FISCAL_YEAR_ID, null);

    // When
    Future<FiscalYearsHolder> result = orderFiscalYearService.getAvailableFiscalYears(ORDER_ID, requestContext);

    // Then
    assertTrue(result.succeeded());
    FiscalYearsHolder holder = result.result();

    // Current should be empty because the current FY exists in transactions but no fund matches
    assertTrue(holder.getCurrent().isEmpty());

    // Previous should contain all fiscal years from transactions
    assertEquals(2, holder.getPrevious().size());
    assertEquals("FY2024", holder.getPrevious().get(0).getName()); // FY2024 > FY2022
    assertEquals("FY2022", holder.getPrevious().get(1).getName());
  }

  @Test
  void testGetAvailableFiscalYears_SortingVerification_DescendingByName() {
    // Given - Multiple fiscal years to test sorting
    CompositePurchaseOrder order = createOrderWithFunds(List.of(FUND_ID_1));

    Transaction t1 = new Transaction().withFiscalYearId("fy-2021").withFromFundId(FUND_ID_1);
    Transaction t2 = new Transaction().withFiscalYearId("fy-2023").withFromFundId(FUND_ID_1);
    Transaction t3 = new Transaction().withFiscalYearId("fy-2022").withFromFundId("other-fund");
    Transaction t4 = new Transaction().withFiscalYearId("fy-2024").withFromFundId("other-fund");
    List<Transaction> transactions = List.of(t1, t2, t3, t4);

    FiscalYear fy2021 = new FiscalYear().withId("fy-2021").withName("FY2021");
    FiscalYear fy2022 = new FiscalYear().withId("fy-2022").withName("FY2022");
    FiscalYear fy2023 = new FiscalYear().withId("fy-2023").withName("FY2023");
    FiscalYear fy2024 = new FiscalYear().withId("fy-2024").withName("FY2024");
    List<FiscalYear> fiscalYears = List.of(fy2021, fy2022, fy2023, fy2024);

    mockBasicServices(order, transactions);
    mockFiscalYearServices(fiscalYears, null, null);

    // When
    Future<FiscalYearsHolder> result = orderFiscalYearService.getAvailableFiscalYears(ORDER_ID, requestContext);

    // Then
    assertTrue(result.succeeded());
    FiscalYearsHolder holder = result.result();

    // Verify current array is sorted descending by name
    assertEquals(2, holder.getCurrent().size());
    assertEquals("FY2023", holder.getCurrent().get(0).getName()); // FY2023 > FY2021
    assertEquals("FY2021", holder.getCurrent().get(1).getName());

    // Verify previous array is sorted descending by name
    assertEquals(2, holder.getPrevious().size());
    assertEquals("FY2024", holder.getPrevious().get(0).getName()); // FY2024 > FY2022
    assertEquals("FY2022", holder.getPrevious().get(1).getName());
  }

  @Test
  void testGetAvailableFiscalYears_ServiceFailures_PropagateErrors() {
    // Given - Transaction service failure
    CompositePurchaseOrder order = createOrderWithFunds(List.of(FUND_ID_1));

    when(purchaseOrderStorageService.getCompositeOrderById(ORDER_ID, requestContext))
      .thenReturn(Future.succeededFuture(order));
    when(transactionService.getTransactions(anyString(), any(RequestContext.class)))
      .thenReturn(Future.failedFuture(new RuntimeException("Transaction service error")));

    // When
    Future<FiscalYearsHolder> result = orderFiscalYearService.getAvailableFiscalYears(ORDER_ID, requestContext);

    // Then
    assertTrue(result.failed());
    assertInstanceOf(RuntimeException.class, result.cause());
    assertEquals("Transaction service error", result.cause().getMessage());
  }

  private CompositePurchaseOrder createOrderWithFunds(List<String> fundIds) {
    List<PoLine> poLines = fundIds.stream()
      .map(fundId -> {
        FundDistribution fundDistribution = new FundDistribution().withFundId(fundId);
        return new PoLine().withFundDistribution(List.of(fundDistribution));
      })
      .toList();

    return new CompositePurchaseOrder().withPoLines(poLines);
  }

  private CompositePurchaseOrder createEmptyOrder() {
    return new CompositePurchaseOrder().withPoLines(List.of());
  }

  private void mockBasicServices(CompositePurchaseOrder order, List<Transaction> transactions) {
    when(purchaseOrderStorageService.getCompositeOrderById(ORDER_ID, requestContext))
      .thenReturn(Future.succeededFuture(order));
    when(transactionService.getTransactions(anyString(), any(RequestContext.class)))
      .thenReturn(Future.succeededFuture(transactions));
  }

  private void mockFiscalYearServices(List<FiscalYear> fiscalYears, String currentFiscalYearId, FiscalYear currentFiscalYear) {
    when(fiscalYearService.getAllFiscalYears(anyCollection(), any(RequestContext.class)))
      .thenReturn(Future.succeededFuture(fiscalYears));
    if (currentFiscalYearId != null) {
      when(fiscalYearService.getCurrentFYForSeriesByFYId(anyString(), any(RequestContext.class)))
        .thenReturn(Future.succeededFuture(currentFiscalYearId));
    }
    if (currentFiscalYear != null) {
      when(fiscalYearService.getFiscalYearById(currentFiscalYearId, requestContext))
        .thenReturn(Future.succeededFuture(currentFiscalYear));
    }
  }
}
