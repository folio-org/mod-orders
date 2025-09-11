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
import org.folio.rest.jaxrs.model.CompositePurchaseOrder;
import org.folio.rest.core.models.RequestContext;
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
  void testGetAvailableFiscalYears_FundsHaveCurrentFiscalYears_ReturnsSeparatedFiscalYears() {
    // Given - Transactions with fund IDs that have current fiscal years
    Transaction transaction1 = new Transaction().withFiscalYearId(FISCAL_YEAR_ID_1).withFromFundId(FUND_ID_1);
    Transaction transaction2 = new Transaction().withFiscalYearId(FISCAL_YEAR_ID_2).withFromFundId(FUND_ID_2);
    Transaction transaction3 = new Transaction().withFiscalYearId(FISCAL_YEAR_ID_3).withFromFundId("other-fund");
    List<Transaction> transactions = List.of(transaction1, transaction2, transaction3);

    FiscalYear fy1 = new FiscalYear().withId(FISCAL_YEAR_ID_1).withName("FY2024");
    FiscalYear fy2 = new FiscalYear().withId(FISCAL_YEAR_ID_2).withName("FY2023");
    FiscalYear fy3 = new FiscalYear().withId(FISCAL_YEAR_ID_3).withName("FY2022");
    List<FiscalYear> fiscalYears = List.of(fy1, fy2, fy3);

    mockServices(transactions, fiscalYears);
    when(fiscalYearService.getCurrentFiscalYearByFundId(FUND_ID_1, requestContext))
      .thenReturn(Future.succeededFuture(fy1)); // FY2024 is current for fund1
    when(fiscalYearService.getCurrentFiscalYearByFundId(FUND_ID_2, requestContext))
      .thenReturn(Future.succeededFuture(fy2)); // FY2023 is current for fund2
    when(fiscalYearService.getCurrentFiscalYearByFundId("other-fund", requestContext))
      .thenReturn(Future.succeededFuture(null)); // No current fiscal year for this fund

    // When
    Future<FiscalYearsHolder> result = orderFiscalYearService.getAvailableFiscalYears(ORDER_ID, requestContext);

    // Then
    assertTrue(result.succeeded());
    FiscalYearsHolder holder = result.result();

    // Current should contain fiscal years that are current for the funds (sorted desc by name)
    assertEquals(2, holder.getCurrent().size());
    assertEquals("FY2024", holder.getCurrent().get(0).getName()); // FY2024 > FY2023
    assertEquals("FY2023", holder.getCurrent().get(1).getName());

    // Previous should contain other fiscal years (sorted desc by name)
    assertEquals(1, holder.getPrevious().size());
    assertEquals("FY2022", holder.getPrevious().get(0).getName());
  }

  @Test
  void testGetAvailableFiscalYears_NoCurrentFiscalYearsFromFunds_AddsCurrentFromSeries() {
    // Given - Funds don't have current fiscal years, so add current FY from series
    Transaction transaction1 = new Transaction().withFiscalYearId(FISCAL_YEAR_ID_1).withFromFundId(FUND_ID_1);
    Transaction transaction2 = new Transaction().withFiscalYearId(FISCAL_YEAR_ID_2).withFromFundId(FUND_ID_2);
    List<Transaction> transactions = List.of(transaction1, transaction2);

    FiscalYear fy1 = new FiscalYear().withId(FISCAL_YEAR_ID_1).withName("FY2023");
    FiscalYear fy2 = new FiscalYear().withId(FISCAL_YEAR_ID_2).withName("FY2022");
    FiscalYear currentFy = new FiscalYear().withId(CURRENT_FISCAL_YEAR_ID).withName("FY2024");
    List<FiscalYear> fiscalYears = List.of(fy1, fy2);

    mockServices(transactions, fiscalYears);
    when(fiscalYearService.getCurrentFiscalYearByFundId(FUND_ID_1, requestContext))
      .thenReturn(Future.succeededFuture(null));
    when(fiscalYearService.getCurrentFiscalYearByFundId(FUND_ID_2, requestContext))
      .thenReturn(Future.succeededFuture(null));
    when(fiscalYearService.getCurrentFYForSeriesByFYId(FISCAL_YEAR_ID_1, requestContext))
      .thenReturn(Future.succeededFuture(CURRENT_FISCAL_YEAR_ID));
    when(fiscalYearService.getFiscalYearById(CURRENT_FISCAL_YEAR_ID, requestContext))
      .thenReturn(Future.succeededFuture(currentFy));

    // When
    Future<FiscalYearsHolder> result = orderFiscalYearService.getAvailableFiscalYears(ORDER_ID, requestContext);

    // Then
    assertTrue(result.succeeded());
    FiscalYearsHolder holder = result.result();

    // Current should contain the added current fiscal year
    assertEquals(1, holder.getCurrent().size());
    assertEquals("FY2024", holder.getCurrent().get(0).getName());

    // Previous should contain all original fiscal years from transactions
    assertEquals(2, holder.getPrevious().size());
    assertEquals("FY2023", holder.getPrevious().get(0).getName()); // FY2023 > FY2022
    assertEquals("FY2022", holder.getPrevious().get(1).getName());
  }

  @Test
  void testGetAvailableFiscalYears_CurrentFiscalYearFromSeriesAlreadyExists_MarksAsCurrentOnly() {
    // Given - Current fiscal year from series already exists in transaction fiscal years
    Transaction transaction1 = new Transaction().withFiscalYearId(FISCAL_YEAR_ID_1).withFromFundId(FUND_ID_1);
    Transaction transaction2 = new Transaction().withFiscalYearId(CURRENT_FISCAL_YEAR_ID).withFromFundId(FUND_ID_2);
    List<Transaction> transactions = List.of(transaction1, transaction2);

    FiscalYear fy1 = new FiscalYear().withId(FISCAL_YEAR_ID_1).withName("FY2022");
    FiscalYear currentFy = new FiscalYear().withId(CURRENT_FISCAL_YEAR_ID).withName("FY2024");
    List<FiscalYear> fiscalYears = List.of(fy1, currentFy);

    mockServices(transactions, fiscalYears);
    when(fiscalYearService.getCurrentFiscalYearByFundId(FUND_ID_1, requestContext))
      .thenReturn(Future.succeededFuture(null));
    when(fiscalYearService.getCurrentFiscalYearByFundId(FUND_ID_2, requestContext))
      .thenReturn(Future.succeededFuture(null));
    when(fiscalYearService.getCurrentFYForSeriesByFYId(FISCAL_YEAR_ID_1, requestContext))
      .thenReturn(Future.succeededFuture(CURRENT_FISCAL_YEAR_ID));

    // When
    Future<FiscalYearsHolder> result = orderFiscalYearService.getAvailableFiscalYears(ORDER_ID, requestContext);

    // Then
    assertTrue(result.succeeded());
    FiscalYearsHolder holder = result.result();

    // Current should contain the existing fiscal year that was marked as current
    assertEquals(1, holder.getCurrent().size());
    assertEquals("FY2024", holder.getCurrent().get(0).getName());

    // Previous should contain the remaining fiscal year
    assertEquals(1, holder.getPrevious().size());
    assertEquals("FY2022", holder.getPrevious().get(0).getName());
  }

  @Test
  void testGetAvailableFiscalYears_NoFundIds_AllArePrevious() {
    // Given - Transactions have no fund IDs
    Transaction transaction1 = new Transaction().withFiscalYearId(FISCAL_YEAR_ID_1);
    Transaction transaction2 = new Transaction().withFiscalYearId(FISCAL_YEAR_ID_2);
    List<Transaction> transactions = List.of(transaction1, transaction2);

    FiscalYear fy1 = new FiscalYear().withId(FISCAL_YEAR_ID_1).withName("FY2023");
    FiscalYear fy2 = new FiscalYear().withId(FISCAL_YEAR_ID_2).withName("FY2022");
    List<FiscalYear> fiscalYears = List.of(fy1, fy2);

    mockServices(transactions, fiscalYears);

    // When
    Future<FiscalYearsHolder> result = orderFiscalYearService.getAvailableFiscalYears(ORDER_ID, requestContext);

    // Then
    assertTrue(result.succeeded());
    FiscalYearsHolder holder = result.result();

    // Current should be empty since no funds to check
    assertTrue(holder.getCurrent().isEmpty());

    // Previous should contain all fiscal years
    assertEquals(2, holder.getPrevious().size());
    assertEquals("FY2023", holder.getPrevious().get(0).getName()); // FY2023 > FY2022
    assertEquals("FY2022", holder.getPrevious().get(1).getName());
  }

  @Test
  void testGetAvailableFiscalYears_EmptyTransactionList_ReturnsEmptyHolder() {
    // Given - No transactions exist
    mockServices(Collections.emptyList(), null);

    // When
    Future<FiscalYearsHolder> result = orderFiscalYearService.getAvailableFiscalYears(ORDER_ID, requestContext);

    // Then
    assertTrue(result.succeeded());
    FiscalYearsHolder holder = result.result();
    assertTrue(holder.getCurrent().isEmpty());
    assertTrue(holder.getPrevious().isEmpty());
  }

  @Test
  void testGetAvailableFiscalYears_SortingVerification_DescendingByName() {
    // Given - Multiple fiscal years to test sorting
    Transaction t1 = new Transaction().withFiscalYearId("fy-2021").withFromFundId(FUND_ID_1);
    Transaction t2 = new Transaction().withFiscalYearId("fy-2023").withFromFundId(FUND_ID_2);
    Transaction t3 = new Transaction().withFiscalYearId("fy-2022").withFromFundId("other-fund");
    Transaction t4 = new Transaction().withFiscalYearId("fy-2024").withFromFundId("another-fund");
    List<Transaction> transactions = List.of(t1, t2, t3, t4);

    FiscalYear fy2021 = new FiscalYear().withId("fy-2021").withName("FY2021");
    FiscalYear fy2022 = new FiscalYear().withId("fy-2022").withName("FY2022");
    FiscalYear fy2023 = new FiscalYear().withId("fy-2023").withName("FY2023");
    FiscalYear fy2024 = new FiscalYear().withId("fy-2024").withName("FY2024");
    List<FiscalYear> fiscalYears = List.of(fy2021, fy2022, fy2023, fy2024);

    mockServices(transactions, fiscalYears);
    when(fiscalYearService.getCurrentFiscalYearByFundId(FUND_ID_1, requestContext))
      .thenReturn(Future.succeededFuture(fy2023)); // FY2023 is current for fund1
    when(fiscalYearService.getCurrentFiscalYearByFundId(FUND_ID_2, requestContext))
      .thenReturn(Future.succeededFuture(fy2024)); // FY2024 is current for fund2
    when(fiscalYearService.getCurrentFiscalYearByFundId("other-fund", requestContext))
      .thenReturn(Future.succeededFuture(null)); // No current fiscal year for this fund
    when(fiscalYearService.getCurrentFiscalYearByFundId("another-fund", requestContext))
      .thenReturn(Future.succeededFuture(null)); // No current fiscal year for this fund

    // When
    Future<FiscalYearsHolder> result = orderFiscalYearService.getAvailableFiscalYears(ORDER_ID, requestContext);

    // Then
    assertTrue(result.succeeded());
    FiscalYearsHolder holder = result.result();

    // Verify current contains the current fiscal years (sorted desc by name)
    assertEquals(2, holder.getCurrent().size());
    assertEquals("FY2024", holder.getCurrent().get(0).getName()); // FY2024 > FY2023
    assertEquals("FY2023", holder.getCurrent().get(1).getName());

    // Verify previous array is sorted descending by name
    assertEquals(2, holder.getPrevious().size());
    assertEquals("FY2022", holder.getPrevious().get(0).getName()); // FY2022 > FY2021
    assertEquals("FY2021", holder.getPrevious().get(1).getName());
  }

  @Test
  void testGetAvailableFiscalYears_ServiceFailures_PropagateErrors() {
    // Given - Transaction service failure
    when(purchaseOrderStorageService.getCompositeOrderById(ORDER_ID, requestContext))
      .thenReturn(Future.succeededFuture(new CompositePurchaseOrder()));
    when(transactionService.getTransactions(anyString(), any(RequestContext.class)))
      .thenReturn(Future.failedFuture(new RuntimeException("Transaction service error")));

    // When
    Future<FiscalYearsHolder> result = orderFiscalYearService.getAvailableFiscalYears(ORDER_ID, requestContext);

    // Then
    assertTrue(result.failed());
    assertInstanceOf(RuntimeException.class, result.cause());
    assertEquals("Transaction service error", result.cause().getMessage());
  }

  @Test
  void testGetAvailableFiscalYears_FundServiceFailures_ContinuesGracefully() {
    // Given - Some fund service calls fail
    Transaction transaction1 = new Transaction().withFiscalYearId(FISCAL_YEAR_ID_1).withFromFundId(FUND_ID_1);
    Transaction transaction2 = new Transaction().withFiscalYearId(FISCAL_YEAR_ID_2).withFromFundId(FUND_ID_2);
    List<Transaction> transactions = List.of(transaction1, transaction2);

    FiscalYear fy1 = new FiscalYear().withId(FISCAL_YEAR_ID_1).withName("FY2023");
    FiscalYear fy2 = new FiscalYear().withId(FISCAL_YEAR_ID_2).withName("FY2022");
    List<FiscalYear> fiscalYears = List.of(fy1, fy2);

    mockServices(transactions, fiscalYears);
    when(fiscalYearService.getCurrentFiscalYearByFundId(FUND_ID_1, requestContext))
      .thenReturn(Future.succeededFuture(fy1)); // This succeeds - returns fy1 which is in the transaction list
    when(fiscalYearService.getCurrentFiscalYearByFundId(FUND_ID_2, requestContext))
      .thenReturn(Future.failedFuture(new RuntimeException("Fund service error"))); // This fails but is handled

    // When
    Future<FiscalYearsHolder> result = orderFiscalYearService.getAvailableFiscalYears(ORDER_ID, requestContext);

    // Then
    assertTrue(result.succeeded());
    FiscalYearsHolder holder = result.result();

    // Current should contain only the successfully retrieved fiscal year
    assertEquals(1, holder.getCurrent().size());
    assertEquals("FY2023", holder.getCurrent().get(0).getName());

    // Previous should contain the remaining transaction fiscal year
    assertEquals(1, holder.getPrevious().size());
    assertEquals("FY2022", holder.getPrevious().get(0).getName());
  }

  private void mockServices(List<Transaction> transactions, List<FiscalYear> fiscalYears) {
    when(purchaseOrderStorageService.getCompositeOrderById(ORDER_ID, requestContext))
      .thenReturn(Future.succeededFuture(new CompositePurchaseOrder()));
    when(transactionService.getTransactions(anyString(), any(RequestContext.class)))
      .thenReturn(Future.succeededFuture(transactions));
    if (fiscalYears != null) {
      when(fiscalYearService.getAllFiscalYears(anyCollection(), any(RequestContext.class)))
        .thenReturn(Future.succeededFuture(fiscalYears));
    }
  }
}