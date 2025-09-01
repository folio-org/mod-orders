package org.folio.service.orders;

import static org.junit.jupiter.api.Assertions.assertEquals;
import static org.junit.jupiter.api.Assertions.assertInstanceOf;
import static org.junit.jupiter.api.Assertions.assertTrue;
import static org.mockito.ArgumentMatchers.any;
import static org.mockito.ArgumentMatchers.anyCollection;
import static org.mockito.ArgumentMatchers.anyString;
import static org.mockito.Mockito.when;

import java.util.Arrays;
import java.util.Collections;
import java.util.List;

import org.folio.CopilotGenerated;
import org.folio.rest.acq.model.finance.FiscalYear;
import org.folio.rest.acq.model.finance.FiscalYearCollection;
import org.folio.rest.acq.model.finance.Transaction;
import org.folio.rest.core.models.RequestContext;
import org.folio.service.finance.FiscalYearService;
import org.folio.service.finance.transaction.TransactionService;
import org.junit.jupiter.api.BeforeEach;
import org.junit.jupiter.api.Test;
import org.junit.jupiter.api.extension.ExtendWith;
import org.mockito.InjectMocks;
import org.mockito.Mock;
import org.mockito.junit.jupiter.MockitoExtension;

import io.vertx.core.Future;

@ExtendWith(MockitoExtension.class)
@CopilotGenerated(model = "Claude Sonnet 4")
class OrderFiscalYearServiceTest {

  @Mock
  private TransactionService transactionService;
  @Mock
  private FiscalYearService fiscalYearService;

  @Mock
  private RequestContext requestContext;

  @InjectMocks
  private OrderFiscalYearService orderFiscalYearService;

  private static final String ORDER_ID = "test-order-id";
  private static final String FISCAL_YEAR_ID_1 = "fiscal-year-1";
  private static final String FISCAL_YEAR_ID_2 = "fiscal-year-2";

  @BeforeEach
  void setUp() {
    orderFiscalYearService = new OrderFiscalYearService(transactionService, fiscalYearService);
  }

  @Test
  void testGetAvailableFiscalYears_ReturnsDistinctFiscalYearIds() {
    // Given - tests both duplicate filtering and null filtering
    Transaction transaction1 = new Transaction()
      .withFiscalYearId(FISCAL_YEAR_ID_1);
    Transaction transaction2 = new Transaction()
      .withFiscalYearId(FISCAL_YEAR_ID_2);
    Transaction transaction3 = new Transaction()
      .withFiscalYearId(FISCAL_YEAR_ID_1); // Duplicate
    Transaction transaction4 = new Transaction()
      .withFiscalYearId(null); // Null fiscal year ID

    List<Transaction> transactions = Arrays.asList(transaction1, transaction2, transaction3, transaction4);

    FiscalYear fiscalYear1 = new FiscalYear().withId(FISCAL_YEAR_ID_1).withName("FY2023");
    FiscalYear fiscalYear2 = new FiscalYear().withId(FISCAL_YEAR_ID_2).withName("FY2024");
    List<FiscalYear> fiscalYears = Arrays.asList(fiscalYear1, fiscalYear2);

    when(transactionService.getTransactions(anyString(), any(RequestContext.class)))
        .thenReturn(Future.succeededFuture(transactions));
    when(fiscalYearService.getAllFiscalYears(anyCollection(), any(RequestContext.class)))
        .thenReturn(Future.succeededFuture(fiscalYears));
    when(fiscalYearService.getCurrentFYForSeriesByFYId(FISCAL_YEAR_ID_1, requestContext))
        .thenReturn(Future.succeededFuture(FISCAL_YEAR_ID_1)); // Current FY is same as existing

    // When
    Future<FiscalYearCollection> result = orderFiscalYearService.getAvailableFiscalYears(ORDER_ID, requestContext);

    // Then
    assertTrue(result.succeeded());
    FiscalYearCollection fiscalYearCollection = result.result();
    List<String> fiscalYearIds = fiscalYearCollection.getFiscalYears().stream().map(FiscalYear::getId).toList();
    assertEquals(2, fiscalYearIds.size());
    assertTrue(fiscalYearIds.contains(FISCAL_YEAR_ID_1));
    assertTrue(fiscalYearIds.contains(FISCAL_YEAR_ID_2));
    assertEquals(2, fiscalYearCollection.getTotalRecords());
  }

  @Test
  void testGetAvailableFiscalYears_EmptyTransactionList() {
    // Given
    when(transactionService.getTransactions(anyString(), any(RequestContext.class)))
      .thenReturn(Future.succeededFuture(Collections.emptyList()));

    // When
    Future<FiscalYearCollection> result = orderFiscalYearService.getAvailableFiscalYears(ORDER_ID, requestContext);

    // Then
    assertTrue(result.succeeded());
    FiscalYearCollection fiscalYearCollection = result.result();
    List<FiscalYear> fiscalYears = fiscalYearCollection.getFiscalYears();
    assertTrue(fiscalYears.isEmpty());
    assertEquals(0, fiscalYearCollection.getTotalRecords());
  }

  @Test
  void testGetAvailableFiscalYears_TransactionServiceFailure() {
    // Given
    when(transactionService.getTransactions(anyString(), any(RequestContext.class)))
      .thenReturn(Future.failedFuture(new RuntimeException("Service error")));

    // When
    Future<FiscalYearCollection> result = orderFiscalYearService.getAvailableFiscalYears(ORDER_ID, requestContext);

    // Then
    assertTrue(result.failed());
    assertInstanceOf(RuntimeException.class, result.cause());
    assertEquals("Service error", result.cause().getMessage());
  }

  @Test
  void testGetAvailableFiscalYears_FiscalYearServiceFailure() {
    // Given
    Transaction transaction1 = new Transaction()
      .withFiscalYearId(FISCAL_YEAR_ID_1);
    List<Transaction> transactions = Arrays.asList(transaction1);

    when(transactionService.getTransactions(anyString(), any(RequestContext.class)))
      .thenReturn(Future.succeededFuture(transactions));
    when(fiscalYearService.getAllFiscalYears(anyCollection(), any(RequestContext.class)))
      .thenReturn(Future.failedFuture(new RuntimeException("FiscalYear service error")));

    // When
    Future<FiscalYearCollection> result = orderFiscalYearService.getAvailableFiscalYears(ORDER_ID, requestContext);

    // Then
    assertTrue(result.failed());
    assertInstanceOf(RuntimeException.class, result.cause());
    assertEquals("FiscalYear service error", result.cause().getMessage());
  }

  @Test
  void testGetAvailableFiscalYears_AddsCurrentFiscalYearWhenMissing() {
    // Given
    String currentFiscalYearId = "current-fiscal-year";
    Transaction transaction1 = new Transaction().withFiscalYearId(FISCAL_YEAR_ID_1);
    List<Transaction> transactions = List.of(transaction1);

    FiscalYear fiscalYear1 = new FiscalYear().withId(FISCAL_YEAR_ID_1).withName("FY2023");
    FiscalYear currentFiscalYear = new FiscalYear().withId(currentFiscalYearId).withName("FY2024");

    when(transactionService.getTransactions(anyString(), any(RequestContext.class)))
      .thenReturn(Future.succeededFuture(transactions));
    when(fiscalYearService.getAllFiscalYears(anyCollection(), any(RequestContext.class)))
      .thenReturn(Future.succeededFuture(List.of(fiscalYear1)));
    when(fiscalYearService.getCurrentFYForSeriesByFYId(FISCAL_YEAR_ID_1, requestContext))
      .thenReturn(Future.succeededFuture(currentFiscalYearId));
    when(fiscalYearService.getFiscalYearById(currentFiscalYearId, requestContext))
      .thenReturn(Future.succeededFuture(currentFiscalYear));

    // When
    Future<FiscalYearCollection> result = orderFiscalYearService.getAvailableFiscalYears(ORDER_ID, requestContext);

    // Then
    assertTrue(result.succeeded());
    FiscalYearCollection collection = result.result();
    assertEquals(2, collection.getTotalRecords());

    List<String> fiscalYearIds = collection.getFiscalYears().stream()
      .map(FiscalYear::getId)
      .toList();
    assertTrue(fiscalYearIds.contains(FISCAL_YEAR_ID_1));
    assertTrue(fiscalYearIds.contains(currentFiscalYearId));

    // Verify sorting by name (FY2023 should come before FY2024)
    assertEquals("FY2023", collection.getFiscalYears().get(0).getName());
    assertEquals("FY2024", collection.getFiscalYears().get(1).getName());
  }


  @Test
  void testGetAvailableFiscalYears_HandlesNoCurrentFiscalYearFound() {
    // Given
    Transaction transaction1 = new Transaction().withFiscalYearId(FISCAL_YEAR_ID_1);
    List<Transaction> transactions = List.of(transaction1);

    FiscalYear fiscalYear1 = new FiscalYear().withId(FISCAL_YEAR_ID_1).withName("FY2023");

    when(transactionService.getTransactions(anyString(), any(RequestContext.class)))
      .thenReturn(Future.succeededFuture(transactions));
    when(fiscalYearService.getAllFiscalYears(anyCollection(), any(RequestContext.class)))
      .thenReturn(Future.succeededFuture(List.of(fiscalYear1)));
    when(fiscalYearService.getCurrentFYForSeriesByFYId(FISCAL_YEAR_ID_1, requestContext))
      .thenReturn(Future.succeededFuture(null)); // No current fiscal year found

    // When
    Future<FiscalYearCollection> result = orderFiscalYearService.getAvailableFiscalYears(ORDER_ID, requestContext);

    // Then
    assertTrue(result.succeeded());
    FiscalYearCollection collection = result.result();
    assertEquals(1, collection.getTotalRecords());
    assertEquals(FISCAL_YEAR_ID_1, collection.getFiscalYears().getFirst().getId());
  }
}
