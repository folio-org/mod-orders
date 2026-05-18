package org.folio.service.finance;

import static io.vertx.core.Future.failedFuture;
import static io.vertx.core.Future.succeededFuture;
import static org.folio.TestConstants.ID_DOES_NOT_EXIST;
import static org.junit.jupiter.api.Assertions.assertEquals;
import static org.junit.jupiter.api.Assertions.assertInstanceOf;
import static org.junit.jupiter.api.Assertions.assertNotNull;
import static org.junit.jupiter.api.Assertions.assertNull;
import static org.junit.jupiter.api.Assertions.assertTrue;
import static org.mockito.ArgumentMatchers.any;
import static org.mockito.ArgumentMatchers.eq;
import static org.mockito.Mockito.doReturn;
import static org.mockito.Mockito.verify;

import java.time.Instant;
import java.time.temporal.ChronoUnit;
import java.util.Date;
import java.util.List;
import java.util.UUID;

import io.vertx.junit5.VertxTestContext;
import org.folio.CopilotGenerated;
import org.folio.TestMate;
import org.folio.rest.acq.model.finance.FiscalYearCollection;
import org.folio.rest.core.models.RequestEntry;
import org.folio.rest.core.RestClient;
import io.vertx.junit5.VertxExtension;
import org.folio.rest.acq.model.finance.FiscalYear;

import org.folio.rest.core.exceptions.HttpException;
import org.folio.rest.core.models.RequestContext;
import org.folio.service.caches.CommonSettingsCache;
import org.junit.jupiter.api.AfterEach;
import org.junit.jupiter.api.BeforeEach;
import org.junit.jupiter.api.Test;
import org.junit.jupiter.api.extension.ExtendWith;

import io.vertx.core.Future;
import org.mockito.InjectMocks;
import org.mockito.Mock;
import org.mockito.MockitoAnnotations;
import org.mockito.Spy;
import java.util.Collections;

@CopilotGenerated(partiallyGenerated = true)
@ExtendWith(VertxExtension.class)
public class FiscalYearServiceTest {

  @InjectMocks
  private FiscalYearService fiscalYearService;
  @Spy
  private RestClient restClientMock;
  @Mock
  private RequestContext requestContextMock;
  @Mock
  private CommonSettingsCache commonSettingsCacheMock;

  private AutoCloseable openMocks;

  @BeforeEach
  public void initMocks() {
    openMocks = MockitoAnnotations.openMocks(this);
    doReturn(Future.succeededFuture("UTC"))
      .when(commonSettingsCacheMock).getSystemTimeZone(any(RequestContext.class));
    fiscalYearService.init();
  }

  @AfterEach
  public void closeMocks() throws Exception {
    if (openMocks != null) {
      openMocks.close();
    }
  }

  @Test
  void testShouldReturnCurrentFiscalYearForLedger() {
    String ledgerId = UUID.randomUUID().toString();

    doReturn(succeededFuture(new FiscalYear()))
      .when(restClientMock)
      .get(any(RequestEntry.class), eq(FiscalYear.class), any(RequestContext.class));

    FiscalYear fy = fiscalYearService.getCurrentFiscalYear(ledgerId, requestContextMock).result();

    assertNotNull(fy);
  }

  @Test
  void testShouldThrowHttpException(VertxTestContext vertxTestContext) {
    doReturn(failedFuture(new HttpException(404, "Fiscal year not found")))
      .when(restClientMock)
      .get(any(RequestEntry.class), eq(FiscalYear.class), any(RequestContext.class));

    Future<FiscalYear> result = fiscalYearService.getCurrentFiscalYear(ID_DOES_NOT_EXIST, requestContextMock);

    vertxTestContext.assertFailure(result)
      .onComplete(expectedException -> {
        HttpException httpException = (HttpException) expectedException.cause();
        assertEquals(404, httpException.getCode());
        assertEquals(result.cause().getMessage(), httpException.getMessage());
        verify(restClientMock).get(any(RequestEntry.class), eq(FiscalYear.class), any(RequestContext.class));
        vertxTestContext.completeNow();
      });
  }

  @Test
  void shouldReturnCurrentFiscalYearIdForSeries() {
    String fiscalYearId = UUID.randomUUID().toString();
    String series = "FY2023";
    String currentFiscalYearId = UUID.randomUUID().toString();
    FiscalYear fiscalYear = new FiscalYear().withId(fiscalYearId).withSeries(series)
      .withPeriodStart(Date.from(Instant.now().minus(265, ChronoUnit.DAYS)))
      .withPeriodEnd(Date.from(Instant.now().plus(100, ChronoUnit.DAYS)));
    FiscalYear currentFiscalYear = new FiscalYear().withId(currentFiscalYearId).withSeries(series)
      .withPeriodStart(Date.from(Instant.now().minus(165, ChronoUnit.DAYS)))
      .withPeriodEnd(Date.from(Instant.now().plus(200, ChronoUnit.DAYS)));
    FiscalYearCollection fiscalYearCollection = new FiscalYearCollection().withFiscalYears(List.of(fiscalYear, currentFiscalYear));

    doReturn(Future.succeededFuture(fiscalYear))
      .when(restClientMock).get(any(RequestEntry.class), eq(FiscalYear.class), any(RequestContext.class));
    doReturn(Future.succeededFuture(fiscalYearCollection))
      .when(restClientMock).get(any(RequestEntry.class), eq(FiscalYearCollection.class), any(RequestContext.class));

    Future<String> result = fiscalYearService.getCurrentFYForSeriesByFYId(fiscalYearId, requestContextMock);

    assertEquals(currentFiscalYearId, result.result());
  }

  @Test
  void shouldReturnCurrentFiscalYearIdForSeriesIfOnlyOneFiscalYearIsFound() {
    String fiscalYearId = UUID.randomUUID().toString();
    String series = "FY2023";
    FiscalYear fiscalYear = new FiscalYear().withId(fiscalYearId).withSeries(series)
      .withPeriodStart(Date.from(Instant.now().minus(265, ChronoUnit.DAYS)))
      .withPeriodEnd(Date.from(Instant.now().plus(100, ChronoUnit.DAYS)));
    FiscalYearCollection fiscalYearCollection = new FiscalYearCollection().withFiscalYears(List.of(fiscalYear));

    doReturn(Future.succeededFuture(fiscalYear))
      .when(restClientMock).get(any(RequestEntry.class), eq(FiscalYear.class), any(RequestContext.class));
    doReturn(Future.succeededFuture(fiscalYearCollection))
      .when(restClientMock).get(any(RequestEntry.class), eq(FiscalYearCollection.class), any(RequestContext.class));

    Future<String> result = fiscalYearService.getCurrentFYForSeriesByFYId(fiscalYearId, requestContextMock);

    assertEquals(fiscalYearId, result.result());
  }

  @Test
  void shouldReturnNullFiscalYearIdForSeriesIfNoFiscalYearIsFound() {
    String fiscalYearId = UUID.randomUUID().toString();
    String series = "FY2023";
    FiscalYear fiscalYear = new FiscalYear().withId(fiscalYearId).withSeries(series)
      .withPeriodStart(Date.from(Instant.now().minus(265, ChronoUnit.DAYS)))
      .withPeriodEnd(Date.from(Instant.now().plus(100, ChronoUnit.DAYS)));
    FiscalYearCollection fiscalYearCollection = new FiscalYearCollection();

    doReturn(Future.succeededFuture(fiscalYear))
      .when(restClientMock).get(any(RequestEntry.class), eq(FiscalYear.class), any(RequestContext.class));
    doReturn(Future.succeededFuture(fiscalYearCollection))
      .when(restClientMock).get(any(RequestEntry.class), eq(FiscalYearCollection.class), any(RequestContext.class));

    Future<String> result = fiscalYearService.getCurrentFYForSeriesByFYId(fiscalYearId, requestContextMock);

    assertNull(result.result());
  }

  @Test
  void shouldThrowExceptionWhenFiscalYearNotFound() {
    String fiscalYearId = UUID.randomUUID().toString();

    doReturn(Future.failedFuture(new HttpException(404, "Fiscal year not found")))
      .when(restClientMock).get(any(RequestEntry.class), eq(FiscalYear.class), any(RequestContext.class));

    Future<String> result = fiscalYearService.getCurrentFYForSeriesByFYId(fiscalYearId, requestContextMock);

    assertTrue(result.failed());
    assertEquals(HttpException.class, result.cause().getClass());
    assertEquals(404, ((HttpException) result.cause()).getCode());
  }

  @Test
  void shouldThrowExceptionWhenCurrentFiscalYearNotFoundForSeries() {
    String fiscalYearId = UUID.randomUUID().toString();
    String series = "FY2023";
    FiscalYear fiscalYear = new FiscalYear().withId(fiscalYearId).withSeries(series);

    doReturn(Future.succeededFuture(fiscalYear))
      .when(restClientMock).get(any(RequestEntry.class), eq(FiscalYear.class), any(RequestContext.class));
    doReturn(Future.failedFuture(new HttpException(404, "Fiscal year not found")))
      .when(restClientMock).get(any(RequestEntry.class), eq(FiscalYearCollection.class), any(RequestContext.class));

    Future<String> result = fiscalYearService.getCurrentFYForSeriesByFYId(fiscalYearId, requestContextMock);

    assertTrue(result.failed());
    assertEquals(HttpException.class, result.cause().getClass());
    assertEquals(404, ((HttpException) result.cause()).getCode());
  }

  @Test
  void shouldReturnAllFiscalYearsForSingleBatch() {
    // Test basic functionality with small number of IDs
    String fyId1 = UUID.randomUUID().toString();
    String fyId2 = UUID.randomUUID().toString();
    List<String> fiscalYearIds = List.of(fyId1, fyId2);

    FiscalYear fy1 = new FiscalYear().withId(fyId1).withSeries("FY2023");
    FiscalYear fy2 = new FiscalYear().withId(fyId2).withSeries("FY2024");
    FiscalYearCollection mockCollection = new FiscalYearCollection()
      .withFiscalYears(List.of(fy1, fy2))
      .withTotalRecords(2);

    doReturn(Future.succeededFuture(mockCollection))
      .when(restClientMock).get(any(RequestEntry.class), eq(FiscalYearCollection.class), any(RequestContext.class));

    Future<List<FiscalYear>> result = fiscalYearService.getAllFiscalYears(fiscalYearIds, requestContextMock);

    List<FiscalYear> fiscalYears = result.result();
    assertEquals(2, fiscalYears.size());
    assertEquals(fyId1, fiscalYears.get(0).getId());
    assertEquals(fyId2, fiscalYears.get(1).getId());
  }

  @Test
  void shouldReturnEmptyCollectionWhenNoFiscalYearsProvided() {
    // Test edge case with empty input
    List<String> emptyIds = List.of();

    Future<List<FiscalYear>> result = fiscalYearService.getAllFiscalYears(emptyIds, requestContextMock);

    List<FiscalYear> fiscalYears = result.result();
    assertEquals(0, fiscalYears.size());
  }

  @Test
  void shouldHandleDuplicateIdsInGetAllFiscalYears() {
    // Test deduplication logic
    String fyId1 = UUID.randomUUID().toString();
    String fyId2 = UUID.randomUUID().toString();
    List<String> fiscalYearIds = List.of(fyId1, fyId2, fyId1); // Contains duplicate

    FiscalYear fy1 = new FiscalYear().withId(fyId1).withSeries("FY2023");
    FiscalYear fy2 = new FiscalYear().withId(fyId2).withSeries("FY2024");
    FiscalYearCollection mockCollection = new FiscalYearCollection()
      .withFiscalYears(List.of(fy1, fy2))
      .withTotalRecords(2);

    doReturn(Future.succeededFuture(mockCollection))
      .when(restClientMock).get(any(RequestEntry.class), eq(FiscalYearCollection.class), any(RequestContext.class));

    Future<List<FiscalYear>> result = fiscalYearService.getAllFiscalYears(fiscalYearIds, requestContextMock);

    List<FiscalYear> fiscalYears = result.result();

    assertEquals(2, fiscalYears.size());
  }

  @Test
  void testShouldReturnPlannedFiscalYearForLedger() {
    // Test successful retrieval of planned fiscal year
    String ledgerId = UUID.randomUUID().toString();
    FiscalYear plannedFiscalYear = new FiscalYear()
      .withId(UUID.randomUUID().toString())
      .withName("FY2025")
      .withSeries("FY");

    doReturn(succeededFuture(plannedFiscalYear))
      .when(restClientMock)
      .get(any(RequestEntry.class), eq(FiscalYear.class), any(RequestContext.class));

    Future<FiscalYear> result = fiscalYearService.getPlannedFiscalYear(ledgerId, requestContextMock);

    assertTrue(result.succeeded());
    FiscalYear fy = result.result();
    assertNotNull(fy);
    assertEquals("FY2025", fy.getName());
    assertEquals("FY", fy.getSeries());
  }

  @Test
  void testShouldReturnNullWhenPlannedFiscalYearNotFound() {
    // Test that method returns null instead of throwing exception when planned fiscal year is not found
    String ledgerId = UUID.randomUUID().toString();

    doReturn(failedFuture(new HttpException(404, "Planned fiscal year not found")))
      .when(restClientMock)
      .get(any(RequestEntry.class), eq(FiscalYear.class), any(RequestContext.class));

    Future<FiscalYear> result = fiscalYearService.getPlannedFiscalYear(ledgerId, requestContextMock);

    assertTrue(result.succeeded());
    assertNull(result.result());
  }

  @Test
  void testShouldHandleGenericExceptionForPlannedFiscalYear(VertxTestContext vertxTestContext) {
    // Test that generic exceptions are wrapped and propagated
    String ledgerId = UUID.randomUUID().toString();
    RuntimeException genericException = new RuntimeException("Network error");

    doReturn(failedFuture(genericException))
      .when(restClientMock)
      .get(any(RequestEntry.class), eq(FiscalYear.class), any(RequestContext.class));

    Future<FiscalYear> result = fiscalYearService.getPlannedFiscalYear(ledgerId, requestContextMock);

    vertxTestContext.assertFailure(result)
      .onComplete(expectedException -> {
        assertInstanceOf(RuntimeException.class, expectedException.cause());
        assertTrue(expectedException.cause().getMessage().contains("Network error"));
        vertxTestContext.completeNow();
      });
  }

  @Test
  @TestMate(name = "TestMate-e21692b9cd8e81d826670de16ac5015d")
  void extractCurrentFiscalYearIdShouldReturnNullWhenListIsEmpty() {
    // Given
    List<FiscalYear> fiscalYears = Collections.emptyList();

    // When
    String result = fiscalYearService.extractCurrentFiscalYearId(fiscalYears);

    // Then
    assertNull(result);
  }

  @Test
  @TestMate(name = "TestMate-50149490ca055a147990762b880eeda5")
  void extractCurrentFiscalYearIdShouldReturnTheOnlyIdWhenListHasOneElement() {
    // Given
    String expectedId = "00000000-0000-4000-8000-000000000001";
    FiscalYear fiscalYear = new FiscalYear().withId(expectedId);
    List<FiscalYear> fiscalYears = List.of(fiscalYear);

    // When
    String result = fiscalYearService.extractCurrentFiscalYearId(fiscalYears);

    // Then
    assertEquals(expectedId, result);
  }

  @Test
  @TestMate(name = "TestMate-84a3ff3ccbdade263f79b241b15a8400")
  void extractCurrentFiscalYearIdShouldHandleOverlappingFiscalYears() {
    // Given
    String firstYearId = UUID.randomUUID().toString();
    String secondYearId = UUID.randomUUID().toString();
    // The method under test uses 'new Date()' to determine the current time.
    // To ensure the test logic for overlapping active years is triggered,
    // we must define the fiscal year periods relative to the actual system time.
    Instant now = Instant.now();
    Date fiveDaysAgo = Date.from(now.minus(5, ChronoUnit.DAYS));
    Date twoDaysAgo = Date.from(now.minus(2, ChronoUnit.DAYS));
    Date fiveDaysFromNow = Date.from(now.plus(5, ChronoUnit.DAYS));
    Date tenDaysFromNow = Date.from(now.plus(10, ChronoUnit.DAYS));

    FiscalYear firstYear = new FiscalYear()
      .withId(firstYearId)
      .withPeriodStart(fiveDaysAgo)
      .withPeriodEnd(fiveDaysFromNow);

    FiscalYear secondYear = new FiscalYear()
      .withId(secondYearId)
      .withPeriodStart(twoDaysAgo)
      .withPeriodEnd(tenDaysFromNow);

    List<FiscalYear> fiscalYears = List.of(firstYear, secondYear);

    // When
    String result = fiscalYearService.extractCurrentFiscalYearId(fiscalYears);

    // Then
    assertEquals(secondYearId, result);
  }

  @Test
  @TestMate(name = "TestMate-71812d5cbd371fe4a0fbae44bc59aacd")
  void extractCurrentFiscalYearIdShouldDefaultToFirstWhenOnlyOneInOverlapActive() {
    // Given
    String firstYearId = "00000000-0000-4000-8000-000000000001";
    String secondYearId = "00000000-0000-4000-8000-000000000002";
    Instant now = Instant.now();

    FiscalYear firstYear = new FiscalYear()
      .withId(firstYearId)
      .withPeriodStart(Date.from(now.minus(5, ChronoUnit.DAYS)))
      .withPeriodEnd(Date.from(now.plus(5, ChronoUnit.DAYS)));

    FiscalYear secondYear = new FiscalYear()
      .withId(secondYearId)
      .withPeriodStart(Date.from(now.plus(1, ChronoUnit.DAYS)))
      .withPeriodEnd(Date.from(now.plus(10, ChronoUnit.DAYS)));

    List<FiscalYear> fiscalYears = List.of(firstYear, secondYear);

    // When
    String result = fiscalYearService.extractCurrentFiscalYearId(fiscalYears);

    // Then
    assertEquals(firstYearId, result);
  }
}
