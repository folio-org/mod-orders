package org.folio.service.finance;

import static io.vertx.core.Future.failedFuture;
import static io.vertx.core.Future.succeededFuture;
import static org.folio.TestConstants.ID_DOES_NOT_EXIST;
import static org.junit.jupiter.api.Assertions.assertEquals;
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
}
