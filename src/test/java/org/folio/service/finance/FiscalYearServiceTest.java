package org.folio.service.finance;

import static io.vertx.core.Future.failedFuture;
import static io.vertx.core.Future.succeededFuture;
import static org.folio.TestConstants.ID_DOES_NOT_EXIST;
import static org.junit.jupiter.api.Assertions.assertEquals;
import static org.junit.jupiter.api.Assertions.assertNotNull;
import static org.mockito.ArgumentMatchers.any;
import static org.mockito.ArgumentMatchers.eq;
import static org.mockito.Mockito.doReturn;
import static org.mockito.Mockito.verify;

import java.util.UUID;

import io.vertx.junit5.VertxTestContext;
import org.folio.rest.core.models.RequestEntry;
import org.folio.rest.core.RestClient;
import io.vertx.junit5.VertxExtension;
import org.folio.rest.acq.model.finance.FiscalYear;

import org.folio.rest.core.exceptions.HttpException;
import org.folio.rest.core.models.RequestContext;
import org.junit.jupiter.api.BeforeEach;
import org.junit.jupiter.api.Test;
import org.mockito.InjectMocks;
import org.mockito.Mock;
import org.mockito.MockitoAnnotations;
import org.junit.jupiter.api.extension.ExtendWith;

import io.vertx.core.Future;
import org.mockito.Spy;

@ExtendWith(VertxExtension.class)
public class FiscalYearServiceTest {

  @InjectMocks
  private FiscalYearService fiscalYearService;
  @Spy
  private RestClient restClientMock;
  @Mock
  private RequestContext requestContextMock;

  @BeforeEach
  public void initMocks() {
    MockitoAnnotations.openMocks(this);
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
}
