package org.folio.service.finance.rollover;

import io.vertx.core.Vertx;
import io.vertx.junit5.VertxExtension;
import io.vertx.junit5.VertxTestContext;
import org.folio.rest.acq.model.finance.LedgerFiscalYearRolloverProgress;
import org.folio.rest.acq.model.finance.LedgerFiscalYearRolloverProgressCollection;
import org.folio.rest.core.RestClient;
import org.folio.rest.core.models.RequestContext;
import org.folio.rest.core.models.RequestEntry;
import org.folio.rest.jaxrs.model.RolloverStatus;
import org.junit.jupiter.api.BeforeEach;
import org.junit.jupiter.api.Test;
import org.junit.jupiter.api.extension.ExtendWith;
import org.mockito.ArgumentCaptor;
import org.mockito.InjectMocks;
import org.mockito.Mock;
import org.mockito.MockitoAnnotations;

import java.net.URLDecoder;
import java.nio.charset.StandardCharsets;
import java.util.HashMap;
import java.util.List;
import java.util.Map;
import java.util.UUID;

import static io.vertx.core.Future.succeededFuture;
import static org.folio.TestConfig.mockPort;
import static org.folio.TestConstants.X_OKAPI_TOKEN;
import static org.folio.TestConstants.X_OKAPI_USER_ID;
import static org.folio.rest.RestConstants.OKAPI_URL;
import static org.folio.rest.core.RestClientTest.X_OKAPI_TENANT;
import static org.hamcrest.MatcherAssert.assertThat;
import static org.hamcrest.Matchers.containsString;
import static org.junit.jupiter.api.Assertions.assertEquals;
import static org.mockito.ArgumentMatchers.any;
import static org.mockito.ArgumentMatchers.eq;
import static org.mockito.Mockito.doReturn;
import static org.mockito.Mockito.verify;

@ExtendWith(VertxExtension.class)
public class LedgerRolloverProgressServiceTest {

  @InjectMocks
  private LedgerRolloverProgressService ledgerRolloverProgressService;
  @Mock
  private RestClient restClient;
  private RequestContext requestContext;

  @BeforeEach
  public void initMocks() {
    Map<String, String> okapiHeadersMock = new HashMap<>();
    okapiHeadersMock.put(OKAPI_URL, "http://localhost:" + mockPort);
    okapiHeadersMock.put(X_OKAPI_TOKEN.getName(), X_OKAPI_TOKEN.getValue());
    okapiHeadersMock.put(X_OKAPI_TENANT.getName(), X_OKAPI_TENANT.getValue());
    okapiHeadersMock.put(X_OKAPI_USER_ID.getName(), X_OKAPI_USER_ID.getValue());
    requestContext = new RequestContext(Vertx.vertx().getOrCreateContext(), okapiHeadersMock);
    MockitoAnnotations.openMocks(this);
  }

  @Test
  void testShouldRetrieveRolloverProgressByRolloverId(VertxTestContext vertxTestContext) {
    //Given
    String rolloverId = UUID.randomUUID().toString();
    String progressId = UUID.randomUUID().toString();

    var rolloverProgress = new LedgerFiscalYearRolloverProgress().withId(progressId).withLedgerRolloverId(rolloverId)
      .withOverallRolloverStatus(RolloverStatus.IN_PROGRESS).withOrdersRolloverStatus(RolloverStatus.NOT_STARTED)
      .withFinancialRolloverStatus(RolloverStatus.SUCCESS).withBudgetsClosingRolloverStatus(RolloverStatus.SUCCESS);
    var progressCollection = new LedgerFiscalYearRolloverProgressCollection()
      .withLedgerFiscalYearRolloverProgresses(List.of(rolloverProgress)).withTotalRecords(1);

    doReturn(succeededFuture(progressCollection)).when(restClient).get(any(RequestEntry.class),
      eq(LedgerFiscalYearRolloverProgressCollection.class), eq(requestContext));

    //When
    var future = ledgerRolloverProgressService.getRolloversProgressByRolloverId(rolloverId, requestContext);
    vertxTestContext.assertComplete(future)
      .onComplete(result -> {
        //Then
        assertEquals(result.result(), rolloverProgress);

        ArgumentCaptor<RequestEntry> argumentCaptor = ArgumentCaptor.forClass(RequestEntry.class);
        verify(restClient).get(argumentCaptor.capture(), eq(LedgerFiscalYearRolloverProgressCollection.class), eq(requestContext));

        RequestEntry requestEntry = argumentCaptor.getValue();
        assertEquals("/finance/ledger-rollovers-progress", requestEntry.getBaseEndpoint());
        assertThat(URLDecoder.decode(requestEntry.buildEndpoint(), StandardCharsets.UTF_8), containsString("ledgerRolloverId==" + rolloverId));
        vertxTestContext.completeNow();
      });
  }

  @Test
  void testShouldUpdateRolloverProgress(VertxTestContext vertxTestContext) {
    //Given
    String rolloverId = UUID.randomUUID().toString();
    String progressId = UUID.randomUUID().toString();

    var rolloverProgress = new LedgerFiscalYearRolloverProgress().withId(progressId).withLedgerRolloverId(rolloverId)
      .withOverallRolloverStatus(RolloverStatus.IN_PROGRESS).withOrdersRolloverStatus(RolloverStatus.NOT_STARTED)
      .withFinancialRolloverStatus(RolloverStatus.SUCCESS).withBudgetsClosingRolloverStatus(RolloverStatus.SUCCESS);

    doReturn(succeededFuture()).when(restClient).put(any(RequestEntry.class),
      any(LedgerFiscalYearRolloverProgress.class), eq(requestContext));

    //When
    var future = ledgerRolloverProgressService.updateRolloverProgress(rolloverProgress,  requestContext);
    vertxTestContext.assertComplete(future)
      .onComplete(result -> {
        //Then

        ArgumentCaptor<RequestEntry> captor = ArgumentCaptor.forClass(RequestEntry.class);
        verify(restClient).put(captor.capture(), any(LedgerFiscalYearRolloverProgress.class), eq(requestContext));
        RequestEntry requestEntry = captor.getValue();

        assertEquals("/finance/ledger-rollovers-progress/" + progressId, requestEntry.buildEndpoint());
        vertxTestContext.completeNow();
      });
  }

}
