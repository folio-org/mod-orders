package org.folio.service.finance.rollover;

import static io.vertx.core.Future.succeededFuture;
import static org.folio.TestConfig.mockPort;
import static org.folio.TestConstants.X_OKAPI_TOKEN;
import static org.folio.TestConstants.X_OKAPI_USER_ID;
import static org.folio.orders.utils.ResourcePathResolver.LEDGER_FY_ROLLOVER_ERRORS;
import static org.folio.orders.utils.ResourcePathResolver.resourcesPath;
import static org.folio.rest.RestConstants.OKAPI_URL;
import static org.folio.rest.core.RestClientTest.X_OKAPI_TENANT;
import static org.hamcrest.MatcherAssert.assertThat;
import static org.hamcrest.Matchers.containsString;
import static org.junit.jupiter.api.Assertions.assertEquals;
import static org.junit.jupiter.api.Assertions.assertNotEquals;
import static org.junit.jupiter.api.Assertions.assertNull;
import static org.mockito.ArgumentMatchers.any;
import static org.mockito.ArgumentMatchers.eq;
import static org.mockito.Mockito.doReturn;
import static org.mockito.Mockito.times;
import static org.mockito.Mockito.verify;

import java.net.URLDecoder;
import java.nio.charset.StandardCharsets;
import java.util.HashMap;
import java.util.List;
import java.util.Map;
import java.util.UUID;

import org.folio.rest.acq.model.finance.LedgerFiscalYearRolloverError;
import org.folio.rest.acq.model.finance.LedgerFiscalYearRolloverErrorCollection;
import org.folio.rest.core.RestClient;
import org.folio.rest.core.models.RequestContext;
import org.folio.rest.core.models.RequestEntry;
import org.junit.jupiter.api.BeforeEach;
import org.junit.jupiter.api.Test;
import org.junit.jupiter.api.extension.ExtendWith;
import org.mockito.ArgumentCaptor;
import org.mockito.InjectMocks;
import org.mockito.Mock;
import org.mockito.Mockito;
import org.mockito.MockitoAnnotations;

import io.vertx.core.Vertx;
import io.vertx.junit5.VertxExtension;
import io.vertx.junit5.VertxTestContext;

@ExtendWith(VertxExtension.class)
public class LedgerRolloverErrorServiceTest {

  @InjectMocks
  private LedgerRolloverErrorService ledgerRolloverErrorService;
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
  void testShouldRetrieveRolloverErrorsByRolloverId(VertxTestContext vertxTestContext) {
    //Given
    String rolloverId = UUID.randomUUID().toString();
    String rolloverErrorId = UUID.randomUUID().toString();

    var rolloverError = new LedgerFiscalYearRolloverError().withId(rolloverErrorId).withLedgerRolloverId(rolloverId)
      .withErrorType(LedgerFiscalYearRolloverError.ErrorType.ORDER_ROLLOVER).withFailedAction("Overall order rollover")
      .withErrorMessage("Error when retrieving exchange rate provider");
    var errorCollection = new LedgerFiscalYearRolloverErrorCollection()
      .withLedgerFiscalYearRolloverErrors(List.of(rolloverError)).withTotalRecords(1);

    doReturn(succeededFuture(errorCollection)).when(restClient).get(any(RequestEntry.class),
      eq(LedgerFiscalYearRolloverErrorCollection.class), eq(requestContext));

    //When
    var future = ledgerRolloverErrorService.getRolloverErrorsByRolloverId(rolloverId, requestContext);
    vertxTestContext.assertComplete(future)
      .onComplete(result -> {
        //Then
        assertEquals(result.result(), errorCollection);

        ArgumentCaptor<RequestEntry> argumentCaptor = ArgumentCaptor.forClass(RequestEntry.class);
        verify(restClient).get(argumentCaptor.capture(), eq(LedgerFiscalYearRolloverErrorCollection.class), eq(requestContext));

        RequestEntry requestEntry = argumentCaptor.getValue();
        assertEquals(resourcesPath(LEDGER_FY_ROLLOVER_ERRORS), requestEntry.getBaseEndpoint());
        assertThat(URLDecoder.decode(requestEntry.buildEndpoint(), StandardCharsets.UTF_8), containsString("ledgerRolloverId==" + rolloverId));
        vertxTestContext.completeNow();
      });
  }

  @Test
  void testShouldSaveRolloverError(VertxTestContext vertxTestContext) {
    //Given
    String rolloverId = UUID.randomUUID().toString();
    String rolloverErrorId = UUID.randomUUID().toString();

    var rolloverError = new LedgerFiscalYearRolloverError().withId(rolloverErrorId).withLedgerRolloverId(rolloverId)
      .withErrorType(LedgerFiscalYearRolloverError.ErrorType.ORDER_ROLLOVER).withFailedAction("Overall order rollover")
      .withErrorMessage("Error when retrieving exchange rate provider");

    Throwable throwable = new RuntimeException("Error when retrieving exchange rate provider");

    doReturn(succeededFuture(rolloverError)).when(restClient).post(any(RequestEntry.class),
      any(LedgerFiscalYearRolloverError.class), eq(LedgerFiscalYearRolloverError.class), eq(requestContext));

    //When
    var future = ledgerRolloverErrorService.saveRolloverError(rolloverId, throwable,
      LedgerFiscalYearRolloverError.ErrorType.ORDER_ROLLOVER, "Overall order rollover", requestContext);
    vertxTestContext.assertComplete(future)
      .onComplete(result -> {
        //Then
        assertEquals(result.result(), rolloverError);

        ArgumentCaptor<RequestEntry> errorCaptor = ArgumentCaptor.forClass(RequestEntry.class);
        ArgumentCaptor<LedgerFiscalYearRolloverError> rolloverErrorCaptor =
          ArgumentCaptor.forClass(LedgerFiscalYearRolloverError.class);
        verify(restClient).post(errorCaptor.capture(), rolloverErrorCaptor.capture(),
          eq(LedgerFiscalYearRolloverError.class), eq(requestContext));

        RequestEntry requestEntry = errorCaptor.getValue();
        LedgerFiscalYearRolloverError rolloverErrorAfterCapture = rolloverErrorCaptor.getValue();

        assertNull(rolloverErrorAfterCapture.getId());
        assertNotEquals(rolloverError, rolloverErrorAfterCapture);
        assertEquals(resourcesPath(LEDGER_FY_ROLLOVER_ERRORS), requestEntry.buildEndpoint());
        vertxTestContext.completeNow();
      });
  }

  @Test
  void testShouldDeleteRolloverErrors(VertxTestContext vertxTestContext) {
    //Given
    String rolloverId = UUID.randomUUID().toString();
    String rolloverErrorId = UUID.randomUUID().toString();

    var rolloverError = new LedgerFiscalYearRolloverError().withId(rolloverErrorId).withLedgerRolloverId(rolloverId)
      .withErrorType(LedgerFiscalYearRolloverError.ErrorType.ORDER_ROLLOVER).withFailedAction("Overall order rollover")
      .withErrorMessage("Error when retrieving exchange rate provider");

    LedgerRolloverErrorService spy = Mockito.spy(ledgerRolloverErrorService);
    doReturn(succeededFuture()).when(restClient).delete(any(RequestEntry.class), eq(requestContext));

    //When
    var future = spy.deleteRolloverErrors(List.of(rolloverError), requestContext);
    vertxTestContext.assertComplete(future)
      .onComplete(result -> {
        //Then
        ArgumentCaptor<RequestEntry> errorCaptor = ArgumentCaptor.forClass(RequestEntry.class);

        verify(restClient).delete(errorCaptor.capture(), eq(requestContext));
        verify(restClient, times(1)).delete(any(RequestEntry.class), eq(requestContext));
        verify(spy, times(1)).deleteRolloverError(rolloverErrorId, requestContext);

        RequestEntry requestEntry = errorCaptor.getValue();

        assertEquals("/finance/ledger-rollovers-errors/" + rolloverErrorId, requestEntry.buildEndpoint());
        vertxTestContext.completeNow();
      });
  }

}
