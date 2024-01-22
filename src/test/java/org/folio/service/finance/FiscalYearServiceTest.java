package org.folio.service.finance;

import static org.folio.TestConfig.mockPort;
import static org.folio.TestConstants.ID_DOES_NOT_EXIST;
import static org.folio.TestConstants.X_OKAPI_TOKEN;
import static org.folio.TestConstants.X_OKAPI_USER_ID;
import static org.folio.rest.RestConstants.OKAPI_URL;
import static org.folio.rest.RestVerticle.OKAPI_HEADER_TENANT;
import static org.junit.jupiter.api.Assertions.assertEquals;
import static org.junit.jupiter.api.Assertions.assertNotNull;
import static org.junit.jupiter.api.Assertions.assertThrows;

import java.lang.reflect.Field;
import java.util.HashMap;
import java.util.Map;
import java.util.UUID;
import java.util.concurrent.CompletionException;
import java.util.concurrent.ExecutionException;

import org.folio.rest.core.models.RequestEntry;
import org.folio.rest.core.RestClient;
import io.vertx.junit5.VertxExtension;
import org.folio.service.finance.FundService;
import org.folio.rest.acq.model.finance.FiscalYear;
import org.folio.service.finance.FiscalYearService;
import static org.mockito.Mockito.mock;
import static org.mockito.Mockito.when;

import org.folio.rest.core.exceptions.HttpException;
import org.folio.rest.core.models.RequestContext;
import org.folio.rest.tools.client.interfaces.HttpClientInterface;
import org.junit.jupiter.api.BeforeEach;
import org.junit.jupiter.api.Test;
import org.mockito.MockitoAnnotations;
import org.junit.jupiter.api.extension.ExtendWith;
import org.mockito.*;

import io.restassured.http.Header;
import io.vertx.core.Context;
import io.vertx.core.Future;
import io.vertx.core.Vertx;
@ExtendWith(VertxExtension.class)
public class FiscalYearServiceTest {

  public static final String TENANT_ID = "ordertest";
  public static final Header X_OKAPI_TENANT = new Header(OKAPI_HEADER_TENANT, TENANT_ID);

  private Context ctxMock;
  private Map<String, String> okapiHeadersMock;
  private HttpClientInterface httpClient;

  @InjectMocks
  private FiscalYearService fiscalYearService;
  @Spy
  private RestClient restClientMock;
  @Mock
  private RequestEntry requestEntry;
  @Mock
  private FundService fundServiceMock;
  @Mock
  private RequestContext requestContextMock;

  @BeforeEach
  public void initMocks() {
    ctxMock = Vertx.vertx().getOrCreateContext();
    okapiHeadersMock = new HashMap<>();
    okapiHeadersMock.put(OKAPI_URL, "http://localhost:" + mockPort);
    okapiHeadersMock.put(X_OKAPI_TOKEN.getName(), X_OKAPI_TOKEN.getValue());
    okapiHeadersMock.put(X_OKAPI_TENANT.getName(), X_OKAPI_TENANT.getValue());
    okapiHeadersMock.put(X_OKAPI_USER_ID.getName(), X_OKAPI_USER_ID.getValue());
    String okapiURL = okapiHeadersMock.getOrDefault(OKAPI_URL, "");
    requestContextMock = new RequestContext(ctxMock, okapiHeadersMock);
    MockitoAnnotations.openMocks(this);
  }

  @Test
  void testShouldReturnCurrentFiscalYearForLedger() {
    String ledgerId = UUID.randomUUID()
      .toString();
    FiscalYear fy = fiscalYearService.getCurrentFiscalYear(ledgerId, requestContextMock)
      .result();
    assertNotNull(fy);
  }

  @Test
  void testShouldThrowHttpException() throws IllegalAccessException, NoSuchFieldException {
    FiscalYear sampleFiscalYear = new FiscalYear();
    FiscalYearService fiscalYearService = new FiscalYearService(restClientMock, fundServiceMock);
    Future<FiscalYear> result = fiscalYearService.getCurrentFiscalYear(ID_DOES_NOT_EXIST, requestContextMock);
    CompletionException expectedException2 = assertThrows(CompletionException.class, result::result);
    HttpException httpException = (HttpException) expectedException2.getCause();
    assertEquals(404, httpException.getCode());
  }
}
