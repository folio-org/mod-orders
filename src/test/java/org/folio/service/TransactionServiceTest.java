package org.folio.service;

import static java.util.concurrent.CompletableFuture.completedFuture;
import static org.folio.orders.utils.HelperUtils.OKAPI_URL;
import static org.folio.rest.RestVerticle.OKAPI_HEADER_TENANT;
import static org.folio.rest.impl.MockServer.ENCUMBRANCE_PATH;
import static org.folio.service.finance.TransactionService.TRANSACTION_STORAGE_ENDPOINT_BYID;
import static org.junit.jupiter.api.Assertions.assertEquals;
import static org.mockito.ArgumentMatchers.any;
import static org.mockito.ArgumentMatchers.eq;
import static org.mockito.Mockito.doReturn;
import static org.mockito.Mockito.spy;
import static org.mockito.Mockito.verify;

import java.util.HashMap;
import java.util.Map;

import org.folio.rest.acq.model.finance.Transaction;
import org.folio.rest.core.models.RequestContext;
import org.folio.rest.impl.ApiTestBase;
import org.folio.rest.tools.client.Response;
import org.folio.rest.tools.client.interfaces.HttpClientInterface;
import org.folio.service.finance.TransactionService;
import org.junit.jupiter.api.BeforeEach;
import org.junit.jupiter.api.Test;
import org.mockito.Mock;
import org.mockito.MockitoAnnotations;

import io.restassured.http.Header;
import io.vertx.core.http.HttpMethod;
import io.vertx.core.impl.EventLoopContext;

public class TransactionServiceTest extends ApiTestBase {
  @Mock
  private EventLoopContext ctxMock;
  @Mock
  private HttpClientInterface httpClient;

  private RequestContext requestContext;
  private Map<String, String> okapiHeaders;
  public static final Header X_OKAPI_TENANT = new Header(OKAPI_HEADER_TENANT, "invoiceimpltest");

  @BeforeEach
  public void initMocks(){
    MockitoAnnotations.openMocks(this);
    okapiHeaders = new HashMap<>();
    okapiHeaders.put(OKAPI_URL, "http://localhost:" + 8081);
    okapiHeaders.put(X_OKAPI_TOKEN.getName(), X_OKAPI_TOKEN.getValue());
    okapiHeaders.put(X_OKAPI_TENANT.getName(), X_OKAPI_TENANT.getValue());
    okapiHeaders.put(X_OKAPI_USER_ID.getName(), X_OKAPI_USER_ID.getValue());


    requestContext = new RequestContext(ctxMock, okapiHeaders);
  }

  @Test
  void testShouldInvokeUpdateTransaction() throws Exception {
    //given
    TransactionService service = spy(new TransactionService(transactionRestClient));
    Transaction encumbrance = getMockAsJson(ENCUMBRANCE_PATH).getJsonArray("transactions").getJsonObject(0).mapTo(Transaction.class);
    String endpoint = String.format(TRANSACTION_STORAGE_ENDPOINT_BYID, encumbrance.getId(), "en");
    Response response = new Response();
    response.setCode(204);
    doReturn(completedFuture(response)).when(httpClient).request(eq(HttpMethod.PUT), any(), eq(endpoint), any());
    //When
    service.updateTransaction(encumbrance, requestContext).get();
    //Then
    verify(httpClient).request(eq(HttpMethod.PUT), any(), eq(endpoint), any());
  }
}
