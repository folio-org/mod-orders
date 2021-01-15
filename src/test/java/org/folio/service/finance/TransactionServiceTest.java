package org.folio.service.finance;

import static java.util.concurrent.CompletableFuture.completedFuture;
import static org.folio.orders.utils.ErrorCodes.MISMATCH_BETWEEN_ID_IN_PATH_AND_BODY;
import static org.folio.orders.utils.HelperUtils.OKAPI_URL;
import static org.folio.orders.utils.ResourcePathResolver.ORDER_TRANSACTION_SUMMARIES;
import static org.folio.orders.utils.ResourcePathResolver.resourceByIdPath;
import static org.folio.orders.utils.ResourcePathResolver.resourcesPath;
import static org.folio.rest.RestVerticle.OKAPI_HEADER_TENANT;
import static org.folio.rest.impl.MockServer.ENCUMBRANCE_PATH;
import static org.folio.service.finance.TransactionService.TRANSACTION_STORAGE_ENDPOINT_BYID;
import static org.junit.jupiter.api.Assertions.assertEquals;
import static org.junit.jupiter.api.Assertions.assertThrows;
import static org.mockito.ArgumentMatchers.any;
import static org.mockito.ArgumentMatchers.eq;
import static org.mockito.Mockito.doReturn;
import static org.mockito.Mockito.spy;
import static org.mockito.Mockito.verify;

import java.util.HashMap;
import java.util.Map;
import java.util.UUID;
import java.util.concurrent.CompletableFuture;
import java.util.concurrent.CompletionException;

import org.folio.orders.rest.exceptions.HttpException;
import org.folio.rest.acq.model.finance.OrderTransactionSummary;
import org.folio.rest.acq.model.finance.Transaction;
import org.folio.rest.impl.ApiTestBase;
import org.folio.rest.jaxrs.model.Error;
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
import io.vertx.core.json.JsonObject;

public class TransactionServiceTest extends ApiTestBase {
  @Mock
  private EventLoopContext ctxMock;
  @Mock
  private HttpClientInterface httpClient;

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
  }

  @Test
  public void testShouldCreateTransactionSummaryInStorageTransactions() throws Exception {
    //given
    TransactionService service = spy(new TransactionService(httpClient, okapiHeaders, ctxMock, "en"));
    String uuid = UUID.randomUUID().toString();
    Response response = new Response();
    response.setBody(new JsonObject("{\"id\": \""+uuid+"\"}"));
    response.setCode(201);
    doReturn(completedFuture(response)).when(httpClient).request(eq(HttpMethod.POST), any(), eq(resourcesPath(ORDER_TRANSACTION_SUMMARIES)), any());
    //When
    String summaryId= service.createOrderTransactionSummary(uuid, 2).join();
    //Then
    assertEquals(uuid, summaryId);
    verify(httpClient).request(eq(HttpMethod.POST), any(), eq(resourcesPath(ORDER_TRANSACTION_SUMMARIES)), any());
  }

  @Test
  public void testShouldSuccessfullyGetOrderTransactionSummary() throws Exception {
    //given
    TransactionService service = spy(new TransactionService(httpClient, okapiHeaders, ctxMock, "en"));
    String orderId = UUID.randomUUID().toString();
    Response response = new Response();
    response.setBody(new JsonObject("{\"id\": \""+orderId+"\", \"numTransactions\": \"1\"}"));
    response.setCode(200);
    doReturn(completedFuture(response)).when(httpClient).request(eq(HttpMethod.GET), eq(resourceByIdPath(ORDER_TRANSACTION_SUMMARIES, orderId)), any());

    //When
    OrderTransactionSummary summary = service.getOrderTransactionSummary(orderId).join();
    //Then
    assertEquals(orderId, summary.getId());
    assertEquals(1, summary.getNumTransactions());
    verify(httpClient).request(eq(HttpMethod.GET), eq(resourceByIdPath(ORDER_TRANSACTION_SUMMARIES, orderId)), any());
  }

  @Test
  public void testShouldCompleteExceptionallyIfResponseFromHttpClientNotSuccess() throws Exception {
    //given
    TransactionService service = spy(new TransactionService(httpClient, okapiHeaders, ctxMock, "en"));
    String orderId = UUID.randomUUID().toString();
    Response response = new Response();
    response.setError(JsonObject.mapFrom(new Error().withCode("404").withMessage("Test")));
    response.setCode(404);
    doReturn(completedFuture(response)).when(httpClient).request(eq(HttpMethod.GET), eq(resourceByIdPath(ORDER_TRANSACTION_SUMMARIES, orderId)), any());

    //When
    CompletableFuture<OrderTransactionSummary> result = service.getOrderTransactionSummary(orderId);
    //Then
    CompletionException expectedException = assertThrows(CompletionException.class, result::join);
    HttpException httpException = (HttpException) expectedException.getCause();
    assertEquals(404, httpException.getCode());
    verify(httpClient).request(eq(HttpMethod.GET), eq(resourceByIdPath(ORDER_TRANSACTION_SUMMARIES, orderId)), any());
  }

  @Test
  public void testShouldInvokeUpdateTransaction() throws Exception {
    //given
    TransactionService service = spy(new TransactionService(httpClient, okapiHeaders, ctxMock, "en"));
    Transaction encumbrance = getMockAsJson(ENCUMBRANCE_PATH).getJsonArray("transactions").getJsonObject(0).mapTo(Transaction.class);
    String endpoint = String.format(TRANSACTION_STORAGE_ENDPOINT_BYID, encumbrance.getId(), "en");
    Response response = new Response();
    response.setCode(204);
    doReturn(completedFuture(response)).when(httpClient).request(eq(HttpMethod.PUT), any(), eq(endpoint), any());
    //When
    service.updateTransaction(encumbrance).get();
    //Then
    verify(httpClient).request(eq(HttpMethod.PUT), any(), eq(endpoint), any());
  }
}
