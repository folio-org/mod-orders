package org.folio.rest.core;

import static java.util.concurrent.CompletableFuture.completedFuture;
import static org.folio.TestConstants.X_OKAPI_TOKEN;
import static org.folio.TestConstants.X_OKAPI_USER_ID;
import static org.folio.orders.utils.ResourcePathResolver.PURCHASE_ORDER_STORAGE;
import static org.folio.orders.utils.ResourcePathResolver.resourcesPath;
import static org.folio.rest.RestConstants.OKAPI_URL;
import static org.folio.rest.RestVerticle.OKAPI_HEADER_TENANT;
import static org.hamcrest.CoreMatchers.equalTo;
import static org.hamcrest.MatcherAssert.assertThat;
import static org.junit.jupiter.api.Assertions.assertThrows;
import static org.mockito.ArgumentMatchers.any;
import static org.mockito.ArgumentMatchers.anyString;
import static org.mockito.ArgumentMatchers.eq;
import static org.mockito.Mockito.doReturn;
import static org.mockito.Mockito.verify;

import java.util.HashMap;
import java.util.Map;
import java.util.UUID;
import java.util.concurrent.CompletableFuture;
import java.util.concurrent.CompletionException;
import java.util.concurrent.ExecutionException;

import org.folio.rest.acq.model.finance.Transaction;
import org.folio.rest.core.models.RequestContext;
import org.folio.rest.core.models.RequestEntry;
import org.folio.rest.jaxrs.model.PurchaseOrder;
import org.folio.rest.tools.client.Response;
import org.folio.rest.tools.client.interfaces.HttpClientInterface;
import org.junit.jupiter.api.BeforeEach;
import org.junit.jupiter.api.Test;
import org.mockito.Mock;
import org.mockito.Mockito;
import org.mockito.MockitoAnnotations;

import io.restassured.http.Header;
import io.vertx.core.http.HttpMethod;
import io.vertx.core.impl.EventLoopContext;
import io.vertx.core.json.JsonObject;

public class RestClientTest {
  public static final Header X_OKAPI_TENANT = new Header(OKAPI_HEADER_TENANT, "invoiceimpltest");

  @Mock
  private EventLoopContext ctxMock;
  @Mock
  private HttpClientInterface httpClient;

  private Map<String, String> okapiHeaders;
  private RequestContext requestContext;

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
  void testGetShouldSearchById() throws Exception {
    RestClient restClient = Mockito.spy(new RestClient());
    String uuid = UUID.randomUUID().toString();
    String endpoint = resourcesPath(PURCHASE_ORDER_STORAGE) + "/{id}";
    Transaction expTransaction = new Transaction().withId(uuid);
    Response response = new Response();
    response.setBody(JsonObject.mapFrom(expTransaction));
    response.setCode(200);

    doReturn(httpClient).when(restClient).getHttpClient(okapiHeaders);
    doReturn(completedFuture(response)).when(httpClient).request(eq(HttpMethod.GET), anyString(), eq(okapiHeaders));

    Transaction actTransaction = restClient.getById(endpoint, uuid, requestContext, Transaction.class).join();

    assertThat(actTransaction, equalTo(expTransaction));
  }

  @Test
  void testSearchShouldThrowExceptionIfItemNotFind() throws Exception {
    RestClient restClient = Mockito.spy(new RestClient());

    String uuid = UUID.randomUUID().toString();
    Response response = new Response();
    response.setCode(404);
    response.setError(new JsonObject("{\"endpoint\":\"/composite-orders\",\"statusCode\": 404 , \"errorMessage\":\"Not found\"}"));

    doReturn(httpClient).when(restClient).getHttpClient(okapiHeaders);
    String endpoint = resourcesPath(PURCHASE_ORDER_STORAGE) + "/{id}";
    doReturn(completedFuture(response)).when(httpClient).request(eq(HttpMethod.DELETE), anyString(), eq(okapiHeaders));

    RequestEntry requestEntry = new RequestEntry(endpoint).withId(uuid);
    CompletableFuture<PurchaseOrder> resultFuture = restClient.get(requestEntry, false,  requestContext, PurchaseOrder.class);

    ExecutionException executionException = assertThrows(ExecutionException.class, resultFuture::get);
    verify(httpClient).request(eq(HttpMethod.GET), eq(requestEntry.buildEndpoint()), eq(okapiHeaders));
  }

  @Test
  void testSearchShouldNotThrowExceptionIfItemNotFind() throws Exception {
    RestClient restClient = Mockito.spy(new RestClient());

    String uuid = UUID.randomUUID().toString();
    Response response = new Response();
    response.setCode(404);
    response.setError(new JsonObject("{\"endpoint\":\"/composite-orders\",\"statusCode\": 404 , \"errorMessage\":\"Not found\"}"));

    doReturn(httpClient).when(restClient).getHttpClient(okapiHeaders);
    String endpoint = resourcesPath(PURCHASE_ORDER_STORAGE) + "/{id}";
    doReturn(completedFuture(response)).when(httpClient).request(eq(HttpMethod.GET), anyString(), eq(okapiHeaders));

    RequestEntry requestEntry = new RequestEntry(endpoint).withId(uuid);
    restClient.get(requestEntry, true,  requestContext, PurchaseOrder.class).get();

    verify(httpClient).request(eq(HttpMethod.GET), eq(requestEntry.buildEndpoint()), eq(okapiHeaders));
  }

  @Test
  void testGetShouldThrowExceptionWhenSearchById() {
    RestClient restClient = Mockito.spy(new RestClient());
    String uuid = UUID.randomUUID().toString();
    doReturn(httpClient).when(restClient).getHttpClient(okapiHeaders);
    CompletableFuture<Transaction> result = restClient.getById(resourcesPath(PURCHASE_ORDER_STORAGE), uuid, requestContext, Transaction.class);
    assertThrows(CompletionException.class, result::join);
  }

  @Test
  void testPostShouldCreateEntity() throws Exception {
    RestClient restClient = Mockito.spy(new RestClient());

    String uuid = UUID.randomUUID().toString();
    Transaction expTransaction = new Transaction().withId(uuid);
    Response response = new Response();
    response.setBody(JsonObject.mapFrom(expTransaction));
    response.setCode(201);

    doReturn(httpClient).when(restClient).getHttpClient(okapiHeaders);
    doReturn(completedFuture(response)).when(httpClient).request(eq(HttpMethod.POST), any(), eq(resourcesPath(
        PURCHASE_ORDER_STORAGE)), eq(okapiHeaders));
    RequestEntry requestEntry = new RequestEntry(resourcesPath(PURCHASE_ORDER_STORAGE));
    Transaction actTransaction = restClient.post(requestEntry, expTransaction, requestContext, Transaction.class).join();

    assertThat(actTransaction, equalTo(expTransaction));
  }

  @Test
  void testPutShouldCreateEntity() throws Exception {
    RestClient restClient = Mockito.spy(new RestClient());

    String uuid = UUID.randomUUID().toString();
    Transaction expTransaction = new Transaction().withId(uuid);
    Response response = new Response();
    response.setCode(204);

    doReturn(httpClient).when(restClient).getHttpClient(okapiHeaders);
    String endpoint = resourcesPath(PURCHASE_ORDER_STORAGE) + "/{id}";
    RequestEntry requestEntry = new RequestEntry(endpoint).withId(uuid);
    doReturn(completedFuture(response)).when(httpClient).request(eq(HttpMethod.PUT), any(), eq(requestEntry.buildEndpoint()), eq(okapiHeaders));

    restClient.put(requestEntry, expTransaction, requestContext).get();

    verify(httpClient).request(eq(HttpMethod.PUT), any(), eq(requestEntry.buildEndpoint()), eq(okapiHeaders));
  }

  @Test
  void testPatchShouldCreateEntity() throws Exception {
    RestClient restClient = Mockito.spy(new RestClient());

    String uuid = UUID.randomUUID().toString();
    Transaction expTransaction = new Transaction().withId(uuid);
    Response response = new Response();
    response.setCode(204);

    doReturn(httpClient).when(restClient).getHttpClient(okapiHeaders);
    String endpoint = resourcesPath(PURCHASE_ORDER_STORAGE) + "/{id}";
    RequestEntry requestEntry = new RequestEntry(endpoint).withId(uuid);
    doReturn(completedFuture(response)).when(httpClient).request(eq(HttpMethod.PATCH), any(), eq(requestEntry.buildEndpoint()), eq(okapiHeaders));

    restClient.patch(requestEntry, expTransaction, requestContext).get();

    verify(httpClient).request(eq(HttpMethod.PATCH), any(), eq(requestEntry.buildEndpoint()), eq(okapiHeaders));
  }

  @Test
  void testDeleteShouldDeleteEntity() throws Exception {
    RestClient restClient = Mockito.spy(new RestClient());

    String uuid = UUID.randomUUID().toString();
    Response response = new Response();
    response.setCode(204);

    doReturn(httpClient).when(restClient).getHttpClient(okapiHeaders);
    String endpoint = resourcesPath(PURCHASE_ORDER_STORAGE) + "/{id}";
    doReturn(completedFuture(response)).when(httpClient).request(eq(HttpMethod.DELETE), anyString(), eq(okapiHeaders));

    RequestEntry requestEntry = new RequestEntry(endpoint).withId(uuid);
    restClient.delete(requestEntry, requestContext).get();

    verify(httpClient).request(eq(HttpMethod.DELETE), eq(requestEntry.buildEndpoint()), eq(okapiHeaders));
  }

  @Test
  void testDeleteShouldNotThrowExceptionIfItemNotFind() throws Exception {
    RestClient restClient = Mockito.spy(new RestClient());

    String uuid = UUID.randomUUID().toString();
    Response response = new Response();
    response.setCode(404);

    doReturn(httpClient).when(restClient).getHttpClient(okapiHeaders);
    String endpoint = resourcesPath(PURCHASE_ORDER_STORAGE) + "/{id}";
    doReturn(completedFuture(response)).when(httpClient).request(eq(HttpMethod.DELETE), anyString(), eq(okapiHeaders));

    RequestEntry requestEntry = new RequestEntry(endpoint).withId(uuid);
    restClient.delete(requestEntry, true, requestContext).get();

    verify(httpClient).request(eq(HttpMethod.DELETE), eq(requestEntry.buildEndpoint()), eq(okapiHeaders));
  }

  @Test
  void testDeleteShouldThrowExceptionIfItemNotFind() throws Exception {
    RestClient restClient = Mockito.spy(new RestClient());

    String uuid = UUID.randomUUID()
      .toString();
    Response response = new Response();
    response.setCode(404);
    response.setError(new JsonObject("{\"endpoint\":\"/composite-orders\",\"statusCode\": 404 , \"errorMessage\":\"Not found\"}"));

    doReturn(httpClient).when(restClient)
      .getHttpClient(okapiHeaders);
    String endpoint = resourcesPath(PURCHASE_ORDER_STORAGE) + "/{id}";
    doReturn(completedFuture(response)).when(httpClient)
      .request(eq(HttpMethod.DELETE), anyString(), eq(okapiHeaders));

    RequestEntry requestEntry = new RequestEntry(endpoint).withId(uuid);
    CompletableFuture<Void> resultFuture = restClient.delete(requestEntry, false, requestContext);

    ExecutionException executionException = assertThrows(ExecutionException.class, resultFuture::get);
    verify(httpClient).request(eq(HttpMethod.DELETE), eq(requestEntry.buildEndpoint()), eq(okapiHeaders));
  }

  @Test
  void testShouldThrowExceptionWhenCreatingEntity() {
    assertThrows(CompletionException.class, () -> {

      RestClient restClient = Mockito.spy(new RestClient());

      String uuid = UUID.randomUUID().toString();
      Transaction expTransaction = new Transaction().withId(uuid);
      doReturn(httpClient).when(restClient).getHttpClient(okapiHeaders);
      RequestEntry requestEntry = new RequestEntry(resourcesPath(PURCHASE_ORDER_STORAGE));
      restClient.post(requestEntry, expTransaction, requestContext, Transaction.class).join();
    });
  }
}


