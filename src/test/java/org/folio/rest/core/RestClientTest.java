package org.folio.rest.core;

import static org.folio.TestConstants.X_OKAPI_TOKEN;
import static org.folio.TestConstants.X_OKAPI_USER_ID;
import static org.folio.rest.RestConstants.OKAPI_URL;
import static org.folio.rest.RestVerticle.OKAPI_HEADER_TENANT;
import static org.folio.rest.core.exceptions.ErrorCodes.GENERIC_ERROR_CODE;
import static org.junit.jupiter.api.Assertions.assertEquals;
import static org.junit.jupiter.api.Assertions.assertInstanceOf;
import static org.junit.jupiter.api.Assertions.assertTrue;
import static org.mockito.Mockito.when;

import java.util.HashMap;
import java.util.Map;

import io.vertx.core.Context;
import io.vertx.core.Future;
import io.vertx.core.buffer.Buffer;
import io.vertx.ext.web.client.HttpResponse;
import org.folio.rest.core.exceptions.HttpException;
import org.folio.rest.core.models.RequestContext;
import org.junit.jupiter.api.BeforeEach;
import org.junit.jupiter.api.Test;
import org.mockito.Mock;
import org.mockito.MockitoAnnotations;

import io.restassured.http.Header;
import io.vertx.ext.web.client.WebClient;

public class RestClientTest {
  public static final Header X_OKAPI_TENANT = new Header(OKAPI_HEADER_TENANT, "invoiceimpltest");

  @Mock
  private Context ctxMock;
  @Mock
  private WebClient httpClient;
  @Mock
  private HttpResponse<Buffer> bufferResponseMock;

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
  void convertHttpResponseShouldSucceedForSuccessStatus() {
    when(bufferResponseMock.statusCode()).thenReturn(200);

    Future<HttpResponse<Buffer>> result = RestClient.convertHttpResponse(bufferResponseMock);

    assertTrue(result.succeeded());
    assertEquals(bufferResponseMock, result.result());
  }

  @Test
  void convertHttpResponseShouldParseJsonErrorsBody() {
    String errorJson = """
      {
        "errors" : [ {
          "message" : "Cannot convert UAH into USD",
          "code" : "cannotConvertAmountInvalidCurrency",
          "parameters" : [ ]
        } ],
        "total_records" : 1
      }""";
    when(bufferResponseMock.statusCode()).thenReturn(404);
    when(bufferResponseMock.bodyAsString()).thenReturn(errorJson);

    Future<HttpResponse<Buffer>> result = RestClient.convertHttpResponse(bufferResponseMock);

    assertTrue(result.failed());
    HttpException ex = assertInstanceOf(HttpException.class, result.cause());
    assertEquals(404, ex.getCode());
    assertEquals("cannotConvertAmountInvalidCurrency", ex.getError().getCode());
    assertEquals("Cannot convert UAH into USD", ex.getError().getMessage());
  }

  @Test
  void convertHttpResponseShouldFallbackToGenericErrorForPlainTextBody() {
    when(bufferResponseMock.statusCode()).thenReturn(500);
    when(bufferResponseMock.bodyAsString()).thenReturn("Module failure");

    Future<HttpResponse<Buffer>> result = RestClient.convertHttpResponse(bufferResponseMock);

    assertTrue(result.failed());
    HttpException ex = assertInstanceOf(HttpException.class, result.cause());
    assertEquals(500, ex.getCode());
    assertEquals(GENERIC_ERROR_CODE.getCode(), ex.getError().getCode());
    assertEquals("Module failure", ex.getError().getMessage());
  }

}
