package org.folio.rest.core;

import static javax.ws.rs.core.HttpHeaders.LOCATION;
import static javax.ws.rs.core.MediaType.APPLICATION_JSON;
import static javax.ws.rs.core.MediaType.TEXT_PLAIN;
import static org.folio.rest.RestConstants.ID;
import static org.folio.rest.RestConstants.OKAPI_TENANT_LOWCASE;
import static org.folio.rest.RestVerticle.OKAPI_HEADER_TENANT;

import java.util.HashMap;
import java.util.Map;

import org.apache.logging.log4j.LogManager;
import org.apache.logging.log4j.Logger;
import org.folio.okapi.common.WebClientFactory;
import org.folio.rest.core.exceptions.HttpException;
import org.folio.rest.core.models.RequestContext;
import org.folio.rest.core.models.RequestEntry;

import io.vertx.core.Context;
import io.vertx.core.Future;
import io.vertx.core.MultiMap;
import io.vertx.core.buffer.Buffer;
import io.vertx.core.http.HttpMethod;
import io.vertx.core.json.JsonObject;
import io.vertx.ext.web.client.HttpResponse;
import io.vertx.ext.web.client.WebClient;
import io.vertx.ext.web.client.predicate.ErrorConverter;
import io.vertx.ext.web.client.predicate.ResponsePredicate;

public class RestClient {

  private static final Logger logger = LogManager.getLogger(RestClient.class);

  public static final String X_OKAPI_URL = "X-Okapi-Url";
  private static final String CALLING_ENDPOINT_MSG = "Sending {} {}";
  private static final ErrorConverter ERROR_CONVERTER = ErrorConverter.createFullBody(
    result -> new HttpException(result.response().statusCode(), result.response().bodyAsString()));
  private static final ResponsePredicate SUCCESS_RESPONSE_PREDICATE =
    ResponsePredicate.create(ResponsePredicate.SC_SUCCESS, ERROR_CONVERTER);


  public <T> Future<T> getById(String baseEndpoint, String id, boolean skip404Error, Class<T> responseType, RequestContext requestContext) {
    RequestEntry requestEntry = new RequestEntry(baseEndpoint).withPathParameter("id", id);
    return get(requestEntry, skip404Error, responseType, requestContext);
  }

  public <T> Future<T> getById(String baseEndpoint, String id, Class<T> responseType, RequestContext requestContext) {
    return getById(baseEndpoint, id, false, responseType, requestContext);
  }

  public <T> Future<T> post(RequestEntry requestEntry, T entity, Class<T> responseType, RequestContext requestContext) {
    return post(requestEntry.buildEndpoint(), entity, responseType, requestContext);
  }

  public <T> Future<T> post(String endpoint, T entity, Class<T> responseType, RequestContext requestContext) {
    if (logger.isDebugEnabled()) {
      logger.debug("Sending 'POST {}' with body: {}", endpoint, JsonObject.mapFrom(entity).encodePrettily());
    }
    return getVertxWebClient(requestContext.getContext()).postAbs(buildAbsEndpoint(requestContext.getHeaders(), endpoint))
      .putHeaders(convertToMultiMap(requestContext.getHeaders()))
      .expect(SUCCESS_RESPONSE_PREDICATE)
      .sendJson(entity)
      .map(bufferHttpResponse -> {
        var id = extractRecordId(bufferHttpResponse);
        return bufferHttpResponse.bodyAsJsonObject()
          .put(ID, id)
          .mapTo(responseType);
      });
  }

  private MultiMap convertToMultiMap(Map<String, String> okapiHeaders) {
    return MultiMap.caseInsensitiveMultiMap().addAll(okapiHeaders);
  }

  public <T> Future<Void> put(RequestEntry requestEntry, T dataObject, RequestContext requestContext) {
    return put(requestEntry.buildEndpoint(), dataObject, requestContext);
  }
  public <T> Future<Void> put(String endpoint, T dataObject, RequestContext requestContext) {
    var recordData = JsonObject.mapFrom(dataObject);
    if (logger.isDebugEnabled()) {
      logger.debug("Sending 'PUT {}' with body: {}", endpoint, recordData.encodePrettily());
    }
    return getVertxWebClient(requestContext.getContext())
      .putAbs(buildAbsEndpoint(requestContext.getHeaders(), endpoint))
      .putHeaders(convertToMultiMap(requestContext.getHeaders()))
      .expect(SUCCESS_RESPONSE_PREDICATE).sendJsonObject(recordData)
      .mapEmpty();
  }

  public <T>Future<Void> patch(RequestEntry requestEntry, T dataObject, RequestContext requestContext ) {
    String endpoint = requestEntry.buildEndpoint();
    return patch(endpoint, dataObject, requestContext);
  }

  public <T>Future<Void> patch(String endpoint, T dataObject, RequestContext requestContext) {
    var recordData = JsonObject.mapFrom(dataObject);
    if (logger.isDebugEnabled()) {
      logger.debug("Sending 'PATCH {}' with body: {}", endpoint, recordData.encodePrettily());
    }
    return getVertxWebClient(requestContext.getContext())
      .patch(buildAbsEndpoint(requestContext.getHeaders(), endpoint))
      .putHeaders(convertToMultiMap(requestContext.getHeaders()))
      .expect(SUCCESS_RESPONSE_PREDICATE).sendJsonObject(recordData)
      .mapEmpty();
  }


  public Future<Void> delete(RequestEntry requestEntry, boolean skip404Error, RequestContext requestContext) {
    return delete(requestEntry.buildEndpoint(), skip404Error, requestContext);
  }

  public Future<Void> delete(String endpointById, boolean skip404Error, RequestContext requestContext) {
    logger.debug(CALLING_ENDPOINT_MSG, HttpMethod.DELETE, endpointById);

    return getVertxWebClient(requestContext.getContext())
      .deleteAbs(buildAbsEndpoint(requestContext.getHeaders(), endpointById))
      .putHeaders(convertToMultiMap(requestContext.getHeaders()))
      .putHeader("Accept", APPLICATION_JSON + ", " + TEXT_PLAIN)
      .expect(SUCCESS_RESPONSE_PREDICATE).send()
      .mapEmpty();
  }

  public Future<Void> delete(String endpoint, RequestContext requestContext) {
    return delete(endpoint, false, requestContext);
  }

  public Future<Void> delete(RequestEntry requestEntry, RequestContext requestContext) {
    return delete(requestEntry.buildEndpoint(), false, requestContext);
  }

  public <T> Future<T> get(RequestEntry requestEntry, boolean skip404Error, Class<T> responseType, RequestContext requestContext) {
    return get(requestEntry.buildEndpoint(), responseType, requestContext);
  }

  public <T> Future<T> get(RequestEntry requestEntry, Class<T> responseType, RequestContext requestContext) {
    return get(requestEntry, false, responseType, requestContext);
  }

  public <T> Future<T> get(String endpoint, Class<T> responseType, RequestContext requestContext) {
    logger.debug("Calling GET {}", endpoint);
    return getVertxWebClient(requestContext.getContext())
      .getAbs(buildAbsEndpoint(requestContext.getHeaders(), endpoint))
      .putHeaders(convertToMultiMap(requestContext.getHeaders()))
      .expect(SUCCESS_RESPONSE_PREDICATE).send()
      .map(HttpResponse::bodyAsJsonObject)
      .map(jsonObject -> jsonObject.mapTo(responseType));
  }


  public Future<JsonObject> getAsJsonObject(String endpoint, boolean skip404Error, RequestContext requestContext) {
    logger.debug("Calling GET {}", endpoint);
    return getVertxWebClient(requestContext.getContext())
      .getAbs(buildAbsEndpoint(requestContext.getHeaders(), endpoint))
      .putHeaders(convertToMultiMap(requestContext.getHeaders()))
      .expect(SUCCESS_RESPONSE_PREDICATE).send()
      .map(HttpResponse::bodyAsJsonObject);
  }

  public Future<JsonObject> getAsJsonObject(RequestEntry requestEntry, boolean skip404Error, RequestContext requestContext) {
    return getVertxWebClient(requestContext.getContext())
      .getAbs(buildAbsEndpoint(requestContext.getHeaders(), requestEntry.buildEndpoint()))
      .putHeaders(convertToMultiMap(requestContext.getHeaders()))
      .expect(SUCCESS_RESPONSE_PREDICATE).send()
      .map(HttpResponse::bodyAsJsonObject);
  }

  public Future<JsonObject> getAsJsonObject(RequestEntry requestEntry, RequestContext requestContext) {
   return getAsJsonObject(requestEntry.buildEndpoint(), false, requestContext);
  }

  private Map<String, String> fixHeadersForRMB34(Map<String, String> requestHeaders) {
    Map<String, String> headers = new HashMap<>(requestHeaders);
    if (headers.get(OKAPI_TENANT_LOWCASE) != null) {
      String tenantId = headers.get(OKAPI_TENANT_LOWCASE);
      headers.remove(OKAPI_TENANT_LOWCASE);
      headers.put(OKAPI_HEADER_TENANT, tenantId);
    }
    return headers;
  }


  public String extractRecordId(HttpResponse<Buffer> response) {
    JsonObject body = response.bodyAsJsonObject();
    String id;
    if (body != null && !body.isEmpty() && body.containsKey(ID)) {
      id = body.getString(ID);
    } else {
      String location = response.getHeader(LOCATION);
      id = location.substring(location.lastIndexOf('/') + 1);
    }
    return id;
  }

  private WebClient getVertxWebClient(Context context) {
    // TODO: get or create with WebClientOptions
    /*
     * WebClientOptions options = new WebClientOptions(); options.setLogActivity(true); options.setKeepAlive(keepAlive);
     * options.setConnectTimeout(connTO); options.setIdleTimeout(idleTO);
     */
    return WebClientFactory.getWebClient(context.owner());
  }
  private static String buildAbsEndpoint(Map<String, String> okapiHeaders, String endpoint) {
    var okapiURL = okapiHeaders.getOrDefault(X_OKAPI_URL, "");
    return okapiURL + endpoint;
  }

  public Future<String> postAndGetId(RequestEntry requestEntry, JsonObject entity, RequestContext requestContext) {
    if (logger.isDebugEnabled()) {
      logger.debug("Sending 'POST {}' with body: {}", requestEntry.buildEndpoint(), JsonObject.mapFrom(entity).encodePrettily());
    }
    return getVertxWebClient(requestContext.getContext())
      .postAbs(buildAbsEndpoint(requestContext.getHeaders(), requestEntry.buildEndpoint()))
      .putHeaders(convertToMultiMap(requestContext.getHeaders()))
      .expect(SUCCESS_RESPONSE_PREDICATE)
      .sendJsonObject(entity)
      .map(this::extractRecordId);
  }
}
