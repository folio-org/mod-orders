package org.folio.rest.core;

import static javax.ws.rs.core.HttpHeaders.LOCATION;
import static javax.ws.rs.core.MediaType.APPLICATION_JSON;
import static javax.ws.rs.core.MediaType.TEXT_PLAIN;
import static org.folio.rest.RestConstants.ID;
import static org.folio.rest.RestConstants.OKAPI_URL;
import static org.folio.rest.core.exceptions.ExceptionUtil.getHttpException;

import java.util.Map;

import io.vertx.core.Context;
import io.vertx.core.Future;
import io.vertx.core.MultiMap;
import io.vertx.core.Promise;
import io.vertx.core.buffer.Buffer;
import io.vertx.core.json.JsonObject;
import io.vertx.ext.web.client.HttpResponse;
import io.vertx.ext.web.client.WebClient;
import io.vertx.ext.web.client.WebClientOptions;
import io.vertx.ext.web.client.predicate.ErrorConverter;
import io.vertx.ext.web.client.predicate.ResponsePredicate;
import org.apache.logging.log4j.LogManager;
import org.apache.logging.log4j.Logger;
import org.folio.okapi.common.WebClientFactory;
import org.folio.rest.core.exceptions.HttpException;
import org.folio.rest.core.models.RequestContext;
import org.folio.rest.core.models.RequestEntry;

public class RestClient {

  private static final Logger log = LogManager.getLogger(RestClient.class);

  private static final ErrorConverter ERROR_CONVERTER = ErrorConverter.createFullBody(
    result -> {
      String error = result.response().bodyAsString();
      return getHttpException(result.response().statusCode(), error);
    });

  private static final ResponsePredicate SUCCESS_RESPONSE_PREDICATE =
    ResponsePredicate.create(ResponsePredicate.SC_SUCCESS, ERROR_CONVERTER);

  public <T> Future<T> post(RequestEntry requestEntry, T entity, Class<T> responseType, RequestContext requestContext) {
    return post(requestEntry.buildEndpoint(), entity, responseType, requestContext);
  }

  public <T> Future<T> post(String endpoint, T entity, Class<T> responseType, RequestContext requestContext) {
    var caseInsensitiveHeader = convertToCaseInsensitiveMap(requestContext.getHeaders());
    return getVertxWebClient(requestContext.getContext()).postAbs(buildAbsEndpoint(caseInsensitiveHeader, endpoint))
      .putHeaders(caseInsensitiveHeader)
      .expect(SUCCESS_RESPONSE_PREDICATE)
      .sendJson(entity)
      .map(bufferHttpResponse -> {
        var id = extractRecordId(bufferHttpResponse);
        return bufferHttpResponse.bodyAsJsonObject()
          .put(ID, id)
          .mapTo(responseType);
      })
      .onFailure(log::error);
  }

  public <T> Future<T> postBatch(RequestEntry requestEntry, T entity, Class<T> responseType, RequestContext requestContext) {
    return postBatch(requestEntry.buildEndpoint(), entity, responseType, requestContext);
  }

  public <T> Future<T> postBatch(String endpoint, T entity, Class<T> responseType, RequestContext requestContext) {
    var caseInsensitiveHeader = convertToCaseInsensitiveMap(requestContext.getHeaders());
    return getVertxWebClient(requestContext.getContext()).postAbs(buildAbsEndpoint(caseInsensitiveHeader, endpoint))
      .putHeaders(caseInsensitiveHeader)
      .sendJson(entity)
      .compose(response -> {
        if (response.statusCode() >= 200 && response.statusCode() < 300) {
          return Future.succeededFuture(response);
        } else {
          String error = response.bodyAsString();
          return Future.failedFuture(getHttpException(response.statusCode(), error));
        }
      })
      .map(bufferHttpResponse -> bufferHttpResponse.bodyAsJsonObject().mapTo(responseType))
      .onFailure(log::error);
  }

  public <T> Future<Void> postEmptyResponse(String endpoint, T entity, RequestContext requestContext) {
    var caseInsensitiveHeader = convertToCaseInsensitiveMap(requestContext.getHeaders());
    return getVertxWebClient(requestContext.getContext())
      .postAbs(buildAbsEndpoint(caseInsensitiveHeader, endpoint))
      .putHeaders(caseInsensitiveHeader)
      .expect(SUCCESS_RESPONSE_PREDICATE)
      .sendJson(entity)
      .onFailure(log::error)
      .mapEmpty();
  }

  private MultiMap convertToCaseInsensitiveMap(Map<String, String> okapiHeaders) {
    return MultiMap.caseInsensitiveMultiMap()
      .addAll(okapiHeaders)
      // set default Accept header
      .add("Accept", APPLICATION_JSON + ", " + TEXT_PLAIN);
  }

  public <T> Future<Void> put(RequestEntry requestEntry, T dataObject, RequestContext requestContext) {
    return put(requestEntry.buildEndpoint(), dataObject, requestContext);
  }
  public <T> Future<Void> put(String endpoint, T dataObject,  RequestContext requestContext) {
    var recordData = JsonObject.mapFrom(dataObject);
    var caseInsensitiveHeader = convertToCaseInsensitiveMap(requestContext.getHeaders());
    return getVertxWebClient(requestContext.getContext())
      .putAbs(buildAbsEndpoint(caseInsensitiveHeader, endpoint))
      .putHeaders(caseInsensitiveHeader)
      .expect(SUCCESS_RESPONSE_PREDICATE)
      .sendJson(recordData)
      .onFailure(log::error)
      .mapEmpty();
  }

  public <T>Future<Void> patch(RequestEntry requestEntry, T dataObject, RequestContext requestContext ) {
    String endpoint = requestEntry.buildEndpoint();
    return patch(endpoint, dataObject, requestContext);
  }

  public <T>Future<Void> patch(String endpoint, T dataObject, RequestContext requestContext) {
    var caseInsensitiveHeader = convertToCaseInsensitiveMap(requestContext.getHeaders());
    Promise<Void> promise = Promise.promise();
    return getVertxWebClient(requestContext.getContext())
      .patchAbs(buildAbsEndpoint(caseInsensitiveHeader, endpoint))
      .putHeaders(caseInsensitiveHeader)
      .expect(SUCCESS_RESPONSE_PREDICATE)
      .sendJson(dataObject)
      .onSuccess(json -> promise.complete())
      .onFailure(log::error)
      .mapEmpty();
  }


  public Future<Void> delete(RequestEntry requestEntry, boolean skipError404, RequestContext requestContext) {
    return delete(requestEntry.buildEndpoint(), skipError404, requestContext);
  }

  public Future<Void> delete(String endpointById, boolean skipError404, RequestContext requestContext) {
    var caseInsensitiveHeader = convertToCaseInsensitiveMap(requestContext.getHeaders());
    Promise<Void> promise = Promise.promise();
    getVertxWebClient(requestContext.getContext())
      .deleteAbs(buildAbsEndpoint(caseInsensitiveHeader, endpointById))
      .putHeaders(caseInsensitiveHeader)
      .expect(SUCCESS_RESPONSE_PREDICATE)
      .send()
      .onSuccess(f -> promise.complete())
      .onFailure(t -> handleErrorResponse(promise, t, skipError404));

      return promise.future();
  }

  private <T>void handleGetMethodErrorResponse(Promise<T> promise, Throwable t, boolean skipError404) {
    if (skipError404 && t instanceof HttpException httpException && httpException.getCode() == 404) {
      log.warn(t);
      promise.complete();
    } else {
      log.error(t);
      promise.fail(t);
    }
  }
  private void handleErrorResponse(Promise<Void> promise, Throwable t, boolean skipError404) {
    if (skipError404 && t instanceof HttpException httpException && httpException.getCode() == 404){
      log.warn(t);
      promise.complete();
    } else {
      log.error(t);
      promise.fail(t);
    }
  }

  public Future<Void> delete(String endpoint, RequestContext requestContext) {
    return delete(endpoint, false, requestContext);
  }

  public Future<Void> delete(RequestEntry requestEntry, RequestContext requestContext) {
    return delete(requestEntry.buildEndpoint(), false, requestContext);
  }

  public <T> Future<T> get(String endpoint, Class<T> responseType, RequestContext requestContext) {
    return get(endpoint, false, responseType, requestContext);
  }

  public <T> Future<T> get(RequestEntry requestEntry, Class<T> responseType, RequestContext requestContext) {
    return get(requestEntry.buildEndpoint(), false, responseType, requestContext);
  }

  public <T> Future<T> get(String endpoint, boolean skipError404, Class<T> responseType,  RequestContext requestContext) {
    var caseInsensitiveHeader = convertToCaseInsensitiveMap(requestContext.getHeaders());
    Promise<T> promise = Promise.promise();
    getVertxWebClient(requestContext.getContext())
      .getAbs(buildAbsEndpoint(caseInsensitiveHeader, endpoint))
      .putHeaders(caseInsensitiveHeader)
      .expect(SUCCESS_RESPONSE_PREDICATE)
      .send()
      .map(HttpResponse::bodyAsJsonObject)
      .map(jsonObject -> jsonObject.mapTo(responseType))
      .onSuccess(promise::complete)
      .onFailure(t -> handleGetMethodErrorResponse(promise, t, skipError404));

    return promise.future();
  }


  public Future<JsonObject> getAsJsonObject(String endpoint, boolean skipError404, RequestContext requestContext) {
    Promise<JsonObject> promise = Promise.promise();
    var caseInsensitiveHeader = convertToCaseInsensitiveMap(requestContext.getHeaders());
    var webClient = getVertxWebClient(requestContext.getContext());
    webClient.getAbs(buildAbsEndpoint(caseInsensitiveHeader, endpoint))
      .putHeaders(caseInsensitiveHeader)
      .expect(SUCCESS_RESPONSE_PREDICATE)
      .send()
      .map(HttpResponse::bodyAsJsonObject)
      .onSuccess(promise::complete)
      .onFailure(t -> handleGetMethodErrorResponse(promise, t, skipError404));
    return promise.future();
  }

  public Future<JsonObject> getAsJsonObject(RequestEntry requestEntry, boolean skipError404, RequestContext requestContext) {
    return getAsJsonObject(requestEntry.buildEndpoint(), skipError404, requestContext);
  }

  public Future<JsonObject> getAsJsonObject(RequestEntry requestEntry, RequestContext requestContext) {
   return getAsJsonObject(requestEntry.buildEndpoint(), false, requestContext);
  }

  public String extractRecordId(HttpResponse<Buffer> response) {
    JsonObject body = response.bodyAsJsonObject();
    String id = "";
    if (body != null && !body.isEmpty() && body.containsKey(ID)) {
      id = body.getString(ID);
    } else {
      String location = response.getHeader(LOCATION);
      if (location != null) {
        id = location.substring(location.lastIndexOf('/') + 1);
      }
    }
    return id;
  }

  private WebClient getVertxWebClient(Context context) {
     WebClientOptions options = new WebClientOptions();
     options.setLogActivity(true);
     options.setKeepAlive(true);
     options.setConnectTimeout(2000);
     options.setIdleTimeout(5000);

    return WebClientFactory.getWebClient(context.owner(), options);
  }
  private String buildAbsEndpoint(MultiMap okapiHeaders, String endpoint) {
    var okapiURL = okapiHeaders.get(OKAPI_URL);
    return okapiURL + endpoint;
  }

  public Future<String> postJsonObjectAndGetId(RequestEntry requestEntry, JsonObject entity, RequestContext requestContext) {
    var caseInsensitiveHeader = convertToCaseInsensitiveMap(requestContext.getHeaders());
    return getVertxWebClient(requestContext.getContext())
      .postAbs(buildAbsEndpoint(caseInsensitiveHeader, requestEntry.buildEndpoint()))
      .putHeaders(caseInsensitiveHeader)
      .expect(SUCCESS_RESPONSE_PREDICATE)
      .sendJsonObject(entity)
      .map(this::extractRecordId)
      .onFailure(t -> log.error("error occurred invoking POST {}", requestEntry.buildEndpoint()));
  }

  public Future<JsonObject> postJsonObject(RequestEntry requestEntry, JsonObject jsonObject, RequestContext requestContext) {
    var endpoint = requestEntry.buildEndpoint();
    var caseInsensitiveHeader = convertToCaseInsensitiveMap(requestContext.getHeaders());
    return getVertxWebClient(requestContext.getContext()).postAbs(buildAbsEndpoint(caseInsensitiveHeader, requestEntry.buildEndpoint()))
      .putHeaders(caseInsensitiveHeader)
      .expect(SUCCESS_RESPONSE_PREDICATE)
      .sendJsonObject(jsonObject)
      .map(bufferHttpResponse -> {
        var id = extractRecordId(bufferHttpResponse);
        return bufferHttpResponse
          .bodyAsJsonObject()
          .put(ID, id);
      })
      .onFailure(t -> log.error("error occurred invoking POST {}", endpoint));
  }
}
