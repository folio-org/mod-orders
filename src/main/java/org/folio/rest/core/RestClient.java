package org.folio.rest.core;

import static javax.ws.rs.core.HttpHeaders.LOCATION;
import static javax.ws.rs.core.MediaType.APPLICATION_JSON;
import static javax.ws.rs.core.MediaType.TEXT_PLAIN;
import static org.folio.rest.RestConstants.ID;
import static org.folio.rest.RestConstants.OKAPI_URL;
import static org.folio.rest.core.exceptions.ExceptionUtil.getHttpException;

import java.util.Map;
import java.util.Objects;

import io.vertx.core.Context;
import io.vertx.core.Future;
import io.vertx.core.MultiMap;
import io.vertx.core.Promise;
import io.vertx.core.buffer.Buffer;
import io.vertx.core.http.HttpResponseExpectation;
import io.vertx.core.json.JsonObject;
import io.vertx.ext.web.client.HttpResponse;
import io.vertx.ext.web.client.WebClient;
import io.vertx.ext.web.client.WebClientOptions;
import lombok.extern.log4j.Log4j2;
import org.folio.okapi.common.WebClientFactory;
import org.folio.rest.core.exceptions.HttpException;
import org.folio.rest.core.models.RequestContext;
import org.folio.rest.core.models.RequestEntry;

@Log4j2
public class RestClient {

  public <T> Future<T> post(RequestEntry requestEntry, T entity, Class<T> responseType, RequestContext requestContext) {
    return post(requestEntry.buildEndpoint(), entity, responseType, requestContext);
  }

  public <T> Future<T> post(String endpoint, T entity, Class<T> responseType, RequestContext requestContext) {
    var caseInsensitiveHeader = convertToCaseInsensitiveMap(requestContext.getHeaders());
    return getVertxWebClient(requestContext.getContext()).postAbs(buildAbsEndpoint(caseInsensitiveHeader, endpoint))
      .putHeaders(caseInsensitiveHeader)
      .sendJson(entity)
      .compose(RestClient::convertHttpResponse)
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
      .map(bufferHttpResponse -> {
        if (Objects.nonNull(bufferHttpResponse) && Objects.nonNull(bufferHttpResponse.bodyAsJsonObject())) {
          return bufferHttpResponse.bodyAsJsonObject().mapTo(responseType);
        }
        return null;
      })
      .onFailure(log::error);
  }

  public <T> Future<Void> postEmptyResponse(String endpoint, T entity, RequestContext requestContext) {
    var caseInsensitiveHeader = convertToCaseInsensitiveMap(requestContext.getHeaders());
    return getVertxWebClient(requestContext.getContext())
      .postAbs(buildAbsEndpoint(caseInsensitiveHeader, endpoint))
      .putHeaders(caseInsensitiveHeader)
      .sendJson(entity)
      .compose(RestClient::convertHttpResponse)
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

  public <T> Future<Void> put(String endpoint, T dataObject, RequestContext requestContext) {
    var recordData = JsonObject.mapFrom(dataObject);
    var caseInsensitiveHeader = convertToCaseInsensitiveMap(requestContext.getHeaders());
    return getVertxWebClient(requestContext.getContext())
      .putAbs(buildAbsEndpoint(caseInsensitiveHeader, endpoint))
      .putHeaders(caseInsensitiveHeader)
      .sendJson(recordData)
      .compose(RestClient::convertHttpResponse)
      .onFailure(log::error)
      .mapEmpty();
  }

  public <T> Future<Void> patch(RequestEntry requestEntry, T dataObject, RequestContext requestContext) {
    String endpoint = requestEntry.buildEndpoint();
    return patch(endpoint, dataObject, requestContext);
  }

  public <T> Future<Void> patch(String endpoint, T dataObject, RequestContext requestContext) {
    var caseInsensitiveHeader = convertToCaseInsensitiveMap(requestContext.getHeaders());
    Promise<Void> promise = Promise.promise();
    return getVertxWebClient(requestContext.getContext())
      .patchAbs(buildAbsEndpoint(caseInsensitiveHeader, endpoint))
      .putHeaders(caseInsensitiveHeader)
      .sendJson(dataObject)
      .compose(RestClient::convertHttpResponse)
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
      .send()
      .compose(RestClient::convertHttpResponse)
      .onSuccess(f -> promise.complete())
      .onFailure(t -> handleErrorResponse(promise, t, skipError404, endpointById));

    return promise.future();
  }

  private <T> void handleGetMethodErrorResponse(Promise<T> promise, Throwable t, boolean skipError404) {
    if (skipError404 && t instanceof HttpException httpException && httpException.getCode() == 404) {
      log.warn(t);
      promise.complete();
    } else {
      log.error(t);
      promise.fail(t);
    }
  }

  private void handleErrorResponse(Promise<Void> promise, Throwable t, boolean skipError404, String path) {
    if (skipError404 && t instanceof HttpException httpException && httpException.getCode() == 404) {
      log.warn("handleErrorResponse: Resource not found: '{}', reason: {}", path, t.getMessage());
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

  public <T> Future<T> get(String endpoint, boolean skipError404, Class<T> responseType, RequestContext requestContext) {
    var caseInsensitiveHeader = convertToCaseInsensitiveMap(requestContext.getHeaders());
    Promise<T> promise = Promise.promise();
    getVertxWebClient(requestContext.getContext())
      .getAbs(buildAbsEndpoint(caseInsensitiveHeader, endpoint))
      .putHeaders(caseInsensitiveHeader)
      .send()
      .compose(RestClient::convertHttpResponse)
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
      .send()
      .compose(RestClient::convertHttpResponse)
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

  private static String extractRecordId(HttpResponse<Buffer> response) {
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

  private static WebClient getVertxWebClient(Context context) {
    WebClientOptions options = new WebClientOptions();
    options.setLogActivity(true);
    options.setKeepAlive(true);
    options.setConnectTimeout(2000);
    options.setIdleTimeout(5000);

    return WebClientFactory.getWebClient(context.owner(), options);
  }

  private static String buildAbsEndpoint(MultiMap okapiHeaders, String endpoint) {
    var okapiURL = okapiHeaders.get(OKAPI_URL);
    return okapiURL + endpoint;
  }

  public Future<String> postJsonObjectAndGetId(RequestEntry requestEntry, JsonObject entity, RequestContext requestContext) {
    var caseInsensitiveHeader = convertToCaseInsensitiveMap(requestContext.getHeaders());
    return getVertxWebClient(requestContext.getContext())
      .postAbs(buildAbsEndpoint(caseInsensitiveHeader, requestEntry.buildEndpoint()))
      .putHeaders(caseInsensitiveHeader)
      .sendJsonObject(entity)
      .compose(RestClient::convertHttpResponse)
      .map(RestClient::extractRecordId)
      .onFailure(t -> log.error("error occurred invoking POST {}", requestEntry.buildEndpoint()));
  }

  public Future<JsonObject> postJsonObject(RequestEntry requestEntry, JsonObject jsonObject, RequestContext requestContext) {
    var endpoint = requestEntry.buildEndpoint();
    var caseInsensitiveHeader = convertToCaseInsensitiveMap(requestContext.getHeaders());
    return getVertxWebClient(requestContext.getContext()).postAbs(buildAbsEndpoint(caseInsensitiveHeader, requestEntry.buildEndpoint()))
      .putHeaders(caseInsensitiveHeader)
      .sendJsonObject(jsonObject)
      .compose(RestClient::convertHttpResponse)
      .map(bufferHttpResponse -> {
        var id = extractRecordId(bufferHttpResponse);
        return bufferHttpResponse
          .bodyAsJsonObject()
          .put(ID, id);
      })
      .onFailure(t -> log.error("error occurred invoking POST {}", endpoint));
  }

  private static <T> Future<HttpResponse<T>> convertHttpResponse(HttpResponse<T> response) {
    return HttpResponseExpectation.SC_SUCCESS.test(response)
      ? Future.succeededFuture(response)
      : Future.failedFuture(new HttpException(response.statusCode(), response.bodyAsString()));
  }

}
