package org.folio.rest.core;

import static java.util.Objects.nonNull;
import static javax.ws.rs.core.MediaType.APPLICATION_JSON;
import static javax.ws.rs.core.MediaType.TEXT_PLAIN;
import static org.folio.orders.utils.HelperUtils.verifyAndExtractBody;
import static org.folio.orders.utils.HelperUtils.verifyResponse;
import static org.folio.rest.RestConstants.ERROR_MESSAGE;
import static org.folio.rest.RestConstants.NOT_FOUND;
import static org.folio.rest.RestVerticle.OKAPI_HEADER_TENANT;

import java.util.Collections;
import java.util.Map;
import java.util.concurrent.CompletableFuture;

import org.apache.commons.lang3.StringUtils;
import org.apache.logging.log4j.LogManager;
import org.apache.logging.log4j.Logger;
import org.folio.orders.utils.HelperUtils;
import org.folio.rest.RestConstants;
import org.folio.rest.core.models.RequestContext;
import org.folio.rest.core.models.RequestEntry;
import org.folio.rest.tools.client.HttpClientFactory;
import org.folio.rest.tools.client.interfaces.HttpClientInterface;
import org.folio.rest.tools.utils.TenantTool;

import io.vertx.core.http.HttpMethod;
import io.vertx.core.json.JsonObject;

public class RestClient {

  private static final Logger logger = LogManager.getLogger();
  private static final String CALLING_ENDPOINT_MSG = "Sending {} {}";
  private static final String EXCEPTION_CALLING_ENDPOINT_MSG = "Exception calling %s %s - %s";

  public <T> CompletableFuture<T> getById(String baseEndpoint, String id, boolean skipThrowNorFoundException,
                                           RequestContext requestContext, Class<T> responseType) {
    RequestEntry requestEntry = new RequestEntry(baseEndpoint).withPathParameter("id", id);
    return get(requestEntry, skipThrowNorFoundException, requestContext, responseType);
  }

  public <T> CompletableFuture<T> getById(String baseEndpoint, String id, RequestContext requestContext, Class<T> responseType) {
    return getById(baseEndpoint, id, false, requestContext, responseType);
  }

  public <T> CompletableFuture<T> post(RequestEntry requestEntry, T entity, RequestContext requestContext, Class<T> responseType) {
    CompletableFuture<T> future = new CompletableFuture<>();
    String endpoint = requestEntry.buildEndpoint();
    JsonObject recordData = JsonObject.mapFrom(entity);

    if (logger.isDebugEnabled()) {
      logger.debug("Sending 'POST {}' with body: {}", endpoint, recordData.encodePrettily());
    }

    HttpClientInterface client = getHttpClient(requestContext.getHeaders());
    try {
      client.request(HttpMethod.POST, recordData.toBuffer(), endpoint, requestContext.getHeaders())
        .thenApply(HelperUtils::verifyAndExtractBody)
        .thenAccept(body -> {
          client.closeClient();
          T responseEntity = body.mapTo(responseType);
          if (logger.isDebugEnabled()) {
            logger.debug("'POST {}' request successfully processed. Record with '{}' id has been created", endpoint, body);
          }
          future.complete(responseEntity);
        })
        .exceptionally(t -> {
          client.closeClient();
          logger.error("'POST {}' request failed. Request body: {}", endpoint, recordData.encodePrettily(), t.getCause());
          future.completeExceptionally(t.getCause());
          return null;
        });
    } catch (Exception e) {
      logger.error("'POST {}' request failed. Request body: {}", endpoint, recordData.encodePrettily(), e);
      client.closeClient();
      future.completeExceptionally(e);
    }

    return future;
  }

  public <T> CompletableFuture<T> post(RequestEntry requestEntry, JsonObject recordData, PostResponseType postResponseType,
                                       Class<T> responseType, RequestContext requestContext) {
    CompletableFuture<T> future = new CompletableFuture<>();
    String endpoint = requestEntry.buildEndpoint();

    if (logger.isDebugEnabled()) {
      logger.debug("Sending 'POST {}' with body: {}", endpoint, recordData.encodePrettily());
    }

    HttpClientInterface client = getHttpClient(requestContext.getHeaders());
    try {
      client
        .request(HttpMethod.POST, recordData.toBuffer(), endpoint, requestContext.getHeaders())
        .thenApply(response -> {
          client.closeClient();
          if (postResponseType == PostResponseType.BODY) {
            JsonObject body =  HelperUtils.verifyAndExtractBody(response);
            if (responseType == JsonObject.class) {
              return body;
            }
            T responseEntity = body.mapTo(responseType);
            if (logger.isDebugEnabled()) {
              logger.debug("'POST {}' request successfully processed. Record with '{}' id has been created", endpoint, body);
            }
            return responseEntity;
          } else if (postResponseType == PostResponseType.UUID) {
            return HelperUtils.verifyAndExtractRecordId(response);
          }
          return HelperUtils.verifyAndExtractBody(response);
        })
        .thenAccept(body -> {
          client.closeClient();
          if (logger.isDebugEnabled()) {
            logger.debug("'POST {}' request successfully processed. Record with '{}' id has been created", endpoint, body);
          }
          future.complete(responseType.cast(body));
        })
        .exceptionally(t -> {
          client.closeClient();
          logger.error("'POST {}' request failed. Request body: {}", endpoint, recordData.encodePrettily(), t.getCause());
          future.completeExceptionally(t.getCause());
          return null;
        });
    } catch (Exception e) {
      logger.error("'POST {}' request failed. Request body: {}", endpoint, recordData.encodePrettily(), e);
      client.closeClient();
      future.completeExceptionally(e);
    }

    return future;
  }

    public <T> CompletableFuture<Void> put(RequestEntry requestEntry, T entity, RequestContext requestContext) {
        CompletableFuture<Void> future = new CompletableFuture<>();
        String endpoint = requestEntry.buildEndpoint();
        JsonObject recordData = JsonObject.mapFrom(entity);

        if (logger.isDebugEnabled()) {
            logger.debug("Sending 'PUT {}' with body: {}", endpoint, recordData.encodePrettily());
        }

        HttpClientInterface client = getHttpClient(requestContext.getHeaders());
        setDefaultHeaders(client);
        try {
            client
                    .request(HttpMethod.PUT, recordData.toBuffer(), endpoint, requestContext.getHeaders())
                    .thenAccept(HelperUtils::verifyResponse)
                    .thenAccept(avoid -> {
                        client.closeClient();
                        future.complete(null);
                    })
                    .exceptionally(t -> {
                        client.closeClient();
                        future.completeExceptionally(t.getCause());
                        logger.error("'PUT {}' request failed. Request body: {}", endpoint, recordData.encodePrettily(), t.getCause());
                        return null;
                    });
        } catch (Exception e) {
            logger.error("'PUT {}' request failed. Request body: {}", endpoint, recordData.encodePrettily(), e);
            client.closeClient();
            future.completeExceptionally(e);
        }

        return future;
    }

  public CompletableFuture<Void> put(RequestEntry requestEntry, JsonObject recordData, RequestContext requestContext) {
    CompletableFuture<Void> future = new CompletableFuture<>();
    String endpoint = requestEntry.buildEndpoint();

    if (logger.isDebugEnabled()) {
      logger.debug("Sending 'PUT {}' with body: {}", endpoint, recordData.encodePrettily());
    }

    HttpClientInterface client = getHttpClient(requestContext.getHeaders());
    setDefaultHeaders(client);
    try {
      client
        .request(HttpMethod.PUT, recordData.toBuffer(), endpoint, requestContext.getHeaders())
        .thenAccept(HelperUtils::verifyResponse)
        .thenAccept(avoid -> {
          client.closeClient();
          future.complete(null);
        })
        .exceptionally(t -> {
          client.closeClient();
          future.completeExceptionally(t.getCause());
          logger.error("'PUT {}' request failed. Request body: {}", endpoint, recordData.encodePrettily(), t.getCause());
          return null;
        });
    } catch (Exception e) {
      logger.error("'PUT {}' request failed. Request body: {}", endpoint, recordData.encodePrettily(), e);
      client.closeClient();
      future.completeExceptionally(e);
    }

    return future;
  }

  public <T> CompletableFuture<Void> patch(RequestEntry requestEntry, T entity, RequestContext requestContext) {
    CompletableFuture<Void> future = new CompletableFuture<>();
    String endpoint = requestEntry.buildEndpoint();
    JsonObject recordData = JsonObject.mapFrom(entity);
    if (logger.isDebugEnabled()) {
      logger.debug("Sending 'PATCH {}' with body: {}", endpoint, recordData.encodePrettily());
    }

    HttpClientInterface client = getHttpClient(requestContext.getHeaders());
    setDefaultHeaders(client);
    try {
      client
          .request(HttpMethod.PATCH, recordData.toBuffer(), endpoint, requestContext.getHeaders())
          .thenAccept(HelperUtils::verifyResponse)
          .thenAccept(avoid -> {
            client.closeClient();
            future.complete(null);
          })
          .exceptionally(t -> {
            client.closeClient();
            future.completeExceptionally(t.getCause());
            logger.error("'PATCH {}' request failed. Request body: {}", endpoint, recordData.encodePrettily(), t.getCause());
            return null;
          });
    } catch (Exception e) {
      logger.error("'PATCH {}' request failed. Request body: {}", endpoint, recordData.encodePrettily(), e);
      client.closeClient();
      future.completeExceptionally(e);
    }

    return future;
  }

  public CompletableFuture<Void> patch(RequestEntry requestEntry, JsonObject recordData, RequestContext requestContext) {
    CompletableFuture<Void> future = new CompletableFuture<>();
    String endpoint = requestEntry.buildEndpoint();

    if (logger.isDebugEnabled()) {
      logger.debug("Sending 'PATCH {}' with body: {}", endpoint, recordData.encodePrettily());
    }

    HttpClientInterface client = getHttpClient(requestContext.getHeaders());
    setDefaultHeaders(client);
    try {
      client
          .request(HttpMethod.PATCH, recordData.toBuffer(), endpoint, requestContext.getHeaders())
          .thenAccept(HelperUtils::verifyResponse)
          .thenAccept(avoid -> {
            client.closeClient();
            future.complete(null);
          })
          .exceptionally(t -> {
            client.closeClient();
            future.completeExceptionally(t.getCause());
            logger.error("'PATCH {}' request failed. Request body: {}", endpoint, recordData.encodePrettily(), t.getCause());
            return null;
          });
    } catch (Exception e) {
      logger.error("'PATCH {}' request failed. Request body: {}", endpoint, recordData.encodePrettily(), e);
      client.closeClient();
      future.completeExceptionally(e);
    }

    return future;
  }

  public CompletableFuture<Void> delete(RequestEntry requestEntry, boolean skipThrowNorFoundException, RequestContext requestContext) {
    CompletableFuture<Void> future = new CompletableFuture<>();
    String endpoint = requestEntry.buildEndpoint();
    if (logger.isDebugEnabled()) {
      logger.debug(CALLING_ENDPOINT_MSG, HttpMethod.DELETE, endpoint);
    }
    HttpClientInterface client = getHttpClient(requestContext.getHeaders());
    setDefaultHeaders(client);

    try {
      client.request(HttpMethod.DELETE, endpoint, requestContext.getHeaders())
        .thenAccept(response -> {
          client.closeClient();
          int code = response.getCode();
          if (code == NOT_FOUND && skipThrowNorFoundException) {
            String errorMessage = (response.getError() != null) ? response.getError().getString(ERROR_MESSAGE) : StringUtils.EMPTY;
            logger.error("The GET {} operation completed with {} error: {}", endpoint, code, errorMessage);
            future.complete(null);
          } else {
            verifyResponse(response);
            future.complete(null);
          }
        })
        .exceptionally(t -> {
          client.closeClient();
          logger.error(String.format(EXCEPTION_CALLING_ENDPOINT_MSG, HttpMethod.DELETE, endpoint, requestContext), t);
          future.completeExceptionally(t.getCause());
          return null;
        });
    } catch (Exception e) {
      client.closeClient();
      logger.error(String.format(EXCEPTION_CALLING_ENDPOINT_MSG, HttpMethod.DELETE, endpoint, requestContext), e);
      future.completeExceptionally(e);
    }

    return future;
  }

  public CompletableFuture<Void> delete(RequestEntry requestEntry, RequestContext requestContext) {
    return delete(requestEntry, false, requestContext);
  }

  public <S> CompletableFuture<S> get(RequestEntry requestEntry, boolean skipThrowNorFoundException,
                                      RequestContext requestContext, Class<S> responseType) {
    CompletableFuture<S> future = new CompletableFuture<>();
    String endpoint = requestEntry.buildEndpoint();
    HttpClientInterface client = getHttpClient(requestContext.getHeaders());
    logger.debug("Calling GET {}", endpoint);

    try {
      client
        .request(HttpMethod.GET, endpoint, requestContext.getHeaders())
        .thenApply(response -> {
          int code = response.getCode();
          if (code == NOT_FOUND && skipThrowNorFoundException) {
            String errorMessage = (response.getError() != null) ? response.getError().getString(ERROR_MESSAGE) : StringUtils.EMPTY;
            logger.error("The GET {} operation completed with {} error: {}", endpoint, code, errorMessage);
            return new JsonObject();
          }
          return verifyAndExtractBody(response);
        })
        .thenAccept(body -> {
          client.closeClient();
          if (logger.isDebugEnabled()) {
            logger.debug("The response body for GET {}: {}", endpoint, nonNull(body) ? body.encodePrettily() : null);
          }
          if (body != null && !body.isEmpty()) {
            S responseEntity = body.mapTo(responseType);
            future.complete(responseEntity);
          }
          future.complete(null);
        })
        .exceptionally(t -> {
          client.closeClient();
          logger.error(String.format(EXCEPTION_CALLING_ENDPOINT_MSG, HttpMethod.GET, endpoint, requestContext), t);
          future.completeExceptionally(t.getCause());
          return null;
        });
    } catch (Exception e) {
      client.closeClient();
      logger.error(String.format(EXCEPTION_CALLING_ENDPOINT_MSG, HttpMethod.GET, requestEntry.getBaseEndpoint(), requestContext), e);
      future.completeExceptionally(e);
    }
    return future;
  }

  public <S> CompletableFuture<S> get(RequestEntry requestEntry, RequestContext requestContext, Class<S> responseType) {
    return get(requestEntry, false, requestContext, responseType);
  }

  public CompletableFuture<JsonObject> getAsJsonObject(RequestEntry requestEntry, boolean skipThrowNorFoundException,
                                                        RequestContext requestContext) {
    CompletableFuture<JsonObject> future = new CompletableFuture<>();
    String endpoint = requestEntry.buildEndpoint();
    HttpClientInterface client = getHttpClient(requestContext.getHeaders());
    logger.debug("Calling GET {}", endpoint);
    try {
      client
        .request(HttpMethod.GET, endpoint, requestContext.getHeaders())
        .thenApply(response -> {
          int code = response.getCode();
          if (code == NOT_FOUND && skipThrowNorFoundException) {
            String errorMessage = (response.getError() != null) ? response.getError().getString(ERROR_MESSAGE) : StringUtils.EMPTY;
            logger.error("The GET {} operation completed with {} error: {}", endpoint, code, errorMessage);
            return new JsonObject();
          }
          return verifyAndExtractBody(response);
        })
        .thenAccept(json -> {
          client.closeClient();
          if (json != null) {
            if (!json.isEmpty() && logger.isDebugEnabled()) {
              logger.debug("The GET {} operation completed with following response body: {}", endpoint, json.encodePrettily());
            }
            future.complete(json);
          } else {
            logger.debug("The GET {} operation completed with no response body", endpoint);
            future.complete(new JsonObject());
          }
        })
        .exceptionally(t -> {
          client.closeClient();
          logger.error(String.format(EXCEPTION_CALLING_ENDPOINT_MSG, HttpMethod.GET, endpoint, requestContext), t);
          future.completeExceptionally(t.getCause());
          return null;
        });
    } catch (Exception e) {
      client.closeClient();
      logger.error(String.format(EXCEPTION_CALLING_ENDPOINT_MSG, HttpMethod.GET, requestEntry.getBaseEndpoint(), requestContext), e);
      future.completeExceptionally(e);
    }
    return future;
  }
  public CompletableFuture<JsonObject> getAsJsonObject(RequestEntry requestEntry, RequestContext requestContext) {
   return getAsJsonObject(requestEntry, false, requestContext);
  }

  public HttpClientInterface getHttpClient(Map<String, String> okapiHeaders) {
    final String okapiURL = okapiHeaders.getOrDefault(RestConstants.OKAPI_URL, "");
    final String tenantId = TenantTool.calculateTenantId(okapiHeaders.get(OKAPI_HEADER_TENANT));

     return HttpClientFactory.getHttpClient(okapiURL, tenantId);
  }

  private void setDefaultHeaders(HttpClientInterface httpClient) {
    // The RMB's HttpModuleClient2.ACCEPT is in sentence case. Using the same format to avoid duplicates
    httpClient.setDefaultHeaders(Collections.singletonMap("Accept", APPLICATION_JSON + ", " + TEXT_PLAIN));
  }
}
