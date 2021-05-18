package org.folio.rest.core;

import static java.util.Objects.nonNull;
import static javax.ws.rs.core.MediaType.APPLICATION_JSON;
import static javax.ws.rs.core.MediaType.TEXT_PLAIN;
import static org.folio.orders.utils.HelperUtils.verifyAndExtractBody;
import static org.folio.rest.RestVerticle.OKAPI_HEADER_TENANT;

import java.util.Collections;
import java.util.Map;
import java.util.concurrent.CompletableFuture;

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


    public <T> CompletableFuture<T> getById(String baseEndpoint, String id, RequestContext requestContext, Class<T> responseType) {
        RequestEntry requestEntry = new RequestEntry(baseEndpoint).withPathParameter("id", id);
        return get(requestEntry, requestContext, responseType);
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
            client
                    .request(HttpMethod.POST, recordData.toBuffer(), endpoint, requestContext.getHeaders())
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
          if (postResponseType == PostResponseType.BODY) {
            return HelperUtils.verifyAndExtractBody(response);
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

    public CompletableFuture<Void> delete(RequestEntry requestEntry, RequestContext requestContext) {
        CompletableFuture<Void> future = new CompletableFuture<>();
        String endpoint = requestEntry.buildEndpoint();
        if (logger.isDebugEnabled()) {
            logger.debug(CALLING_ENDPOINT_MSG, HttpMethod.DELETE, endpoint);
        }
        HttpClientInterface client = getHttpClient(requestContext.getHeaders());
        setDefaultHeaders(client);

        try {
          client.request(HttpMethod.DELETE, endpoint, requestContext.getHeaders())
            .thenAccept(HelperUtils::verifyResponse)
            .thenAccept(aVoid -> {
              client.closeClient();
              future.complete(null);
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

    public <S> CompletableFuture<S> get(RequestEntry requestEntry, RequestContext requestContext, Class<S> responseType) {
        CompletableFuture<S> future = new CompletableFuture<>();
        String endpoint = requestEntry.buildEndpoint();
        HttpClientInterface client = getHttpClient(requestContext.getHeaders());
        if (logger.isDebugEnabled()) {
            logger.debug("Calling GET {}", endpoint);
        }

        try {
            client
                    .request(HttpMethod.GET, endpoint, requestContext.getHeaders())
                    .thenApply(response -> {
                        if (logger.isDebugEnabled()) {
                            logger.debug("Validating response for GET {}", endpoint);
                        }
                        return verifyAndExtractBody(response);
                    })
                    .thenAccept(body -> {
                        client.closeClient();
                        if (logger.isDebugEnabled()) {
                            logger.debug("The response body for GET {}: {}", endpoint, nonNull(body) ? body.encodePrettily() : null);
                        }
                        S responseEntity = body.mapTo(responseType);
                        future.complete(responseEntity);
                    })
                    .exceptionally(t -> {
                        client.closeClient();
                        logger.error(String.format(EXCEPTION_CALLING_ENDPOINT_MSG, HttpMethod.GET, endpoint, requestContext), t);
                        future.completeExceptionally(t.getCause());
                        return null;
                    });
        } catch (Exception e) {
          logger.error(String.format(EXCEPTION_CALLING_ENDPOINT_MSG, HttpMethod.GET, requestEntry.getBaseEndpoint(), requestContext), e);
          client.closeClient();
          future.completeExceptionally(e);
        }
        return future;
    }

  public CompletableFuture<JsonObject> getAsJsonObject(RequestEntry requestEntry, RequestContext requestContext) {
    CompletableFuture<JsonObject> future = new CompletableFuture<>();
    String endpoint = requestEntry.buildEndpoint();
    HttpClientInterface client = getHttpClient(requestContext.getHeaders());
    if (logger.isDebugEnabled()) {
      logger.debug("Calling GET {}", endpoint);
    }

    try {
      client
        .request(HttpMethod.GET, endpoint, requestContext.getHeaders())
        .thenApply(response -> {
          if (logger.isDebugEnabled()) {
            logger.debug("Validating response for GET {}", endpoint);
          }
          return verifyAndExtractBody(response);
        })
        .thenAccept(body -> {
          client.closeClient();
          if (logger.isDebugEnabled()) {
            logger.debug("The response body for GET {}: {}", endpoint, nonNull(body) ? body.encodePrettily() : null);
          }
          future.complete(body);
        })
        .exceptionally(t -> {
          client.closeClient();
          logger.error(String.format(EXCEPTION_CALLING_ENDPOINT_MSG, HttpMethod.GET, endpoint, requestContext), t);
          future.completeExceptionally(t.getCause());
          return null;
        });
    } catch (Exception e) {
      logger.error(String.format(EXCEPTION_CALLING_ENDPOINT_MSG, HttpMethod.GET, requestEntry.getBaseEndpoint(), requestContext), e);
      client.closeClient();
      future.completeExceptionally(e);
    }
    return future;
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
