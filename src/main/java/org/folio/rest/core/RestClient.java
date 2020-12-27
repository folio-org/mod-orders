package org.folio.rest.core;

import static java.util.Objects.nonNull;
import static javax.ws.rs.core.MediaType.APPLICATION_JSON;
import static javax.ws.rs.core.MediaType.TEXT_PLAIN;
import static org.folio.orders.utils.HelperUtils.getEndpointWithQuery;
import static org.folio.orders.utils.HelperUtils.verifyAndExtractBody;
import static org.folio.rest.RestConstants.SEARCH_ENDPOINT;
import static org.folio.rest.RestVerticle.OKAPI_HEADER_TENANT;

import java.util.Collections;
import java.util.Map;
import java.util.concurrent.CompletableFuture;

import org.apache.logging.log4j.LogManager;
import org.apache.logging.log4j.Logger;
import org.folio.orders.utils.HelperUtils;
import org.folio.rest.RestConstants;
import org.folio.rest.core.models.RequestContext;
import org.folio.rest.tools.client.HttpClientFactory;
import org.folio.rest.tools.client.interfaces.HttpClientInterface;
import org.folio.rest.tools.utils.TenantTool;

import io.vertx.core.http.HttpMethod;
import io.vertx.core.json.JsonObject;


public class RestClient {

  private static final Logger logger = LogManager.getLogger();
  private static final String CALLING_ENDPOINT_MSG = "Sending {} {}";
  private static final String EXCEPTION_CALLING_ENDPOINT_MSG = "Exception calling {} {} : {}";

  private final String baseEndpoint;
  private final String endpointById;

  public RestClient(String baseEndpoint, String suffix) {
    this.baseEndpoint = baseEndpoint;
    this.endpointById = baseEndpoint + suffix;
  }

  public RestClient(String baseEndpoint) {
    this(baseEndpoint, "/%s");
  }

  public <T> CompletableFuture<T> get(String cqlQuery, int offset, int limit, RequestContext requestContext, Class<T> responseType) {
    String endpoint = String.format(SEARCH_ENDPOINT, baseEndpoint, limit, offset, getEndpointWithQuery(cqlQuery, logger));
    return get(endpoint, requestContext, responseType);
  }

  public <T> CompletableFuture<T> getById(String id, RequestContext requestContext, Class<T> responseType) {
    String endpoint = String.format(endpointById, id);
    return get(endpoint, requestContext, responseType);
  }

  public <T> CompletableFuture<T> post(T entity, RequestContext requestContext, Class<T> responseType) {
    CompletableFuture<T> future = new CompletableFuture<>();
    String endpoint = baseEndpoint;
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
          logger.error("'POST {}' request failed. Request body: {} : {}", endpoint, recordData.encodePrettily(), t.getCause());
          future.completeExceptionally(t.getCause());
          return null;
        });
    } catch (Exception e) {
      logger.error("'POST {}' request failed. Request body: {} : {}", endpoint, recordData.encodePrettily(), e);
      client.closeClient();
      future.completeExceptionally(e);
    }

    return future;
  }

  public <T> CompletableFuture<Void> put(String id, T entity, RequestContext requestContext) {
    CompletableFuture<Void> future = new CompletableFuture<>();
    String endpoint = String.format(endpointById, id);
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
          logger.error("'PUT {}' request failed. Request body: {} : {}", endpoint, recordData.encodePrettily(), t.getCause());
          return null;
        });
    } catch (Exception e) {
      logger.error("'PUT {}' request failed. Request body: {} : {}", endpoint, recordData.encodePrettily(), e);
      client.closeClient();
      future.completeExceptionally(e);
    }

    return future;
  }

  public CompletableFuture<Void> delete(String id, RequestContext requestContext) {
    CompletableFuture<Void> future = new CompletableFuture<>();
    String endpoint = String.format(endpointById, id);
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
          logger.error(EXCEPTION_CALLING_ENDPOINT_MSG, t, HttpMethod.DELETE, endpoint);
          future.completeExceptionally(t.getCause());
          return null;
        });
    } catch (Exception e) {
      client.closeClient();
      logger.error(EXCEPTION_CALLING_ENDPOINT_MSG, e, HttpMethod.DELETE, endpoint);
      future.completeExceptionally(e);
    }

    return future;
  }

  public <S> CompletableFuture<S> get(String endpoint, RequestContext requestContext, Class<S> responseType) {
    CompletableFuture<S> future = new CompletableFuture<>();
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
          logger.error(EXCEPTION_CALLING_ENDPOINT_MSG, t, HttpMethod.GET, endpoint);
          future.completeExceptionally(t.getCause());
          return null;
        });
    } catch (Exception e) {
      logger.error(EXCEPTION_CALLING_ENDPOINT_MSG, e, HttpMethod.GET, baseEndpoint);
      client.closeClient();
      future.completeExceptionally(e);
    }
    return future;
  }

  protected HttpClientInterface getHttpClient(Map<String, String> okapiHeaders) {
    final String okapiURL = okapiHeaders.getOrDefault(RestConstants.OKAPI_URL, "");
    final String tenantId = TenantTool.calculateTenantId(okapiHeaders.get(OKAPI_HEADER_TENANT));

    return HttpClientFactory.getHttpClient(okapiURL, tenantId);

  }

  private void setDefaultHeaders(HttpClientInterface httpClient) {
    // The RMB's HttpModuleClient2.ACCEPT is in sentence case. Using the same format to avoid duplicates
    httpClient.setDefaultHeaders(Collections.singletonMap("Accept", APPLICATION_JSON + ", " + TEXT_PLAIN));
  }
}

