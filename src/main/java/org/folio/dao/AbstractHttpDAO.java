package org.folio.dao;

import static java.util.Objects.nonNull;
import static javax.ws.rs.core.MediaType.APPLICATION_JSON;
import static javax.ws.rs.core.MediaType.TEXT_PLAIN;
import static org.folio.orders.utils.HelperUtils.buildQuery;
import static org.folio.orders.utils.HelperUtils.verifyAndExtractBody;
import static org.folio.rest.RestVerticle.OKAPI_HEADER_TENANT;

import java.util.Collections;
import java.util.Map;
import java.util.concurrent.CompletableFuture;

import org.apache.logging.log4j.LogManager;
import org.apache.logging.log4j.Logger;
import org.folio.orders.utils.HelperUtils;
import org.folio.rest.jaxrs.model.Entity;
import org.folio.rest.tools.client.HttpClientFactory;
import org.folio.rest.tools.client.interfaces.HttpClientInterface;
import org.folio.rest.tools.utils.TenantTool;

import io.vertx.core.Context;
import io.vertx.core.http.HttpMethod;
import io.vertx.core.json.JsonObject;


public abstract class AbstractHttpDAO<T extends Entity, E> implements GenericDAO<T, E> {

  protected final Logger logger = LogManager.getLogger();

  private static final String CALLING_ENDPOINT_MSG = "Sending {} {}";
  private static final String EXCEPTION_CALLING_ENDPOINT_MSG = "Exception calling {} {} {}";
  public static final String OKAPI_URL = "x-okapi-url";
  static final String SEARCH_ENDPOINT = "%s?limit=%s&offset=%s%s";

  @Override
  public CompletableFuture<E> get(String query, int offset, int limit, Context context, Map<String, String> okapiHeaders) {
    String endpoint = String.format(SEARCH_ENDPOINT, getEndpoint(), limit, offset, buildQuery(query, logger));
    return get(okapiHeaders, endpoint, getCollectionClazz());
  }

  @Override
  public CompletableFuture<T> getById(String id, Context context, Map<String, String> okapiHeaders) {
    String endpoint = getByIdEndpoint(id);
    return get(okapiHeaders, endpoint, getClazz());
  }

  private <S> CompletableFuture<S> get(Map<String, String> okapiHeaders, String endpoint, Class<S> sClass) {
    CompletableFuture<S> future = new CompletableFuture<>();
    HttpClientInterface client = getHttpClient(okapiHeaders);
    if (logger.isDebugEnabled()) {
      logger.debug("Calling GET {}", endpoint);
    }

    try {
      client
        .request(HttpMethod.GET, endpoint, okapiHeaders)
        .thenApply(response -> {
          if (logger.isDebugEnabled()) {
            logger.debug("Validating response for GET {}", endpoint);
          }
          return verifyAndExtractBody(response);
        })
        .handle((body, t) -> {
          client.closeClient();
          if (t != null) {
            logger.error(EXCEPTION_CALLING_ENDPOINT_MSG, t, HttpMethod.GET, endpoint);
            future.completeExceptionally(t.getCause());
          } else {
            if (logger.isDebugEnabled()) {
              logger.debug("The response body for GET {}: {}", endpoint, nonNull(body) ? body.encodePrettily() : null);
            }
            S responseEntity = body.mapTo(sClass);
            future.complete(responseEntity);
          }
          return null;
        });
    } catch (Exception e) {
      logger.error(EXCEPTION_CALLING_ENDPOINT_MSG, e, HttpMethod.GET, getEndpoint());
      client.closeClient();
      future.completeExceptionally(e);
    }
    return future;
  }

  @Override
  public CompletableFuture<T> save(T entity, Context context, Map<String, String> okapiHeaders) {
    CompletableFuture<T> future = new CompletableFuture<>();
    String endpoint = getEndpoint();
    JsonObject recordData = JsonObject.mapFrom(entity);

    if (logger.isDebugEnabled()) {
      logger.debug("Sending 'POST {}' with body: {}", endpoint, recordData.encodePrettily());
    }

    HttpClientInterface client = getHttpClient(okapiHeaders);
    try {
      client
        .request(HttpMethod.POST, recordData.toBuffer(), endpoint, okapiHeaders)
        .thenApply(HelperUtils::verifyAndExtractBody)
        .handle((body, t) -> {
          client.closeClient();
          if (t != null) {
            logger.error("'POST {}' request failed. Request body: {} {}", t.getCause(), endpoint, recordData.encodePrettily());
            future.completeExceptionally(t.getCause());
          } else {
            T responseEntity = body.mapTo(getClazz());
            if (logger.isDebugEnabled()) {
              logger.debug("'POST {}' request successfully processed. Record with '{}' id has been created", endpoint, responseEntity.getId());
            }
            future.complete(responseEntity);
          }
          return null;
        });
    } catch (Exception e) {
      logger.error("'POST {}' request failed. Request body: {} {}", e, endpoint, recordData.encodePrettily());
      client.closeClient();
      future.completeExceptionally(e);
    }

    return future;
  }

  @Override
  public CompletableFuture<Void> update(String id, T entity, Context context, Map<String, String> okapiHeaders) {
    CompletableFuture<Void> future = new CompletableFuture<>();
    String endpoint = getByIdEndpoint(id);
    JsonObject recordData = JsonObject.mapFrom(entity);

    if (logger.isDebugEnabled()) {
      logger.debug("Sending 'PUT {}' with body: {}", endpoint, recordData.encodePrettily());
    }

    HttpClientInterface client = getHttpClient(okapiHeaders);
    try {
      client
        .request(HttpMethod.PUT, recordData.toBuffer(), endpoint, okapiHeaders)
        .thenAccept(HelperUtils::verifyResponse)
        .handle((aVoid, t) -> {
          client.closeClient();
          if (t != null) {
            future.completeExceptionally(t.getCause());
            logger.error("'PUT {}' request failed. Request body: {} {}", t.getCause(), endpoint, recordData.encodePrettily());
          } else {
            future.complete(null);
          }
          return null;
        });
    } catch (Exception e) {
      logger.error("'PUT {}' request failed. Request body: {} {}", e, endpoint, recordData.encodePrettily());
      client.closeClient();
      future.completeExceptionally(e);
    }

    return future;
  }

  @Override
  public CompletableFuture<Void> delete(String id, Context context, Map<String, String> okapiHeaders) {
    CompletableFuture<Void> future = new CompletableFuture<>();
    String endpoint = getByIdEndpoint(id);
    if (logger.isDebugEnabled()) {
      logger.debug(CALLING_ENDPOINT_MSG, HttpMethod.DELETE, endpoint);
    }
    HttpClientInterface client = getHttpClient(okapiHeaders);
    setDefaultHeaders(client);

    try {
      client.request(HttpMethod.DELETE, getByIdEndpoint(id), okapiHeaders)
        .thenAccept(HelperUtils::verifyResponse)
        .handle((aVoid, t) -> {
          client.closeClient();
          if (t != null) {
            logger.error(EXCEPTION_CALLING_ENDPOINT_MSG, t, HttpMethod.DELETE, endpoint);
            future.completeExceptionally(t.getCause());
          } else {
            future.complete(null);
          }
          return null;
        });
    } catch (Exception e) {
      client.closeClient();
      logger.error(EXCEPTION_CALLING_ENDPOINT_MSG, e, HttpMethod.DELETE, endpoint);
      future.completeExceptionally(e);
    }

    return future;
  }

  protected abstract String getByIdEndpoint(String id);

  protected abstract String getEndpoint();

  protected abstract Class<T> getClazz();

  protected abstract Class<E> getCollectionClazz();

  public static HttpClientInterface getHttpClient(Map<String, String> okapiHeaders) {
    final String okapiURL = okapiHeaders.getOrDefault(OKAPI_URL, "");
    final String tenantId = TenantTool.calculateTenantId(okapiHeaders.get(OKAPI_HEADER_TENANT));

    return HttpClientFactory.getHttpClient(okapiURL, tenantId);

  }

  private static void setDefaultHeaders(HttpClientInterface httpClient) {
    // The RMB's HttpModuleClient2.ACCEPT is in sentence case. Using the same format to avoid duplicates
    httpClient.setDefaultHeaders(Collections.singletonMap("Accept", APPLICATION_JSON + ", " + TEXT_PLAIN));
  }

}
