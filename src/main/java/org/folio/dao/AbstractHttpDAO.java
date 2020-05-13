package org.folio.dao;

import static java.util.Objects.nonNull;
import static org.folio.orders.utils.HelperUtils.buildQuery;
import static org.folio.orders.utils.HelperUtils.verifyAndExtractBody;
import static org.folio.rest.RestVerticle.OKAPI_HEADER_TENANT;

import java.util.Map;
import java.util.concurrent.CompletableFuture;

import org.folio.orders.utils.HelperUtils;
import org.folio.rest.jaxrs.model.Entity;
import org.folio.rest.tools.client.HttpClientFactory;
import org.folio.rest.tools.client.interfaces.HttpClientInterface;
import org.folio.rest.tools.utils.TenantTool;

import io.vertx.core.Context;
import io.vertx.core.http.HttpMethod;
import io.vertx.core.json.JsonObject;
import io.vertx.core.logging.Logger;
import io.vertx.core.logging.LoggerFactory;
import me.escoffier.vertx.completablefuture.VertxCompletableFuture;

public abstract class AbstractHttpDAO<T extends Entity, E> implements GenericDAO<T, E> {

  protected final Logger logger = LoggerFactory.getLogger(this.getClass());

  private static final String CALLING_ENDPOINT_MSG = "Sending {} {}";
  private static final String EXCEPTION_CALLING_ENDPOINT_MSG = "Exception calling {} {}";
  public static final String OKAPI_URL = "x-okapi-url";
  static final String SEARCH_ENDPOINT = "%s?limit=%s&offset=%s%s";

  @Override
  public CompletableFuture<E> get(String query, int limit, int offset, Context context, Map<String, String> okapiHeaders) {
    CompletableFuture<E> future = new VertxCompletableFuture<>(context);
    HttpClientInterface client = getHttpClient(okapiHeaders);

    try {
      String endpoint = String.format(SEARCH_ENDPOINT, getEndpoint(), limit, offset, buildQuery(query, logger));
      if (logger.isDebugEnabled()) {
        logger.debug("Calling GET {}", endpoint);
      }

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
            E entitiesCollection = body.mapTo(getCollectionClazz());
            future.complete(entitiesCollection);
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
  public CompletableFuture<T> getById(String id, Context context, Map<String, String> okapiHeaders) {
    CompletableFuture<T> future = new VertxCompletableFuture<>(context);
    String endpoint = getByIdEndpoint(id);

    if (logger.isDebugEnabled()) {
      logger.debug("Calling GET {}", endpoint);
    }

    HttpClientInterface client = getHttpClient(okapiHeaders);
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
            T responseEntity = body.mapTo(getClazz());
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
    CompletableFuture<T> future = new VertxCompletableFuture<>(context);
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
            logger.error("'POST {}' request failed. Request body: {}", t.getCause(), endpoint, recordData.encodePrettily());
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
      logger.error("'POST {}' request failed. Request body: {}", e, endpoint, recordData.encodePrettily());
      client.closeClient();
      future.completeExceptionally(e);
    }

    return future;
  }

  @Override
  public CompletableFuture<Void> update(String id, T entity, Context context, Map<String, String> okapiHeaders) {
    CompletableFuture<Void> future = new VertxCompletableFuture<>(context);
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
            logger.error("'PUT {}' request failed. Request body: {}", t.getCause(), endpoint, recordData.encodePrettily());
          } else {
            future.complete(null);
          }
          return null;
        });
    } catch (Exception e) {
      logger.error("'PUT {}' request failed. Request body: {}", e, endpoint, recordData.encodePrettily());
      client.closeClient();
      future.completeExceptionally(e);
    }

    return future;
  }

  @Override
  public CompletableFuture<Void> delete(String id, Context context, Map<String, String> okapiHeaders) {
    CompletableFuture<Void> future = new VertxCompletableFuture<>(context);
    String endpoint = getByIdEndpoint(id);
    if (logger.isDebugEnabled()) {
      logger.debug(CALLING_ENDPOINT_MSG, HttpMethod.DELETE, endpoint);
    }
    HttpClientInterface client = getHttpClient(okapiHeaders);
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

}
