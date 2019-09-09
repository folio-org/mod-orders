package org.folio.rest.impl;

import io.vertx.core.Context;
import io.vertx.core.json.JsonObject;
import me.escoffier.vertx.completablefuture.VertxCompletableFuture;
import org.folio.rest.jaxrs.model.OrderTemplate;
import org.folio.rest.jaxrs.model.OrderTemplateCollection;

import java.util.Map;
import java.util.concurrent.CompletableFuture;

import static org.folio.orders.utils.HelperUtils.buildQuery;
import static org.folio.orders.utils.HelperUtils.handleDeleteRequest;
import static org.folio.orders.utils.HelperUtils.handleGetRequest;
import static org.folio.orders.utils.HelperUtils.handlePutRequest;
import static org.folio.orders.utils.ResourcePathResolver.ORDER_TEMPLATES;
import static org.folio.orders.utils.ResourcePathResolver.resourceByIdPath;
import static org.folio.orders.utils.ResourcePathResolver.resourcesPath;

public class OrderTemplatesHelper extends AbstractHelper {

  private static final String GET_ORDER_TEMPLATES_BY_QUERY = resourcesPath(ORDER_TEMPLATES) + SEARCH_PARAMS;

  OrderTemplatesHelper(Map<String, String> okapiHeaders, Context ctx, String lang) {
    super(getHttpClient(okapiHeaders), okapiHeaders, ctx, lang);
  }

  CompletableFuture<OrderTemplate> createOrderTemplate(OrderTemplate template) {
    return createRecordInStorage(JsonObject.mapFrom(template), resourcesPath(ORDER_TEMPLATES)).thenApply(template::withId);
  }

  CompletableFuture<Void> updateOrderTemplate(OrderTemplate template) {
    String endpoint = resourceByIdPath(ORDER_TEMPLATES, template.getId());
    return handlePutRequest(endpoint, JsonObject.mapFrom(template), httpClient, ctx, okapiHeaders, logger);
  }

  CompletableFuture<OrderTemplate> getOrderTemplateById(String id) {
    return handleGetRequest(resourceByIdPath(ORDER_TEMPLATES, id), httpClient, ctx, okapiHeaders, logger)
      .thenApply(json -> json.mapTo(OrderTemplate.class));
  }

  CompletableFuture<OrderTemplateCollection> getOrderTemplates(String query, int offset, int limit) {
    CompletableFuture<OrderTemplateCollection> future = new VertxCompletableFuture<>(ctx);
    try {
      String endpoint = String.format(GET_ORDER_TEMPLATES_BY_QUERY, limit, offset, buildQuery(query, logger), lang);
      handleGetRequest(endpoint, httpClient, ctx, okapiHeaders, logger)
        .thenApply(json -> json.mapTo(OrderTemplateCollection.class))
        .thenAccept(future::complete)
        .exceptionally(t -> {
          future.completeExceptionally(t.getCause());
          return null;
        });
    } catch (Exception e) {
      future.completeExceptionally(e);
    }
    return future;
  }

  CompletableFuture<Void> deleteOrderTemplate(String id) {
    return handleDeleteRequest(resourceByIdPath(ORDER_TEMPLATES, id), httpClient, ctx, okapiHeaders, logger);
  }
}
