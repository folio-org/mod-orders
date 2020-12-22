package org.folio.helper;

import static org.folio.orders.utils.HelperUtils.buildQuery;
import static org.folio.orders.utils.HelperUtils.handleDeleteRequest;
import static org.folio.orders.utils.HelperUtils.handleGetRequest;
import static org.folio.orders.utils.HelperUtils.handlePutRequest;
import static org.folio.orders.utils.ResourcePathResolver.ORDER_TEMPLATES;
import static org.folio.orders.utils.ResourcePathResolver.resourceByIdPath;
import static org.folio.orders.utils.ResourcePathResolver.resourcesPath;

import java.util.Map;
import java.util.concurrent.CompletableFuture;

import org.folio.rest.jaxrs.model.OrderTemplate;
import org.folio.rest.jaxrs.model.OrderTemplateCollection;

import io.vertx.core.Context;
import io.vertx.core.json.JsonObject;


public class OrderTemplatesHelper extends AbstractHelper {

  private static final String GET_ORDER_TEMPLATES_BY_QUERY = resourcesPath(ORDER_TEMPLATES) + SEARCH_PARAMS;

  public OrderTemplatesHelper(Map<String, String> okapiHeaders, Context ctx, String lang) {
    super(getHttpClient(okapiHeaders), okapiHeaders, ctx, lang);
  }

  public CompletableFuture<OrderTemplate> createOrderTemplate(OrderTemplate template) {
    return createRecordInStorage(JsonObject.mapFrom(template), resourcesPath(ORDER_TEMPLATES)).thenApply(template::withId);
  }

  public CompletableFuture<Void> updateOrderTemplate(OrderTemplate template) {
    String endpoint = resourceByIdPath(ORDER_TEMPLATES, template.getId());
    return handlePutRequest(endpoint, JsonObject.mapFrom(template), httpClient, okapiHeaders, logger);
  }

  public CompletableFuture<OrderTemplate> getOrderTemplateById(String id) {
    return handleGetRequest(resourceByIdPath(ORDER_TEMPLATES, id), httpClient, okapiHeaders, logger)
      .thenApply(json -> json.mapTo(OrderTemplate.class));
  }

  public CompletableFuture<OrderTemplateCollection> getOrderTemplates(String query, int offset, int limit) {
    CompletableFuture<OrderTemplateCollection> future = new CompletableFuture<>();
    try {
      String endpoint = String.format(GET_ORDER_TEMPLATES_BY_QUERY, limit, offset, buildQuery(query, logger), lang);
      handleGetRequest(endpoint, httpClient, okapiHeaders, logger)
        .thenCompose(json -> CompletableFuture.supplyAsync(() -> json.mapTo(OrderTemplateCollection.class)))
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

  public CompletableFuture<Void> deleteOrderTemplate(String id) {
    return handleDeleteRequest(resourceByIdPath(ORDER_TEMPLATES, id), httpClient, okapiHeaders, logger);
  }
}
