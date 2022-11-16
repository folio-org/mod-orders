package org.folio.service;

import static org.folio.orders.utils.HelperUtils.buildQuery;
import static org.folio.orders.utils.ResourcePathResolver.ORDER_TEMPLATES;
import static org.folio.orders.utils.ResourcePathResolver.resourceByIdPath;
import static org.folio.orders.utils.ResourcePathResolver.resourcesPath;
import static org.folio.rest.RestConstants.SEARCH_PARAMS;

import org.folio.rest.core.RestClient;
import org.folio.rest.core.models.RequestContext;
import org.folio.rest.jaxrs.model.OrderTemplate;
import org.folio.rest.jaxrs.model.OrderTemplateCollection;

import io.vertx.core.Future;
import io.vertx.core.Promise;

public class OrderTemplatesService {
  private final RestClient restClient;
  private static final String GET_ORDER_TEMPLATES_BY_QUERY = resourcesPath(ORDER_TEMPLATES) + SEARCH_PARAMS;

  public OrderTemplatesService() {
    this.restClient = new RestClient();

  }

  public Future<OrderTemplate> createOrderTemplate(OrderTemplate template, RequestContext requestContext) {
    return restClient.post(resourcesPath(ORDER_TEMPLATES), template, OrderTemplate.class, requestContext);
  }

  public Future<Void> updateOrderTemplate(OrderTemplate template, RequestContext requestContext) {
    String endpoint = resourceByIdPath(ORDER_TEMPLATES, template.getId());
    return restClient.put(endpoint, template, requestContext);
  }

  public Future<OrderTemplate> getOrderTemplateById(String id, RequestContext requestContext) {
    return restClient.getById(resourcesPath(ORDER_TEMPLATES), id, false,  OrderTemplate.class, requestContext);
  }

  public Future<OrderTemplateCollection> getOrderTemplates(String query, int offset, int limit, RequestContext requestContext) {
    Promise<OrderTemplateCollection> promise = Promise.promise();
    try {
      String endpoint = String.format(GET_ORDER_TEMPLATES_BY_QUERY, limit, offset, buildQuery(query));
      restClient.get(endpoint, OrderTemplateCollection.class, requestContext)
        .onSuccess(promise::complete)
        .onFailure(t -> promise.fail(t.getCause()));
    } catch (Exception e) {
      promise.fail(e);
    }
    return promise.future();
  }

  public Future<Void> deleteOrderTemplate(String id, RequestContext requestContext) {
    return restClient.delete(resourceByIdPath(ORDER_TEMPLATES, id), requestContext);
  }
}
