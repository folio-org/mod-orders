package org.folio.helper;

import java.util.Map;

import org.folio.rest.core.models.RequestContext;
import org.folio.rest.jaxrs.model.OrderTemplate;
import org.folio.rest.jaxrs.model.OrderTemplateCollection;
import org.folio.service.OrderTemplatesService;
import org.folio.spring.SpringContextUtil;
import org.springframework.beans.factory.annotation.Autowired;

import io.vertx.core.Context;
import io.vertx.core.Future;
import io.vertx.core.Vertx;

public class OrderTemplatesHelper extends BaseHelper {

  @Autowired
  OrderTemplatesService orderTemplatesService;
  private final RequestContext requestContext;

  public OrderTemplatesHelper(Map<String, String> okapiHeaders, Context ctx) {
    super(okapiHeaders, ctx);
    this.requestContext = new RequestContext(ctx, okapiHeaders);
    SpringContextUtil.autowireDependencies(this, Vertx.currentContext());
  }

  public Future<OrderTemplate> createOrderTemplate(OrderTemplate template) {
    return orderTemplatesService.createOrderTemplate(template, requestContext);
  }

  public Future<Void> updateOrderTemplate(OrderTemplate template) {
    return orderTemplatesService.updateOrderTemplate(template, requestContext);
  }

  public Future<OrderTemplate> getOrderTemplateById(String id) {
    return orderTemplatesService.getOrderTemplateById(id, requestContext);
  }

  public Future<OrderTemplateCollection> getOrderTemplates(String query, int offset, int limit) {
    return orderTemplatesService.getOrderTemplates(query, offset, limit, requestContext);
  }

  public Future<Void> deleteOrderTemplate(String id) {
    return orderTemplatesService.deleteOrderTemplate(id, requestContext);
  }

}
