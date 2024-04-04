package org.folio.rest.impl;

import static io.vertx.core.Future.succeededFuture;

import javax.ws.rs.core.Response;
import java.util.Map;

import io.vertx.core.AsyncResult;
import io.vertx.core.Context;
import io.vertx.core.Handler;
import io.vertx.core.Vertx;
import org.apache.commons.lang.NotImplementedException;
import org.folio.rest.core.models.RequestContext;
import org.folio.rest.jaxrs.model.RoutingList;
import org.folio.rest.jaxrs.resource.OrdersRoutingLists;
import org.folio.service.RoutingListService;
import org.folio.spring.SpringContextUtil;
import org.springframework.beans.factory.annotation.Autowired;

public class RoutingListsAPI extends BaseApi implements OrdersRoutingLists {

  @Autowired
  private RoutingListService routingListService;

  public RoutingListsAPI() {
    SpringContextUtil.autowireDependencies(this, Vertx.currentContext());
  }

  @Override
  public void getOrdersRoutingLists(String query, String totalRecords, int offset, int limit, Map<String, String> okapiHeaders,
                                    Handler<AsyncResult<Response>> asyncResultHandler, Context vertxContext) {
    throw new NotImplementedException();
  }

  @Override
  public void postOrdersRoutingLists(RoutingList entity, Map<String, String> okapiHeaders,
                                     Handler<AsyncResult<Response>> asyncResultHandler, Context vertxContext) {
    throw new NotImplementedException();
  }

  @Override
  public void getOrdersRoutingListsTemplateById(String id, Map<String, String> okapiHeaders,
                                                Handler<AsyncResult<Response>> asyncResultHandler, Context vertxContext) {
    routingListService.processTemplateRequest(id, new RequestContext(vertxContext, okapiHeaders))
      .onSuccess(jsonObject -> asyncResultHandler.handle(succeededFuture(this.buildOkResponse(jsonObject))))
      .onFailure(t -> handleErrorResponse(asyncResultHandler, t));
  }
}
