package org.folio.rest.impl;

import io.vertx.core.AsyncResult;
import io.vertx.core.Context;
import io.vertx.core.Handler;
import io.vertx.core.Vertx;
import org.folio.rest.annotations.Validate;
import org.folio.rest.core.models.RequestContext;
import org.folio.rest.jaxrs.model.RoutingList;
import org.folio.rest.jaxrs.resource.OrdersRoutingLists;
import org.folio.service.routinglists.RoutingListsService;
import org.folio.spring.SpringContextUtil;
import org.springframework.beans.factory.annotation.Autowired;

import javax.ws.rs.core.Response;
import java.util.Map;

import static io.vertx.core.Future.succeededFuture;

public class RoutingListsAPI extends BaseApi implements OrdersRoutingLists {

  @Autowired
  private RoutingListService routingListsService;

  public RoutingListsAPI() {
    SpringContextUtil.autowireDependencies(this, Vertx.currentContext());
  }

  @Override
  @Validate
  public void getOrdersRoutingLists(String query, String totalRecords, int offset, int limit, Map<String, String> okapiHeaders, Handler<AsyncResult<Response>> asyncResultHandler, Context vertxContext) {
    routingListsStorageService.getRoutingLists(limit, offset, query, new RequestContext(vertxContext, okapiHeaders))
      .onSuccess(lists -> asyncResultHandler.handle(succeededFuture(buildOkResponse(lists))))
      .onFailure(fail -> handleErrorResponse(asyncResultHandler, fail));
  }

  @Override
  @Validate
  public void postOrdersRoutingLists(RoutingList entity, Map<String, String> okapiHeaders, Handler<AsyncResult<Response>> asyncResultHandler, Context vertxContext) {
    routingListsStorageService.createRoutingList(entity, new RequestContext(vertxContext, okapiHeaders))
      .onSuccess(list -> asyncResultHandler.handle(succeededFuture(buildOkResponse(list))))
      .onFailure(fail -> handleErrorResponse(asyncResultHandler, fail));
  }

  @Override
  @Validate
  public void getOrdersRoutingListsById(String id, Map<String, String> okapiHeaders, Handler<AsyncResult<Response>> asyncResultHandler, Context vertxContext) {
    routingListsStorageService.getRoutingList(id, new RequestContext(vertxContext, okapiHeaders))
      .onSuccess(list -> asyncResultHandler.handle(succeededFuture(buildOkResponse(list))))
      .onFailure(fail -> handleErrorResponse(asyncResultHandler, fail));
  }

  @Override
  @Validate
  public void deleteOrdersRoutingListsById(String id, Map<String, String> okapiHeaders, Handler<AsyncResult<Response>> asyncResultHandler, Context vertxContext) {
    routingListsStorageService.deleteRoutingList(id, new RequestContext(vertxContext, okapiHeaders))
      .onSuccess(list -> asyncResultHandler.handle(succeededFuture(buildNoContentResponse())))
      .onFailure(fail -> handleErrorResponse(asyncResultHandler, fail));

  }

  @Override
  @Validate
  public void putOrdersRoutingListsById(String id, RoutingList entity, Map<String, String> okapiHeaders, Handler<AsyncResult<Response>> asyncResultHandler, Context vertxContext) {
    routingListsStorageService.updateRoutingList(entity, new RequestContext(vertxContext, okapiHeaders))
      .onSuccess(list -> asyncResultHandler.handle(succeededFuture(buildOkResponse(list))))
      .onFailure(fail -> handleErrorResponse(asyncResultHandler, fail));
  }

  @Override
  public void getOrdersRoutingListsTemplateById(String id, Map<String, String> okapiHeaders,
                                                Handler<AsyncResult<Response>> asyncResultHandler, Context vertxContext) {
    routingListService.processTemplateRequest(id, new RequestContext(vertxContext, okapiHeaders))
      .onSuccess(jsonObject -> asyncResultHandler.handle(succeededFuture(this.buildOkResponse(jsonObject))))
      .onFailure(t -> handleErrorResponse(asyncResultHandler, t));
  }
}
