package org.folio.rest.impl;

import static io.vertx.core.Future.succeededFuture;
import static org.folio.orders.utils.ResourcePathResolver.ORDERS_BUSINESS;
import static org.folio.orders.utils.ResourcePathResolver.resourceByIdPath;
import static org.folio.rest.RestConstants.OKAPI_URL;

import java.util.Map;

import javax.ws.rs.core.Response;

import org.folio.helper.PurchaseOrderHelper;
import org.folio.rest.annotations.Validate;
import org.folio.rest.core.models.RequestContext;
import org.folio.rest.jaxrs.model.CompositePurchaseOrder;
import org.folio.rest.jaxrs.model.LedgerFiscalYearRollover;
import org.folio.rest.jaxrs.resource.OrdersCompositeOrders;
import org.folio.rest.jaxrs.resource.OrdersRollover;
import org.folio.service.orders.OrderFiscalYearService;
import org.folio.service.orders.OrderReEncumberService;
import org.folio.service.orders.OrderRolloverService;
import org.folio.spring.SpringContextUtil;
import org.springframework.beans.factory.annotation.Autowired;

import io.vertx.core.AsyncResult;
import io.vertx.core.Context;
import io.vertx.core.Handler;
import io.vertx.core.Vertx;

public class OrdersApi extends BaseApi implements OrdersCompositeOrders, OrdersRollover {

  @Autowired
  private OrderRolloverService orderRolloverService;
  @Autowired
  private OrderReEncumberService orderReEncumberService;
  @Autowired
  private PurchaseOrderHelper purchaseOrderHelper;
  @Autowired
  private OrderFiscalYearService orderFiscalYearService;

  public OrdersApi(Vertx vertx, String tenantId) {
    SpringContextUtil.autowireDependencies(this, Vertx.currentContext());
  }

  @Override
  @Validate
  public void deleteOrdersCompositeOrdersById(String id, Map<String, String> okapiHeaders,
      Handler<AsyncResult<Response>> asyncResultHandler, Context vertxContext) {

    purchaseOrderHelper.deleteOrder(id, new RequestContext(vertxContext, okapiHeaders))
      .onSuccess(v -> asyncResultHandler.handle(succeededFuture(buildNoContentResponse())))
      .onFailure(t -> handleErrorResponse(asyncResultHandler, t));
  }

  @Override
  @Validate
  public void getOrdersCompositeOrdersById(String id, String fiscalYearId, Map<String, String> okapiHeaders,
      Handler<AsyncResult<Response>> asyncResultHandler, Context vertxContext) {

    purchaseOrderHelper.getCompositeOrder(id, fiscalYearId, new RequestContext(vertxContext, okapiHeaders))
      .onSuccess(order -> asyncResultHandler.handle(succeededFuture(buildOkResponse(order))))
      .onFailure(t -> handleErrorResponse(asyncResultHandler, t));
  }

  @Override
  @Validate
  public void postOrdersCompositeOrders(CompositePurchaseOrder compPO, Map<String, String> okapiHeaders,
      Handler<AsyncResult<Response>> asyncResultHandler, Context vertxContext) {
    RequestContext requestContext = new RequestContext(vertxContext, okapiHeaders);
    purchaseOrderHelper.postCompositeOrder(compPO, requestContext)
      .onSuccess(createdOrder -> {
        String okapiUrl = okapiHeaders.get(OKAPI_URL);
        String url = resourceByIdPath(ORDERS_BUSINESS, compPO.getId());
        asyncResultHandler.handle(succeededFuture(buildResponseWithLocation(okapiUrl, url, createdOrder)));
      })
      .onFailure(t -> handleErrorResponse(asyncResultHandler, t));
  }

  @Override
  @Validate
  public void putOrdersCompositeOrdersById(String orderId, boolean deleteHoldings, CompositePurchaseOrder compPO,
      Map<String, String> okapiHeaders, Handler<AsyncResult<Response>> asyncResultHandler, Context vertxContext) {
    RequestContext requestContext = new RequestContext(vertxContext, okapiHeaders);
    purchaseOrderHelper.putCompositeOrderById(orderId, deleteHoldings, compPO, requestContext)
      .onSuccess(v -> asyncResultHandler.handle(succeededFuture(buildNoContentResponse())))
      .onFailure(t -> handleErrorResponse(asyncResultHandler, t));
  }

  @Override
  @Validate
  public void getOrdersCompositeOrders(String totalRecords, int offset, int limit, String query,
      Map<String, String> okapiHeaders, Handler<AsyncResult<Response>> asyncResultHandler, Context vertxContext) {

    purchaseOrderHelper
      .getPurchaseOrders(limit, offset, query, new RequestContext(vertxContext, okapiHeaders))
      .onSuccess(orders -> asyncResultHandler.handle(succeededFuture(buildOkResponse(orders))))
      .onFailure(t -> handleErrorResponse(asyncResultHandler, t));
  }

  @Override
  @Validate
  public void postOrdersCompositeOrdersReEncumberById(String id, Map<String, String> okapiHeaders, Handler<AsyncResult<Response>> asyncResultHandler, Context vertxContext) {
    orderReEncumberService.reEncumber(id, new RequestContext(vertxContext, okapiHeaders))
      .onSuccess(v -> asyncResultHandler.handle(succeededFuture(buildNoContentResponse())))
      .onFailure(fail -> handleErrorResponse(asyncResultHandler, fail));
  }

  @Override
  @Validate
  public void postOrdersRollover(LedgerFiscalYearRollover ledgerFYRollover, Map<String, String> okapiHeaders, Handler<AsyncResult<Response>> asyncResultHandler, Context vertxContext) {
    orderRolloverService.rollover(ledgerFYRollover, new RequestContext(vertxContext, okapiHeaders))
      .onSuccess(v -> asyncResultHandler.handle(succeededFuture(buildNoContentResponse())))
      .onFailure(fail -> handleErrorResponse(asyncResultHandler, fail));
  }

  @Override
  @Validate
  public void getOrdersCompositeOrdersFiscalYearsById(String id, Map<String, String> okapiHeaders, Handler<AsyncResult<Response>> asyncResultHandler, Context vertxContext) {
    orderFiscalYearService.getAvailableFiscalYears(id, new RequestContext(vertxContext, okapiHeaders))
      .onSuccess(fiscalYears -> asyncResultHandler.handle(succeededFuture(buildOkResponse(fiscalYears))))
      .onFailure(t -> handleErrorResponse(asyncResultHandler, t));
  }
}
