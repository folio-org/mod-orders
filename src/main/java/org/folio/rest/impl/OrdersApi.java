package org.folio.rest.impl;

import static io.vertx.core.Future.succeededFuture;
import static org.folio.orders.utils.ErrorCodes.GENERIC_ERROR_CODE;

import java.util.Map;

import javax.ws.rs.core.Response;

import org.apache.commons.collections4.CollectionUtils;
import org.apache.commons.lang3.StringUtils;
import org.apache.logging.log4j.LogManager;
import org.apache.logging.log4j.Logger;
import org.folio.completablefuture.FolioVertxCompletableFuture;
import org.folio.helper.PurchaseOrderHelper;
import org.folio.orders.rest.exceptions.HttpException;
import org.folio.orders.utils.HelperUtils;
import org.folio.rest.annotations.Validate;
import org.folio.rest.core.models.RequestContext;
import org.folio.rest.jaxrs.model.CompositePurchaseOrder;
import org.folio.rest.jaxrs.model.LedgerFiscalYearRollover;
import org.folio.rest.jaxrs.resource.OrdersCompositeOrders;
import org.folio.rest.jaxrs.resource.OrdersRollover;
import org.folio.service.orders.OrderReEncumberService;
import org.folio.service.orders.OrderRolloverService;
import org.folio.spring.SpringContextUtil;
import org.springframework.beans.factory.annotation.Autowired;

import io.vertx.core.AsyncResult;
import io.vertx.core.Context;
import io.vertx.core.Handler;
import io.vertx.core.Vertx;
import io.vertx.core.json.JsonObject;

public class OrdersApi extends BaseApi implements OrdersCompositeOrders, OrdersRollover {

  private static final Logger logger = LogManager.getLogger();

  private static final String ORDERS_LOCATION_PREFIX = "/orders/composite-orders/%s";

  @Autowired
  private OrderRolloverService orderRolloverService;
  @Autowired
  private OrderReEncumberService orderReEncumberService;

  public OrdersApi(Vertx vertx, String tenantId) {
    SpringContextUtil.autowireDependencies(this, Vertx.currentContext());
  }

  @Override
  @Validate
  public void deleteOrdersCompositeOrdersById(String id, String lang, Map<String, String> okapiHeaders,
      Handler<AsyncResult<Response>> asyncResultHandler, Context vertxContext) {

    PurchaseOrderHelper helper = new PurchaseOrderHelper(okapiHeaders, vertxContext, lang);
    helper
      .deleteOrder(id)
      .thenAccept(v -> asyncResultHandler.handle(succeededFuture(helper.buildNoContentResponse())))
      .exceptionally(t -> HelperUtils.handleErrorResponse(asyncResultHandler, helper, t));
  }

  @Override
  @Validate
  public void getOrdersCompositeOrdersById(String id, String lang, Map<String, String> okapiHeaders,
      Handler<AsyncResult<Response>> asyncResultHandler, Context vertxContext) {

    PurchaseOrderHelper helper = new PurchaseOrderHelper(okapiHeaders, vertxContext, lang);
    helper
      .getCompositeOrder(id)
      .thenAccept(order -> asyncResultHandler.handle(succeededFuture(helper.buildOkResponse(order))))
      .exceptionally(t -> HelperUtils.handleErrorResponse(asyncResultHandler, helper, t));
  }

  @Override
  @Validate
  public void postOrdersCompositeOrders(String lang, CompositePurchaseOrder compPO, Map<String, String> okapiHeaders,
      Handler<AsyncResult<Response>> asyncResultHandler, Context vertxContext) {

    PurchaseOrderHelper helper = new PurchaseOrderHelper(okapiHeaders, vertxContext, lang);

    // First validate content of the PO and proceed only if all is okay
    helper
      .validateOrder(compPO, new RequestContext(vertxContext, okapiHeaders))
      .thenCompose(isValid -> {
        if (Boolean.TRUE.equals(isValid)) {
          logger.info("Creating PO and POLines...");
          return helper.createPurchaseOrder(compPO, new RequestContext(vertxContext, okapiHeaders))
            .thenAccept(withIds -> {
              logger.info("Successfully Placed Order: {}", JsonObject.mapFrom(withIds).encodePrettily());
              asyncResultHandler.handle(succeededFuture(helper
                .buildResponseWithLocation(String.format(ORDERS_LOCATION_PREFIX, withIds.getId()), withIds)));
            });
        } else {
          throw new HttpException(422, GENERIC_ERROR_CODE);
        }
      })
      .exceptionally(t -> HelperUtils.handleErrorResponse(asyncResultHandler, helper, t));
  }

  @Override
  @Validate
  public void putOrdersCompositeOrdersById(String orderId, String lang, CompositePurchaseOrder compPO,
      Map<String, String> okapiHeaders, Handler<AsyncResult<Response>> asyncResultHandler, Context vertxContext) {
    // Set order id from path if not specified in body
    populateOrderId(orderId, compPO);

    PurchaseOrderHelper helper = new PurchaseOrderHelper(okapiHeaders, vertxContext, lang);
    RequestContext requestContext = helper.getRequestContext();
    helper.validateExistingOrder(orderId, compPO, requestContext)
      .thenCompose(isValid -> {
        logger.info("Order is valid: {}", isValid);
        if (Boolean.TRUE.equals(isValid)) {
          return helper.updateOrder(compPO, requestContext)
            .thenAccept(v -> {
              if (logger.isInfoEnabled()) {
                logger.info("Successfully Updated Order: {}", JsonObject.mapFrom(compPO).encodePrettily());
              }
              asyncResultHandler.handle(succeededFuture(helper.buildNoContentResponse()));
            });
        } else {
          logger.error("Validation error. Failed to update purchase order with id={}", orderId);
          return FolioVertxCompletableFuture.runAsync(vertxContext, () -> asyncResultHandler.handle(succeededFuture(helper.buildErrorResponse(422))));
        }
      })
      .exceptionally(t -> {
        logger.error("Failed to update purchase order with id={}", orderId, t);
        return HelperUtils.handleErrorResponse(asyncResultHandler, helper, t);
      });
  }

  private void populateOrderId(String orderId, CompositePurchaseOrder compPO) {
    if (StringUtils.isEmpty(compPO.getId())) {
      compPO.setId(orderId);
    }
    if (CollectionUtils.isNotEmpty(compPO.getCompositePoLines())) {
      compPO.getCompositePoLines().forEach(poLine -> {
        if (StringUtils.isEmpty(poLine.getPurchaseOrderId())) {
          poLine.setPurchaseOrderId(orderId);
        }
      });
    }
  }

  @Override
  @Validate
  public void getOrdersCompositeOrders(int offset, int limit, String query, String lang,
      Map<String, String> okapiHeaders, Handler<AsyncResult<Response>> asyncResultHandler, Context vertxContext) {
    PurchaseOrderHelper helper = new PurchaseOrderHelper(okapiHeaders, vertxContext, lang);
    helper
      .getPurchaseOrders(limit, offset, query)
      .thenAccept(orders -> {
        if (logger.isInfoEnabled()) {
          logger.info("Successfully retrieved orders: {}", JsonObject.mapFrom(orders).encodePrettily());
        }
        asyncResultHandler.handle(succeededFuture(helper.buildOkResponse(orders)));
      })
      .exceptionally(t -> HelperUtils.handleErrorResponse(asyncResultHandler, helper, t));
  }

  @Override
  @Validate
  public void postOrdersCompositeOrdersReEncumberById(String id, String lang, Map<String, String> okapiHeaders, Handler<AsyncResult<Response>> asyncResultHandler, Context vertxContext) {
    orderReEncumberService.reEncumber(id, new RequestContext(vertxContext, okapiHeaders))
          .thenAccept(v -> asyncResultHandler.handle(succeededFuture(buildNoContentResponse())))
          .exceptionally(fail -> handleErrorResponse(asyncResultHandler, fail));
  }

  @Override
  @Validate
  public void postOrdersRollover(String lang, LedgerFiscalYearRollover ledgerFYRollover, Map<String, String> okapiHeaders, Handler<AsyncResult<Response>> asyncResultHandler, Context vertxContext) {
    orderRolloverService.rollover(ledgerFYRollover, new RequestContext(vertxContext, okapiHeaders))
        .thenAccept(v -> asyncResultHandler.handle(succeededFuture(buildNoContentResponse())))
        .exceptionally(fail -> handleErrorResponse(asyncResultHandler, fail));
  }
}
