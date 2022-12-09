package org.folio.rest.impl;

import static io.vertx.core.Future.succeededFuture;
import static org.folio.orders.utils.HelperUtils.ORDER_CONFIG_MODULE_NAME;
import static org.folio.orders.utils.ResourcePathResolver.PO_LINES_BUSINESS;
import static org.folio.orders.utils.ResourcePathResolver.resourceByIdPath;
import static org.folio.rest.RestConstants.OKAPI_URL;

import java.util.Map;

import javax.ws.rs.core.Response;

import org.apache.commons.collections4.CollectionUtils;
import org.apache.commons.lang3.StringUtils;
import org.apache.logging.log4j.LogManager;
import org.apache.logging.log4j.Logger;
import org.folio.helper.PurchaseOrderHelper;
import org.folio.rest.RestConstants;
import org.folio.rest.annotations.Validate;
import org.folio.rest.core.exceptions.HttpException;
import org.folio.rest.core.models.RequestContext;
import org.folio.rest.jaxrs.model.CompositePurchaseOrder;
import org.folio.rest.jaxrs.model.Errors;
import org.folio.rest.jaxrs.model.LedgerFiscalYearRollover;
import org.folio.rest.jaxrs.resource.OrdersCompositeOrders;
import org.folio.rest.jaxrs.resource.OrdersRollover;
import org.folio.service.configuration.ConfigurationEntriesService;
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

  @Autowired
  private OrderRolloverService orderRolloverService;
  @Autowired
  private OrderReEncumberService orderReEncumberService;
  @Autowired
  private ConfigurationEntriesService configurationEntriesService;
  @Autowired
  private PurchaseOrderHelper purchaseOrderHelper;

  public OrdersApi(Vertx vertx, String tenantId) {
    SpringContextUtil.autowireDependencies(this, Vertx.currentContext());
  }

  @Override
  @Validate
  public void deleteOrdersCompositeOrdersById(String id, String lang, Map<String, String> okapiHeaders,
      Handler<AsyncResult<Response>> asyncResultHandler, Context vertxContext) {

    purchaseOrderHelper.deleteOrder(id, new RequestContext(vertxContext, okapiHeaders))
      .onSuccess(v -> asyncResultHandler.handle(succeededFuture(buildNoContentResponse())))
      .onFailure(t -> handleErrorResponse(asyncResultHandler, t));
  }

  @Override
  @Validate
  public void getOrdersCompositeOrdersById(String id, String lang, Map<String, String> okapiHeaders,
      Handler<AsyncResult<Response>> asyncResultHandler, Context vertxContext) {

    purchaseOrderHelper.getCompositeOrder(id, new RequestContext(vertxContext, okapiHeaders))
      .onSuccess(order -> asyncResultHandler.handle(succeededFuture(buildOkResponse(order))))
      .onFailure(t -> handleErrorResponse(asyncResultHandler, t));
  }

  @Override
  @Validate
  public void postOrdersCompositeOrders(String lang, CompositePurchaseOrder compPO, Map<String, String> okapiHeaders,
      Handler<AsyncResult<Response>> asyncResultHandler, Context vertxContext) {

    RequestContext requestContext = new RequestContext(vertxContext, okapiHeaders);
    // First validate content of the PO and proceed only if all is okay
    configurationEntriesService.loadConfiguration(ORDER_CONFIG_MODULE_NAME, requestContext)
      .compose(tenantConfig -> purchaseOrderHelper.validateOrder(compPO, tenantConfig, requestContext))
      .compose(errors -> {
        if (CollectionUtils.isEmpty(errors)) {
          logger.info("Creating PO and POLines...");
          return purchaseOrderHelper.createPurchaseOrder(compPO, requestContext)
            .onSuccess(withIds -> {
              logger.info("Successfully Placed Order: {}", JsonObject.mapFrom(withIds).encodePrettily());
              String okapiUrl = okapiHeaders.get(OKAPI_URL);
              String url = resourceByIdPath(PO_LINES_BUSINESS, compPO.getId());
              asyncResultHandler.handle(succeededFuture(buildResponseWithLocation(okapiUrl, url, compPO)));
            });
        } else {
          throw new HttpException(422, new Errors().withErrors(errors).withTotalRecords(errors.size()));
        }
      })
      .onFailure(t -> handleErrorResponse(asyncResultHandler, t));
  }

  @Override
  @Validate
  public void putOrdersCompositeOrdersById(String orderId, String lang, CompositePurchaseOrder compPO,
      Map<String, String> okapiHeaders, Handler<AsyncResult<Response>> asyncResultHandler, Context vertxContext) {
    // Set order id from path if not specified in body
    populateOrderId(orderId, compPO);

    RequestContext requestContext = new RequestContext(vertxContext, okapiHeaders);
    purchaseOrderHelper.validateExistingOrder(orderId, compPO, requestContext)
      .compose(validationErrors -> {
        if (CollectionUtils.isEmpty(validationErrors)) {
          return purchaseOrderHelper.updateOrder(compPO, requestContext)
            .onSuccess(v -> {
              if (logger.isInfoEnabled()) {
                logger.info("Successfully Updated Order: {}", JsonObject.mapFrom(compPO).encodePrettily());
              }
              asyncResultHandler.handle(succeededFuture(buildNoContentResponse()));
            });
        } else {
          Errors errors = new Errors().withErrors(validationErrors).withTotalRecords(validationErrors.size());
          logger.error("Validation error. Failed to update purchase order : {}", JsonObject.mapFrom(errors).encodePrettily());
          throw new HttpException(RestConstants.VALIDATION_ERROR, errors);
        }
      })
      .onFailure(t -> handleErrorResponse(asyncResultHandler, t));
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

    purchaseOrderHelper
      .getPurchaseOrders(limit, offset, query, new RequestContext(vertxContext, okapiHeaders))
      .onSuccess(orders -> {
        if (logger.isInfoEnabled()) {
          logger.info("Successfully retrieved orders: {}", JsonObject.mapFrom(orders).encodePrettily());
        }
        asyncResultHandler.handle(succeededFuture(buildOkResponse(orders)));
      })
      .onFailure(t -> handleErrorResponse(asyncResultHandler, t));
  }

  @Override
  @Validate
  public void postOrdersCompositeOrdersReEncumberById(String id, String lang, Map<String, String> okapiHeaders, Handler<AsyncResult<Response>> asyncResultHandler, Context vertxContext) {
    orderReEncumberService.reEncumber(id, new RequestContext(vertxContext, okapiHeaders))
      .onSuccess(v -> asyncResultHandler.handle(succeededFuture(buildNoContentResponse())))
      .onFailure(fail -> handleErrorResponse(asyncResultHandler, fail));
  }

  @Override
  @Validate
  public void postOrdersRollover(String lang, LedgerFiscalYearRollover ledgerFYRollover, Map<String, String> okapiHeaders, Handler<AsyncResult<Response>> asyncResultHandler, Context vertxContext) {
    orderRolloverService.rollover(ledgerFYRollover, new RequestContext(vertxContext, okapiHeaders))
      .onSuccess(v -> asyncResultHandler.handle(succeededFuture(buildNoContentResponse())))
      .onFailure(fail -> handleErrorResponse(asyncResultHandler, fail));
  }
}
