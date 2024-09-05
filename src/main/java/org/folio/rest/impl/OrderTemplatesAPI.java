package org.folio.rest.impl;

import static io.vertx.core.Future.succeededFuture;
import static org.folio.orders.utils.HelperUtils.handleErrorResponse;
import static org.folio.rest.core.exceptions.ErrorCodes.MISMATCH_BETWEEN_ID_IN_PATH_AND_BODY;
import static org.folio.rest.core.exceptions.ErrorCodes.TEMPLATE_NAME_ALREADY_EXISTS;

import java.util.Map;

import javax.ws.rs.core.Response;

import org.apache.commons.lang3.StringUtils;
import org.apache.logging.log4j.LogManager;
import org.apache.logging.log4j.Logger;
import org.folio.helper.OrderTemplatesHelper;
import org.folio.rest.annotations.Validate;
import org.folio.rest.core.exceptions.HttpException;
import org.folio.rest.jaxrs.model.OrderTemplate;
import org.folio.rest.jaxrs.resource.OrdersOrderTemplates;

import io.vertx.core.AsyncResult;
import io.vertx.core.Context;
import io.vertx.core.Handler;
import io.vertx.core.json.JsonObject;

public class OrderTemplatesAPI implements OrdersOrderTemplates {

  private static final Logger logger = LogManager.getLogger();
  private static final String ORDER_TEMPLATE_LOCATION_PREFIX = "/orders/order-templates/%s";
  private static final String TEMPLATE_NAME_ALREADY_EXIST_ERROR = "lower(f_unaccent(jsonb ->> 'templateName'::text)) value already exists in table order_templates";

  @Override
  @Validate
  public void postOrdersOrderTemplates(OrderTemplate entity, Map<String, String> okapiHeaders,
      Handler<AsyncResult<Response>> asyncResultHandler, Context vertxContext) {
    OrderTemplatesHelper orderTemplatesHelper = new OrderTemplatesHelper(okapiHeaders, vertxContext);
    orderTemplatesHelper.createOrderTemplate(entity)
      .onSuccess(template -> {
        if (logger.isDebugEnabled()) {
          logger.debug("Successfully created new order template: {}", JsonObject.mapFrom(template)
            .encodePrettily());
        }
        asyncResultHandler.handle(succeededFuture(
          orderTemplatesHelper.buildResponseWithLocation(String.format(ORDER_TEMPLATE_LOCATION_PREFIX, template.getId()), template)));
      })
      .onFailure(t -> handlePostPutErrorResponse(asyncResultHandler, t, orderTemplatesHelper));
  }

  @Override
  @Validate
  public void getOrdersOrderTemplates(String query, String totalRecords, int offset, int limit, Map<String, String> okapiHeaders,
      Handler<AsyncResult<Response>> asyncResultHandler, Context vertxContext) {
    OrderTemplatesHelper helper = new OrderTemplatesHelper(okapiHeaders, vertxContext);
    helper.getOrderTemplates(query, offset, limit)
      .onSuccess(templates -> {
        if (logger.isDebugEnabled()) {
          logger.debug("Successfully retrieved order templates collection: {}", JsonObject.mapFrom(templates).encodePrettily());
        }
        asyncResultHandler.handle(succeededFuture(helper.buildOkResponse(templates)));
      })
      .onFailure(t -> handleErrorResponse(asyncResultHandler, helper, t));
  }

  @Override
  @Validate
  public void putOrdersOrderTemplatesById(String id, OrderTemplate entity, Map<String, String> okapiHeaders,
      Handler<AsyncResult<Response>> asyncResultHandler, Context vertxContext) {
    OrderTemplatesHelper helper = new OrderTemplatesHelper(okapiHeaders, vertxContext);

    // Set template id if this is available only in path
    if (StringUtils.isEmpty(entity.getId())) {
      entity.setId(id);
    }

    if (!entity.getId().equals(id)) {
      helper.addProcessingError(MISMATCH_BETWEEN_ID_IN_PATH_AND_BODY.toError());
      asyncResultHandler.handle(succeededFuture(helper.buildErrorResponse(422)));
    } else {
      helper.updateOrderTemplate(entity.withId(id))
        .onSuccess(template -> {
          logger.debug("Successfully updated order template with id={}", id);
          asyncResultHandler.handle(succeededFuture(helper.buildNoContentResponse()));
        })
        .onFailure(t -> handlePostPutErrorResponse(asyncResultHandler, t, helper));
    }
  }

  private static void handlePostPutErrorResponse(Handler<AsyncResult<Response>> asyncResultHandler, Throwable t, OrderTemplatesHelper helper) {
    if (StringUtils.isNotEmpty(t.getMessage()) && t.getMessage().contains(TEMPLATE_NAME_ALREADY_EXIST_ERROR)) {
      handleErrorResponse(asyncResultHandler, helper,
        new HttpException(422, TEMPLATE_NAME_ALREADY_EXISTS.toError()));
    } else {
      handleErrorResponse(asyncResultHandler, helper, t);
    }
  }

  @Override
  @Validate
  public void getOrdersOrderTemplatesById(String id, Map<String, String> okapiHeaders,
      Handler<AsyncResult<Response>> asyncResultHandler, Context vertxContext) {
    OrderTemplatesHelper helper = new OrderTemplatesHelper(okapiHeaders, vertxContext);
    helper.getOrderTemplateById(id)
      .onSuccess(template -> {
        if (logger.isDebugEnabled()) {
          logger.debug("Successfully retrieved order template: {}", JsonObject.mapFrom(template)
            .encodePrettily());
        }
        asyncResultHandler.handle(succeededFuture(helper.buildOkResponse(template)));
      })
      .onFailure(t -> handleErrorResponse(asyncResultHandler, helper, t));
  }

  @Override
  @Validate
  public void deleteOrdersOrderTemplatesById(String id, Map<String, String> okapiHeaders,
      Handler<AsyncResult<Response>> asyncResultHandler, Context vertxContext) {
    OrderTemplatesHelper helper = new OrderTemplatesHelper(okapiHeaders, vertxContext);
    helper.deleteOrderTemplate(id)
      .onSuccess(ok -> {
        if (logger.isDebugEnabled()) {
          logger.debug("Successfully deleted order template with id={}", id);
        }
        asyncResultHandler.handle(succeededFuture(helper.buildNoContentResponse()));
      })
      .onFailure(t -> handleErrorResponse(asyncResultHandler, helper, t));
  }
}
