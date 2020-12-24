package org.folio.rest.impl;

import static io.vertx.core.Future.succeededFuture;
import static org.folio.orders.utils.HelperUtils.handleErrorResponse;
import static org.folio.orders.utils.validators.CompositePoLineValidationUtil.validatePoLine;

import java.util.ArrayList;
import java.util.List;
import java.util.Map;

import javax.ws.rs.core.Response;

import org.apache.commons.lang3.StringUtils;
import org.apache.logging.log4j.LogManager;
import org.apache.logging.log4j.Logger;
import org.folio.helper.PurchaseOrderLineHelper;
import org.folio.orders.rest.exceptions.HttpException;
import org.folio.orders.utils.ErrorCodes;
import org.folio.rest.annotations.Validate;
import org.folio.rest.jaxrs.model.CompositePoLine;
import org.folio.rest.jaxrs.model.Error;
import org.folio.rest.jaxrs.model.Errors;
import org.folio.rest.jaxrs.resource.OrdersOrderLines;

import io.vertx.core.AsyncResult;
import io.vertx.core.Context;
import io.vertx.core.Handler;
import io.vertx.core.json.JsonObject;

public class CompositePoLineAPI implements OrdersOrderLines {

  private static final Logger logger = LogManager.getLogger();

  private static final String ORDER_LINE_LOCATION_PREFIX = "/orders/order-lines/%s";

  @Override
  @Validate
  public void getOrdersOrderLines(int offset, int limit, String query, String lang, Map<String, String> okapiHeaders,
      Handler<AsyncResult<Response>> asyncResultHandler, Context vertxContext) {
    PurchaseOrderLineHelper helper = new PurchaseOrderLineHelper(okapiHeaders, vertxContext, lang);
    helper.getOrderLines(limit, offset, query)
      .thenAccept(lines -> asyncResultHandler.handle(succeededFuture(helper.buildOkResponse(lines))))
      .exceptionally(t -> handleErrorResponse(asyncResultHandler, helper, t));
  }

  @Override
  @Validate
  public void postOrdersOrderLines(String lang, CompositePoLine poLine, Map<String, String> okapiHeaders,
      Handler<AsyncResult<Response>> asyncResultHandler, Context vertxContext) {

    PurchaseOrderLineHelper helper = new PurchaseOrderLineHelper(okapiHeaders, vertxContext, lang);

    logger.info("Creating POLine to an existing order...");

    // The validation of the PO Line content and its order state is done in
    // scope of the 'createPoLine' method logic
    helper.createPoLine(poLine)
      .thenAccept(pol -> {
        if (helper.getErrors()
          .isEmpty()) {
          if (logger.isInfoEnabled()) {
            logger.info("Successfully added PO Line: {}", JsonObject.mapFrom(pol)
              .encodePrettily());
          }
          asyncResultHandler
            .handle(succeededFuture(helper.buildResponseWithLocation(String.format(ORDER_LINE_LOCATION_PREFIX, pol.getId()), pol)));
        } else {
          throw new HttpException(422, "");
        }
      })
      .exceptionally(t -> handleErrorResponse(asyncResultHandler, helper, t));
  }

  @Override
  @Validate
  public void getOrdersOrderLinesById(String lineId, String lang, Map<String, String> okapiHeaders,
      Handler<AsyncResult<Response>> asyncResultHandler, Context vertxContext) {
    logger.info("Started Invocation of POLine Request with id = {}", lineId);
    PurchaseOrderLineHelper helper = new PurchaseOrderLineHelper(okapiHeaders, vertxContext, lang);

    helper.getCompositePoLine(lineId)
      .thenAccept(poLine -> {
        if (logger.isInfoEnabled()) {
          logger.info("Received PO Line Response: {}", JsonObject.mapFrom(poLine)
            .encodePrettily());
        }
        asyncResultHandler.handle(succeededFuture(helper.buildOkResponse(poLine)));
      })
      .exceptionally(t -> handleErrorResponse(asyncResultHandler, helper, t));
  }

  @Override
  @Validate
  public void deleteOrdersOrderLinesById(String lineId, String lang, Map<String, String> okapiHeaders,
      Handler<AsyncResult<Response>> asyncResultHandler, Context vertxContext) {

    PurchaseOrderLineHelper helper = new PurchaseOrderLineHelper(okapiHeaders, vertxContext, lang);
    helper.deleteLine(lineId)
      .thenAccept(v -> asyncResultHandler.handle(succeededFuture(helper.buildNoContentResponse())))
      .exceptionally(t -> handleErrorResponse(asyncResultHandler, helper, t));
  }

  @Override
  @Validate
  public void putOrdersOrderLinesById(String lineId, String lang, CompositePoLine poLine, Map<String, String> okapiHeaders,
      Handler<AsyncResult<Response>> asyncResultHandler, Context vertxContext) {
    logger.info("Handling PUT Order Line operation...");

    PurchaseOrderLineHelper helper = new PurchaseOrderLineHelper(okapiHeaders, vertxContext, lang);
    // Set id if this is available only in path
    if (StringUtils.isEmpty(poLine.getId())) {
      poLine.setId(lineId);
    }

    // First validate content of the PO Line and proceed only if all is okay
    List<Error> errors = new ArrayList<>();

    if (!lineId.equals(poLine.getId())) {
      errors.add(ErrorCodes.MISMATCH_BETWEEN_ID_IN_PATH_AND_BODY.toError());
    }

    helper.setTenantDefaultCreateInventoryValues(poLine)
      .thenAccept(empty -> {
        errors.addAll(validatePoLine(poLine));

        if (!errors.isEmpty()) {
          PutOrdersOrderLinesByIdResponse response = PutOrdersOrderLinesByIdResponse
            .respond422WithApplicationJson(new Errors().withErrors(errors));
          asyncResultHandler.handle(succeededFuture(response));
          return;
        }
        helper.updateOrderLine(poLine)
          .thenAccept(v -> asyncResultHandler.handle(succeededFuture(helper.buildNoContentResponse())))
          .exceptionally(t -> handleErrorResponse(asyncResultHandler, helper, t));
      })
      .exceptionally(t -> handleErrorResponse(asyncResultHandler, helper, t));
  }

}
