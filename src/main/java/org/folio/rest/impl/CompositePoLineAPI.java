package org.folio.rest.impl;

import static io.vertx.core.Future.succeededFuture;
import static org.folio.orders.utils.HelperUtils.handleErrorResponse;
import static org.folio.orders.utils.validators.CompositePoLineValidationUtil.validatePoLine;

import java.util.ArrayList;
import java.util.Collection;
import java.util.Collections;
import java.util.List;
import java.util.Map;
import java.util.concurrent.CompletableFuture;

import javax.ws.rs.core.Response;

import org.apache.commons.lang3.StringUtils;
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
import io.vertx.core.logging.Logger;
import io.vertx.core.logging.LoggerFactory;

public class CompositePoLineAPI implements OrdersOrderLines {

  private static final Logger logger = LoggerFactory.getLogger(CompositePoLineAPI.class);

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
    if (poLine.getPurchaseOrderId() == null) {
      finishWithErrors(asyncResultHandler, Collections.singletonList(ErrorCodes.MISSING_ORDER_ID_IN_POL.toError()));
    } else {
      helper.getCompositePurchaseOrder(poLine.getPurchaseOrderId())
        .thenCompose(compPO -> helper.saveValidPoLine(compPO, poLine))
        .thenAccept(pol -> {
          if (helper.getErrors().isEmpty()) {
            if (logger.isInfoEnabled()) {
              logger.info("Successfully added PO Line: " + JsonObject.mapFrom(pol).encodePrettily());
            }
            asyncResultHandler
              .handle(succeededFuture(helper.buildResponseWithLocation(String.format(ORDER_LINE_LOCATION_PREFIX, pol.getId()), pol)));
          } else {
            throw new HttpException(422, "");
          }
        })
        .exceptionally(t -> handleErrorResponse(asyncResultHandler, helper, t));
    }
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
    } else if (poLine.getPurchaseOrderId() == null) {
      errors.add(ErrorCodes.MISSING_ORDER_ID_IN_POL.toError());
    }

    if (!errors.isEmpty()) {
      finishWithErrors(asyncResultHandler, errors);
    } else {
      helper.setTenantDefaultCreateInventoryValues(poLine)
        .thenCompose(empty -> helper.getCompositePurchaseOrder(poLine.getPurchaseOrderId()))
        .thenAccept(compPO -> {
          errors.addAll(validatePoLine(compPO.getOrderType().value(), poLine));
          if (finishWithErrors(asyncResultHandler, errors)) return;
          helper.updateOrderLine(compPO, poLine)
            .thenAccept(v -> asyncResultHandler.handle(succeededFuture(helper.buildNoContentResponse())))
            .exceptionally(t -> handleErrorResponse(asyncResultHandler, helper, t));
        })
        .exceptionally(t -> handleErrorResponse(asyncResultHandler, helper, t));
    }
  }

  private boolean finishWithErrors(Handler<AsyncResult<Response>> asyncResultHandler, List<Error> errors) {
    if (!errors.isEmpty()) {
      PutOrdersOrderLinesByIdResponse response = PutOrdersOrderLinesByIdResponse
        .respond422WithApplicationJson(new Errors().withErrors(errors));
      asyncResultHandler.handle(succeededFuture(response));
      return true;
    }
    return false;
  }

}
