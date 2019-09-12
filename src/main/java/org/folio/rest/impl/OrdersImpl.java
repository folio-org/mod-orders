package org.folio.rest.impl;

import static io.vertx.core.Future.succeededFuture;
import static org.folio.orders.utils.ErrorCodes.GENERIC_ERROR_CODE;
import static org.folio.orders.utils.ErrorCodes.MISMATCH_BETWEEN_ID_IN_PATH_AND_BODY;
import static org.folio.orders.utils.HelperUtils.validatePoLine;

import java.util.ArrayList;
import java.util.List;
import java.util.Map;

import javax.ws.rs.core.Response;

import org.apache.commons.collections4.CollectionUtils;
import org.apache.commons.lang3.StringUtils;
import org.folio.orders.rest.exceptions.HttpException;
import org.folio.orders.utils.ErrorCodes;
import org.folio.rest.annotations.Validate;
import org.folio.rest.jaxrs.model.CheckinCollection;
import org.folio.rest.jaxrs.model.CompositePoLine;
import org.folio.rest.jaxrs.model.CompositePurchaseOrder;
import org.folio.rest.jaxrs.model.Error;
import org.folio.rest.jaxrs.model.Errors;
import org.folio.rest.jaxrs.model.OrderTemplate;
import org.folio.rest.jaxrs.model.Piece;
import org.folio.rest.jaxrs.model.PoNumber;
import org.folio.rest.jaxrs.model.ReceivingCollection;
import org.folio.rest.jaxrs.resource.Orders;

import io.vertx.core.AsyncResult;
import io.vertx.core.Context;
import io.vertx.core.Handler;
import io.vertx.core.json.JsonObject;
import io.vertx.core.logging.Logger;
import io.vertx.core.logging.LoggerFactory;

public class OrdersImpl implements Orders {

  private static final Logger logger = LoggerFactory.getLogger(OrdersImpl.class);

  private static final String ORDERS_LOCATION_PREFIX = "/orders/composite-orders/%s";
  private static final String ORDER_LINE_LOCATION_PREFIX = "/orders/order-lines/%s";
  private static final String ORDER_TEMPLATE_LOCATION_PREFIX = "/orders/order-templates/%s";


  @Override
  @Validate
  public void deleteOrdersCompositeOrdersById(String id, String lang, Map<String, String> okapiHeaders,
      Handler<AsyncResult<Response>> asyncResultHandler, Context vertxContext) {

    PurchaseOrderHelper helper = new PurchaseOrderHelper(okapiHeaders, vertxContext, lang);
    helper
      .deleteOrder(id)
      .thenAccept(v -> asyncResultHandler.handle(succeededFuture(helper.buildNoContentResponse())))
      .exceptionally(t -> handleErrorResponse(asyncResultHandler, helper, t));
  }

  @Override
  @Validate
  public void getOrdersOrderLines(int offset, int limit, String query, String lang, Map<String, String> okapiHeaders,
      Handler<AsyncResult<Response>> asyncResultHandler, Context vertxContext) {
    PurchaseOrderLineHelper helper = new PurchaseOrderLineHelper(okapiHeaders, vertxContext, lang);
    helper
      .getOrderLines(limit, offset, query)
      .thenAccept(lines -> asyncResultHandler.handle(succeededFuture(helper.buildOkResponse(lines))))
      .exceptionally(t -> handleErrorResponse(asyncResultHandler, helper, t));
  }

  @Override
  @Validate
  public void getOrdersCompositeOrdersById(String id, String lang, Map<String, String> okapiHeaders,
      Handler<AsyncResult<Response>> asyncResultHandler, Context vertxContext) {

    PurchaseOrderHelper helper = new PurchaseOrderHelper(okapiHeaders, vertxContext, lang);
    helper
      .getCompositeOrder(id)
      .thenAccept(order -> asyncResultHandler.handle(succeededFuture(helper.buildOkResponse(order))))
      .exceptionally(t -> handleErrorResponse(asyncResultHandler, helper, t));
  }

  @Override
  @Validate
  public void postOrdersCompositeOrders(String lang, CompositePurchaseOrder compPO, Map<String, String> okapiHeaders,
      Handler<AsyncResult<Response>> asyncResultHandler, Context vertxContext) {

    PurchaseOrderHelper helper = new PurchaseOrderHelper(okapiHeaders, vertxContext, lang);

    // First validate content of the PO and proceed only if all is okay
    helper
      .validateOrder(compPO)
      .thenCompose(isValid -> {
        if (isValid) {
          logger.info("Creating PO and POLines...");
          return helper.createPurchaseOrder(compPO)
            .thenAccept(withIds -> {
              logger.info("Successfully Placed Order: " + JsonObject.mapFrom(withIds).encodePrettily());
              asyncResultHandler.handle(succeededFuture(helper
                .buildResponseWithLocation(String.format(ORDERS_LOCATION_PREFIX, withIds.getId()), withIds)));
            });
        } else {
          throw new HttpException(422, GENERIC_ERROR_CODE);
        }
      })
      .exceptionally(t -> handleErrorResponse(asyncResultHandler, helper, t));
  }

  @Override
  @Validate
  public void putOrdersCompositeOrdersById(String orderId, String lang, CompositePurchaseOrder compPO,
      Map<String, String> okapiHeaders,
      Handler<AsyncResult<Response>> asyncResultHandler, Context vertxContext) {

    // Set order id from path if not specified in body
    populateOrderId(orderId, compPO);

    PurchaseOrderHelper helper = new PurchaseOrderHelper(okapiHeaders, vertxContext, lang);
    helper
      .validateExistingOrder(orderId, compPO)
      .thenAccept(isValid -> {
        logger.info("Order is valid: {}", isValid);
        if (isValid) {
          helper
            .updateOrder(compPO)
            .thenAccept(v -> {
              if (logger.isInfoEnabled()) {
                logger.info("Successfully Updated Order: " + JsonObject.mapFrom(compPO).encodePrettily());
              }
              asyncResultHandler.handle(succeededFuture(helper.buildNoContentResponse()));
            })
            .exceptionally(t -> {
              logger.error("Failed to update purchase order with id={}", t, orderId);
              return handleErrorResponse(asyncResultHandler, helper, t);
            });
        } else {
          asyncResultHandler.handle(succeededFuture(helper.buildErrorResponse(422)));
        }
      })
      .exceptionally(t -> handleErrorResponse(asyncResultHandler, helper, t));
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
  public void postOrdersOrderLines(String lang, CompositePoLine poLine, Map<String, String> okapiHeaders,
      Handler<AsyncResult<Response>> asyncResultHandler, Context vertxContext) {

    PurchaseOrderLineHelper helper = new PurchaseOrderLineHelper(okapiHeaders, vertxContext, lang);

    logger.info("Creating POLine to an existing order...");

    // The validation of the PO Line content and its order state is done in
    // scope of the 'createPoLine' method logic
    helper
      .createPoLine(poLine)
      .thenAccept(pol -> {
        if (helper.getErrors().isEmpty()) {
          if (logger.isInfoEnabled()) {
            logger.info("Successfully added PO Line: " + JsonObject.mapFrom(pol).encodePrettily());
          }
          asyncResultHandler.handle(succeededFuture(helper
            .buildResponseWithLocation(String.format(ORDER_LINE_LOCATION_PREFIX, pol.getId()), pol)));
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

    helper
      .getCompositePoLine(lineId)
      .thenAccept(poLine -> {
        if (logger.isInfoEnabled()) {
          logger.info("Received PO Line Response: {}", JsonObject.mapFrom(poLine).encodePrettily());
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
    helper
      .deleteLine(lineId)
      .thenAccept(v -> asyncResultHandler.handle(succeededFuture(helper.buildNoContentResponse())))
      .exceptionally(t -> handleErrorResponse(asyncResultHandler, helper, t));
  }

  @Override
  @Validate
  public void getOrdersPoNumber(String lang, Map<String, String> okapiHeaders,
      Handler<AsyncResult<Response>> asyncResultHandler, Context vertxContext) {
    logger.info("Receiving generated poNumber ...");

    new PoNumberHelper(okapiHeaders, vertxContext, lang)
      .getPoNumber()
      .thenAccept(response -> asyncResultHandler.handle(succeededFuture(response)));
  }

  @Override
  @Validate
  public void putOrdersOrderLinesById(String lineId, String lang, CompositePoLine poLine,
      Map<String, String> okapiHeaders,
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
        helper.validateAndNormalizeISBN(poLine)
          .thenCompose(vo -> helper.updateOrderLine(poLine))
          .thenAccept(v -> asyncResultHandler.handle(succeededFuture(helper.buildNoContentResponse())))
          .exceptionally(t -> handleErrorResponse(asyncResultHandler, helper, t));
      })
      .exceptionally(t -> handleErrorResponse(asyncResultHandler, helper, t));
  }

  @Override
  @Validate
  public void postOrdersPoNumberValidate(String lang, PoNumber poNumber, Map<String, String> okapiHeaders,
      Handler<AsyncResult<Response>> asyncResultHandler, Context vertxContext) {
    PoNumberHelper helper = new PoNumberHelper(okapiHeaders, vertxContext, lang);
    logger.info("Validating a PO Number");

    // @Validate asserts the pattern of a PO Number, the below method is used to
    // check for uniqueness
    helper.checkPONumberUnique(poNumber)
      .thenAccept(response -> asyncResultHandler.handle(succeededFuture(response)));
  }

  @Override
  @Validate
  public void postOrdersReceive(String lang, ReceivingCollection entity, Map<String, String> okapiHeaders,
      Handler<AsyncResult<Response>> asyncResultHandler, Context vertxContext) {
    logger.info("Receiving {} items", entity.getTotalRecords());
    ReceivingHelper helper = new ReceivingHelper(entity, okapiHeaders, vertxContext, lang);
    helper
      .receiveItems(entity)
      .thenAccept(result -> asyncResultHandler.handle(succeededFuture(helper.buildOkResponse(result))))
      .exceptionally(t -> handleErrorResponse(asyncResultHandler, helper, t));
  }

  @Override
  @Validate
  public void postOrdersCheckIn(String lang, CheckinCollection entity, Map<String, String> okapiHeaders,
      Handler<AsyncResult<Response>> asyncResultHandler, Context vertxContext) {
    logger.info("Checkin {} items", entity.getTotalRecords());
    CheckinHelper helper = new CheckinHelper(entity, okapiHeaders, vertxContext, lang);
    helper
      .checkinPieces(entity)
      .thenAccept(result -> asyncResultHandler.handle(succeededFuture(helper.buildOkResponse(result))))
      .exceptionally(t -> handleErrorResponse(asyncResultHandler, helper, t));
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
          logger.info("Successfully retrieved orders: " + JsonObject.mapFrom(orders).encodePrettily());
        }
        asyncResultHandler.handle(succeededFuture(helper.buildOkResponse(orders)));
      })
      .exceptionally(t -> handleErrorResponse(asyncResultHandler, helper, t));
  }

  @Override
  @Validate
  public void getOrdersReceivingHistory(int offset, int limit, String query, String lang,
      Map<String, String> okapiHeaders,
      Handler<AsyncResult<Response>> asyncResultHandler, Context vertxContext) {

    ReceivingHelper helper = new ReceivingHelper(okapiHeaders, vertxContext, lang);

    helper
      .getReceivingHistory(limit, offset, query)
      .thenAccept(receivingHistory -> {
        if (logger.isInfoEnabled()) {
          logger
            .info("Successfully retrieved receiving history: " + JsonObject.mapFrom(receivingHistory).encodePrettily());
        }
        asyncResultHandler.handle(succeededFuture(helper.buildOkResponse(receivingHistory)));
      })
      .exceptionally(t -> handleErrorResponse(asyncResultHandler, helper, t));
  }

  private Void handleErrorResponse(Handler<AsyncResult<Response>> asyncResultHandler, AbstractHelper helper,
      Throwable t) {
    asyncResultHandler.handle(succeededFuture(helper.buildErrorResponse(t)));
    return null;
  }

  @Override
  @Validate
  public void postOrdersPieces(String lang, Piece entity, Map<String, String> okapiHeaders,
      Handler<AsyncResult<Response>> asyncResultHandler, Context vertxContext) {
    PiecesHelper helper = new PiecesHelper(okapiHeaders, vertxContext, lang);
    helper
      .createPiece(entity)
      .thenAccept(piece -> {
        if (logger.isInfoEnabled()) {
          logger.info("Successfully created piece: " + JsonObject.mapFrom(piece).encodePrettily());
        }
        asyncResultHandler.handle(succeededFuture(helper.buildCreatedResponse(piece)));
      })
      .exceptionally(t -> handleErrorResponse(asyncResultHandler, helper, t));
  }

  @Override
  @Validate
  public void putOrdersPiecesById(String pieceId, String lang, Piece piece, Map<String, String> okapiHeaders,
      Handler<AsyncResult<Response>> asyncResultHandler, Context vertxContext) {
    PiecesHelper putPieceHelper = new PiecesHelper(okapiHeaders, vertxContext, lang);

    if (StringUtils.isEmpty(piece.getId())) {
      piece.setId(pieceId);
    }

    putPieceHelper.updatePieceRecord(piece)
      .thenAccept(v -> asyncResultHandler.handle(succeededFuture(putPieceHelper.buildNoContentResponse())))
      .exceptionally(t -> handleErrorResponse(asyncResultHandler, putPieceHelper, t));
  }

  @Override
  @Validate
  public void deleteOrdersPiecesById(String pieceId, String lang, Map<String, String> okapiHeaders,
      Handler<AsyncResult<Response>> asyncResultHandler, Context vertxContext) {
    PiecesHelper deletePieceHelper = new PiecesHelper(okapiHeaders, vertxContext, lang);
    deletePieceHelper.deletePiece(pieceId)
      .thenAccept(ok -> asyncResultHandler.handle(succeededFuture(deletePieceHelper.buildNoContentResponse())))
      .exceptionally(fail -> handleErrorResponse(asyncResultHandler, deletePieceHelper, fail));
  }

  @Override
  @Validate
  public void postOrdersOrderTemplates(String lang, OrderTemplate entity, Map<String, String> okapiHeaders, Handler<AsyncResult<Response>> asyncResultHandler, Context vertxContext) {
    OrderTemplatesHelper helper = new OrderTemplatesHelper(okapiHeaders, vertxContext, lang);
    helper.createOrderTemplate(entity)
      .thenAccept(template -> {
        if (logger.isInfoEnabled()) {
          logger.info("Successfully created new order template: " + JsonObject.mapFrom(template).encodePrettily());
        }
        asyncResultHandler.handle(succeededFuture(helper.buildResponseWithLocation(String.format(ORDER_TEMPLATE_LOCATION_PREFIX, template.getId()), template)));
      })
      .exceptionally(t -> handleErrorResponse(asyncResultHandler, helper, t));
  }

  @Override
  @Validate
  public void getOrdersOrderTemplates(String query, int offset, int limit, String lang, Map<String, String> okapiHeaders, Handler<AsyncResult<Response>> asyncResultHandler, Context vertxContext) {
    OrderTemplatesHelper helper = new OrderTemplatesHelper(okapiHeaders, vertxContext, lang);
    helper.getOrderTemplates(query, offset, limit)
      .thenAccept(templates -> {
        if (logger.isInfoEnabled()) {
          logger.info("Successfully retrieved order templates collection: " + JsonObject.mapFrom(templates).encodePrettily());
        }
        asyncResultHandler.handle(succeededFuture(helper.buildOkResponse(templates)));
      })
      .exceptionally(t -> handleErrorResponse(asyncResultHandler, helper, t));
  }

  @Override
  @Validate
  public void putOrdersOrderTemplatesById(String id, String lang, OrderTemplate entity, Map<String, String> okapiHeaders, Handler<AsyncResult<Response>> asyncResultHandler, Context vertxContext) {
    OrderTemplatesHelper helper = new OrderTemplatesHelper(okapiHeaders, vertxContext, lang);

    // Set template id if this is available only in path
    if (StringUtils.isEmpty(entity.getId())) {
      entity.setId(id);
    }

    if (!entity.getId().equals(id)) {
      helper.addProcessingError(MISMATCH_BETWEEN_ID_IN_PATH_AND_BODY.toError());
      asyncResultHandler.handle(succeededFuture(helper.buildErrorResponse(422)));
    } else {
      helper.updateOrderTemplate(entity.withId(id))
        .thenAccept(template -> {
          logger.info("Successfully updated order template with id={}", id);
          asyncResultHandler.handle(succeededFuture(helper.buildNoContentResponse()));
        })
        .exceptionally(t -> handleErrorResponse(asyncResultHandler, helper, t));
    }
  }

  @Override
  @Validate
  public void getOrdersOrderTemplatesById(String id, String lang, Map<String, String> okapiHeaders, Handler<AsyncResult<Response>> asyncResultHandler, Context vertxContext) {
    OrderTemplatesHelper helper = new OrderTemplatesHelper(okapiHeaders, vertxContext, lang);
    helper.getOrderTemplateById(id)
      .thenAccept(template -> {
        if (logger.isInfoEnabled()) {
          logger.info("Successfully retrieved order template: " + JsonObject.mapFrom(template).encodePrettily());
        }
        asyncResultHandler.handle(succeededFuture(helper.buildOkResponse(template)));
      })
      .exceptionally(t -> handleErrorResponse(asyncResultHandler, helper, t));
  }

  @Override
  @Validate
  public void deleteOrdersOrderTemplatesById(String id, String lang, Map<String, String> okapiHeaders, Handler<AsyncResult<Response>> asyncResultHandler, Context vertxContext) {
    OrderTemplatesHelper helper = new OrderTemplatesHelper(okapiHeaders, vertxContext, lang);
    helper.deleteOrderTemplate(id)
      .thenAccept(ok -> {
        if (logger.isInfoEnabled()) {
          logger.info("Successfully deleted order template with id={}", id);
        }
        asyncResultHandler.handle(succeededFuture(helper.buildNoContentResponse()));
      })
      .exceptionally(t -> handleErrorResponse(asyncResultHandler, helper, t));
  }
}
