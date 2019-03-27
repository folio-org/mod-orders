package org.folio.rest.impl;

import static io.vertx.core.Future.succeededFuture;
import static org.folio.orders.utils.HelperUtils.validatePoLine;

import java.util.ArrayList;
import java.util.List;
import java.util.Map;

import javax.ws.rs.core.Response;

import org.apache.commons.collections4.CollectionUtils;
import org.apache.commons.lang3.StringUtils;
import org.folio.orders.utils.ErrorCodes;
import org.folio.rest.jaxrs.model.Piece;
import org.folio.rest.annotations.Validate;
import org.folio.rest.jaxrs.model.*;
import org.folio.rest.jaxrs.model.Error;
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
      .getPoLines(limit, offset, query)
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
      .thenAccept(isValid -> {
        if (isValid) {
          logger.info("Creating PO and POLines...");
          helper.createPurchaseOrder(compPO)
            .thenAccept(withIds -> {
              logger.info("Applying Funds...");
              helper.applyFunds(withIds)
                .thenAccept(withFunds -> {
                  logger.info("Successfully Placed Order: " + JsonObject.mapFrom(withFunds).encodePrettily());
                  helper.closeHttpClient();
                  Response response = PostOrdersCompositeOrdersResponse.respond201WithApplicationJson(withFunds,
                    PostOrdersCompositeOrdersResponse.headersFor201()
                                                     .withLocation(String.format(ORDERS_LOCATION_PREFIX, withFunds.getId())));
                  asyncResultHandler.handle(succeededFuture(response));
                })
                .exceptionally(t -> {
                  logger.error("Failure happened applying funds to created order");
                  asyncResultHandler.handle(succeededFuture(helper.buildErrorResponse(t)));
                  return null;
                });
            })
            .exceptionally(t -> {
              logger.error("Failure to create order");
              asyncResultHandler.handle(succeededFuture(helper.buildErrorResponse(t)));
              return null;
            });
        } else {
          asyncResultHandler.handle(succeededFuture(helper.buildErrorResponse(422)));
        }
      })
      .exceptionally(t -> handleErrorResponse(asyncResultHandler, helper, t));
  }

  @Override
  @Validate
  public void putOrdersCompositeOrdersById(String orderId, String lang, CompositePurchaseOrder compPO, Map<String, String> okapiHeaders,
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

    // The validation of the PO Line content and its order state is done in scope of the 'createPoLine' method logic
    helper
      .createPoLine(poLine)
      .thenAccept(pol -> {
        Response response;
        if (helper.getErrors().isEmpty()) {
          if (logger.isInfoEnabled()) {
            logger.info("Successfully added PO Line: " + JsonObject.mapFrom(pol).encodePrettily());
          }
          response = PostOrdersOrderLinesResponse.respond201WithApplicationJson(pol,
            PostOrdersOrderLinesResponse.headersFor201()
                                        .withLocation(String.format(ORDER_LINE_LOCATION_PREFIX, pol.getId())));
        } else {
          response = helper.buildErrorResponse(422);
        }
        asyncResultHandler.handle(succeededFuture(response));
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
  public void getOrdersPoNumber(String lang, Map<String, String> okapiHeaders, Handler<AsyncResult<Response>> asyncResultHandler, Context vertxContext) {
    logger.info("Receiving generated poNumber ...");

    new PoNumberHelper(okapiHeaders, vertxContext, lang)
      .getPoNumber()
      .thenAccept(response -> asyncResultHandler.handle(succeededFuture(response)));
  }

  @Override
  @Validate
  public void putOrdersOrderLinesById(String lineId, String lang, CompositePoLine poLine, Map<String, String> okapiHeaders,
                                      Handler<AsyncResult<Response>> asyncResultHandler, Context vertxContext) {
    logger.info("Handling PUT Order Line operation...");

    // Set id if this is available only in path
    if (StringUtils.isEmpty(poLine.getId())) {
      poLine.setId(lineId);
    }

    // First validate content of the PO Line and proceed only if all is okay
    List<Error> errors = new ArrayList<>(validatePoLine(poLine));
    if (!lineId.equals(poLine.getId())) {
      errors.add(ErrorCodes.MISMATCH_BETWEEN_ID_IN_PATH_AND_BODY.toError());
    }
    if (!errors.isEmpty()) {
      PutOrdersOrderLinesByIdResponse response = PutOrdersOrderLinesByIdResponse.respond422WithApplicationJson(new Errors().withErrors(errors));
      asyncResultHandler.handle(succeededFuture(response));
      return;
    }

    PurchaseOrderLineHelper helper = new PurchaseOrderLineHelper(okapiHeaders, vertxContext, lang);
    helper.updateOrderLine(poLine)
      .thenAccept(v -> asyncResultHandler.handle(succeededFuture(helper.buildNoContentResponse())))
      .exceptionally(t -> handleErrorResponse(asyncResultHandler, helper, t));
  }

  @Override
  @Validate
  public void postOrdersPoNumberValidate(String lang, PoNumber poNumber, Map<String, String> okapiHeaders,
                                         Handler<AsyncResult<Response>> asyncResultHandler, Context vertxContext) {
    PoNumberHelper helper = new PoNumberHelper(okapiHeaders, vertxContext, lang);
    logger.info("Validating a PO Number");

    //@Validate asserts the pattern of a PO Number, the below method is used to check for uniqueness
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
  public void getOrdersCompositeOrders(int offset, int limit, String query, String lang, Map<String, String> okapiHeaders, Handler<AsyncResult<Response>> asyncResultHandler, Context vertxContext) {
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
  public void getOrdersReceivingHistory(int offset, int limit, String query, String lang, Map<String, String> okapiHeaders,
                                        Handler<AsyncResult<Response>> asyncResultHandler, Context vertxContext) {

    ReceivingHelper helper = new ReceivingHelper(okapiHeaders, vertxContext, lang);

    helper
      .getReceivingHistory(limit, offset, query)
      .thenAccept(receivingHistory -> {
        if (logger.isInfoEnabled()) {
          logger.info("Successfully retrieved receiving history: " + JsonObject.mapFrom(receivingHistory).encodePrettily());
        }
        asyncResultHandler.handle(succeededFuture(helper.buildOkResponse(receivingHistory)));
      })
      .exceptionally(t -> handleErrorResponse(asyncResultHandler, helper, t));
  }

  private Void handleErrorResponse(Handler<AsyncResult<Response>> asyncResultHandler, AbstractHelper helper, Throwable t) {
    asyncResultHandler.handle(succeededFuture(helper.buildErrorResponse(t)));
    return null;
  }

  @Override
  @Validate
  public void postOrdersPieces(String lang, Piece entity, Map<String, String> okapiHeaders, Handler<AsyncResult<Response>> asyncResultHandler, Context vertxContext) {
    PiecesHelper helper = new PiecesHelper(okapiHeaders, vertxContext, lang);
    helper
      .createRecordInStorage(entity)
      .thenAccept(piece -> {
        if (logger.isInfoEnabled()) {
          logger.info("Successfully created piece: " + JsonObject.mapFrom(piece).encodePrettily());
        }
        asyncResultHandler.handle(succeededFuture(helper.buildCreatedResponse(piece)));
      })
      .exceptionally(t -> handleErrorResponse(asyncResultHandler, helper, t));
  }
}
