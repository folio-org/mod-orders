package org.folio.rest.impl;

import static io.vertx.core.Future.succeededFuture;
import static org.folio.orders.utils.HelperUtils.DEFAULT_POLINE_LIMIT;
import static org.folio.orders.utils.HelperUtils.GET_ALL_POLINES_QUERY_WITH_LIMIT;
import static org.folio.orders.utils.HelperUtils.PO_LINES_LIMIT_PROPERTY;
import static org.folio.orders.utils.HelperUtils.getPurchaseOrderById;
import static org.folio.orders.utils.HelperUtils.handleGetRequest;
import static org.folio.orders.utils.HelperUtils.loadConfiguration;
import static org.folio.orders.utils.HelperUtils.validateOrder;
import static org.folio.orders.utils.HelperUtils.validatePoLine;
import static org.folio.orders.utils.ResourcePathResolver.PO_NUMBER;

import java.util.ArrayList;
import java.util.List;
import java.util.Map;

import javax.ws.rs.core.Response;

import org.apache.commons.collections4.CollectionUtils;
import org.apache.commons.lang3.StringUtils;
import org.folio.orders.rest.exceptions.HttpException;
import org.folio.orders.utils.ErrorCodes;
import org.folio.rest.annotations.Validate;
import org.folio.rest.jaxrs.model.*;
import org.folio.rest.jaxrs.model.Error;
import org.folio.rest.jaxrs.resource.Orders;
import org.folio.rest.tools.client.interfaces.HttpClientInterface;

import io.vertx.core.AsyncResult;
import io.vertx.core.Context;
import io.vertx.core.Future;
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
    DeleteOrdersByIdHelper helper = new DeleteOrdersByIdHelper(okapiHeaders, asyncResultHandler, vertxContext, lang);
    helper.deleteOrder(id)
    .thenRun(()->{
      logger.info("Successfully deleted order: ");
      javax.ws.rs.core.Response response = DeleteOrdersCompositeOrdersByIdResponse.respond204();
      AsyncResult<javax.ws.rs.core.Response> result = succeededFuture(response);
      asyncResultHandler.handle(result);
    })
    .exceptionally(helper::handleError);
  }

  @Override
  @Validate
  public void getOrdersOrderLines(int offset, int limit, String query, String lang, Map<String, String> okapiHeaders, Handler<AsyncResult<Response>> asyncResultHandler, Context vertxContext) {
    final HttpClientInterface httpClient = AbstractHelper.getHttpClient(okapiHeaders);
    GetPOLinesHelper helper = new GetPOLinesHelper(httpClient, okapiHeaders, asyncResultHandler, vertxContext, lang);
    helper.getPOLines(limit, offset, query)
      .thenAccept(lines -> {
        logger.info("Successfully retrieved orders: " + JsonObject.mapFrom(lines).encodePrettily());
        httpClient.closeClient();
        javax.ws.rs.core.Response response = GetOrdersOrderLinesResponse.respond200WithApplicationJson(lines);
        AsyncResult<javax.ws.rs.core.Response> result = succeededFuture(response);
        asyncResultHandler.handle(result);
      })
      .exceptionally(helper::handleError);
  }

  @Override
  @Validate
  public void getOrdersCompositeOrdersById(String id, String lang, Map<String, String> okapiHeaders,
      Handler<AsyncResult<javax.ws.rs.core.Response>> asyncResultHandler, Context vertxContext) {
    final HttpClientInterface httpClient = AbstractHelper.getHttpClient(okapiHeaders);
    GetOrdersByIdHelper helper = new GetOrdersByIdHelper(httpClient, okapiHeaders, asyncResultHandler, vertxContext, lang);

    helper.getOrder(id)
      .thenAccept(order -> {
        logger.info("Successfully retrieved order: " + JsonObject.mapFrom(order).encodePrettily());
        httpClient.closeClient();
        javax.ws.rs.core.Response response = GetOrdersCompositeOrdersByIdResponse.respond200WithApplicationJson(order);
        AsyncResult<javax.ws.rs.core.Response> result = succeededFuture(response);
        asyncResultHandler.handle(result);
      })
      .exceptionally(helper::handleError);
  }

  @Override
  @Validate
  public void postOrdersCompositeOrders(String lang, CompositePurchaseOrder compPO, Map<String, String> okapiHeaders, Handler<AsyncResult<Response>> asyncResultHandler, Context vertxContext) {

    // First validate content of the PO and proceed only if all is okay
    List<Error> errors = validateOrder(compPO);
    if (!errors.isEmpty()) {
      PostOrdersCompositeOrdersResponse response = PostOrdersCompositeOrdersResponse.respond422WithApplicationJson(new Errors().withErrors(errors));
      asyncResultHandler.handle(succeededFuture(response));
      return;
    }

    final HttpClientInterface httpClient = AbstractHelper.getHttpClient(okapiHeaders);
    PostOrdersHelper helper = new PostOrdersHelper(httpClient, okapiHeaders, asyncResultHandler, vertxContext, lang);
    loadConfiguration(okapiHeaders, vertxContext, logger).thenAccept(config -> {
      validatePoLinesQuantity(compPO, config);
      logger.info("Creating PO and POLines...");
      helper.createPurchaseOrder(compPO)
        .thenAccept(withIds -> {

          logger.info("Applying Funds...");
          helper.applyFunds(withIds)
            .thenAccept(withFunds -> {

              logger.info("Successfully Placed Order: " + JsonObject.mapFrom(withFunds).encodePrettily());
              httpClient.closeClient();
              javax.ws.rs.core.Response response = PostOrdersCompositeOrdersResponse.respond201WithApplicationJson(withFunds,
                PostOrdersCompositeOrdersResponse.headersFor201().withLocation(String.format(ORDERS_LOCATION_PREFIX, withFunds.getId())));
              AsyncResult<javax.ws.rs.core.Response> result = Future.succeededFuture(response);
              asyncResultHandler.handle(result);
            })
            .exceptionally(helper::handleError);
        })
        .exceptionally(helper::handleError);

    }).exceptionally(helper::handleError);
  }

  @Override
  @Validate
  public void putOrdersCompositeOrdersById(String orderId, String lang, CompositePurchaseOrder compPO, Map<String, String> okapiHeaders,
                                           Handler<AsyncResult<Response>> asyncResultHandler, Context vertxContext) {

    // First validate content of the PO and proceed only if all is okay
    List<Error> errors = new ArrayList<>(validateOrder(compPO));
    if (StringUtils.isEmpty(compPO.getPoNumber())) {
      errors.add(ErrorCodes.PO_NUMBER_REQUIRED.toError());
    }
    if (!errors.isEmpty()) {
      PutOrdersCompositeOrdersByIdResponse response = PutOrdersCompositeOrdersByIdResponse.respond422WithApplicationJson(new Errors().withErrors(errors));
      asyncResultHandler.handle(succeededFuture(response));
      return;
    }

    PutOrdersByIdHelper putHelper = new PutOrdersByIdHelper(okapiHeaders, asyncResultHandler, vertxContext, lang);
    if (CollectionUtils.isEmpty(compPO.getCompositePoLines())) {
      putHelper.updateOrder(orderId, compPO);
    } else {
      loadConfiguration(okapiHeaders, vertxContext, logger)
        .thenAccept(config -> {
          validatePoLinesQuantity(compPO, config);
          compPO.getCompositePoLines().forEach(poLine -> {
            if (StringUtils.isEmpty(poLine.getPurchaseOrderId())) {
              poLine.setPurchaseOrderId(orderId);
            }
            if (!orderId.equals(poLine.getPurchaseOrderId())) {
              throw new HttpException(422, ErrorCodes.MISMATCH_BETWEEN_ID_IN_PATH_AND_PO_LINE);
            }
          });
          putHelper.updateOrder(orderId, compPO);
      }).exceptionally(putHelper::handleError);
    }
  }

  @Override
  @Validate
  public void postOrdersOrderLines(String lang, CompositePoLine poLine, Map<String, String> okapiHeaders,
                                   Handler<AsyncResult<Response>> asyncResultHandler, Context vertxContext) {

    // First validate content of the PO Line and proceed only if all is okay
    List<Error> errors = new ArrayList<>();
    if (poLine.getPurchaseOrderId() == null) {
      errors.add(ErrorCodes.MISSING_ORDER_ID_IN_POL.toError());
    }
    errors.addAll(validatePoLine(poLine));
    if (!errors.isEmpty()) {
      PostOrdersOrderLinesResponse response = PostOrdersOrderLinesResponse.respond422WithApplicationJson(new Errors().withErrors(errors));
      asyncResultHandler.handle(succeededFuture(response));
      return;
    }

    final HttpClientInterface httpClient = AbstractHelper.getHttpClient(okapiHeaders);
    PostOrderLineHelper helper = new PostOrderLineHelper(httpClient, okapiHeaders, asyncResultHandler, vertxContext, lang);

    logger.info("Creating POLine to an existing order...");

    loadConfiguration(okapiHeaders, vertxContext, logger)
      .thenAccept(config -> {
        String endpoint = String.format(GET_ALL_POLINES_QUERY_WITH_LIMIT, 1, poLine.getPurchaseOrderId(), lang);
        handleGetRequest(endpoint, httpClient, vertxContext, okapiHeaders, logger)
          .thenAccept(entries -> {
            int limit = getPoLineLimit(config);
            if (entries.getInteger("total_records") < limit) {
              getPurchaseOrderById(poLine.getPurchaseOrderId(), lang, httpClient, vertxContext, okapiHeaders, logger)
                .thenCompose( purchaseOrder -> {
                  poLine.setPoLineNumber(purchaseOrder.getString(PO_NUMBER));
                  return helper.createPoLine(poLine)
                    .thenAccept(pol -> {
                      logger.info("Successfully added PO Line: " + JsonObject.mapFrom(pol).encodePrettily());
                      httpClient.closeClient();
                      Response response = PostOrdersOrderLinesResponse.respond201WithApplicationJson
                        (poLine, PostOrdersOrderLinesResponse.headersFor201().withLocation(String.format(ORDER_LINE_LOCATION_PREFIX, pol.getId())));
                      asyncResultHandler.handle(succeededFuture(response));

                    });
                  }
                )
                .exceptionally(helper::handleError);
            } else {
              throw new HttpException(422, ErrorCodes.POL_LINES_LIMIT_EXCEEDED);
            }
          }).exceptionally(helper::handleError);
      }).exceptionally(helper::handleError);
  }

  @Override
  @Validate
  public void getOrdersOrderLinesById(String lineId, String lang, Map<String, String> okapiHeaders,
                                          Handler<AsyncResult<Response>> asyncResultHandler, Context vertxContext) {
    logger.info("Started Invocation of POLine Request with id = " + lineId);
    final HttpClientInterface httpClient = AbstractHelper.getHttpClient(okapiHeaders);
    GetPOLineByIdHelper helper = new GetPOLineByIdHelper(httpClient, okapiHeaders, asyncResultHandler, vertxContext, lang);

    helper.getPOLineByPOLineId(lineId)
      .thenAccept(poline -> {
        logger.info("Received POLine Response: " + JsonObject.mapFrom(poline).encodePrettily());
        httpClient.closeClient();
        javax.ws.rs.core.Response response = GetOrdersOrderLinesByIdResponse.respond200WithApplicationJson(poline);
        AsyncResult<javax.ws.rs.core.Response> result = succeededFuture(response);
        asyncResultHandler.handle(result);
      })
      .exceptionally(helper::handleError);
  }

  @Override
  @Validate
  public void deleteOrdersOrderLinesById(String lineId, String lang, Map<String, String> okapiHeaders,
                                             Handler<AsyncResult<Response>> asyncResultHandler, Context vertxContext) {
    new DeleteOrderLineByIdHelper(okapiHeaders, asyncResultHandler, vertxContext, lang)
      .deleteLine(lineId);
  }

  @Override
  @Validate
  public void getOrdersPoNumber(String lang, Map<String, String> okapiHeaders, Handler<AsyncResult<Response>> asyncResultHandler, Context vertxContext) {
    logger.info("Receiving generated po_number ...");

    final HttpClientInterface httpClient = AbstractHelper.getHttpClient(okapiHeaders);
    PoNumberHelper helper = new PoNumberHelper(httpClient, okapiHeaders, asyncResultHandler, vertxContext, lang);

    helper.generatePoNumber()
      .thenAccept(poNumber -> {
        logger.info("Received PoNumber Response: " + poNumber);
        httpClient.closeClient();
        javax.ws.rs.core.Response response
          = GetOrdersPoNumberResponse.respond200WithApplicationJson(new PoNumber().withPoNumber(poNumber));
        AsyncResult<javax.ws.rs.core.Response> result = succeededFuture(response);
        asyncResultHandler.handle(result);
      }).exceptionally(helper::handleError);
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
      errors.add(ErrorCodes.MISMATCH_BETWEEN_ID_IN_PATH_AND_PO_LINE.toError());
    }
    if (!errors.isEmpty()) {
      PutOrdersOrderLinesByIdResponse response = PutOrdersOrderLinesByIdResponse.respond422WithApplicationJson(new Errors().withErrors(errors));
      asyncResultHandler.handle(succeededFuture(response));
      return;
    }

    new PutOrderLineByIdHelper(okapiHeaders, asyncResultHandler, vertxContext, lang).updateOrderLine(poLine);
  }

  @Override
  @Validate
  public void postOrdersPoNumberValidate(String lang, PoNumber poNumber, Map<String, String> okapiHeaders,
     Handler<AsyncResult<Response>> asyncResultHandler, Context vertxContext) {
    final HttpClientInterface httpClient = AbstractHelper.getHttpClient(okapiHeaders);
    PoNumberHelper helper=new PoNumberHelper(httpClient, okapiHeaders, asyncResultHandler, vertxContext, lang);
    logger.info("Validating a PO Number");
    //@Validate asserts the pattern of a PO Number, the below method is used to check for uniqueness
     helper.checkPONumberUnique(poNumber);
  }

  @Override
  @Validate
  public void postOrdersReceive(String lang, ReceivingCollection entity, Map<String, String> okapiHeaders,
                                Handler<AsyncResult<Response>> asyncResultHandler, Context vertxContext) {
    logger.info("Receiving {} items", entity.getTotalRecords());
    new ReceivingHelper(entity, okapiHeaders, asyncResultHandler, vertxContext, lang)
      .receiveItems(entity);
  }

  @Override
  @Validate
  public void postOrdersCheckIn(String lang, CheckinCollection entity, Map<String, String> okapiHeaders,
                                Handler<AsyncResult<Response>> asyncResultHandler, Context vertxContext) {
    // Return 501 (Not Implemented) for now
    asyncResultHandler.handle(succeededFuture(Response.status(501).build()));
  }

  private static int getPoLineLimit(JsonObject config) {
    try {
      return Integer.parseInt(config.getString(PO_LINES_LIMIT_PROPERTY, DEFAULT_POLINE_LIMIT));
    } catch (NumberFormatException e) {
      throw new NumberFormatException("Invalid limit value in configuration.");
    }
  }

  private void validatePoLinesQuantity(CompositePurchaseOrder compPO, JsonObject config) {
    int limit = getPoLineLimit(config);
    if (compPO.getCompositePoLines().size() > limit) {
      throw new HttpException(422, ErrorCodes.POL_LINES_LIMIT_EXCEEDED);
    }
  }

  @Override
  @Validate
  public void getOrdersCompositeOrders(int offset, int limit, String query, String lang, Map<String, String> okapiHeaders, Handler<AsyncResult<Response>> asyncResultHandler, Context vertxContext) {
    final HttpClientInterface httpClient = AbstractHelper.getHttpClient(okapiHeaders);
    GetOrdersHelper helper = new GetOrdersHelper(httpClient, okapiHeaders, asyncResultHandler, vertxContext, lang);
    helper.getPurchaseOrders(limit, offset, query)
      .thenAccept(order -> {
        logger.info("Successfully retrieved orders: " + JsonObject.mapFrom(order).encodePrettily());
        httpClient.closeClient();
        javax.ws.rs.core.Response response = GetOrdersCompositeOrdersResponse.respond200WithApplicationJson(order);
        AsyncResult<javax.ws.rs.core.Response> result = succeededFuture(response);
        asyncResultHandler.handle(result);
      })
      .exceptionally(helper::handleError);
  }


  @Override
  @Validate
  public void getOrdersReceivingHistory(int offset, int limit, String query, String lang,
      Map<String, String> okapiHeaders, Handler<AsyncResult<Response>> asyncResultHandler, Context vertxContext) {

    final HttpClientInterface httpClient = AbstractHelper.getHttpClient(okapiHeaders);
    GetReceivingHistoryHelper helper= new GetReceivingHistoryHelper(httpClient, okapiHeaders, asyncResultHandler, vertxContext, lang);

    helper.getReceivingHistory(limit, offset, query)
    .thenAccept(receivingHistory -> {
      logger.info("Successfully retrieved receiving history: " + JsonObject.mapFrom(receivingHistory).encodePrettily());
      httpClient.closeClient();
      javax.ws.rs.core.Response response = GetOrdersReceivingHistoryResponse.respond200WithApplicationJson(receivingHistory);
      AsyncResult<javax.ws.rs.core.Response> result = succeededFuture(response);
      asyncResultHandler.handle(result);
    })
    .exceptionally(helper::handleError);

  }

}

