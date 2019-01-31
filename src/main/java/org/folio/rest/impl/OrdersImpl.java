package org.folio.rest.impl;

import static io.vertx.core.Future.succeededFuture;
import static org.folio.orders.utils.HelperUtils.DEFAULT_POLINE_LIMIT;
import static org.folio.orders.utils.HelperUtils.GET_ALL_POLINES_QUERY_WITH_LIMIT;
import static org.folio.orders.utils.HelperUtils.PO_LINES_LIMIT_PROPERTY;
import static org.folio.orders.utils.HelperUtils.handleGetRequest;
import static org.folio.orders.utils.HelperUtils.loadConfiguration;

import java.util.Map;
import java.util.concurrent.CompletionException;

import javax.ws.rs.core.Response;

import org.apache.commons.collections4.CollectionUtils;
import org.apache.commons.lang3.StringUtils;
import org.folio.orders.rest.exceptions.ValidationException;
import org.folio.rest.annotations.Validate;
import org.folio.rest.jaxrs.model.CheckinCollection;
import org.folio.rest.jaxrs.model.CompositePurchaseOrder;
import org.folio.rest.jaxrs.model.PoLine;
import org.folio.rest.jaxrs.model.PoNumber;
import org.folio.rest.jaxrs.model.ReceivingCollection;
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

  private static final String ORDERS_LOCATION_PREFIX = "/orders/composite-orders";
  private static final String ORDER_LINE_LOCATION_PREFIX = "/orders/order-lines/%s";
  public static final String OVER_LIMIT_ERROR_MESSAGE = "Your FOLIO system is configured to limit the number of PO Lines on each order to %s.";
  public static final String MISMATCH_BETWEEN_ID_IN_PATH_AND_PO_LINE = "Mismatch between id in path and PoLine";
  public static final String LINES_LIMIT_ERROR_CODE = "lines_limit";
  private static final String MISSING_ORDER_ID = "Purchase order id is missing in PoLine object";


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

              logger.info("Updating Inventory...");
              helper.updateInventory(withFunds)
                .thenAccept(withInventory -> {

                  logger.info("Successfully Placed Order: " + JsonObject.mapFrom(withInventory).encodePrettily());
                  httpClient.closeClient();
                  javax.ws.rs.core.Response response = PostOrdersCompositeOrdersResponse.respond201WithApplicationJson(withInventory,
                    PostOrdersCompositeOrdersResponse.headersFor201().withLocation(ORDERS_LOCATION_PREFIX + withInventory.getId()));
                  AsyncResult<javax.ws.rs.core.Response> result = Future.succeededFuture(response);
                  asyncResultHandler.handle(result);
                })
                .exceptionally(helper::handleError);
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
    PutOrdersByIdHelper putHelper = new PutOrdersByIdHelper(okapiHeaders, asyncResultHandler, vertxContext, lang);
    if (org.apache.commons.lang.StringUtils.isEmpty(compPO.getPoNumber())) {
      putHelper.handleError(new CompletionException((new ValidationException("po_number is missing"))));
    } else {
      if (CollectionUtils.isEmpty(compPO.getPoLines())) {
        putHelper.updateOrder(orderId, compPO);
      } else {
        loadConfiguration(okapiHeaders, vertxContext, logger)
          .thenAccept(config -> {
            validatePoLinesQuantity(compPO, config);
            compPO.getPoLines().forEach(poLine -> {
              if (StringUtils.isEmpty(poLine.getPurchaseOrderId())) {
                poLine.setPurchaseOrderId(orderId);
              }
              if (!orderId.equals(poLine.getPurchaseOrderId())) {
                throw new ValidationException(MISMATCH_BETWEEN_ID_IN_PATH_AND_PO_LINE, AbstractHelper.ID_MISMATCH_ERROR_CODE);
              }
            });
            putHelper.updateOrder(orderId, compPO);
        }).exceptionally(putHelper::handleError);
      }
    }
  }

  @Override
  @Validate
  public void postOrdersOrderLines(String lang, PoLine poLine, Map<String, String> okapiHeaders,
                                   Handler<AsyncResult<Response>> asyncResultHandler, Context vertxContext) {
    final HttpClientInterface httpClient = AbstractHelper.getHttpClient(okapiHeaders);
    PostOrderLineHelper helper = new PostOrderLineHelper(httpClient, okapiHeaders, asyncResultHandler, vertxContext, lang);

    logger.info("Creating POLine to an existing order...");
    String orderId = poLine.getPurchaseOrderId();
    if (orderId == null) {
      helper.handleError(new CompletionException(new ValidationException(MISSING_ORDER_ID, "id_not_exists")));
    } else {
      loadConfiguration(okapiHeaders, vertxContext, logger)
        .thenAccept(config -> {
          String endpoint = String.format(GET_ALL_POLINES_QUERY_WITH_LIMIT, 1, orderId, lang);
          handleGetRequest(endpoint, httpClient, vertxContext, okapiHeaders, logger)
            .thenAccept(entries -> {
              int limit = getPoLineLimit(config);
              if (entries.getInteger("total_records") < limit) {
                helper.createPoLine(poLine)
                  .thenAccept(pol -> {
                    logger.info("Successfully added PO Line: " + JsonObject.mapFrom(pol).encodePrettily());
                    httpClient.closeClient();
                    Response response = PostOrdersOrderLinesResponse.respond201WithApplicationJson
                      (poLine, PostOrdersOrderLinesResponse.headersFor201().withLocation(String.format(ORDER_LINE_LOCATION_PREFIX, pol.getId())));
                    asyncResultHandler.handle(succeededFuture(response));
                  })
                  .exceptionally(helper::handleError);
              } else {
                throw new ValidationException(String.format(OVER_LIMIT_ERROR_MESSAGE, limit), LINES_LIMIT_ERROR_CODE);
              }
            }).exceptionally(helper::handleError);
        }).exceptionally(helper::handleError);
    }
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
      .thenAccept(poNumberJson -> {
        logger.info("Received PoNumber Response: " + poNumberJson.encodePrettily());
        httpClient.closeClient();
        javax.ws.rs.core.Response response = GetOrdersPoNumberResponse.respond200WithApplicationJson(poNumberJson.mapTo(PoNumber.class));
        AsyncResult<javax.ws.rs.core.Response> result = succeededFuture(response);
        asyncResultHandler.handle(result);
      }).exceptionally(helper::handleError);
  }

  @Override
  @Validate
  public void putOrdersOrderLinesById(String lineId, String lang, PoLine poLine, Map<String, String> okapiHeaders,
                                          Handler<AsyncResult<Response>> asyncResultHandler, Context vertxContext) {
    logger.info("Handling PUT Order Line operation...");

    PutOrderLineByIdHelper helper = new PutOrderLineByIdHelper(okapiHeaders, asyncResultHandler, vertxContext, lang);
    if (StringUtils.isEmpty(poLine.getId())) {
      poLine.setId(lineId);
    }
    if (lineId.equals(poLine.getId())) {
      helper.updateOrderLine(poLine);
    } else {
      helper.handleError(new CompletionException(new ValidationException(MISMATCH_BETWEEN_ID_IN_PATH_AND_PO_LINE, AbstractHelper.ID_MISMATCH_ERROR_CODE)));
    }
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
    // Return 501 (Not Implemented) for now
    asyncResultHandler.handle(succeededFuture(Response.status(501).build()));
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
    if (compPO.getPoLines().size() > limit) {
      throw new ValidationException(String.format(OVER_LIMIT_ERROR_MESSAGE, limit), LINES_LIMIT_ERROR_CODE);
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

