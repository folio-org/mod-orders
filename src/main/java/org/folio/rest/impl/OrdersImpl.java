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
import org.folio.rest.jaxrs.model.CompositePurchaseOrder;
import org.folio.rest.jaxrs.model.PoLine;
import org.folio.rest.jaxrs.model.PoNumber;
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

  private static final String ORDERS_LOCATION_PREFIX = "/orders/";
  private static final String ORDER_LINE_LOCATION_PREFIX = "/orders/%s/lines/%s";
  public static final String OVER_LIMIT_ERROR_MESSAGE = "Your FOLIO system is configured to limit the number of PO Lines on each order to %s.";
  public static final String MISMATCH_BETWEEN_ID_IN_PATH_AND_PO_LINE = "Mismatch between id in path and PoLine";
  public static final String LINES_LIMIT_ERROR_CODE = "lines_limit";

  @Override
  @Validate
  public void deleteOrdersById(String id, String lang, Map<String, String> okapiHeaders,
      Handler<AsyncResult<javax.ws.rs.core.Response>> asyncResultHandler, Context vertxContext) {

    DeleteOrdersByIdHelper helper = new DeleteOrdersByIdHelper(okapiHeaders, asyncResultHandler, vertxContext, lang);
    helper.deleteOrder(id)
    .thenRun(()->{
      logger.info("Successfully deleted order: ");
      javax.ws.rs.core.Response response = DeleteOrdersByIdResponse.respond204();
      AsyncResult<javax.ws.rs.core.Response> result = succeededFuture(response);
      asyncResultHandler.handle(result);
    })
    .exceptionally(helper::handleError);
  }

  @Override
  @Validate
  public void getOrdersById(String id, String lang, Map<String, String> okapiHeaders,
      Handler<AsyncResult<javax.ws.rs.core.Response>> asyncResultHandler, Context vertxContext) {
    final HttpClientInterface httpClient = AbstractHelper.getHttpClient(okapiHeaders);
    GetOrdersByIdHelper helper = new GetOrdersByIdHelper(httpClient, okapiHeaders, asyncResultHandler, vertxContext, lang);

    helper.getOrder(id)
      .thenAccept(order -> {
        logger.info("Successfully retrieved order: " + JsonObject.mapFrom(order).encodePrettily());
        httpClient.closeClient();
        javax.ws.rs.core.Response response = GetOrdersByIdResponse.respond200WithApplicationJson(order);
        AsyncResult<javax.ws.rs.core.Response> result = succeededFuture(response);
        asyncResultHandler.handle(result);
      })
      .exceptionally(helper::handleError);
  }

  @Override
  @Validate
  public void postOrders(String lang, CompositePurchaseOrder compPO, Map<String, String> okapiHeaders,
      Handler<AsyncResult<javax.ws.rs.core.Response>> asyncResultHandler, Context vertxContext) {

    final HttpClientInterface httpClient = AbstractHelper.getHttpClient(okapiHeaders);
    PostOrdersHelper helper = new PostOrdersHelper(httpClient, okapiHeaders, asyncResultHandler, vertxContext, lang);
    loadConfiguration(okapiHeaders, vertxContext, logger).thenAccept(config -> {
      int limit = getPoLineLimit(config);
      if (compPO.getPoLines().size() <= limit) {
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
                    javax.ws.rs.core.Response response = PostOrdersResponse.respond201WithApplicationJson(withInventory,
                      PostOrdersResponse.headersFor201().withLocation(ORDERS_LOCATION_PREFIX + withInventory.getId()));
                    AsyncResult<javax.ws.rs.core.Response> result = Future.succeededFuture(response);
                    asyncResultHandler.handle(result);
                  })
                  .exceptionally(helper::handleError);
              })
              .exceptionally(helper::handleError);
          })
          .exceptionally(helper::handleError);
      } else {
        throw new ValidationException(String.format(OVER_LIMIT_ERROR_MESSAGE, limit), LINES_LIMIT_ERROR_CODE);
      }
    }).exceptionally(helper::handleError);
  }

  @Override
  @Validate
  public void putOrdersById(String orderId, String lang, CompositePurchaseOrder compPO, Map<String, String> okapiHeaders,
      Handler<AsyncResult<javax.ws.rs.core.Response>> asyncResultHandler, Context vertxContext) {
    PutOrdersByIdHelper putHelper = new PutOrdersByIdHelper(okapiHeaders, asyncResultHandler, vertxContext, lang);
    if (CollectionUtils.isEmpty(compPO.getPoLines())) {
      putHelper.updateOrder(orderId, compPO);
    } else {
      loadConfiguration(okapiHeaders, vertxContext, logger).thenAccept(config -> {
        int limit = getPoLineLimit(config);
        if (compPO.getPoLines().size() > limit) {
          throw new ValidationException(String.format(OVER_LIMIT_ERROR_MESSAGE, limit), LINES_LIMIT_ERROR_CODE);
        }
        compPO.getPoLines().forEach(poLine -> {
          if (StringUtils.isEmpty(poLine.getPurchaseOrderId())) {
            poLine.setPurchaseOrderId(orderId);
          }
          if (!orderId.equals(poLine.getPurchaseOrderId())) {
            throw new ValidationException(MISMATCH_BETWEEN_ID_IN_PATH_AND_PO_LINE, AbstractHelper.ID_MISMATCH_ERROR_CODE);
          }
        });
        putHelper.updateOrderWithPoLines(orderId, compPO);
      }).exceptionally(putHelper::handleError);
    }

  }

  @Override
  @Validate
  public void postOrdersLinesById(String orderId, String lang, PoLine poLine, Map<String, String> okapiHeaders,
                                  Handler<AsyncResult<javax.ws.rs.core.Response>> asyncResultHandler, Context vertxContext) {
    final HttpClientInterface httpClient = AbstractHelper.getHttpClient(okapiHeaders);
    PostOrderLineHelper helper = new PostOrderLineHelper(httpClient, okapiHeaders, asyncResultHandler, vertxContext, lang);

    logger.info("Creating POLine to an existing order...");

    if (poLine.getPurchaseOrderId() == null) {
      logger.info("POLine without id. Set id from path url: " + orderId);
      poLine.setPurchaseOrderId(orderId);
    }
    loadConfiguration(okapiHeaders, vertxContext, logger).thenAccept(config -> {
      if (orderId.equals(poLine.getPurchaseOrderId())) {
        String endpoint = String.format(GET_ALL_POLINES_QUERY_WITH_LIMIT, 1, orderId, lang);
        handleGetRequest(endpoint, httpClient, vertxContext, okapiHeaders, logger)
          .thenAccept(entries -> {
            int limit = getPoLineLimit(config);
            if (entries.getInteger("total_records") < limit) {
              helper.createPoLine(poLine, false)
                .thenAccept(pol -> {
                  logger.info("Successfully added PO Line: " + JsonObject.mapFrom(pol).encodePrettily());
                  httpClient.closeClient();
                  Response response = PostOrdersLinesByIdResponse.respond201WithApplicationJson
                    (poLine, PostOrdersLinesByIdResponse.headersFor201().withLocation(String.format(ORDER_LINE_LOCATION_PREFIX, orderId, pol.getId())));
                  asyncResultHandler.handle(succeededFuture(response));
                })
                .exceptionally(helper::handleError);
            } else {
              throw new ValidationException(String.format(OVER_LIMIT_ERROR_MESSAGE, limit), LINES_LIMIT_ERROR_CODE);
            }
          }).exceptionally(helper::handleError);
      } else {
        throw new ValidationException(MISMATCH_BETWEEN_ID_IN_PATH_AND_PO_LINE, AbstractHelper.ID_MISMATCH_ERROR_CODE);
      }
    }).exceptionally(helper::handleError);
  }

  @Override
  @Validate
  public void getOrdersLinesByIdAndLineId(String id, String lineId, String lang, Map<String, String> okapiHeaders,
                                          Handler<AsyncResult<javax.ws.rs.core.Response>> asyncResultHandler, Context vertxContext) {

    logger.info("Started Invocation of POLine Request with id = " + lineId);
    final HttpClientInterface httpClient = AbstractHelper.getHttpClient(okapiHeaders);
    GetPOLineByIdHelper helper = new GetPOLineByIdHelper(httpClient, okapiHeaders, asyncResultHandler, vertxContext, lang);

    helper.getPOLineByPOLineId(id, lineId)
      .thenAccept(poline -> {
        logger.info("Received POLine Response: " + JsonObject.mapFrom(poline).encodePrettily());
        httpClient.closeClient();
        javax.ws.rs.core.Response response = GetOrdersLinesByIdAndLineIdResponse.respond200WithApplicationJson(poline);
        AsyncResult<javax.ws.rs.core.Response> result = succeededFuture(response);
        asyncResultHandler.handle(result);
      })
      .exceptionally(helper::handleError);
  }

  @Override
  @Validate
  public void deleteOrdersLinesByIdAndLineId(String orderId, String lineId, String lang, Map<String, String> okapiHeaders,
                                             Handler<AsyncResult<javax.ws.rs.core.Response>> asyncResultHandler, Context vertxContext) {
    new DeleteOrderLineByIdHelper(okapiHeaders, asyncResultHandler, vertxContext, lang)
      .deleteLine(orderId, lineId);
  }

  @Override
  @Validate
  public void putOrdersLinesByIdAndLineId(String orderId, String lineId, String lang, PoLine poLine, Map<String, String> okapiHeaders,
                                            Handler<AsyncResult<javax.ws.rs.core.Response>> asyncResultHandler, Context vertxContext) {
    logger.info("Handling PUT Order Line operation...");

    PutOrderLineByIdHelper helper = new PutOrderLineByIdHelper(okapiHeaders, asyncResultHandler, vertxContext, lang);
    if (StringUtils.isEmpty(poLine.getPurchaseOrderId())) {
      poLine.setPurchaseOrderId(orderId);
    }
    if (StringUtils.isEmpty(poLine.getId())) {
      poLine.setId(lineId);
    }
    if (orderId.equals(poLine.getPurchaseOrderId()) && lineId.equals(poLine.getId())) {
      helper.updateOrderLine(orderId, poLine);
    } else {
      helper.handleError(new CompletionException(new ValidationException(MISMATCH_BETWEEN_ID_IN_PATH_AND_PO_LINE, AbstractHelper.ID_MISMATCH_ERROR_CODE)));
    }
  }

  @Override
  @Validate
  public void postOrdersPoNumberValidate(String lang, PoNumber entity, Map<String, String> okapiHeaders,
     Handler<AsyncResult<Response>> asyncResultHandler, Context vertxContext) {
    final HttpClientInterface httpClient = AbstractHelper.getHttpClient(okapiHeaders);
    ValidationHelper helper=new ValidationHelper(httpClient, okapiHeaders, asyncResultHandler, vertxContext, lang);
    logger.info("Validating a PO Number");
    //@Validate asserts the pattern of a PO Number, the below method is used to check for uniqueness
     helper.checkPONumberUnique(entity, lang);

  }

  private static int getPoLineLimit(JsonObject config) {
    try {
      return Integer.parseInt(config.getString(PO_LINES_LIMIT_PROPERTY, DEFAULT_POLINE_LIMIT));
    } catch (NumberFormatException e) {
      throw new NumberFormatException("Invalid limit value in configuration.");
    }
  }

}

