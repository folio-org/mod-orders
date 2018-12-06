package org.folio.rest.impl;

import java.util.HashMap;
import java.util.Map;

import org.apache.commons.lang3.NotImplementedException;
import org.folio.rest.RestVerticle;
import org.folio.rest.jaxrs.model.CompositePoLine;
import org.folio.rest.jaxrs.model.CompositePurchaseOrder;
import org.folio.rest.jaxrs.resource.Orders;
import org.folio.rest.tools.client.HttpClientFactory;
import org.folio.rest.tools.client.interfaces.HttpClientInterface;
import org.folio.rest.tools.utils.TenantTool;

import io.vertx.core.AsyncResult;
import io.vertx.core.Context;
import io.vertx.core.Future;
import io.vertx.core.Handler;
import io.vertx.core.http.HttpHeaders;
import io.vertx.core.json.JsonObject;
import io.vertx.core.logging.Logger;
import io.vertx.core.logging.LoggerFactory;

public class OrdersImpl implements Orders {

  private static final Logger logger = LoggerFactory.getLogger(OrdersImpl.class);

  public static final String OKAPI_HEADER_URL = "X-Okapi-Url";
  private static final String ORDERS_LOCATION_PREFIX = "/orders/";

  @Override
  public void deleteOrdersById(String id, String lang, Map<String, String> okapiHeaders,
      Handler<AsyncResult<javax.ws.rs.core.Response>> asyncResultHandler, Context vertxContext) {
    final HttpClientInterface httpClient = getHttpClient(okapiHeaders);
    
    //to handle delete API's content-type text/plain  
    Map<String,String> customHeader=new HashMap<>();
    customHeader.put(HttpHeaders.ACCEPT.toString(), "application/json, text/plain");
    httpClient.setDefaultHeaders(customHeader);
    
    DeleteOrdersByIdHelper helper = new DeleteOrdersByIdHelper(httpClient, okapiHeaders, asyncResultHandler, vertxContext);
    helper.deleteOrder(id,lang)
    .thenRun(()->{
      logger.info("Successfully deleted order: ");
      httpClient.closeClient();
      javax.ws.rs.core.Response response = DeleteOrdersByIdResponse.respond204();
      AsyncResult<javax.ws.rs.core.Response> result = Future.succeededFuture(response);
      asyncResultHandler.handle(result);    
    })
    .exceptionally(helper::handleError);
  }

  @Override
  public void getOrders(String query, int offset, int limit, String lang, Map<String, String> okapiHeaders,
      Handler<AsyncResult<javax.ws.rs.core.Response>> asyncResultHandler, Context vertxContext) {

    final HttpClientInterface httpClient = getHttpClient(okapiHeaders);
    GetOrdersHelper helper = new GetOrdersHelper(httpClient, okapiHeaders, asyncResultHandler, vertxContext);

    helper.getOrders(query, offset, limit, lang)
      .thenAccept(orders -> {
        logger.info("Successfully queried orders: " + JsonObject.mapFrom(orders).encodePrettily());
        httpClient.closeClient();
        javax.ws.rs.core.Response response = GetOrdersResponse.respond200WithApplicationJson(orders);
        AsyncResult<javax.ws.rs.core.Response> result = Future.succeededFuture(response);
        asyncResultHandler.handle(result);
      })
      .exceptionally(helper::handleError);
  }

  @Override
  public void getOrdersById(String id, String lang, Map<String, String> okapiHeaders,
      Handler<AsyncResult<javax.ws.rs.core.Response>> asyncResultHandler, Context vertxContext) {

    final HttpClientInterface httpClient = getHttpClient(okapiHeaders);
    GetOrdersByIdHelper helper = new GetOrdersByIdHelper(httpClient, okapiHeaders, asyncResultHandler, vertxContext);

    helper.getOrder(id, lang)
      .thenAccept(order -> {
        logger.info("Successfully retrieved order: " + JsonObject.mapFrom(order).encodePrettily());
        httpClient.closeClient();
        javax.ws.rs.core.Response response = GetOrdersByIdResponse.respond200WithApplicationJson(order);
        AsyncResult<javax.ws.rs.core.Response> result = Future.succeededFuture(response);
        asyncResultHandler.handle(result);
      })
      .exceptionally(helper::handleError);
  }

  @Override
  public void postOrders(String lang, CompositePurchaseOrder compPO, Map<String, String> okapiHeaders,
      Handler<AsyncResult<javax.ws.rs.core.Response>> asyncResultHandler, Context vertxContext) {

    final HttpClientInterface httpClient = getHttpClient(okapiHeaders);
    PostOrdersHelper helper = new PostOrdersHelper(httpClient, okapiHeaders, asyncResultHandler, vertxContext);

    logger.info("Creating PO and POLines...");
    helper.createPOandPOLines(compPO)
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
  }

  @Override
  public void putOrdersById(String id, String lang, CompositePurchaseOrder compPO, Map<String, String> okapiHeaders,
      Handler<AsyncResult<javax.ws.rs.core.Response>> asyncResultHandler, Context vertxContext) {
        final HttpClientInterface httpClient = getHttpClient(okapiHeaders);
        PutOrdersByIdHelper putHelper = new PutOrdersByIdHelper(httpClient, okapiHeaders, asyncResultHandler, vertxContext);
       putHelper.updateOrder(id, lang, compPO, vertxContext)
         .exceptionally(putHelper::handleError);
  }

  @Override
  public void postOrdersLinesById(String id, String lang, CompositePoLine entity, Map<String, String> okapiHeaders,
                                  Handler<AsyncResult<javax.ws.rs.core.Response>> asyncResultHandler, Context vertxContext) {
    asyncResultHandler.handle(Future.failedFuture(new NotImplementedException("POST PO line is not implemented yet")));
  }

  @Override
  public void getOrdersLinesByIdAndLineId(String id, String lineId, String lang, Map<String, String> okapiHeaders,
                                          Handler<AsyncResult<javax.ws.rs.core.Response>> asyncResultHandler, Context vertxContext) {

    logger.info("Started Invocation of POLine Request with id = " + lineId);
    final HttpClientInterface httpClient = getHttpClient(okapiHeaders);
    GetPOLineByIdHelper helper = new GetPOLineByIdHelper(httpClient, okapiHeaders, asyncResultHandler, vertxContext);

    helper.getCompositePOLineByPOLineId(id, lineId, lang)
      .thenAccept(poline -> {
        logger.info("Received POLine Response: " + JsonObject.mapFrom(poline).encodePrettily());
        httpClient.closeClient();
        javax.ws.rs.core.Response response = GetOrdersLinesByIdAndLineIdResponse.respond200WithApplicationJson(poline);
        AsyncResult<javax.ws.rs.core.Response> result = Future.succeededFuture(response);
        asyncResultHandler.handle(result);
      })
      .exceptionally(helper::handleError);
  }

  @Override
  public void deleteOrdersLinesByIdAndLineId(String id, String lineId, String lang, Map<String, String> okapiHeaders,
                                               Handler<AsyncResult<javax.ws.rs.core.Response>> asyncResultHandler, Context vertxContext) {
    asyncResultHandler.handle(Future.failedFuture(new NotImplementedException("DELETE PO line by id is not implemented yet")));
  }

  @Override
  public void putOrdersLinesByIdAndLineId(String id, String lineId, String lang, CompositePoLine entity, Map<String, String> okapiHeaders,
                                            Handler<AsyncResult<javax.ws.rs.core.Response>> asyncResultHandler, Context vertxContext) {
    asyncResultHandler.handle(Future.failedFuture(new NotImplementedException("PUT PO line by id is not implemented yet")));
  }

  public static HttpClientInterface getHttpClient(Map<String, String> okapiHeaders) {
    final String okapiURL = okapiHeaders.getOrDefault(OKAPI_HEADER_URL, "");
    final String tenantId = TenantTool.calculateTenantId(okapiHeaders.get(RestVerticle.OKAPI_HEADER_TENANT));
    
    return HttpClientFactory.getHttpClient(okapiURL, tenantId);
  }

}

