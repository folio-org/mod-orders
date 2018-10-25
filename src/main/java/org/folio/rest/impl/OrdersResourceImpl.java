package org.folio.rest.impl;

import java.util.Map;

import org.apache.log4j.Logger;
import org.folio.rest.RestVerticle;
import org.folio.rest.jaxrs.model.CompositePurchaseOrder;
import org.folio.rest.jaxrs.resource.OrdersResource;
import org.folio.rest.tools.client.HttpClientFactory;
import org.folio.rest.tools.client.interfaces.HttpClientInterface;
import org.folio.rest.tools.utils.TenantTool;

import io.vertx.core.AsyncResult;
import io.vertx.core.Context;
import io.vertx.core.Future;
import io.vertx.core.Handler;
import io.vertx.core.json.JsonObject;

public class OrdersResourceImpl implements OrdersResource {

  private static final Logger logger = Logger.getLogger(OrdersResourceImpl.class);

  public static final String OKAPI_HEADER_URL = "X-Okapi-Url";

  @Override
  public void deleteOrdersById(String id, String lang, Map<String, String> okapiHeaders,
      Handler<AsyncResult<javax.ws.rs.core.Response>> asyncResultHandler, Context vertxContext) throws Exception {
    // TODO Auto-generated method stub
  }

  @Override
  public void getOrders(String query, int offset, int limit, String lang, Map<String, String> okapiHeaders,
      Handler<AsyncResult<javax.ws.rs.core.Response>> asyncResultHandler, Context vertxContext) throws Exception {

    final HttpClientInterface httpClient = getHttpClient(okapiHeaders);
    GetOrdersHelper helper = new GetOrdersHelper(httpClient, okapiHeaders, asyncResultHandler, vertxContext);

    helper.getOrders(query, offset, limit, lang)
      .thenAccept(orders -> {
        logger.info("Successfully queried orders: " + JsonObject.mapFrom(orders).encodePrettily());
        httpClient.closeClient();
        javax.ws.rs.core.Response response = GetOrdersResponse.withJsonOK(orders);
        AsyncResult<javax.ws.rs.core.Response> result = Future.succeededFuture(response);
        asyncResultHandler.handle(result);
      })
      .exceptionally(helper::handleError);
  }

  @Override
  public void getOrdersById(String id, String lang, Map<String, String> okapiHeaders,
      Handler<AsyncResult<javax.ws.rs.core.Response>> asyncResultHandler, Context vertxContext) throws Exception {

    final HttpClientInterface httpClient = getHttpClient(okapiHeaders);
    GetOrdersByIdHelper helper = new GetOrdersByIdHelper(httpClient, okapiHeaders, asyncResultHandler, vertxContext);

    helper.getOrder(id, lang)
      .thenAccept(order -> {
        logger.info("Successfully retrieved order: " + JsonObject.mapFrom(order).encodePrettily());
        httpClient.closeClient();
        javax.ws.rs.core.Response response = GetOrdersByIdResponse.withJsonOK(order);
        AsyncResult<javax.ws.rs.core.Response> result = Future.succeededFuture(response);
        asyncResultHandler.handle(result);
      })
      .exceptionally(helper::handleError);
  }

  @Override
  public void postOrders(String lang, CompositePurchaseOrder compPO, Map<String, String> okapiHeaders,
      Handler<AsyncResult<javax.ws.rs.core.Response>> asyncResultHandler, Context vertxContext)
      throws Exception {

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
                javax.ws.rs.core.Response response = PostOrdersResponse.withJsonCreated("", withInventory);
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
      Handler<AsyncResult<javax.ws.rs.core.Response>> asyncResultHandler, Context vertxContext) throws Exception {
    // TODO Auto-generated method stub

  }

  public static HttpClientInterface getHttpClient(Map<String, String> okapiHeaders) {
    final String okapiURL = okapiHeaders.getOrDefault(OKAPI_HEADER_URL, "");
    final String tenantId = TenantTool.calculateTenantId(okapiHeaders.get(RestVerticle.OKAPI_HEADER_TENANT));

    return HttpClientFactory.getHttpClient(okapiURL, tenantId);
  }

}
