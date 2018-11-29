package org.folio.rest.impl;

import java.util.HashMap;
import java.util.Map;
import java.util.concurrent.CompletableFuture;

import javax.ws.rs.core.Response;

import org.apache.log4j.Logger;
import org.folio.orders.rest.exceptions.HttpException;
import org.folio.rest.jaxrs.model.CompositePurchaseOrder;
import org.folio.rest.jaxrs.resource.OrdersResource.PutOrdersByIdResponse;
import org.folio.rest.tools.client.interfaces.HttpClientInterface;

import io.vertx.core.AsyncResult;
import io.vertx.core.Context;
import io.vertx.core.Future;
import io.vertx.core.Handler;
import io.vertx.core.http.HttpHeaders;
import io.vertx.core.json.JsonObject;
import me.escoffier.vertx.completablefuture.VertxCompletableFuture;

public class PutOrdersByIdHelper {
  private static final Logger logger = Logger.getLogger(DeleteOrdersByIdHelper.class);

  private final HttpClientInterface httpClient;
  private final Context ctx;
  private final Handler<AsyncResult<javax.ws.rs.core.Response>> asyncResultHandler;
  private final Map<String, String> okapiHeaders;

  public PutOrdersByIdHelper(HttpClientInterface httpClient, Map<String, String> okapiHeaders,
      Handler<AsyncResult<Response>> asyncResultHandler, Context ctx) {
    Map<String,String> customHeader=new HashMap<>();
    customHeader.put(HttpHeaders.ACCEPT.toString(), "application/json, text/plain");
    httpClient.setDefaultHeaders(customHeader);
    
    this.httpClient = httpClient;
    this.okapiHeaders = okapiHeaders;
    this.ctx = ctx;
    this.asyncResultHandler = asyncResultHandler;
  }
  
  public CompletableFuture<Void> updateOrder(String id, String lang, CompositePurchaseOrder compPO, Context vertxContext) {
    CompletableFuture<Void> future = new VertxCompletableFuture<>(ctx);
    
    PostOrdersHelper postHelper = new PostOrdersHelper(httpClient, okapiHeaders, asyncResultHandler, vertxContext);
    DeleteOrdersByIdHelper delHelper = new DeleteOrdersByIdHelper(httpClient, okapiHeaders, asyncResultHandler, vertxContext);
    
    delHelper.deleteOrder(id,lang)
    .thenRun(() -> {
      try {
        compPO.setId(id);
        logger.info("POST id is: -------------------------> "+ id);
        postHelper.createPOandPOLines(compPO)
        .thenAccept(withCompPO -> {
          logger.info("Successfully Placed Order: " + JsonObject.mapFrom(withCompPO).encodePrettily());
          httpClient.closeClient();
          javax.ws.rs.core.Response response = PutOrdersByIdResponse.withNoContent();
          AsyncResult<javax.ws.rs.core.Response> result = Future.succeededFuture(response);
          asyncResultHandler.handle(result);
        })
        .exceptionally(postHelper::handleError);
      } catch (Exception e) {
        logger.error(e);
      }
    })
    .exceptionally(delHelper::handleError);
    return future;
  }
  
  public Void handleError(Throwable throwable) {
    final Future<javax.ws.rs.core.Response> result;

    logger.error("Exception querying for orders", throwable.getCause());

    final Throwable t = throwable.getCause();
    if (t instanceof HttpException) {
      final int code = ((HttpException) t).getCode();
      final String message = ((HttpException) t).getMessage();
      switch (code) {
      case 404:
        result = Future.succeededFuture(PutOrdersByIdResponse.withPlainNotFound(message));
        break;
      case 500:
        result = Future.succeededFuture(PutOrdersByIdResponse.withPlainInternalServerError(message));
        break;
      default:
        result = Future.succeededFuture(PutOrdersByIdResponse.withPlainInternalServerError(message));
      }
    } else {
      result = Future.succeededFuture(PutOrdersByIdResponse.withPlainInternalServerError(throwable.getMessage()));
    }

    httpClient.closeClient();

    asyncResultHandler.handle(result);

    return null;
  }
}
