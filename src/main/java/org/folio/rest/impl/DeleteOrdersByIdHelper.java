package org.folio.rest.impl;

import java.util.HashMap;
import java.util.Map;
import java.util.concurrent.CompletableFuture;

import javax.ws.rs.core.Response;

import org.apache.log4j.Logger;
import org.folio.orders.rest.exceptions.HttpException;
import org.folio.orders.utils.HelperUtils;
import org.folio.rest.jaxrs.resource.OrdersResource.GetOrdersByIdResponse;
import org.folio.rest.tools.client.interfaces.HttpClientInterface;

import io.vertx.core.AsyncResult;
import io.vertx.core.Context;
import io.vertx.core.Future;
import io.vertx.core.Handler;
import io.vertx.core.http.HttpHeaders;
import io.vertx.core.http.HttpMethod;
import me.escoffier.vertx.completablefuture.VertxCompletableFuture;

public class DeleteOrdersByIdHelper {
  private static final Logger logger = Logger.getLogger(DeleteOrdersByIdHelper.class);

  private final HttpClientInterface httpClient;
  private final Context ctx;
  private final Handler<AsyncResult<javax.ws.rs.core.Response>> asyncResultHandler;
  private final Map<String, String> okapiHeaders;

  public DeleteOrdersByIdHelper(HttpClientInterface httpClient, Map<String, String> okapiHeaders,
      Handler<AsyncResult<Response>> asyncResultHandler, Context ctx) {
    Map<String,String> customHeader=new HashMap<>();
    customHeader.put(HttpHeaders.ACCEPT.toString(), "application/json, text/plain");
    httpClient.setDefaultHeaders(customHeader);
    
    this.httpClient = httpClient;
    this.okapiHeaders = okapiHeaders;
    this.ctx = ctx;
    this.asyncResultHandler = asyncResultHandler;
  }

  public CompletableFuture<Void> deleteOrder(String id, String lang) {
    CompletableFuture<Void> future = new VertxCompletableFuture<>(ctx);

      HelperUtils.deletePoLines(id, lang, httpClient, ctx, okapiHeaders, logger)
      .thenRun(()-> {
        HelperUtils.operateOnSubObj(HttpMethod.DELETE,"/purchase_order/"+id, httpClient, ctx, okapiHeaders, logger)
        .thenAccept(action -> 
          future.complete(null)
        );          
      })
      .exceptionally(t -> {
        logger.error("Failed to delete PO", t);
        future.completeExceptionally(t.getCause());
        return null;
      });
    
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
        result = Future.succeededFuture(GetOrdersByIdResponse.withPlainNotFound(message));
        break;
      case 500:
        result = Future.succeededFuture(GetOrdersByIdResponse.withPlainInternalServerError(message));
        break;
      default:
        result = Future.succeededFuture(GetOrdersByIdResponse.withPlainInternalServerError(message));
      }
    } else {
      result = Future.succeededFuture(GetOrdersByIdResponse.withPlainInternalServerError(throwable.getMessage()));
    }

    httpClient.closeClient();

    asyncResultHandler.handle(result);

    return null;
  }
}
