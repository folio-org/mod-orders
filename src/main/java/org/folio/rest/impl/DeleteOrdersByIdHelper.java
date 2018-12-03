package org.folio.rest.impl;

import java.util.Map;
import java.util.concurrent.CompletableFuture;

import javax.ws.rs.core.Response;

import org.folio.orders.rest.exceptions.HttpException;
import org.folio.orders.utils.HelperUtils;
import org.folio.rest.jaxrs.resource.Orders.DeleteOrdersByIdResponse;
import org.folio.rest.tools.client.interfaces.HttpClientInterface;

import io.vertx.core.AsyncResult;
import io.vertx.core.Context;
import io.vertx.core.Future;
import io.vertx.core.Handler;
import io.vertx.core.http.HttpMethod;
import io.vertx.core.logging.Logger;
import io.vertx.core.logging.LoggerFactory;
import me.escoffier.vertx.completablefuture.VertxCompletableFuture;

public class DeleteOrdersByIdHelper {
  private static final Logger logger = LoggerFactory.getLogger(DeleteOrdersByIdHelper.class);

  private final HttpClientInterface httpClient;
  private final Context ctx;
  private final Handler<AsyncResult<javax.ws.rs.core.Response>> asyncResultHandler;
  private final Map<String, String> okapiHeaders;

  public DeleteOrdersByIdHelper(HttpClientInterface httpClient, Map<String, String> okapiHeaders,
      Handler<AsyncResult<Response>> asyncResultHandler, Context ctx) {
    this.httpClient = httpClient;
    this.okapiHeaders = okapiHeaders;
    this.ctx = ctx;
    this.asyncResultHandler = asyncResultHandler;
  }

  public CompletableFuture<Void> deleteOrder(String id, String lang) {
    CompletableFuture<Void> future = new VertxCompletableFuture<>(ctx);

      HelperUtils.deletePoLines(id, lang, httpClient, ctx, okapiHeaders, logger)
      .thenRun(()-> {
        HelperUtils.operateOnSubObj(HttpMethod.DELETE,"/purchase_order/"+id, httpClient, ctx, okapiHeaders, logger);       
        future.complete(null);      
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
      final String message = t.getMessage();
      switch (code) {
      case 404:
        result = Future.succeededFuture(DeleteOrdersByIdResponse.respond404WithTextPlain(message));
        break;
      case 500:
        result = Future.succeededFuture(DeleteOrdersByIdResponse.respond500WithTextPlain(message));
        break;
      default:
        result = Future.succeededFuture(DeleteOrdersByIdResponse.respond500WithTextPlain(message));
      }
    } else {
      result = Future.succeededFuture(DeleteOrdersByIdResponse.respond500WithTextPlain(throwable.getMessage()));
    }

    httpClient.closeClient();

    asyncResultHandler.handle(result);

    return null;
  }
}
