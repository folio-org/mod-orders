package org.folio.rest.impl;

import java.util.Map;
import java.util.concurrent.CompletableFuture;

import javax.ws.rs.core.Response;

import org.folio.orders.rest.exceptions.HttpException;
import org.folio.orders.utils.HelperUtils;
import org.folio.rest.jaxrs.model.CompositePurchaseOrder;
import org.folio.rest.jaxrs.resource.Orders.GetOrdersByIdResponse;
import org.folio.rest.tools.client.interfaces.HttpClientInterface;

import io.vertx.core.AsyncResult;
import io.vertx.core.Context;
import io.vertx.core.Future;
import io.vertx.core.Handler;
import io.vertx.core.json.JsonObject;
import io.vertx.core.logging.Logger;
import io.vertx.core.logging.LoggerFactory;
import me.escoffier.vertx.completablefuture.VertxCompletableFuture;

public class GetOrdersByIdHelper {

  private static final Logger logger = LoggerFactory.getLogger(GetOrdersByIdHelper.class);

  private final HttpClientInterface httpClient;
  private final Context ctx;
  private final Handler<AsyncResult<javax.ws.rs.core.Response>> asyncResultHandler;
  private final Map<String, String> okapiHeaders;

  public GetOrdersByIdHelper(HttpClientInterface httpClient, Map<String, String> okapiHeaders,
      Handler<AsyncResult<Response>> asyncResultHandler, Context ctx) {
    this.httpClient = httpClient;
    this.okapiHeaders = okapiHeaders;
    this.ctx = ctx;
    this.asyncResultHandler = asyncResultHandler;
  }

  public CompletableFuture<CompositePurchaseOrder> getOrder(String id, String lang) {
    CompletableFuture<CompositePurchaseOrder> future = new VertxCompletableFuture<>(ctx);

    getCompositePurchaseOrderById(id, lang)
      .thenAccept(orders -> {
        logger.info("Returning Composite Purchase Order: " + JsonObject.mapFrom(orders).encodePrettily());
        future.complete(orders);
      })
      .exceptionally(t -> {
        logger.error("Error getting orders", t);
        future.completeExceptionally(t.getCause());
        return null;
      });

    return future;
  }

  private CompletableFuture<CompositePurchaseOrder> getCompositePurchaseOrderById(String id, String lang) {
    CompletableFuture<CompositePurchaseOrder> future = new VertxCompletableFuture<>(ctx);

    HelperUtils.getPurchaseOrder(id, lang, httpClient, ctx, okapiHeaders, logger)
      .thenAccept(po -> {
        logger.info("got: " + po.encodePrettily());
        po.remove("adjustment");
        CompositePurchaseOrder compPO = po.mapTo(CompositePurchaseOrder.class);

        HelperUtils.getPoLines(id, lang, httpClient, ctx, okapiHeaders, logger)
          .thenAccept(poLines -> {
            compPO.setPoLines(poLines);
            compPO.setAdjustment(HelperUtils.calculateAdjustment(poLines));
            future.complete(compPO);
          })
          .exceptionally(t -> {
            logger.error("Failed to get POLines", t);
            future.completeExceptionally(t.getCause());
            return null;
          });
      })
      .exceptionally(t -> {
        logger.error("Failed to build composite purchase order", t.getCause());
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
        result = Future.succeededFuture(GetOrdersByIdResponse.respond404WithTextPlain(message));
        break;
      case 500:
        result = Future.succeededFuture(GetOrdersByIdResponse.respond500WithTextPlain(message));
        break;
      default:
        result = Future.succeededFuture(GetOrdersByIdResponse.respond500WithTextPlain(message));
      }
    } else {
      result = Future.succeededFuture(GetOrdersByIdResponse.respond500WithTextPlain(throwable.getMessage()));
    }

    httpClient.closeClient();

    asyncResultHandler.handle(result);

    return null;
  }

}
