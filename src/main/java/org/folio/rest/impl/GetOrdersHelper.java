package org.folio.rest.impl;

import java.util.Map;
import java.util.concurrent.CompletableFuture;
import java.util.concurrent.CompletionException;

import javax.ws.rs.core.Response;

import org.folio.orders.rest.exceptions.HttpException;
import org.folio.orders.utils.HelperUtils;
import org.folio.rest.jaxrs.model.CompositePurchaseOrders;
import org.folio.rest.jaxrs.resource.Orders.GetOrdersResponse;
import org.folio.rest.tools.client.interfaces.HttpClientInterface;

import io.vertx.core.AsyncResult;
import io.vertx.core.Context;
import io.vertx.core.Future;
import io.vertx.core.Handler;
import io.vertx.core.json.JsonObject;
import io.vertx.core.logging.Logger;
import io.vertx.core.logging.LoggerFactory;
import me.escoffier.vertx.completablefuture.VertxCompletableFuture;

public class GetOrdersHelper {

  private static final Logger logger = LoggerFactory.getLogger(GetOrdersHelper.class);

  public static final String MOCK_DATA_PATH = "mockdata/getOrders.json";

  private final HttpClientInterface httpClient;
  private final Context ctx;
  private final Handler<AsyncResult<javax.ws.rs.core.Response>> asyncResultHandler;
  private final Map<String, String> okapiHeaders;

  public GetOrdersHelper(HttpClientInterface httpClient, Map<String, String> okapiHeaders,
      Handler<AsyncResult<Response>> asyncResultHandler, Context ctx) {
    this.httpClient = httpClient;
    this.okapiHeaders = okapiHeaders;
    this.ctx = ctx;
    this.asyncResultHandler = asyncResultHandler;
  }

  public CompletableFuture<CompositePurchaseOrders> getOrders(String query, int offset, int limit, String lang) {
    CompletableFuture<CompositePurchaseOrders> future = new VertxCompletableFuture<>(ctx);

    //TODO replace this with a call to mod-orders-storage
    getMockOrders()
      .thenAccept(orders -> {
        logger.info("Returning mock data: " + JsonObject.mapFrom(orders).encodePrettily());
        future.complete(orders);
      })
      .exceptionally(t -> {
        logger.error("Error getting orders", t);
        future.completeExceptionally(t.getCause());
        return null;
      });

    return future;
  }

  private CompletableFuture<CompositePurchaseOrders> getMockOrders() {
    return VertxCompletableFuture.supplyAsync(ctx, () -> {
      try {
        JsonObject json = new JsonObject(HelperUtils.getMockData(MOCK_DATA_PATH));
        return json.mapTo(CompositePurchaseOrders.class);
      } catch (Exception e) {
        logger.error("Failed to read mock data", e);
        throw new CompletionException(e);
      }
    });
  }

  public Void handleError(Throwable throwable) {
    final Future<javax.ws.rs.core.Response> result;

    logger.error("Exception querying for orders", throwable.getCause());

    final Throwable t = throwable.getCause();
    if (t instanceof HttpException) {
      final int code = ((HttpException) t).getCode();
      final String message = t.getMessage();
      switch (code) {
      case 400:
        result = Future.succeededFuture(GetOrdersResponse.respond400WithTextPlain(message));
        break;
      case 500:
        result = Future.succeededFuture(GetOrdersResponse.respond500WithTextPlain(message));
        break;
      case 401:
        result = Future.succeededFuture(GetOrdersResponse.respond401WithTextPlain(message));
        break;
      default:
        result = Future.succeededFuture(GetOrdersResponse.respond500WithTextPlain(message));
      }
    } else {
      result = Future.succeededFuture(GetOrdersResponse.respond500WithTextPlain(throwable.getMessage()));
    }

    httpClient.closeClient();

    asyncResultHandler.handle(result);

    return null;
  }

}
