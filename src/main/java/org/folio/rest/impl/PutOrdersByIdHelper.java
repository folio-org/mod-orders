package org.folio.rest.impl;

import io.vertx.core.AsyncResult;
import io.vertx.core.Context;
import io.vertx.core.Future;
import io.vertx.core.Handler;
import io.vertx.core.http.HttpHeaders;
import io.vertx.core.json.JsonObject;
import io.vertx.core.logging.Logger;
import io.vertx.core.logging.LoggerFactory;
import me.escoffier.vertx.completablefuture.VertxCompletableFuture;
import org.folio.orders.rest.exceptions.HttpException;
import org.folio.rest.jaxrs.model.CompositePurchaseOrder;
import org.folio.rest.jaxrs.resource.Orders.PutOrdersByIdResponse;
import org.folio.rest.tools.client.interfaces.HttpClientInterface;

import javax.ws.rs.core.Response;
import java.util.HashMap;
import java.util.Map;
import java.util.concurrent.CompletableFuture;

public class PutOrdersByIdHelper {
  private static final Logger logger = LoggerFactory.getLogger(DeleteOrdersByIdHelper.class);

  private final HttpClientInterface httpClient;
  private final Context ctx;
  private final Handler<AsyncResult<javax.ws.rs.core.Response>> asyncResultHandler;
  private final Map<String, String> okapiHeaders;

  public PutOrdersByIdHelper(HttpClientInterface httpClient, Map<String, String> okapiHeaders,
                             Handler<AsyncResult<Response>> asyncResultHandler, Context ctx) {
    Map<String, String> customHeader = new HashMap<>();
    customHeader.put(HttpHeaders.ACCEPT.toString(), "application/json, text/plain");
    httpClient.setDefaultHeaders(customHeader);

    this.httpClient = httpClient;
    this.okapiHeaders = okapiHeaders;
    this.ctx = ctx;
    this.asyncResultHandler = asyncResultHandler;
  }

  /*
   * Handle update order by doing Delete and Post
   */
  public CompletableFuture<Void> updateOrder(String id, String lang, CompositePurchaseOrder compPO,
                                             Context vertxContext) {
    CompletableFuture<Void> future = new VertxCompletableFuture<>(ctx);
    GetOrdersByIdHelper getHelper = new GetOrdersByIdHelper(httpClient, okapiHeaders,
      asyncResultHandler, vertxContext);
    PostOrdersHelper postHelper = new PostOrdersHelper(httpClient, okapiHeaders, asyncResultHandler, vertxContext);
    DeleteOrdersByIdHelper delHelper = new DeleteOrdersByIdHelper(httpClient, okapiHeaders, asyncResultHandler, vertxContext);
    getHelper.getOrder(id, lang).thenAccept(existedCompPO ->
      delHelper.deleteOrder(id, lang)
        .thenRun(() -> {
          compPO.setId(id);
          postHelper.createPOandPOLines(compPO, existedCompPO.getCreated())
            .thenAccept(withCompPO -> {

              logger.info("Applying Funds...");
              postHelper.applyFunds(withCompPO)
                .thenAccept(withFunds -> {

                  logger.info("Updating Inventory...");
                  postHelper.updateInventory(withFunds)
                    .thenAccept(withInventory -> {

                      logger.info("Successfully Placed Order: " + JsonObject.mapFrom(withCompPO).encodePrettily());
                      httpClient.closeClient();
                      javax.ws.rs.core.Response response = PutOrdersByIdResponse.respond204();
                      AsyncResult<javax.ws.rs.core.Response> result = Future.succeededFuture(response);
                      asyncResultHandler.handle(result);
                    })
                    .exceptionally(this::handleError);
                })
                .exceptionally(this::handleError);
            })
            .exceptionally(this::handleError);
        })
        .exceptionally(this::handleError)
    ).exceptionally(this::handleError);
    return future;
  }

  public Void handleError(Throwable throwable) {
    final Future<javax.ws.rs.core.Response> result;

    logger.error("Exception while updating orders", throwable.getCause());

    final Throwable t = throwable.getCause();
    if (t instanceof HttpException) {
      final int code = ((HttpException) t).getCode();
      final String message = ((HttpException) t).getMessage();
      switch (code) {
        case 404:
          result = Future.succeededFuture(PutOrdersByIdResponse.respond404WithTextPlain(message));
          break;
        case 500:
          result = Future.succeededFuture(PutOrdersByIdResponse.respond500WithTextPlain(message));
          break;
        default:
          result = Future.succeededFuture(PutOrdersByIdResponse.respond500WithTextPlain(message));
      }
    } else {
      result = Future.succeededFuture(PutOrdersByIdResponse.respond500WithTextPlain(throwable.getMessage()));
    }

    httpClient.closeClient();

    asyncResultHandler.handle(result);

    return null;
  }
}
