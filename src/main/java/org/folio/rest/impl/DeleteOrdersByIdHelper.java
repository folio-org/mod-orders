package org.folio.rest.impl;

import static org.folio.orders.utils.ResourcePathResolver.PURCHASE_ORDER;
import static org.folio.orders.utils.ResourcePathResolver.resourceByIdPath;

import java.util.Map;
import java.util.concurrent.CompletableFuture;

import javax.ws.rs.core.Response;

import org.folio.orders.utils.HelperUtils;
import org.folio.rest.jaxrs.model.Error;
import static org.folio.rest.jaxrs.resource.Orders.DeleteOrdersCompositeOrdersByIdResponse.respond404WithApplicationJson;
import static org.folio.rest.jaxrs.resource.Orders.DeleteOrdersCompositeOrdersByIdResponse.respond500WithApplicationJson;

import io.vertx.core.AsyncResult;
import io.vertx.core.Context;
import io.vertx.core.Handler;
import io.vertx.core.http.HttpMethod;
import me.escoffier.vertx.completablefuture.VertxCompletableFuture;

public class DeleteOrdersByIdHelper extends AbstractHelper {

  public DeleteOrdersByIdHelper(Map<String, String> okapiHeaders,
      Handler<AsyncResult<Response>> asyncResultHandler, Context ctx, String lang) {
    super(getHttpClient(okapiHeaders), okapiHeaders, asyncResultHandler, ctx, lang);
    setDefaultHeaders(httpClient);
  }

  public CompletableFuture<Void> deleteOrder(String id) {
    CompletableFuture<Void> future = new VertxCompletableFuture<>(ctx);

    HelperUtils.deletePoLines(id, lang, httpClient, ctx, okapiHeaders, logger).thenRun(() -> {
      logger.info("Successfully deleted po_lines, proceding with purchase order");
      HelperUtils.operateOnSubObj(HttpMethod.DELETE, resourceByIdPath(PURCHASE_ORDER, id), httpClient, ctx, okapiHeaders, logger)
          .thenAccept(action -> {
            httpClient.closeClient();
            future.complete(null);
          }).exceptionally(t -> {
            logger.error("Failed to delete PO", t);
            future.completeExceptionally(t);
            return null;
          });
    }).exceptionally(t -> {
      logger.error("Failed to delete PO Lines", t);
      future.completeExceptionally(t);
      return null;
    });

    return future;
  }


  @Override
  Response buildErrorResponse(int code, Error error) {
    if (code == 404) {
      return respond404WithApplicationJson(withErrors(error));
    }
    return respond500WithApplicationJson(withErrors(error));
  }
}
