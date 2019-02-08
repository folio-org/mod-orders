package org.folio.rest.impl;

import java.util.Map;
import java.util.concurrent.CompletableFuture;

import javax.ws.rs.core.Response;

import org.folio.orders.utils.HelperUtils;
import org.folio.rest.jaxrs.model.CompositePurchaseOrder;
import org.folio.rest.jaxrs.model.Error;
import org.folio.rest.jaxrs.resource.Orders.GetOrdersCompositeOrdersByIdResponse;
import org.folio.rest.tools.client.interfaces.HttpClientInterface;

import io.vertx.core.AsyncResult;
import io.vertx.core.Context;
import io.vertx.core.Handler;
import io.vertx.core.json.JsonObject;
import me.escoffier.vertx.completablefuture.VertxCompletableFuture;

import static org.folio.orders.utils.ResourcePathResolver.ADJUSTMENT;

public class GetOrdersByIdHelper extends AbstractHelper {

  public GetOrdersByIdHelper(HttpClientInterface httpClient, Map<String, String> okapiHeaders,
                             Handler<AsyncResult<Response>> asyncResultHandler, Context ctx, String lang) {
    super(httpClient, okapiHeaders, asyncResultHandler, ctx, lang);
  }

  public CompletableFuture<CompositePurchaseOrder> getOrder(String id) {
    CompletableFuture<CompositePurchaseOrder> future = new VertxCompletableFuture<>(ctx);

    getCompositePurchaseOrderById(id, lang)
      .thenAccept(orders -> {
        logger.info("Returning Composite Purchase Order: " + JsonObject.mapFrom(orders).encodePrettily());
        future.complete(orders);
      })
      .exceptionally(t -> {
        logger.error("Error getting orders", t);
        future.completeExceptionally(t);
        return null;
      });

    return future;
  }

  private CompletableFuture<CompositePurchaseOrder> getCompositePurchaseOrderById(String id, String lang) {
    CompletableFuture<CompositePurchaseOrder> future = new VertxCompletableFuture<>(ctx);

    HelperUtils.getPurchaseOrderById(id, lang, httpClient, ctx, okapiHeaders, logger)
      .thenAccept(po -> {
        logger.info("got: " + po.encodePrettily());
        po.remove(ADJUSTMENT);
        CompositePurchaseOrder compPO = po.mapTo(CompositePurchaseOrder.class);

        HelperUtils.getCompositePoLines(id, lang, httpClient, ctx, okapiHeaders, logger)
          .thenAccept(poLines -> {
            compPO.setCompositePoLines(poLines);
            compPO.setAdjustment(HelperUtils.calculateAdjustment(poLines));
            future.complete(compPO);
          })
          .exceptionally(t -> {
            logger.error("Failed to get POLines", t);
            future.completeExceptionally(t);
            return null;
          });
      })
      .exceptionally(t -> {
        logger.error("Failed to build composite purchase order", t.getCause());
        future.completeExceptionally(t);
        return null;
      });

    return future;
  }

  @Override
  Response buildErrorResponse(int code, Error error) {
    if (code == 404) {
       return GetOrdersCompositeOrdersByIdResponse.respond404WithApplicationJson(withErrors(error));
    }
    return GetOrdersCompositeOrdersByIdResponse.respond500WithApplicationJson(withErrors(error));
  }

}
