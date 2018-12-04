package org.folio.rest.impl;

import io.vertx.core.AsyncResult;
import io.vertx.core.Context;
import io.vertx.core.Future;
import io.vertx.core.Handler;
import io.vertx.core.json.JsonObject;
import io.vertx.core.logging.Logger;
import io.vertx.core.logging.LoggerFactory;
import me.escoffier.vertx.completablefuture.VertxCompletableFuture;
import org.folio.orders.rest.exceptions.HttpException;
import org.folio.orders.utils.HelperUtils;
import org.folio.rest.jaxrs.model.CompositePoLine;
import org.folio.rest.jaxrs.resource.Orders;
import org.folio.rest.tools.client.interfaces.HttpClientInterface;

import javax.ws.rs.core.Response;
import java.util.Map;
import java.util.concurrent.CompletableFuture;

public class GetPOLineByIdHelper {

  private static final Logger logger = LoggerFactory.getLogger(GetOrdersByIdHelper.class);
  
  public static final String MOCK_DATA_PATH = "mockdata/poline-c0d08448-347b-418a-8c2f-5fb50248d67e.json";

  private final HttpClientInterface httpClient;
  private final Context ctx;
  private final Handler<AsyncResult<Response>> asyncResultHandler;
  private final Map<String, String> okapiHeaders;

  public GetPOLineByIdHelper(HttpClientInterface httpClient, Map<String, String> okapiHeaders,
                             Handler<AsyncResult<Response>> asyncResultHandler, Context ctx) {
    this.httpClient = httpClient;
    this.okapiHeaders = okapiHeaders;
    this.ctx = ctx;
    this.asyncResultHandler = asyncResultHandler;
  }

  public CompletableFuture<CompositePoLine> getCompositePoLine(String id, String lang) {
    CompletableFuture<CompositePoLine> future = new VertxCompletableFuture<>(ctx);
    getCompositePolineByPolineId(id, lang)
      .thenAccept(poline -> {
        logger.info("Returning Composite Purchase Order Line: " + JsonObject.mapFrom(poline).encodePrettily());
        future.complete(poline);
      })
      .exceptionally(t -> {
        logger.error("Error getting POLine", t);
        future.completeExceptionally(t.getCause());
        return null;
      });

    return future;
  }

  private CompletableFuture<CompositePoLine> getCompositePolineByPolineId(String polineId, String lang) {
    CompletableFuture<CompositePoLine> future = new VertxCompletableFuture<>(ctx);
    CompositePoLine comPOLine = new CompositePoLine();

    HelperUtils.getPoLineById(polineId, lang, httpClient, ctx, okapiHeaders, logger)
      .thenAccept(poline -> {
        future.complete(poline);
      })
      .exceptionally(t -> {
        logger.error("Failed to get composite purchase order line", t.getCause());
        future.completeExceptionally(t.getCause());
        return null;
      });

    return future;
  }

  public Void handleError(Throwable throwable) {
    final Future<Response> result;

    logger.error("Exception getting POLine", throwable.getCause());

    final Throwable t = throwable.getCause();
    if (t instanceof HttpException) {
      final int code = ((HttpException) t).getCode();
      final String message = t.getMessage();
      switch (code) {
        case 404:
          result = Future.succeededFuture(Orders.GetOrdersLinesByIdAndLineIdResponse.respond404WithTextPlain(message));
          break;
        case 500:
          result = Future.succeededFuture(Orders.GetOrdersLinesByIdAndLineIdResponse.respond500WithTextPlain(message));
          break;
        default:
          result = Future.succeededFuture(Orders.GetOrdersLinesByIdAndLineIdResponse.respond500WithTextPlain(message));
      }
    } else {
      result = Future.succeededFuture(Orders.GetOrdersLinesByIdAndLineIdResponse.respond500WithTextPlain(throwable.getMessage()));
    }

    httpClient.closeClient();

    asyncResultHandler.handle(result);

    return null;
  }

}
