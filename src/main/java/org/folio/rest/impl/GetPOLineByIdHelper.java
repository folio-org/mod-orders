package org.folio.rest.impl;

import io.vertx.core.AsyncResult;
import io.vertx.core.Context;
import io.vertx.core.Handler;
import io.vertx.core.http.HttpMethod;
import io.vertx.core.json.JsonObject;
import me.escoffier.vertx.completablefuture.VertxCompletableFuture;
import org.folio.orders.utils.HelperUtils;
import org.folio.rest.jaxrs.model.Error;
import org.folio.rest.jaxrs.model.PoLine;
import org.folio.rest.jaxrs.resource.Orders.GetOrdersLinesByIdAndLineIdResponse;
import org.folio.rest.tools.client.interfaces.HttpClientInterface;

import javax.ws.rs.core.Response;
import java.util.Map;
import java.util.concurrent.CompletableFuture;
import java.util.concurrent.CompletionStage;

public class GetPOLineByIdHelper extends AbstractHelper {

  GetPOLineByIdHelper(HttpClientInterface httpClient, Map<String, String> okapiHeaders,
                      Handler<AsyncResult<Response>> asyncResultHandler, Context ctx) {
    super(httpClient, okapiHeaders, asyncResultHandler, ctx);
  }

  public CompletableFuture<PoLine> getPOLineByPOLineId(String orderId, String polineId, String lang) {
    CompletableFuture<PoLine> future = new VertxCompletableFuture<>(ctx);

    getPoLineByIdAndValidate(orderId, polineId, lang)
      .thenCompose(this::populateCompositeLine)
      .thenAccept(future::complete)
      .exceptionally(t -> {
        logger.error("Failed to get composite purchase order line", t.getCause());
        future.completeExceptionally(t);
        return null;
      });

    return future;
  }

  private CompletionStage<PoLine> populateCompositeLine(JsonObject poline) {
    return HelperUtils.operateOnPoLine(HttpMethod.GET, poline, httpClient, ctx, okapiHeaders, logger);
  }

  @Override
  protected Response buildErrorResponse(int code, Error error) {
    final Response result;
    switch (code) {
      case 404:
        result = GetOrdersLinesByIdAndLineIdResponse.respond404WithTextPlain(error.getMessage());
        break;
      case 422:
        result = GetOrdersLinesByIdAndLineIdResponse.respond422WithApplicationJson(withErrors(error));
        break;
      default:
        result = GetOrdersLinesByIdAndLineIdResponse.respond500WithTextPlain(error.getMessage());
    }
    return result;
  }
}
