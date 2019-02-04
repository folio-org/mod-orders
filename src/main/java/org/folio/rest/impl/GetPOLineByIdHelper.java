package org.folio.rest.impl;

import java.util.Map;
import java.util.concurrent.CompletableFuture;
import java.util.concurrent.CompletionStage;

import javax.ws.rs.core.Response;

import org.folio.orders.utils.HelperUtils;
import org.folio.rest.jaxrs.model.CompositePoLine;
import org.folio.rest.jaxrs.model.Error;
import org.folio.rest.jaxrs.resource.Orders.GetOrdersOrderLinesByIdResponse;
import org.folio.rest.tools.client.interfaces.HttpClientInterface;

import io.vertx.core.AsyncResult;
import io.vertx.core.Context;
import io.vertx.core.Handler;
import io.vertx.core.http.HttpMethod;
import io.vertx.core.json.JsonObject;
import me.escoffier.vertx.completablefuture.VertxCompletableFuture;

public class GetPOLineByIdHelper extends AbstractHelper {

  GetPOLineByIdHelper(HttpClientInterface httpClient, Map<String, String> okapiHeaders,
                      Handler<AsyncResult<Response>> asyncResultHandler, Context ctx, String lang) {
    super(httpClient, okapiHeaders, asyncResultHandler, ctx, lang);
  }

  public CompletableFuture<CompositePoLine> getPOLineByPOLineId(String polineId) {
    CompletableFuture<CompositePoLine> future = new VertxCompletableFuture<>(ctx);
    HelperUtils.getPoLineById(polineId, lang, httpClient,ctx, okapiHeaders, logger)
      .thenCompose(this::populateCompositeLine)
      .thenAccept(future::complete)
      .exceptionally(t -> {
        logger.error("Failed to get composite purchase order line", t.getCause());
        future.completeExceptionally(t);
        return null;
      });
    return future;
  }

  private CompletionStage<CompositePoLine> populateCompositeLine(JsonObject poline) {
    return HelperUtils.operateOnPoLine(HttpMethod.GET, poline, httpClient, ctx, okapiHeaders, logger);
  }

  @Override
  protected Response buildErrorResponse(int code, Error error) {
    final Response result;
    switch (code) {
      case 404:
        result = GetOrdersOrderLinesByIdResponse.respond404WithApplicationJson(withErrors(error));
        break;
      case 422:
        result = GetOrdersOrderLinesByIdResponse.respond422WithApplicationJson(withErrors(error));
        break;
      default:
        result = GetOrdersOrderLinesByIdResponse.respond500WithApplicationJson(withErrors(error));
    }
    return result;
  }
}
