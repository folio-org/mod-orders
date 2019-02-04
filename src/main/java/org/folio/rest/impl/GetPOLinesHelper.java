package org.folio.rest.impl;


import io.vertx.core.AsyncResult;
import io.vertx.core.Context;
import io.vertx.core.Handler;
import me.escoffier.vertx.completablefuture.VertxCompletableFuture;
import org.folio.orders.utils.HelperUtils;
import org.folio.rest.jaxrs.model.Error;
import org.folio.rest.jaxrs.model.PoLineCollection;
import org.folio.rest.jaxrs.resource.Orders.GetOrdersOrderLinesResponse;
import org.folio.rest.tools.client.interfaces.HttpClientInterface;

import javax.ws.rs.core.Response;
import java.util.Map;
import java.util.concurrent.CompletableFuture;

import static org.folio.orders.utils.ResourcePathResolver.PO_LINES;
import static org.folio.orders.utils.ResourcePathResolver.resourcesPath;

import static org.apache.commons.lang3.StringUtils.EMPTY;
import static org.apache.commons.lang3.StringUtils.isEmpty;
import static org.folio.orders.utils.HelperUtils.encodeQuery;

public class GetPOLinesHelper extends AbstractHelper {
  public static final String GET_PO_LINES_BY_QUERY = resourcesPath(PO_LINES) + "?limit=%s&offset=%s%s&lang=%s";

  GetPOLinesHelper(HttpClientInterface httpClient, Map<String, String> okapiHeaders,
                   Handler<AsyncResult<Response>> asyncResultHandler, Context ctx, String lang) {
    super(httpClient, okapiHeaders, asyncResultHandler, ctx, lang);
  }


  public CompletableFuture<PoLineCollection> getPOLines(int limit, int offset, String query) {
    CompletableFuture<PoLineCollection> future = new VertxCompletableFuture<>(ctx);
    try {
      String queryParam = isEmpty(query) ? EMPTY : "&query=" + encodeQuery(query, logger);
      String endpoint = String.format(GET_PO_LINES_BY_QUERY, limit, offset, queryParam, lang);
      HelperUtils.handleGetRequest(endpoint, httpClient, ctx, okapiHeaders, logger)
        .thenAccept(jsonOrders -> future.complete(jsonOrders.mapTo(PoLineCollection.class)))
        .exceptionally(t -> {
          logger.error("Error getting PO lines", t);
          future.completeExceptionally(t.getCause());
          return null;
        });
    } catch (Exception e) {
      future.completeExceptionally(e);
    }

    return future;
  }


  @Override
  protected Response buildErrorResponse(int code, Error error) {
    final Response result;
    switch (code) {
      case 400:
        result = GetOrdersOrderLinesResponse.respond400WithApplicationJson(withErrors(error));
        break;
      case 401:
        result = GetOrdersOrderLinesResponse.respond401WithApplicationJson(withErrors(error));
        break;
      default:
        result = GetOrdersOrderLinesResponse.respond500WithApplicationJson(withErrors(error));
    }
    return result;
  }
}
