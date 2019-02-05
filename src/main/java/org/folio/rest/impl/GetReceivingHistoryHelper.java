package org.folio.rest.impl;

import static org.folio.orders.utils.HelperUtils.getEndpointWithQuery;
import static org.folio.orders.utils.ResourcePathResolver.RECEIVING_HISTORY;
import static org.folio.orders.utils.ResourcePathResolver.resourcesPath;
import static org.folio.rest.jaxrs.resource.Orders.GetOrdersReceivingHistoryResponse.respond400WithTextPlain;
import static org.folio.rest.jaxrs.resource.Orders.GetOrdersReceivingHistoryResponse.respond404WithTextPlain;
import static org.folio.rest.jaxrs.resource.Orders.GetOrdersReceivingHistoryResponse.respond500WithTextPlain;

import io.vertx.core.AsyncResult;
import io.vertx.core.Context;
import io.vertx.core.Handler;
import java.util.Map;
import java.util.concurrent.CompletableFuture;
import javax.ws.rs.core.Response;
import me.escoffier.vertx.completablefuture.VertxCompletableFuture;
import org.folio.orders.utils.HelperUtils;
import org.folio.rest.jaxrs.model.Error;
import org.folio.rest.jaxrs.model.ReceivingHistoryCollection;
import org.folio.rest.tools.client.interfaces.HttpClientInterface;

public class GetReceivingHistoryHelper extends AbstractHelper{

  public static final String GET_RECEIVING_HISTORY_BY_QUERY = resourcesPath(RECEIVING_HISTORY) + "?limit=%s&offset=%s%s&lang=%s";

  GetReceivingHistoryHelper(HttpClientInterface httpClient, Map<String, String> okapiHeaders,
      Handler<AsyncResult<Response>> asyncResultHandler, Context ctx, String lang) {
    super(httpClient, okapiHeaders, asyncResultHandler, ctx, lang);
  }


  public CompletableFuture<ReceivingHistoryCollection> getReceivingHistory(int limit, int offset, String query) {
    CompletableFuture<ReceivingHistoryCollection> future = new VertxCompletableFuture<>(ctx);

    try {
      String queryParam = getEndpointWithQuery(query, logger);
      String endpoint = String.format(GET_RECEIVING_HISTORY_BY_QUERY, limit, offset, queryParam, lang);
      HelperUtils.handleGetRequest(endpoint, httpClient, ctx, okapiHeaders, logger)
        .thenAccept(jsonReceivingHistory -> future.complete(jsonReceivingHistory.mapTo(ReceivingHistoryCollection.class)))
        .exceptionally(t -> {
          logger.error("Error retrtieving receiving history", t);
          future.completeExceptionally(t.getCause());
          return null;
        });
    } catch (Exception e) {
      future.completeExceptionally(e);
    }

    return future;
  }

  @Override
  Response buildErrorResponse(int code, Error error) {
    final Response result;
    switch (code) {
      case 400:
        result = respond400WithTextPlain(error.getMessage());
        break;
      case 404:
        result = respond404WithTextPlain(withErrors(error));
        break;
      default:
        result = respond500WithTextPlain(error.getMessage());
    }
    return result;
  }

}
