package org.folio.rest.impl;

import io.vertx.core.AsyncResult;
import io.vertx.core.Context;
import io.vertx.core.Handler;
import me.escoffier.vertx.completablefuture.VertxCompletableFuture;
import org.folio.orders.utils.HelperUtils;
import org.folio.rest.jaxrs.model.Error;
import org.folio.rest.jaxrs.model.PurchaseOrders;
import org.folio.rest.jaxrs.resource.Orders.GetOrdersCompositeOrdersResponse;
import org.folio.rest.tools.client.interfaces.HttpClientInterface;

import javax.ws.rs.core.Response;
import java.util.Map;
import java.util.concurrent.CompletableFuture;

import static org.apache.commons.lang3.StringUtils.EMPTY;
import static org.apache.commons.lang3.StringUtils.isEmpty;
import static org.folio.orders.utils.HelperUtils.encodeQuery;
import static org.folio.orders.utils.SubObjects.PURCHASE_ORDER;
import static org.folio.orders.utils.SubObjects.resourcesPath;

public class GetOrdersHelper extends AbstractHelper {

  public static final String GET_PURCHASE_ORDERS_BY_QUERY = resourcesPath(PURCHASE_ORDER) + "?limit=%s&offset=%s%s&lang=%s";

  GetOrdersHelper(HttpClientInterface httpClient, Map<String, String> okapiHeaders,
                  Handler<AsyncResult<Response>> asyncResultHandler, Context ctx, String lang) {
    super(httpClient, okapiHeaders, asyncResultHandler, ctx, lang);
  }


  public CompletableFuture<PurchaseOrders> getPurchaseOrders(int limit, int offset, String query) {
    CompletableFuture<PurchaseOrders> future = new VertxCompletableFuture<>(ctx);

    try {
      String queryParam = isEmpty(query) ? EMPTY : "&query=" + encodeQuery(query, logger);
      String endpoint = String.format(GET_PURCHASE_ORDERS_BY_QUERY, limit, offset, queryParam, lang);
      HelperUtils.handleGetRequest(endpoint, httpClient, ctx, okapiHeaders, logger)
        .thenAccept(jsonOrders -> future.complete(jsonOrders.mapTo(PurchaseOrders.class)))
        .exceptionally(t -> {
          logger.error("Error getting orders", t);
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
        result = GetOrdersCompositeOrdersResponse.respond400WithTextPlain(error.getMessage());
        break;
      case 500:
        result = GetOrdersCompositeOrdersResponse.respond500WithTextPlain(error.getMessage());
        break;
      case 401:
        result = GetOrdersCompositeOrdersResponse.respond401WithTextPlain(error.getMessage());
        break;
      default:
        result = GetOrdersCompositeOrdersResponse.respond500WithTextPlain(error.getMessage());
    }
    return result;
  }
}
