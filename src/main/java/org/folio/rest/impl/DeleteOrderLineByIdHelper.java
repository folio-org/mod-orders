package org.folio.rest.impl;

import static io.vertx.core.Future.succeededFuture;
import static org.folio.orders.utils.HelperUtils.deletePoLine;
import org.folio.orders.utils.HelperUtils;
import org.folio.rest.jaxrs.resource.Orders.DeleteOrdersOrderLinesByIdResponse;

import java.util.Map;

import javax.ws.rs.core.Response;

import org.folio.rest.jaxrs.model.Error;

import io.vertx.core.AsyncResult;
import io.vertx.core.Context;
import io.vertx.core.Handler;

class DeleteOrderLineByIdHelper extends AbstractHelper {

  DeleteOrderLineByIdHelper(Map<String, String> okapiHeaders, Handler<AsyncResult<Response>> asyncResultHandler, Context ctx, String lang) {
    super(getHttpClient(okapiHeaders), okapiHeaders, asyncResultHandler, ctx, lang);
    setDefaultHeaders(httpClient);
  }

  void deleteLine(String lineId) {
    HelperUtils.getPoLineById(lineId, lang, httpClient,ctx, okapiHeaders, logger)
      .thenCompose(line -> {
        logger.debug("Deleting PO line...");
        return deletePoLine(line, httpClient, ctx, okapiHeaders, logger);
      })
      .thenAccept(v -> {
        httpClient.closeClient();
        asyncResultHandler.handle(succeededFuture(DeleteOrdersOrderLinesByIdResponse.respond204()));
      })
      .exceptionally(this::handleError);
  }

  @Override
  protected Response buildErrorResponse(int code, Error error) {
    final Response result;
    switch (code) {
      case 404:
        result = DeleteOrdersOrderLinesByIdResponse.respond404WithApplicationJson(withErrors(error));
        break;
      case 422:
        result = DeleteOrdersOrderLinesByIdResponse.respond422WithApplicationJson(withErrors(error));
        break;
      default:
        result = DeleteOrdersOrderLinesByIdResponse.respond500WithApplicationJson(withErrors(error));
    }
    return result;
  }
}
