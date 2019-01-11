package org.folio.rest.impl;

import static io.vertx.core.Future.succeededFuture;
import static org.folio.orders.utils.HelperUtils.deletePoLine;
import static org.folio.rest.jaxrs.resource.Orders.DeleteOrdersLinesByIdAndLineIdResponse.respond204;
import static org.folio.rest.jaxrs.resource.Orders.DeleteOrdersLinesByIdAndLineIdResponse.respond404WithTextPlain;
import static org.folio.rest.jaxrs.resource.Orders.DeleteOrdersLinesByIdAndLineIdResponse.respond422WithApplicationJson;
import static org.folio.rest.jaxrs.resource.Orders.DeleteOrdersLinesByIdAndLineIdResponse.respond500WithTextPlain;

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

  void deleteLine(String orderId, String lineId) {
    getPoLineByIdAndValidate(orderId, lineId)
      .thenCompose(line -> {
        logger.debug("Deleting PO line...");
        return deletePoLine(line, httpClient, ctx, okapiHeaders, logger);
      })
      .thenAccept(v -> {
        httpClient.closeClient();
        asyncResultHandler.handle(succeededFuture(respond204()));
      })
      .exceptionally(this::handleError);
  }

  @Override
  protected Response buildErrorResponse(int code, Error error) {
    final Response result;
    switch (code) {
      case 404:
        result = respond404WithTextPlain(error.getMessage());
        break;
      case 422:
        result = respond422WithApplicationJson(withErrors(error));
        break;
      default:
        result = respond500WithTextPlain(error.getMessage());
    }
    return result;
  }
}
