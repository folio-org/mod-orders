package org.folio.rest.impl;

import io.vertx.core.AsyncResult;
import io.vertx.core.Context;
import io.vertx.core.Handler;
import org.folio.rest.jaxrs.model.Error;
import org.folio.rest.jaxrs.model.Errors;

import javax.ws.rs.core.Response;
import java.util.Map;

import static io.vertx.core.Future.succeededFuture;
import static org.folio.orders.utils.HelperUtils.deletePoLine;
import static org.folio.rest.jaxrs.resource.Orders.DeleteOrdersLinesByIdAndLineIdResponse.respond204;
import static org.folio.rest.jaxrs.resource.Orders.DeleteOrdersLinesByIdAndLineIdResponse.respond404WithTextPlain;
import static org.folio.rest.jaxrs.resource.Orders.DeleteOrdersLinesByIdAndLineIdResponse.respond422WithApplicationJson;
import static org.folio.rest.jaxrs.resource.Orders.DeleteOrdersLinesByIdAndLineIdResponse.respond500WithTextPlain;

class DeleteOrderLineByIdHelper extends AbstractOrderLineHelper {

  DeleteOrderLineByIdHelper(Map<String, String> okapiHeaders, Handler<AsyncResult<Response>> asyncResultHandler, Context ctx) {
    super(OrdersImpl.getHttpClient(okapiHeaders), okapiHeaders, asyncResultHandler, ctx);
    setDefaultHeaders(httpClient);
  }

  void deleteLine(String orderId, String lineId, String lang) {
    getPoLineByIdAndValidate(orderId, lineId, lang)
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

  protected Response buildErrorResponse(int code, String message) {
    final Response result;
    switch (code) {
      case 404:
        result = respond404WithTextPlain(message);
        break;
      case 422:
        Errors errors = new Errors();
        errors.getErrors()
              .add(new Error().withMessage(message));
        result = respond422WithApplicationJson(errors);
        break;
      default:
        result = respond500WithTextPlain(message);
    }
    return result;
  }
}
