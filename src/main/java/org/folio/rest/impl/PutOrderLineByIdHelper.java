package org.folio.rest.impl;

import io.vertx.core.AsyncResult;
import io.vertx.core.Context;
import io.vertx.core.Handler;
import org.folio.rest.jaxrs.model.Error;
import org.folio.rest.jaxrs.model.Errors;
import org.folio.rest.jaxrs.model.PoLine;
import org.folio.rest.jaxrs.resource.Orders.PutOrdersByIdResponse;

import javax.ws.rs.core.Response;
import java.util.Map;

import static io.vertx.core.Future.succeededFuture;
import static org.folio.orders.utils.HelperUtils.deletePoLine;
import static org.folio.rest.jaxrs.resource.Orders.PutOrdersLinesByIdAndLineIdResponse.respond204;

public class PutOrderLineByIdHelper extends AbstractOrderLineHelper {

  public PutOrderLineByIdHelper(Map<String, String> okapiHeaders, Handler<AsyncResult<Response>> asyncResultHandler, Context ctx) {
    super(OrdersImpl.getHttpClient(okapiHeaders), okapiHeaders, asyncResultHandler, ctx);
    setDefaultHeaders(httpClient);
  }

  /*
   * Handle update order line by doing Delete and Post
   */
  public void updateOrder(String orderId, String lang, PoLine compOrderLine) {
    getPoLineByIdAndValidate(orderId, compOrderLine.getId(), lang)
      .thenCompose(lineFromStorage -> {
        org.folio.rest.acq.model.PoLine existedPoLine = lineFromStorage.mapTo(org.folio.rest.acq.model.PoLine.class);
        compOrderLine.setCreatedBy(existedPoLine.getCreatedBy());
        compOrderLine.setCreated(existedPoLine.getCreated());
        logger.debug("Deleting PO line...");
        return deletePoLine(lineFromStorage, httpClient, ctx, okapiHeaders, logger);
      })
      .thenCompose(emptyJson -> {
        logger.debug("Recreating PO line...");
        return new PostOrderLineHelper(httpClient, okapiHeaders, asyncResultHandler, ctx)
          .createPoLine(compOrderLine);
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
        result = PutOrdersByIdResponse.respond404WithTextPlain(message);
        break;
      case 422:
        Errors errors = new Errors();
        errors.getErrors()
              .add(new Error().withMessage(message));
        result = PutOrdersByIdResponse.respond422WithApplicationJson(errors);
        break;
      default:
        result = PutOrdersByIdResponse.respond500WithTextPlain(message);
    }
    return result;
  }
}
