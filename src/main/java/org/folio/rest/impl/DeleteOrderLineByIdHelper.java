package org.folio.rest.impl;

import io.vertx.core.AsyncResult;
import io.vertx.core.Context;
import io.vertx.core.Future;
import io.vertx.core.Handler;
import io.vertx.core.http.HttpHeaders;
import io.vertx.core.json.JsonObject;
import io.vertx.core.logging.Logger;
import io.vertx.core.logging.LoggerFactory;
import org.apache.commons.lang3.StringUtils;
import org.folio.orders.rest.exceptions.HttpException;
import org.folio.rest.jaxrs.model.Error;
import org.folio.rest.jaxrs.model.Errors;
import org.folio.rest.tools.client.interfaces.HttpClientInterface;

import javax.ws.rs.core.Response;
import java.util.HashMap;
import java.util.Map;
import java.util.concurrent.CompletionException;

import static io.vertx.core.Future.succeededFuture;
import static org.folio.orders.utils.HelperUtils.deletePoLine;
import static org.folio.orders.utils.HelperUtils.getPoLineById;
import static org.folio.rest.jaxrs.resource.Orders.DeleteOrdersLinesByIdAndLineIdResponse.respond204;
import static org.folio.rest.jaxrs.resource.Orders.DeleteOrdersLinesByIdAndLineIdResponse.respond404WithTextPlain;
import static org.folio.rest.jaxrs.resource.Orders.DeleteOrdersLinesByIdAndLineIdResponse.respond422WithApplicationJson;
import static org.folio.rest.jaxrs.resource.Orders.DeleteOrdersLinesByIdAndLineIdResponse.respond500WithTextPlain;

class DeleteOrderLineByIdHelper {
  private static final Logger logger = LoggerFactory.getLogger(DeleteOrderLineByIdHelper.class);

  private final HttpClientInterface httpClient;
  private final Context ctx;
  private final Handler<AsyncResult<Response>> asyncResultHandler;
  private final Map<String, String> okapiHeaders;

  DeleteOrderLineByIdHelper(HttpClientInterface httpClient, Map<String, String> okapiHeaders,
                            Handler<AsyncResult<Response>> asyncResultHandler, Context ctx) {
    Map<String,String> customHeader=new HashMap<>();
    customHeader.put(HttpHeaders.ACCEPT.toString(), "application/json, text/plain");
    httpClient.setDefaultHeaders(customHeader);

    this.httpClient = httpClient;
    this.okapiHeaders = okapiHeaders;
    this.ctx = ctx;
    this.asyncResultHandler = asyncResultHandler;
  }

  void deleteLine(String orderId, String lineId, String lang) {
    getPoLineById(lineId, lang, httpClient, ctx, okapiHeaders, logger)
      .thenApply(line -> {
        logger.debug("Validating if the retrieved PO line corresponds to PO");
        return validateOrderId(orderId, line);
      })
      .thenCompose(line -> {
        logger.debug("Validation checks passed. Deleting PO line.");
        return deletePoLine(line, httpClient, ctx, okapiHeaders, logger);
      })
      .thenAccept(v -> asyncResultHandler.handle(succeededFuture(respond204())))
      .exceptionally(this::handleError);
  }

  /**
   * Validates if the retrieved PO line corresponds to PO (orderId). In case the PO line does not correspond to order id the exception is thrown
   * @param orderId order identifier
   * @param line PO line retrieved from storage
   * @return PO line json object if validation is passed
   */
  private JsonObject validateOrderId(String orderId, JsonObject line) {
    if (!StringUtils.equals(orderId, line.getString("purchase_order_id"))) {
      String msg = String.format("The PO line with id=%s does not belong to order with id=%s", line.getString("id"), orderId);
      throw new CompletionException(new HttpException(422, msg));
    }
    return line;
  }

  private Void handleError(Throwable throwable) {
    final Future<Response> result;

    logger.error("Exception while deleting PO line", throwable.getCause());

    final Throwable t = throwable.getCause();
    if (t instanceof HttpException) {
      final int code = ((HttpException) t).getCode();
      final String message = t.getMessage();
      switch (code) {
      case 404:
        result = succeededFuture(respond404WithTextPlain(message));
        break;
      case 422:
        Errors errors = new Errors();
        errors.getErrors().add(new Error().withMessage(message));
        result = succeededFuture(respond422WithApplicationJson(errors));
        break;
      case 500:
        result = succeededFuture(respond500WithTextPlain(message));
        break;
      default:
        result = succeededFuture(respond500WithTextPlain(message));
      }
    } else {
      result = succeededFuture(respond500WithTextPlain(throwable.getMessage()));
    }

    httpClient.closeClient();

    asyncResultHandler.handle(result);

    return null;
  }
}
