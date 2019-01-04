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
import org.folio.rest.jaxrs.model.CompositePurchaseOrder;
import org.folio.rest.jaxrs.model.Error;
import org.folio.rest.jaxrs.model.Errors;
import org.folio.rest.tools.client.interfaces.HttpClientInterface;

import javax.ws.rs.core.Response;
import java.util.HashMap;
import java.util.Map;
import java.util.concurrent.CompletableFuture;
import java.util.concurrent.CompletionException;

import static io.vertx.core.Future.succeededFuture;
import static org.folio.orders.utils.HelperUtils.getPoLineById;
import static org.folio.orders.utils.SubObjects.ADJUSTMENT;
import static org.folio.orders.utils.SubObjects.PO_LINES;

public abstract class AbstractHelper {
  protected final Logger logger = LoggerFactory.getLogger(this.getClass());

  protected final HttpClientInterface httpClient;
  protected final Handler<AsyncResult<Response>> asyncResultHandler;
  protected final Map<String, String> okapiHeaders;
  protected final Context ctx;

  AbstractHelper(HttpClientInterface httpClient, Map<String, String> okapiHeaders,
                      Handler<AsyncResult<Response>> asyncResultHandler, Context ctx) {
    this.httpClient = httpClient;
    this.asyncResultHandler = asyncResultHandler;
    this.okapiHeaders = okapiHeaders;
    this.ctx = ctx;
  }

  /**
   * Retrieves PO line from storage by PO line id as JsonObject and validates order id match.
   */
  protected CompletableFuture<JsonObject> getPoLineByIdAndValidate(String orderId, String lineId, String lang) {
    return getPoLineById(lineId, lang, httpClient, ctx, okapiHeaders, logger)
      .thenApply(line -> {
        logger.debug("Validating if the retrieved PO line corresponds to PO");
        validateOrderId(orderId, line);
        return line;
      });
  }

  /**
   * Validates if the retrieved PO line corresponds to PO (orderId). In case the PO line does not correspond to order id the exception is thrown
   * @param orderId order identifier
   * @param line PO line retrieved from storage
   */
  private void validateOrderId(String orderId, JsonObject line) {
    if (!StringUtils.equals(orderId, line.getString("purchase_order_id"))) {
      String msg = String.format("The PO line with id=%s does not belong to order with id=%s", line.getString("id"), orderId);
      throw new CompletionException(new HttpException(422, msg));
    }
  }

  /**
   * Some requests do not have body and in happy flow do not produce response body. The Accept header is required for calls to storage
   * @param httpClient
   */
  protected void setDefaultHeaders(HttpClientInterface httpClient) {
    Map<String,String> customHeader = new HashMap<>();
    customHeader.put(HttpHeaders.ACCEPT.toString(), "application/json, text/plain");
    httpClient.setDefaultHeaders(customHeader);
  }

  protected JsonObject getJsonOrderWithoutUnusedSubObjects(CompositePurchaseOrder compPO) {
    JsonObject purchaseOrder = JsonObject.mapFrom(compPO);
    if (purchaseOrder.containsKey(ADJUSTMENT)) {
      purchaseOrder.remove(ADJUSTMENT);
    }
    if (purchaseOrder.containsKey(PO_LINES)) {
      purchaseOrder.remove(PO_LINES);
    }
    return purchaseOrder;
  }


  protected Void handleError(Throwable throwable) {
    final Future<Response> result;

    logger.error("Exception while operation on PO line", throwable.getCause());

    final Throwable t = throwable.getCause();
    final String message = t.getMessage();
    if (t instanceof HttpException) {
      final int code = ((HttpException) t).getCode();
      result = succeededFuture(buildErrorResponse(code, message));
    } else {
      result = succeededFuture(buildErrorResponse(-1, message));
    }

    httpClient.closeClient();
    asyncResultHandler.handle(result);

    return null;
  }

  protected Errors withErrors(String message) {
    Errors errors = new Errors();
    errors.getErrors()
      .add(new Error().withMessage(message).withCode("-1"));
    return errors;
  }

  protected Errors withErrors(String message, String errorCode) {
    Errors errors = new Errors();
    errors.getErrors()
      .add(new Error().withMessage(message).withCode(errorCode));
    return errors;
  }

  abstract Response buildErrorResponse(int code, String message);
}
