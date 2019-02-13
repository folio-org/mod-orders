package org.folio.rest.impl;

import static io.vertx.core.Future.succeededFuture;
import static org.folio.orders.utils.HelperUtils.COMPOSITE_PO_LINES;
import static org.folio.orders.utils.HelperUtils.OKAPI_URL;
import static org.folio.orders.utils.ResourcePathResolver.ADJUSTMENT;
import static org.folio.rest.RestVerticle.OKAPI_HEADER_TENANT;

import io.vertx.core.AsyncResult;
import io.vertx.core.Context;
import io.vertx.core.Future;
import io.vertx.core.Handler;
import io.vertx.core.http.HttpHeaders;
import io.vertx.core.json.JsonObject;
import io.vertx.core.logging.Logger;
import io.vertx.core.logging.LoggerFactory;

import java.util.HashMap;
import java.util.Map;
import javax.ws.rs.core.Response;

import org.folio.orders.rest.exceptions.HttpException;
import org.folio.orders.rest.exceptions.ValidationException;
import org.folio.rest.jaxrs.model.CompositePurchaseOrder;
import org.folio.rest.jaxrs.model.Error;
import org.folio.rest.jaxrs.model.Errors;
import org.folio.rest.tools.client.HttpClientFactory;
import org.folio.rest.tools.client.interfaces.HttpClientInterface;
import org.folio.rest.tools.utils.TenantTool;

public abstract class AbstractHelper {
  public static final String PO_LINE_NUMBER = "po_line_number";
  public static final String ID = "id";
  public static final String PO_NUMBER = "po_number";

  protected final Logger logger = LoggerFactory.getLogger(this.getClass());

  protected final HttpClientInterface httpClient;
  protected final Handler<AsyncResult<Response>> asyncResultHandler;
  protected final Map<String, String> okapiHeaders;
  protected final Context ctx;
  protected final String lang;

  AbstractHelper(HttpClientInterface httpClient, Map<String, String> okapiHeaders,
                      Handler<AsyncResult<Response>> asyncResultHandler, Context ctx, String lang) {
    this.httpClient = httpClient;
    this.asyncResultHandler = asyncResultHandler;
    this.okapiHeaders = okapiHeaders;
    this.ctx = ctx;
    this.lang = lang;
  }

  public static HttpClientInterface getHttpClient(Map<String, String> okapiHeaders) {
    final String okapiURL = okapiHeaders.getOrDefault(OKAPI_URL, "");
    final String tenantId = TenantTool.calculateTenantId(okapiHeaders.get(OKAPI_HEADER_TENANT));

    return HttpClientFactory.getHttpClient(okapiURL, tenantId);
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

  /**
   * Convert CompositePurchaseOrder to Json representation of PurchaseOrder.
   * These objects are the same except PurchaseOrder doesn't contains adjustment and poLines fields.
   * @param compPO
   * @return JsonObject representation of PurchaseOrder
   */
  protected JsonObject convertToPurchaseOrder(CompositePurchaseOrder compPO) {
    JsonObject purchaseOrder = JsonObject.mapFrom(compPO);
    if (purchaseOrder.containsKey(ADJUSTMENT)) {
      purchaseOrder.remove(ADJUSTMENT);
    }
    if (purchaseOrder.containsKey(COMPOSITE_PO_LINES)) {
      purchaseOrder.remove(COMPOSITE_PO_LINES);
    }
    return purchaseOrder;
  }

  protected Void handleError(Throwable throwable) {
    final Future<Response> result;

    logger.error("Exception encountered", throwable.getCause());
    final Throwable t = throwable.getCause();
    final String message = t.getMessage();
    if (t instanceof HttpException) {
      final int httpCode = ((HttpException) t).getCode();
      final String errorCode = ((HttpException) t).getErrorCode();
      result = succeededFuture(buildErrorResponse(httpCode, new Error().withMessage(message).withCode(errorCode)));
    } else if (t instanceof ValidationException) {
      result = succeededFuture(buildErrorResponse(422, ((ValidationException) t).getError()));
    } else {
      result = succeededFuture(buildErrorResponse(-1, new Error().withMessage(message)));
    }

    httpClient.closeClient();
    asyncResultHandler.handle(result);

    return null;
  }

  protected Errors withErrors(Error error) {
    Errors errors = new Errors();
    errors.getErrors().add(error);
    return errors;
  }

  abstract Response buildErrorResponse(int code, Error error);
}
