package org.folio.rest.impl;

import io.vertx.core.AsyncResult;
import io.vertx.core.Context;
import io.vertx.core.Future;
import io.vertx.core.Handler;
import io.vertx.core.http.HttpMethod;
import io.vertx.core.json.JsonObject;
import io.vertx.core.logging.Logger;
import io.vertx.core.logging.LoggerFactory;
import me.escoffier.vertx.completablefuture.VertxCompletableFuture;
import org.folio.orders.rest.exceptions.HttpException;
import org.folio.orders.utils.HelperUtils;
import org.folio.rest.jaxrs.model.Error;
import org.folio.rest.jaxrs.model.Errors;
import org.folio.rest.jaxrs.model.PoLine;
import org.folio.rest.jaxrs.resource.Orders;
import org.folio.rest.tools.client.interfaces.HttpClientInterface;

import javax.ws.rs.core.Response;
import java.util.Map;
import java.util.concurrent.CompletableFuture;
import java.util.concurrent.CompletionException;

public class GetPOLineByIdHelper {

  private static final Logger logger = LoggerFactory.getLogger(GetOrdersByIdHelper.class);

  public static final String MOCK_DATA_PATH = "mockdata/lines/c0d08448-347b-418a-8c2f-5fb50248d67e.json";
  public static final String ORDER_REFERENCE_ERROR_MESSAGE = "Current order do not referenced to requested PO line";
  public static final String ERROR_CODE_422 = "422";

  private final HttpClientInterface httpClient;
  private final Context ctx;
  private final Handler<AsyncResult<Response>> asyncResultHandler;
  private final Map<String, String> okapiHeaders;

  public GetPOLineByIdHelper(HttpClientInterface httpClient, Map<String, String> okapiHeaders,
                             Handler<AsyncResult<Response>> asyncResultHandler, Context ctx) {
    this.httpClient = httpClient;
    this.okapiHeaders = okapiHeaders;
    this.ctx = ctx;
    this.asyncResultHandler = asyncResultHandler;
  }

  public CompletableFuture<PoLine> getPOLineByPOLineId(String orderId, String polineId, String lang) {
    CompletableFuture<PoLine> future = new VertxCompletableFuture<>(ctx);

    HelperUtils.getPoLineById(polineId, lang, httpClient, ctx, okapiHeaders, logger)
      .thenApply(poline -> validateOrderIdReference(orderId, poline))
      .thenCompose(poline -> HelperUtils.operateOnPoLine(HttpMethod.GET, poline, httpClient, ctx, okapiHeaders, logger))
      .thenAccept(future::complete)
      .exceptionally(t -> {
        logger.error("Failed to get composite purchase order line", t.getCause());
        future.completeExceptionally(t.getCause());
        return null;
      });

    return future;
  }

  /**
   * Validates if the retrieved PO line corresponds to PO (orderId). In case the PO line does not correspond to order id the exception is thrown
   *
   * @param orderId order identifier
   * @param poline  PO line retrieved from storage
   * @return PO line json object if validation is passed
   */
  private JsonObject validateOrderIdReference(String orderId, JsonObject poline) {
    if (!orderId.equals(poline.getValue("purchase_order_id")))
      throw new CompletionException(ORDER_REFERENCE_ERROR_MESSAGE, new HttpException(422, ORDER_REFERENCE_ERROR_MESSAGE));
    return poline;
  }

  public Void handleError(Throwable throwable) {
    final Future<Response> result;

    logger.error("Exception getting POLine", throwable.getCause());

    final Throwable t = throwable.getCause();
    if (t instanceof HttpException) {
      final int code = ((HttpException) t).getCode();
      final String message = t.getMessage();
      switch (code) {
        case 404:
          result = Future.succeededFuture(Orders.GetOrdersLinesByIdAndLineIdResponse.respond404WithTextPlain(message));
          break;
        case 422:
          Errors errors = generate422ErrorList(throwable);
          result = Future.succeededFuture(Orders.GetOrdersLinesByIdAndLineIdResponse.respond422WithApplicationJson(errors));
          break;
        case 500:
          result = Future.succeededFuture(Orders.GetOrdersLinesByIdAndLineIdResponse.respond500WithTextPlain(message));
          break;
        default:
          result = Future.succeededFuture(Orders.GetOrdersLinesByIdAndLineIdResponse.respond500WithTextPlain(message));
      }
    } else {
      result = Future.succeededFuture(Orders.GetOrdersLinesByIdAndLineIdResponse.respond500WithTextPlain(throwable.getMessage()));
    }

    httpClient.closeClient();

    asyncResultHandler.handle(result);

    return null;
  }

  private Errors generate422ErrorList(Throwable throwable) {
    Errors errors = new Errors();
    String message = throwable.getMessage();
    errors.getErrors()
      .add(new Error().withMessage(message).withCode(ERROR_CODE_422));
    return errors;
  }

}
