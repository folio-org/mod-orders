package org.folio.orders.events.handlers;

import static me.escoffier.vertx.completablefuture.VertxCompletableFuture.supplyBlockingAsync;
import static org.folio.orders.utils.HelperUtils.changeOrderStatus;
import static org.folio.orders.utils.HelperUtils.getOkapiHeaders;
import static org.folio.orders.utils.HelperUtils.getPoLines;
import static org.folio.orders.utils.HelperUtils.getPurchaseOrderById;

import java.util.ArrayList;
import java.util.List;
import java.util.Map;
import java.util.concurrent.CompletableFuture;

import org.folio.orders.utils.HelperUtils;
import org.folio.helper.AbstractHelper;
import org.folio.helper.PurchaseOrderHelper;
import org.folio.rest.jaxrs.model.PoLine;
import org.folio.rest.jaxrs.model.PurchaseOrder;
import org.folio.rest.tools.client.interfaces.HttpClientInterface;

import io.vertx.core.Context;
import io.vertx.core.Handler;
import io.vertx.core.eventbus.Message;
import io.vertx.core.json.JsonArray;
import io.vertx.core.json.JsonObject;
import me.escoffier.vertx.completablefuture.VertxCompletableFuture;

public abstract class AbstractOrderStatusHandler extends AbstractHelper implements Handler<Message<JsonObject>> {

  protected AbstractOrderStatusHandler(Context ctx) {
    super(ctx);
  }

  @Override
  public void handle(Message<JsonObject> message) {
    JsonObject body = message.body();
    logger.debug("Received message body: {}", body);
    String lang = body.getString(HelperUtils.LANG);

    Map<String, String> okapiHeaders = getOkapiHeaders(message);
    HttpClientInterface httpClient = getHttpClient(okapiHeaders, true);

    List<CompletableFuture<Void>> futures = new ArrayList<>();
    JsonArray orderItemStatusArray = messageAsJsonArray(EVENT_PAYLOAD, message);
    for (Object orderItemStatus : orderItemStatusArray.getList()) {
      JsonObject ordersPayload = (JsonObject) orderItemStatus;
      String orderId = ordersPayload.getString(ORDER_ID);
      // Add future which would hold result of operation
      CompletableFuture<Void> future = new VertxCompletableFuture<>(ctx);
      futures.add(future);

      // Get purchase order to check if order status needs to be changed.
      getPurchaseOrderById(orderId, lang, httpClient, ctx, okapiHeaders, logger)
        .thenAccept(orderJson -> {
          PurchaseOrder purchaseOrder = orderJson.mapTo(PurchaseOrder.class);

          if (isOrdersStatusChangeSkip(purchaseOrder, ordersPayload)) {
            future.complete(null);
          } else {
            // Get purchase order lines to check if order status needs to be changed.
            getPoLines(orderId, lang, httpClient, ctx, okapiHeaders, logger)
              .thenCompose(linesArray -> supplyBlockingAsync(ctx, () -> HelperUtils.convertJsonToPoLines(linesArray)))
              .thenCompose(poLines -> updateOrderStatus(okapiHeaders, httpClient, purchaseOrder, poLines))
              .thenAccept(future::complete)
              .exceptionally(e -> {
                logger.error("The error happened processing workflow status update logic for order {}", orderId, e);
                future.completeExceptionally(e);
                return null;
              });
          }
        })
        .exceptionally(e -> {
          logger.error("The error happened getting order {}", orderId, e);
          future.completeExceptionally(e);
          return null;
        });
    }

    // Now wait for all operations to be completed and send reply
    completeAllFutures(ctx, httpClient, futures, message);
  }

  protected CompletableFuture<Void> updateOrderStatus(Map<String, String> okapiHeaders, HttpClientInterface httpClient,
    PurchaseOrder purchaseOrder, List<PoLine> poLines) {
    PurchaseOrder.WorkflowStatus initialStatus = purchaseOrder.getWorkflowStatus();
    PurchaseOrderHelper helper = new PurchaseOrderHelper(httpClient, okapiHeaders, ctx, lang);
    return VertxCompletableFuture.supplyBlockingAsync(ctx, () -> changeOrderStatus(purchaseOrder, poLines))
      .thenCompose(isStatusChanged -> {
        if (Boolean.TRUE.equals(isStatusChanged)) {
          return helper.handleFinalOrderStatus(purchaseOrder, poLines, initialStatus.value())
            .thenCompose(aVoid -> helper.updateOrderSummary(purchaseOrder));
        }
        return CompletableFuture.completedFuture(null);
      });
  }

  protected JsonArray messageAsJsonArray(String rootElement, Message<JsonObject> message) {
    JsonObject body = message.body();
    logger.debug("Received message body: {}", body);
    return body.getJsonArray(rootElement);
  }

  protected abstract boolean isOrdersStatusChangeSkip(PurchaseOrder purchaseOrder, JsonObject ordersPayload);
}
