package org.folio.orders.events.handlers;

import static me.escoffier.vertx.completablefuture.VertxCompletableFuture.supplyBlockingAsync;
import static org.folio.orders.utils.HelperUtils.changeOrderStatus;
import static org.folio.orders.utils.HelperUtils.getOkapiHeaders;
import static org.folio.orders.utils.HelperUtils.getPoLines;
import static org.folio.orders.utils.HelperUtils.getPurchaseOrderById;
import static org.folio.rest.impl.CheckinHelper.IS_ITEM_ORDER_CLOSED_PRESENT;
import static org.folio.rest.impl.CheckinHelper.ORDER_ID;
import static org.folio.rest.impl.CheckinHelper.ORDER_ITEM_STATUS_MAP;

import java.util.ArrayList;
import java.util.List;
import java.util.Map;
import java.util.concurrent.CompletableFuture;

import org.folio.orders.utils.HelperUtils;
import org.folio.rest.impl.AbstractHelper;
import org.folio.rest.impl.PurchaseOrderHelper;
import org.folio.rest.jaxrs.model.PoLine;
import org.folio.rest.jaxrs.model.PurchaseOrder;
import org.folio.rest.tools.client.interfaces.HttpClientInterface;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.stereotype.Component;

import io.vertx.core.Handler;
import io.vertx.core.Vertx;
import io.vertx.core.eventbus.Message;
import io.vertx.core.json.JsonArray;
import io.vertx.core.json.JsonObject;
import me.escoffier.vertx.completablefuture.VertxCompletableFuture;

@Component("checkInOrderStatusChangeHandler")
public class CheckInOrderStatusChangeHandler extends AbstractHelper implements Handler<Message<JsonObject>> {

  @Autowired
  public CheckInOrderStatusChangeHandler(Vertx vertx) {
    super(vertx.getOrCreateContext());
  }

  @Override
  public void handle(Message<JsonObject> message) {
    JsonObject body = message.body();

    logger.debug("Received message body: {}", body);

    Map<String, String> okapiHeaders = getOkapiHeaders(message);
    HttpClientInterface httpClient = getHttpClient(okapiHeaders, true);

    JsonArray orderItemStatusArray = body.getJsonArray(ORDER_ITEM_STATUS_MAP);
    String lang = body.getString(HelperUtils.LANG);

    List<CompletableFuture<Void>> futures = new ArrayList<>();
    for (Object orderItemStatus : orderItemStatusArray.getList()) {
      JsonObject orderItemStatusMap = JsonObject.class.cast(orderItemStatus);
      String orderId = orderItemStatusMap.getString(ORDER_ID);
      boolean orderItemOrderClosedPresent = orderItemStatusMap.getBoolean(IS_ITEM_ORDER_CLOSED_PRESENT);
      // Add future which would hold result of operation
      CompletableFuture<Void> future = new VertxCompletableFuture<>(ctx);
      futures.add(future);

      // Get purchase order to check if order status needs to be changed.
      getPurchaseOrderById(orderId, lang, httpClient, ctx, okapiHeaders, logger)
        .thenAccept(orderJson -> {
          PurchaseOrder purchaseOrder = orderJson.mapTo(PurchaseOrder.class);

          if (purchaseOrder.getWorkflowStatus() == PurchaseOrder.WorkflowStatus.PENDING) {
            future.complete(null);
          } else if (purchaseOrder.getWorkflowStatus() == PurchaseOrder.WorkflowStatus.CLOSED && orderItemOrderClosedPresent) {
            future.complete(null);
          }  else {
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

  CompletableFuture<Void> updateOrderStatus(Map<String, String> okapiHeaders, HttpClientInterface httpClient, PurchaseOrder purchaseOrder, List<PoLine> poLines) {
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

}
