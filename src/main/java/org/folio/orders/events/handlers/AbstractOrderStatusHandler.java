package org.folio.orders.events.handlers;

import static org.folio.orders.utils.HelperUtils.changeOrderStatus;
import static org.folio.orders.utils.HelperUtils.getOkapiHeaders;
import static org.folio.orders.utils.HelperUtils.getPoLines;
import static org.folio.orders.utils.HelperUtils.getPurchaseOrderById;

import java.util.ArrayList;
import java.util.List;
import java.util.Map;
import java.util.concurrent.CompletableFuture;

import org.folio.helper.AbstractHelper;
import org.folio.helper.PurchaseOrderHelper;
import org.folio.orders.utils.AsyncUtil;
import org.folio.orders.utils.HelperUtils;
import org.folio.rest.core.models.RequestContext;
import org.folio.rest.jaxrs.model.CompositePurchaseOrder;
import org.folio.rest.jaxrs.model.PoLine;
import org.folio.rest.jaxrs.model.PurchaseOrder;
import org.folio.rest.tools.client.interfaces.HttpClientInterface;
import org.folio.service.finance.EncumbranceService;

import io.vertx.core.Context;
import io.vertx.core.Handler;
import io.vertx.core.eventbus.Message;
import io.vertx.core.json.JsonArray;
import io.vertx.core.json.JsonObject;


public abstract class AbstractOrderStatusHandler extends AbstractHelper implements Handler<Message<JsonObject>> {


  private final EncumbranceService encumbranceService;
  protected AbstractOrderStatusHandler(Context ctx, EncumbranceService encumbranceService) {
    super(ctx);
    this.encumbranceService = encumbranceService;
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
      CompletableFuture<Void> future = new CompletableFuture<>();
      futures.add(future);

      // Get purchase order to check if order status needs to be changed.
      getPurchaseOrderById(orderId, lang, httpClient, okapiHeaders, logger)
        .thenAccept(orderJson -> {
          PurchaseOrder purchaseOrder = orderJson.mapTo(PurchaseOrder.class);

          if (isOrdersStatusChangeSkip(purchaseOrder, ordersPayload)) {
            future.complete(null);
          } else {
            // Get purchase order lines to check if order status needs to be changed.
            getPoLines(orderId, lang, httpClient, okapiHeaders, logger)
              .thenCompose(linesArray -> AsyncUtil.executeBlocking(ctx, false, () -> HelperUtils.convertJsonToPoLines(linesArray)))
              .thenCompose(poLines -> updateOrderStatus(okapiHeaders, lang, httpClient, purchaseOrder, poLines))
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
    completeAllFutures(httpClient, futures, message);
  }

  protected CompletableFuture<Void> updateOrderStatus(Map<String, String> okapiHeaders, String lang, HttpClientInterface httpClient,
      PurchaseOrder purchaseOrder, List<PoLine> poLines) {

    PurchaseOrder.WorkflowStatus initialStatus = purchaseOrder.getWorkflowStatus();
    PurchaseOrderHelper helper = new PurchaseOrderHelper(httpClient, okapiHeaders, ctx, lang);
    return AsyncUtil.executeBlocking(ctx, false, () -> changeOrderStatus(purchaseOrder, poLines))
      .thenCompose(isStatusChanged -> {
        if (Boolean.TRUE.equals(isStatusChanged)) {
          return helper.handleFinalOrderItemsStatus(purchaseOrder, poLines, initialStatus.value())
            .thenCompose(aVoid -> helper.updateOrderSummary(purchaseOrder))
            .thenCompose(purchaseOrderParam -> encumbranceService.updateEncumbrancesOrderStatus(purchaseOrder.getId(), convert(purchaseOrder.getWorkflowStatus()), new RequestContext(ctx, okapiHeaders)));
        }
        return CompletableFuture.completedFuture(null);
      });
  }

  protected JsonArray messageAsJsonArray(String rootElement, Message<JsonObject> message) {
    JsonObject body = message.body();
    logger.debug("Received message body: {}", body);
    return body.getJsonArray(rootElement);
  }

  protected CompositePurchaseOrder.WorkflowStatus convert(PurchaseOrder.WorkflowStatus workflowStatus) {
    return CompositePurchaseOrder.WorkflowStatus.fromValue(workflowStatus.value());
  }

  protected abstract boolean isOrdersStatusChangeSkip(PurchaseOrder purchaseOrder, JsonObject ordersPayload);
}
