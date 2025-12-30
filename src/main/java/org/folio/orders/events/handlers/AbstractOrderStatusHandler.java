package org.folio.orders.events.handlers;

import static org.folio.orders.utils.HelperUtils.getOkapiHeaders;
import static org.folio.service.orders.utils.StatusUtils.changeOrderStatusForPoLineUpdate;

import java.util.ArrayList;
import java.util.List;
import java.util.Map;

import org.folio.helper.BaseHelper;
import org.folio.helper.PurchaseOrderHelper;
import org.folio.rest.core.models.RequestContext;
import org.folio.rest.jaxrs.model.CompositePurchaseOrder;
import org.folio.rest.jaxrs.model.PoLine;
import org.folio.rest.jaxrs.model.PurchaseOrder;
import org.folio.service.finance.transaction.EncumbranceService;
import org.folio.service.orders.PurchaseOrderLineService;
import org.folio.service.orders.PurchaseOrderStorageService;

import io.vertx.core.Context;
import io.vertx.core.Future;
import io.vertx.core.Handler;
import io.vertx.core.Promise;
import io.vertx.core.eventbus.Message;
import io.vertx.core.json.JsonArray;
import io.vertx.core.json.JsonObject;

public abstract class AbstractOrderStatusHandler extends BaseHelper implements Handler<Message<JsonObject>> {
  private final EncumbranceService encumbranceService;
  private final PurchaseOrderStorageService purchaseOrderStorageService;
  private final PurchaseOrderHelper purchaseOrderHelper;
  private final PurchaseOrderLineService purchaseOrderLineService;

  protected AbstractOrderStatusHandler(Context ctx, EncumbranceService encumbranceService,
                              PurchaseOrderStorageService purchaseOrderStorageService, PurchaseOrderHelper purchaseOrderHelper,
    PurchaseOrderLineService purchaseOrderLineService) {
    super(ctx);
    this.encumbranceService = encumbranceService;
    this.purchaseOrderStorageService = purchaseOrderStorageService;
    this.purchaseOrderHelper = purchaseOrderHelper;
    this.purchaseOrderLineService = purchaseOrderLineService;
  }

  @Override
  public void handle(Message<JsonObject> message) {
    Map<String, String> okapiHeaders = getOkapiHeaders(message);

    List<Future<Void>> futures = new ArrayList<>();
    JsonArray orderItemStatusArray = messageAsJsonArray(EVENT_PAYLOAD, message);
    for (Object orderItemStatus : orderItemStatusArray.getList()) {
      JsonObject ordersPayload = (JsonObject) orderItemStatus;
      String orderId = ordersPayload.getString(ORDER_ID);
      // Add future which would hold result of operation
      Promise<Void> promise = Promise.promise();
      futures.add(promise.future());

      var requestContext = new RequestContext(ctx, okapiHeaders);
      // Get purchase order to check if order status needs to be changed.
      purchaseOrderStorageService.getPurchaseOrderById(orderId, requestContext)
        .onSuccess(purchaseOrder -> {
          if (isOrdersStatusChangeSkip(purchaseOrder, ordersPayload)) {
            promise.complete();
          } else {
            // Get purchase order lines to check if order status needs to be changed.
            purchaseOrderLineService.getPoLinesByOrderId(orderId, requestContext)
              .compose(poLines -> updateOrderStatus(purchaseOrder, poLines, new RequestContext(ctx, okapiHeaders)))
              .onSuccess(ok -> promise.complete())
              .onFailure(e -> {
                logger.error("The error happened processing workflow status update logic for order {}", orderId);
                promise.fail(e);
              });
          }
        })
        .onFailure(e -> {
          logger.error("The error happened getting order {}", orderId, e);
          promise.fail(e);
        });
    }

    // Now wait for all operations to be completed and send reply
    completeAllFutures(futures, message);
  }

  protected Future<Void> updateOrderStatus(PurchaseOrder purchaseOrder, List<PoLine> poLines, RequestContext requestContext) {
    var initialStatus = purchaseOrder.getWorkflowStatus();
    return ctx.owner().executeBlocking(() -> changeOrderStatusForPoLineUpdate(purchaseOrder, poLines), false)
      .compose(isStatusChanged -> {
        if (Boolean.TRUE.equals(isStatusChanged)) {
          return purchaseOrderHelper.handleFinalOrderItemsStatus(purchaseOrder, poLines, initialStatus.value(), requestContext)
            .compose(aVoid -> purchaseOrderStorageService.saveOrder(purchaseOrder, requestContext))
            .compose(purchaseOrderParam -> encumbranceService.updateEncumbrancesOrderStatusAndReleaseIfClosed(
              convert(purchaseOrder, poLines), requestContext));
        }
        return Future.succeededFuture();
      });
  }

  protected JsonArray messageAsJsonArray(String rootElement, Message<JsonObject> message) {
    JsonObject body = message.body();
    return body.getJsonArray(rootElement);
  }

  protected CompositePurchaseOrder convert(PurchaseOrder po, List<PoLine> poLines) {
    return JsonObject.mapFrom(po).mapTo(CompositePurchaseOrder.class).withPoLines(poLines);
  }

  protected abstract boolean isOrdersStatusChangeSkip(PurchaseOrder purchaseOrder, JsonObject ordersPayload);
}
