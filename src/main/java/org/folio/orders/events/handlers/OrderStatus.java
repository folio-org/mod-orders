package org.folio.orders.events.handlers;

import static me.escoffier.vertx.completablefuture.VertxCompletableFuture.supplyBlockingAsync;
import static org.folio.orders.utils.HelperUtils.getOkapiHeaders;
import static org.folio.orders.utils.HelperUtils.getPoLines;
import static org.folio.orders.utils.HelperUtils.getPurchaseOrderById;
import static org.folio.orders.utils.HelperUtils.handlePutRequest;
import static org.folio.orders.utils.ResourcePathResolver.PURCHASE_ORDER;
import static org.folio.orders.utils.ResourcePathResolver.resourceByIdPath;

import java.util.ArrayList;
import java.util.List;
import java.util.Map;
import java.util.concurrent.CompletableFuture;

import org.folio.orders.utils.HelperUtils;
import org.folio.rest.impl.AbstractHelper;
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

@Component("orderStatusHandler")
public class OrderStatus extends AbstractHelper implements Handler<Message<JsonObject>> {

  @Autowired
  public OrderStatus(Vertx vertx) {
    super(vertx.getOrCreateContext());
  }

  @Override
  public void handle(Message<JsonObject> message) {
    JsonObject body = message.body();

    logger.debug("Received message body: {}", body);

    Map<String, String> okapiHeaders = getOkapiHeaders(message);
    HttpClientInterface httpClient = getHttpClient(okapiHeaders, true);

    JsonArray orderIds = body.getJsonArray("orderIds");
    String lang = body.getString(HelperUtils.LANG);

    List<CompletableFuture<Void>> futures = new ArrayList<>();
    for (Object id : orderIds) {
      String orderId = (String) id;

      // Add future which would hold result of operation
      CompletableFuture<Void> future = new VertxCompletableFuture<>(ctx);
      futures.add(future);

      // Get purchase order to check if order status needs to be changed.
      getPurchaseOrderById(orderId, lang, httpClient, ctx, okapiHeaders, logger)
        .thenAccept(orderJson -> {
          PurchaseOrder purchaseOrder = orderJson.mapTo(PurchaseOrder.class);

          if (purchaseOrder.getWorkflowStatus() == PurchaseOrder.WorkflowStatus.PENDING) {
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

  CompletableFuture<Void> updateOrderStatus(Map<String, String> okapiHeaders, HttpClientInterface httpClient, PurchaseOrder purchaseOrder, List<PoLine> poLines) {
    String orderId = purchaseOrder.getId();
    if (HelperUtils.changeOrderStatus(purchaseOrder, poLines)) {
      logger.debug("Workflow status update required for order with id={}", orderId);

      return handlePutRequest(resourceByIdPath(PURCHASE_ORDER, orderId), JsonObject.mapFrom(purchaseOrder),
        httpClient, ctx, okapiHeaders, logger);
    }
    return VertxCompletableFuture.completedFuture(null);
  }

}
