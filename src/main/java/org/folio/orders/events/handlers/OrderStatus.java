package org.folio.orders.events.handlers;

import static me.escoffier.vertx.completablefuture.VertxCompletableFuture.allOf;
import static me.escoffier.vertx.completablefuture.VertxCompletableFuture.supplyBlockingAsync;
import static org.folio.orders.utils.HelperUtils.getPoLines;
import static org.folio.orders.utils.HelperUtils.getPurchaseOrderById;
import static org.folio.orders.utils.HelperUtils.handlePutRequest;
import static org.folio.orders.utils.ResourcePathResolver.PURCHASE_ORDER;
import static org.folio.orders.utils.ResourcePathResolver.resourceByIdPath;
import static org.folio.rest.jaxrs.model.PoLine.PaymentStatus.FULLY_PAID;
import static org.folio.rest.jaxrs.model.PoLine.PaymentStatus.PAYMENT_NOT_REQUIRED;
import static org.folio.rest.jaxrs.model.PoLine.ReceiptStatus.FULLY_RECEIVED;
import static org.folio.rest.jaxrs.model.PoLine.ReceiptStatus.RECEIPT_NOT_REQUIRED;

import java.util.ArrayList;
import java.util.List;
import java.util.Map;
import java.util.concurrent.CompletableFuture;
import java.util.stream.Collectors;

import javax.ws.rs.core.Response;

import org.apache.commons.collections4.map.CaseInsensitiveMap;
import org.folio.orders.utils.HelperUtils;
import org.folio.rest.impl.AbstractHelper;
import org.folio.rest.jaxrs.model.CloseReason;
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

  static final String REASON_COMPLETE = "Complete";

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
              .thenCompose(linesArray -> supplyBlockingAsync(ctx, () -> convertToPoLines(linesArray)))
              .thenAccept(poLines -> {
                if (changeOrderStatus(purchaseOrder, poLines)) {
                  logger.debug("Workflow status update required for order with id={}", orderId);

                  handlePutRequest(resourceByIdPath(PURCHASE_ORDER, orderId), JsonObject.mapFrom(purchaseOrder),
                    httpClient, ctx, okapiHeaders, logger)
                    .thenAccept(future::complete)
                    .exceptionally(e -> {
                      future.completeExceptionally(e);
                      return null;
                    });
                } else {
                  future.complete(null);
                }
              })
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
    allOf(ctx, futures.toArray(new CompletableFuture[0]))
      .thenAccept(v -> {
        // Sending reply message just in case some logic requires it
        message.reply(Response.Status.OK.getReasonPhrase());
        httpClient.closeClient();
      })
      .exceptionally(e -> {
        message.fail(handleProcessingError(e), getErrors().get(0).getMessage());
        httpClient.closeClient();
        return null;
      });
  }

  private boolean changeOrderStatus(PurchaseOrder purchaseOrder, List<PoLine> poLines) {
    boolean isUpdateRequired = false;
    if (toBeClosed(purchaseOrder, poLines)) {
      isUpdateRequired = true;
      purchaseOrder.setWorkflowStatus(PurchaseOrder.WorkflowStatus.CLOSED);
      purchaseOrder.setCloseReason(new CloseReason().withReason(REASON_COMPLETE));
    } else if (toBeReopened(purchaseOrder, poLines)) {
      isUpdateRequired = true;
      purchaseOrder.setWorkflowStatus(PurchaseOrder.WorkflowStatus.OPEN);
    }
    return isUpdateRequired;
  }

  private List<PoLine> convertToPoLines(JsonArray linesArray) {
    return linesArray.stream()
                     .map(json -> ((JsonObject) json).mapTo(PoLine.class))
                     .collect(Collectors.toList());
  }

  private Map<String, String> getOkapiHeaders(Message<JsonObject> message) {
    Map<String, String> okapiHeaders = new CaseInsensitiveMap<>();
    message.headers().entries().forEach(entry -> okapiHeaders.put(entry.getKey(), entry.getValue()));
    return okapiHeaders;
  }

  private boolean toBeClosed(PurchaseOrder purchaseOrder, List<PoLine> poLines) {
    return purchaseOrder.getWorkflowStatus() == PurchaseOrder.WorkflowStatus.OPEN
      && poLines.stream().allMatch(this::isCompletedPoLine);
  }

  private boolean toBeReopened(PurchaseOrder purchaseOrder, List<PoLine> poLines) {
    return purchaseOrder.getWorkflowStatus() == PurchaseOrder.WorkflowStatus.CLOSED
      && poLines.stream().anyMatch(line -> !isCompletedPoLine(line));
  }

  private boolean isCompletedPoLine(PoLine line) {
    PoLine.PaymentStatus paymentStatus = line.getPaymentStatus();
    PoLine.ReceiptStatus receiptStatus = line.getReceiptStatus();
    return (paymentStatus == PAYMENT_NOT_REQUIRED || paymentStatus == FULLY_PAID)
      && (receiptStatus == FULLY_RECEIVED || receiptStatus == RECEIPT_NOT_REQUIRED);
  }
}
