package org.folio.orders.events.handlers;

import static me.escoffier.vertx.completablefuture.VertxCompletableFuture.allOf;
import static me.escoffier.vertx.completablefuture.VertxCompletableFuture.supplyBlockingAsync;
import static org.folio.orders.utils.HelperUtils.getPoLines;
import static org.folio.orders.utils.HelperUtils.getPurchaseOrderById;
import static org.folio.orders.utils.HelperUtils.getPoLineById;
import static org.folio.orders.utils.HelperUtils.handlePutRequest;
import static org.folio.orders.utils.ResourcePathResolver.PURCHASE_ORDER;
import static org.folio.orders.utils.ResourcePathResolver.resourceByIdPath;

import java.util.ArrayList;
import java.util.List;
import java.util.Map;
import java.util.concurrent.CompletableFuture;
import java.util.stream.Collectors;

import javax.ws.rs.core.Response;

import org.apache.commons.collections4.map.CaseInsensitiveMap;
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

@Component("receiptStatusHandler")
public class ReceiptStatus extends AbstractHelper implements Handler<Message<JsonObject>> {

  @Autowired
  public ReceiptStatus(Vertx vertx) {
    super(vertx.getOrCreateContext());
  }

  @Override
  public void handle(Message<JsonObject> message) {
    JsonObject body = message.body();

    logger.debug("Received message body: {}", body);

    Map<String, String> okapiHeaders = getOkapiHeaders(message);
    HttpClientInterface httpClient = getHttpClient(okapiHeaders, true);

    JsonArray poLines = body.getJsonArray("poLines");
    String lang = body.getString(HelperUtils.LANG);

    List<CompletableFuture<Void>> futures = new ArrayList<>();

    for (Object id : poLines) {
      String lineId = (String) id;

      // Add future which would hold result of operation
      CompletableFuture<Void> future = new VertxCompletableFuture<>(ctx);
      futures.add(future);

      // Get po line to check if receipt status needs to be changed.
      getPoLineById(lineId, lang, httpClient, ctx, okapiHeaders, logger)
        .thenAccept(lineJson -> {
          PoLine poLine = lineJson.mapTo(PoLine.class);

//          if (purchaseOrder.getWorkflowStatus() == PurchaseOrder.WorkflowStatus.PENDING) {
//            future.complete(null);
//          } else {
//            // Get purchase order lines to check if order status needs to be changed.
//            getPoLines(orderId, lang, httpClient, ctx, okapiHeaders, logger)
//              .thenCompose(linesArray -> supplyBlockingAsync(ctx, () -> convertToPoLines(linesArray)))
//              .thenCompose(poLines -> updateOrderStatus(okapiHeaders, httpClient, purchaseOrder, poLines))
//              .thenAccept(future::complete)
//              .exceptionally(e -> {
//                logger.error("The error happened processing workflow status update logic for order {}", orderId, e);
//                future.completeExceptionally(e);
//                return null;
//              });
//          }
        })
        .exceptionally(e -> {
          logger.error("The error happened getting order {}", lineId, e);
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

  CompletableFuture<Void> updateOrderStatus(Map<String, String> okapiHeaders, HttpClientInterface httpClient, PurchaseOrder purchaseOrder, List<PoLine> poLines) {
    String orderId = purchaseOrder.getId();
    if (HelperUtils.changeOrderStatus(purchaseOrder, poLines)) {
      logger.debug("Workflow status update required for order with id={}", orderId);

      return handlePutRequest(resourceByIdPath(PURCHASE_ORDER, orderId), JsonObject.mapFrom(purchaseOrder),
        httpClient, ctx, okapiHeaders, logger);
    }
    return VertxCompletableFuture.completedFuture(null);
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

}
