package org.folio.rest.impl;

import static java.util.concurrent.CompletableFuture.completedFuture;
import static org.folio.orders.utils.SubObjects.PURCHASE_ORDER;
import static org.folio.orders.utils.SubObjects.resourcesPath;
import static org.folio.rest.jaxrs.model.CompositePurchaseOrder.WorkflowStatus.OPEN;
import static org.folio.rest.jaxrs.model.CompositePurchaseOrder.WorkflowStatus.PENDING;

import java.util.ArrayList;
import java.util.List;
import java.util.Map;
import java.util.concurrent.CompletableFuture;

import javax.ws.rs.core.Response;

import org.folio.orders.utils.HelperUtils;
import org.folio.rest.jaxrs.model.CompositePurchaseOrder;
import org.folio.rest.jaxrs.model.CompositePurchaseOrder.WorkflowStatus;
import org.folio.rest.jaxrs.model.Error;
import org.folio.rest.jaxrs.model.PoLine;
import org.folio.rest.jaxrs.resource.Orders.PostOrdersCompositeOrdersResponse;
import org.folio.rest.tools.client.interfaces.HttpClientInterface;

import io.vertx.core.AsyncResult;
import io.vertx.core.Context;
import io.vertx.core.Handler;
import io.vertx.core.http.HttpMethod;
import io.vertx.core.json.JsonObject;
import me.escoffier.vertx.completablefuture.VertxCompletableFuture;

public class PostOrdersHelper extends AbstractHelper {

  // epoch time @ 9/1/2018.
  // if you change this, you run the risk of duplicate poNumbers
  private static final long PO_NUMBER_EPOCH = 1535774400000L;

  private final ValidationHelper validationHelper;
  private final PutOrdersByIdHelper putOrderHelper;

  public PostOrdersHelper(HttpClientInterface httpClient, Map<String, String> okapiHeaders,
      Handler<AsyncResult<javax.ws.rs.core.Response>> asyncResultHandler, Context ctx, String lang) {
    super(httpClient, okapiHeaders, asyncResultHandler, ctx, lang);
    this.validationHelper = new ValidationHelper(httpClient, okapiHeaders, asyncResultHandler, ctx, lang);
    this.putOrderHelper = new PutOrdersByIdHelper(okapiHeaders, asyncResultHandler, ctx, lang);
  }

  public CompletableFuture<CompositePurchaseOrder> createPurchaseOrder(CompositePurchaseOrder compPO) {
    CompletableFuture<CompositePurchaseOrder> future = new VertxCompletableFuture<>(ctx);
    if (null == compPO.getPoNumber()) {
      compPO.setPoNumber(generatePoNumber());
      return createPOandPOLines(compPO);
    }
    else {
      //If a PO  Number is already supplied, then verify if its unique
      validationHelper.checkPONumberUnique(compPO.getPoNumber())
      .thenAccept(v ->
        createPOandPOLines(compPO)
          .thenAccept(future::complete)
          .exceptionally(e -> {
             future.completeExceptionally(e);
             return null;
          })
      )
      .exceptionally(e -> {
           logger.error(e.getMessage());
           future.completeExceptionally(e);
           return null;
      });
    }
    return future;
  }

  private CompletableFuture<CompositePurchaseOrder> createPOandPOLines(CompositePurchaseOrder compPO) {
    CompletableFuture<CompositePurchaseOrder> future = new VertxCompletableFuture<>(ctx);
    final WorkflowStatus finalStatus = compPO.getWorkflowStatus();

    try {
      // we should always create PO and PO lines in PENDING status and transition to OPEN only when it's all set
      // (e.g. PO lines are created, Inventory is updated, etc.)
      if (finalStatus == OPEN) {
        compPO.setWorkflowStatus(PENDING);
      }
      JsonObject purchaseOrder = convertToPurchaseOrder(compPO);
      httpClient.request(HttpMethod.POST, purchaseOrder, resourcesPath(PURCHASE_ORDER), okapiHeaders)
        .thenApply(HelperUtils::verifyAndExtractBody)
        .thenAccept(poBody -> {
          CompositePurchaseOrder po = poBody.mapTo(CompositePurchaseOrder.class);
          String poId = po.getId();
          compPO.setId(poId);
        })
        .thenCompose(v -> handlePoLines(compPO))
        .thenAccept(lines -> {
          compPO.setPoLines(lines);
          compPO.setAdjustment(HelperUtils.calculateAdjustment(lines));
        })
        .thenCompose(v -> {
          if (finalStatus == OPEN) {
            return putOrderHelper.openOrder(compPO);
          }
          return completedFuture(null);
        })
        .thenAccept(v -> future.complete(compPO))
        .exceptionally(e -> {
          future.completeExceptionally(e.getCause());
          return null;
        });
    } catch (Exception e) {
      logger.error("Exception calling POST /orders-storage/purchase_orders", e);
      future.completeExceptionally(e);
    }
    return future;
  }

  private CompletableFuture<List<PoLine>> handlePoLines(CompositePurchaseOrder compPO) {
    List<PoLine> lines = new ArrayList<>(compPO.getPoLines().size());
    List<CompletableFuture<Void>> futures = new ArrayList<>();
    for (int i = 0; i < compPO.getPoLines().size(); i++) {
      PoLine compPOL = compPO.getPoLines().get(i);
      compPOL.setPurchaseOrderId(compPO.getId());
      compPOL.setPoLineNumber(compPO.getPoNumber() + "-" + (i + 1));

      futures.add(new PostOrderLineHelper(httpClient, okapiHeaders, asyncResultHandler, ctx, lang)
        .createPoLine(compPOL)
        .thenAccept(lines::add));
    }

    return VertxCompletableFuture.allOf(ctx, futures.toArray(new CompletableFuture[0]))
      .thenApply(v -> lines);
  }

  public CompletableFuture<CompositePurchaseOrder> applyFunds(CompositePurchaseOrder compPO) {
    CompletableFuture<CompositePurchaseOrder> future = new VertxCompletableFuture<>(ctx);
    future.complete(compPO);
    return future;
  }

  public CompletableFuture<CompositePurchaseOrder> updateInventory(CompositePurchaseOrder compPO) {
    CompletableFuture<CompositePurchaseOrder> future = new VertxCompletableFuture<>(ctx);
    future.complete(compPO);
    return future;
  }

  @Override
  Response buildErrorResponse(int code, Error error) {
    final Response result;
    switch (code) {
      case 400:
        result = PostOrdersCompositeOrdersResponse.respond400WithTextPlain(error.getMessage());
        break;
      case 401:
        result = PostOrdersCompositeOrdersResponse.respond401WithTextPlain(error.getMessage());
        break;
      case 422:
        result = PostOrdersCompositeOrdersResponse.respond422WithApplicationJson(withErrors(error));
        break;
      default:
        result = PostOrdersCompositeOrdersResponse.respond500WithTextPlain(error.getMessage());
    }
    return result;
  }

  private static String generatePoNumber() {
    return (Long.toHexString(System.currentTimeMillis() - PO_NUMBER_EPOCH) +
        Long.toHexString(System.nanoTime() % 100))
          .toUpperCase();
  }

}
