package org.folio.rest.impl;

import static java.util.concurrent.CompletableFuture.completedFuture;
import static org.folio.rest.jaxrs.model.CompositePurchaseOrder.WorkflowStatus.OPEN;
import static org.folio.rest.jaxrs.model.CompositePurchaseOrder.WorkflowStatus.PENDING;
import static org.folio.orders.utils.ResourcePathResolver.PURCHASE_ORDER;
import static org.folio.orders.utils.ResourcePathResolver.resourcesPath;

import java.util.ArrayList;
import java.util.List;
import java.util.Map;
import java.util.concurrent.CompletableFuture;

import javax.ws.rs.core.Response;

import org.folio.orders.utils.HelperUtils;
import org.folio.rest.jaxrs.model.CompositePoLine;
import org.folio.rest.jaxrs.model.CompositePurchaseOrder;
import org.folio.rest.jaxrs.model.CompositePurchaseOrder.WorkflowStatus;
import org.folio.rest.jaxrs.model.Error;
import org.folio.rest.jaxrs.resource.Orders.PostOrdersCompositeOrdersResponse;
import org.folio.rest.tools.client.interfaces.HttpClientInterface;

import io.vertx.core.AsyncResult;
import io.vertx.core.Context;
import io.vertx.core.Handler;
import io.vertx.core.http.HttpMethod;
import io.vertx.core.json.JsonObject;
import me.escoffier.vertx.completablefuture.VertxCompletableFuture;

public class PostOrdersHelper extends AbstractHelper {

  private PoNumberHelper poNumberHelper;


  private final PutOrdersByIdHelper putOrderHelper;

  public PostOrdersHelper(HttpClientInterface httpClient, Map<String, String> okapiHeaders,
      Handler<AsyncResult<javax.ws.rs.core.Response>> asyncResultHandler, Context ctx, String lang) {
    super(httpClient, okapiHeaders, asyncResultHandler, ctx, lang);
    this.putOrderHelper = new PutOrdersByIdHelper(okapiHeaders, asyncResultHandler, ctx, lang);
    poNumberHelper = new PoNumberHelper(httpClient, okapiHeaders, asyncResultHandler, ctx, lang);
  }

  public CompletableFuture<CompositePurchaseOrder> createPurchaseOrder(CompositePurchaseOrder compPO) {
    CompletableFuture<CompositePurchaseOrder> future = new VertxCompletableFuture<>(ctx);
    if(null==compPO.getPoNumber()){
      return poNumberHelper.generatePoNumber()
        .thenAccept(compPO::setPoNumber)
        .thenCompose(rVoid -> createPOandPOLines(compPO));
    }
    else {
      //If a PO  Number is already supplied, then verify if its unique
      poNumberHelper.checkPONumberUnique(compPO.getPoNumber())
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
          compPO.setCompositePoLines(lines);
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

  private CompletableFuture<List<CompositePoLine>> handlePoLines(CompositePurchaseOrder compPO) {
    List<CompositePoLine> lines = new ArrayList<>(compPO.getCompositePoLines().size());
    List<CompletableFuture<Void>> futures = new ArrayList<>();
    for (int i = 0; i < compPO.getCompositePoLines().size(); i++) {
      CompositePoLine compPOL = compPO.getCompositePoLines().get(i);
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
}
