package org.folio.rest.impl;

import static org.folio.orders.utils.ResourcePathResolver.PURCHASE_ORDER;
import static org.folio.orders.utils.ResourcePathResolver.resourcesPath;

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
import org.folio.rest.jaxrs.model.PoNumber;
import org.folio.rest.tools.client.interfaces.HttpClientInterface;

import io.vertx.core.AsyncResult;
import io.vertx.core.Context;
import io.vertx.core.Handler;
import io.vertx.core.http.HttpMethod;
import io.vertx.core.json.JsonObject;
import me.escoffier.vertx.completablefuture.VertxCompletableFuture;

public class PostOrdersHelper extends AbstractHelper {

  private PoNumberHelper poNumberHelper;


  public PostOrdersHelper(HttpClientInterface httpClient, Map<String, String> okapiHeaders,
      Handler<AsyncResult<javax.ws.rs.core.Response>> asyncResultHandler, Context ctx, String lang) {
    super(httpClient, okapiHeaders, asyncResultHandler, ctx, lang);
    poNumberHelper = new PoNumberHelper(httpClient, okapiHeaders, asyncResultHandler, ctx, lang);
  }

  public CompletableFuture<CompositePurchaseOrder> createPurchaseOrder(CompositePurchaseOrder compPO) {
    CompletableFuture<CompositePurchaseOrder> future = new VertxCompletableFuture<>(ctx);
    if(null==compPO.getPoNumber()){
      return poNumberHelper.generatePoNumber()
        .thenAccept(poNumberResp -> compPO.setPoNumber(poNumberResp.mapTo(PoNumber.class).getPoNumber()))
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

    try {
      JsonObject purchaseOrder = convertToPurchaseOrder(compPO);
      httpClient.request(HttpMethod.POST, purchaseOrder, resourcesPath(PURCHASE_ORDER), okapiHeaders)
        .thenApply(HelperUtils::verifyAndExtractBody)
        .thenAccept(poBody -> {
          CompositePurchaseOrder po = poBody.mapTo(CompositePurchaseOrder.class);
          String poNumber = po.getPoNumber();
          String poId = po.getId();
          compPO.setId(poId);

          List<PoLine> lines = new ArrayList<>(compPO.getPoLines().size());
          List<CompletableFuture<Void>> futures = new ArrayList<>();
          boolean updateInventory = compPO.getWorkflowStatus() == WorkflowStatus.OPEN;
          for (int i = 0; i < compPO.getPoLines().size(); i++) {
            PoLine compPOL = compPO.getPoLines().get(i);
            compPOL.setPurchaseOrderId(poId);
            compPOL.setPoLineNumber(poNumber + "-" + (i + 1));

            futures.add(new PostOrderLineHelper(httpClient, okapiHeaders, asyncResultHandler, ctx, lang)
              .createPoLine(compPOL, updateInventory)
              .thenAccept(lines::add));
          }

          VertxCompletableFuture.allOf(ctx, futures.toArray(new CompletableFuture[0]))
            .thenAccept(v -> {
              compPO.setPoLines(lines);
              compPO.setAdjustment(HelperUtils.calculateAdjustment(lines));
              future.complete(compPO);
            })
            .exceptionally(e -> {
              future.completeExceptionally(e.getCause());
              return null;
            });
        })
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
