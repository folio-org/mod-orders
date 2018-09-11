package org.folio.rest.impl;

import java.util.ArrayList;
import java.util.List;
import java.util.Map;
import java.util.concurrent.CompletableFuture;
import java.util.concurrent.CompletionException;

import org.apache.log4j.Logger;
import org.folio.orders.rest.exceptions.HttpException;
import org.folio.rest.jaxrs.model.CompositePurchaseOrder;
import org.folio.rest.jaxrs.model.PoLine;
import org.folio.rest.jaxrs.model.PurchaseOrder;
import org.folio.rest.jaxrs.resource.OrdersResource.PostOrdersResponse;
import org.folio.rest.tools.client.interfaces.HttpClientInterface;

import io.vertx.core.AsyncResult;
import io.vertx.core.Context;
import io.vertx.core.Future;
import io.vertx.core.Handler;
import io.vertx.core.buffer.Buffer;
import io.vertx.core.http.HttpMethod;
import io.vertx.core.json.JsonObject;
import me.escoffier.vertx.completablefuture.VertxCompletableFuture;

public class PostOrdersHelper {

  private static final Logger logger = Logger.getLogger(PostOrdersHelper.class);

  // epoch time @ 9/1/2018.
  // if you change this, you run the risk of duplicate poNumbers
  private static final long PO_NUMBER_EPOCH = 1535774400000L;

  private final HttpClientInterface httpClient;
  private final Context ctx;
  private final Handler<AsyncResult<javax.ws.rs.core.Response>> asyncResultHandler;
  private final Map<String, String> okapiHeaders;

  public PostOrdersHelper(HttpClientInterface httpClient, Map<String, String> okapiHeaders,
      Handler<AsyncResult<javax.ws.rs.core.Response>> asyncResultHandler, Context ctx) {
    this.httpClient = httpClient;
    this.okapiHeaders = okapiHeaders;
    this.ctx = ctx;
    this.asyncResultHandler = asyncResultHandler;
  }

  public CompletableFuture<CompositePurchaseOrder> createPOandPOLines(CompositePurchaseOrder compPO) {
    CompletableFuture<CompositePurchaseOrder> future = new VertxCompletableFuture<>(ctx);

    compPO.getPurchaseOrder().setPoNumber(generatePoNumber());

    try {
      Buffer poBuf = JsonObject.mapFrom(compPO.getPurchaseOrder()).toBuffer();
      httpClient.request(HttpMethod.POST, poBuf, "/purchase_order", okapiHeaders)
        .thenApply(OrdersResourceImpl::verifyAndExtractBody)
        .thenAccept(poBody -> {
          PurchaseOrder po = poBody.mapTo(PurchaseOrder.class);
          String poNumber = po.getPoNumber();
          String poId = po.getId();
          compPO.setPurchaseOrder(po);

          List<PoLine> lines = new ArrayList<>(compPO.getPoLines().size());
          List<CompletableFuture<Void>> futures = new ArrayList<>();
          for (int i = 0; i < compPO.getPoLines().size(); i++) {
            PoLine line = compPO.getPoLines().get(i);
            line.setPurchaseOrderId(poId);
            line.setPoLineNumber(poNumber + "-" + (i + 1));
            try {
              Buffer polBuf = JsonObject.mapFrom(line).toBuffer();
              CompletableFuture<Void> polFut = httpClient.request(HttpMethod.POST, polBuf, "/po_line", okapiHeaders)
                .thenApply(OrdersResourceImpl::verifyAndExtractBody)
                .thenAccept(body -> lines.add((PoLine) body.mapTo(PoLine.class)));
              futures.add(polFut);
            } catch (Exception e) {
              logger.error("Exception calling POST /po_line", e);
              throw new CompletionException(e);
            }
          }

          VertxCompletableFuture.allOf(ctx, futures.toArray(new CompletableFuture[futures.size()]))
            .thenAccept(v -> {
              compPO.setPoLines(lines);
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
      logger.error("Exception calling POST /purchase_order", e);
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

  public Void handleError(Throwable throwable) {
    final Future<javax.ws.rs.core.Response> result;

    logger.error("Exception placing order", throwable.getCause());

    final Throwable t = throwable.getCause();
    if (t instanceof HttpException) {
      final int code = ((HttpException) t).getCode();
      final String message = ((HttpException) t).getMessage();
      switch (code) {
      case 400:
        result = Future.succeededFuture(PostOrdersResponse.withPlainBadRequest(message));
        break;
      case 500:
        result = Future.succeededFuture(PostOrdersResponse.withPlainInternalServerError(message));
        break;
      case 401:
        result = Future.succeededFuture(PostOrdersResponse.withPlainUnauthorized(message));
        break;
      default:
        result = Future.succeededFuture(PostOrdersResponse.withPlainInternalServerError(message));
      }
    } else {
      result = Future.succeededFuture(PostOrdersResponse.withPlainInternalServerError(throwable.getMessage()));
    }

    httpClient.closeClient();

    asyncResultHandler.handle(result);

    return null;
  }

  public static String generatePoNumber() {
    return (Long.toHexString(System.currentTimeMillis() - PO_NUMBER_EPOCH) +
        Long.toHexString(System.nanoTime() % 100))
          .toUpperCase();
  }

}
