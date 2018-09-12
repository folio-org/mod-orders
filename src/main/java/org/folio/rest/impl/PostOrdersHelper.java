package org.folio.rest.impl;

import java.util.ArrayList;
import java.util.List;
import java.util.Map;
import java.util.concurrent.CompletableFuture;
import java.util.concurrent.CompletionException;

import org.apache.log4j.Logger;
import org.folio.orders.rest.exceptions.HttpException;
import org.folio.rest.jaxrs.model.CompositePurchaseOrder;
import org.folio.rest.jaxrs.model.Cost;
import org.folio.rest.jaxrs.model.Details;
import org.folio.rest.jaxrs.model.Eresource;
import org.folio.rest.jaxrs.model.Location;
import org.folio.rest.jaxrs.model.PoLine;
import org.folio.rest.jaxrs.model.PurchaseOrder;
import org.folio.rest.jaxrs.model.Vendor;
import org.folio.rest.jaxrs.resource.OrdersResource.PostOrdersResponse;
import org.folio.rest.tools.client.Response;
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
        .thenApply(PostOrdersHelper::verifyAndExtractBody)
        .thenAccept(poBody -> {
          PurchaseOrder po = poBody.mapTo(PurchaseOrder.class);
          String poNumber = po.getPoNumber();
          String poId = po.getId();
          compPO.setPurchaseOrder(po);

          List<PoLine> lines = new ArrayList<>(compPO.getPoLines().size());
          List<CompletableFuture<Void>> futures = new ArrayList<>();
          for (int i = 0; i < compPO.getPoLines().size(); i++) {
            PoLine compPOL = compPO.getPoLines().get(i);
            compPOL.setPurchaseOrderId(poId);
            compPOL.setPoLineNumber(poNumber + "-" + (i + 1));

            futures.add(createPoLine(compPOL)
              .thenAccept(lines::add)
              .exceptionally(t -> {
                future.completeExceptionally(t);
                return null;
              }));
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

  public CompletableFuture<PoLine> createPoLine(PoLine compPOL) {
    CompletableFuture<PoLine> future = new VertxCompletableFuture<>(ctx);

    Cost cost = compPOL.getCost();
    Details details = compPOL.getDetails();
    Eresource eresource = compPOL.getEresource();
    Location location = compPOL.getLocation();
    Vendor vendor = compPOL.getVendor();

    JsonObject line = JsonObject.mapFrom(compPOL);
    List<CompletableFuture<Void>> subObjFuts = new ArrayList<>();

    if (cost != null) {
      JsonObject obj = JsonObject.mapFrom(cost);
      if (!obj.isEmpty()) {
        subObjFuts.add(createSubObj(line, obj, "cost", "/cost")
          .thenAccept(id -> compPOL.getCost().setId(id)));
      }
    }
    if (details != null) {
      JsonObject obj = JsonObject.mapFrom(details);
      if (!obj.isEmpty()) {
        subObjFuts.add(createSubObj(line, obj, "details", "/details")
          .thenAccept(id -> compPOL.getDetails().setId(id)));
      }
    }
    if (eresource != null) {
      JsonObject obj = JsonObject.mapFrom(eresource);
      if (!obj.isEmpty()) {
        subObjFuts.add(createSubObj(line, obj, "eresource", "/eresource")
          .thenAccept(id -> compPOL.getEresource().setId(id)));
      }
    }
    if (location != null) {
      JsonObject obj = JsonObject.mapFrom(location);
      if (!obj.isEmpty()) {
        subObjFuts.add(createSubObj(line, obj, "location", "/location")
          .thenAccept(id -> compPOL.getLocation().setId(id)));
      }
    }
    if (vendor != null) {
      JsonObject obj = JsonObject.mapFrom(vendor);
      if (!obj.isEmpty()) {
        subObjFuts.add(createSubObj(line, obj, "vendor", "/vendor_detail")
          .thenAccept(id -> compPOL.getVendor().setId(id)));
      }
    }

    CompletableFuture.allOf(subObjFuts.toArray(new CompletableFuture[subObjFuts.size()]))
      .thenAccept(v -> {
        try {
          Buffer polBuf = JsonObject.mapFrom(line).toBuffer();
          httpClient.request(HttpMethod.POST, polBuf, "/po_line", okapiHeaders)
            .thenApply(PostOrdersHelper::verifyAndExtractBody)
            .thenAccept(body -> {
              logger.info("response from /po_line: " + body.encodePrettily());

              compPOL.setId(body.getString("id"));
              future.complete(compPOL);
            })
            .exceptionally(t -> {
              future.completeExceptionally(t);
              return null;
            });
        } catch (Exception e) {
          logger.error("Exception calling POST /po_line", e);
          future.completeExceptionally(e);
        }
      });
    return future;
  }

  public CompletableFuture<String> createSubObj(JsonObject pol, JsonObject obj, String field, String url) {
    CompletableFuture<String> future = new VertxCompletableFuture<>(ctx);

    try {
      httpClient.request(HttpMethod.POST, obj.toBuffer(), url, okapiHeaders)
        .thenApply(PostOrdersHelper::verifyAndExtractBody)
        .thenAccept(body -> {
          String id = JsonObject.mapFrom(body).getString("id");
          pol.put(field, id);
          logger.info("POL after " + field + ": " + pol.encodePrettily());
          future.complete(id);
        })
        .exceptionally(t -> {
          future.completeExceptionally(t);
          return null;
        });
    } catch (Exception e) {
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

  public static JsonObject verifyAndExtractBody(Response response) {
    if (!Response.isSuccess(response.getCode())) {
      throw new CompletionException(
          new HttpException(response.getCode(), response.getError().getString("errorMessage")));
    }

    return response.getBody();
  }

}
