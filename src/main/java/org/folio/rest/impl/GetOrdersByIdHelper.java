package org.folio.rest.impl;

import java.nio.file.NoSuchFileException;
import java.util.ArrayList;
import java.util.List;
import java.util.Map;
import java.util.concurrent.CompletableFuture;
import java.util.concurrent.CompletionException;

import javax.ws.rs.core.Response;

import org.apache.log4j.Logger;
import org.folio.orders.rest.exceptions.HttpException;
import org.folio.orders.utils.HelperUtils;
import org.folio.rest.jaxrs.model.Adjustment;
import org.folio.rest.jaxrs.model.CompositePurchaseOrder;
import org.folio.rest.jaxrs.model.PoLine;
import org.folio.rest.jaxrs.model.PurchaseOrder;
import org.folio.rest.jaxrs.resource.OrdersResource.GetOrdersByIdResponse;
import org.folio.rest.tools.client.interfaces.HttpClientInterface;

import io.vertx.core.AsyncResult;
import io.vertx.core.Context;
import io.vertx.core.Future;
import io.vertx.core.Handler;
import io.vertx.core.http.HttpMethod;
import io.vertx.core.json.JsonObject;
import me.escoffier.vertx.completablefuture.VertxCompletableFuture;

public class GetOrdersByIdHelper {

  private static final Logger logger = Logger.getLogger(GetOrdersByIdHelper.class);

  public static final String BASE_MOCK_DATA_PATH = "mockdata/";

  private final HttpClientInterface httpClient;
  private final Context ctx;
  private final Handler<AsyncResult<javax.ws.rs.core.Response>> asyncResultHandler;
  private final Map<String, String> okapiHeaders;

  public GetOrdersByIdHelper(HttpClientInterface httpClient, Map<String, String> okapiHeaders,
      Handler<AsyncResult<Response>> asyncResultHandler, Context ctx) {
    this.httpClient = httpClient;
    this.okapiHeaders = okapiHeaders;
    this.ctx = ctx;
    this.asyncResultHandler = asyncResultHandler;
  }

  public CompletableFuture<CompositePurchaseOrder> getOrder(String id, String lang) {
    CompletableFuture<CompositePurchaseOrder> future = new VertxCompletableFuture<>(ctx);

    // TODO replace this with a call to mod-orders-storage
    // getMockOrder(id)
    getCompositePurchaseOrderById(id, lang)
      .thenAccept(orders -> {
        logger.info("Returning mock data: " + JsonObject.mapFrom(orders).encodePrettily());
        future.complete(orders);
      })
      .exceptionally(t -> {
        logger.error("Error getting orders", t);
        future.completeExceptionally(t);
        return null;
      });

    return future;
  }

  private CompletableFuture<CompositePurchaseOrder> getCompositePurchaseOrderById(String id, String lang) {
    CompletableFuture<CompositePurchaseOrder> future = new VertxCompletableFuture<>(ctx);

    CompositePurchaseOrder compPO = new CompositePurchaseOrder();

    CompletableFuture<Void> poFuture = getPurchaseOrder(id, lang)
      .thenAccept(compPO::setPurchaseOrder);

    CompletableFuture<Void> polFuture = getPoLines(id, lang)
      .thenAccept(compPO::setPoLines);

    CompletableFuture.allOf(poFuture, polFuture).thenAccept(v -> {
      future.complete(compPO);
    }).exceptionally(t -> {
      logger.error("Failed to build composite purchase order", t.getCause());
      future.completeExceptionally(t.getCause());
      return null;
    });

    return future;
  }

  private CompletableFuture<PurchaseOrder> getPurchaseOrder(String id, String lang) {
    CompletableFuture<PurchaseOrder> future = new VertxCompletableFuture<>(ctx);

    try {
      httpClient.request(HttpMethod.GET, String.format("/purchase_order/%s?lang=%s", id, lang), okapiHeaders)
        .thenApply(HelperUtils::verifyAndExtractBody)
        .thenAccept(poBody -> {
          PurchaseOrder po = poBody.mapTo(PurchaseOrder.class);
          future.complete(po);
        });
    } catch (Exception e) {
      logger.error("Exception calling GET /purchase_order/" + id, e);
      future.completeExceptionally(e);
    }

    return future;
  }

  private CompletableFuture<List<PoLine>> getPoLines(String id, String lang) {
    CompletableFuture<List<PoLine>> future = new VertxCompletableFuture<>(ctx);

    List<PoLine> lines = new ArrayList<>();
    try {
      httpClient.request(HttpMethod.GET,
          String.format("/po_line?limit=999&query=purchase_order_id==%s&lang=%s", id, lang), okapiHeaders)
        .thenApply(HelperUtils::verifyAndExtractBody)
        .thenAccept(body -> {
          body.getJsonArray("po_lines").forEach(l -> {
            JsonObject line = (JsonObject) l;

            List<CompletableFuture<Void>> futures = new ArrayList<>();
            futures.add(getAdjustmentById((String) line.remove("adjustment"))
              .thenAccept(a -> line.put("adjustment", a)));
            futures.add(getAdjustmentById((String) line.remove("cost"))
              .thenAccept(a -> line.put("cost", a)));
            futures.add(getAdjustmentById((String) line.remove("details"))
              .thenAccept(a -> line.put("details", a)));
            futures.add(getAdjustmentById((String) line.remove("eresource"))
              .thenAccept(a -> line.put("eresource", a)));
            futures.add(getAdjustmentById((String) line.remove("location"))
              .thenAccept(a -> line.put("location", a)));
            futures.add(getAdjustmentById((String) line.remove("physical"))
              .thenAccept(a -> line.put("physical", a)));
            futures.add(getAdjustmentById((String) line.remove("renewal"))
              .thenAccept(a -> line.put("renewal", a)));
            futures.add(getAdjustmentById((String) line.remove("source"))
              .thenAccept(a -> line.put("source", a)));
            futures.add(getAdjustmentById((String) line.remove("vendor_detail"))
              .thenAccept(a -> line.put("vendor_detail", a)));
            
            //TODO handle claims, fund_distribution, alerts
            
            CompletableFuture.allOf(futures.toArray(new CompletableFuture[futures.size()]))
              .thenAccept(v -> {
                lines.add((line).mapTo(PoLine.class));
              })
              .exceptionally(t -> {
                future.completeExceptionally(t.getCause());
                return null;
              });
          });
          future.complete(lines);
        })
        .exceptionally(t -> {
          throw new CompletionException(t);
        });
    } catch (Exception e) {
      logger.error("Exception calling GET /po_line/" + id, e);
      future.completeExceptionally(e);
    }

    return future;
  }

  private CompletableFuture<JsonObject> getAdjustmentById(String id) {
    CompletableFuture<JsonObject> future = new VertxCompletableFuture<JsonObject>(ctx);
    Adjustment ret = new Adjustment();
    ret.setId(id);
    future.complete(JsonObject.mapFrom(ret));
    return future;
  }

  private CompletableFuture<CompositePurchaseOrder> getMockOrder(String id) {
    return VertxCompletableFuture.supplyAsync(ctx, () -> {
      try {
        JsonObject json = new JsonObject(HelperUtils.getMockData(String.format("%s%s.json", BASE_MOCK_DATA_PATH, id)));
        return json.mapTo(CompositePurchaseOrder.class);
      } catch (NoSuchFileException e) {
        logger.error("No such file", e);
        throw new CompletionException(new HttpException(404, id));
      } catch (Exception e) {
        logger.error("Failed to read mock data", e);
        throw new CompletionException(e);
      }
    });
  }

  public Void handleError(Throwable throwable) {
    final Future<javax.ws.rs.core.Response> result;

    logger.error("Exception querying for orders", throwable.getCause());

    final Throwable t = throwable.getCause();
    if (t instanceof HttpException) {
      final int code = ((HttpException) t).getCode();
      final String message = ((HttpException) t).getMessage();
      switch (code) {
      case 404:
        result = Future.succeededFuture(GetOrdersByIdResponse.withPlainNotFound(message));
        break;
      case 500:
        result = Future.succeededFuture(GetOrdersByIdResponse.withPlainInternalServerError(message));
        break;
      default:
        result = Future.succeededFuture(GetOrdersByIdResponse.withPlainInternalServerError(message));
      }
    } else {
      result = Future.succeededFuture(GetOrdersByIdResponse.withPlainInternalServerError(throwable.getMessage()));
    }

    httpClient.closeClient();

    asyncResultHandler.handle(result);

    return null;
  }

}
