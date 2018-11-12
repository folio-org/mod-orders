package org.folio.rest.impl;

import java.util.ArrayList;
import java.util.List;
import java.util.Map;
import java.util.concurrent.CompletableFuture;
import java.util.concurrent.CompletionException;

import javax.ws.rs.core.Response;

import org.apache.log4j.Logger;
import org.folio.orders.rest.exceptions.HttpException;
import org.folio.orders.utils.HelperUtils;
import org.folio.rest.jaxrs.model.CompositePurchaseOrder;
import org.folio.rest.jaxrs.model.PoLine;
import org.folio.rest.jaxrs.resource.OrdersResource.GetOrdersByIdResponse;
import org.folio.rest.tools.client.interfaces.HttpClientInterface;

import io.vertx.core.AsyncResult;
import io.vertx.core.Context;
import io.vertx.core.Future;
import io.vertx.core.Handler;
import io.vertx.core.http.HttpMethod;
import io.vertx.core.json.JsonArray;
import io.vertx.core.json.JsonObject;
import me.escoffier.vertx.completablefuture.VertxCompletableFuture;

public class GetOrdersByIdHelper {

  private static final Logger logger = Logger.getLogger(GetOrdersByIdHelper.class);

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

    getCompositePurchaseOrderById(id, lang)
      .thenAccept(orders -> {
        logger.info("Returning Composite Purchase Order: " + JsonObject.mapFrom(orders).encodePrettily());
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

    HelperUtils.getPurchaseOrder(id, lang, httpClient, ctx, okapiHeaders, logger)
      .thenAccept(po -> {
        logger.info("got: " + po.encodePrettily());
        po.remove("adjustment");
        CompositePurchaseOrder compPO = po.mapTo(CompositePurchaseOrder.class);

        HelperUtils.getPoLines(id, lang, httpClient, ctx, okapiHeaders, logger)
          .thenAccept(poLines -> {
            compPO.setPoLines(poLines);
            compPO.setAdjustment(HelperUtils.calculateAdjustment(poLines));
            future.complete(compPO);
          }).exceptionally(t -> {
            logger.error("Failed to get POLines", t);
            return null;
          });
      }).exceptionally(t -> {
        logger.error("Failed to build composite purchase order", t.getCause());
        future.completeExceptionally(t.getCause());
        return null;
      });

    return future;
  }

  private CompletableFuture<JsonObject> getPurchaseOrder(String id, String lang) {
    CompletableFuture<JsonObject> future = new VertxCompletableFuture<>(ctx);

    try {
      httpClient.request(HttpMethod.GET, String.format("/purchase_order/%s?lang=%s", id, lang), okapiHeaders)
        .thenApply(HelperUtils::verifyAndExtractBody)
        .thenAccept(future::complete)
        .exceptionally(t -> {
          logger.error("Exception calling GET /purchase_order/" + id, t);
          future.completeExceptionally(t);
          return null;
        });
    } catch (Exception e) {
      logger.error("Exception calling GET /purchase_order/" + id, e);
      future.completeExceptionally(e);
    }

    return future;
  }

  private CompletableFuture<List<PoLine>> getPoLines(String id, String lang) {
    CompletableFuture<List<PoLine>> future = new VertxCompletableFuture<>(ctx);

    try {
      httpClient.request(HttpMethod.GET,
          String.format("/po_line?limit=999&query=purchase_order_id==%s&lang=%s", id, lang), okapiHeaders)
        .thenApply(HelperUtils::verifyAndExtractBody)
        .thenAccept(body -> {
          List<PoLine> lines = new ArrayList<>();
          List<CompletableFuture<Void>> futures = new ArrayList<>();

          for (int i = 0; i < body.getJsonArray("po_lines").size(); i++) {
            JsonObject line = body.getJsonArray("po_lines").getJsonObject(i);
            futures.add(resolvePoLine(line)
              .thenAccept(lines::add));
          }

          VertxCompletableFuture.allOf(ctx, futures.toArray(new CompletableFuture[futures.size()]))
            .thenAccept(v -> future.complete(lines))
            .exceptionally(t -> {
              future.completeExceptionally(t.getCause());
              return null;
            });
        })
        .exceptionally(t -> {
          logger.error("Exception gathering po_line data:", t);
          throw new CompletionException(t);
        });
    } catch (Exception e) {
      logger.error("Exception calling GET /po_line/" + id, e);
      future.completeExceptionally(e);
    }

    return future;
  }

  private CompletableFuture<PoLine> resolvePoLine(JsonObject line) {
    CompletableFuture<PoLine> future = new VertxCompletableFuture<>(ctx);

    List<CompletableFuture<Void>> futures = new ArrayList<>();
    futures.add(resolveSubObjIfPresent(line, "adjustment", "/adjustment/"));
    futures.add(resolveSubObjIfPresent(line, "cost", "/cost/"));
    futures.add(resolveSubObjIfPresent(line, "details", "/details/"));
    futures.add(resolveSubObjIfPresent(line, "eresource", "/eresource/"));
    futures.add(resolveSubObjIfPresent(line, "location", "/location/"));
    futures.add(resolveSubObjIfPresent(line, "physical", "/physical/"));
    futures.add(resolveSubObjIfPresent(line, "renewal", "/renewal/"));
    futures.add(resolveSubObjIfPresent(line, "source", "/source/"));
    futures.add(resolveSubObjIfPresent(line, "vendor_detail", "/vendor_detail/"));

    futures.addAll(resolveSubObjsIfPresent(line, "alerts", "/alerts/"));
    futures.addAll(resolveSubObjsIfPresent(line, "claims", "/claims/"));
    futures.addAll(resolveSubObjsIfPresent(line, "fund_distribution", "/fund_distribution/"));

    logger.info(line.encodePrettily());

    CompletableFuture.allOf(futures.toArray(new CompletableFuture[futures.size()]))
      .thenAccept(v -> future.complete(line.mapTo(PoLine.class)))
      .exceptionally(t -> {
        logger.error("Exception resolving one or more po_line sub-object(s):", t);
        future.completeExceptionally(t.getCause());
        return null;
      });
    return future;
  }

  private List<CompletableFuture<Void>> resolveSubObjsIfPresent(JsonObject pol, String field,
      String baseUrl) {
    JsonArray array = new JsonArray();
    List<CompletableFuture<Void>> futures = new ArrayList<>();
    ((List<?>) pol.remove(field))
      .forEach(fundDistroId -> futures.add(resolveSubObj(baseUrl + fundDistroId)
        .thenAccept(array::add)));
    pol.put(field, array);
    return futures;
  }

  private CompletableFuture<Void> resolveSubObjIfPresent(JsonObject pol, String field, String baseUrl) {
    String id = (String) pol.remove(field);
    if (id != null) {
      return resolveSubObj(baseUrl + id).thenAccept(json -> {
        if (json != null) {
          pol.put(field, json);
        }
      });
    }
    return CompletableFuture.completedFuture(null);
  }

  private CompletableFuture<JsonObject> resolveSubObj(String url) {
    CompletableFuture<JsonObject> future = new VertxCompletableFuture<>(ctx);

    logger.info(String.format("calling GET %s", url));

    try {
      httpClient.request(HttpMethod.GET, url, okapiHeaders)
        .thenApply(HelperUtils::verifyAndExtractBody)
        .thenAccept(future::complete)
        .exceptionally(t -> {
          future.completeExceptionally(t);
          return null;
        });
    } catch (Exception e) {
      future.completeExceptionally(e);
    }

    return future;
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
