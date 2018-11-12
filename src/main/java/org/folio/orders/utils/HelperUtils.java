package org.folio.orders.utils;

import java.io.IOException;
import java.nio.file.Files;
import java.nio.file.Paths;
import java.util.ArrayList;
import java.util.List;
import java.util.Map;
import java.util.concurrent.CompletableFuture;
import java.util.concurrent.CompletionException;
import java.util.stream.Stream;

import io.vertx.core.http.HttpMethod;
import io.vertx.core.json.JsonArray;
import me.escoffier.vertx.completablefuture.VertxCompletableFuture;
import org.apache.commons.io.IOUtils;
import org.folio.orders.rest.exceptions.HttpException;
import org.folio.rest.jaxrs.model.Adjustment;
import org.folio.rest.jaxrs.model.PoLine;
import org.folio.rest.tools.client.Response;
import io.vertx.core.Context;
import org.apache.log4j.Logger;

import io.vertx.core.json.JsonObject;
import org.folio.rest.tools.client.interfaces.HttpClientInterface;

public class HelperUtils {

  private HelperUtils() {

  }

  public static String getMockData(String path) throws IOException {
    try {
      return IOUtils.toString(HelperUtils.class.getClassLoader().getResourceAsStream(path));
    } catch (NullPointerException e) {
      StringBuilder sb = new StringBuilder();
      try (Stream<String> lines = Files.lines(Paths.get(path))) {
        lines.forEach(sb::append);
      }
      return sb.toString();
    }
  }

  public static JsonObject verifyAndExtractBody(Response response) {
    if (!Response.isSuccess(response.getCode())) {
      throw new CompletionException(
          new HttpException(response.getCode(), response.getError().getString("errorMessage")));
    }

    return response.getBody();
  }

  public static Adjustment calculateAdjustment(List<PoLine> lines) {
    Adjustment ret = null;
    for (int i = 0; i < lines.size(); i++) {
      Adjustment a = lines.get(i).getAdjustment();
      if (a != null) {
        if (ret == null) {
          ret = a;
        } else {
          ret.setCredit(accumulate(ret.getCredit(), a.getCredit()));
          ret.setDiscount(accumulate(ret.getDiscount(), a.getDiscount()));
          ret.setInsurance(accumulate(ret.getInsurance(), a.getInsurance()));
          ret.setOverhead(accumulate(ret.getOverhead(), a.getOverhead()));
          ret.setShipment(accumulate(ret.getShipment(), a.getShipment()));
          ret.setTax1(accumulate(ret.getTax1(), a.getTax1()));
          ret.setTax2(accumulate(ret.getTax2(), a.getTax2()));
        }
      }
    }
    return ret;
  }

  private static double accumulate(Double a, Double b) {
    if (a == null && b == null)
      return 0d;
    if (a == null)
      return b;
    if (b == null)
      return a;

    return (a.doubleValue() + b.doubleValue());
  }



  public static CompletableFuture<JsonObject> getPurchaseOrder(String id, String lang, HttpClientInterface httpClient, Context ctx, Map<String, String> okapiHeaders, Logger logger) {
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

  public static CompletableFuture<List<PoLine>> getPoLines(String id, String lang, HttpClientInterface httpClient, Context ctx, Map<String, String> okapiHeaders, Logger logger) {
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
            futures.add(resolvePoLine(line, httpClient, ctx, okapiHeaders, logger)
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

  public static CompletableFuture<PoLine> resolvePoLine(JsonObject line, HttpClientInterface httpClient, Context ctx, Map<String, String> okapiHeaders, Logger logger) {
    CompletableFuture<PoLine> future = new VertxCompletableFuture<>(ctx);

    List<CompletableFuture<Void>> futures = new ArrayList<>();
    futures.add(resolveSubObjIfPresent(line, "adjustment", "/adjustment/", httpClient, ctx, okapiHeaders, logger));
    futures.add(resolveSubObjIfPresent(line, "cost", "/cost/", httpClient, ctx, okapiHeaders, logger));
    futures.add(resolveSubObjIfPresent(line, "details", "/details/", httpClient, ctx, okapiHeaders, logger));
    futures.add(resolveSubObjIfPresent(line, "eresource", "/eresource/", httpClient, ctx, okapiHeaders, logger));
    futures.add(resolveSubObjIfPresent(line, "location", "/location/", httpClient, ctx, okapiHeaders, logger));
    futures.add(resolveSubObjIfPresent(line, "physical", "/physical/", httpClient, ctx, okapiHeaders, logger));
    futures.add(resolveSubObjIfPresent(line, "renewal", "/renewal/", httpClient, ctx, okapiHeaders, logger));
    futures.add(resolveSubObjIfPresent(line, "source", "/source/", httpClient, ctx, okapiHeaders, logger));
    futures.add(resolveSubObjIfPresent(line, "vendor_detail", "/vendor_detail/", httpClient, ctx, okapiHeaders, logger));
    futures.addAll(resolveSubObjsIfPresent(line, "alerts", "/alerts/", httpClient, ctx, okapiHeaders, logger));
    futures.addAll(resolveSubObjsIfPresent(line, "claims", "/claims/", httpClient, ctx, okapiHeaders, logger));
    futures.addAll(resolveSubObjsIfPresent(line, "fund_distribution", "/fund_distribution/", httpClient, ctx, okapiHeaders, logger));

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

  private static List<CompletableFuture<Void>> resolveSubObjsIfPresent(JsonObject pol, String field,
                                                                String baseUrl, HttpClientInterface httpClient, Context ctx, Map<String, String> okapiHeaders, Logger logger) {
    JsonArray array = new JsonArray();
    List<CompletableFuture<Void>> futures = new ArrayList<>();
    ((List<?>) pol.remove(field))
      .forEach(fundDistroId -> futures.add(resolveSubObj(baseUrl + fundDistroId, httpClient, ctx, okapiHeaders, logger)
        .thenAccept(array::add)));
    pol.put(field, array);
    return futures;
  }

  private static CompletableFuture<Void> resolveSubObjIfPresent(JsonObject pol, String field, String baseUrl, HttpClientInterface httpClient, Context ctx, Map<String, String> okapiHeaders, Logger logger) {
    String id = (String) pol.remove(field);
    if (id != null) {
      return resolveSubObj(baseUrl + id, httpClient, ctx, okapiHeaders, logger).thenAccept(json -> {
        if (json != null) {
          pol.put(field, json);
        }
      });
    }
    return CompletableFuture.completedFuture(null);
  }

  private static CompletableFuture<JsonObject> resolveSubObj(String url, HttpClientInterface httpClient, Context ctx, Map<String, String> okapiHeaders, Logger logger) {
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
}
