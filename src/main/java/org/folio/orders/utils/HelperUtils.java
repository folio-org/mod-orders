package org.folio.orders.utils;

import java.io.IOException;
import java.nio.charset.StandardCharsets;
import java.nio.file.Files;
import java.nio.file.Paths;
import java.util.ArrayList;
import java.util.HashMap;
import java.util.List;
import java.util.Map;
import java.util.concurrent.CompletableFuture;
import java.util.concurrent.CompletionException;
import java.util.stream.Stream;

import org.apache.commons.io.IOUtils;
import org.folio.orders.rest.exceptions.HttpException;
import org.folio.rest.jaxrs.model.Adjustment;
import org.folio.rest.jaxrs.model.PoLine;
import org.folio.rest.tools.client.Response;
import org.folio.rest.tools.client.interfaces.HttpClientInterface;

import io.vertx.core.Context;
import io.vertx.core.http.HttpMethod;
import io.vertx.core.json.JsonArray;
import io.vertx.core.json.JsonObject;
import io.vertx.core.logging.Logger;
import me.escoffier.vertx.completablefuture.VertxCompletableFuture;

public class HelperUtils {
  private static final String PO_LINES = "po_lines";
  static final Map<String,String> subObjectApis=new HashMap<>();
  static {
    subObjectApis.put("adjustment", "/adjustment/"); 
    subObjectApis.put("cost","/cost/");
    subObjectApis.put("details", "/details/");
    subObjectApis.put("eresource", "/eresource/");
    subObjectApis.put("location", "/location/");
    subObjectApis.put("physical", "/physical/");
    subObjectApis.put("renewal", "/renewal/");
    subObjectApis.put("source", "/source/");
    subObjectApis.put("vendor_detail", "/vendor_detail/");
    subObjectApis.put("alerts", "/alerts/");
    subObjectApis.put("claims", "/claims/");
    subObjectApis.put("fund_distribution", "/fund_distribution/");
    subObjectApis.put(PO_LINES, "/po_line/");
  }

  private HelperUtils() {

  }

  public static String getMockData(String path) throws IOException {
    try {
      return IOUtils.toString(HelperUtils.class.getClassLoader().getResourceAsStream(path), StandardCharsets.UTF_8);
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
  
  public static CompletableFuture<JsonObject> getPoLine(String id, String lang, HttpClientInterface httpClient, Context ctx, Map<String, String> okapiHeaders, Logger logger) {
    CompletableFuture<JsonObject> future = new VertxCompletableFuture<>(ctx);

    try {
      httpClient.request(HttpMethod.GET,
          String.format("/po_line?limit=999&query=purchase_order_id==%s&lang=%s", id, lang), okapiHeaders)
          .thenApply(HelperUtils::verifyAndExtractBody)
          .thenAccept(future::complete)
        .exceptionally(t -> {
          logger.error("Exception calling GET /po_line/" + id, t);
          future.completeExceptionally(t);
          return null;
        });
    } catch (Exception e) {
      logger.error("Exception calling GET /po_line/" + id, e);
      future.completeExceptionally(e);
    }

    return future;
  }

  public static CompletableFuture<List<Void>> deletePoLines(String id, String lang, HttpClientInterface httpClient, Context ctx, Map<String, String> okapiHeaders, Logger logger) {
	  CompletableFuture<List<Void>> future = new VertxCompletableFuture<>(ctx);

      getPoLine(id,lang, httpClient,ctx, okapiHeaders, logger)
        .thenAccept(body -> {
          List<CompletableFuture<Void>> futures = new ArrayList<>();
          List<CompletableFuture<JsonObject>> lineFuture=new ArrayList<>();

          for (int i = 0; i < body.getJsonArray(PO_LINES).size(); i++) {
            JsonObject line = body.getJsonArray(PO_LINES).getJsonObject(i);
            futures.add(resolvePoLine(HttpMethod.DELETE, line, httpClient, ctx, okapiHeaders, logger)
            		.thenAccept(poline->{
                            String polineId = poline.getId();
                            lineFuture.add(operateOnSubObj(HttpMethod.DELETE,subObjectApis.get(PO_LINES) + polineId, httpClient, ctx, okapiHeaders, logger));
                      })
            		);
          }
          
          VertxCompletableFuture.allOf(ctx, futures.toArray(new CompletableFuture[futures.size()]))
          .thenAccept(v -> {CompletableFuture.allOf(lineFuture.toArray(new CompletableFuture[lineFuture.size()]))
            	              .thenAccept(n->future.complete(null))
            	              .exceptionally(e -> {logger.error("Exception deleting po_lines:", e);
            	                                   future.completeExceptionally(e.getCause());
            	                                   return null;});
        	                })
          .exceptionally(t -> {
        	logger.error("Exception deleting po_line data:", t);
            future.completeExceptionally(t.getCause());
            return null;
          });
        })
        .exceptionally(t -> {
          logger.error("Exception fetching po_line data:", t);
          throw new CompletionException(t);
        });

    return future;
  }

  public static CompletableFuture<List<PoLine>> getPoLines(String id, String lang, HttpClientInterface httpClient, Context ctx, Map<String, String> okapiHeaders, Logger logger) {
    CompletableFuture<List<PoLine>> future = new VertxCompletableFuture<>(ctx);
  
      getPoLine(id,lang, httpClient,ctx, okapiHeaders, logger)
        .thenAccept(body -> {
          List<PoLine> lines = new ArrayList<>();
          List<CompletableFuture<Void>> futures = new ArrayList<>();

          for (int i = 0; i < body.getJsonArray(PO_LINES).size(); i++) {
            JsonObject line = body.getJsonArray(PO_LINES).getJsonObject(i);
            futures.add(resolvePoLine(HttpMethod.GET, line, httpClient, ctx, okapiHeaders, logger)
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
    return future;
  }

  public static CompletableFuture<PoLine> resolvePoLine(HttpMethod operation,JsonObject line, HttpClientInterface httpClient, Context ctx, Map<String, String> okapiHeaders, Logger logger) {
    CompletableFuture<PoLine> future = new VertxCompletableFuture<>(ctx);

    List<CompletableFuture<Void>> futures = new ArrayList<>();
    futures.add(operateOnSubObjIfPresent(operation, line, "adjustment", httpClient, ctx, okapiHeaders, logger));
    futures.add(operateOnSubObjIfPresent(operation, line, "cost", httpClient, ctx, okapiHeaders, logger));
    futures.add(operateOnSubObjIfPresent(operation, line, "details", httpClient, ctx, okapiHeaders, logger));
    futures.add(operateOnSubObjIfPresent(operation, line, "eresource", httpClient, ctx, okapiHeaders, logger));
    futures.add(operateOnSubObjIfPresent(operation, line, "location", httpClient, ctx, okapiHeaders, logger));
    futures.add(operateOnSubObjIfPresent(operation, line, "physical", httpClient, ctx, okapiHeaders, logger));
    futures.add(operateOnSubObjIfPresent(operation, line, "renewal", httpClient, ctx, okapiHeaders, logger));
    futures.add(operateOnSubObjIfPresent(operation, line, "source", httpClient, ctx, okapiHeaders, logger));
    futures.add(operateOnSubObjIfPresent(operation, line, "vendor_detail", httpClient, ctx, okapiHeaders, logger));
    futures.addAll(operateOnSubObjsIfPresent(operation, line, "alerts", httpClient, ctx, okapiHeaders, logger));
    futures.addAll(operateOnSubObjsIfPresent(operation, line, "claims", httpClient, ctx, okapiHeaders, logger));
    futures.addAll(operateOnSubObjsIfPresent(operation, line, "fund_distribution", httpClient, ctx, okapiHeaders, logger));

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

  private static List<CompletableFuture<Void>> operateOnSubObjsIfPresent(HttpMethod operation, JsonObject pol, String field,
                                                                HttpClientInterface httpClient, Context ctx, Map<String, String> okapiHeaders, Logger logger) {
    JsonArray array = new JsonArray();
    List<CompletableFuture<Void>> futures = new ArrayList<>();
    ((List<?>) pol.remove(field))
      .forEach(fundDistroId -> futures.add(operateOnSubObj(operation, subObjectApis.get(field) + fundDistroId, httpClient, ctx, okapiHeaders, logger)
                .thenAccept(array::add)));
    pol.put(field, array);
    return futures;
  }

  private static CompletableFuture<Void> operateOnSubObjIfPresent(HttpMethod operation, JsonObject pol, String field, HttpClientInterface httpClient, Context ctx, Map<String, String> okapiHeaders, Logger logger) {
    String id = (String) pol.remove(field);
    if (id != null) {
      return operateOnSubObj(operation, subObjectApis.get(field) + id, httpClient, ctx, okapiHeaders, logger).thenAccept(json -> {
        if (json != null) {
          pol.put(field, json);
        }
      });
    }
    return CompletableFuture.completedFuture(null);
  }
  
  public static CompletableFuture<JsonObject> operateOnSubObj(HttpMethod operation, String url, HttpClientInterface httpClient, Context ctx, Map<String, String> okapiHeaders, Logger logger){
    CompletableFuture<JsonObject> future = new VertxCompletableFuture<>(ctx);

    logger.info(String.format("calling %s %s", operation.toString(), url));

    try {
      httpClient.request(operation, url, okapiHeaders)
        .thenApply(HelperUtils::verifyAndExtractBody)
        .thenAccept(json->{
          if(json!=null)
           future.complete(json);
          else{
            //Handling the delete API where it sends no response body
            future.complete(new JsonObject());
          }
        })
        .exceptionally(t -> {
          logger.error(String.format("Exception calling %s %s %s",operation.toString(), url, t));
          future.completeExceptionally(t);
          return null;
        });
    } catch (Exception e) {
    	logger.error(String.format("Exception calling %s %s %s",operation.toString(), url, e));
      future.completeExceptionally(e);
    }

    return future;
  }
}
