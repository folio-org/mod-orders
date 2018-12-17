package org.folio.orders.utils;

import io.vertx.core.Context;
import io.vertx.core.http.HttpMethod;
import io.vertx.core.json.JsonArray;
import io.vertx.core.json.JsonObject;
import io.vertx.core.logging.Logger;
import me.escoffier.vertx.completablefuture.VertxCompletableFuture;
import org.apache.commons.io.IOUtils;
import org.apache.commons.lang3.StringUtils;
import org.folio.orders.rest.exceptions.HttpException;
import org.folio.rest.client.ConfigurationsClient;
import org.folio.rest.jaxrs.model.Adjustment;
import org.folio.rest.jaxrs.model.PoLine;
import org.folio.rest.tools.client.Response;
import org.folio.rest.tools.client.interfaces.HttpClientInterface;

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
import java.util.regex.Matcher;
import java.util.regex.Pattern;
import java.util.stream.Collectors;
import java.util.stream.Stream;

import static java.util.Objects.nonNull;
import static org.folio.rest.RestVerticle.OKAPI_HEADER_TENANT;
import static org.folio.rest.RestVerticle.OKAPI_HEADER_TOKEN;

public class HelperUtils {


  public static final String OKAPI_URL = "X-Okapi-Url";
  private static final Pattern HOST_PORT_PATTERN = Pattern.compile("https?://([^:/]+)(?::?(\\d+)?)");
  private static final int DEFAULT_PORT = 9130;
  private static final String QUERY = "module=ORDERS";

  private static final String EXCEPTION_CALLING_ENDPOINT_MSG = "Exception calling {} {}";

  public static final String PO_LINES = "po_lines";
  public static final String ALERTS = "alerts";
  public static final String CLAIMS = "claims";
  public static final String SOURCE = "source";
  public static final String RENEWAL = "renewal";
  public static final String REPORTING_CODES = "reporting_codes";
  public static final String ADJUSTMENT = "adjustment";
  public static final String COST = "cost";
  public static final String DETAILS = "details";
  public static final String ERESOURCE = "eresource";
  public static final String LOCATION = "location";
  public static final String PHYSICAL = "physical";
  public static final String VENDOR_DETAIL = "vendor_detail";
  public static final String FUND_DISTRIBUTION = "fund_distribution";


  private static final Map<String,String> subObjectApis = new HashMap<>();
  public static final int DEFAULT_POLINE_LIMIT = 500;
  public static final String PO_LINES_LIMIT_PROPERTY = "poLines-limit";
  public static final String GET_ALL_POLINES_QUERY_WITH_LIMIT = "/po_line?limit=%s&query=purchase_order_id==%s&lang=%s";

  static {
    subObjectApis.put(ADJUSTMENT, "/adjustment/");
    subObjectApis.put(COST,"/cost/");
    subObjectApis.put(DETAILS, "/details/");
    subObjectApis.put(ERESOURCE, "/eresource/");
    subObjectApis.put(LOCATION, "/location/");
    subObjectApis.put(PHYSICAL, "/physical/");
    subObjectApis.put(RENEWAL, "/renewal/");
    subObjectApis.put(SOURCE, "/source/");
    subObjectApis.put(VENDOR_DETAIL, "/vendor_detail/");
    subObjectApis.put(ALERTS, "/alert/");
    subObjectApis.put(CLAIMS, "/claim/");
    subObjectApis.put(REPORTING_CODES, "/reporting_code/");
    subObjectApis.put(FUND_DISTRIBUTION, "/orders-storage/fund_distributions/");
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

  /**
   * If response http code is {@code 404}, empty {@link JsonObject} is returned. Otherwise verifies if response is successful and extracts body
   * In case there was failed attempt to delete PO or particular PO line, the sub-objects might be already partially deleted.
   * This check allows user to retrieve order/line again and retry DELETE operation
   *
   * @param response response to verify
   * @return empty {@link JsonObject} if response http code is {@code 404}, otherwise calls {@link #verifyAndExtractBody(Response)}
   */
  private static JsonObject verifyAndExtractBodyIfFound(Response response) {
    if (response.getCode() == 404) {
      return new JsonObject();
    }
    return verifyAndExtractBody(response);
  }

  public static Adjustment calculateAdjustment(List<PoLine> lines) {
    Adjustment ret = null;
    for (PoLine line : lines) {
      Adjustment a = line.getAdjustment();
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

    return (a + b);
  }

  public static CompletableFuture<JsonObject> getPurchaseOrder(String id, String lang, HttpClientInterface httpClient, Context ctx, Map<String, String> okapiHeaders, Logger logger) {
    String endpoint = String.format("/purchase_order/%s?lang=%s", id, lang);
    return handleGetRequest(endpoint, httpClient, ctx, okapiHeaders, logger);
  }

  /**
   *  Retrieves PO lines from storage by PO id as JsonObject with array of po_lines (/acq-models/mod-orders-storage/schemas/po_line.json objects)
   */
  public static CompletableFuture<JsonObject> getPoLines(String id, String lang, HttpClientInterface httpClient, Context ctx, Map<String, String> okapiHeaders, Logger logger) {
    String endpoint = String.format(GET_ALL_POLINES_QUERY_WITH_LIMIT, DEFAULT_POLINE_LIMIT, id, lang);
    return handleGetRequest(endpoint, httpClient, ctx, okapiHeaders, logger);
  }

  /**
   * Retrieves PO line from storage by PO line id as JsonObject (/acq-models/mod-orders-storage/schemas/po_line.json object)
   */
  public static CompletableFuture<JsonObject> getPoLineById(String lineId, String lang, HttpClientInterface httpClient, Context ctx,
                                                            Map<String, String> okapiHeaders, Logger logger) {
    String endpoint = String.format("%s%s?lang=%s", subObjectApis.get(PO_LINES), lineId, lang);
    return handleGetRequest(endpoint, httpClient, ctx, okapiHeaders, logger);
  }

	public static CompletableFuture<Void> deletePoLines(String orderId, String lang, HttpClientInterface httpClient,
      Context ctx, Map<String, String> okapiHeaders, Logger logger) {

    return getPoLines(orderId, lang, httpClient,ctx, okapiHeaders, logger)
      .thenCompose(body -> {
        List<CompletableFuture<JsonObject>> futures = new ArrayList<>();

        for (int i = 0; i < body.getJsonArray(PO_LINES).size(); i++) {
          JsonObject line = body.getJsonArray(PO_LINES).getJsonObject(i);
          futures.add(deletePoLine(line, httpClient, ctx, okapiHeaders, logger));
        }

        return VertxCompletableFuture.allOf(ctx, futures.toArray(new CompletableFuture[0]));
      })
      .exceptionally(t -> {
        logger.error("Exception deleting po_line data for order id={}:", t, orderId);
        throw new CompletionException(t);
      });
  }

  public static CompletableFuture<JsonObject> deletePoLine(JsonObject line, HttpClientInterface httpClient, Context ctx, Map<String, String> okapiHeaders, Logger logger) {
    return operateOnPoLine(HttpMethod.DELETE, line, httpClient, ctx, okapiHeaders, logger)
      .thenCompose(poline -> {
        String polineId = poline.getId();
        return operateOnSubObj(HttpMethod.DELETE, subObjectApis.get(PO_LINES) + polineId, httpClient, ctx, okapiHeaders, logger);
      });
  }

  public static CompletableFuture<List<PoLine>> getCompositePoLines(String id, String lang, HttpClientInterface httpClient, Context ctx, Map<String, String> okapiHeaders, Logger logger) {
    CompletableFuture<List<PoLine>> future = new VertxCompletableFuture<>(ctx);

    getPoLines(id,lang, httpClient,ctx, okapiHeaders, logger)
      .thenAccept(body -> {
        List<PoLine> lines = new ArrayList<>();
        List<CompletableFuture<Void>> futures = new ArrayList<>();

        for (int i = 0; i < body.getJsonArray(PO_LINES).size(); i++) {
          JsonObject line = body.getJsonArray(PO_LINES).getJsonObject(i);
          futures.add(operateOnPoLine(HttpMethod.GET, line, httpClient, ctx, okapiHeaders, logger)
            .thenAccept(lines::add));
        }

        VertxCompletableFuture.allOf(ctx, futures.toArray(new CompletableFuture[0]))
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

  public static CompletableFuture<PoLine> operateOnPoLine(HttpMethod operation, JsonObject line, HttpClientInterface httpClient, Context ctx, Map<String, String> okapiHeaders, Logger logger) {
    CompletableFuture<PoLine> future = new VertxCompletableFuture<>(ctx);

    if (logger.isDebugEnabled()) {
      logger.debug("The PO line prior to {} operation: {}", operation, line.encodePrettily());
    }

    List<CompletableFuture<Void>> futures = new ArrayList<>();
    futures.add(operateOnSubObjIfPresent(operation, line, ADJUSTMENT, httpClient, ctx, okapiHeaders, logger));
    futures.add(operateOnSubObjIfPresent(operation, line, COST, httpClient, ctx, okapiHeaders, logger));
    futures.add(operateOnSubObjIfPresent(operation, line, DETAILS, httpClient, ctx, okapiHeaders, logger));
    futures.add(operateOnSubObjIfPresent(operation, line, ERESOURCE, httpClient, ctx, okapiHeaders, logger));
    futures.add(operateOnSubObjIfPresent(operation, line, LOCATION, httpClient, ctx, okapiHeaders, logger));
    futures.add(operateOnSubObjIfPresent(operation, line, PHYSICAL, httpClient, ctx, okapiHeaders, logger));
    futures.add(operateOnSubObjIfPresent(operation, line, RENEWAL, httpClient, ctx, okapiHeaders, logger));
    futures.add(operateOnSubObjIfPresent(operation, line, SOURCE, httpClient, ctx, okapiHeaders, logger));
    futures.add(operateOnSubObjIfPresent(operation, line, VENDOR_DETAIL, httpClient, ctx, okapiHeaders, logger));
    futures.addAll(operateOnSubObjsIfPresent(operation, line, ALERTS, httpClient, ctx, okapiHeaders, logger));
    futures.addAll(operateOnSubObjsIfPresent(operation, line, CLAIMS, httpClient, ctx, okapiHeaders, logger));
    futures.addAll(operateOnSubObjsIfPresent(operation, line, REPORTING_CODES, httpClient, ctx, okapiHeaders, logger));
    futures.addAll(operateOnSubObjsIfPresent(operation, line, FUND_DISTRIBUTION, httpClient, ctx, okapiHeaders, logger));

    CompletableFuture.allOf(futures.toArray(new CompletableFuture[0]))
      .thenAccept(v -> {
        if (logger.isDebugEnabled()) {
          logger.debug("The PO line after {} operation on sub-objects: {}", operation, line.encodePrettily());
        }
        future.complete(line.mapTo(PoLine.class));
      })
      .exceptionally(t -> {
        logger.error("Exception resolving one or more po_line sub-object(s) on {} operation:", t, operation);
        future.completeExceptionally(t);
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
      return operateOnSubObj(operation, subObjectApis.get(field) + id, httpClient, ctx, okapiHeaders, logger)
        .thenAccept(json -> {
          if (json != null) {
            if (!json.isEmpty()) {
              pol.put(field, json);
            } else if (HttpMethod.DELETE != operation) {
              logger.warn("The '{}' sub-object with id={} is empty for Order line with id={}", field, id, pol.getString("id"));
            }
          }
        });
    }
    return CompletableFuture.completedFuture(null);
  }

  public static CompletableFuture<JsonObject> operateOnSubObj(HttpMethod operation, String url,
      HttpClientInterface httpClient, Context ctx, Map<String, String> okapiHeaders, Logger logger) {
    CompletableFuture<JsonObject> future = new VertxCompletableFuture<>(ctx);

    logger.info("Calling {} {}", operation, url);

    try {
      httpClient.request(operation, url, okapiHeaders)
        // In case there was failed attempt to delete order or particular PO line, the sub-objects might be already partially deleted.
        .thenApply(HelperUtils::verifyAndExtractBodyIfFound)
        .thenAccept(json -> {
          if (json != null) {
            if (json.isEmpty()) {
              logger.warn("The {} {} operation completed with empty response body", operation, url);
            } else if (logger.isInfoEnabled()) {
              logger.info("The {} {} operation completed with following response body: {}", operation, url, json.encodePrettily());
            }
            future.complete(json);
          } else {
            //Handling the delete API where it sends no response body
            logger.info("The {} {} operation completed with no response body", operation, url);
            future.complete(new JsonObject());
          }
        })
        .exceptionally(t -> {
          logger.error(EXCEPTION_CALLING_ENDPOINT_MSG, t, operation, url);
          future.completeExceptionally(t);
          return null;
        });
    } catch (Exception e) {
      logger.error("Exception performing http request {} {}", e, operation, url);
      future.completeExceptionally(e);
    }

    return future;
  }

  public static CompletableFuture<JsonObject> handleGetRequest(String endpoint, HttpClientInterface
    httpClient, Context ctx, Map<String, String> okapiHeaders,
                                       Logger logger) {
    CompletableFuture<JsonObject> future = new VertxCompletableFuture<>(ctx);
    try {
      logger.debug("Calling GET {}", endpoint);

      httpClient
        .request(HttpMethod.GET, endpoint, okapiHeaders)
        .thenApply(response -> {
          logger.debug("Validating received response");
          return verifyAndExtractBody(response);
        })
        .thenAccept(body -> {
          if (logger.isDebugEnabled()) {
            logger.debug("The response is valid. The response body: {}", nonNull(body) ? body.encodePrettily() : null);
          }
          future.complete(body);
        })
        .exceptionally(t -> {
          logger.error(EXCEPTION_CALLING_ENDPOINT_MSG, t, HttpMethod.GET, endpoint);
          future.completeExceptionally(t);
          return null;
        });
    } catch (Exception e) {
      logger.error(EXCEPTION_CALLING_ENDPOINT_MSG, e, HttpMethod.GET, endpoint);
      future.completeExceptionally(e);
    }
    return future;
  }

  /**
   * Retrieve configuration for mod-orders from mod-configuration.
   * @param okapiHeaders
   * @param ctx the context
   * @param logger
   * @return CompletableFuture with JsonObject
   */
  public static CompletableFuture<JsonObject> loadConfiguration(Map<String, String> okapiHeaders,
                                                          Context ctx, Logger logger) {

    String okapiURL = StringUtils.trimToEmpty(okapiHeaders.get(OKAPI_URL));
    String tenant = okapiHeaders.get(OKAPI_HEADER_TENANT);
    String token = okapiHeaders.get(OKAPI_HEADER_TOKEN);
    CompletableFuture<JsonObject> future = new VertxCompletableFuture<>(ctx);
    JsonObject config = new JsonObject();
    Matcher matcher = HOST_PORT_PATTERN.matcher(okapiURL);
    if (!matcher.find()) {
      future.complete(config);
      return future;
    }

    String host = matcher.group(1);
    String port = matcher.group(2);
    ConfigurationsClient configurationsClient = new ConfigurationsClient(host,
      StringUtils.isNotBlank(port) ? Integer.valueOf(port) : DEFAULT_PORT, tenant, token);

    try {
      configurationsClient.getEntries(QUERY, 0, 7, null, null, response ->
        response.bodyHandler(body -> {

          if (response.statusCode() != 200) {
            logger.error(String.format("Expected status code 200, got '%s' :%s",
              response.statusCode(), body.toString()));
            future.complete(config);
            return;
          }

          JsonObject entries = body.toJsonObject();
          entries.getJsonArray("configs").stream()
            .forEach(o ->
              config.put(((JsonObject) o).getString("configName"),
                ((JsonObject) o).getValue("value")));
          future.complete(config);
        })
      );
    } catch (Exception e) {
      logger.error(e.getMessage());
      future.complete(config);
    }
    return future;
  }


  public static Map<String,String> getSubObjectapisForPost() {
    return subObjectApis.entrySet().stream()
        .collect(Collectors.toMap(Map.Entry::getKey, e-> e.getValue().replaceAll("/$", "")));
  }
}
