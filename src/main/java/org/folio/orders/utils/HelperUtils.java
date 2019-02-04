package org.folio.orders.utils;

import static java.util.Objects.nonNull;
import static org.apache.commons.lang3.StringUtils.EMPTY;
import static org.apache.commons.lang3.StringUtils.isEmpty;
import static org.folio.orders.utils.HelperUtils.encodeQuery;
import static org.folio.orders.utils.ResourcePathResolver.*;
import static org.folio.rest.RestVerticle.OKAPI_HEADER_TENANT;
import static org.folio.rest.RestVerticle.OKAPI_HEADER_TOKEN;

import java.io.UnsupportedEncodingException;
import java.net.URLEncoder;
import java.nio.charset.StandardCharsets;
import java.util.ArrayList;
import java.util.List;
import java.util.Map;
import java.util.Optional;
import java.util.concurrent.CompletableFuture;
import java.util.concurrent.CompletionException;
import java.util.regex.Matcher;
import java.util.regex.Pattern;

import org.apache.commons.lang3.StringUtils;
import org.folio.orders.rest.exceptions.HttpException;
import org.folio.rest.client.ConfigurationsClient;
import org.folio.rest.jaxrs.model.Adjustment;
import org.folio.rest.jaxrs.model.CompositePoLine;
import org.folio.rest.jaxrs.model.Eresource;
import org.folio.rest.jaxrs.model.Location;
import org.folio.rest.tools.client.Response;
import org.folio.rest.tools.client.interfaces.HttpClientInterface;

import io.vertx.core.Context;
import io.vertx.core.http.HttpMethod;
import io.vertx.core.json.JsonArray;
import io.vertx.core.json.JsonObject;
import io.vertx.core.logging.Logger;
import me.escoffier.vertx.completablefuture.VertxCompletableFuture;

public class HelperUtils {

  public static final String COMPOSITE_PO_LINES = "compositePoLines";

  public static final String PO_NUMBER_ALREADY_EXISTS = "PO Number already exists";
  public static final String DEFAULT_POLINE_LIMIT = "1";
  private static final String MAX_POLINE_LIMIT = "500";
  public static final String OKAPI_URL = "X-Okapi-Url";
  public static final String PO_LINES_LIMIT_PROPERTY = "poLines-limit";
  public static final String URL_WITH_LANG_PARAM = "%s?lang=%s";
  public static final String GET_ALL_POLINES_QUERY_WITH_LIMIT = resourcesPath(PO_LINES)+"?limit=%s&query=purchase_order_id==%s&lang=%s";
  public static final String GET_PURCHASE_ORDER_BYID = resourceByIdPath(PURCHASE_ORDER)+URL_WITH_LANG_PARAM;
  public static final String GET_PURCHASE_ORDER_BYPONUMBER_QUERY = resourcesPath(PURCHASE_ORDER)+"?query=po_number==%s&lang=%s";


  private static final int DEFAULT_PORT = 9130;
  private static final String EXCEPTION_CALLING_ENDPOINT_MSG = "Exception calling {} {}";
  private static final String QUERY = "module=ORDERS";
  private static final Pattern HOST_PORT_PATTERN = Pattern.compile("https?://([^:/]+)(?::?(\\d+)?)");

  private HelperUtils() {

  }

  public static JsonObject verifyAndExtractBody(Response response) {
    if (!Response.isSuccess(response.getCode())) {
      throw new CompletionException(
          new HttpException(response.getCode(), response.getError().getString("errorMessage")));
    }

    return response.getBody();
  }

  public static Adjustment calculateAdjustment(List<CompositePoLine> lines) {
    Adjustment ret = null;
    for (CompositePoLine line : lines) {
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

  public static CompletableFuture<JsonObject> getPurchaseOrderById(String id, String lang, HttpClientInterface httpClient, Context ctx,
                                                               Map<String, String> okapiHeaders, Logger logger) {
    String endpoint = String.format(GET_PURCHASE_ORDER_BYID, id, lang);
    return handleGetRequest(endpoint, httpClient, ctx, okapiHeaders, logger);
  }

  public static CompletableFuture<JsonObject> getPurchaseOrderByPONumber(String poNumber, String lang, HttpClientInterface httpClient, Context ctx,
      Map<String, String> okapiHeaders, Logger logger) {
      String endpoint = String.format(GET_PURCHASE_ORDER_BYPONUMBER_QUERY, poNumber, lang);
      return handleGetRequest(endpoint, httpClient, ctx, okapiHeaders, logger);
  }


  /**
   *  Retrieves PO lines from storage by PO id as JsonObject with array of po_lines (/acq-models/mod-orders-storage/schemas/po_line.json objects)
   */
  public static CompletableFuture<JsonObject> getPoLines(String id, String lang, HttpClientInterface httpClient, Context ctx,
                                                          Map<String, String> okapiHeaders, Logger logger) {
    String endpoint = String.format(GET_ALL_POLINES_QUERY_WITH_LIMIT, MAX_POLINE_LIMIT, id, lang);
    return handleGetRequest(endpoint, httpClient, ctx, okapiHeaders, logger);
  }

  /**
   * Retrieves PO line from storage by PO line id as JsonObject (/acq-models/mod-orders-storage/schemas/po_line.json object)
   */
  public static CompletableFuture<JsonObject> getPoLineById(String lineId, String lang, HttpClientInterface httpClient, Context ctx,
                                                            Map<String, String> okapiHeaders, Logger logger) {
    String endpoint = String.format(URL_WITH_LANG_PARAM, resourceByIdPath(PO_LINES, lineId), lang);
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
        throw new CompletionException(t.getCause());
      });
  }

  public static CompletableFuture<JsonObject> deletePoLine(JsonObject line, HttpClientInterface httpClient, Context ctx,
                                                           Map<String, String> okapiHeaders, Logger logger) {
    return operateOnPoLine(HttpMethod.DELETE, line, httpClient, ctx, okapiHeaders, logger)
      .thenCompose(poline -> {
        String polineId = poline.getId();
        return operateOnSubObj(HttpMethod.DELETE, resourceByIdPath(PO_LINES, polineId), httpClient, ctx, okapiHeaders, logger);
      });
  }

  public static CompletableFuture<List<CompositePoLine>> getCompositePoLines(String id, String lang, HttpClientInterface httpClient,
                                                                    Context ctx, Map<String, String> okapiHeaders, Logger logger) {
    CompletableFuture<List<CompositePoLine>> future = new VertxCompletableFuture<>(ctx);

    getPoLines(id,lang, httpClient,ctx, okapiHeaders, logger)
      .thenAccept(body -> {
        List<CompositePoLine> lines = new ArrayList<>();
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
        future.completeExceptionally(t);
        return null;
      });
    return future;
  }

  public static CompletableFuture<CompositePoLine> operateOnPoLine(HttpMethod operation, JsonObject line, HttpClientInterface httpClient,
                                                                   Context ctx, Map<String, String> okapiHeaders, Logger logger) {
    CompletableFuture<CompositePoLine> future = new VertxCompletableFuture<>(ctx);

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
    futures.add(operateOnSubObjIfPresent(operation, line, SOURCE, httpClient, ctx, okapiHeaders, logger));
    futures.add(operateOnSubObjIfPresent(operation, line, VENDOR_DETAIL, httpClient, ctx, okapiHeaders, logger));
    futures.addAll(operateOnSubObjsIfPresent(operation, line, ALERTS, httpClient, ctx, okapiHeaders, logger));
    futures.addAll(operateOnSubObjsIfPresent(operation, line, CLAIMS, httpClient, ctx, okapiHeaders, logger));
    futures.addAll(operateOnSubObjsIfPresent(operation, line, FUND_DISTRIBUTION, httpClient, ctx, okapiHeaders, logger));
    futures.addAll(operateOnSubObjsIfPresent(operation, line, REPORTING_CODES, httpClient, ctx, okapiHeaders, logger));

    CompletableFuture.allOf(futures.toArray(new CompletableFuture[0]))
      .thenAccept(v -> {
        if (logger.isDebugEnabled()) {
          logger.debug("The PO line after {} operation on sub-objects: {}", operation, line.encodePrettily());
        }
        future.complete(line.mapTo(CompositePoLine.class));
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
      .forEach(fieldId -> futures.add(operateOnSubObj(operation, resourceByIdPath(field) + fieldId, httpClient, ctx, okapiHeaders, logger)
                .thenAccept(value -> {
                  if (value != null && !value.isEmpty()) {
                    array.add(value);
                  }
                })));
    pol.put(field, array);
    return futures;
  }

  private static CompletableFuture<Void> operateOnSubObjIfPresent(HttpMethod operation, JsonObject pol, String field, HttpClientInterface httpClient,
                                                                  Context ctx, Map<String, String> okapiHeaders, Logger logger) {
    String id = (String) pol.remove(field);
    if (id != null) {
      return operateOnSubObj(operation, resourceByIdPath(field, id), httpClient, ctx, okapiHeaders, logger)
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
    return operateOnSubObj(operation, url, null, httpClient, ctx, okapiHeaders, logger);
  }

  public static CompletableFuture<JsonObject> operateOnSubObj(HttpMethod operation, String url, JsonObject body,
      HttpClientInterface httpClient, Context ctx, Map<String, String> okapiHeaders, Logger logger) {
    CompletableFuture<JsonObject> future = new VertxCompletableFuture<>(ctx);

    logger.info("Calling {} {}", operation, url);

    try {
      httpClient.request(operation, (body != null ? body.toBuffer() : null), url, okapiHeaders)
        .thenApply(response -> {
          /*
           * In case there was failed attempt to delete order or particular PO line, the sub-objects might be already partially deleted.
           * This check allows user to retrieve order/line again and retry DELETE operation if required
           */
          int code = response.getCode();
          if (code == 404 && operation == HttpMethod.GET) {
            String errorMessage = (response.getError() != null) ? response.getError().getString("errorMessage") : StringUtils.EMPTY;
            logger.error("The {} {} operation completed with {} error: {}", operation, url, code, errorMessage);

            return new JsonObject();
          }

          return verifyAndExtractBody(response);
        })
        .thenAccept(json -> {
          if (json != null) {
            if (!json.isEmpty() && logger.isInfoEnabled()) {
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

  /**
   * @param query string representing CQL query
   * @param logger {@link Logger} to log error if any
   * @return URL encoded string
   */
  public static String encodeQuery(String query, Logger logger) {
    try {
      return URLEncoder.encode(query, StandardCharsets.UTF_8.toString());
    } catch (UnsupportedEncodingException e) {
      logger.error("Error happened while attempting to encode '{}'", e, query);
      throw new CompletionException(e);
    }
  }

  public static String getEndpointWithQuery(String query, Logger logger) {
    return isEmpty(query) ? EMPTY : "&query=" + encodeQuery(query, logger);
  }

  /**
   * The quantity is based on Order Format (please see MODORDERS-117):<br/>
   * If format equals Physical the associated quantities will result in item records<br/>
   * If format equals Other the associated quantities will NOT result in item records<br/>
   * If format = Electronic and Create Item = True, the associated electronic quantities will result in item records being created in inventory<br/>
   * If format = Electronic and Create Item = False, the associated electronic quantities will NOT result in item records being created in inventory
   *
   * @param compPOL composite PO Line
   * @return quantity of items expected in the inventory for PO Line
   */
  public static int calculateInventoryItemsQuantity(CompositePoLine compPOL) {
    switch (compPOL.getOrderFormat()) {
      case P_E_MIX:
        return getPhysicalQuantity(compPOL) + getElectronicQuantity(compPOL);
      case PHYSICAL_RESOURCE:
        return getPhysicalQuantity(compPOL);
      case ELECTRONIC_RESOURCE:
        return getElectronicQuantity(compPOL);
      case OTHER:
      default:
        return 0;
    }
  }

  private static int getPhysicalQuantity(CompositePoLine compPOL) {
    return Optional.ofNullable(compPOL.getLocation())
                   .map(Location::getQuantityPhysical)
                   .orElse(0);
  }

  private static int getElectronicQuantity(CompositePoLine compPOL) {
    boolean createItems = Optional.ofNullable(compPOL.getEresource())
                                  .map(Eresource::getCreateInventory)
                                  .orElse(false);
    if (createItems) {
      return Optional.ofNullable(compPOL.getLocation())
                     .map(Location::getQuantityElectronic)
                     .orElse(0);
    } else {
      return 0;
    }
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
          logger.debug("Validating response for GET {}", endpoint);
          return verifyAndExtractBody(response);
        })
        .thenAccept(body -> {
          if (logger.isDebugEnabled()) {
            logger.debug("The response body for GET {}: {}", endpoint, nonNull(body) ? body.encodePrettily() : null);
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
   * @param okapiHeaders the headers provided by okapi
   * @param ctx the context
   * @param logger logger instance
   * @return CompletableFuture with JsonObject
   */
  public static CompletableFuture<JsonObject> loadConfiguration(Map<String, String> okapiHeaders, Context ctx, Logger logger) {

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

}
