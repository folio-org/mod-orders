package org.folio.orders.utils;

import static java.util.Objects.nonNull;
import static java.util.stream.Collectors.toList;
import static org.apache.commons.lang3.ObjectUtils.defaultIfNull;
import static org.apache.commons.lang3.StringUtils.EMPTY;
import static org.apache.commons.lang3.StringUtils.isEmpty;
import static org.folio.orders.utils.ResourcePathResolver.*;
import static org.folio.rest.RestVerticle.OKAPI_HEADER_TENANT;
import static org.folio.rest.RestVerticle.OKAPI_HEADER_TOKEN;
import static org.folio.rest.jaxrs.model.CompositePoLine.OrderFormat.ELECTRONIC_RESOURCE;
import static org.folio.rest.jaxrs.model.CompositePoLine.OrderFormat.PHYSICAL_RESOURCE;
import static org.folio.rest.jaxrs.model.CompositePoLine.OrderFormat.P_E_MIX;

import java.io.UnsupportedEncodingException;
import java.math.BigDecimal;
import java.math.RoundingMode;
import java.net.URLEncoder;
import java.nio.charset.StandardCharsets;
import java.util.ArrayList;
import java.util.Collections;
import java.util.Currency;
import java.util.List;
import java.util.Map;
import java.util.Objects;
import java.util.Optional;
import java.util.concurrent.CompletableFuture;
import java.util.concurrent.CompletionException;
import java.util.regex.Matcher;
import java.util.regex.Pattern;
import java.util.stream.Collectors;

import one.util.streamex.StreamEx;
import org.apache.commons.collections4.CollectionUtils;
import org.apache.commons.lang3.ObjectUtils;
import org.apache.commons.lang3.StringUtils;
import org.folio.orders.rest.exceptions.HttpException;
import org.folio.rest.acq.model.Piece;
import org.folio.rest.client.ConfigurationsClient;
import org.folio.rest.jaxrs.model.Cost;
import org.folio.rest.jaxrs.model.CompositePoLine;
import org.folio.rest.jaxrs.model.CompositePurchaseOrder;
import org.folio.rest.jaxrs.model.Eresource;
import org.folio.rest.jaxrs.model.Error;
import org.folio.rest.jaxrs.model.Location;
import org.folio.rest.jaxrs.model.Parameter;
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

  public static final String DEFAULT_POLINE_LIMIT = "1";
  private static final String MAX_POLINE_LIMIT = "500";
  public static final String OKAPI_URL = "X-Okapi-Url";
  public static final String PO_LINES_LIMIT_PROPERTY = "poLines-limit";
  public static final String URL_WITH_LANG_PARAM = "%s?lang=%s";
  public static final String GET_ALL_POLINES_QUERY_WITH_LIMIT = resourcesPath(PO_LINES) + "?limit=%s&query=purchaseOrderId==%s&lang=%s";
  private static final String GET_PURCHASE_ORDER_BYID = resourceByIdPath(PURCHASE_ORDER) + URL_WITH_LANG_PARAM;
  private static final String GET_PURCHASE_ORDER_BYPONUMBER_QUERY = resourcesPath(PURCHASE_ORDER) + "?query=poNumber==%s&lang=%s";


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
   *  Retrieves PO lines from storage by PO id as JsonArray of poLines (/acq-models/mod-orders-storage/schemas/po_line.json objects)
   */
  public static CompletableFuture<JsonArray> getPoLines(String id, String lang, HttpClientInterface httpClient, Context ctx,
                                                          Map<String, String> okapiHeaders, Logger logger) {
    String endpoint = String.format(GET_ALL_POLINES_QUERY_WITH_LIMIT, MAX_POLINE_LIMIT, id, lang);
    return handleGetRequest(endpoint, httpClient, ctx, okapiHeaders, logger)
      .thenApply(body -> body.getJsonArray(PO_LINES));
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
      .thenCompose(jsonArray -> {
        List<CompletableFuture<JsonObject>> futures = new ArrayList<>();

        for (int i = 0; i < jsonArray.size(); i++) {
          JsonObject line = jsonArray.getJsonObject(i);
          futures.add(deletePoLine(line, httpClient, ctx, okapiHeaders, logger));
        }

        return VertxCompletableFuture.allOf(ctx, futures.toArray(new CompletableFuture[0]));
      })
      .exceptionally(t -> {
        logger.error("Exception deleting poLine data for order id={}:", t, orderId);
        throw new CompletionException(t.getCause());
      });
  }

  public static CompletableFuture<JsonObject> deletePoLine(JsonObject line, HttpClientInterface httpClient, Context ctx,
                                                           Map<String, String> okapiHeaders, Logger logger) {
    return operateOnPoLine(HttpMethod.DELETE, line, httpClient, ctx, okapiHeaders, logger)
      .thenCompose(poline -> {
        String polineId = poline.getId();
        return operateOnObject(HttpMethod.DELETE, resourceByIdPath(PO_LINES, polineId), httpClient, ctx, okapiHeaders, logger);
      });
  }

  public static CompletableFuture<List<CompositePoLine>> getCompositePoLines(String id, String lang, HttpClientInterface httpClient,
                                                                    Context ctx, Map<String, String> okapiHeaders, Logger logger) {
    CompletableFuture<List<CompositePoLine>> future = new VertxCompletableFuture<>(ctx);

    getPoLines(id,lang, httpClient,ctx, okapiHeaders, logger)
      .thenAccept(jsonArray -> {
        List<CompletableFuture<CompositePoLine>> futures = new ArrayList<>();

        for (int i = 0; i < jsonArray.size(); i++) {
          JsonObject line = jsonArray.getJsonObject(i);
          futures.add(operateOnPoLine(HttpMethod.GET, line, httpClient, ctx, okapiHeaders, logger));
        }

        collectResultsOnSuccess(futures)
          .thenAccept(future::complete)
          .exceptionally(t -> {
            future.completeExceptionally(t.getCause());
            return null;
          });
      })
      .exceptionally(t -> {
        logger.error("Exception gathering poLine data:", t);
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
    futures.addAll(operateOnSubObjsIfPresent(operation, line, ALERTS, httpClient, ctx, okapiHeaders, logger));
    futures.addAll(operateOnSubObjsIfPresent(operation, line, REPORTING_CODES, httpClient, ctx, okapiHeaders, logger));

    CompletableFuture.allOf(futures.toArray(new CompletableFuture[0]))
      .thenAccept(v -> {
        if (logger.isDebugEnabled()) {
          logger.debug("The PO line after {} operation on sub-objects: {}", operation, line.encodePrettily());
        }
        future.complete(line.mapTo(CompositePoLine.class));
      })
      .exceptionally(t -> {
        logger.error("Exception resolving one or more poLine sub-object(s) on {} operation:", t, operation);
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
      .forEach(fieldId -> futures.add(operateOnObject(operation, resourceByIdPath(field) + fieldId, httpClient, ctx, okapiHeaders, logger)
                .thenAccept(value -> {
                  if (value != null && !value.isEmpty()) {
                    array.add(value);
                  }
                })));
    pol.put(field, array);
    return futures;
  }

  public static CompletableFuture<JsonObject> operateOnObject(HttpMethod operation, String url,
      HttpClientInterface httpClient, Context ctx, Map<String, String> okapiHeaders, Logger logger) {
    return operateOnObject(operation, url, null, httpClient, ctx, okapiHeaders, logger);
  }

  public static CompletableFuture<JsonObject> operateOnObject(HttpMethod operation, String url, JsonObject body,
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

          /*
           * Ignoring not found error from storage because after MODORDSTOR-52 changes mod-orders-storage returns 404 instead of 204
           * when we try to delete the record which is not in the database.
           */
          if (code == 404 && operation == HttpMethod.DELETE) {
            return null;
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

  public static List<Error> validateOrder(CompositePurchaseOrder compositeOrder) {
    if (CollectionUtils.isEmpty(compositeOrder.getCompositePoLines())) {
      return Collections.emptyList();
    }

    List<Error> errors = new ArrayList<>();
    for (CompositePoLine compositePoLine : compositeOrder.getCompositePoLines()) {
      errors.addAll(validatePoLine(compositePoLine));
    }
    return errors;
  }

  public static List<Error> validatePoLine(CompositePoLine compPOL) {
    CompositePoLine.OrderFormat orderFormat = compPOL.getOrderFormat();
    if (orderFormat == P_E_MIX) {
      return validatePoLineWithMixedFormat(compPOL);
    } else if (orderFormat == ELECTRONIC_RESOURCE) {
      return validatePoLineWithElectronicFormat(compPOL);
    } else if (orderFormat == CompositePoLine.OrderFormat.PHYSICAL_RESOURCE) {
      return validatePoLineWithPhysicalFormat(compPOL);
    } else if (orderFormat == CompositePoLine.OrderFormat.OTHER) {
      return validatePoLineWithOtherFormat(compPOL);
    }

    return Collections.emptyList();
  }

  private static List<Error> validatePoLineWithMixedFormat(CompositePoLine compPOL) {
    int costPhysicalQuantity = defaultIfNull(compPOL.getCost().getQuantityPhysical(), 0);
    int costElectronicQuantity = defaultIfNull(compPOL.getCost().getQuantityElectronic(), 0);
    int locPhysicalQuantity = getPhysicalQuantity(compPOL.getLocations());
    int locElectronicQuantity = getElectronicQuantity(compPOL.getLocations());

    List<ErrorCodes> errors = new ArrayList<>();
    // The quantity of the physical and electronic resources in the cost must be specified
    if (costPhysicalQuantity + costElectronicQuantity == 0) {
      errors.add(ErrorCodes.ZERO_COST_QTY);
    } else if (costPhysicalQuantity == 0) {
      errors.add(ErrorCodes.ZERO_COST_PHYSICAL_QTY);
    } else if (costElectronicQuantity == 0) {
      errors.add(ErrorCodes.ZERO_COST_ELECTRONIC_QTY);
    }
    // The total quantity of the physical resources of all locations must not exceed specified in the cost
    if (locPhysicalQuantity > costPhysicalQuantity) {
      errors.add(ErrorCodes.PHYSICAL_LOC_QTY_EXCEEDS_COST);
    } else if (locPhysicalQuantity < costPhysicalQuantity) {
      errors.add(ErrorCodes.PHYSICAL_COST_QTY_EXCEEDS_LOC);
    }
    // The total quantity of the electronic resources of all locations must not exceed specified in the cost
    if (locElectronicQuantity > costElectronicQuantity) {
      errors.add(ErrorCodes.ELECTRONIC_LOC_QTY_EXCEEDS_COST);
    }

    validateCostPrices(compPOL.getCost(), P_E_MIX, errors);

    return convertErrorCodesToErrors(compPOL, errors);
  }

  private static void validateCostPrices(Cost cost, CompositePoLine.OrderFormat orderFormat, List<ErrorCodes> errors) {
    // Using default value as -1 to avoid null checks
    double unitPrice = defaultIfNull(cost.getListUnitPrice(), -1d);
    if (orderFormat == ELECTRONIC_RESOURCE) {
      if (unitPrice > 0d) {
        errors.add(ErrorCodes.COST_UNIT_PRICE_INVALID);
      }
    } else if (unitPrice < 0d) {
      errors.add(ErrorCodes.COST_UNIT_PRICE_INVALID);
    }

    double unitPriceElectronic = defaultIfNull(cost.getListUnitPriceElectronic(), -1d);
    if (orderFormat == ELECTRONIC_RESOURCE || orderFormat == P_E_MIX) {
      if (unitPriceElectronic < 0d) {
        errors.add(ErrorCodes.COST_UNIT_PRICE_ELECTRONIC_INVALID);
      }
    } else if (unitPriceElectronic > 0d) {
      errors.add(ErrorCodes.COST_UNIT_PRICE_ELECTRONIC_INVALID);
    }

    double additionalCost = defaultIfNull(cost.getAdditionalCost(), 0d);
    if (additionalCost < 0d) {
      errors.add(ErrorCodes.COST_ADDITIONAL_COST_INVALID);
    }

    double discount = defaultIfNull(cost.getDiscount(), 0d);
    if (discount < 0d || (cost.getDiscountType() == Cost.DiscountType.PERCENTAGE && discount > 100d)
      // validate that discount does not exceed total price
      || (discount > 0d && cost.getDiscountType() == Cost.DiscountType.AMOUNT && calculateEstimatedPrice(cost) < 0d)) {
      errors.add(ErrorCodes.COST_DISCOUNT_INVALID);
    }
  }

  private static List<Error> validatePoLineWithPhysicalFormat(CompositePoLine compPOL) {
    List<ErrorCodes> errors = new ArrayList<>();

    int costPhysicalQuantity = defaultIfNull(compPOL.getCost().getQuantityPhysical(), 0);
    // The quantity of the physical resources in the cost must be specified
    if (costPhysicalQuantity == 0) {
      errors.add(ErrorCodes.ZERO_COST_PHYSICAL_QTY);
    }
    // The quantity of the electronic resources in the cost must not be specified
    if (defaultIfNull(compPOL.getCost().getQuantityElectronic(), 0) > 0) {
      errors.add(ErrorCodes.NON_ZERO_COST_ELECTRONIC_QTY);
    }
    // The total quantity of the physical resources of all locations must not exceed specified in the cost
    int locationsPhysicalQuantity = getPhysicalQuantity(compPOL.getLocations());
    if (locationsPhysicalQuantity > costPhysicalQuantity) {
      errors.add(ErrorCodes.PHYSICAL_LOC_QTY_EXCEEDS_COST);
    } else if (locationsPhysicalQuantity < costPhysicalQuantity) {
      errors.add(ErrorCodes.PHYSICAL_COST_QTY_EXCEEDS_LOC);
    }

    validateCostPrices(compPOL.getCost(), PHYSICAL_RESOURCE, errors);

    return convertErrorCodesToErrors(compPOL, errors);
  }

  private static List<Error> validatePoLineWithElectronicFormat(CompositePoLine compPOL) {
    List<ErrorCodes> errors = new ArrayList<>();

    int costElectronicQuantity = defaultIfNull(compPOL.getCost().getQuantityElectronic(), 0);
    // The quantity of the electronic resources in the cost must be specified
    if (costElectronicQuantity == 0) {
      errors.add(ErrorCodes.ZERO_COST_ELECTRONIC_QTY);
    }
    // The quantity of the physical resources in the cost must not be specified
    if (defaultIfNull(compPOL.getCost().getQuantityPhysical(), 0) > 0) {
      errors.add(ErrorCodes.NON_ZERO_COST_PHYSICAL_QTY);
    }
    // The total quantity of the electronic resources of all locations must not exceed specified in the cost
    if (getElectronicQuantity(compPOL.getLocations()) > costElectronicQuantity) {
      errors.add(ErrorCodes.ELECTRONIC_LOC_QTY_EXCEEDS_COST);
    }

    validateCostPrices(compPOL.getCost(), ELECTRONIC_RESOURCE, errors);

    return convertErrorCodesToErrors(compPOL, errors);
  }

  private static List<Error> validatePoLineWithOtherFormat(CompositePoLine compPOL) {
    return validatePoLineWithPhysicalFormat(compPOL);
  }

  /**
   * The method converts {@link ErrorCodes} elements to {@link Error} adding additionally {@link Parameter} with PO Line number is presents
   * @param compPOL Composite PO Line
   * @param errors list of static {@link ErrorCodes}
   * @return List of {@link Error} elements
   */
  private static List<Error> convertErrorCodesToErrors(CompositePoLine compPOL, List<ErrorCodes> errors) {
    return errors.stream()
                 .map(errorCode -> {
                   Error error = errorCode.toError();
                   String poLineNumber = compPOL.getPoLineNumber();
                   if (StringUtils.isNotEmpty(poLineNumber)) {
                     error.getParameters()
                          .add(new Parameter().withKey(PO_LINE_NUMBER)
                                              .withValue(poLineNumber));
                   }
                   return error;
                 })
                 .collect(toList());
  }

  /**
   * Calculates total quantity based of cost for electronic and physical resources
   *
   * @param compPOL PO line to calculate total quantity
   * @return total quantity for PO Line
   */
  public static int calculateTotalQuantity(CompositePoLine compPOL) {
    Cost cost = compPOL.getCost();
    if (cost == null) {
      return 0;
    }
  	int eQuantity = ObjectUtils.defaultIfNull(cost.getQuantityElectronic(), 0);
    int physicalQuantity = ObjectUtils.defaultIfNull(cost.getQuantityPhysical(), 0);
    return eQuantity + physicalQuantity;
  }

  /**
   * Calculates total items quantity for all locations.
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
    return calculateInventoryItemsQuantity(compPOL, compPOL.getLocations());
  }

  /**
   * Calculates items quantity for specified locations.
   *
   * @param compPOL composite PO Line
   * @param locations list of locations to calculate quantity for
   * @return quantity of items expected in the inventory for PO Line
   * @see #calculateInventoryItemsQuantity(CompositePoLine)
   */
  public static int calculateInventoryItemsQuantity(CompositePoLine compPOL, List<Location> locations) {
    if (compPOL.getCheckinItems() != null && compPOL.getCheckinItems()) return 0;
    switch (compPOL.getOrderFormat()) {
      case P_E_MIX:
        int quantity = getPhysicalQuantity(locations);
        return isInventoryUpdateRequiredForEresource(compPOL) ? quantity + getElectronicQuantity(locations) : quantity;
      case PHYSICAL_RESOURCE:
        return getPhysicalQuantity(locations);
      case ELECTRONIC_RESOURCE:
        return isInventoryUpdateRequiredForEresource(compPOL) ? getElectronicQuantity(locations) : 0;
      case OTHER:
      default:
        return 0;
    }
  }

  public static int calculateExpectedQuantityOfPiecesWithoutItemCreation(CompositePoLine compPOL, List<Location> locations) {
    switch (compPOL.getOrderFormat()) {
      case P_E_MIX:
        return isInventoryUpdateRequiredForEresource(compPOL) ? 0 : getElectronicQuantity(locations);
      case ELECTRONIC_RESOURCE:
        return isInventoryUpdateRequiredForEresource(compPOL) ? 0 : getElectronicQuantity(locations);
      case OTHER:
        return getPhysicalQuantity(locations);
      case PHYSICAL_RESOURCE:
      default:
        return 0;
    }
  }

  /**
   * Calculates total estimated price. See MODORDERS-180 for more details.
   * @param cost PO Line's cost
   */
  public static double calculateEstimatedPrice(Cost cost) {
    BigDecimal total = BigDecimal.ZERO;
    if (cost.getListUnitPrice() != null && cost.getQuantityPhysical() != null) {
      BigDecimal pPrice = BigDecimal.valueOf(cost.getListUnitPrice())
                                    .multiply(BigDecimal.valueOf(cost.getQuantityPhysical()));
      total = total.add(pPrice);
    }
    if (cost.getListUnitPriceElectronic() != null && cost.getQuantityElectronic() != null) {
      BigDecimal ePrice = BigDecimal.valueOf(cost.getListUnitPriceElectronic())
                                    .multiply(BigDecimal.valueOf(cost.getQuantityElectronic()));
      total = total.add(ePrice);
    }
    if (cost.getDiscount() != null) {
      BigDecimal discount;
      if (Cost.DiscountType.AMOUNT == cost.getDiscountType()) {
        discount = BigDecimal.valueOf(cost.getDiscount());
      } else {
        discount = total.multiply(BigDecimal.valueOf(cost.getDiscount()/100d));
      }
      total = total.subtract(discount);
    }
    if (cost.getAdditionalCost() != null) {
      total = total.add(BigDecimal.valueOf(cost.getAdditionalCost()));
    }
    Currency currency = Currency.getInstance(cost.getCurrency());
    return total.setScale(currency.getDefaultFractionDigits(), RoundingMode.HALF_UP).doubleValue();
  }

  public static List<Piece> constructPieces(List<String> itemIds, String poLineId, String locationId) {
    return itemIds.stream()
      .map(itemId -> constructPiece(locationId, poLineId, itemId))
      .collect(toList());
  }

  public static Piece constructPiece(String locationId, String poLineId, String itemId) {
    Piece piece = new Piece();
    piece.setItemId(itemId);
    piece.setPoLineId(poLineId);
    piece.setLocationId(locationId);
    return piece;
  }

  private static int getPhysicalQuantity(List<Location> locations) {
    if (CollectionUtils.isNotEmpty(locations)) {
      return locations.stream()
                      .map(Location::getQuantityPhysical)
                      .filter(Objects::nonNull)
                      .mapToInt(Integer::intValue)
                      .sum();
    } else {
      return 0;
    }
  }

  private static int getElectronicQuantity(List<Location> locations) {
    if (CollectionUtils.isNotEmpty(locations)) {
      return locations.stream()
                      .map(Location::getQuantityElectronic)
                      .filter(Objects::nonNull)
                      .mapToInt(Integer::intValue)
                      .sum();
    } else {
      return 0;
    }
  }

  private static boolean isInventoryUpdateRequiredForEresource(CompositePoLine compPOL) {
    return Optional.ofNullable(compPOL.getEresource())
                   .map(Eresource::getCreateInventory)
                   .orElse(false);
  }

  /**
   * Groups all PO Line's locations by location id
   * @param compPOL PO line with locations to group
   * @return map of grouped locations where key is location id and value is list of locations with the same id
   */
  public static Map<String, List<Location>> groupLocationsById(CompositePoLine compPOL) {
    if (CollectionUtils.isEmpty(compPOL.getLocations())) {
      return Collections.emptyMap();
    }

    return compPOL.getLocations()
                  .stream()
                  .collect(Collectors.groupingBy(Location::getLocationId));
  }

  /**
   * Wait for all requests completion and collect all resulting objects. In case any failed, complete resulting future with the exception
   * @param futures list of futures and each produces resulting object on completion
   * @param <T> resulting type
   * @return resulting objects
   */
  public static <T> CompletableFuture<List<T>> collectResultsOnSuccess(List<CompletableFuture<T>> futures) {
    return VertxCompletableFuture.allOf(futures.toArray(new CompletableFuture[0]))
       .thenApply(v -> futures
         .stream()
         // The CompletableFuture::join can be safely used because the `allOf` guaranties success at this step
         .map(CompletableFuture::join)
         .filter(Objects::nonNull)
         .collect(toList())
       );
  }

  /**
   * Transform list of id's to CQL query using 'or' operation
   * @param ids list of id's
   * @return String representing CQL query to get records by id's
   */
  public static String convertIdsToCqlQuery(List<String> ids) {
    return StreamEx.of(ids).map(id -> "id==" + id).joining(" or ");
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
   * A common method to update an entry in the storage
   *
   * @param recordData json to use for update operation
   * @param endpoint endpoint
   */
  public static CompletableFuture<Void> handlePutRequest(String endpoint, JsonObject recordData, HttpClientInterface httpClient,
                                                         Context ctx, Map<String, String> okapiHeaders, Logger logger) {
    CompletableFuture<Void> future = new VertxCompletableFuture<>(ctx);
    try {
      if (logger.isDebugEnabled()) {
        logger.debug("Sending 'PUT {}' with body: {}", endpoint, recordData.encodePrettily());
      }
      httpClient
        .request(HttpMethod.PUT, recordData.toBuffer(), endpoint, okapiHeaders)
        .thenApply(HelperUtils::verifyAndExtractBody)
        .thenAccept(response -> {
          logger.debug("'PUT {}' request successfully processed", endpoint);
          future.complete(null);
        })
        .exceptionally(e -> {
          future.completeExceptionally(e);
          logger.error("'PUT {}' request failed. Request body: {}", e, endpoint, recordData.encodePrettily());
          return null;
        });
    } catch (Exception e) {
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
          if (logger.isDebugEnabled()) {
            logger.debug("The response from mod-configuration: {}", entries.encodePrettily());
          }
          entries.getJsonArray("configs").stream()
            .forEach(o ->
              config.put(((JsonObject) o).getString("configName"),
                ((JsonObject) o).getValue("value")));
          future.complete(config);
        })
      );
    } catch (Exception e) {
      logger.error("Error happened while getting configs", e);
      future.complete(config);
    }
    return future;
  }

  public static int getPoLineLimit(JsonObject config) {
    try {
      return Integer.parseInt(config.getString(PO_LINES_LIMIT_PROPERTY, DEFAULT_POLINE_LIMIT));
    } catch (NumberFormatException e) {
      throw new NumberFormatException("Invalid limit value in configuration.");
    }
  }

  /**
   * Convert {@link JsonObject} which actually represents org.folio.rest.acq.model.PurchaseOrder to {@link CompositePurchaseOrder}
   * These objects are the same except PurchaseOrder doesn't contain poLines field.
   * @param poJson {@link JsonObject} representing org.folio.rest.acq.model.PurchaseOrder
   * @return {@link CompositePurchaseOrder}
   */
  public static CompositePurchaseOrder convertToCompositePurchaseOrder(JsonObject poJson) {
    return poJson.mapTo(CompositePurchaseOrder.class);
  }
}
