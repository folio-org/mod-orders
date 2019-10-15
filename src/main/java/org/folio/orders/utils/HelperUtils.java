package org.folio.orders.utils;

import static java.util.Objects.nonNull;
import static java.util.concurrent.CompletableFuture.completedFuture;
import static java.util.stream.Collectors.toList;
import static org.apache.commons.lang3.ObjectUtils.defaultIfNull;
import static org.apache.commons.lang3.StringUtils.EMPTY;
import static org.apache.commons.lang3.StringUtils.isEmpty;
import static org.folio.orders.utils.ErrorCodes.PROHIBITED_FIELD_CHANGING;
import static org.folio.orders.utils.ResourcePathResolver.*;
import static org.folio.rest.RestVerticle.OKAPI_HEADER_TENANT;
import static org.folio.rest.RestVerticle.OKAPI_HEADER_TOKEN;
import static org.folio.rest.jaxrs.model.CompositePoLine.OrderFormat.*;
import static org.folio.rest.jaxrs.model.PoLine.PaymentStatus.FULLY_PAID;
import static org.folio.rest.jaxrs.model.PoLine.PaymentStatus.PAYMENT_NOT_REQUIRED;
import static org.folio.rest.jaxrs.model.PoLine.ReceiptStatus.FULLY_RECEIVED;
import static org.folio.rest.jaxrs.model.PoLine.ReceiptStatus.RECEIPT_NOT_REQUIRED;
import static org.folio.orders.utils.ResourcePathResolver.PO_LINES;
import static org.folio.orders.utils.ResourcePathResolver.resourceByIdPath;
import java.util.List;
import java.util.Map;
import java.util.concurrent.CompletableFuture;
import org.apache.commons.collections4.map.CaseInsensitiveMap;
import org.apache.commons.lang3.ArrayUtils;
import org.folio.rest.acq.model.Piece;
import org.folio.rest.jaxrs.model.PoLine;

import java.io.UnsupportedEncodingException;
import java.math.BigDecimal;
import java.net.URLEncoder;
import java.nio.charset.StandardCharsets;
import java.util.*;
import java.util.concurrent.CompletionException;
import java.util.regex.Matcher;
import java.util.regex.Pattern;
import java.util.stream.Collectors;

import javax.money.CurrencyUnit;
import javax.money.Monetary;
import javax.money.MonetaryAmount;

import one.util.streamex.IntStreamEx;
import one.util.streamex.StreamEx;
import org.apache.commons.collections4.CollectionUtils;
import org.apache.commons.lang3.ObjectUtils;
import org.apache.commons.lang3.StringUtils;
import org.folio.orders.rest.exceptions.HttpException;
import org.folio.rest.client.ConfigurationsClient;
import org.folio.rest.jaxrs.model.*;
import org.folio.rest.jaxrs.model.CompositePoLine.OrderFormat;
import org.folio.rest.jaxrs.model.PoLine.ReceiptStatus;
import org.folio.rest.jaxrs.model.Error;
import org.folio.rest.tools.client.Response;
import org.folio.rest.tools.client.interfaces.HttpClientInterface;
import org.folio.rest.tools.parser.JsonPathParser;
import org.javamoney.moneta.Money;
import org.javamoney.moneta.function.MonetaryOperators;

import io.vertx.core.Context;
import io.vertx.core.eventbus.Message;
import io.vertx.core.http.HttpMethod;
import io.vertx.core.json.JsonArray;
import io.vertx.core.json.JsonObject;
import io.vertx.core.logging.Logger;
import me.escoffier.vertx.completablefuture.VertxCompletableFuture;

public class HelperUtils {

  public static final String ID = "id";
  public static final String COMPOSITE_PO_LINES = "compositePoLines";
  public static final String ACQ_UNIT_IDS = "acqUnitIds";
  public static final String CONFIGS = "configs";
  public static final String CONFIG_NAME = "configName";
  public static final String CONFIG_VALUE = "value";
  private static final String CONFIG_QUERY = "module=ORDERS";
  private static final String ERROR_MESSAGE = "errorMessage";

  public static final String DEFAULT_POLINE_LIMIT = "1";
  public static final String REASON_COMPLETE = "Complete";
  private static final String MAX_POLINE_LIMIT = "500";
  public static final String OKAPI_URL = "X-Okapi-Url";
  private static final String PO_LINES_LIMIT_PROPERTY = "poLines-limit";
  public static final String LANG = "lang";
  public static final String URL_WITH_LANG_PARAM = "%s?" + LANG + "=%s";
  private static final String GET_ALL_POLINES_QUERY_WITH_LIMIT = resourcesPath(PO_LINES) + "?limit=%s&query=purchaseOrderId==%s&" + LANG + "=%s";
  private static final String GET_PURCHASE_ORDER_BYID = resourceByIdPath(PURCHASE_ORDER) + URL_WITH_LANG_PARAM;
  private static final String GET_PURCHASE_ORDER_BYPONUMBER_QUERY = resourcesPath(PURCHASE_ORDER) + "?query=poNumber==%s&" + LANG + "=%s";

  private static final int DEFAULT_PORT = 9130;
  private static final String EXCEPTION_CALLING_ENDPOINT_MSG = "Exception calling {} {}";
  private static final String CALLING_ENDPOINT_MSG = "Sending {} {}";
  private static final String PROTECTED_AND_MODIFIED_FIELDS = "protectedAndModifiedFields";
  public static final String WORKFLOW_STATUS = "workflowStatus";

  private static final Pattern HOST_PORT_PATTERN = Pattern.compile("https?://([^:/]+)(?::?(\\d+)?)");
  private static final Pattern CQL_SORT_BY_PATTERN = Pattern.compile("(.*)(\\ssortBy\\s.*)", Pattern.CASE_INSENSITIVE);

  private HelperUtils() {

  }

  public static Map<String, String> getOkapiHeaders(Message<JsonObject> message) {
    Map<String, String> okapiHeaders = new CaseInsensitiveMap<>();
    message.headers()
      .entries()
      .forEach(entry -> okapiHeaders.put(entry.getKey(), entry.getValue()));
    return okapiHeaders;
  }

  public static JsonObject verifyAndExtractBody(Response response) {
    if (!Response.isSuccess(response.getCode())) {
      throw new CompletionException(
          new HttpException(response.getCode(), response.getError().getString(ERROR_MESSAGE)));
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

    getPoLines(id, lang, httpClient,ctx, okapiHeaders, logger)
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
            String errorMessage = (response.getError() != null) ? response.getError().getString(ERROR_MESSAGE) : StringUtils.EMPTY;
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

  public static String buildQuery(String query, Logger logger) {
    return isEmpty(query) ? EMPTY : "&query=" + encodeQuery(query, logger);
  }

  public static String combineCqlExpressions(String operator, String... expressions) {
    if (ArrayUtils.isEmpty(expressions)) {
      return EMPTY;
    }

    String sorting = EMPTY;

    // Check whether last expression contains sorting query. If it does, extract it to be added in the end of the resulting query
    Matcher matcher = CQL_SORT_BY_PATTERN.matcher(expressions[expressions.length - 1]);
    if (matcher.find()) {
      expressions[expressions.length - 1] = matcher.group(1);
      sorting = matcher.group(2);
    }

    return StreamEx.of(expressions)
      .filter(StringUtils::isNotBlank)
      .joining(") " + operator + " (", "(", ")") + sorting;
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

    List<ErrorCodes> errors = new ArrayList<>();
    // The quantity of the physical and electronic resources in the cost must be specified
    if (getPhysicalCostQuantity(compPOL) == 0) {
      errors.add(ErrorCodes.ZERO_COST_PHYSICAL_QTY);
    }
    if (getElectronicCostQuantity(compPOL) == 0) {
      errors.add(ErrorCodes.ZERO_COST_ELECTRONIC_QTY);
    }

    if (isMaterialMissing(compPOL)) {
      errors.add(ErrorCodes.MISSING_MATERIAL_TYPE);
    }

    errors.addAll(validateLocations(compPOL));
    errors.addAll(validateCostPrices(compPOL));

    return convertErrorCodesToErrors(compPOL, errors);
  }

  public static int getPhysicalCostQuantity(CompositePoLine compPOL) {
    return defaultIfNull(compPOL.getCost().getQuantityPhysical(), 0);
  }

  public static int getElectronicCostQuantity(CompositePoLine compPOL) {
    return defaultIfNull(compPOL.getCost().getQuantityElectronic(), 0);
  }

  private static List<ErrorCodes> validateLocations(CompositePoLine compPOL) {
    List<ErrorCodes> errors = new ArrayList<>();
    List<Location> locations = compPOL.getLocations();

    // The total quantity of the physical and electronic resources of all locations must match specified in the cost
    if (isLocationsEresourceQuantityNotValid(compPOL)) {
      errors.add(ErrorCodes.ELECTRONIC_COST_LOC_QTY_MISMATCH);
    }
    if (isLocationsPhysicalQuantityNotValid(compPOL)) {
      errors.add(ErrorCodes.PHYSICAL_COST_LOC_QTY_MISMATCH);
    }

    // The total quantity of any location must exceed 0
    if (locations.stream().anyMatch(location -> calculateTotalLocationQuantity(location) == 0)) {
      errors.add(ErrorCodes.ZERO_LOCATION_QTY);
    }
    return errors;
  }


  public static Integer calculateTotalLocationQuantity(Location location) {
    int quantity = 0;
    quantity += defaultIfNull(location.getQuantityElectronic(), 0);
    quantity += defaultIfNull(location.getQuantityPhysical(), 0);
    return quantity;
  }

  private static boolean isLocationsPhysicalQuantityNotValid(CompositePoLine compPOL) {
    int physicalQuantity = getPhysicalLocationsQuantity(compPOL.getLocations());
    return (isHoldingUpdateRequiredForPhysical(compPOL.getPhysical()) || physicalQuantity > 0) && (physicalQuantity != getPhysicalCostQuantity(compPOL));
  }

  private static boolean isLocationsEresourceQuantityNotValid(CompositePoLine compPOL) {
    int electronicQuantity = getElectronicLocationsQuantity(compPOL.getLocations());
    return (isHoldingUpdateRequiredForEresource(compPOL.getEresource()) || electronicQuantity > 0) && (electronicQuantity != getElectronicCostQuantity(compPOL));
  }

  private static List<ErrorCodes> validateCostPrices(CompositePoLine compLine) {
    List<ErrorCodes> errors = new ArrayList<>();
    Cost cost = compLine.getCost();
    CompositePoLine.OrderFormat orderFormat = compLine.getOrderFormat();
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

    if (isDiscountNotValid(cost)) {
      errors.add(ErrorCodes.COST_DISCOUNT_INVALID);
    }

    return errors;
  }

  /**
   * Checks if discount is negative or exceed cost totalPrice
   *
   * @param cost for which discount is checked
   * @return true if cost.discount not valid
   */
  private static boolean isDiscountNotValid(Cost cost) {
    double discount = defaultIfNull(cost.getDiscount(), 0d);
    return (discount < 0d || cost.getDiscountType() == Cost.DiscountType.PERCENTAGE && discount > 100d)
      || (discount > 0d && cost.getDiscountType() == Cost.DiscountType.AMOUNT && calculateEstimatedPrice(cost).isNegative());
  }

  private static List<Error> validatePoLineWithPhysicalFormat(CompositePoLine compPOL) {
    List<ErrorCodes> errors = new ArrayList<>();

    // The quantity of the physical resources in the cost must be specified
    if (getPhysicalCostQuantity(compPOL) == 0) {
      errors.add(ErrorCodes.ZERO_COST_PHYSICAL_QTY);
    }
    // The quantity of the electronic resources in the cost must not be specified
    if (getElectronicCostQuantity(compPOL) > 0) {
      errors.add(ErrorCodes.NON_ZERO_COST_ELECTRONIC_QTY);
    }

    if (isMaterialMissing(compPOL)) {
      errors.add(ErrorCodes.MISSING_MATERIAL_TYPE);
    }

    errors.addAll(validateLocations(compPOL));
    errors.addAll(validateCostPrices(compPOL));

    return convertErrorCodesToErrors(compPOL, errors);
  }

  private static List<Error> validatePoLineWithElectronicFormat(CompositePoLine compPOL) {
    List<ErrorCodes> errors = new ArrayList<>();

    // The quantity of the electronic resources in the cost must be specified
    if (getElectronicCostQuantity(compPOL) == 0) {
      errors.add(ErrorCodes.ZERO_COST_ELECTRONIC_QTY);
    }
    // The quantity of the physical resources in the cost must not be specified
    if (getPhysicalCostQuantity(compPOL) > 0) {
      errors.add(ErrorCodes.NON_ZERO_COST_PHYSICAL_QTY);
    }


    if (isMaterialMissing(compPOL)) {
      errors.add(ErrorCodes.MISSING_MATERIAL_TYPE);
    }

    errors.addAll(validateLocations(compPOL));
    errors.addAll(validateCostPrices(compPOL));

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
  	int eQuantity = ObjectUtils.defaultIfNull(cost.getQuantityElectronic(), 0);
    int physicalQuantity = ObjectUtils.defaultIfNull(cost.getQuantityPhysical(), 0);
    return eQuantity + physicalQuantity;
  }

  /**
   * Calculates total items quantity for all locations.
   * The quantity is based on Order Format (please see MODORDERS-117):<br/>
   * If format equals Physical or Other the associated quantities will result in item records<br/>
   * If format = Electronic and Create Inventory = Instance,Holding,Item, the associated electronic quantities will result in item records being created in inventory<br/>
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
    return IntStreamEx.of(calculatePiecesQuantity(compPOL, locations, true).values()).sum();
  }

  /**
   * Calculates pieces quantity for list of locations and return map where piece format is a key and corresponding quantity of pieces as value.
   *
   * @param compPOL composite PO Line
   * @param locations list of locations to calculate quantity for
   * @return quantity of pieces per piece format either required Inventory item or not (depending on {@code withItem} parameter) for PO Line
   */
  public static Map<Piece.Format, Integer> calculatePiecesQuantity(CompositePoLine compPOL, List<Location> locations, boolean withItem) {
    // Piece records are not going to be created for PO Line which is going to be checked-in
    if (compPOL.getCheckinItems() != null && compPOL.getCheckinItems()) {
      return Collections.emptyMap();
    }

    EnumMap<Piece.Format, Integer> quantities = new EnumMap<>(Piece.Format.class);
    switch (compPOL.getOrderFormat()) {
      case P_E_MIX:
        if (withItem == isItemsUpdateRequiredForPhysical(compPOL)) {
          quantities.put(Piece.Format.PHYSICAL, calculatePiecesQuantity(Piece.Format.PHYSICAL, locations));
        }
        if (withItem == isItemsUpdateRequiredForEresource(compPOL)) {
          quantities.put(Piece.Format.ELECTRONIC, calculatePiecesQuantity(Piece.Format.ELECTRONIC, locations));
        }
        return quantities;
      case PHYSICAL_RESOURCE:
        int pQty = (withItem == isItemsUpdateRequiredForPhysical(compPOL)) ? calculatePiecesQuantity(Piece.Format.PHYSICAL, locations) : 0;
        quantities.put(Piece.Format.PHYSICAL, pQty);
        return quantities;
      case ELECTRONIC_RESOURCE:
        int eQty = (withItem == isItemsUpdateRequiredForEresource(compPOL)) ? calculatePiecesQuantity(Piece.Format.ELECTRONIC, locations) : 0;
        quantities.put(Piece.Format.ELECTRONIC, eQty);
        return quantities;
      case OTHER:
        int oQty = (withItem == isItemsUpdateRequiredForPhysical(compPOL)) ? calculatePiecesQuantity(Piece.Format.OTHER, locations) : 0;
        quantities.put(Piece.Format.OTHER, oQty);
        return quantities;
      default:
        return Collections.emptyMap();
    }
  }

  public static Map<Piece.Format, Integer> calculatePiecesQuantityWithoutLocation(CompositePoLine compPOL) {
    EnumMap<Piece.Format, Integer> quantities = new EnumMap<>(Piece.Format.class);

    if (compPOL.getOrderFormat() == OTHER && (compPOL.getPhysical().getCreateInventory() == Physical.CreateInventory.NONE || compPOL.getPhysical().getCreateInventory() == Physical.CreateInventory.INSTANCE)) {
      Physical.CreateInventory physicalCreateInventory = compPOL.getPhysical().getCreateInventory();
      if (physicalCreateInventory == Physical.CreateInventory.NONE || physicalCreateInventory == Physical.CreateInventory.INSTANCE) {
        quantities.put(Piece.Format.OTHER, getPhysicalCostQuantity(compPOL));
      }
    } else {
      quantities.putAll(calculatePhysicalPiecesQuantityWithoutLocation(compPOL));
      quantities.putAll(calculateElectronicPiecesQuantityWithoutLocation(compPOL));
    }
    return quantities;
  }

  private static EnumMap<Piece.Format, Integer> calculatePhysicalPiecesQuantityWithoutLocation(CompositePoLine compPOL) {
    EnumMap<Piece.Format, Integer> quantities = new EnumMap<>(Piece.Format.class);
    Physical.CreateInventory physicalCreateInventory = Optional.ofNullable(compPOL.getPhysical()).map(Physical::getCreateInventory).orElse(null);
    if (physicalCreateInventory == Physical.CreateInventory.NONE || physicalCreateInventory == Physical.CreateInventory.INSTANCE) {
      quantities.put(Piece.Format.PHYSICAL, getPhysicalCostQuantity(compPOL));
    }
    return quantities;
  }

  private static EnumMap<Piece.Format, Integer> calculateElectronicPiecesQuantityWithoutLocation(CompositePoLine compPOL) {
    EnumMap<Piece.Format, Integer> quantities = new EnumMap<>(Piece.Format.class);
    Eresource.CreateInventory eresourceCreateInventory = Optional.ofNullable(compPOL.getEresource()).map(Eresource::getCreateInventory).orElse(null);
    if (eresourceCreateInventory == Eresource.CreateInventory.NONE || eresourceCreateInventory == Eresource.CreateInventory.INSTANCE) {
      quantities.put(Piece.Format.ELECTRONIC, getElectronicCostQuantity(compPOL));
    }
    return quantities;
  }

  /**
   * Calculates pieces quantity for specified locations based on piece format.
   *
   * @param format piece format
   * @param locations list of locations to calculate quantity for
   * @return quantity of items expected in the inventory for PO Line
   */
  public static int calculatePiecesQuantity(Piece.Format format, List<Location> locations) {
    switch (format) {
      case ELECTRONIC:
        return getElectronicLocationsQuantity(locations);
      case PHYSICAL:
      case OTHER:
        return getPhysicalLocationsQuantity(locations);
      default:
        return 0;
    }
  }

  /**
   * Calculates quantity of pieces for specified locations which do not require item records in Inventory.
   *
   * @param compPOL composite PO Line
   * @param locations list of locations to calculate quantity for
   * @return quantity of pieces without items in the inventory for PO Line
   * @see #calculatePiecesQuantity(CompositePoLine, List, boolean)
   */
  public static int calculateExpectedQuantityOfPiecesWithoutItemCreation(CompositePoLine compPOL, List<Location> locations) {
    return IntStreamEx.of(calculatePiecesQuantity(compPOL, locations, false).values()).sum();
  }

  /**
   * Calculates total estimated price. See MODORDERS-180 for more details.
   * @param cost PO Line's cost
   */
  public static MonetaryAmount calculateEstimatedPrice(Cost cost) {
    CurrencyUnit currency = Monetary.getCurrency(cost.getCurrency());
    MonetaryAmount total = Money.of(0, currency);

    // Physical resources price
    if (cost.getListUnitPrice() != null && cost.getQuantityPhysical() != null) {
      MonetaryAmount pPrice = Money.of(cost.getListUnitPrice(), currency)
        .multiply(cost.getQuantityPhysical());
      total = total.add(pPrice);
    }
    // Electronic resources price
    if (cost.getListUnitPriceElectronic() != null && cost.getQuantityElectronic() != null) {
      MonetaryAmount ePrice = Money.of(cost.getListUnitPriceElectronic(), currency)
        .multiply(cost.getQuantityElectronic());
      total = total.add(ePrice);
    }
    // Discount amount
    if (cost.getDiscount() != null) {
      MonetaryAmount discount;
      if (Cost.DiscountType.AMOUNT == cost.getDiscountType()) {
        discount = Money.of(cost.getDiscount(), currency);
      } else {
        discount = total.with(MonetaryOperators.percent(cost.getDiscount()));
      }
      total = total.subtract(discount);
    }
    // Additional cost
    if (cost.getAdditionalCost() != null) {
      total = total.add(Money.of(cost.getAdditionalCost(), currency));
    }
    // Grand total
    return total.with(MonetaryOperators.rounding());
  }

  /**
   * Calculates PO's estimated price by summing the Estimated Price of the associated PO Lines.
   * See MODORDERS-181 for more details. At the moment assumption is that all prices are in the same currency.
   * @param poLines list of composite PO Lines
   * @return estimated purchase order's total price
   */
  public static double calculateTotalEstimatedPrice(List<CompositePoLine> poLines) {
    return poLines
      .stream()
      .map(CompositePoLine::getCost)
      .map(Cost::getPoLineEstimatedPrice)
      .map(BigDecimal::valueOf)
      .reduce(BigDecimal.ZERO, BigDecimal::add)
      .doubleValue();
  }

  private static int getPhysicalLocationsQuantity(List<Location> locations) {
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

  private static int getElectronicLocationsQuantity(List<Location> locations) {
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

  private static boolean isItemsUpdateRequiredForEresource(CompositePoLine compPOL) {
    if (compPOL.getCheckinItems() != null && compPOL.getCheckinItems()) {
      return false;
    }
    return Optional.ofNullable(compPOL.getEresource())
      .map(eresource -> eresource.getCreateInventory() == Eresource.CreateInventory.INSTANCE_HOLDING_ITEM)
      .orElse(false);
  }

  private static boolean isItemsUpdateRequiredForPhysical(CompositePoLine compPOL) {
    if (compPOL.getCheckinItems() != null && compPOL.getCheckinItems()) {
      return false;
    }
    return Optional.ofNullable(compPOL.getPhysical())
      .map(physical -> physical.getCreateInventory() == Physical.CreateInventory.INSTANCE_HOLDING_ITEM)
      .orElse(false);
  }

  /**
   * Group all PO Line's locations for which the holding should be created by location identifier
   * @param compPOL PO line with locations to group
   * @return map of grouped locations where key is location id and value is list of locations with the same id
   */
  public static Map<String, List<Location>> groupLocationsById(CompositePoLine compPOL) {
    if (CollectionUtils.isEmpty(compPOL.getLocations())) {
      return Collections.emptyMap();
    }

    return compPOL.getLocations()
                  .stream()
                  .filter(location -> isHoldingCreationRequiredForLocation(compPOL, location))
                  .collect(Collectors.groupingBy(Location::getLocationId));
  }

  public static boolean isHoldingCreationRequiredForLocation(CompositePoLine compPOL, Location location) {
    return (isHoldingUpdateRequiredForPhysical(compPOL.getPhysical()) && ObjectUtils.defaultIfNull(location.getQuantityPhysical(), 0) > 0)
      || (isHoldingUpdateRequiredForEresource(compPOL.getEresource()) && ObjectUtils.defaultIfNull(location.getQuantityElectronic(), 0) > 0);
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
  public static String convertIdsToCqlQuery(Collection<String> ids) {
    return convertIdsToCqlQuery(ids, ID, true);
  }

  /**
   * Transform list of values for some property to CQL query using 'or' operation
   * @param values list of field values
   * @param fieldName the property name to search by
   * @param strictMatch indicates whether strict match mode (i.e. ==) should be used or not (i.e. =)
   * @return String representing CQL query to get records by some property values
   */
  public static String convertIdsToCqlQuery(Collection<String> values, String fieldName, boolean strictMatch) {
    String prefix = fieldName + (strictMatch ? "==(" : "=(");
    return StreamEx.of(values).joining(" or ", prefix, ")");
  }

  public static CompletableFuture<JsonObject> handleGetRequest(String endpoint, HttpClientInterface
    httpClient, Context ctx, Map<String, String> okapiHeaders,
                                       Logger logger) {
    CompletableFuture<JsonObject> future = new VertxCompletableFuture<>(ctx);
    try {
      logger.info("Calling GET {}", endpoint);

      httpClient
        .request(HttpMethod.GET, endpoint, okapiHeaders)
        .thenApply(response -> {
          logger.debug("Validating response for GET {}", endpoint);
          return verifyAndExtractBody(response);
        })
        .thenAccept(body -> {
          if (logger.isInfoEnabled()) {
            logger.info("The response body for GET {}: {}", endpoint, nonNull(body) ? body.encodePrettily() : null);
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

  public static void verifyResponse(Response response) {
    if (!Response.isSuccess(response.getCode())) {
      throw new CompletionException(
        new HttpException(response.getCode(), response.getError().getString(ERROR_MESSAGE)));
    }
  }

  /**
   * A common method to delete an entry in the storage
   * @param endpoint endpoint
   */
  public static CompletableFuture<Void> handleDeleteRequest(String endpoint, HttpClientInterface httpClient, Context ctx,
      Map<String, String> okapiHeaders, Logger logger) {
    CompletableFuture<Void> future = new VertxCompletableFuture<>(ctx);

    logger.debug(CALLING_ENDPOINT_MSG, HttpMethod.DELETE, endpoint);

    try {
      httpClient.request(HttpMethod.DELETE, endpoint, okapiHeaders)
        .thenAccept(HelperUtils::verifyResponse)
        .thenApply(future::complete)
        .exceptionally(t -> {
          logger.error(EXCEPTION_CALLING_ENDPOINT_MSG, t, HttpMethod.DELETE, endpoint);
          future.completeExceptionally(t);
          return null;
        });
    } catch (Exception e) {
      logger.error(EXCEPTION_CALLING_ENDPOINT_MSG, e, HttpMethod.DELETE, endpoint);
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
      configurationsClient.getEntries(CONFIG_QUERY, 0, 100, null, null, response -> response.bodyHandler(body -> {
        if (response.statusCode() != 200) {
          logger.error(String.format("Expected status code 200, got '%s' :%s", response.statusCode(), body.toString()));
          future.complete(config);
          return;
        }

        JsonObject entries = body.toJsonObject();

        if (logger.isDebugEnabled()) {
          logger.debug("The response from mod-configuration: {}", entries.encodePrettily());
        }

        entries.getJsonArray(CONFIGS)
          .stream()
          .map(o -> (JsonObject) o)
          .forEach(entry -> config.put(entry.getString(CONFIG_NAME), entry.getValue(CONFIG_VALUE)));

        future.complete(config);
      }));
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

  public static boolean inventoryUpdateNotRequired(CompositePoLine compPOL) {
    // in case of "Other" order format check Physical createInventory value only
    if (compPOL.getOrderFormat() == OTHER) {
      return compPOL.getPhysical() == null || compPOL.getPhysical().getCreateInventory() == Physical.CreateInventory.NONE;
    }
    boolean physicalUpdateNotRequired = (compPOL.getPhysical() == null || compPOL.getPhysical().getCreateInventory() == Physical.CreateInventory.NONE);
    boolean eresourceUpdateNotRequired = (compPOL.getEresource() == null || compPOL.getEresource().getCreateInventory() == Eresource.CreateInventory.NONE);

    return physicalUpdateNotRequired && eresourceUpdateNotRequired;
  }

  public static boolean isHoldingsUpdateRequired(Eresource eresource, Physical physical) {
    return isHoldingUpdateRequiredForEresource(eresource) || isHoldingUpdateRequiredForPhysical(physical);
  }

  private static boolean isHoldingUpdateRequiredForPhysical(Physical physical) {
     return physical != null && (physical.getCreateInventory() == Physical.CreateInventory.INSTANCE_HOLDING
        || physical.getCreateInventory() == Physical.CreateInventory.INSTANCE_HOLDING_ITEM);
  }

  private static boolean isHoldingUpdateRequiredForEresource(Eresource eresource) {
    return eresource != null && (eresource.getCreateInventory() == Eresource.CreateInventory.INSTANCE_HOLDING
        || eresource.getCreateInventory() == Eresource.CreateInventory.INSTANCE_HOLDING_ITEM);
  }

  public static boolean isItemsUpdateRequired(CompositePoLine compPOL) {
    return isItemsUpdateRequiredForPhysical(compPOL) || isItemsUpdateRequiredForEresource(compPOL);
  }

  private static boolean isMaterialMissing(CompositePoLine compPOL) {
    boolean isMissing = false;
    if (compPOL.getOrderFormat().equals(OrderFormat.ELECTRONIC_RESOURCE)
        || compPOL.getOrderFormat().equals(OrderFormat.P_E_MIX)) {
      isMissing = compPOL.getEresource().getCreateInventory() == (Eresource.CreateInventory.INSTANCE_HOLDING_ITEM)
          && isEmpty(compPOL.getEresource().getMaterialType());
    }

    if (!compPOL.getOrderFormat().equals(OrderFormat.ELECTRONIC_RESOURCE)) {
      isMissing = isMissing
          || (compPOL.getPhysical().getCreateInventory() == Physical.CreateInventory.INSTANCE_HOLDING_ITEM
          && isEmpty(compPOL.getPhysical().getMaterialType()));
    }

    return isMissing;
  }

  public static boolean changeOrderStatus(PurchaseOrder purchaseOrder, List<PoLine> poLines) {
    boolean isUpdateRequired = false;
    if (toBeClosed(purchaseOrder, poLines)) {
      isUpdateRequired = true;
      purchaseOrder.setWorkflowStatus(PurchaseOrder.WorkflowStatus.CLOSED);
      purchaseOrder.setCloseReason(new CloseReason().withReason(REASON_COMPLETE));
    } else if (toBeReopened(purchaseOrder, poLines)) {
      isUpdateRequired = true;
      purchaseOrder.setWorkflowStatus(PurchaseOrder.WorkflowStatus.OPEN);
    }
    return isUpdateRequired;
  }

  private static boolean toBeClosed(PurchaseOrder purchaseOrder, List<PoLine> poLines) {
    return purchaseOrder.getWorkflowStatus() == PurchaseOrder.WorkflowStatus.OPEN
      && poLines.stream().allMatch(HelperUtils::isCompletedPoLine);
  }

  private static boolean toBeReopened(PurchaseOrder purchaseOrder, List<PoLine> poLines) {
    return purchaseOrder.getWorkflowStatus() == PurchaseOrder.WorkflowStatus.CLOSED
      && poLines.stream().anyMatch(line -> !isCompletedPoLine(line));
  }

  private static boolean isCompletedPoLine(PoLine line) {
    PoLine.PaymentStatus paymentStatus = line.getPaymentStatus();
    PoLine.ReceiptStatus receiptStatus = line.getReceiptStatus();
    return (paymentStatus == PAYMENT_NOT_REQUIRED || paymentStatus == FULLY_PAID)
      && (receiptStatus == FULLY_RECEIVED || receiptStatus == RECEIPT_NOT_REQUIRED);
  }

  public static PoLine convertToPoLine(CompositePoLine compPoLine) {
    JsonObject pol = JsonObject.mapFrom(compPoLine);
    pol.remove(ALERTS);
    pol.remove(REPORTING_CODES);
    PoLine poLine = pol.mapTo(PoLine.class);
    poLine.setAlerts(compPoLine.getAlerts().stream().map(Alert::getId).collect(toList()));
    poLine.setReportingCodes(compPoLine.getReportingCodes().stream().map(ReportingCode::getId).collect(toList()));
    return poLine;
  }

  public static CompletableFuture<String> updatePoLineReceiptStatus(PoLine poLine, ReceiptStatus status, HttpClientInterface httpClient,
      Context ctx, Map<String, String> okapiHeaders, Logger logger) {

    if (status == null || poLine.getReceiptStatus() == status) {
      return completedFuture(null);
    }

    // Update receipt date and receipt status
    if (status == FULLY_RECEIVED) {
      poLine.setReceiptDate(new Date());
    } else if (poLine.getCheckinItems() && poLine.getReceiptStatus()
      .equals(ReceiptStatus.AWAITING_RECEIPT) && status == ReceiptStatus.PARTIALLY_RECEIVED) {
      // if checking in, set the receipt date only for the first piece
      poLine.setReceiptDate(new Date());
    } else {
      poLine.setReceiptDate(null);
    }

    poLine.setReceiptStatus(status);
    // Update PO Line in storage
    return handlePutRequest(resourceByIdPath(PO_LINES, poLine.getId()), JsonObject.mapFrom(poLine), httpClient, ctx, okapiHeaders,
        logger).thenApply(v -> poLine.getId())
          .exceptionally(e -> {
            logger.error("The PO Line '{}' cannot be updated with new receipt status", e, poLine.getId());
            return null;
          });
  }

  public static JsonObject verifyProtectedFieldsChanged(List<String> protectedFields, JsonObject objectFromStorage,
      JsonObject requestObject) {
    Set<String> fields = new HashSet<>();
    JsonPathParser oldObject = new JsonPathParser(objectFromStorage);
    JsonPathParser newObject = new JsonPathParser(requestObject);
    for (String field : protectedFields) {
      if (!Objects.equals(oldObject.getValueAt(field), newObject.getValueAt(field))) {
        fields.add(field);
      }
    }

    if (CollectionUtils.isNotEmpty(fields)) {
      Error error = PROHIBITED_FIELD_CHANGING.toError()
        .withAdditionalProperty(PROTECTED_AND_MODIFIED_FIELDS, fields);
      throw new HttpException(400, error);
    }

    return objectFromStorage;
  }

  public static List<PoLine> convertToPoLines(JsonArray linesArray) {
    return linesArray.stream()
                     .map(json -> ((JsonObject) json).mapTo(PoLine.class))
                     .collect(toList());
  }

  public static List<PoLine> convertToPoLines(List<CompositePoLine> compositePoLines) {
    return compositePoLines
      .stream()
      .map(HelperUtils::convertToPoLine)
      .collect(toList());
  }

  public static boolean isProductIdsExist(CompositePoLine compPOL) {
    return compPOL.getDetails() != null && CollectionUtils.isNotEmpty(compPOL.getDetails().getProductIds());
  }
}
