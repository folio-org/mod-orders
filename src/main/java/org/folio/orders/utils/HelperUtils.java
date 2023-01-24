package org.folio.orders.utils;

import static io.vertx.core.Future.succeededFuture;
import static java.util.stream.Collectors.toList;
import static org.apache.commons.lang3.ObjectUtils.defaultIfNull;
import static org.apache.commons.lang3.StringUtils.EMPTY;
import static org.apache.commons.lang3.StringUtils.isEmpty;
import static org.folio.orders.utils.ResourcePathResolver.ALERTS;
import static org.folio.orders.utils.ResourcePathResolver.REPORTING_CODES;
import static org.folio.rest.RestConstants.EN;
import static org.folio.rest.core.exceptions.ErrorCodes.MULTIPLE_NONPACKAGE_TITLES;
import static org.folio.rest.core.exceptions.ErrorCodes.TITLE_NOT_FOUND;
import static org.folio.rest.jaxrs.model.PoLine.PaymentStatus.FULLY_PAID;
import static org.folio.rest.jaxrs.model.PoLine.PaymentStatus.PAYMENT_NOT_REQUIRED;
import static org.folio.rest.jaxrs.model.PoLine.ReceiptStatus.FULLY_RECEIVED;
import static org.folio.rest.jaxrs.model.PoLine.ReceiptStatus.RECEIPT_NOT_REQUIRED;
import static org.folio.service.exchange.ExchangeRateProviderResolver.RATE_KEY;

import java.net.URLEncoder;
import java.nio.charset.StandardCharsets;
import java.util.ArrayList;
import java.util.Collection;
import java.util.Collections;
import java.util.EnumMap;
import java.util.List;
import java.util.Map;
import java.util.Objects;
import java.util.Optional;
import java.util.concurrent.CompletionException;
import java.util.regex.Matcher;
import java.util.regex.Pattern;

import javax.money.CurrencyUnit;
import javax.money.Monetary;
import javax.money.MonetaryAmount;
import javax.money.convert.ConversionQuery;
import javax.money.convert.ConversionQueryBuilder;

import org.apache.commons.collections4.CollectionUtils;
import org.apache.commons.collections4.map.CaseInsensitiveMap;
import org.apache.commons.lang3.ArrayUtils;
import org.apache.commons.lang3.ObjectUtils;
import org.apache.commons.lang3.StringUtils;
import org.folio.helper.BaseHelper;
import org.folio.okapi.common.GenericCompositeFuture;
import org.folio.orders.events.handlers.MessageAddress;
import org.folio.rest.core.exceptions.HttpException;
import org.folio.rest.core.exceptions.InventoryException;
import org.folio.rest.core.models.RequestContext;
import org.folio.rest.jaxrs.model.Alert;
import org.folio.rest.jaxrs.model.CloseReason;
import org.folio.rest.jaxrs.model.CompositePoLine;
import org.folio.rest.jaxrs.model.CompositePurchaseOrder;
import org.folio.rest.jaxrs.model.Cost;
import org.folio.rest.jaxrs.model.Location;
import org.folio.rest.jaxrs.model.Piece;
import org.folio.rest.jaxrs.model.PoLine;
import org.folio.rest.jaxrs.model.PoLine.PaymentStatus;
import org.folio.rest.jaxrs.model.PoLine.ReceiptStatus;
import org.folio.rest.jaxrs.model.PurchaseOrder;
import org.folio.rest.jaxrs.model.ReportingCode;
import org.folio.rest.jaxrs.model.Title;
import org.folio.service.exchange.ExchangeRateProviderResolver;
import org.javamoney.moneta.Money;
import org.javamoney.moneta.function.MonetaryOperators;

import io.vertx.core.AsyncResult;
import io.vertx.core.CompositeFuture;
import io.vertx.core.Future;
import io.vertx.core.Handler;
import io.vertx.core.eventbus.DeliveryOptions;
import io.vertx.core.eventbus.Message;
import io.vertx.core.json.JsonObject;
import one.util.streamex.IntStreamEx;
import one.util.streamex.StreamEx;

public class HelperUtils {

  public static final String ID = "id";
  public static final String FUND_ID = "fundId";
  public static final String COMPOSITE_PO_LINES = "compositePoLines";
  public static final String CONFIGS = "configs";
  public static final String CONFIG_NAME = "configName";
  public static final String CONFIG_VALUE = "value";

  public static final String SYSTEM_CONFIG_MODULE_NAME = "ORG";
  public static final String ORDER_CONFIG_MODULE_NAME = "ORDERS";

  public static final String DEFAULT_POLINE_LIMIT = "1";
  public static final String REASON_COMPLETE = "Complete";
  public static final String REASON_CANCELLED = "Cancelled";
  public static final String OKAPI_URL = "x-okapi-url";
  public static final String PO_LINES_LIMIT_PROPERTY = "poLines-limit";
  public static final String LANG = "lang";
  public static final String URL_WITH_LANG_PARAM = "%s?" + LANG + "=%s";
  public static final String WORKFLOW_STATUS = "workflowStatus";

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

  /**
   * @param query string representing CQL query
   * @return URL encoded string
   */
  public static String encodeQuery(String query) {
    return URLEncoder.encode(query, StandardCharsets.UTF_8);
  }

  public static String buildQuery(String query) {
    return isEmpty(query) ? EMPTY : "&query=" + encodeQuery(query);
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

  public static Integer calculateTotalLocationQuantity(Location location) {
    int quantity = 0;
    quantity += defaultIfNull(location.getQuantityElectronic(), 0);
    quantity += defaultIfNull(location.getQuantityPhysical(), 0);
    return quantity;
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
    return IntStreamEx.of(calculatePiecesWithItemIdQuantity(compPOL, locations).values()).sum();
  }

  /**
   * Calculates pieces quantity for list of locations and return map where piece format is a key and corresponding quantity of pieces as value.
   *
   * @param compPOL composite PO Line
   * @param locations list of locations to calculate quantity for
   * @return quantity of pieces per piece format either required Inventory item for PO Line
   */
  public static Map<Piece.Format, Integer> calculatePiecesWithItemIdQuantity(CompositePoLine compPOL, List<Location> locations) {
    // Piece records are not going to be created for PO Line which is going to be checked-in
    if (compPOL.getCheckinItems() != null && compPOL.getCheckinItems()) {
      return Collections.emptyMap();
    }

    EnumMap<Piece.Format, Integer> quantities = new EnumMap<>(Piece.Format.class);
    switch (compPOL.getOrderFormat()) {
      case P_E_MIX:
        if (PoLineCommonUtil.isItemsUpdateRequiredForPhysical(compPOL)) {
          quantities.put(Piece.Format.PHYSICAL, calculatePiecesQuantity(Piece.Format.PHYSICAL, locations));
        }
        if (PoLineCommonUtil.isItemsUpdateRequiredForEresource(compPOL)) {
          quantities.put(Piece.Format.ELECTRONIC, calculatePiecesQuantity(Piece.Format.ELECTRONIC, locations));
        }

        return quantities;
      case PHYSICAL_RESOURCE:
        int pQty = PoLineCommonUtil.isItemsUpdateRequiredForPhysical(compPOL) ? calculatePiecesQuantity(Piece.Format.PHYSICAL, locations) : 0;
        quantities.put(Piece.Format.PHYSICAL, pQty);
        return quantities;
      case ELECTRONIC_RESOURCE:
        int eQty = PoLineCommonUtil.isItemsUpdateRequiredForEresource(compPOL) ? calculatePiecesQuantity(Piece.Format.ELECTRONIC, locations) : 0;
        quantities.put(Piece.Format.ELECTRONIC, eQty);
        return quantities;
      case OTHER:
        int oQty = PoLineCommonUtil.isItemsUpdateRequiredForPhysical(compPOL) ? calculatePiecesQuantity(Piece.Format.OTHER, locations) : 0;
        quantities.put(Piece.Format.OTHER, oQty);
        return quantities;
      default:
        return Collections.emptyMap();
    }
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
   * Calculates total estimated price. See MODORDERS-180 for more details.
   * @param cost PO Line's cost
   */
  public static MonetaryAmount calculateEstimatedPrice(Cost cost) {
    CurrencyUnit currency = Monetary.getCurrency(cost.getCurrency());
    MonetaryAmount total = calculateCostUnitsTotal(cost);
    Double fyroAdjustmentAmountD = Optional.ofNullable(cost.getFyroAdjustmentAmount()).orElse(0.0d);
    MonetaryAmount fyroAdjustmentAmount = Money.of(fyroAdjustmentAmountD, currency);
    return total.add(fyroAdjustmentAmount).with(MonetaryOperators.rounding());
  }

  public static MonetaryAmount calculateCostUnitsTotal(Cost cost) {
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
    return total;
  }

  public static int getPhysicalLocationsQuantity(List<Location> locations) {
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

  public static int getElectronicLocationsQuantity(List<Location> locations) {
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

  /**
   * Wait for all requests completion and collect all resulting objects. In case any failed, complete resulting future with the exception
   * @param futures list of futures and each produces resulting object on completion
   * @param <T> resulting type
   * @return resulting objects
   */
  public static <T> Future<List<T>> collectResultsOnSuccess(List<Future<T>> futures) {
    return GenericCompositeFuture.join(new ArrayList<>(futures))
      .map(CompositeFuture::list);
  }

  /**
   * Transform list of id's to CQL query using 'or' operation
   * @param ids list of id's
   * @return String representing CQL query to get records by id's
   */
  public static String convertIdsToCqlQuery(Collection<String> ids, String idField) {
    return convertFieldListToCqlQuery(ids, idField, true);
  }

  public static String convertIdsToCqlQuery(Collection<String> ids) {
    return convertFieldListToCqlQuery(ids, ID, true);
  }

  /**
   * Transform list of values for some property to CQL query using 'or' operation
   * @param values list of field values
   * @param fieldName the property name to search by
   * @param strictMatch indicates whether strict match mode (i.e. ==) should be used or not (i.e. =)
   * @return String representing CQL query to get records by some property values
   */
  public static String convertFieldListToCqlQuery(Collection<String> values, String fieldName, boolean strictMatch) {
    String prefix = fieldName + (strictMatch ? "==(" : "=(");
    return StreamEx.of(values).joining(" or ", prefix, ")");
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

  public static boolean changeOrderStatus(PurchaseOrder purchaseOrder, List<PoLine> poLines) {
    boolean isUpdateRequired = false;

    if (toBeCancelled(purchaseOrder, poLines)) {
      purchaseOrder.setWorkflowStatus(PurchaseOrder.WorkflowStatus.CLOSED);
      purchaseOrder.setCloseReason(new CloseReason().withReason(REASON_CANCELLED));
      return true;
    }

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

  public static String convertTagListToCqlQuery(Collection<String> values, String fieldName, boolean strictMatch) {

    String prefix = fieldName + (strictMatch ? "==(\"" : "=(\"");
    return StreamEx.of(values).joining("\" or \"", prefix, "\")");
  }

  private static boolean toBeClosed(PurchaseOrder purchaseOrder, List<PoLine> poLines) {
    return purchaseOrder.getWorkflowStatus() == PurchaseOrder.WorkflowStatus.OPEN
      && poLines.stream().allMatch(HelperUtils::isCompletedPoLine);
  }

  private static boolean toBeCancelled(PurchaseOrder purchaseOrder, List<PoLine> poLines) {
    return purchaseOrder.getWorkflowStatus() == PurchaseOrder.WorkflowStatus.OPEN
      && poLines.stream().allMatch(HelperUtils::isCancelledPoLine);
  }

  private static boolean isCancelledPoLine(PoLine line) {
    PoLine.PaymentStatus paymentStatus = line.getPaymentStatus();
    PoLine.ReceiptStatus receiptStatus = line.getReceiptStatus();
    return paymentStatus == PaymentStatus.CANCELLED && receiptStatus == ReceiptStatus.CANCELLED;
  }

  private static boolean toBeReopened(PurchaseOrder purchaseOrder, List<PoLine> poLines) {
    return purchaseOrder.getWorkflowStatus() == PurchaseOrder.WorkflowStatus.CLOSED
      && poLines.stream().anyMatch(line -> !isCompletedPoLine(line));
  }

  private static boolean isCompletedPoLine(PoLine line) {
    PoLine.PaymentStatus paymentStatus = line.getPaymentStatus();
    PoLine.ReceiptStatus receiptStatus = line.getReceiptStatus();
    return (paymentStatus == PAYMENT_NOT_REQUIRED || paymentStatus == FULLY_PAID || paymentStatus == PaymentStatus.CANCELLED)
      && (receiptStatus == FULLY_RECEIVED || receiptStatus == RECEIPT_NOT_REQUIRED || receiptStatus == ReceiptStatus.CANCELLED);
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


  public static List<PoLine> convertToPoLines(List<CompositePoLine> compositePoLines) {
    return compositePoLines
      .stream()
      .map(HelperUtils::convertToPoLine)
      .collect(toList());
  }

  public static boolean isProductIdsExist(CompositePoLine compPOL) {
    return compPOL.getDetails() != null && CollectionUtils.isNotEmpty(compPOL.getDetails().getProductIds());
  }

  public static Void handleErrorResponse(Handler<AsyncResult<javax.ws.rs.core.Response>> asyncResultHandler, BaseHelper helper,
                                   Throwable t) {
    asyncResultHandler.handle(succeededFuture(helper.buildErrorResponse(t)));
    return null;
  }

  /**
   * Check the number of titles per po line.
   * @param lineIdTitles Map po line id -> list of titles
   * @param poLineById Map po line id -> composite po line
   */
  public static void verifyTitles(Map<String, List<Title>> lineIdTitles, Map<String, CompositePoLine> poLineById) {
    verifyAllTitlesExist(lineIdTitles, poLineById);
    verifyNonPackageLinesHaveSingleTitle(lineIdTitles, poLineById);
  }

  private static void verifyNonPackageLinesHaveSingleTitle(Map<String, List<Title>> titles,
      Map<String, CompositePoLine> poLineById) {
    if (titles.keySet().stream().anyMatch(lineId -> titles.get(lineId).size() > 1 &&
        !poLineById.get(lineId).getIsPackage())) {
      throw new HttpException(400, MULTIPLE_NONPACKAGE_TITLES);
    }
  }

  public static void verifyAllTitlesExist(Map<String, List<Title>> titles, Map<String, CompositePoLine> poLineById) {
    if (titles.size() < poLineById.size())
      throw new HttpException(400, TITLE_NOT_FOUND);
  }

  public static boolean isNotFound(Throwable t) {
    return t instanceof HttpException && ((HttpException) t).getCode() == 404;
  }

  public static ConversionQuery getConversionQuery(Double exchangeRate, String fromCurrency, String toCurrency) {
    ConversionQuery conversionQuery;
    if (exchangeRate != null) {
      conversionQuery = ConversionQueryBuilder.of().setBaseCurrency(fromCurrency)
        .setTermCurrency(toCurrency)
        .set(ExchangeRateProviderResolver.RATE_KEY, exchangeRate)
        .build();
    } else {
      conversionQuery = ConversionQueryBuilder.of().setBaseCurrency(fromCurrency)
        .setTermCurrency(toCurrency).build();
    }
    return conversionQuery;
  }

  public static void makePoLinePending(CompositePoLine poLine) {
      if (poLine.getPaymentStatus() == CompositePoLine.PaymentStatus.AWAITING_PAYMENT) {
        poLine.setPaymentStatus(CompositePoLine.PaymentStatus.PENDING);
      }
      if (poLine.getReceiptStatus() == CompositePoLine.ReceiptStatus.AWAITING_RECEIPT) {
        poLine.setReceiptStatus(CompositePoLine.ReceiptStatus.PENDING);
      }
  }

  public static ConversionQuery buildConversionQuery(PoLine poLine, String systemCurrency) {
    Cost cost = poLine.getCost();
    if (cost.getExchangeRate() != null){
      return ConversionQueryBuilder.of().setBaseCurrency(cost.getCurrency())
        .setTermCurrency(systemCurrency)
        .set(RATE_KEY, cost.getExchangeRate()).build();
    }
    return ConversionQueryBuilder.of().setBaseCurrency(cost.getCurrency()).setTermCurrency(systemCurrency).build();
  }

  /**
   * Accepts response with collection of the elements and tries to extract the first one.
   * In case the response is incorrect or empty, the {@link CompletionException} will be thrown
   * @param response     {@link JsonObject} representing service response which should contain array of objects
   * @param propertyName name of the property which holds array of objects
   * @return the first element of the array
   */
  public static JsonObject getFirstObjectFromResponse(JsonObject response, String propertyName) {
    return Optional.ofNullable(response.getJsonArray(propertyName))
      .flatMap(items -> items.stream().findFirst())
      .map(JsonObject.class::cast)
      .orElseThrow(() -> new CompletionException(new InventoryException(String.format("No records of '%s' can be found", propertyName))));
  }

  public static String extractId(JsonObject json) {
    return json.getString(ID);
  }

  public static CompositePurchaseOrder convertToCompositePurchaseOrder(PurchaseOrder purchaseOrder, List<PoLine> poLineList) {
    List<CompositePoLine> compositePoLines = new ArrayList<>(poLineList.size());
    if (CollectionUtils.isNotEmpty(poLineList)) {
      poLineList.forEach(poLine -> {
        poLine.setAlerts(null);
        poLine.setReportingCodes(null);
        CompositePoLine compositePoLine = PoLineCommonUtil.convertToCompositePoLine(poLine);
        compositePoLines.add(compositePoLine);
      });
    }
    JsonObject jsonLine = JsonObject.mapFrom(purchaseOrder);
    return jsonLine.mapTo(CompositePurchaseOrder.class).withCompositePoLines(compositePoLines);
  }

  public static void sendEvent(MessageAddress messageAddress, JsonObject data, RequestContext requestContext) {
    DeliveryOptions deliveryOptions = new DeliveryOptions();

    // Add okapi headers
    Map<String, String> okapiHeaders = requestContext.getHeaders();
    okapiHeaders.forEach(deliveryOptions::addHeader);
    data.put(LANG, EN);

    requestContext.getContext().owner()
      .eventBus()
      .send(messageAddress.address, data, deliveryOptions);
  }

  public static <T> T clone(Class<T> clazz, T object) {
    return JsonObject.mapFrom(object).mapTo(clazz);
  }

}
