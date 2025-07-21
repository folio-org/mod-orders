package org.folio.orders.utils;

import io.vertx.core.AsyncResult;
import io.vertx.core.CompositeFuture;
import io.vertx.core.Future;
import io.vertx.core.Handler;
import io.vertx.core.eventbus.DeliveryOptions;
import io.vertx.core.eventbus.Message;
import io.vertx.core.json.JsonObject;
import one.util.streamex.IntStreamEx;
import one.util.streamex.StreamEx;
import org.apache.commons.collections4.CollectionUtils;
import org.apache.commons.collections4.map.CaseInsensitiveMap;
import org.apache.commons.lang3.ObjectUtils;
import org.folio.helper.BaseHelper;
import org.folio.okapi.common.GenericCompositeFuture;
import org.folio.orders.events.handlers.MessageAddress;
import org.folio.rest.core.exceptions.HttpException;
import org.folio.rest.core.exceptions.NoInventoryRecordException;
import org.folio.rest.core.models.RequestContext;
import org.folio.rest.jaxrs.model.PoLine;
import org.folio.rest.jaxrs.model.CompositePurchaseOrder;
import org.folio.rest.jaxrs.model.Cost;
import org.folio.rest.jaxrs.model.Location;
import org.folio.rest.jaxrs.model.Piece;
import org.folio.rest.jaxrs.model.PurchaseOrder;
import org.folio.rest.jaxrs.model.Title;
import org.javamoney.moneta.Money;
import org.javamoney.moneta.function.MonetaryOperators;

import javax.money.CurrencyUnit;
import javax.money.Monetary;
import javax.money.MonetaryAmount;
import javax.money.convert.ConversionQuery;
import javax.money.convert.ConversionQueryBuilder;
import java.util.ArrayList;
import java.util.Collection;
import java.util.Collections;
import java.util.EnumMap;
import java.util.List;
import java.util.Map;
import java.util.Objects;
import java.util.Optional;
import java.util.concurrent.CompletionException;
import java.util.function.Function;

import static io.vertx.core.Future.succeededFuture;
import static org.apache.commons.lang3.ObjectUtils.defaultIfNull;
import static org.folio.rest.RestConstants.EN;
import static org.folio.rest.RestConstants.SEMAPHORE_MAX_ACTIVE_THREADS;
import static org.folio.rest.core.exceptions.ErrorCodes.MULTIPLE_NONPACKAGE_TITLES;
import static org.folio.rest.core.exceptions.ErrorCodes.TITLE_NOT_FOUND;
import static org.folio.service.exchange.CustomExchangeRateProvider.RATE_KEY;

public class HelperUtils {

  public static final String ID = "id";
  public static final String FUND_ID = "fundId";
  public static final String PO_LINES = "poLines";
  public static final String CONFIGS = "configs";
  public static final String CONFIG_NAME = "configName";
  public static final String CONFIG_VALUE = "value";

  public static final String ORDER_CONFIG_MODULE_NAME = "ORDERS";
  public static final String DATA_EXPORT_SPRING_CONFIG_MODULE_NAME = "mod-data-export-spring";

  public static final String DEFAULT_POLINE_LIMIT = "1";
  public static final String REASON_COMPLETE = "Complete";
  public static final String REASON_CANCELLED = "Cancelled";
  public static final String OKAPI_URL = "x-okapi-url";
  public static final String PO_LINES_LIMIT_PROPERTY = "poLines-limit";
  public static final String LANG = "lang";
  public static final String WORKFLOW_STATUS = "workflowStatus";

  private HelperUtils() {

  }

  public static Map<String, String> getOkapiHeaders(Message<JsonObject> message) {
    Map<String, String> okapiHeaders = new CaseInsensitiveMap<>();
    message.headers()
      .entries()
      .forEach(entry -> okapiHeaders.put(entry.getKey(), entry.getValue()));
    return okapiHeaders;
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
   * @param poLine PO line to calculate total quantity
   * @return total quantity for PO Line
   */
  public static int calculateTotalQuantity(PoLine poLine) {
    Cost cost = poLine.getCost();
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
   * @param poLine PO Line
   * @return quantity of items expected in the inventory for PO Line
   */
  public static int calculateInventoryItemsQuantity(PoLine poLine) {
    return calculateInventoryItemsQuantity(poLine, poLine.getLocations());
  }

  /**
   * Calculates items quantity for specified locations.
   *
   * @param poLine   PO Line
   * @param locations list of locations to calculate quantity for
   * @return quantity of items expected in the inventory for PO Line
   * @see #calculateInventoryItemsQuantity(PoLine)
   */
  public static int calculateInventoryItemsQuantity(PoLine poLine, List<Location> locations) {
    return IntStreamEx.of(calculatePiecesWithItemIdQuantity(poLine, locations).values()).sum();
  }

  /**
   * Calculates pieces quantity for list of locations and return map where piece format is a key and corresponding quantity of pieces as value.
   *
   * @param poLine   PO Line
   * @param locations list of locations to calculate quantity for
   * @return quantity of pieces per piece format either required Inventory item for PO Line
   */
  public static Map<Piece.Format, Integer> calculatePiecesWithItemIdQuantity(PoLine poLine, List<Location> locations) {
    // Piece records are not going to be created for PO Line which is going to be checked-in
    if (poLine.getCheckinItems() != null && poLine.getCheckinItems()) {
      return Collections.emptyMap();
    }

    var quantities = new EnumMap<Piece.Format, Integer>(Piece.Format.class);
    return switch (poLine.getOrderFormat()) {
      case P_E_MIX -> {
        if (PoLineCommonUtil.isItemsUpdateRequiredForPhysical(poLine)) {
          quantities.put(Piece.Format.PHYSICAL, calculatePiecesQuantity(Piece.Format.PHYSICAL, locations));
        }
        if (PoLineCommonUtil.isItemsUpdateRequiredForEresource(poLine)) {
          quantities.put(Piece.Format.ELECTRONIC, calculatePiecesQuantity(Piece.Format.ELECTRONIC, locations));
        }
        yield quantities;
      }
      case PHYSICAL_RESOURCE -> {
        int pQty = PoLineCommonUtil.isItemsUpdateRequiredForPhysical(poLine) ? calculatePiecesQuantity(Piece.Format.PHYSICAL, locations) : 0;
        quantities.put(Piece.Format.PHYSICAL, pQty);
        yield quantities;
      }
      case ELECTRONIC_RESOURCE -> {
        int eQty = PoLineCommonUtil.isItemsUpdateRequiredForEresource(poLine) ? calculatePiecesQuantity(Piece.Format.ELECTRONIC, locations) : 0;
        quantities.put(Piece.Format.ELECTRONIC, eQty);
        yield quantities;
      }
      case OTHER -> {
        int oQty = PoLineCommonUtil.isItemsUpdateRequiredForPhysical(poLine) ? calculatePiecesQuantity(Piece.Format.OTHER, locations) : 0;
        quantities.put(Piece.Format.OTHER, oQty);
        yield quantities;
      }
    };
  }

  /**
   * Calculates pieces quantity for specified locations based on piece format.
   *
   * @param format    piece format
   * @param locations list of locations to calculate quantity for
   * @return quantity of items expected in the inventory for PO Line
   */
  public static int calculatePiecesQuantity(Piece.Format format, List<Location> locations) {
    return switch (format) {
      case ELECTRONIC -> getElectronicLocationsQuantity(locations);
      case PHYSICAL, OTHER -> getPhysicalLocationsQuantity(locations);
    };
  }

  /**
   * Calculates total estimated price. See MODORDERS-180 for more details.
   *
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
    if (CollectionUtils.isEmpty(locations)) {
      return 0;
    }
    return locations.stream()
      .map(Location::getQuantityPhysical)
      .filter(Objects::nonNull)
      .mapToInt(Integer::intValue)
      .sum();
  }

  public static int getElectronicLocationsQuantity(List<Location> locations) {
    if (CollectionUtils.isEmpty(locations)) {
      return 0;
    }
    return locations.stream()
      .map(Location::getQuantityElectronic)
      .filter(Objects::nonNull)
      .mapToInt(Integer::intValue)
      .sum();
  }

  /**
   * Almost same as collectResultsOnSuccess with addition that each future itself returns a list and
   * this implementation combines all elements of all lists into the single list.
   * @param futures The list of futures to be combined
   * @return Single list containing all elements returned from all futures
   * @param <E> element type
   * @param <T> list type
   */
  public static <E, T extends List<E>> Future<List<E>> combineResultListsOnSuccess(Collection<Future<T>> futures) {
    return collectResultsOnSuccess(futures)
      .map(lists -> lists.stream().flatMap(List::stream).toList());
  }

  /**
   * Wait for all requests completion and collect all resulting objects. In case any failed, complete resulting future with the exception
   *
   * @param futures list of futures and each produces resulting object on completion
   * @param <T>     resulting type
   * @return resulting objects
   */
  public static <T> Future<List<T>> collectResultsOnSuccess(Collection<Future<T>> futures) {
    return GenericCompositeFuture.join(new ArrayList<>(futures))
      .map(CompositeFuture::list);
  }

  /**
   * Same as {@link #collectResultsOnSuccess(Collection)} but filters out null results.
   *
   * @param futures list of futures and each produces resulting object on completion
   * @param <T>     resulting type
   * @return resulting objects that are not null
   */
  public static <T> Future<List<T>> collectResultsOnSuccessNonNull(Collection<Future<T>> futures) {
    return collectResultsOnSuccess(futures)
      .map(results -> StreamEx.of(results).nonNull().toList());
  }

  /**
   * Executes a collection of futures and fails if any of them fail.
   * Returns a Void future that completes when all futures complete successfully.
   *
   * @param <T> The type of result in the futures
   * @param futures Collection of futures to be joined
   * @return A future that succeeds with void if all futures succeed, or fails if any future fails
   */
  public static <T> Future<Void> executeAllFailFast(Collection<Future<T>> futures) {
    if (futures.isEmpty()) {
      return Future.succeededFuture();
    }

    return Future.all(new ArrayList<>(futures))
      .mapEmpty();
  }

  /**
   * The method allows to compose any elements with the same action in sequence.
   *
   * @param  list    elements to be executed in sequence
   * @param  method  action that will be executed sequentially based on the number of list items
   * @return         the last composed element(Feature result)
   */
  public static <T, R> Future<R> chainCall(List<T> list, Function<T, Future<R>> method) {
    Future<R> f = Future.succeededFuture();
    for (T item : list) {
      f = f.compose(r -> method.apply(item));
    }
    return f;
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
   *
   * @param poJson {@link JsonObject} representing org.folio.rest.acq.model.PurchaseOrder
   * @return {@link CompositePurchaseOrder}
   */
  public static CompositePurchaseOrder convertToCompositePurchaseOrder(JsonObject poJson) {
    return poJson.mapTo(CompositePurchaseOrder.class);
  }

  public static boolean isProductIdsExist(PoLine poLine) {
    return poLine.getDetails() != null && CollectionUtils.isNotEmpty(poLine.getDetails().getProductIds());
  }

  public static Void handleErrorResponse(Handler<AsyncResult<javax.ws.rs.core.Response>> asyncResultHandler, BaseHelper helper,
                                         Throwable t) {
    asyncResultHandler.handle(succeededFuture(helper.buildErrorResponse(t)));
    return null;
  }

  /**
   * Check the number of titles per po line.
   *
   * @param lineIdTitles Map po line id -> list of titles
   * @param poLineById   Map po line id -> po line
   */
  public static void verifyTitles(Map<String, List<Title>> lineIdTitles, Map<String, PoLine> poLineById) {
    verifyAllTitlesExist(lineIdTitles, poLineById);
    verifyNonPackageLinesHaveSingleTitle(lineIdTitles, poLineById);
  }

  private static void verifyNonPackageLinesHaveSingleTitle(Map<String, List<Title>> titles,
                                                           Map<String, PoLine> poLineById) {
    if (titles.keySet().stream().anyMatch(lineId -> titles.get(lineId).size() > 1 && !poLineById.get(lineId).getIsPackage())) {
      throw new HttpException(400, MULTIPLE_NONPACKAGE_TITLES);
    }
  }

  public static void verifyAllTitlesExist(Map<String, List<Title>> titles, Map<String, PoLine> poLineById) {
    if (titles.size() < poLineById.size())
      throw new HttpException(400, TITLE_NOT_FOUND);
  }

  public static boolean isNotFound(Throwable t) {
    return t instanceof HttpException httpException && httpException.getCode() == 404;
  }

  public static ConversionQuery buildConversionQuery(String fromCurrency, String toCurrency, Number exchangeRate) {
    return ConversionQueryBuilder.of()
      .setBaseCurrency(fromCurrency)
      .setTermCurrency(toCurrency)
      .set(RATE_KEY, exchangeRate)
      .build();
  }

  /**
   * Accepts response with collection of the elements and tries to extract the first one.
   * In case the response is incorrect or empty, the {@link CompletionException} will be thrown
   *
   * @param response     {@link JsonObject} representing service response which should contain array of objects
   * @param propertyName name of the property which holds array of objects
   * @return the first element of the array
   */
  public static JsonObject getFirstObjectFromResponse(JsonObject response, String propertyName) {
    return Optional.ofNullable(response.getJsonArray(propertyName))
      .flatMap(items -> items.stream().findFirst())
      .map(JsonObject.class::cast)
      .orElseThrow(() -> new CompletionException(new NoInventoryRecordException(
        String.format("No records of '%s' can be found", propertyName))));
  }

  public static String extractId(JsonObject json) {
    return json.getString(ID);
  }

  public static String extractCreatedDate(JsonObject json) {
    return json.getJsonObject(CommonFields.METADATA.getValue())
      .getString(CommonFields.CREATED_DATE.getValue());
  }

  public static CompositePurchaseOrder convertToCompositePurchaseOrder(PurchaseOrder purchaseOrder, List<PoLine> poLines) {
    var purchaseOrderJson = JsonObject.mapFrom(purchaseOrder);
    return purchaseOrderJson.mapTo(CompositePurchaseOrder.class).withPoLines(poLines);
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
