package org.folio.service.inventory;

import io.vertx.core.Future;
import io.vertx.core.Promise;
import io.vertx.core.json.JsonArray;
import io.vertx.core.json.JsonObject;
import java.util.Objects;
import lombok.experimental.UtilityClass;
import org.apache.commons.lang3.StringUtils;
import org.folio.models.ItemStatus;
import org.folio.models.pieces.PiecesHolder;
import org.folio.rest.core.exceptions.ErrorCodes;
import org.folio.rest.core.exceptions.HttpException;
import org.folio.rest.core.models.RequestContext;
import org.folio.rest.jaxrs.model.CheckInPiece;
import org.folio.rest.jaxrs.model.PoLine;
import org.folio.rest.jaxrs.model.CompositePurchaseOrder;
import org.folio.rest.jaxrs.model.Contributor;
import org.folio.rest.jaxrs.model.Error;
import org.folio.rest.jaxrs.model.Parameter;
import org.folio.rest.jaxrs.model.Piece;
import org.folio.rest.jaxrs.model.ProductId;
import org.folio.rest.jaxrs.model.ReceivedItem;
import org.folio.service.caches.CommonSettingsCache;
import org.folio.service.caches.InventoryCache;

import java.util.ArrayList;
import java.util.List;
import java.util.Map;

import static java.util.Map.entry;
import static org.folio.orders.utils.HelperUtils.ORDER_CONFIG_MODULE_NAME;
import static org.folio.orders.utils.RequestContextUtil.createContextWithNewTenantId;
import static org.folio.rest.core.exceptions.ErrorCodes.MISSING_HOLDINGS_SOURCE_ID;
import static org.folio.rest.core.exceptions.ErrorCodes.MISSING_LOAN_TYPE;
import static org.folio.service.inventory.InventoryInstanceManager.CONTRIBUTOR_NAME;
import static org.folio.service.inventory.InventoryInstanceManager.CONTRIBUTOR_NAME_TYPE_ID;
import static org.folio.service.inventory.InventoryInstanceManager.INSTANCE_CONTRIBUTORS;
import static org.folio.service.inventory.InventoryInstanceManager.INSTANCE_DATE_OF_PUBLICATION;
import static org.folio.service.inventory.InventoryInstanceManager.INSTANCE_IDENTIFIERS;
import static org.folio.service.inventory.InventoryInstanceManager.INSTANCE_IDENTIFIER_TYPE_ID;
import static org.folio.service.inventory.InventoryInstanceManager.INSTANCE_IDENTIFIER_TYPE_VALUE;
import static org.folio.service.inventory.InventoryInstanceManager.INSTANCE_PUBLICATION;
import static org.folio.service.inventory.InventoryInstanceManager.INSTANCE_PUBLISHER;
import static org.folio.service.inventory.InventoryItemManager.COPY_NUMBER;
import static org.folio.service.inventory.InventoryItemManager.ITEM_ACCESSION_NUMBER;
import static org.folio.service.inventory.InventoryItemManager.ITEM_BARCODE;
import static org.folio.service.inventory.InventoryItemManager.ITEM_CHRONOLOGY;
import static org.folio.service.inventory.InventoryItemManager.ITEM_DISCOVERY_SUPPRESS;
import static org.folio.service.inventory.InventoryItemManager.ITEM_DISPLAY_SUMMARY;
import static org.folio.service.inventory.InventoryItemManager.ITEM_ENUMERATION;
import static org.folio.service.inventory.InventoryItemManager.ITEM_LEVEL_CALL_NUMBER;
import static org.folio.service.inventory.InventoryItemManager.ITEM_PURCHASE_ORDER_LINE_IDENTIFIER;
import static org.folio.service.inventory.InventoryItemManager.ITEM_STATUS;
import static org.folio.service.inventory.InventoryItemManager.ITEM_STATUS_NAME;

@UtilityClass
public class InventoryUtils {

  // mod-configuration: config names and default values
  public static final String CONFIG_NAME_HOLDINGS_SOURCE_NAME = "inventory-holdingsSourceName";
  public static final String CONFIG_NAME_INSTANCE_TYPE_CODE = "inventory-instanceTypeCode";
  public static final String CONFIG_NAME_INSTANCE_STATUS_CODE = "inventory-instanceStatusCode";
  public static final String CONFIG_NAME_LOAN_TYPE_NAME = "inventory-loanTypeName";
  public static final String DEFAULT_HOLDINGS_SOURCE_NAME = "FOLIO";
  public static final String DEFAULT_INSTANCE_TYPE_CODE = "zzz";
  public static final String DEFAULT_INSTANCE_STATUS_CODE = "temp";
  public static final String DEFAULT_LOAN_TYPE_NAME = "Can circulate";

  public static final String CONTRIBUTOR_NAME_TYPES = "contributorNameTypes";
  public static final String HOLDINGS_RECORDS = "holdingsRecords";
  public static final String HOLDINGS_RECORDS_BY_ID_ENDPOINT = "holdingsRecordsById";
  public static final String LOAN_TYPES = "loantypes";
  public static final String INSTANCE_STATUSES = "instanceStatuses";
  public static final String INSTANCE_TYPES = "instanceTypes";
  public static final String INSTANCES = "instances";
  public static final String ITEMS = "items";
  public static final String ITEM_BY_ID_ENDPOINT = "itemRecordById";
  public static final String REQUESTS = "requests";
  public static final String HOLDINGS_SOURCES = "holdingsRecordsSources";
  public static final String INSTANCE_RECORDS_BY_ID_ENDPOINT = "instanceRecordsById";

  public static final Map<String, String> INVENTORY_LOOKUP_ENDPOINTS;

  static {
    INVENTORY_LOOKUP_ENDPOINTS = Map.ofEntries(
      entry(CONTRIBUTOR_NAME_TYPES, "/contributor-name-types"),
      entry(HOLDINGS_RECORDS, "/holdings-storage/holdings"),
      entry(HOLDINGS_RECORDS_BY_ID_ENDPOINT, "/holdings-storage/holdings/{id}"),
      entry(LOAN_TYPES, "/loan-types?query=name==%s&limit=1"),
      entry(INSTANCE_STATUSES, "/instance-statuses?query=code==%s&limit=1"),
      entry(INSTANCE_TYPES, "/instance-types?query=code==%s"),
      entry(INSTANCES, "/inventory/instances"),
      entry(ITEMS, "/inventory/items"),
      entry(ITEM_BY_ID_ENDPOINT, "/inventory/items/{id}"),
      entry(REQUESTS, "/circulation/requests"),
      entry(HOLDINGS_SOURCES, "/holdings-sources?query=name==%s"),
      entry(INSTANCE_RECORDS_BY_ID_ENDPOINT, "/inventory/instances/{id}")
    );
  }

  private static final List<String> ITEM_STATUSES_FOR_TITLE_DELETE = List.of(ItemStatus.ON_ORDER.value(), ItemStatus.ORDER_CLOSED.value());

  public static Future<String> getLoanTypeId(CommonSettingsCache commonSettingsCache,
                                             InventoryCache inventoryCache,
                                             RequestContext requestContext) {
    return getEntryId(commonSettingsCache, inventoryCache, LOAN_TYPES, MISSING_LOAN_TYPE, requestContext)
      .map(jsonObject -> jsonObject.getString(LOAN_TYPES));
  }

  public static Future<String> getSourceId(CommonSettingsCache commonSettingsCache,
                                           InventoryCache inventoryCache,
                                           RequestContext requestContext) {
    return getEntryId(commonSettingsCache, inventoryCache, HOLDINGS_SOURCES, MISSING_HOLDINGS_SOURCE_ID, requestContext)
      .map(jsonObject -> jsonObject.getString(HOLDINGS_SOURCES));
  }

  public static Future<JsonObject> getEntryId(CommonSettingsCache commonSettingsCache,
                                              InventoryCache inventoryCache,
                                              String entryType,
                                              ErrorCodes errorCode,
                                              RequestContext requestContext) {
    Promise<JsonObject> promise = Promise.promise();
    getEntryTypeValue(commonSettingsCache, entryType, requestContext)
      .compose(entryTypeValue -> inventoryCache.getEntryId(entryType, entryTypeValue, requestContext))
      .onSuccess(promise::complete)
      .onFailure(t -> getEntryTypeValue(commonSettingsCache, entryType, requestContext)
        .onComplete(result -> {
          if (result.succeeded()) {
            promise.fail(new HttpException(500, buildErrorWithParameter(result.result(), errorCode)));

          } else {
            promise.fail(result.cause());
          }
        }));

    return promise.future();
  }

  private static Future<String> getEntryTypeValue(CommonSettingsCache commonSettingsCache,
                                                  String entryType,
                                                  RequestContext requestContext) {
    return commonSettingsCache.loadConfiguration(ORDER_CONFIG_MODULE_NAME, requestContext)
      .map(configs -> switch (entryType) {
        case HOLDINGS_SOURCES -> configs.getString(CONFIG_NAME_HOLDINGS_SOURCE_NAME, DEFAULT_HOLDINGS_SOURCE_NAME);
        case INSTANCE_TYPES -> configs.getString(CONFIG_NAME_INSTANCE_TYPE_CODE, DEFAULT_INSTANCE_TYPE_CODE);
        case INSTANCE_STATUSES -> configs.getString(CONFIG_NAME_INSTANCE_STATUS_CODE, DEFAULT_INSTANCE_STATUS_CODE);
        case LOAN_TYPES -> configs.getString(CONFIG_NAME_LOAN_TYPE_NAME, DEFAULT_LOAN_TYPE_NAME);
        default -> throw new IllegalArgumentException("Unexpected inventory entry type: " + entryType);
      });
  }

  public static Error buildErrorWithParameter(String value, ErrorCodes errorCode) {
    List<Parameter> parameters = new ArrayList<>();
    parameters.add(new Parameter().withKey("missingEntry").withValue(value));
    return errorCode.toError()
      .withParameters(parameters);
  }

  public static String getPublisher(JsonObject instance) {
    var publication = instance.getJsonArray(INSTANCE_PUBLICATION);
    if (publication == null || publication.isEmpty()) {
      return null;
    }
    return publication.getJsonObject(0).getString(INSTANCE_PUBLISHER);
  }

  public static String getPublicationDate(JsonObject instance) {
    var publication = instance.getJsonArray(INSTANCE_PUBLICATION);
    if (publication == null || publication.isEmpty()) {
      return null;
    }
    return publication.getJsonObject(0).getString(INSTANCE_DATE_OF_PUBLICATION);
  }

  public static List<ProductId> getProductIds(JsonObject instance) {
    JsonArray productIds = instance.getJsonArray(INSTANCE_IDENTIFIERS);
    if (productIds == null || productIds.isEmpty()) {
      return List.of();
    }
    return productIds
      .stream()
      .map(JsonObject.class::cast)
      .map(jsonObject -> new ProductId()
        .withProductId(jsonObject.getString(INSTANCE_IDENTIFIER_TYPE_VALUE))
        .withProductIdType(jsonObject.getString(INSTANCE_IDENTIFIER_TYPE_ID)))
      .toList();
  }

  public static List<Contributor> getContributors(JsonObject instance) {
    JsonArray contributors = instance.getJsonArray(INSTANCE_CONTRIBUTORS);
    if (contributors == null || contributors.isEmpty()) {
      return List.of();
    }
    return contributors
      .stream()
      .map(JsonObject.class::cast)
      .map(jsonObject -> new Contributor()
        .withContributor(jsonObject.getString(CONTRIBUTOR_NAME))
        .withContributorNameTypeId(jsonObject.getString(CONTRIBUTOR_NAME_TYPE_ID)))
      .toList();
  }

  public static void updateItemWithPieceFields(JsonObject item, Piece piece) {
    updateCommonItemFields(item,
      piece.getDisplaySummary(),
      piece.getEnumeration(),
      piece.getCopyNumber(),
      piece.getChronology(),
      piece.getBarcode(),
      piece.getAccessionNumber(),
      piece.getCallNumber());
  }

  public static void updateItemWithCheckinPieceFields(JsonObject item, CheckInPiece checkinPiece) {
    item.put(ITEM_STATUS, new JsonObject().put(ITEM_STATUS_NAME, checkinPiece.getItemStatus().value()));
    updateCommonItemFields(item,
      checkinPiece.getDisplaySummary(),
      checkinPiece.getEnumeration(),
      checkinPiece.getCopyNumber(),
      checkinPiece.getChronology(),
      checkinPiece.getBarcode(),
      checkinPiece.getAccessionNumber(),
      checkinPiece.getCallNumber());
  }

  public static void updateItemWithReceivedItemFields(PiecesHolder holder, JsonObject item, ReceivedItem receivedItem) {
    if (isOrderClosedOrPoLineCancelled(holder)) {
      receivedItem.withItemStatus(ReceivedItem.ItemStatus.ORDER_CLOSED);
    }
    item.put(ITEM_STATUS, new JsonObject().put(ITEM_STATUS_NAME, receivedItem.getItemStatus().value()));
    updateCommonItemFields(item,
      receivedItem.getDisplaySummary(),
      receivedItem.getEnumeration(),
      receivedItem.getCopyNumber(),
      receivedItem.getChronology(),
      receivedItem.getBarcode(),
      null, // ReceivedItem has no getAccessionNumber method
      receivedItem.getCallNumber());
  }

  private static void updateCommonItemFields(JsonObject item,
                                             String displaySummary,
                                             String enumeration,
                                             String copyNumber,
                                             String chronology,
                                             String barcode,
                                             String accessionNumber,
                                             String callNumber) {
    if (StringUtils.isNotEmpty(displaySummary)) {
      item.put(ITEM_DISPLAY_SUMMARY, displaySummary);
    }
    if (StringUtils.isNotEmpty(enumeration)) {
      item.put(ITEM_ENUMERATION, enumeration);
    }
    if (StringUtils.isNotEmpty(copyNumber)) {
      item.put(COPY_NUMBER, copyNumber);
    }
    if (StringUtils.isNotEmpty(chronology)) {
      item.put(ITEM_CHRONOLOGY, chronology);
    }
    if (StringUtils.isNotEmpty(barcode)) {
      item.put(ITEM_BARCODE, barcode);
    }
    if (StringUtils.isNotEmpty(accessionNumber)) {
      item.put(ITEM_ACCESSION_NUMBER, accessionNumber);
    }
    if (StringUtils.isNotEmpty(callNumber)) {
      item.put(ITEM_LEVEL_CALL_NUMBER, callNumber);
    }
  }

  private static boolean isOrderClosedOrPoLineCancelled(PiecesHolder holder) {
    var orderPoLinePair = holder.getPurchaseOrderPoLinePair();
    if (Objects.isNull(orderPoLinePair)) {
      return false;
    }
    var purchaseOrder = orderPoLinePair.getKey();
    var poLine = orderPoLinePair.getValue();
    return isPurchaseOrderClosedOrPoLineCancelled(purchaseOrder, poLine);
  }

  public static boolean isPurchaseOrderClosedOrPoLineCancelled(CompositePurchaseOrder purchaseOrder, PoLine poLine) {
    return purchaseOrder.getWorkflowStatus().equals(CompositePurchaseOrder.WorkflowStatus.CLOSED)
      || (poLine.getReceiptStatus().equals(PoLine.ReceiptStatus.CANCELLED)
      && poLine.getPaymentStatus().equals(PoLine.PaymentStatus.CANCELLED));
  }

  public static ItemRecreateConfig constructItemRecreateConfig(String receivingTenantId, RequestContext requestContext, boolean reuseInitialRequestContext) {
    if (Objects.isNull(receivingTenantId)) {
      return new ItemRecreateConfig(null, reuseInitialRequestContext ? requestContext : null);
    }
    return new ItemRecreateConfig(receivingTenantId, createContextWithNewTenantId(requestContext, receivingTenantId));
  }

  public static boolean allowItemRecreate(String srcTenantId, String dstTenantId) {
    return Objects.nonNull(srcTenantId) && Objects.nonNull(dstTenantId) && !srcTenantId.equals(dstTenantId);
  }

  /**
   * Only consider the holding for deletion if either:
   * - It has no items
   * - All items are associated with the current POL being processed and have status "On order" or "Order closed"
   *
   * @param items    List of item json objects
   * @param poLineId POL id of the title being deleted
   * @return true if all the items can be deleted
   */
  public static boolean canDeleteHoldingForTitleRemoval(List<JsonObject> items, String poLineId) {
    return items.isEmpty() || items.stream().allMatch(item -> canDeleteItemForTitleRemoval(item, poLineId));
  }

  /**
   * Only consider the item for deletion if both:
   * - It is associated with the current POL being processed
   * - It has status "On order" or "Order closed"
   *
   * @param item     Item json object
   * @param poLineId POL id of the title being deleted
   * @return true if all the items can be deleted
   */
  public static boolean canDeleteItemForTitleRemoval(JsonObject item, String poLineId) {
    var itemPolId = item.getString(ITEM_PURCHASE_ORDER_LINE_IDENTIFIER);
    var itemStatus = item.getJsonObject(ITEM_STATUS).getString(ITEM_STATUS_NAME);
    return StringUtils.equals(poLineId, itemPolId)
      && ITEM_STATUSES_FOR_TITLE_DELETE.contains(itemStatus);
  }

  public record ItemRecreateConfig(String tenantId, RequestContext context) {}

}
