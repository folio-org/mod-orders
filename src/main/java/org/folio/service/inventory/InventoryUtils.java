package org.folio.service.inventory;

import io.vertx.core.Future;
import io.vertx.core.Promise;
import io.vertx.core.json.JsonArray;
import io.vertx.core.json.JsonObject;
import java.util.Objects;
import org.apache.commons.lang3.StringUtils;
import org.folio.rest.core.exceptions.ErrorCodes;
import org.folio.rest.core.exceptions.HttpException;
import org.folio.rest.core.models.RequestContext;
import org.folio.rest.jaxrs.model.CheckInPiece;
import org.folio.rest.jaxrs.model.Contributor;
import org.folio.rest.jaxrs.model.Error;
import org.folio.rest.jaxrs.model.Parameter;
import org.folio.rest.jaxrs.model.Piece;
import org.folio.rest.jaxrs.model.ProductId;
import org.folio.rest.jaxrs.model.ReceivedItem;
import org.folio.service.caches.ConfigurationEntriesCache;
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
import static org.folio.service.inventory.InventoryItemManager.ITEM_STATUS;
import static org.folio.service.inventory.InventoryItemManager.ITEM_STATUS_NAME;

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

  private InventoryUtils() {
  }

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

  public static Future<String> getLoanTypeId(ConfigurationEntriesCache configurationEntriesCache,
                                             InventoryCache inventoryCache,
                                             RequestContext requestContext) {
    return getEntryId(configurationEntriesCache, inventoryCache, LOAN_TYPES, MISSING_LOAN_TYPE, requestContext)
      .map(jsonObject -> jsonObject.getString(LOAN_TYPES));
  }

  public static Future<String> getSourceId(ConfigurationEntriesCache configurationEntriesCache,
                                           InventoryCache inventoryCache,
                                           RequestContext requestContext) {
    return getEntryId(configurationEntriesCache, inventoryCache, HOLDINGS_SOURCES, MISSING_HOLDINGS_SOURCE_ID, requestContext)
      .map(jsonObject -> jsonObject.getString(HOLDINGS_SOURCES));
  }

  public static Future<JsonObject> getEntryId(ConfigurationEntriesCache configurationEntriesCache,
                                              InventoryCache inventoryCache,
                                              String entryType,
                                              ErrorCodes errorCode,
                                              RequestContext requestContext) {
    Promise<JsonObject> promise = Promise.promise();
    getEntryTypeValue(configurationEntriesCache, entryType, requestContext)
      .compose(entryTypeValue -> inventoryCache.getEntryId(entryType, entryTypeValue, requestContext))
      .onSuccess(promise::complete)
      .onFailure(t -> getEntryTypeValue(configurationEntriesCache, entryType, requestContext)
        .onComplete(result -> {
          if (result.succeeded()) {
            promise.fail(new HttpException(500, buildErrorWithParameter(result.result(), errorCode)));

          } else {
            promise.fail(result.cause());
          }
        }));

    return promise.future();
  }

  private static Future<String> getEntryTypeValue(ConfigurationEntriesCache configurationEntriesCache,
                                                  String entryType,
                                                  RequestContext requestContext) {
    return configurationEntriesCache.loadConfiguration(ORDER_CONFIG_MODULE_NAME, requestContext)
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
    if (StringUtils.isNotEmpty(piece.getDisplaySummary())) {
      item.put(ITEM_DISPLAY_SUMMARY, piece.getDisplaySummary());
    }
    if (StringUtils.isNotEmpty(piece.getEnumeration())) {
      item.put(ITEM_ENUMERATION, piece.getEnumeration());
    }
    if (StringUtils.isNotEmpty(piece.getCopyNumber())) {
      item.put(COPY_NUMBER, piece.getCopyNumber());
    }
    if (StringUtils.isNotEmpty(piece.getChronology())) {
      item.put(ITEM_CHRONOLOGY, piece.getChronology());
    }
    if (StringUtils.isNotEmpty(piece.getBarcode())) {
      item.put(ITEM_BARCODE, piece.getBarcode());
    }
    if (StringUtils.isNotEmpty(piece.getAccessionNumber())) {
      item.put(ITEM_ACCESSION_NUMBER, piece.getAccessionNumber());
    }
    if (StringUtils.isNotEmpty(piece.getCallNumber())) {
      item.put(ITEM_LEVEL_CALL_NUMBER, piece.getCallNumber());
    }
    if (piece.getDiscoverySuppress() != null) {
      item.put(ITEM_DISCOVERY_SUPPRESS, piece.getDiscoverySuppress());
    }
  }

  public static void updateItemWithCheckinPieceFields(JsonObject item, CheckInPiece checkinPiece) {
    item.put(ITEM_STATUS, new JsonObject().put(ITEM_STATUS_NAME, checkinPiece.getItemStatus().value()));

    if (StringUtils.isNotEmpty(checkinPiece.getDisplaySummary())) {
      item.put(ITEM_DISPLAY_SUMMARY, checkinPiece.getDisplaySummary());
    }
    if (StringUtils.isNotEmpty(checkinPiece.getEnumeration())) {
      item.put(ITEM_ENUMERATION, checkinPiece.getEnumeration());
    }
    if (StringUtils.isNotEmpty(checkinPiece.getCopyNumber())) {
      item.put(COPY_NUMBER, checkinPiece.getCopyNumber());
    }
    if (StringUtils.isNotEmpty(checkinPiece.getChronology())) {
      item.put(ITEM_CHRONOLOGY, checkinPiece.getChronology());
    }
    if (StringUtils.isNotEmpty(checkinPiece.getBarcode())) {
      item.put(ITEM_BARCODE, checkinPiece.getBarcode());
    }
    if (StringUtils.isNotEmpty(checkinPiece.getAccessionNumber())) {
      item.put(ITEM_ACCESSION_NUMBER, checkinPiece.getAccessionNumber());
    }
    if (StringUtils.isNotEmpty(checkinPiece.getCallNumber())) {
      item.put(ITEM_LEVEL_CALL_NUMBER, checkinPiece.getCallNumber());
    }
    if (checkinPiece.getDiscoverySuppress() != null) {
      item.put(ITEM_DISCOVERY_SUPPRESS, checkinPiece.getDiscoverySuppress());
    }
  }

  public static void updateItemWithReceivedItemFields(JsonObject item, ReceivedItem receivedItem) {
    item.put(ITEM_STATUS, new JsonObject().put(ITEM_STATUS_NAME, receivedItem.getItemStatus().value()));

    if (StringUtils.isNotEmpty(receivedItem.getDisplaySummary())) {
      item.put(ITEM_DISPLAY_SUMMARY, receivedItem.getDisplaySummary());
    }
    if (StringUtils.isNotEmpty(receivedItem.getEnumeration())) {
      item.put(ITEM_ENUMERATION, receivedItem.getEnumeration());
    }
    if (StringUtils.isNotEmpty(receivedItem.getCopyNumber())) {
      item.put(COPY_NUMBER, receivedItem.getCopyNumber());
    }
    if (StringUtils.isNotEmpty(receivedItem.getChronology())) {
      item.put(ITEM_CHRONOLOGY, receivedItem.getChronology());
    }
    if (StringUtils.isNotEmpty(receivedItem.getBarcode())) {
      item.put(ITEM_BARCODE, receivedItem.getBarcode());
    }
    if (StringUtils.isNotEmpty(receivedItem.getCallNumber())) {
      item.put(ITEM_LEVEL_CALL_NUMBER, receivedItem.getCallNumber());
    }
  }

  public static ItemRecreateConfig constructItemRecreateConfig(Piece piece, RequestContext requestContext, boolean reuseInitialRequestContext) {
    if (Objects.isNull(piece.getReceivingTenantId())) {
      return new ItemRecreateConfig(null, reuseInitialRequestContext ? requestContext : null);
    }
    var tenantId = piece.getReceivingTenantId();
    return new ItemRecreateConfig(tenantId, createContextWithNewTenantId(requestContext, tenantId));
  }

  public static ItemRecreateConfig constructItemRecreateConfig(String receivingTenantId, RequestContext requestContext, boolean reuseInitialRequestContext) {
    if (Objects.isNull(receivingTenantId)) {
      return new ItemRecreateConfig(null, reuseInitialRequestContext ? requestContext : null);
    }
    return new ItemRecreateConfig(receivingTenantId, createContextWithNewTenantId(requestContext, receivingTenantId));
  }

  public static boolean allowItemRecreate(ItemRecreateConfig srcConfig, ItemRecreateConfig dstConfig) {
    return Objects.nonNull(srcConfig.tenantId()) && Objects.nonNull(dstConfig.tenantId())
      && !srcConfig.tenantId().equals(dstConfig.tenantId());
  }

  public record ItemRecreateConfig(String tenantId, RequestContext context) {}
}
