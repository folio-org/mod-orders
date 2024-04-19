package org.folio.service.inventory;

import static java.util.Collections.singletonList;
import static java.util.Map.entry;
import static java.util.stream.Collectors.joining;
import static java.util.stream.Collectors.toList;
import static one.util.streamex.StreamEx.ofSubLists;
import static org.apache.commons.collections4.CollectionUtils.isNotEmpty;
import static org.folio.orders.utils.HelperUtils.ORDER_CONFIG_MODULE_NAME;
import static org.folio.orders.utils.HelperUtils.collectResultsOnSuccess;
import static org.folio.orders.utils.HelperUtils.convertIdsToCqlQuery;
import static org.folio.orders.utils.HelperUtils.encodeQuery;
import static org.folio.orders.utils.HelperUtils.extractId;
import static org.folio.orders.utils.HelperUtils.getFirstObjectFromResponse;
import static org.folio.orders.utils.HelperUtils.isProductIdsExist;
import static org.folio.rest.RestConstants.MAX_IDS_FOR_GET_RQ_15;
import static org.folio.rest.RestConstants.NOT_FOUND;
import static org.folio.rest.core.exceptions.ErrorCodes.BARCODE_IS_NOT_UNIQUE;
import static org.folio.rest.core.exceptions.ErrorCodes.HOLDINGS_BY_ID_NOT_FOUND;
import static org.folio.rest.core.exceptions.ErrorCodes.ITEM_CREATION_FAILED;
import static org.folio.rest.core.exceptions.ErrorCodes.MISSING_CONTRIBUTOR_NAME_TYPE;
import static org.folio.rest.core.exceptions.ErrorCodes.MISSING_HOLDINGS_SOURCE_ID;
import static org.folio.rest.core.exceptions.ErrorCodes.MISSING_INSTANCE_STATUS;
import static org.folio.rest.core.exceptions.ErrorCodes.MISSING_INSTANCE_TYPE;
import static org.folio.rest.core.exceptions.ErrorCodes.MISSING_LOAN_TYPE;
import static org.folio.rest.core.exceptions.ErrorCodes.PARTIALLY_RETURNED_COLLECTION;
import static org.folio.rest.jaxrs.model.CompositePoLine.OrderFormat.ELECTRONIC_RESOURCE;

import java.util.ArrayList;
import java.util.Collection;
import java.util.Collections;
import java.util.List;
import java.util.Map;
import java.util.Objects;
import java.util.Optional;
import java.util.concurrent.CompletionException;
import java.util.function.UnaryOperator;
import java.util.stream.Collectors;

import org.apache.commons.collections4.CollectionUtils;
import org.apache.commons.collections4.ListUtils;
import org.apache.commons.lang3.StringUtils;
import org.apache.logging.log4j.LogManager;
import org.apache.logging.log4j.Logger;
import org.folio.models.PieceItemPair;
import org.folio.models.PoLineUpdateHolder;
import org.folio.models.consortium.SharingInstance;
import org.folio.okapi.common.GenericCompositeFuture;
import org.folio.orders.utils.HelperUtils;
import org.folio.orders.utils.PoLineCommonUtil;
import org.folio.rest.core.RestClient;
import org.folio.rest.core.exceptions.ErrorCodes;
import org.folio.rest.core.exceptions.HttpException;
import org.folio.rest.core.exceptions.InventoryException;
import org.folio.rest.core.models.RequestContext;
import org.folio.rest.core.models.RequestEntry;
import org.folio.rest.jaxrs.model.CheckInPiece;
import org.folio.rest.jaxrs.model.CompositePoLine;
import org.folio.rest.jaxrs.model.Contributor;
import org.folio.rest.jaxrs.model.Eresource;
import org.folio.rest.jaxrs.model.Error;
import org.folio.rest.jaxrs.model.Location;
import org.folio.rest.jaxrs.model.Parameter;
import org.folio.rest.jaxrs.model.Piece;
import org.folio.rest.jaxrs.model.ProductId;
import org.folio.rest.jaxrs.model.ReceivedItem;
import org.folio.rest.jaxrs.model.Title;
import org.folio.rest.tools.utils.TenantTool;
import org.folio.service.caches.ConfigurationEntriesCache;
import org.folio.service.caches.InventoryCache;
import org.folio.service.consortium.ConsortiumConfigurationService;
import org.folio.service.consortium.SharingInstanceService;
import org.folio.service.pieces.PieceStorageService;

import io.vertx.core.CompositeFuture;
import io.vertx.core.Context;
import io.vertx.core.Future;
import io.vertx.core.Promise;
import io.vertx.core.json.JsonArray;
import io.vertx.core.json.JsonObject;
import one.util.streamex.IntStreamEx;
import one.util.streamex.StreamEx;

public class InventoryManager {
  private static final Logger logger = LogManager.getLogger(InventoryManager.class);

  public static final String IDENTIFIER_TYPES = "identifierTypes";
  public static final String SOURCE_FOLIO = "FOLIO";
  public static final String INSTANCE_SOURCE = "source";
  public static final String INSTANCE_TITLE = "title";
  public static final String INSTANCE_EDITIONS = "editions";
  public static final String INSTANCE_STATUS_ID = "statusId";
  public static final String INSTANCE_TYPE_ID = "instanceTypeId";
  public static final String INSTANCE_PUBLISHER = "publisher";
  public static final String INSTANCE_CONTRIBUTORS = "contributors";
  public static final String INSTANCE_DATE_OF_PUBLICATION = "dateOfPublication";
  public static final String INSTANCE_PUBLICATION = "publication";
  public static final String INSTANCE_IDENTIFIER_TYPE_ID = "identifierTypeId";
  public static final String INSTANCE_IDENTIFIERS = "identifiers";
  public static final String INSTANCE_IDENTIFIER_TYPE_VALUE = "value";
  public static final String HOLDING_INSTANCE_ID = "instanceId";
  public static final String HOLDING_PERMANENT_LOCATION_ID = "permanentLocationId";
  public static final String HOLDING_SOURCE = "sourceId";
  public static final String ITEM_HOLDINGS_RECORD_ID = "holdingsRecordId";
  public static final String ITEM_BARCODE = "barcode";
  public static final String ITEM_ACCESSION_NUMBER = "accessionNumber";
  public static final String ITEM_LEVEL_CALL_NUMBER = "itemLevelCallNumber";
  public static final String ITEM_STATUS = "status";
  public static final String ITEM_STATUS_NAME = "name";
  public static final String ITEM_MATERIAL_TYPE_ID = "materialTypeId";
  public static final String ITEM_MATERIAL_TYPE = "materialType";
  public static final String ITEM_PERMANENT_LOAN_TYPE_ID = "permanentLoanTypeId";
  public static final String ITEM_PURCHASE_ORDER_LINE_IDENTIFIER = "purchaseOrderLineIdentifier";
  public static final String ITEM_EFFECTIVE_LOCATION = "effectiveLocation";
  public static final String ITEM_ENUMERATION = "enumeration";
  public static final String ITEM_CHRONOLOGY = "chronology";
  public static final String ITEM_DISCOVERY_SUPPRESS = "discoverySuppress";
  public static final String ITEM_DISPLAY_SUMMARY = "displaySummary";
  public static final String CONTRIBUTOR_NAME = "name";
  public static final String CONTRIBUTOR_NAME_TYPE_ID = "contributorNameTypeId";
  public static final String CONTRIBUTOR_NAME_TYPES = "contributorNameTypes";
  public static final String INSTANCE_STATUSES = "instanceStatuses";
  public static final String COPY_NUMBER = "copyNumber";
  public static final String INSTANCE_TYPES = "instanceTypes";
  public static final String ITEMS = "items";
  public static final String LOAN_TYPES = "loantypes";
  public static final String REQUESTS = "requests";
  public static final String HOLDINGS_SOURCES = "holdingsRecordsSources";

  // mod-configuration: config names and default values
  public static final String CONFIG_NAME_INSTANCE_TYPE_CODE = "inventory-instanceTypeCode";
  public static final String CONFIG_NAME_INSTANCE_STATUS_CODE = "inventory-instanceStatusCode";
  public static final String CONFIG_NAME_LOAN_TYPE_NAME = "inventory-loanTypeName";
  public static final String CONFIG_NAME_HOLDINGS_SOURCE_NAME = "inventory-holdingsSourceName";
  public static final String DEFAULT_INSTANCE_TYPE_CODE = "zzz";
  public static final String DEFAULT_INSTANCE_STATUS_CODE = "temp";
  public static final String DEFAULT_LOAN_TYPE_NAME = "Can circulate";
  public static final String DEFAULT_HOLDINGS_SOURCE_NAME = "FOLIO";

  public static final String HOLDINGS_RECORDS = "holdingsRecords";
  public static final String HOLDINGS_RECORDS_BY_ID_ENDPOINT = "holdingsRecordsById";
  public static final String INSTANCES = "instances";
  public static final String INSTANCE_RECORDS_BY_ID_ENDPOINT = "instanceRecordsById";

  private static final String TENANT_SPECIFIC_KEY_FORMAT = "%s.%s.%s";
  private static final String LOOKUP_ITEM_QUERY = "purchaseOrderLineIdentifier==%s and holdingsRecordId==%s";
  private static final String ITEM_STOR_ENDPOINT = "/item-storage/items";
  public static final String ITEM_BY_ID_ENDPOINT = "itemRecordById";
  public static final String HOLDINGS_LOOKUP_QUERY = "instanceId==%s and permanentLocationId==%s";
  public static final String ID = "id";
  public static final String TOTAL_RECORDS = "totalRecords";
  public static final Map<String, String> INVENTORY_LOOKUP_ENDPOINTS;
  public static final String BUILDING_PIECE_MESSAGE = "Building {} {} piece(s) for PO Line with id={}";
  public static final String EFFECTIVE_LOCATION = "effectiveLocation";
  public static final String BARCODE_ALREADY_EXIST_ERROR = "lower(jsonb ->> 'barcode'::text) value already exists in table item";
  private final RestClient restClient;
  private final ConfigurationEntriesCache configurationEntriesCache;
  private final InventoryCache inventoryCache;
  private final InventoryService inventoryService;
  private final PieceStorageService pieceStorageService;
  private final SharingInstanceService sharingInstanceService;
  private final ConsortiumConfigurationService consortiumConfigurationService;

  public InventoryManager(RestClient restClient, ConfigurationEntriesCache configurationEntriesCache,
                          PieceStorageService pieceStorageService, InventoryCache inventoryCache, InventoryService inventoryService, SharingInstanceService sharingInstanceService, ConsortiumConfigurationService consortiumConfigurationService) {
    this.restClient = restClient;
    this.configurationEntriesCache = configurationEntriesCache;
    this.inventoryCache = inventoryCache;
    this.inventoryService = inventoryService;
    this.pieceStorageService = pieceStorageService;
    this.sharingInstanceService = sharingInstanceService;
    this.consortiumConfigurationService = consortiumConfigurationService;
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


  /**
   * Returns list of item records for specified id's.
   *
   * @param ids List of item id's
   * @return future with list of item records
   */
  public Future<List<JsonObject>> getItemRecordsByIds(List<String> ids, RequestContext requestContext) {
    String query = convertIdsToCqlQuery(ids);
    RequestEntry requestEntry = new RequestEntry(INVENTORY_LOOKUP_ENDPOINTS.get(ITEMS))
      .withQuery(query).withOffset(0).withLimit(ids.size());
    return restClient.getAsJsonObject(requestEntry, requestContext)
      .map(response -> extractEntities(response, ITEMS));
  }

  public Future<JsonObject> getItemRecordById(String itemId, boolean skipThrowNorFoundException, RequestContext requestContext) {
    RequestEntry requestEntry = new RequestEntry(INVENTORY_LOOKUP_ENDPOINTS.get(ITEM_BY_ID_ENDPOINT)).withId(itemId);
    return restClient.getAsJsonObject(requestEntry, skipThrowNorFoundException, requestContext);
  }

  /**
   * Returns list of requests for specified item.
   *
   * @param itemId id of Item
   * @return future with list of requests
   */
  public Future<Integer> getNumberOfRequestsByItemId(String itemId, RequestContext requestContext) {
    String query = String.format("(itemId==%s and status=\"*\")", itemId);
    RequestEntry requestEntry = new RequestEntry(INVENTORY_LOOKUP_ENDPOINTS.get(REQUESTS))
      .withQuery(query).withOffset(0).withLimit(0);
    return restClient.getAsJsonObject(requestEntry, requestContext)
      .map(this::extractTotalRecords);
  }

  /**
   * Returns list of item records for specified query.
   *
   * @param query item records query
   * @return future with list of item records
   */
  public Future<List<JsonObject>> getItemRecordsByQuery(String query, RequestContext requestContext) {
    RequestEntry requestEntry = new RequestEntry(INVENTORY_LOOKUP_ENDPOINTS.get(ITEMS))
      .withQuery(query).withOffset(0).withLimit(Integer.MAX_VALUE);
    return restClient.getAsJsonObject(requestEntry, requestContext)
      .map(response -> extractEntities(response, ITEMS));
  }

  public Future<Void> updateItem(JsonObject item, RequestContext requestContext) {
    RequestEntry requestEntry = new RequestEntry(INVENTORY_LOOKUP_ENDPOINTS.get(ITEM_BY_ID_ENDPOINT)).withId(item.getString(ID));
    return restClient.put(requestEntry, item, requestContext);
  }

  public Future<String> saveItem(JsonObject item, RequestContext requestContext) {
    return updateItem(item, requestContext)
      .map(v -> item.getString(ID));
  }

  /**
   * Wait for item creation requests completion and filter failed items if any
   *
   * @param itemRecords item record to be created
   * @return completable future with list of item id's
   */
  public Future<List<String>> updateItemRecords(List<JsonObject> itemRecords, RequestContext requestContext) {
    List<Future<String>> futures = new ArrayList<>(itemRecords.size());
    itemRecords.forEach(itemRecord -> futures.add(updateItem(itemRecord, requestContext).map(v -> itemRecord.getString(ID))));
    return collectResultsOnSuccess(futures);
  }

  public Future<Void> deleteItem(String id, boolean skipNotFoundException, RequestContext requestContext) {
    RequestEntry requestEntry = new RequestEntry(INVENTORY_LOOKUP_ENDPOINTS.get(ITEM_BY_ID_ENDPOINT)).withId(id);
    return restClient.delete(requestEntry, skipNotFoundException, requestContext);
  }

  public Future<List<Void>> deleteItems(List<String> itemIds, boolean skipNotFoundException, RequestContext requestContext) {
    List<Future<Void>> futures = new ArrayList<>(itemIds.size());
    itemIds.forEach(itemId -> futures.add(deleteItem(itemId, skipNotFoundException, requestContext)));
    return collectResultsOnSuccess(futures);
  }

  /**
   * Checks if the {@link ReceivedItem} has item status as "On order"
   *
   * @param receivedItem details specified by user upon receiving flow
   * @return {@code true} if the item status is "On order"
   */
  public boolean isOnOrderItemStatus(ReceivedItem receivedItem) {
    return ReceivedItem.ItemStatus.ON_ORDER == receivedItem.getItemStatus();
  }

  /**
   * Checks if the {@link ReceivedItem} has item status as "On order"
   *
   * @param checkinPiece details specified by user upon check-in flow
   * @return {@code true} if the item status is "On order"
   */
  public boolean isOnOrderPieceStatus(CheckInPiece checkinPiece) {
    return CheckInPiece.ItemStatus.ON_ORDER == checkinPiece.getItemStatus();
  }

  public Future<String> getOrCreateHoldingsRecord(String instanceId, Location location, RequestContext requestContext) {
    if (Objects.nonNull(location.getHoldingId())) {
      Context ctx = requestContext.getContext();
      String tenantId = TenantTool.tenantId(requestContext.getHeaders());

      String holdingId = location.getHoldingId();
      RequestEntry requestEntry = new RequestEntry(INVENTORY_LOOKUP_ENDPOINTS.get(HOLDINGS_RECORDS_BY_ID_ENDPOINT)).withId(holdingId);

      Future<String> holdingIdFuture;
      var holdingIdKey = String.format(TENANT_SPECIFIC_KEY_FORMAT, tenantId, "getOrCreateHoldingsRecord", holdingId);
      String holdingIdCached = ctx.get(holdingIdKey);

      if (Objects.nonNull(holdingIdCached)) {
        holdingIdFuture =  Future.succeededFuture(holdingIdCached);
      } else {
        holdingIdFuture = restClient.getAsJsonObject(requestEntry, requestContext)
          .onSuccess(id -> ctx.put(holdingIdKey, id))
          .map(holdingJson -> {
            var id = HelperUtils.extractId(holdingJson);
            ctx.put(holdingIdKey, id);
            return id;
          });
      }

      return holdingIdFuture.recover(throwable -> {
        handleHoldingsError(holdingId, throwable);
        return null;
      });
    } else {
      return createHoldingsRecordId(instanceId, location.getLocationId(), requestContext);
    }
  }

  public Future<JsonObject> getOrCreateHoldingsJsonRecord(Eresource eresource, String instanceId, Location location, RequestContext requestContext) {
    if (StringUtils.isNotEmpty(location.getHoldingId())) {
      String holdingId = location.getHoldingId();
      RequestEntry requestEntry = new RequestEntry(INVENTORY_LOOKUP_ENDPOINTS.get(HOLDINGS_RECORDS_BY_ID_ENDPOINT))
        .withId(holdingId);
      return restClient.getAsJsonObject(requestEntry, requestContext)
        .recover(throwable -> {
          handleHoldingsError(holdingId, throwable);
          return null;
        });
    } else if (Eresource.CreateInventory.NONE == eresource.getCreateInventory() || Eresource.CreateInventory.INSTANCE == eresource.getCreateInventory()) {
      if (location.getQuantityPhysical() != null && location.getQuantityPhysical() > 0) {
        return createHoldingsRecord(instanceId, location.getLocationId(), requestContext);
      }
    } else {
      return createHoldingsRecord(instanceId, location.getLocationId(), requestContext);
    }
    return Future.succeededFuture();
  }

  private static void handleHoldingsError(String holdingId, Throwable throwable) {
    if (throwable instanceof HttpException && ((HttpException) throwable).getCode() == 404) {
      String msg = String.format(HOLDINGS_BY_ID_NOT_FOUND.getDescription(), holdingId);
      Error error = new Error().withCode(HOLDINGS_BY_ID_NOT_FOUND.getCode()).withMessage(msg);
      throw new HttpException(NOT_FOUND, error);
    } else {
      throw new CompletionException(throwable.getCause());
    }
  }

  public Future<List<JsonObject>> getHoldingsByIds(List<String> holdingIds, RequestContext requestContext) {
    return getHoldingsByIds(holdingIds, requestContext, this::fetchHoldingsByHoldingIds);
  }

  public Future<List<JsonObject>> getHoldingsByIdsWithoutVerification(List<String> holdingIds, RequestContext requestContext) {
    return getHoldingsByIds(holdingIds, requestContext, this::fetchHoldingsByHoldingIdsWithoutVerification);
  }

  public Future<List<JsonObject>> getHoldingsByIds(List<String> holdingIds, RequestContext requestContext,
                                                   org.folio.service.orders.utils.HelperUtils.BiFunctionReturningFuture<List<String>, RequestContext, List<JsonObject>> biFunction) {
    return collectResultsOnSuccess(
      ofSubLists(new ArrayList<>(holdingIds), MAX_IDS_FOR_GET_RQ_15).map(ids -> biFunction.apply(ids, requestContext)).toList())
      .map(lists -> lists.stream()
        .flatMap(Collection::stream)
        .collect(Collectors.toList()));
  }

  public Future<JsonObject> getHoldingById(String holdingId, boolean skipNotFoundException, RequestContext requestContext) {
    if (StringUtils.isNotEmpty(holdingId)) {
      RequestEntry requestEntry = new RequestEntry(INVENTORY_LOOKUP_ENDPOINTS.get(HOLDINGS_RECORDS_BY_ID_ENDPOINT))
        .withId(holdingId);
      return restClient.getAsJsonObject(requestEntry, skipNotFoundException, requestContext);
    }
    return Future.succeededFuture(new JsonObject());
  }

  public Future<JsonObject> getHoldingById(String holdingId, RequestContext requestContext) {
    return getHoldingById(holdingId, false,requestContext);
  }

  public Future<List<JsonObject>> getHoldingRecords(String instanceId, List<String> locationIds, RequestContext requestContext) {
    List<Future<JsonObject>> futures = new ArrayList<>();
    locationIds.forEach(locationId -> futures.add(getFirstHoldingRecord(instanceId, locationId, requestContext)));
    return collectResultsOnSuccess(futures).map(holdings -> {
      if (logger.isDebugEnabled()) {
        String deletedIds = holdings.stream().map(holding -> holding.getString(ID)).collect(Collectors.joining(","));
        logger.debug(String.format("Holding ids : %s", deletedIds));
      }
      return holdings.stream().filter(Objects::nonNull).collect(toList());
    });
  }

  public Future<JsonObject> getFirstHoldingRecord(String instanceId, String locationId, RequestContext requestContext) {
    String query = String.format(HOLDINGS_LOOKUP_QUERY, instanceId, locationId);
    RequestEntry requestEntry = new RequestEntry(INVENTORY_LOOKUP_ENDPOINTS.get(HOLDINGS_RECORDS))
      .withQuery(query).withOffset(0).withLimit(Integer.MAX_VALUE);
    return restClient.getAsJsonObject(requestEntry, requestContext)
      .compose(holdings -> {
        if (!holdings.getJsonArray(HOLDINGS_RECORDS).isEmpty()) {
          return Future.succeededFuture(getFirstObjectFromResponse(holdings, HOLDINGS_RECORDS));
        }
        return Future.succeededFuture();
      });
  }

  private Future<JsonObject> createHoldingsRecord(String instanceId, String locationId, RequestContext requestContext) {
    return getSourceId(requestContext)
      .map(sourceId -> {
        JsonObject holdingsRecJson = new JsonObject();
        holdingsRecJson.put(HOLDING_INSTANCE_ID, instanceId);
        holdingsRecJson.put(HOLDING_PERMANENT_LOCATION_ID, locationId);
        holdingsRecJson.put(HOLDING_SOURCE, sourceId);
        return holdingsRecJson;
      })
      .compose(holdingsRecJson -> {
        RequestEntry requestEntry = new RequestEntry(INVENTORY_LOOKUP_ENDPOINTS.get(HOLDINGS_RECORDS));
        return restClient.postJsonObject(requestEntry, holdingsRecJson, requestContext);
      });
  }

  private Future<String> createHoldingsRecordId(String instanceId, String locationId, RequestContext requestContext) {
    return createHoldingsRecord(instanceId, locationId, requestContext)
      .map(holding-> holding.getString(ID));
  }

  public Future<Void> updateInstanceForHoldingRecords(List<JsonObject> holdingRecords, String instanceId, RequestContext requestContext) {
    if (isNotEmpty(holdingRecords)) {
      holdingRecords.forEach(holding -> holding.put(HOLDING_INSTANCE_ID, instanceId));
      return updateHoldingRecords(holdingRecords, requestContext);
    } else {
      return Future.succeededFuture();
    }
  }

  private Future<Void> updateHoldingRecords(List<JsonObject> holdingRecords, RequestContext requestContext) {
    return GenericCompositeFuture.join(holdingRecords.stream()
        .map(holdingRecord -> updateHolding(holdingRecord, requestContext))
        .collect(toList()))
        .mapEmpty();
  }

  private Future<Void> updateHolding(JsonObject holding, RequestContext requestContext) {
    RequestEntry requestEntry = new RequestEntry(INVENTORY_LOOKUP_ENDPOINTS.get(HOLDINGS_RECORDS_BY_ID_ENDPOINT)).withId(holding.getString(ID));
    return restClient.put(requestEntry, holding, requestContext);
  }

  public Future<String> getOrCreateHoldingRecordByInstanceAndLocation(String instanceId, Location location, RequestContext requestContext) {
    if (Objects.isNull(location.getLocationId())) {
      return getHoldingById(location.getHoldingId(), true, requestContext)
          .compose(holding -> {
            String locationId = holding.getString(HOLDING_PERMANENT_LOCATION_ID);
            return getFirstHoldingRecord(instanceId, locationId, requestContext)
                .compose(jsonHolding -> {
                  if(Objects.nonNull(jsonHolding)) {
                    String holdingId = jsonHolding.getString(ID);
                    return Future.succeededFuture(holdingId);
                  }
                  return createHoldingsRecordId(instanceId, locationId, requestContext);
                });
          });
    } else {
      return getFirstHoldingRecord(instanceId, location.getLocationId(), requestContext)
          .compose(jsonHolding -> {
            if(Objects.nonNull(jsonHolding)) {
              String holdingId = jsonHolding.getString(ID);
              return Future.succeededFuture(holdingId);
            }
            return createHoldingsRecordId(instanceId, location.getLocationId(), requestContext);
          });
    }
  }

  public Future<String> createHolding(String instanceId, Location location,  RequestContext requestContext) {
    if (Objects.isNull(location.getLocationId())) {
      return getHoldingById(location.getHoldingId(), true, requestContext)
          .compose(holding -> {
            String locationId = holding.getString(HOLDING_PERMANENT_LOCATION_ID);
            return createHoldingsRecordId(instanceId, locationId, requestContext);
          });
    } else {
      return createHoldingsRecordId(instanceId, location.getLocationId(), requestContext);
    }
  }

  /**
   * Handles Inventory items for passed list of locations. Items are either retrieved from Inventory or new ones are created
   * if no corresponding item records exist yet.
   * Returns list of {@link Piece} records with populated item id (and other info) corresponding to given PO line.
   *
   * @param compPOL  PO line to retrieve/create Item Records for
   * @param location list of location holdingId is associated with
   * @return future with list of piece objects
   */
  public Future<List<Piece>> handleItemRecords(CompositePoLine compPOL, Location location, RequestContext requestContext) {
    Map<Piece.Format, Integer> piecesWithItemsQuantities = HelperUtils.calculatePiecesWithItemIdQuantity(compPOL, List.of(location));
    int piecesWithItemsQty = IntStreamEx.of(piecesWithItemsQuantities.values()).sum();
    String polId = compPOL.getId();

    logger.debug("Handling {} items for PO Line with id={} and holdings with id={}", piecesWithItemsQty, polId,
        location.getHoldingId());
    if (piecesWithItemsQty == 0) {
      return Future.succeededFuture(Collections.emptyList());
    }

    // Search for already existing items
    return searchStorageExistingItems(compPOL.getId(), location.getHoldingId(), piecesWithItemsQty, requestContext)
      .compose(existingItems -> {
        List<Future<List<Piece>>> pieces = new ArrayList<>(Piece.Format.values().length);

        for (Map.Entry<Piece.Format, Integer> pieceEntry : piecesWithItemsQuantities.entrySet()) {
          Piece.Format pieceFormat = pieceEntry.getKey();
          Integer expectedQuantity = pieceEntry.getValue();

          // The expected quantity might be zero for particular piece format if the PO Line's order format is P/E Mix
          if (expectedQuantity > 0) {
            // Depending on piece format get already existing existingItemIds and send requests to create missing existingItemIds
            Piece pieceWithHoldingId = new Piece().withHoldingId(location.getHoldingId());

            var future = Future.succeededFuture().compose(v -> {
              List<String> existingItemIds;
              if (pieceFormat == Piece.Format.ELECTRONIC) {
                existingItemIds = getElectronicItemIds(compPOL, existingItems);
                return createMissingElectronicItems(compPOL, pieceWithHoldingId, expectedQuantity - existingItemIds.size(), requestContext)
                      .map(createdItemIds -> buildPieces(location, polId, pieceFormat, createdItemIds, existingItemIds));
              } else {
                existingItemIds = getPhysicalItemIds(compPOL, existingItems);
                return createMissingPhysicalItems(compPOL, pieceWithHoldingId, expectedQuantity - existingItemIds.size(), requestContext)
                      .map(createdItemIds -> buildPieces(location, polId, pieceFormat, createdItemIds, existingItemIds));
              }
            });
            // Build piece records once new existingItemIds are created
            pieces.add(future);
          }
        }

        // Wait for all items to be created and corresponding pieces are built
        return collectResultsOnSuccess(pieces)
          .map(results -> {
          validateItemsCreation(compPOL.getId(), pieces.size(), results.size());
          return results.stream()
            .flatMap(List::stream)
            .collect(toList());
        });
      });
  }

  private List<Piece> buildPieces(Location location, String polId, Piece.Format pieceFormat, List<String> createdItemIds,
    List<String> existingItemIds) {
    List<String> itemIds = ListUtils.union(createdItemIds, existingItemIds);
    logger.info(BUILDING_PIECE_MESSAGE, itemIds.size(), pieceFormat, polId);
    return StreamEx.of(itemIds).map(itemId -> openOrderBuildPiece(polId, itemId, pieceFormat, location)).toList();
  }

  private Piece openOrderBuildPiece(String polId, String itemId, Piece.Format pieceFormat, Location location) {
    if (location.getHoldingId() != null) {
      return new Piece().withFormat(pieceFormat)
        .withItemId(itemId)
        .withPoLineId(polId)
        .withHoldingId(location.getHoldingId());
    } else {
      return new Piece().withFormat(pieceFormat)
        .withItemId(itemId)
        .withPoLineId(polId)
        .withLocationId(location.getLocationId());
    }
  }

  /**
   * Handles Inventory items for passed list of locations. Items are either retrieved from Inventory or new ones are created
   * if no corresponding item records exist yet.
   * Returns list of {@link Piece} records with populated item id (and other info) corresponding to given PO line.
   *
   * @param compPOL PO line to retrieve/create Item Records for
   * @param holder  pair of new location provided from POl and location from storage
   * @return future with list of piece objects
   */
  public Future<List<Piece>> handleItemRecords(CompositePoLine compPOL, PoLineUpdateHolder holder, RequestContext requestContext) {
    List<Location> polLocations = compPOL.getLocations().stream()
      .filter(location -> location.getLocationId().equals(holder.getNewLocationId()))
      .collect(toList());
    Map<Piece.Format, Integer> piecesWithItemsQuantities = HelperUtils.calculatePiecesWithItemIdQuantity(compPOL, polLocations);
    int piecesWithItemsQty = IntStreamEx.of(piecesWithItemsQuantities.values()).sum();
    String polId = compPOL.getId();

    logger.debug("Handling {} items for PO Line with id={} and holdings with id={}", piecesWithItemsQty, polId, holder.getOldHoldingId());
    if (piecesWithItemsQty == 0) {
      return Future.succeededFuture(Collections.emptyList());
    }
    return pieceStorageService.getExpectedPiecesByLineId(compPOL.getId(), requestContext)
      .map(existingPieces -> {
        List<Piece> needUpdatePieces = new ArrayList<>();
        List<Piece> pieces = existingPieces.getPieces().stream()
          .filter(piece -> piece.getLocationId().equals(holder.getOldLocationId()))
          .map(piece -> piece.withLocationId(holder.getNewLocationId()))
          .collect(toList());
        if (!pieces.isEmpty()) {
          needUpdatePieces.addAll(pieces);
        }
        return needUpdatePieces;
      })
      .compose(needUpdatePieces -> {
        if (!needUpdatePieces.isEmpty()) {
          return getItemRecordsByIds(needUpdatePieces.stream().map(Piece::getItemId)
            .collect(toList()), requestContext)
            .map(items -> buildPieceItemPairList(needUpdatePieces, items));
        }
        return Future.succeededFuture(Collections.<PieceItemPair>emptyList());
      })
      .compose(pieceItemPairs -> {
        List<Future<String>> updatedItemIds = new ArrayList<>(pieceItemPairs.size());
        pieceItemPairs.forEach(pair -> {
          JsonObject item = pair.getItem();
          if (isLocationContainsItemLocation(polLocations, item)) {
            item.put(ITEM_HOLDINGS_RECORD_ID, holder.getNewHoldingId());
            updatedItemIds.add(saveItem(item, requestContext));
          }
        });
        // Wait for all items to be created and corresponding updatedItemIds are built
        return collectResultsOnSuccess(updatedItemIds)
          .map(results -> {
            validateItemsCreation(compPOL.getId(), updatedItemIds.size(), results.size());
            return pieceItemPairs.stream().map(PieceItemPair::getPiece).collect(toList());
          });
      });
  }

  boolean isLocationContainsItemLocation(List<Location> polLocations, JsonObject item) {
    return item != null && polLocations.stream().noneMatch(location -> location.getLocationId().equals(item.getJsonObject(EFFECTIVE_LOCATION).getString(ID)));
  }

  private List<PieceItemPair> buildPieceItemPairList(List<Piece> needUpdatePieces, List<JsonObject> items) {
    return needUpdatePieces.stream()
      .map(piece -> {
        PieceItemPair pieceItemPair = new PieceItemPair().withPiece(piece);
        items.stream().filter(item -> item.getString(ID).equals(piece.getItemId()))
          .findAny()
          .ifPresent(pieceItemPair::withItem);
        return pieceItemPair;
      }).collect(toList());
  }

  private List<String> getPhysicalItemIds(CompositePoLine compPOL, List<JsonObject> existingItems) {
    return getItemsByMaterialType(existingItems, compPOL.getPhysical().getMaterialType());
  }

  private List<String> getElectronicItemIds(CompositePoLine compPOL, List<JsonObject> existingItems) {
    return getItemsByMaterialType(existingItems, compPOL.getEresource().getMaterialType());
  }

  private List<String> getItemsByMaterialType(List<JsonObject> existingItems, String materialTypeId) {
    return existingItems
      .stream()
      .filter(item -> {
        String typeId = item.getString(ITEM_MATERIAL_TYPE_ID);
        if (typeId == null) {
          typeId = item.getJsonObject(ITEM_MATERIAL_TYPE).getString(ID);
        }
        return materialTypeId.equals(typeId);
      })
      .map(HelperUtils::extractId)
      .collect(toList());
  }

  /**
   * Returns Id of the Instance Record corresponding to given PO line.
   * Instance record is either retrieved from Inventory or a new one is created if no corresponding Record exists.
   *
   * @param compPOL PO line to retrieve Instance Record Id for
   * @return future with Instance Id
   */
  public Future<String> getInstanceRecord(CompositePoLine compPOL, boolean isInstanceMatchingDisabled, RequestContext requestContext) {
    // proceed with new Instance Record creation if no productId is provided
    if (!isProductIdsExist(compPOL) || isInstanceMatchingDisabled) {
      return createInstanceRecord(compPOL, requestContext);
    }

    String query = compPOL.getDetails().getProductIds().stream()
      .map(this::buildProductIdQuery)
      .collect(joining(" or "));
    RequestEntry requestEntry = new RequestEntry(INVENTORY_LOOKUP_ENDPOINTS.get(INSTANCES))
      .withQuery(query).withOffset(0).withLimit(Integer.MAX_VALUE);
    return restClient.getAsJsonObject(requestEntry, requestContext)
      .compose(instances -> {
        if (!instances.getJsonArray(INSTANCES).isEmpty()) {
          return Future.succeededFuture(extractId(getFirstObjectFromResponse(instances, INSTANCES)));
        }
        return createInstanceRecord(compPOL, requestContext);
      });
  }

  /**
   * Creates Instance Record in Inventory and returns its Id.
   *
   * @param compPOL PO line to create Instance Record for
   * @return id of newly created Instance Record
   */
  private Future<String> createInstanceRecord(CompositePoLine compPOL, RequestContext requestContext) {
    logger.debug("Start processing instance record");
    JsonObject lookupObj = new JsonObject();
    Future<Void> instanceTypeFuture = getEntryId(INSTANCE_TYPES, MISSING_INSTANCE_TYPE, requestContext)
      .onSuccess(lookupObj::mergeIn)
      .mapEmpty();

    Future<Void> statusFuture = getEntryId(INSTANCE_STATUSES, MISSING_INSTANCE_STATUS, requestContext)
      .onSuccess(lookupObj::mergeIn)
      .mapEmpty();

    Future<Void> contributorNameTypeIdFuture = verifyContributorNameTypesExist(compPOL.getContributors(), requestContext);

    return CompositeFuture.join(instanceTypeFuture, statusFuture, contributorNameTypeIdFuture)
      .map(v -> buildInstanceRecordJsonObject(compPOL, lookupObj))
      .compose(instanceRecJson -> {
        logger.debug("Instance record to save : {}", instanceRecJson);
        RequestEntry requestEntry = new RequestEntry(INVENTORY_LOOKUP_ENDPOINTS.get(INSTANCES));
        return restClient.postJsonObjectAndGetId(requestEntry, instanceRecJson, requestContext);
      });
  }

  public Future<Void> verifyContributorNameTypesExist(List<Contributor> contributors, RequestContext requestContext) {
    List<String> ids = contributors.stream()
      .map(contributor -> contributor.getContributorNameTypeId().toLowerCase())
      .distinct()
      .collect(toList());

    return getContributorNameTypes(ids, requestContext)
      .map(contributorNameTypes -> {
        List<String> retrievedIds = contributorNameTypes.stream()
          .map(o -> o.getString(ID).toLowerCase())
          .collect(toList());
        if (retrievedIds.size() != ids.size()) {
          ids.removeAll(retrievedIds);
          throw new HttpException(500, buildErrorWithParameter(String.join(", ", ids), MISSING_CONTRIBUTOR_NAME_TYPE));
        }
        return null;
      });
  }

  private Future<List<JsonObject>> getContributorNameTypes(List<String> ids, RequestContext requestContext) {
    return collectResultsOnSuccess(StreamEx
      .ofSubLists(ids, MAX_IDS_FOR_GET_RQ_15)
      .map(idChunk -> getContributorNameTypeByIds(idChunk, requestContext))
      .toList())
      .map(lists -> StreamEx.of(lists).toFlatList(contributorNameTypes -> contributorNameTypes));
  }

  private Future<List<JsonObject>> getContributorNameTypeByIds(List<String> ids, RequestContext requestContext) {
    String query = convertIdsToCqlQuery(ids);
    RequestEntry requestEntry = new RequestEntry(INVENTORY_LOOKUP_ENDPOINTS.get(CONTRIBUTOR_NAME_TYPES))
      .withQuery(query).withOffset(0).withLimit(ids.size());
    return restClient.getAsJsonObject(requestEntry, requestContext)
      .map(entries -> entries.getJsonArray(CONTRIBUTOR_NAME_TYPES).stream()
        .map(JsonObject::mapFrom)
        .collect(Collectors.toList())
      )
       .recover(e -> {
        logger.error("The issue happened getting contributor name types", e);
        throw new CompletionException(e.getCause());
      });
  }

  public Future<JsonObject> getEntryId(String entryType, ErrorCodes errorCode, RequestContext requestContext) {
    Promise<JsonObject> promise = Promise.promise();
    getEntryTypeValue(entryType, requestContext)
      .compose(entryTypeValue -> inventoryCache.getEntryId(entryType, entryTypeValue, requestContext))
      .onSuccess(promise::complete)
      .onFailure(t -> getEntryTypeValue(entryType, requestContext)
        .onComplete(result -> {
          if (result.succeeded()) {
            promise.fail(new HttpException(500, buildErrorWithParameter(result.result(), errorCode)));

          } else {
            promise.fail(result.cause());
          }
        }));

    return promise.future();
  }

  private Error buildErrorWithParameter(String value, ErrorCodes errorCode) {
    List<Parameter> parameters = new ArrayList<>();
    parameters.add(new Parameter().withKey("missingEntry").withValue(value));
    return errorCode.toError()
      .withParameters(parameters);
  }

  public String buildProductIdQuery(ProductId productId) {
    return String.format("(identifiers =/@identifierTypeId=\"%s\"/@value \"%s\")",
      productId.getProductIdType(), productId.getProductId());
  }

  public JsonObject buildInstanceRecordJsonObject(CompositePoLine compPOL, JsonObject lookupObj) {
    JsonObject instance = new JsonObject();

    // MODORDERS-145 The Source and source code are required by schema
    instance.put(INSTANCE_SOURCE, SOURCE_FOLIO);
    instance.put(INSTANCE_TITLE, compPOL.getTitleOrPackage());

    if (compPOL.getEdition() != null) {
      instance.put(INSTANCE_EDITIONS, new JsonArray(singletonList(compPOL.getEdition())));
    }
    instance.put(INSTANCE_STATUS_ID, lookupObj.getString(INSTANCE_STATUSES));
    instance.put(INSTANCE_TYPE_ID, lookupObj.getString(INSTANCE_TYPES));

    if (compPOL.getPublisher() != null || compPOL.getPublicationDate() != null) {
      JsonObject publication = new JsonObject();
      publication.put(INSTANCE_PUBLISHER, compPOL.getPublisher());
      publication.put(INSTANCE_DATE_OF_PUBLICATION, compPOL.getPublicationDate());
      instance.put(INSTANCE_PUBLICATION, new JsonArray(singletonList(publication)));
    }

    if (isNotEmpty(compPOL.getContributors())) {
      List<JsonObject> contributors = compPOL.getContributors().stream().map(compPolContributor -> {
        JsonObject invContributor = new JsonObject();
        invContributor.put(CONTRIBUTOR_NAME_TYPE_ID, compPolContributor.getContributorNameTypeId());
        invContributor.put(CONTRIBUTOR_NAME, compPolContributor.getContributor());
        return invContributor;
      }).collect(toList());
      instance.put(INSTANCE_CONTRIBUTORS, contributors);
    }

    if (isProductIdsExist(compPOL)) {
      List<JsonObject> identifiers =
        compPOL.getDetails()
          .getProductIds()
          .stream()
          .map(pId -> {
            JsonObject identifier = new JsonObject();
            identifier.put(INSTANCE_IDENTIFIER_TYPE_ID, pId.getProductIdType());
            identifier.put(INSTANCE_IDENTIFIER_TYPE_VALUE, pId.getProductId());
            return identifier;
          })
          .collect(toList());
      instance.put(INSTANCE_IDENTIFIERS, new JsonArray(identifiers));
    }
    return instance;
  }

  public JsonObject buildInstanceRecordJsonObject(Title title, JsonObject lookupObj) {
    JsonObject instance = new JsonObject();

    // MODORDERS-145 The Source and source code are required by schema
    instance.put(INSTANCE_SOURCE, SOURCE_FOLIO);
    instance.put(INSTANCE_TITLE, title.getTitle());

    if (title.getEdition() != null) {
      instance.put(INSTANCE_EDITIONS, new JsonArray(singletonList(title.getEdition())));
    }
    instance.put(INSTANCE_STATUS_ID, lookupObj.getString(INSTANCE_STATUSES));
    instance.put(INSTANCE_TYPE_ID, lookupObj.getString(INSTANCE_TYPES));

    if (title.getPublisher() != null || title.getPublishedDate() != null) {
      JsonObject publication = new JsonObject();
      publication.put(INSTANCE_PUBLISHER, title.getPublisher());
      publication.put(INSTANCE_DATE_OF_PUBLICATION, title.getPublishedDate());
      instance.put(INSTANCE_PUBLICATION, new JsonArray(singletonList(publication)));
    }

    List<Contributor> titleContributors = title.getContributors();
    if (isNotEmpty(titleContributors)) {
      List<JsonObject> contributors = titleContributors.stream().map(compPolContributor -> {
        JsonObject invContributor = new JsonObject();
        invContributor.put(CONTRIBUTOR_NAME_TYPE_ID, compPolContributor.getContributorNameTypeId());
        invContributor.put(CONTRIBUTOR_NAME, compPolContributor.getContributor());
        return invContributor;
      }).collect(toList());
      instance.put(INSTANCE_CONTRIBUTORS, contributors);
    }

    List<ProductId> productIds = title.getProductIds();
    if (CollectionUtils.isNotEmpty(productIds)) {
      List<JsonObject> identifiers =
        productIds.stream()
          .map(pId -> {
            JsonObject identifier = new JsonObject();
            identifier.put(INSTANCE_IDENTIFIER_TYPE_ID, pId.getProductIdType());
            identifier.put(INSTANCE_IDENTIFIER_TYPE_VALUE, pId.getProductId());
            return identifier;
          })
          .collect(toList());
      instance.put(INSTANCE_IDENTIFIERS, new JsonArray(identifiers));
    }
    return instance;
  }

  public Future<List<JsonObject>> getItemsByHoldingId(String holdingId, RequestContext requestContext) {
    String query = String.format("holdingsRecordId==%s", holdingId);
    RequestEntry requestEntry = new RequestEntry(INVENTORY_LOOKUP_ENDPOINTS.get(ITEMS)).withQuery(query)
      .withOffset(0).withLimit(Integer.MAX_VALUE);
    return restClient.getAsJsonObject(requestEntry, requestContext)
      .map(itemsCollection -> {
        List<JsonObject> items = extractEntities(itemsCollection, ITEMS);
        logger.debug("{} existing items found for holding with '{}' id", items.size(), holdingId);
        return items;
      });
  }

  public Future<List<JsonObject>> getItemsByHoldingIdAndOrderLineId(String holdingId, String purchaseOrderLineId, RequestContext requestContext) {
    String query = String.format("holdingsRecordId==%s and purchaseOrderLineIdentifier==%s", holdingId, purchaseOrderLineId);
    return getItemRecordsByQuery(query, requestContext);
  }

  /**
   * Return id of created  Item
   */
  public Future<String> openOrderCreateItemRecord(CompositePoLine compPOL, String holdingId, RequestContext requestContext) {
    final int ITEM_QUANTITY = 1;
    logger.debug("Handling {} items for PO Line and holdings with id={}", ITEM_QUANTITY, holdingId);
    Promise<String> itemFuture = Promise.promise();
    try {
      Piece pieceWithHoldingId = new Piece().withHoldingId(holdingId);
      if (compPOL.getOrderFormat() == ELECTRONIC_RESOURCE) {
        createMissingElectronicItems(compPOL, pieceWithHoldingId, ITEM_QUANTITY, requestContext)
          .onSuccess(idS -> itemFuture.complete(idS.get(0)))
          .onFailure(itemFuture::fail);
      } else {
        createMissingPhysicalItems(compPOL, pieceWithHoldingId, ITEM_QUANTITY, requestContext)
          .onSuccess(idS -> itemFuture.complete(idS.get(0)))
          .onFailure(itemFuture::fail);
      }
    } catch (Exception e) {
      itemFuture.fail(e);
    }
    return itemFuture.future();
  }

  /**
   * Creates Items in the inventory based on the PO line data.
   *
   * @param compPOL   PO line to create Instance Record for
   * @param piece base piece to build item
   * @param quantity  expected number of items to create
   * @return id of newly created Instance Record
   */
  public Future<List<String>> createMissingElectronicItems(CompositePoLine compPOL, Piece piece, int quantity, RequestContext requestContext) {
    if (quantity > 0) {
      String holdingId = piece.getHoldingId();
      return buildElectronicItemRecordJsonObject(compPOL, holdingId, requestContext)
        .map(item -> {
          updateItemWithPieceFields(piece, item);
          return item;
        })
        .compose(item -> {
          logger.debug("Posting {} electronic item(s) for PO Line with '{}' id", quantity, compPOL.getId());
          return createItemRecords(item, quantity, requestContext);
        });
    } else {
      return Future.succeededFuture(Collections.emptyList());
    }
  }

  /**
   * Creates Items in the inventory based on the PO line data.
   *
   * @param compPOL   PO line to create Instance Record for
   * @param piece base piece to build item
   * @param quantity  expected number of items to create
   * @return id of newly created Instance Record
   */
  public Future<List<String>> createMissingPhysicalItems(CompositePoLine compPOL, Piece piece, int quantity,
      RequestContext requestContext) {
    if (quantity > 0) {
      String holdingId = piece.getHoldingId();
      return buildPhysicalItemRecordJsonObject(compPOL, holdingId, requestContext)
        .map(item -> {
          updateItemWithPieceFields(piece, item);
          return item;
        })
        .compose(item -> {
          logger.debug("Posting {} physical item(s) for PO Line with '{}' id", quantity, compPOL.getId());
          return createItemRecords(item, quantity, requestContext);
        });
    } else {
      return Future.succeededFuture(Collections.emptyList());
    }
  }

  public Integer extractTotalRecords(JsonObject json) {
    return json.getInteger(TOTAL_RECORDS);
  }

  private Future<String> getLoanTypeId(RequestContext requestContext) {
    return getEntryId(LOAN_TYPES, MISSING_LOAN_TYPE, requestContext)
      .map(jsonObject -> jsonObject.getString(LOAN_TYPES));
  }

  private Future<String> getSourceId(RequestContext requestContext) {
    return getEntryId(HOLDINGS_SOURCES, MISSING_HOLDINGS_SOURCE_ID, requestContext)
      .map(jsonObject -> jsonObject.getString(HOLDINGS_SOURCES));
  }

  public Future<Void> updateItemWithPieceFields(Piece piece, RequestContext requestContext) {
    if (piece.getItemId() == null || piece.getPoLineId() == null) {
      return Future.succeededFuture();
    }
    String itemId = piece.getItemId();
    String poLineId = piece.getPoLineId();
    return getItemRecordById(itemId, true, requestContext)
      .compose(item -> {
        if (poLineId != null && item != null && !item.isEmpty()) {
          updateItemWithPieceFields(piece, item);
          return updateItem(item, requestContext);
        }
        return Future.succeededFuture();
      });
  }

  public Future<JsonObject> searchInstancesByProducts(List<ProductId> productIds, RequestContext requestContext) {
    String query = productIds.stream()
      .map(this::buildProductIdQuery)
      .collect(joining(" or "));

    // query contains special characters so must be encoded before submitting
    String endpoint = inventoryService.buildInventoryLookupEndpoint(INSTANCES, encodeQuery(query));
    return restClient.getAsJsonObject(endpoint, false, requestContext);
  }

  public Future<String> createInstanceRecord(Title title, RequestContext requestContext) {
    JsonObject lookupObj = new JsonObject();
    Future<Void> instanceTypeFuture = getEntryId(INSTANCE_TYPES, MISSING_INSTANCE_TYPE, requestContext)
      .onSuccess(lookupObj::mergeIn)
      .mapEmpty();

    Future<Void> statusFuture = getEntryId(INSTANCE_STATUSES, MISSING_INSTANCE_STATUS, requestContext)
      .onSuccess(lookupObj::mergeIn)
      .mapEmpty();

    Future<Void> contributorNameTypeIdFuture = verifyContributorNameTypesExist(title.getContributors(), requestContext);

    return CompositeFuture.join(instanceTypeFuture, statusFuture, contributorNameTypeIdFuture)
      .map(cf -> buildInstanceRecordJsonObject(title, lookupObj))
      .compose(instanceJson -> createInstance(instanceJson, requestContext));
  }

  public Future<String> createInstance(JsonObject instanceRecJson, RequestContext requestContext) {
    RequestEntry requestEntry = new RequestEntry(INVENTORY_LOOKUP_ENDPOINTS.get(INSTANCES));
    return restClient.postJsonObjectAndGetId(requestEntry, instanceRecJson, requestContext);
  }

  public Future<JsonObject> getInstanceById(String instanceId, boolean skipNotFoundException, RequestContext requestContext) {
    RequestEntry requestEntry = new RequestEntry(INVENTORY_LOOKUP_ENDPOINTS.get(INSTANCE_RECORDS_BY_ID_ENDPOINT)).withId(instanceId);
    return restClient.getAsJsonObject(requestEntry, skipNotFoundException, requestContext);
  }

  public Future<Void> deleteHoldingById(String holdingId, boolean skipNotFoundException, RequestContext requestContext) {
    if (StringUtils.isNotEmpty(holdingId)) {
      RequestEntry requestEntry = new RequestEntry(INVENTORY_LOOKUP_ENDPOINTS.get(HOLDINGS_RECORDS_BY_ID_ENDPOINT))
        .withId(holdingId);
      return restClient.delete(requestEntry, skipNotFoundException, requestContext);
    }
    return Future.succeededFuture();
  }

  public Future<List<JsonObject>> getItemsByPoLineIdsAndStatus(List<String> poLineIds, String itemStatus, RequestContext requestContext) {
    logger.debug("getItemsByStatus start");
    List<Future<List<JsonObject>>> futures = StreamEx
      .ofSubLists(poLineIds, MAX_IDS_FOR_GET_RQ_15)
      .map(ids -> {
        String query = String.format("status.name==%s and %s", itemStatus, HelperUtils.convertFieldListToCqlQuery(ids, InventoryManager.ITEM_PURCHASE_ORDER_LINE_IDENTIFIER, true));
        return getItemRecordsByQuery(query, requestContext);
      })
      .toList();

    return collectResultsOnSuccess(futures)
      .map(lists -> StreamEx.of(lists).toFlatList(jsonObjects -> jsonObjects));
  }

  public Future<CompositePoLine> openOrderHandleInstance(CompositePoLine compPOL, boolean isInstanceMatchingDisabled, RequestContext requestContext) {
    if (compPOL.getInstanceId() != null) {
      return Future.succeededFuture(compPOL);
    } else {
      return getInstanceRecord(compPOL, isInstanceMatchingDisabled, requestContext)
        .map(compPOL::withInstanceId);
    }
  }

  public Future<Title> openOrderHandlePackageLineInstance(Title title, boolean isInstanceMatchingDisabled, RequestContext requestContext) {
    if (title.getInstanceId() != null) {
      return Future.succeededFuture(title);
    } else {
      return getOrCreateInstanceRecord(title, isInstanceMatchingDisabled,requestContext)
        .map(title::withInstanceId);
    }
  }

  public Future<String> getOrCreateInstanceRecord(Title title, RequestContext requestContext) {
    return getOrCreateInstanceRecord(title, false, requestContext);
  }

  public Future<String> getOrCreateInstanceRecord(Title title, boolean isInstanceMatchingDisabled, RequestContext requestContext) {
    if (CollectionUtils.isEmpty(title.getProductIds()) || isInstanceMatchingDisabled) {
      return createInstanceRecord(title, requestContext);
    }

    return searchInstancesByProducts(title.getProductIds(), requestContext)
      .compose(instances -> {
        if (!instances.getJsonArray(INSTANCES).isEmpty()) {
          String instanceId = getFirstObjectFromResponse(instances, INSTANCES).getString(ID);
          return Future.succeededFuture(instanceId);
        }
        return createInstanceRecord(title, requestContext);
      });
  }

  /**
   * Return id of created  Holding
   */
  public Future<String> handleHoldingsRecord(CompositePoLine compPOL, Location location, String instanceId, RequestContext requestContext) {
    try {
      if (PoLineCommonUtil.isHoldingsUpdateRequired(compPOL)) {
        return getOrCreateHoldingsRecord(instanceId, location, requestContext);
      } else {
        return Future.succeededFuture();
      }
    } catch (Exception e) {
      return Future.failedFuture(e);
    }
  }


  private void validateItemsCreation(String poLineId, int expectedItemsQuantity, int itemsSize) {
    if (itemsSize != expectedItemsQuantity) {
      String message = String.format("Error creating items for PO Line with '%s' id. Expected %d but %d created",
        poLineId, expectedItemsQuantity, itemsSize);
      throw new InventoryException(message);
    }
  }

  /**
   * Loads configuration and gets tenant specific value
   *
   * @param entryType type of the entry
   * @return tenant specific value or system default one
   */
  private Future<String> getEntryTypeValue(String entryType, RequestContext requestContext) {
    return configurationEntriesCache.loadConfiguration(ORDER_CONFIG_MODULE_NAME, requestContext)
      .map(configs -> switch (entryType) {
        case INSTANCE_STATUSES -> configs.getString(CONFIG_NAME_INSTANCE_STATUS_CODE, DEFAULT_INSTANCE_STATUS_CODE);
        case INSTANCE_TYPES -> configs.getString(CONFIG_NAME_INSTANCE_TYPE_CODE, DEFAULT_INSTANCE_TYPE_CODE);
        case LOAN_TYPES -> configs.getString(CONFIG_NAME_LOAN_TYPE_NAME, DEFAULT_LOAN_TYPE_NAME);
        case HOLDINGS_SOURCES -> configs.getString(CONFIG_NAME_HOLDINGS_SOURCE_NAME, DEFAULT_HOLDINGS_SOURCE_NAME);
        default -> throw new IllegalArgumentException("Unexpected inventory entry type: " + entryType);
      });
  }

  /**
   * Wait for item creation requests completion and filter failed items if any
   *
   * @param itemRecord    item record to be created
   * @param expectedCount count of the items to be created
   * @return completable future with list of item id's
   */
  private Future<List<String>> createItemRecords(JsonObject itemRecord, int expectedCount, RequestContext requestContext) {
    List<Future<String>> futures = new ArrayList<>(expectedCount);
    for (int i = 0; i < expectedCount; i++) {
      futures.add(createItemInInventory(itemRecord, requestContext));
    }

    return collectResultsOnSuccess(futures);
  }

  /**
   * Creates new entry in the inventory storage based on the PO line data.
   *
   * @param itemData json to post
   * @return id of newly created entity Record
   */
  private Future<String> createItemInInventory(JsonObject itemData, RequestContext requestContext) {
    Promise<String> promise = Promise.promise();
    RequestEntry requestEntry = new RequestEntry(ITEM_STOR_ENDPOINT);
    logger.info("Trying to create Item in inventory");
    restClient.postJsonObjectAndGetId(requestEntry, itemData, requestContext)
      .onSuccess(promise::complete)
      // In case item creation failed, return null instead of id
      .onFailure(t -> {
        if (StringUtils.isNotEmpty(t.getMessage()) && t.getMessage().contains(BARCODE_ALREADY_EXIST_ERROR)) {
          logger.info("Barcode is already exists, full response message: {}", t.getMessage(), t);
          promise.fail(new HttpException(409, BARCODE_IS_NOT_UNIQUE));
        } else {
          var causeParam = new Parameter().withKey("cause").withValue(t.getMessage());
          promise.fail(new HttpException(500, ITEM_CREATION_FAILED, List.of(causeParam)));
        }
      });
    return promise.future();
  }

  /**
   * Builds JsonObject representing inventory item minimal data. The schema is located directly in 'mod-inventory-storage' module.
   *
   * @param compPOL   PO line to create Item Records for
   * @param holdingId holding uuid from the inventory
   * @return item data to be used as request body for POST operation
   */
  private Future<JsonObject> buildBaseItemRecordJsonObject(CompositePoLine compPOL, String holdingId, RequestContext requestContext) {
    return getLoanTypeId(requestContext)
      .map(loanTypeId -> {
        JsonObject itemRecord = new JsonObject();
        itemRecord.put(ITEM_HOLDINGS_RECORD_ID, holdingId);
        itemRecord.put(ITEM_STATUS, new JsonObject().put(ITEM_STATUS_NAME, ReceivedItem.ItemStatus.ON_ORDER.value()));
        itemRecord.put(ITEM_PERMANENT_LOAN_TYPE_ID, loanTypeId);
        itemRecord.put(ITEM_PURCHASE_ORDER_LINE_IDENTIFIER, compPOL.getId());
        return itemRecord;
      });
  }

  /**
   * Builds JsonObject representing inventory item minimal data. The schema is located directly in 'mod-inventory-storage' module.
   *
   * @param compPOL   PO line to create Item Records for
   * @param holdingId holding uuid from the inventory
   * @return item data to be used as request body for POST operation
   */
  private Future<JsonObject> buildElectronicItemRecordJsonObject(CompositePoLine compPOL, String holdingId, RequestContext requestContext) {
    return buildBaseItemRecordJsonObject(compPOL, holdingId, requestContext)
      .map(itemRecord -> itemRecord.put(ITEM_MATERIAL_TYPE_ID, compPOL.getEresource().getMaterialType()));
  }

  /**
   * Builds JsonObject representing inventory item minimal data. The schema is located directly in 'mod-inventory-storage' module.
   *
   * @param compPOL   PO line to create Item Records for
   * @param holdingId holding uuid from the inventory
   * @return item data to be used as request body for POST operation
   */
  private Future<JsonObject> buildPhysicalItemRecordJsonObject(CompositePoLine compPOL, String holdingId, RequestContext requestContext) {
    return buildBaseItemRecordJsonObject(compPOL, holdingId, requestContext)
      .map(itemRecord -> itemRecord.put(ITEM_MATERIAL_TYPE_ID, compPOL.getPhysical().getMaterialType()));
  }

  /**
   * Validates if the json object contains entries and returns entries as list of JsonObject elements
   *
   * @param entries {@link JsonObject} representing item storage response
   * @return list of the entry records as JsonObject elements
   */
  private List<JsonObject> extractEntities(JsonObject entries, String key) {
    return Optional.ofNullable(entries.getJsonArray(key))
      .map(objects -> objects.stream()
        .map(JsonObject.class::cast)
        .collect(toList()))
      .orElseGet(Collections::emptyList);
  }

  /**
   * Search for items which might be already created for the PO line
   *
   * @param poLineId         purchase order line Id
   * @param holdingId        holding uuid from the inventory
   * @param expectedQuantity expected quantity of the items for combination of the holding and PO Line uuid's from the inventory
   * @return future with list of item id's
   */
  private Future<List<JsonObject>> searchStorageExistingItems(String poLineId, String holdingId, int expectedQuantity,
                                                                         RequestContext requestContext) {
    String query = String.format(LOOKUP_ITEM_QUERY, poLineId, holdingId);
    RequestEntry requestEntry = new RequestEntry(ITEM_STOR_ENDPOINT).withQuery(query).withOffset(0).withLimit(expectedQuantity);
    return restClient.getAsJsonObject(requestEntry, requestContext)
      .map(itemsCollection -> {
        List<JsonObject> items = extractEntities(itemsCollection, ITEMS);
        logger.debug("{} existing items found out of {} for PO Line with '{}' id", items.size(), expectedQuantity, poLineId);
        return items;
      });
  }

  private Future<List<JsonObject>> fetchHoldingsByHoldingIds(List<String> holdingIds, RequestContext requestContext) {
    return fetchHoldingsByHoldingIds(holdingIds, requestContext, holdings -> {
      if (holdings.size() == holdingIds.size()) {
        return holdings;
      }
      List<Parameter> parameters = holdingIds.stream()
        .filter(id -> holdings.stream()
          .noneMatch(holding -> holding.getString(ID).equals(id)))
        .map(id -> new Parameter().withValue(id).withKey("holdings"))
        .collect(Collectors.toList());
      throw new HttpException(404, PARTIALLY_RETURNED_COLLECTION.toError().withParameters(parameters));
    });
  }

  private Future<List<JsonObject>> fetchHoldingsByHoldingIdsWithoutVerification(List<String> holdingIds, RequestContext requestContext) {
    return fetchHoldingsByHoldingIds(holdingIds, requestContext, UnaryOperator.identity());
  }

  private Future<List<JsonObject>> fetchHoldingsByHoldingIds(List<String> holdingIds, RequestContext requestContext,
                                                          UnaryOperator<List<JsonObject>> unaryOperator) {
    String query = convertIdsToCqlQuery(holdingIds);
    RequestEntry requestEntry = new RequestEntry(INVENTORY_LOOKUP_ENDPOINTS.get(HOLDINGS_RECORDS))
      .withQuery(query).withOffset(0).withLimit(MAX_IDS_FOR_GET_RQ_15);
    return restClient.getAsJsonObject(requestEntry, requestContext)
      .map(jsonHoldings -> jsonHoldings.getJsonArray(HOLDINGS_RECORDS)
        .stream()
        .map(JsonObject.class::cast)
        .collect(toList()))
      .map(unaryOperator);
  }

  void updateItemWithPieceFields(Piece piece, JsonObject item) {
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

  public Future<SharingInstance> createShadowInstanceIfNeeded(String instanceId, RequestContext requestContext) {
    return consortiumConfigurationService.getConsortiumConfiguration(requestContext)
      .compose(consortiumConfiguration -> {
        if (consortiumConfiguration.isEmpty()) {
          logger.debug("Skipping creating shadow instance for non ECS mode.");
          return Future.succeededFuture();
        }
        if (StringUtils.isBlank(instanceId)) {
          logger.info("Provided instanceId is blank, skip creating of shadow instance.");
          return Future.succeededFuture();
        }
        return getInstanceById(instanceId, true, requestContext)
          .compose(instance -> {
            if (Objects.nonNull(instance) && !instance.isEmpty()) {
              logger.info("Shadow instance already exists, skipping...");
              return Future.succeededFuture();
            }
            logger.info("Creating shadow instance with instanceId: {}", instanceId);
            return sharingInstanceService.createShadowInstance(instanceId, consortiumConfiguration.get(), requestContext);
          });
      });
  }

}
