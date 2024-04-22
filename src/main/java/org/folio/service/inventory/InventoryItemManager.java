package org.folio.service.inventory;

import io.vertx.core.Future;
import io.vertx.core.Promise;
import io.vertx.core.json.JsonObject;
import one.util.streamex.IntStreamEx;
import one.util.streamex.StreamEx;
import org.apache.commons.collections4.ListUtils;
import org.apache.commons.lang3.StringUtils;
import org.apache.logging.log4j.LogManager;
import org.apache.logging.log4j.Logger;
import org.folio.models.PieceItemPair;
import org.folio.models.PoLineUpdateHolder;
import org.folio.orders.utils.HelperUtils;
import org.folio.rest.core.RestClient;
import org.folio.rest.core.exceptions.HttpException;
import org.folio.rest.core.exceptions.InventoryException;
import org.folio.rest.core.models.RequestContext;
import org.folio.rest.core.models.RequestEntry;
import org.folio.rest.jaxrs.model.CheckInPiece;
import org.folio.rest.jaxrs.model.CompositePoLine;
import org.folio.rest.jaxrs.model.Location;
import org.folio.rest.jaxrs.model.Parameter;
import org.folio.rest.jaxrs.model.Piece;
import org.folio.rest.jaxrs.model.ReceivedItem;
import org.folio.service.caches.ConfigurationEntriesCache;
import org.folio.service.caches.InventoryCache;
import org.folio.service.consortium.ConsortiumConfigurationService;
import org.folio.service.pieces.PieceStorageService;

import java.util.ArrayList;
import java.util.Collections;
import java.util.List;
import java.util.Map;
import java.util.Optional;

import static java.util.stream.Collectors.toList;
import static org.folio.orders.utils.HelperUtils.collectResultsOnSuccess;
import static org.folio.orders.utils.HelperUtils.convertIdsToCqlQuery;
import static org.folio.orders.utils.RequestContextUtil.cloneRequestContextBasedOnLocation;
import static org.folio.rest.RestConstants.MAX_IDS_FOR_GET_RQ_15;
import static org.folio.rest.core.exceptions.ErrorCodes.BARCODE_IS_NOT_UNIQUE;
import static org.folio.rest.core.exceptions.ErrorCodes.ITEM_CREATION_FAILED;
import static org.folio.rest.jaxrs.model.CompositePoLine.OrderFormat.ELECTRONIC_RESOURCE;
import static org.folio.service.inventory.InventoryUtils.INVENTORY_LOOKUP_ENDPOINTS;
import static org.folio.service.inventory.InventoryUtils.ITEMS;
import static org.folio.service.inventory.InventoryUtils.ITEM_BY_ID_ENDPOINT;
import static org.folio.service.inventory.InventoryUtils.REQUESTS;

public class InventoryItemManager {
  private static final Logger logger = LogManager.getLogger(InventoryItemManager.class);

  public static final String ID = "id";
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
  public static final String COPY_NUMBER = "copyNumber";

  public static final String BARCODE_ALREADY_EXIST_ERROR = "lower(jsonb ->> 'barcode'::text) value already exists in table item";
  private static final String LOOKUP_ITEM_QUERY = "purchaseOrderLineIdentifier==%s and holdingsRecordId==%s";
  private static final String ITEM_STOR_ENDPOINT = "/item-storage/items";
  private static final String TOTAL_RECORDS = "totalRecords";
  private static final String BUILDING_PIECE_MESSAGE = "Building {} {} piece(s) for PO Line with id={}";
  private static final String EFFECTIVE_LOCATION = "effectiveLocation";

  private final RestClient restClient;
  private final ConfigurationEntriesCache configurationEntriesCache;
  private final InventoryCache inventoryCache;
  private final PieceStorageService pieceStorageService;
  private final ConsortiumConfigurationService consortiumConfigurationService;

  public InventoryItemManager(RestClient restClient,
                              ConfigurationEntriesCache configurationEntriesCache,
                              PieceStorageService pieceStorageService,
                              InventoryCache inventoryCache,
                              ConsortiumConfigurationService consortiumConfigurationService) {
    this.restClient = restClient;
    this.configurationEntriesCache = configurationEntriesCache;
    this.inventoryCache = inventoryCache;
    this.pieceStorageService = pieceStorageService;
    this.consortiumConfigurationService = consortiumConfigurationService;
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
      .map(json -> json.getInteger(TOTAL_RECORDS));
  }

  /**
   * Returns list of item records for specified query.
   *
   * @param query item records query
   * @return future with list of item records
   */
  private Future<List<JsonObject>> getItemRecordsByQuery(String query, RequestContext requestContext) {
    RequestEntry requestEntry = new RequestEntry(INVENTORY_LOOKUP_ENDPOINTS.get(ITEMS))
      .withQuery(query).withOffset(0).withLimit(Integer.MAX_VALUE);
    return restClient.getAsJsonObject(requestEntry, requestContext)
      .map(response -> extractEntities(response, ITEMS));
  }

  public Future<Void> updateItem(JsonObject item, RequestContext requestContext) {
    RequestEntry requestEntry = new RequestEntry(INVENTORY_LOOKUP_ENDPOINTS.get(ITEM_BY_ID_ENDPOINT)).withId(item.getString(ID));
    return restClient.put(requestEntry, item, requestContext);
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

            var future = consortiumConfigurationService.getConsortiumConfiguration(requestContext)
              .map(optionalConfiguration -> optionalConfiguration.map(configuration ->
                cloneRequestContextBasedOnLocation(requestContext, location)).orElse(requestContext))
              .compose(updatedRequestContext -> {
                List<String> existingItemIds;
                if (pieceFormat == Piece.Format.ELECTRONIC) {
                  existingItemIds = getElectronicItemIds(compPOL, existingItems);
                  return createMissingElectronicItems(compPOL, pieceWithHoldingId, expectedQuantity - existingItemIds.size(), updatedRequestContext)
                    .map(createdItemIds -> buildPieces(location, polId, pieceFormat, createdItemIds, existingItemIds));
                } else {
                  existingItemIds = getPhysicalItemIds(compPOL, existingItems);
                  return createMissingPhysicalItems(compPOL, pieceWithHoldingId, expectedQuantity - existingItemIds.size(), updatedRequestContext)
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
        .withHoldingId(location.getHoldingId())
        .withReceivingTenantId(location.getTenantId());
    } else {
      return new Piece().withFormat(pieceFormat)
        .withItemId(itemId)
        .withPoLineId(polId)
        .withLocationId(location.getLocationId())
        .withReceivingTenantId(location.getTenantId());
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
          .toList();
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
            updatedItemIds.add(updateItem(item, requestContext).map(v -> item.getString(ID)));
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

  private boolean isLocationContainsItemLocation(List<Location> polLocations, JsonObject item) {
    return item != null && polLocations.stream().noneMatch(
      location -> location.getLocationId().equals(item.getJsonObject(EFFECTIVE_LOCATION).getString(ID))
    );
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
   * @param compPOL  PO line to create Instance Record for
   * @param piece    base piece to build item
   * @param quantity expected number of items to create
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
   * @param compPOL  PO line to create Instance Record for
   * @param piece    base piece to build item
   * @param quantity expected number of items to create
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

  public Future<List<JsonObject>> getItemsByPoLineIdsAndStatus(List<String> poLineIds, String itemStatus, RequestContext requestContext) {
    logger.debug("getItemsByStatus start");
    List<Future<List<JsonObject>>> futures = StreamEx
      .ofSubLists(poLineIds, MAX_IDS_FOR_GET_RQ_15)
      .map(ids -> {
        String query = String.format("status.name==%s and %s", itemStatus, HelperUtils.convertFieldListToCqlQuery(ids, InventoryItemManager.ITEM_PURCHASE_ORDER_LINE_IDENTIFIER, true));
        return getItemRecordsByQuery(query, requestContext);
      })
      .toList();

    return collectResultsOnSuccess(futures)
      .map(lists -> StreamEx.of(lists).toFlatList(jsonObjects -> jsonObjects));
  }

  private void validateItemsCreation(String poLineId, int expectedItemsQuantity, int itemsSize) {
    if (itemsSize != expectedItemsQuantity) {
      String message = String.format("Error creating items for PO Line with '%s' id. Expected %d but %d created",
        poLineId, expectedItemsQuantity, itemsSize);
      throw new InventoryException(message);
    }
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
    return InventoryUtils.getLoanTypeId(configurationEntriesCache, inventoryCache, requestContext)
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

}