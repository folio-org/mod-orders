package org.folio.service.inventory;

import static java.util.Collections.singletonList;
import static java.util.concurrent.CompletableFuture.allOf;
import static java.util.concurrent.CompletableFuture.completedFuture;
import static java.util.stream.Collectors.joining;
import static java.util.stream.Collectors.toList;
import static one.util.streamex.StreamEx.ofSubLists;
import static org.apache.commons.collections4.CollectionUtils.isNotEmpty;
import static org.folio.orders.utils.ErrorCodes.FUNDS_NOT_FOUND;
import static org.folio.orders.utils.ErrorCodes.HOLDINGS_BY_INSTANCE_AND_LOCATION_NOT_FOUND;
import static org.folio.orders.utils.ErrorCodes.ISBN_NOT_VALID;
import static org.folio.orders.utils.ErrorCodes.ITEM_CREATION_FAILED;
import static org.folio.orders.utils.ErrorCodes.MISSING_CONTRIBUTOR_NAME_TYPE;
import static org.folio.orders.utils.ErrorCodes.MISSING_INSTANCE_STATUS;
import static org.folio.orders.utils.ErrorCodes.MISSING_INSTANCE_TYPE;
import static org.folio.orders.utils.ErrorCodes.MISSING_LOAN_TYPE;
import static org.folio.orders.utils.HelperUtils.LANG;
import static org.folio.orders.utils.HelperUtils.ORDER_CONFIG_MODULE_NAME;
import static org.folio.orders.utils.HelperUtils.collectResultsOnSuccess;
import static org.folio.orders.utils.HelperUtils.convertIdsToCqlQuery;
import static org.folio.orders.utils.HelperUtils.encodeQuery;
import static org.folio.orders.utils.HelperUtils.groupLocationsById;
import static org.folio.orders.utils.HelperUtils.handleGetRequest;
import static org.folio.orders.utils.HelperUtils.isProductIdsExist;
import static org.folio.orders.utils.ResourcePathResolver.PIECES;
import static org.folio.orders.utils.ResourcePathResolver.resourcesPath;
import static org.folio.rest.RestConstants.MAX_IDS_FOR_GET_RQ;
import static org.folio.rest.jaxrs.model.CompositePoLine.OrderFormat.ELECTRONIC_RESOURCE;

import java.util.ArrayList;
import java.util.Arrays;
import java.util.Collection;
import java.util.Collections;
import java.util.List;
import java.util.Map;
import java.util.Objects;
import java.util.Optional;
import java.util.concurrent.CompletableFuture;
import java.util.concurrent.CompletionException;
import java.util.concurrent.CompletionStage;
import java.util.stream.Collectors;

import org.apache.commons.collections4.CollectionUtils;
import org.apache.commons.collections4.ListUtils;
import org.apache.commons.lang3.StringUtils;
import org.apache.logging.log4j.LogManager;
import org.apache.logging.log4j.Logger;
import org.folio.models.PieceItemPair;
import org.folio.models.PoLineUpdateHolder;
import org.folio.orders.rest.exceptions.HttpException;
import org.folio.orders.rest.exceptions.InventoryException;
import org.folio.orders.utils.ErrorCodes;
import org.folio.orders.utils.HelperUtils;
import org.folio.orders.utils.LocationUtil;
import org.folio.orders.utils.PoLineCommonUtil;
import org.folio.rest.acq.model.finance.Budget;
import org.folio.rest.acq.model.finance.FundCollection;
import org.folio.rest.core.PostResponseType;
import org.folio.rest.core.RestClient;
import org.folio.rest.core.models.RequestContext;
import org.folio.rest.core.models.RequestEntry;
import org.folio.rest.jaxrs.model.CheckInPiece;
import org.folio.rest.jaxrs.model.CompositePoLine;
import org.folio.rest.jaxrs.model.Contributor;
import org.folio.rest.jaxrs.model.Error;
import org.folio.rest.jaxrs.model.Location;
import org.folio.rest.jaxrs.model.Parameter;
import org.folio.rest.jaxrs.model.Piece;
import org.folio.rest.jaxrs.model.PoLine;
import org.folio.rest.jaxrs.model.ProductId;
import org.folio.rest.jaxrs.model.ReceivedItem;
import org.folio.rest.jaxrs.model.Title;
import org.folio.rest.tools.utils.TenantTool;
import org.folio.service.configuration.ConfigurationEntriesService;
import org.folio.service.pieces.PieceRetrieveService;

import com.google.common.collect.ImmutableList;

import io.vertx.core.Context;
import io.vertx.core.json.JsonArray;
import io.vertx.core.json.JsonObject;
import one.util.streamex.IntStreamEx;
import one.util.streamex.StreamEx;

public class InventoryManager {
  private static final Logger logger = LogManager.getLogger(InventoryManager.class);

  private static final String IDENTIFIER_TYPES = "identifierTypes";
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
  public static final String ITEM_HOLDINGS_RECORD_ID = "holdingsRecordId";
  public static final String ITEM_BARCODE = "barcode";
  public static final String ITEM_LEVEL_CALL_NUMBER = "itemLevelCallNumber";
  public static final String ITEM_STATUS = "status";
  public static final String ITEM_STATUS_NAME = "name";
  public static final String ITEM_MATERIAL_TYPE_ID = "materialTypeId";
  public static final String ITEM_MATERIAL_TYPE = "materialType";
  public static final String ITEM_PERMANENT_LOAN_TYPE_ID = "permanentLoanTypeId";
  public static final String ITEM_PURCHASE_ORDER_LINE_IDENTIFIER = "purchaseOrderLineIdentifier";
  public static final String ITEM_EFFECTIVE_LOCATION = "effectiveLocation";
  public static final String CONTRIBUTOR_NAME = "name";
  public static final String CONTRIBUTOR_NAME_TYPE_ID = "contributorNameTypeId";
  public static final String CONTRIBUTOR_NAME_TYPES = "contributorNameTypes";
  public static final String INSTANCE_STATUSES = "instanceStatuses";
  public static final String INSTANCE_TYPES = "instanceTypes";
  public static final String ITEMS = "items";
  public static final String LOAN_TYPES = "loantypes";
  public static final String REQUESTS = "requests";

  // mod-configuration: config names and default values
  public static final String CONFIG_NAME_INSTANCE_TYPE_CODE = "inventory-instanceTypeCode";
  public static final String CONFIG_NAME_INSTANCE_STATUS_CODE = "inventory-instanceStatusCode";
  public static final String CONFIG_NAME_LOAN_TYPE_NAME = "inventory-loanTypeName";
  public static final String DEFAULT_INSTANCE_TYPE_CODE = "zzz";
  public static final String DEFAULT_INSTANCE_STATUS_CODE = "temp";
  public static final String DEFAULT_LOAN_TYPE_NAME = "Can circulate";

  private static final String HOLDINGS_RECORDS = "holdingsRecords";
  private static final String HOLDINGS_RECORDS_BY_ID_ENDPOINT = "holdingsRecordsById";
  private static final String INSTANCES = "instances";

  private static final String TENANT_SPECIFIC_KEY_FORMAT = "%s.%s.%s";
  private static final String LOOKUP_ITEM_QUERY = "purchaseOrderLineIdentifier==%s and holdingsRecordId==%s";
  private static final String ITEM_STOR_ENDPOINT = "/item-storage/items";
  public static final String  ITEM_BY_ID_ENDPOINT = "/inventory/items/{id}";
  private static final String CREATE_ITEM_STOR_ENDPOINT = "/item-storage/items";
  private static final String HOLDINGS_LOOKUP_QUERY = "instanceId==%s and permanentLocationId==%s";
  public static final String ID = "id";
  public static final String TOTAL_RECORDS = "totalRecords";
  public static final String SEARCH_PARAMS_WITHOUT_LANG = "?limit=%s&offset=%s%s";
  private static final Map<String, String> INVENTORY_LOOKUP_ENDPOINTS;
  public static final String BUILDING_PIECE_MESSAGE = "Building {} {} piece(s) for PO Line with id={}";
  private static final String PIECES_BY_POL_ID_AND_STATUS_QUERY = "poLineId==%s and receivingStatus==%s";
  private static final String GET_PIECES_BY_QUERY = resourcesPath(PIECES) + SEARCH_PARAMS_WITHOUT_LANG;
  public static final String EFFECTIVE_LOCATION = "effectiveLocation";

  private RestClient restClient;
  private ConfigurationEntriesService configurationEntriesService;
  private PieceRetrieveService pieceRetrieveService;

  public InventoryManager(RestClient restClient, ConfigurationEntriesService configurationEntriesService,
                          PieceRetrieveService pieceRetrieveService) {
    this.restClient = restClient;
    this.configurationEntriesService = configurationEntriesService;
    this.pieceRetrieveService = pieceRetrieveService;
  }

  static {
    INVENTORY_LOOKUP_ENDPOINTS = Map.of(
      CONTRIBUTOR_NAME_TYPES, "/contributor-name-types",
      HOLDINGS_RECORDS, "/holdings-storage/holdings",
      HOLDINGS_RECORDS_BY_ID_ENDPOINT, "/holdings-storage/holdings/{id}",
      LOAN_TYPES, "/loan-types?query=name==%s&limit=1",
      INSTANCE_STATUSES, "/instance-statuses?query=code==%s&limit=1",
      INSTANCE_TYPES,"/instance-types?query=code==%s",
      INSTANCES, "/inventory/instances",
      ITEMS,"/inventory/items",
      REQUESTS, "/circulation/requests");
  }

  public CompletableFuture<CompositePoLine> handleInstanceRecord(CompositePoLine compPOL, RequestContext requestContext) {
    if(compPOL.getInstanceId() != null) {
      return CompletableFuture.completedFuture(compPOL);
    } else {
      return getInstanceRecord(compPOL, requestContext)
                  .thenApply(compPOL::withInstanceId);
    }
  }

  /**
   * Returns list of pieces with populated item and location id's corresponding to given PO line.
   * Items are either retrieved from Inventory or new ones are created if no corresponding item records exist yet.
   *
   * @param compPOL   PO line to retrieve/create Item Records for. At this step PO Line must contain instance Id
   * @return future with list of pieces with item and location id's
   */
  public CompletableFuture<List<Piece>> handleHoldingsAndItemsRecords(CompositePoLine compPOL, RequestContext requestContext) {
    List<CompletableFuture<List<Piece>>> itemsPerHolding = new ArrayList<>();
    boolean isItemsUpdateRequired = PoLineCommonUtil.isItemsUpdateRequired(compPOL);

    // Group all locations by location id because the holding should be unique for different locations
    if (PoLineCommonUtil.isHoldingsUpdateRequired(compPOL.getEresource(), compPOL.getPhysical())) {
      groupLocationsById(compPOL)
        .forEach((locationId, polLocations) -> itemsPerHolding.add(
          // Search for or create a new holdings record and then create items for it if required
          getOrCreateHoldingsRecord(compPOL.getInstanceId(), locationId, requestContext)
            .thenCompose(holdingId -> {
                // Items are not going to be created when create inventory is "Instance, Holding"
                addHoldingId(polLocations, holdingId);
                if (isItemsUpdateRequired) {
                  return handleItemRecords(compPOL, holdingId, polLocations, requestContext);
                } else {
                  return completedFuture(Collections.emptyList());
                }
              }
            )));
    }
    return collectResultsOnSuccess(itemsPerHolding)
      .thenApply(results -> results.stream()
        .flatMap(List::stream)
        .collect(toList())
      );
  }

  /**
   * Returns list of pieces with populated item and location id's corresponding to given PO line.
   * Items are either retrieved from Inventory or new ones are created if no corresponding item records exist yet.
   *
   * @param compPOL   PO line to retrieve/create Item Records for. At this step PO Line must contain instance Id
   * @return future with list of pieces with item and location id's
   */
  public CompletableFuture<List<Piece>> handleHoldingsAndItemsRecords(CompositePoLine compPOL, PoLine storagePoLine,
                                                                      RequestContext requestContext) {
    List<CompletableFuture<List<Piece>>> itemsPerHolding = new ArrayList<>();
    boolean isItemsUpdateRequired = PoLineCommonUtil.isItemsUpdateRequired(compPOL);

    // Group all locations by location id because the holding should be unique for different locations
    if (PoLineCommonUtil.isHoldingsUpdateRequired(compPOL.getEresource(), compPOL.getPhysical())) {
      List<PoLineUpdateHolder> poLineUpdateHolders = LocationUtil.convertToOldNewLocationIdPair(compPOL.getLocations(), storagePoLine.getLocations());
      if (!poLineUpdateHolders.isEmpty()) {
        poLineUpdateHolders.forEach(poLineUpdateHolder -> {
          poLineUpdateHolder.withInstanceId(compPOL.getInstanceId());
          itemsPerHolding.add(
            // Search for or create a new holdings record and then create items for it if required
            updateHoldingsRecord(poLineUpdateHolder, requestContext)
              .thenCompose(v -> {
                  // Items are not going to be created when create inventory is "Instance, Holding"
                  if (isItemsUpdateRequired) {
                    return handleItemRecords(compPOL, poLineUpdateHolder, requestContext);
                  } else {
                    return completedFuture(Collections.emptyList());
                  }
                }
              )
          );
        });
      } else {
        groupLocationsById(compPOL)
          .forEach((locationId, locations) -> itemsPerHolding.add(
            // Search for or create a new holdings record and then create items for it if required
            getOrCreateHoldingsRecord(compPOL.getInstanceId(), locationId, requestContext)
              .thenCompose(holdingId -> {
                  // Items are not going to be created when create inventory is "Instance, Holding"
                  if (isItemsUpdateRequired) {
                    return handleItemRecords(compPOL, holdingId, locations, requestContext);
                  } else {
                    return completedFuture(Collections.emptyList());
                  }
                }
              )));
      }
    }
    return collectResultsOnSuccess(itemsPerHolding)
      .thenApply(results -> results.stream()
        .flatMap(List::stream)
        .collect(toList())
      );
  }

  /**
   * Returns list of item records for specified id's.
   *
   * @param ids   List of item id's
   * @return future with list of item records
   */
  public CompletableFuture<List<JsonObject>> getItemRecordsByIds(List<String> ids, RequestContext requestContext) {
    String query = convertIdsToCqlQuery(ids);
    RequestEntry requestEntry = new RequestEntry(INVENTORY_LOOKUP_ENDPOINTS.get(ITEMS))
            .withQuery(query).withOffset(0).withLimit(ids.size());
    return restClient.getAsJsonObject(requestEntry, requestContext)
                     .thenApply(response -> extractEntities(response, ITEMS));
  }

  /**
   * Returns list of requests for specified item.
   *
   * @param itemId id of Item
   * @return future with list of requests
   */
  public CompletableFuture<Integer> getNumberOfRequestsByItemId(String itemId, RequestContext requestContext) {
    String query = String.format("(itemId==%s and status=\"*\")", itemId);
    RequestEntry requestEntry = new RequestEntry(INVENTORY_LOOKUP_ENDPOINTS.get(REQUESTS))
                                          .withQuery(query).withOffset(0).withLimit(0);
    return restClient.getAsJsonObject(requestEntry, requestContext)
                     .thenApply(this::extractTotalRecords);
  }

  /**
   * Returns list of item records for specified query.
   *
   * @param query item records query
   * @return future with list of item records
   */
  public CompletableFuture<List<JsonObject>> getItemRecordsByQuery(String query, RequestContext requestContext) {
    RequestEntry requestEntry = new RequestEntry(INVENTORY_LOOKUP_ENDPOINTS.get(ITEMS))
      .withQuery(query).withOffset(0).withLimit(Integer.MAX_VALUE);
    return restClient.getAsJsonObject(requestEntry, requestContext)
                     .thenApply(response -> extractEntities(response, ITEMS));
  }

  public CompletableFuture<Void> updateItem(JsonObject item, RequestContext requestContext) {
    RequestEntry requestEntry = new RequestEntry(ITEM_BY_ID_ENDPOINT).withId(item.getString(ID));
    return restClient.put(requestEntry, item, requestContext);
  }

  public CompletableFuture<String> saveItem(JsonObject item, RequestContext requestContext) {
    return updateItem(item, requestContext).thenApply(v -> item.getString(ID));
  }

  /**
   * Wait for item creation requests completion and filter failed items if any
   * @param itemRecords item record to be created
   * @return completable future with list of item id's
   */
  public CompletableFuture<List<String>> updateItemRecords(List<JsonObject> itemRecords, RequestContext requestContext) {
    List<CompletableFuture<String>> futures = new ArrayList<>(itemRecords.size());
    itemRecords.forEach(itemRecord -> futures.add(updateItem(itemRecord, requestContext).thenApply(v -> itemRecord.getString(ID))));
    return collectResultsOnSuccess(futures);
  }

  public CompletableFuture<Void> deleteItem(String id, RequestContext requestContext) {
    RequestEntry requestEntry = new RequestEntry(ITEM_BY_ID_ENDPOINT).withId(id);
    return restClient.delete(requestEntry, requestContext);
  }

  public CompletableFuture<List<Void>> deleteItems(List<String> itemIds, RequestContext requestContext) {
    List<CompletableFuture<Void>> futures = new ArrayList<>(itemIds.size());
    itemIds.forEach(itemId -> futures.add(deleteItem(itemId, requestContext)));
    return collectResultsOnSuccess(futures);
  }

  /**
   * Returns list of item records for specified id's.
   *
   * @param itemRecord item record
   * @param receivedItem item details specified by user upon receiving flow
   * @return future with list of item records
   */
  public CompletableFuture<Void> receiveItem(JsonObject itemRecord, ReceivedItem receivedItem, RequestContext requestContext) {
    // Update item record with receiving details
    itemRecord.put(ITEM_STATUS, new JsonObject().put(ITEM_STATUS_NAME, receivedItem.getItemStatus().value()));
    if (StringUtils.isNotEmpty(receivedItem.getBarcode())) {
      itemRecord.put(ITEM_BARCODE, receivedItem.getBarcode());
    }
    if (StringUtils.isNotEmpty(receivedItem.getCallNumber())) {
      itemRecord.put(ITEM_LEVEL_CALL_NUMBER, receivedItem.getCallNumber());
    }
    return updateItem(itemRecord, requestContext);
  }

  public CompletableFuture<Void> checkinItem(JsonObject itemRecord, CheckInPiece checkinPiece, RequestContext requestContext) {

    // Update item record with checkIn details
    itemRecord.put(ITEM_STATUS, new JsonObject().put(ITEM_STATUS_NAME, checkinPiece.getItemStatus().value()));
    if (StringUtils.isNotEmpty(checkinPiece.getBarcode())) {
      itemRecord.put(ITEM_BARCODE, checkinPiece.getBarcode());
    }
    if (StringUtils.isNotEmpty(checkinPiece.getCallNumber())) {
      itemRecord.put(ITEM_LEVEL_CALL_NUMBER, checkinPiece.getCallNumber());
    }
    return updateItem(itemRecord, requestContext);
  }

  /**
   * Checks if the {@link ReceivedItem} has item status as "On order"
   * @param receivedItem details specified by user upon receiving flow
   * @return {@code true} if the item status is "On order"
   */
  public boolean isOnOrderItemStatus(ReceivedItem receivedItem) {
    return ReceivedItem.ItemStatus.ON_ORDER == receivedItem.getItemStatus();
  }

  /**
   * Checks if the {@link ReceivedItem} has item status as "On order"
   * @param checkinPiece details specified by user upon check-in flow
   * @return {@code true} if the item status is "On order"
   */
  public boolean isOnOrderPieceStatus(CheckInPiece checkinPiece) {
    return CheckInPiece.ItemStatus.ON_ORDER == checkinPiece.getItemStatus();
  }

  public CompletableFuture<Void> updateHoldingsRecord(PoLineUpdateHolder holder, RequestContext requestContext) {
    String locationIds = StreamEx.of(Arrays.asList(holder.getOldLocationId(), holder.getNewLocationId()))
      .joining(" or ", "(", ")");
    String query = String.format(HOLDINGS_LOOKUP_QUERY, holder.getInstanceId(), locationIds);
    RequestEntry requestEntry = new RequestEntry(INVENTORY_LOOKUP_ENDPOINTS.get(HOLDINGS_RECORDS))
                                                  .withQuery(query).withOffset(0).withLimit(Integer.MAX_VALUE);
    return restClient.getAsJsonObject(requestEntry, requestContext)
      .thenCompose(holdings -> {
        JsonObject prevHolding;
        JsonObject newHolding;
        if (holdings.getJsonArray(HOLDINGS_RECORDS).isEmpty()) {
          throw new HttpException(400, HOLDINGS_BY_INSTANCE_AND_LOCATION_NOT_FOUND);
        } else {
          prevHolding = getHoldingByLocationId(holdings, holder.getOldLocationId());
          holder.withOldHoldingId(prevHolding.getString(ID));
        }
        if (holdings.getJsonArray(HOLDINGS_RECORDS).size() == 1) {
          return getOrCreateHoldingsRecord(holder.getInstanceId(), holder.getNewLocationId(), requestContext)
            .thenAccept(holder::withNewHoldingId);
        } else if (holdings.getJsonArray(HOLDINGS_RECORDS).size() == 2) {
          newHolding = getHoldingByLocationId(holdings, holder.getNewLocationId());
          holder.withNewHoldingId(newHolding.getString(ID));
        }
        return completedFuture(null);
      });
  }

  public CompletableFuture<String> getOrCreateHoldingsRecord(String instanceId, String locationId, RequestContext requestContext) {
    String query = String.format(HOLDINGS_LOOKUP_QUERY, instanceId, locationId);
    RequestEntry requestEntry = new RequestEntry(INVENTORY_LOOKUP_ENDPOINTS.get(HOLDINGS_RECORDS))
                                            .withQuery(query).withOffset(0).withLimit(Integer.MAX_VALUE);
    return restClient.getAsJsonObject(requestEntry, requestContext)
      .thenCompose(holdings -> {
        if (!holdings.getJsonArray(HOLDINGS_RECORDS).isEmpty()) {
          return completedFuture(extractId(getFirstObjectFromResponse(holdings, HOLDINGS_RECORDS)));
        }
        return createHoldingsRecord(instanceId, locationId, requestContext);
      });
  }

  public CompletableFuture<List<JsonObject>> getHoldingsByIds(List<String> holdingIds, RequestContext requestContext) {
   return collectResultsOnSuccess(
     ofSubLists(new ArrayList<>(holdingIds), MAX_IDS_FOR_GET_RQ).map(ids -> fetchHoldingsByFundIds(ids, requestContext)).toList())
        .thenApply(lists -> lists.stream()
                                 .flatMap(Collection::stream)
                                 .collect(Collectors.toList()));
  }

  public CompletableFuture<JsonObject> getHolding(String holdingId, RequestContext requestContext) {
    RequestEntry requestEntry = new RequestEntry(INVENTORY_LOOKUP_ENDPOINTS.get(HOLDINGS_RECORDS_BY_ID_ENDPOINT)).withId(holdingId).withQueryParameter(LANG, "en");
    return restClient.get(requestEntry, requestContext, JsonObject.class);
  }

  public CompletableFuture<List<JsonObject>> getHoldingRecords(String instanceId, List<String> locationIds, RequestContext requestContext) {
    List<CompletableFuture<JsonObject>> futures = new ArrayList<>();
    locationIds.forEach(locationId -> futures.add(getFirstHoldingRecord(instanceId, locationId, requestContext)));
    return collectResultsOnSuccess(futures).thenApply(holdings -> {
      if (logger.isDebugEnabled()) {
        String deletedIds = holdings.stream().map(holding -> holding.getString(ID)).collect(Collectors.joining(","));
        logger.debug(String.format("Holding ids : %s", deletedIds));
      }
      return holdings.stream().filter(Objects::nonNull).collect(toList());
    });
  }

  public CompletableFuture<JsonObject> getFirstHoldingRecord(String instanceId, String locationId, RequestContext requestContext) {
    String query = String.format(HOLDINGS_LOOKUP_QUERY, instanceId, locationId);
    RequestEntry requestEntry = new RequestEntry(INVENTORY_LOOKUP_ENDPOINTS.get(HOLDINGS_RECORDS))
      .withQuery(query).withOffset(0).withLimit(Integer.MAX_VALUE);
    return restClient.getAsJsonObject(requestEntry, requestContext)
      .thenCompose(holdings -> {
        if (!holdings.getJsonArray(HOLDINGS_RECORDS).isEmpty()) {
          return completedFuture(getFirstObjectFromResponse(holdings, HOLDINGS_RECORDS));
        }
        return CompletableFuture.completedFuture(null);
      });
  }

  public String buildLookupEndpoint(String type, Object... params) {
    return String.format(INVENTORY_LOOKUP_ENDPOINTS.get(type), params);
  }

  private CompletableFuture<String> createHoldingsRecord(String instanceId, String locationId, RequestContext requestContext) {
    JsonObject holdingsRecJson = new JsonObject();
    holdingsRecJson.put(HOLDING_INSTANCE_ID, instanceId);
    holdingsRecJson.put(HOLDING_PERMANENT_LOCATION_ID, locationId);
    RequestEntry requestEntry = new RequestEntry(INVENTORY_LOOKUP_ENDPOINTS.get(HOLDINGS_RECORDS));
    return restClient.post(requestEntry, holdingsRecJson, PostResponseType.UUID, String.class, requestContext);
  }

  /**
   * Handles Inventory items for passed list of locations. Items are either retrieved from Inventory or new ones are created
   * if no corresponding item records exist yet.
   * Returns list of {@link Piece} records with populated item id (and other info) corresponding to given PO line.
   *
   * @param compPOL   PO line to retrieve/create Item Records for
   * @param holdingId holding uuid from the inventory
   * @param locations list of locations holdingId is associated with
   * @return future with list of piece objects
   */
  public CompletableFuture<List<Piece>> handleItemRecords(CompositePoLine compPOL, String holdingId, List<Location> locations,
                                                          RequestContext requestContext) {
    Map<Piece.Format, Integer> piecesWithItemsQuantities = HelperUtils.calculatePiecesWithItemIdQuantity(compPOL, locations);
    int piecesWithItemsQty = IntStreamEx.of(piecesWithItemsQuantities.values()).sum();
    String polId = compPOL.getId();

    logger.debug("Handling {} items for PO Line with id={} and holdings with id={}", piecesWithItemsQty, polId, holdingId);
    if (piecesWithItemsQty == 0) {
      return completedFuture(Collections.emptyList());
    }

    // Search for already existing items
    return searchStorageExistingItems(compPOL.getId(), holdingId, piecesWithItemsQty, requestContext)
      .thenCompose(existingItems -> {
          String locationId = locations.get(0).getLocationId();
          List<CompletableFuture<List<Piece>>> pieces = new ArrayList<>(Piece.Format.values().length);
          piecesWithItemsQuantities.forEach((pieceFormat, expectedQuantity) -> {
            // The expected quantity might be zero for particular piece format if the PO Line's order format is P/E Mix
            if (expectedQuantity > 0) {
              List<String> items;
              CompletableFuture<List<String>> newItems;
              // Depending on piece format get already existing items and send requests to create missing items
              if (pieceFormat == Piece.Format.ELECTRONIC) {
                items = getElectronicItemIds(compPOL, existingItems);
                newItems = createMissingElectronicItems(compPOL, holdingId, expectedQuantity - items.size(), requestContext);
              } else {
                items = getPhysicalItemIds(compPOL, existingItems);
                newItems = createMissingPhysicalItems(compPOL, holdingId, expectedQuantity - items.size(), requestContext);
              }

              // Build piece records once new items are created
              pieces.add(newItems.thenApply(createdItemIds -> {
                List<String> itemIds = ListUtils.union(createdItemIds, items);
                logger.info(BUILDING_PIECE_MESSAGE, itemIds.size(), pieceFormat, polId);
                return StreamEx.of(itemIds)
                  .map(itemId -> new Piece().withFormat(pieceFormat)
                    .withItemId(itemId)
                    .withPoLineId(polId)
                    .withLocationId(locationId))
                  .toList();
              }));
            }
          });

          // Wait for all items to be created and corresponding pieces are built
          return collectResultsOnSuccess(pieces)
            .thenApply(results -> {
              validateItemsCreation(compPOL.getId(), pieces.size(), results.size());
              return results.stream().flatMap(List::stream).collect(toList());
            });
        }
      );
  }


  /**
   * Handles Inventory items for passed list of locations. Items are either retrieved from Inventory or new ones are created
   * if no corresponding item records exist yet.
   * Returns list of {@link Piece} records with populated item id (and other info) corresponding to given PO line.
   *
   * @param compPOL   PO line to retrieve/create Item Records for
   * @param holder pair of new location provided from POl and location from storage
   * @return future with list of piece objects
   */
  public CompletableFuture<List<Piece>> handleItemRecords(CompositePoLine compPOL, PoLineUpdateHolder holder, RequestContext requestContext) {
    List<Location> polLocations = compPOL.getLocations().stream()
      .filter(location -> location.getLocationId().equals(holder.getNewLocationId()))
      .collect(toList());
    Map<Piece.Format, Integer> piecesWithItemsQuantities = HelperUtils.calculatePiecesWithItemIdQuantity(compPOL, polLocations);
    int piecesWithItemsQty = IntStreamEx.of(piecesWithItemsQuantities.values()).sum();
    String polId = compPOL.getId();

    logger.debug("Handling {} items for PO Line with id={} and holdings with id={}", piecesWithItemsQty, polId, holder.getOldHoldingId());
    if (piecesWithItemsQty == 0) {
      return completedFuture(Collections.emptyList());
    }
    return pieceRetrieveService.getExpectedPiecesByLineId(compPOL.getId(), requestContext)
      .thenApply(existingPieces -> {
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
      .thenCompose(needUpdatePieces -> {
        if (!needUpdatePieces.isEmpty()) {
          return getItemRecordsByIds(needUpdatePieces.stream().map(Piece::getItemId)
                                                              .collect(toList()), requestContext)
                                      .thenApply(items -> buildPieceItemPairList(needUpdatePieces, items));
        }
        return completedFuture(Collections.<PieceItemPair>emptyList());
      })
      .thenCompose(pieceItemPairs -> {
        List<CompletableFuture<String>> updatedItemIds = new ArrayList<>(pieceItemPairs.size());
        pieceItemPairs.forEach(pair -> {
          JsonObject item = pair.getItem();
          if (isLocationContainsItemLocation(polLocations, item)) {
            item.put(ITEM_HOLDINGS_RECORD_ID, holder.getNewHoldingId());
            updatedItemIds.add(saveItem(item, requestContext));
          }
        });
        // Wait for all items to be created and corresponding updatedItemIds are built
        return collectResultsOnSuccess(updatedItemIds)
          .thenApply(results -> {
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
      .map(this::extractId)
      .collect(toList());
  }

  /**
   * Returns Id of the Instance Record corresponding to given PO line.
   * Instance record is either retrieved from Inventory or a new one is created if no corresponding Record exists.
   *
   * @param compPOL PO line to retrieve Instance Record Id for
   * @return future with Instance Id
   */
  public CompletableFuture<String> getInstanceRecord(CompositePoLine compPOL, RequestContext requestContext) {
    // proceed with new Instance Record creation if no productId is provided
    if (!isProductIdsExist(compPOL)) {
      return createInstanceRecord(compPOL, requestContext);
    }

    String query = compPOL.getDetails().getProductIds().stream()
      .map(this::buildProductIdQuery)
      .collect(joining(" or "));

    // query contains special characters so must be encoded before submitting
    RequestEntry requestEntry = new RequestEntry(INVENTORY_LOOKUP_ENDPOINTS.get(INSTANCES))
                                        .withQuery(query).withOffset(0).withLimit(Integer.MAX_VALUE);
    return restClient.getAsJsonObject(requestEntry, requestContext)
      .thenCompose(instances -> {
        if (!instances.getJsonArray(INSTANCES).isEmpty()) {
          return completedFuture(extractId(getFirstObjectFromResponse(instances, INSTANCES)));
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
  private CompletableFuture<String> createInstanceRecord(CompositePoLine compPOL, RequestContext requestContext) {
    logger.debug("Start processing instance record");
    JsonObject lookupObj = new JsonObject();
    CompletableFuture<Void> instanceTypeFuture = getEntryId(INSTANCE_TYPES, MISSING_INSTANCE_TYPE, requestContext)
      .thenAccept(lookupObj::mergeIn);

    CompletableFuture<Void> statusFuture = getEntryId(INSTANCE_STATUSES, MISSING_INSTANCE_STATUS, requestContext)
      .thenAccept(lookupObj::mergeIn);

    CompletableFuture<Void> contributorNameTypeIdFuture = verifyContributorNameTypesExist(compPOL.getContributors(), requestContext);

    return CompletableFuture.allOf(instanceTypeFuture, statusFuture, contributorNameTypeIdFuture)
      .thenApply(v -> buildInstanceRecordJsonObject(compPOL, lookupObj))
      .thenCompose(instanceRecJson -> {
        logger.debug("Instance record to save : {}", instanceRecJson);
        RequestEntry requestEntry = new RequestEntry(INVENTORY_LOOKUP_ENDPOINTS.get(INSTANCES));
        return restClient.post(requestEntry, instanceRecJson, PostResponseType.UUID, String.class, requestContext);
      });
  }

  public CompletableFuture<Void> verifyContributorNameTypesExist(List<Contributor> contributors, RequestContext requestContext) {
    List<String> ids = contributors.stream()
      .map(contributor -> contributor.getContributorNameTypeId().toLowerCase())
      .distinct()
      .collect(toList());

    return getContributorNameTypes(ids, requestContext)
      .thenAccept(contributorNameTypes -> {
        List<String> retrievedIds = contributorNameTypes.stream()
          .map(o -> o.getString(ID).toLowerCase())
          .collect(toList());
        if (retrievedIds.size() != ids.size()) {
          ids.removeAll(retrievedIds);
          throw new HttpException(500, buildErrorWithParameter(String.join(", ", ids), MISSING_CONTRIBUTOR_NAME_TYPE));
        }
      });
  }

  private CompletableFuture<List<JsonObject>> getContributorNameTypes(List<String> ids, RequestContext requestContext) {
    return collectResultsOnSuccess(StreamEx
      .ofSubLists(ids, MAX_IDS_FOR_GET_RQ)
      .map(idChunk -> getContributorNameTypeByIds(idChunk, requestContext))
      .toList())
      .thenApply(lists -> StreamEx.of(lists).toFlatList(contributorNameTypes -> contributorNameTypes));
  }

  private CompletableFuture<List<JsonObject>> getContributorNameTypeByIds(List<String> ids, RequestContext requestContext) {
    String query = convertIdsToCqlQuery(ids);
    RequestEntry requestEntry = new RequestEntry(INVENTORY_LOOKUP_ENDPOINTS.get(CONTRIBUTOR_NAME_TYPES))
                                        .withQuery(query).withOffset(0).withLimit(ids.size());
    return restClient.getAsJsonObject(requestEntry, requestContext)
                      .thenApply(entries -> entries.getJsonArray(CONTRIBUTOR_NAME_TYPES).stream()
                          .map(JsonObject::mapFrom)
                          .collect(Collectors.toList())
                      )
                      .exceptionally(e -> {
                        logger.error("The issue happened getting contributor name types", e);
                        throw new CompletionException(e.getCause());
                      });
  }

  public CompletableFuture<JsonObject> getEntryId(String entryType, ErrorCodes errorCode, RequestContext requestContext) {
    CompletableFuture<JsonObject> future = new CompletableFuture<>();
    getAndCache(entryType, requestContext)
      .thenAccept(future::complete)
      .exceptionally(throwable -> {
        getEntryTypeValue(entryType, requestContext)
          .thenAccept(entryTypeValue -> future.completeExceptionally(new HttpException(500, buildErrorWithParameter(entryTypeValue, errorCode))));
        return null;
      });
    return future;
  }

  private Error buildErrorWithParameter(String value, ErrorCodes errorCode) {
    List<Parameter> parameters = new ArrayList<>();
    parameters.add(new Parameter().withKey("missingEntry").withValue(value));
    return errorCode.toError()
      .withParameters(parameters);
  }

  public String buildProductIdQuery(ProductId productId) {
    return String.format(
      "(identifiers adj \"\\\"identifierTypeId\\\": \\\"%s\\\"\" " + "and identifiers adj \"\\\"value\\\": \\\"%s\\\"\")",
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

    if(isNotEmpty(compPOL.getContributors())) {
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
    if(isNotEmpty(titleContributors)) {
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

  public CompletableFuture<List<JsonObject>> getItemsByHoldingId(String holdingId, RequestContext requestContext) {
    String query = String.format("holdingsRecordId==%s", holdingId);
    RequestEntry requestEntry = new RequestEntry(INVENTORY_LOOKUP_ENDPOINTS.get(ITEMS)).withQuery(query).withQueryParameter(LANG, "en")
                                        .withOffset(0).withLimit(Integer.MAX_VALUE);
    return restClient.getAsJsonObject(requestEntry, requestContext)
      .thenApply(itemsCollection -> {
        List<JsonObject> items = extractEntities(itemsCollection, ITEMS);
        logger.debug("{} existing items found for holding with '{}' id", items.size(), holdingId);
        return items;
      });
  }

  /**
   * Creates Items in the inventory based on the PO line data.
   *
   * @param compPOL PO line to create Instance Record for
   * @param holdingId holding id
   * @param quantity expected number of items to create
   * @return id of newly created Instance Record
   */
  public CompletableFuture<List<String>> createMissingElectronicItems(CompositePoLine compPOL, String holdingId, int quantity,
                                                                      RequestContext requestContext) {
    if (quantity > 0) {
      return buildElectronicItemRecordJsonObject(compPOL, holdingId, requestContext)
        .thenCompose(itemData -> {
          logger.debug("Posting {} electronic item(s) for PO Line with '{}' id", quantity, compPOL.getId());
          return createItemRecords(itemData, quantity, requestContext);
        });
    } else {
      return completedFuture(Collections.emptyList());
    }
  }

  /**
   * Creates Items in the inventory based on the PO line data.
   *
   * @param compPOL PO line to create Instance Record for
   * @param holdingId holding id
   * @param quantity expected number of items to create
   * @return id of newly created Instance Record
   */
  public CompletableFuture<List<String>> createMissingPhysicalItems(CompositePoLine compPOL, String holdingId, int quantity,
                                                                    RequestContext requestContext) {
    if (quantity > 0) {
      return buildPhysicalItemRecordJsonObject(compPOL, holdingId, requestContext)
        .thenCompose(itemData -> {
          logger.debug("Posting {} physical item(s) for PO Line with '{}' id", quantity, compPOL.getId());
          return createItemRecords(itemData, quantity, requestContext);
        });
    } else {
      return completedFuture(Collections.emptyList());
    }
  }

   public String extractId(JsonObject json) {
    return json.getString(ID);
  }

  public Integer extractTotalRecords(JsonObject json) {
    return json.getInteger(TOTAL_RECORDS);
  }

  private CompletableFuture<String> getLoanTypeId(RequestContext requestContext) {
    ///loan-types?query=name==%s&limit=1&lang=%s
    return getEntryId(LOAN_TYPES, MISSING_LOAN_TYPE, requestContext)
      .thenApply(jsonObject -> jsonObject.getString(LOAN_TYPES));
  }

  /**
   * Accepts response with collection of the elements and tries to extract the first one.
   * In case the response is incorrect or empty, the {@link CompletionException} will be thrown
   * @param response     {@link JsonObject} representing service response which should contain array of objects
   * @param propertyName name of the property which holds array of objects
   * @return the first element of the array
   */
  public JsonObject getFirstObjectFromResponse(JsonObject response, String propertyName) {
    return Optional.ofNullable(response.getJsonArray(propertyName))
      .flatMap(items -> items.stream().findFirst())
      .map(JsonObject.class::cast)
      .orElseThrow(() -> new CompletionException(new InventoryException(String.format("No records of '%s' can be found", propertyName))));
  }


  /**
   * Caches id's in Vert.X Context and returns it by tenantId.entryType.key.
   *
   * @param entryType name of object whose id we want to get from cache
   *
   * @return configuration value by entry type
   */
  public CompletableFuture<JsonObject> getAndCache(String entryType, RequestContext requestContext) {
    return getEntryTypeValue(entryType, requestContext)
      .thenCompose(key -> {
        Context ctx = requestContext.getContext();
        Map<String, String> okapiHeaders = requestContext.getHeaders();
        String tenantSpecificKey = buildTenantSpecificKey(key, entryType, okapiHeaders);
        JsonObject response = ctx.get(tenantSpecificKey);
        if(response == null) {
        String endpoint = buildLookupEndpoint(entryType, encodeQuery(key, logger));
        return handleGetRequest(endpoint, restClient.getHttpClient(requestContext.getHeaders()), okapiHeaders, logger)
            .thenApply(entries -> {
              JsonObject result = new JsonObject();
              result.put(entryType, getFirstObjectFromResponse(entries, entryType).getString(ID));
              ctx.put(tenantSpecificKey, result);
              return result;
            });
        } else {
          return completedFuture(response);
        }
      });
  }

  public CompletableFuture<String> getProductTypeUuidByIsbn(String identifierType, RequestContext requestContext) {
    // return id of already retrieved identifier type
      String endpoint = String.format("/identifier-types?limit=1&query=name==%s", identifierType);
      Map<String, String> okapiHeaders = requestContext.getHeaders();
      return handleGetRequest(endpoint, restClient.getHttpClient(okapiHeaders), okapiHeaders, logger)
        .thenCompose(identifierTypes -> {
          String identifierTypeId = extractId(getFirstObjectFromResponse(identifierTypes, IDENTIFIER_TYPES));
          return completedFuture(identifierTypeId);
        });
  }

  public CompletableFuture<String> convertToISBN13(String isbn, RequestContext requestContext) {
    String convertEndpoint = String.format("/isbn/convertTo13?isbn=%s", isbn);
    Map<String, String> okapiHeaders = requestContext.getHeaders();
    return handleGetRequest(convertEndpoint, restClient.getHttpClient(okapiHeaders), okapiHeaders, logger)
      .thenApply(json -> json.getString("isbn"))
      .exceptionally(throwable -> {
        logger.error("Can't convert {} to isbn13", isbn);
        List<Parameter> parameters = Collections.singletonList(new Parameter().withKey("isbn").withValue(isbn));
        throw new HttpException(400, ISBN_NOT_VALID.toError().withParameters(parameters));
      });
  }

  public CompletableFuture<Void> updateItemWithPoLineId(String itemId, String poLineId, RequestContext requestContext) {
    if (itemId == null || poLineId == null) return CompletableFuture.completedFuture(null);

    return getItemRecordsByIds(ImmutableList.of(itemId), requestContext)
      .thenCompose(items -> {
        if (items.isEmpty() || poLineId.equals(items.get(0).getString(ITEM_PURCHASE_ORDER_LINE_IDENTIFIER))) {
          return CompletableFuture.completedFuture(null);
        } else {
          return updateItem(items.get(0).put(ITEM_PURCHASE_ORDER_LINE_IDENTIFIER, poLineId), requestContext);
        }
      });
  }

  public CompletableFuture<JsonObject> searchInstancesByProducts(List<ProductId> productIds, RequestContext requestContext) {
    String query = productIds.stream()
      .map(this::buildProductIdQuery)
      .collect(joining(" or "));

    // query contains special characters so must be encoded before submitting
    String endpoint = buildLookupEndpoint(INSTANCES, encodeQuery(query, logger));
    Map<String, String> okapiHeaders = requestContext.getHeaders();
    return handleGetRequest(endpoint, restClient.getHttpClient(okapiHeaders), okapiHeaders, logger);
  }

  public CompletableFuture<String> createInstanceRecord(Title title, RequestContext requestContext) {
    JsonObject lookupObj = new JsonObject();
    CompletableFuture<Void> instanceTypeFuture = getEntryId(INSTANCE_TYPES, MISSING_INSTANCE_TYPE, requestContext)
      .thenAccept(lookupObj::mergeIn);

    CompletableFuture<Void> statusFuture = getEntryId(INSTANCE_STATUSES, MISSING_INSTANCE_STATUS, requestContext)
      .thenAccept(lookupObj::mergeIn);

    CompletableFuture<Void> contributorNameTypeIdFuture = verifyContributorNameTypesExist(title.getContributors(), requestContext);

    return allOf(instanceTypeFuture, statusFuture, contributorNameTypeIdFuture)
      .thenApply(v -> buildInstanceRecordJsonObject(title, lookupObj))
      .thenCompose(instanceJson -> createInstance(instanceJson, requestContext));
  }

  public CompletableFuture<String> createInstance(JsonObject instanceRecJson, RequestContext requestContext) {
    RequestEntry requestEntry = new RequestEntry(INVENTORY_LOOKUP_ENDPOINTS.get(INSTANCES));
    return restClient.post(requestEntry, instanceRecJson, PostResponseType.UUID,  String.class, requestContext);
  }

  public CompletionStage<Void> deleteHolding(String holdingId, RequestContext requestContext) {
    RequestEntry requestEntry = new RequestEntry(INVENTORY_LOOKUP_ENDPOINTS.get(HOLDINGS_RECORDS_BY_ID_ENDPOINT)).withId(holdingId).withQueryParameter(LANG, "en");
    return restClient.delete(requestEntry, requestContext);
  }

  public CompletableFuture<List<JsonObject>> getItemsByStatus(List<String> poLineIds, String itemStatus, RequestContext requestContext) {
    logger.debug("getItemsByStatus start");
    List<CompletableFuture<List<JsonObject>>> futures = StreamEx
      .ofSubLists(poLineIds, MAX_IDS_FOR_GET_RQ)
      .map(ids -> {
        String query = String.format("status.name==%s and %s", itemStatus, HelperUtils.convertFieldListToCqlQuery(ids, InventoryManager.ITEM_PURCHASE_ORDER_LINE_IDENTIFIER, true));
        return getItemRecordsByQuery(query, requestContext);
      })
      .toList();

    return collectResultsOnSuccess(futures)
      .thenApply(lists -> StreamEx.of(lists).toFlatList(jsonObjects -> jsonObjects));
  }

  public CompletableFuture<String> getOrCreateInstanceRecord(Title title, RequestContext requestContext) {
    // proceed with new Instance Record creation if no productId is provided
    if (!CollectionUtils.isNotEmpty(title.getProductIds())) {
      return createInstanceRecord(title, requestContext);
    }

    return searchInstancesByProducts(title.getProductIds(), requestContext)
      .thenCompose(instances -> {
        if (!instances.getJsonArray(INSTANCES).isEmpty()) {
          String instanceId = getFirstObjectFromResponse(instances, INSTANCES).getString(ID);
          return completedFuture(instanceId);
        }
        return createInstanceRecord(title, requestContext);
      });
  }


  private void addHoldingId(List<Location> polLocations, String holdingId) {
    polLocations.forEach(location -> {
      location.setHoldingId(holdingId);
    });
  }

  private void validateItemsCreation(String poLineId, int expectedItemsQuantity, int itemsSize) {
    if (itemsSize != expectedItemsQuantity) {
      String message = String.format("Error creating items for PO Line with '%s' id. Expected %d but %d created",
        poLineId, expectedItemsQuantity, itemsSize);
      throw new InventoryException(message);
    }
  }

  private String buildTenantSpecificKey(String key, String entryType, Map<String, String> okapiHeaders) {
    String tenantId = TenantTool.tenantId(okapiHeaders);
    return String.format(TENANT_SPECIFIC_KEY_FORMAT, tenantId, entryType, key);
  }

  /**
   * Loads configuration and gets tenant specific value
   * @param entryType type of the entry
   * @return tenant specific value or system default one
   */
  private CompletableFuture<String> getEntryTypeValue(String entryType, RequestContext requestContext) {
    return configurationEntriesService.loadConfiguration(ORDER_CONFIG_MODULE_NAME, requestContext)
      .thenApply(configs -> {
        switch (entryType) {
          case INSTANCE_STATUSES:
            return configs.getString(CONFIG_NAME_INSTANCE_STATUS_CODE, DEFAULT_INSTANCE_STATUS_CODE);
          case INSTANCE_TYPES:
            return configs.getString(CONFIG_NAME_INSTANCE_TYPE_CODE, DEFAULT_INSTANCE_TYPE_CODE);
          case LOAN_TYPES:
            return configs.getString(CONFIG_NAME_LOAN_TYPE_NAME, DEFAULT_LOAN_TYPE_NAME);
          default:
            throw new IllegalArgumentException("Unexpected inventory entry type: " + entryType);
        }
      });
  }

  /**
   * Wait for item creation requests completion and filter failed items if any
   * @param itemRecord item record to be created
   * @param expectedCount count of the items to be created
   * @return completable future with list of item id's
   */
  private CompletableFuture<List<String>> createItemRecords(JsonObject itemRecord, int expectedCount, RequestContext requestContext) {
    List<CompletableFuture<String>> futures = new ArrayList<>(expectedCount);
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
  private CompletableFuture<String> createItemInInventory(JsonObject itemData, RequestContext requestContext) {
    CompletableFuture<String> future = new CompletableFuture<>();
    RequestEntry requestEntry = new RequestEntry(ITEM_STOR_ENDPOINT).withQueryParameter(LANG, "en");
    logger.info("Trying to create Item in inventory");
    restClient.post(requestEntry, itemData, PostResponseType.UUID, String.class, requestContext)
      .thenApply(future::complete)
      // In case item creation failed, return null instead of id
      .exceptionally(throwable -> {
        logger.error(ITEM_CREATION_FAILED.getDescription());
        future.complete(null);
        return null;
      });
    return future;
  }

  /**
   * Builds JsonObject representing inventory item minimal data. The schema is located directly in 'mod-inventory-storage' module.
   *
   * @param compPOL   PO line to create Item Records for
   * @param holdingId holding uuid from the inventory
   * @return item data to be used as request body for POST operation
   */
  private CompletableFuture<JsonObject> buildBaseItemRecordJsonObject(CompositePoLine compPOL, String holdingId, RequestContext requestContext) {
    return getLoanTypeId(requestContext)
      .thenApply(loanTypeId -> {
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
  private CompletableFuture<JsonObject> buildElectronicItemRecordJsonObject(CompositePoLine compPOL, String holdingId, RequestContext requestContext) {
    return buildBaseItemRecordJsonObject(compPOL, holdingId, requestContext)
      .thenApply(itemRecord -> itemRecord.put(ITEM_MATERIAL_TYPE_ID, compPOL.getEresource().getMaterialType()));
  }

  /**
   * Builds JsonObject representing inventory item minimal data. The schema is located directly in 'mod-inventory-storage' module.
   *
   * @param compPOL   PO line to create Item Records for
   * @param holdingId holding uuid from the inventory
   * @return item data to be used as request body for POST operation
   */
  private CompletableFuture<JsonObject> buildPhysicalItemRecordJsonObject(CompositePoLine compPOL, String holdingId, RequestContext requestContext) {
    return buildBaseItemRecordJsonObject(compPOL, holdingId, requestContext)
      .thenApply(itemRecord -> itemRecord.put(ITEM_MATERIAL_TYPE_ID, compPOL.getPhysical().getMaterialType()));
  }

  private JsonObject getHoldingByLocationId(JsonObject holdings, String locationId) {
    JsonObject prevHolding;
    prevHolding = holdings.getJsonArray(HOLDINGS_RECORDS).stream()
      .filter(item -> ((JsonObject) item).getString(HOLDING_PERMANENT_LOCATION_ID).equals(locationId))
      .map(JsonObject.class::cast)
      .findAny()
      .orElseThrow(() -> new CompletionException(new InventoryException(String.format("No records for location '%s' can be found", locationId))));
    return prevHolding;
  }

  /**
   * Validates if the json object contains entries and returns entries as list of JsonObject elements
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
   * @param poLineId purchase order line Id
   * @param holdingId holding uuid from the inventory
   * @param expectedQuantity expected quantity of the items for combination of the holding and PO Line uuid's from the inventory
   * @return future with list of item id's
   */
  private CompletableFuture<List<JsonObject>> searchStorageExistingItems(String poLineId, String holdingId, int expectedQuantity,
                                                                         RequestContext requestContext) {
    String query = String.format(LOOKUP_ITEM_QUERY, poLineId, holdingId);
    RequestEntry requestEntry = new RequestEntry(ITEM_STOR_ENDPOINT).withQuery(query).withOffset(0).withLimit(expectedQuantity);
    return restClient.getAsJsonObject(requestEntry, requestContext)
      .thenApply(itemsCollection -> {
        List<JsonObject> items = extractEntities(itemsCollection, ITEMS);
        logger.debug("{} existing items found out of {} for PO Line with '{}' id", items.size(), expectedQuantity, poLineId);
        return items;
      });
  }

  private CompletableFuture<List<JsonObject>> fetchHoldingsByFundIds(List<String> holdingIds, RequestContext requestContext) {
    String query = convertIdsToCqlQuery(holdingIds);
    RequestEntry requestEntry = new RequestEntry(INVENTORY_LOOKUP_ENDPOINTS.get(HOLDINGS_RECORDS))
      .withQuery(query).withOffset(0).withLimit(MAX_IDS_FOR_GET_RQ);
    return restClient.getAsJsonObject(requestEntry, requestContext)
      .thenApply(jsonHoldings -> jsonHoldings.getJsonArray(HOLDINGS_RECORDS).stream().map(o -> ((JsonObject) o)).collect(toList()))
      .thenApply(holdings -> {
        if (holdings.size() == holdingIds.size()) {
          return holdings;
        }
        List<Parameter> parameters = holdingIds.stream()
          .filter(id -> holdings.stream()
            .noneMatch(holding -> holding.getString(ID).equals(id)))
            .map(id -> new Parameter().withValue(id).withKey("holdings"))
          .collect(Collectors.toList());
        throw new HttpException(404, FUNDS_NOT_FOUND.toError().withParameters(parameters));
      });
  }
}
