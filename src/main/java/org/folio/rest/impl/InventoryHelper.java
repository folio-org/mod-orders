package org.folio.rest.impl;

import static java.util.Collections.singletonList;
import static java.util.concurrent.CompletableFuture.completedFuture;
import static java.util.stream.Collectors.joining;
import static java.util.stream.Collectors.toList;
import static me.escoffier.vertx.completablefuture.VertxCompletableFuture.allOf;
import static org.apache.commons.collections4.CollectionUtils.isNotEmpty;
import static org.folio.orders.utils.ErrorCodes.ISBN_NOT_VALID;
import static org.folio.orders.utils.ErrorCodes.ITEM_CREATION_FAILED;
import static org.folio.orders.utils.ErrorCodes.MISSING_CONTRIBUTOR_NAME_TYPE;
import static org.folio.orders.utils.ErrorCodes.MISSING_INSTANCE_STATUS;
import static org.folio.orders.utils.ErrorCodes.MISSING_INSTANCE_TYPE;
import static org.folio.orders.utils.ErrorCodes.MISSING_LOAN_TYPE;
import static org.folio.orders.utils.HelperUtils.collectResultsOnSuccess;
import static org.folio.orders.utils.HelperUtils.convertIdsToCqlQuery;
import static org.folio.orders.utils.HelperUtils.encodeQuery;
import static org.folio.orders.utils.HelperUtils.groupLocationsById;
import static org.folio.orders.utils.HelperUtils.handleGetRequest;
import static org.folio.orders.utils.HelperUtils.handlePutRequest;
import static org.folio.orders.utils.HelperUtils.isItemsUpdateRequired;
import static org.folio.orders.utils.HelperUtils.isProductIdsExist;
import static org.folio.rest.acq.model.Piece.Format.ELECTRONIC;

import java.util.ArrayList;
import java.util.Collections;
import java.util.HashMap;
import java.util.List;
import java.util.Map;
import java.util.Optional;
import java.util.concurrent.CompletableFuture;
import java.util.concurrent.CompletionException;
import java.util.stream.Collectors;

import org.apache.commons.collections4.ListUtils;
import org.apache.commons.lang3.StringUtils;
import org.folio.orders.rest.exceptions.HttpException;
import org.folio.orders.rest.exceptions.InventoryException;
import org.folio.orders.utils.ErrorCodes;
import org.folio.orders.utils.HelperUtils;
import org.folio.rest.acq.model.Piece;
import org.folio.rest.jaxrs.model.CheckInPiece;
import org.folio.rest.jaxrs.model.CompositePoLine;
import org.folio.rest.jaxrs.model.Contributor;
import org.folio.rest.jaxrs.model.Error;
import org.folio.rest.jaxrs.model.Location;
import org.folio.rest.jaxrs.model.Parameter;
import org.folio.rest.jaxrs.model.ProductId;
import org.folio.rest.jaxrs.model.ReceivedItem;
import org.folio.rest.tools.client.interfaces.HttpClientInterface;
import org.folio.rest.tools.utils.TenantTool;

import io.vertx.core.Context;
import io.vertx.core.json.JsonArray;
import io.vertx.core.json.JsonObject;
import me.escoffier.vertx.completablefuture.VertxCompletableFuture;
import one.util.streamex.IntStreamEx;
import one.util.streamex.StreamEx;

public class InventoryHelper extends AbstractHelper {

  private static final String IDENTIFIER_TYPES = "identifierTypes";
  private static final String SOURCE_FOLIO = "FOLIO";
  static final String INSTANCE_SOURCE = "source";
  static final String INSTANCE_TITLE = "title";
  static final String INSTANCE_EDITIONS = "editions";
  static final String INSTANCE_STATUS_ID = "statusId";
  static final String INSTANCE_TYPE_ID = "instanceTypeId";
  static final String INSTANCE_PUBLISHER = "publisher";
  static final String INSTANCE_CONTRIBUTORS = "contributors";
  static final String INSTANCE_DATE_OF_PUBLICATION = "dateOfPublication";
  static final String INSTANCE_PUBLICATION = "publication";
  static final String INSTANCE_IDENTIFIER_TYPE_ID = "identifierTypeId";
  static final String INSTANCE_IDENTIFIERS = "identifiers";
  static final String INSTANCE_IDENTIFIER_TYPE_VALUE = "value";
  static final String HOLDING_INSTANCE_ID = "instanceId";
  static final String HOLDING_PERMANENT_LOCATION_ID = "permanentLocationId";
  static final String ITEM_HOLDINGS_RECORD_ID = "holdingsRecordId";
  static final String ITEM_BARCODE = "barcode";
  static final String ITEM_LEVEL_CALL_NUMBER = "itemLevelCallNumber";
  static final String ITEM_STATUS = "status";
  static final String ITEM_STATUS_NAME = "name";
  static final String ITEM_MATERIAL_TYPE_ID = "materialTypeId";
  static final String ITEM_PERMANENT_LOAN_TYPE_ID = "permanentLoanTypeId";
  static final String ITEM_PURCHASE_ORDER_LINE_IDENTIFIER = "purchaseOrderLineIdentifier";
  static final String CONTRIBUTOR_NAME = "name";
  static final String CONTRIBUTOR_NAME_TYPE_ID = "contributorNameTypeId";
  static final String CONTRIBUTOR_NAME_TYPES = "contributorNameTypes";
  static final String INSTANCE_STATUSES = "instanceStatuses";
  static final String INSTANCE_TYPES = "instanceTypes";
  public static final String ITEMS = "items";
  static final String LOAN_TYPES = "loantypes";

  // mod-configuration: config names and default values
  static final String CONFIG_NAME_INSTANCE_TYPE_CODE = "inventory-instanceTypeCode";
  static final String CONFIG_NAME_INSTANCE_STATUS_CODE = "inventory-instanceStatusCode";
  static final String CONFIG_NAME_LOAN_TYPE_NAME = "inventory-loanTypeName";
  static final String DEFAULT_INSTANCE_TYPE_CODE = "zzz";
  static final String DEFAULT_INSTANCE_STATUS_CODE = "temp";
  static final String DEFAULT_LOAN_TYPE_NAME = "Can circulate";

  private static final String HOLDINGS_RECORDS = "holdingsRecords";
  private static final String INSTANCES = "instances";

  private static final String TENANT_SPECIFIC_KEY_FORMAT = "%s.%s.%s";
  private static final String CREATE_INSTANCE_ENDPOINT = "/inventory/instances?lang=%s";
  private static final String LOOKUP_ITEM_STOR_QUERY = "purchaseOrderLineIdentifier==%s and holdingsRecordId==%s";
  private static final String LOOKUP_ITEM_STOR_ENDPOINT = "/item-storage/items?query=%s&limit=%d&lang=%s";
  private static final String CREATE_ITEM_STOR_ENDPOINT = "/item-storage/items?lang=%s";
  private static final String UPDATE_ITEM_ENDPOINT = "/inventory/items/%s?lang=%s";
  private static final String HOLDINGS_LOOKUP_QUERY = "instanceId==%s and permanentLocationId==%s";
  private static final String HOLDINGS_CREATE_ENDPOINT = "/holdings-storage/holdings?lang=%s";
  public static final String ID = "id";

  private static final Map<String, String> INVENTORY_LOOKUP_ENDPOINTS;

  static {
    Map<String, String> apis = new HashMap<>();
    apis.put(CONTRIBUTOR_NAME_TYPES, "/contributor-name-types?limit=%s&query=%s&lang=%s");
    apis.put(HOLDINGS_RECORDS, "/holdings-storage/holdings?query=%s&limit=1&lang=%s");
    apis.put(LOAN_TYPES, "/loan-types?query=name==%s&limit=1&lang=%s");
    apis.put(INSTANCE_STATUSES, "/instance-statuses?query=code==%s&limit=1&lang=%s");
    apis.put(INSTANCE_TYPES, "/instance-types?query=code==%s&lang=%s");
    apis.put(INSTANCES, "/inventory/instances?query=%s&lang=%s");
    apis.put(ITEMS, "/inventory/items?query=%s&limit=%d&lang=%s");

    INVENTORY_LOOKUP_ENDPOINTS = Collections.unmodifiableMap(apis);
  }

  InventoryHelper(HttpClientInterface httpClient, Map<String, String> okapiHeaders, Context ctx, String lang) {
    super(httpClient, okapiHeaders, ctx, lang);
  }

  public CompletableFuture<CompositePoLine> handleInstanceRecord(CompositePoLine compPOL) {
    if(compPOL.getInstanceId() != null) {
      return CompletableFuture.completedFuture(compPOL);
    } else {
      return getInstanceRecord(compPOL)
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
  public CompletableFuture<List<Piece>> handleHoldingsAndItemsRecords(CompositePoLine compPOL) {
    List<CompletableFuture<List<Piece>>> itemsPerHolding = new ArrayList<>();
    boolean isItemsUpdateRequired = isItemsUpdateRequired(compPOL);

    // Group all locations by location id because the holding should be unique for different locations
    if (HelperUtils.isHoldingsUpdateRequired(compPOL.getEresource(), compPOL.getPhysical())) {
      groupLocationsById(compPOL)
        .forEach((locationId, locations) -> itemsPerHolding.add(
          // Search for or create a new holdings record and then create items for it if required
          getOrCreateHoldingsRecord(compPOL.getInstanceId(), locationId)
            .thenCompose(holdingId -> {
                // Items are not going to be created when create inventory is "Instance, Holding"
                if (isItemsUpdateRequired) {
                  return handleItemRecords(compPOL, holdingId, locations);
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
   * Returns list of item records for specified id's.
   *
   * @param ids   List of item id's
   * @return future with list of item records
   */
  public CompletableFuture<List<JsonObject>> getItemRecordsByIds(List<String> ids) {
    String query = encodeQuery(HelperUtils.convertIdsToCqlQuery(ids), logger);
    String endpoint = buildLookupEndpoint(ITEMS, query, ids.size(), lang);
    return handleGetRequest(endpoint, httpClient, ctx, okapiHeaders, logger)
      .thenApply(this::extractItems);
  }

  /**
   * Returns list of item records for specified query.
   *
   * @param query
   * @return future with list of item records
   */
  public CompletableFuture<List<JsonObject>> getItemRecordsByQuery(String query) {
    String endpoint = buildLookupEndpoint(ITEMS, query, Integer.MAX_VALUE, lang);
    return handleGetRequest(endpoint, httpClient, ctx, okapiHeaders, logger)
      .thenApply(this::extractItems);
  }

  public CompletableFuture<Void> updateItem(JsonObject item) {
    String endpoint = String.format(UPDATE_ITEM_ENDPOINT, item.getString(ID), lang);
    return handlePutRequest(endpoint, item, httpClient, ctx, okapiHeaders, logger);
  }

  /**
   * Returns list of item records for specified id's.
   *
   * @param itemRecord item record
   * @param receivedItem item details specified by user upon receiving flow
   * @return future with list of item records
   */
  public CompletableFuture<Void> receiveItem(JsonObject itemRecord, ReceivedItem receivedItem) {

    // Update item record with receiving details
    itemRecord.put(ITEM_STATUS, new JsonObject().put(ITEM_STATUS_NAME, receivedItem.getItemStatus().value()));
    if (StringUtils.isNotEmpty(receivedItem.getBarcode())) {
      itemRecord.put(ITEM_BARCODE, receivedItem.getBarcode());
    }
    if (StringUtils.isNotEmpty(receivedItem.getCallNumber())) {
      itemRecord.put(ITEM_LEVEL_CALL_NUMBER, receivedItem.getCallNumber());
    }
    return updateItem(itemRecord);
  }

  public CompletableFuture<Void> checkinItem(JsonObject itemRecord, CheckInPiece checkinPiece) {

    // Update item record with checkIn details
    itemRecord.put(ITEM_STATUS, new JsonObject().put(ITEM_STATUS_NAME, checkinPiece.getItemStatus().value()));
    if (StringUtils.isNotEmpty(checkinPiece.getBarcode())) {
      itemRecord.put(ITEM_BARCODE, checkinPiece.getBarcode());
    }
    if (StringUtils.isNotEmpty(checkinPiece.getCallNumber())) {
      itemRecord.put(ITEM_LEVEL_CALL_NUMBER, checkinPiece.getCallNumber());
    }
    return updateItem(itemRecord);
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

  CompletableFuture<String> getOrCreateHoldingsRecord(String instanceId, String locationId) {

    String query = encodeQuery(String.format(HOLDINGS_LOOKUP_QUERY, instanceId, locationId), logger);
    String endpoint = buildLookupEndpoint(HOLDINGS_RECORDS, query, lang);
    return handleGetRequest(endpoint, httpClient, ctx, okapiHeaders, logger)
          .thenCompose(holdings -> {
            if (!holdings.getJsonArray(HOLDINGS_RECORDS).isEmpty()) {
              return completedFuture(extractId(getFirstObjectFromResponse(holdings, HOLDINGS_RECORDS)));
            }
            return createHoldingsRecord(instanceId, locationId);
          });
  }

  private String buildLookupEndpoint(String type, Object... params) {
    return String.format(INVENTORY_LOOKUP_ENDPOINTS.get(type), params);
  }

  private CompletableFuture<String> createHoldingsRecord(String instanceId, String locationId) {
    JsonObject holdingsRecJson = new JsonObject();
    holdingsRecJson.put(HOLDING_INSTANCE_ID, instanceId);
    holdingsRecJson.put(HOLDING_PERMANENT_LOCATION_ID, locationId);

    return createRecordInStorage(holdingsRecJson, String.format(HOLDINGS_CREATE_ENDPOINT, lang));
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
  private CompletableFuture<List<Piece>> handleItemRecords(CompositePoLine compPOL, String holdingId, List<Location> locations) {
    Map<Piece.Format, Integer> piecesWithItemsQuantities = HelperUtils.calculatePiecesWithItemIdQuantity(compPOL, locations);
    int piecesWithItemsQty = IntStreamEx.of(piecesWithItemsQuantities.values()).sum();
    String polId = compPOL.getId();

    logger.debug("Handling {} items for PO Line with id={} and holdings with id={}", piecesWithItemsQty, polId, holdingId);
    if (piecesWithItemsQty == 0) {
      return completedFuture(Collections.emptyList());
    }

    // Search for already existing items
    return searchForExistingItems(compPOL, holdingId, piecesWithItemsQty)
      .thenCompose(existingItems -> {
        String locationId = locations.get(0).getLocationId();
        List<CompletableFuture<List<Piece>>> pieces = new ArrayList<>(Piece.Format.values().length);

        piecesWithItemsQuantities.forEach((pieceFormat, expectedQuantity) -> {
          // The expected quantity might be zero for particular piece format if the PO Line's order format is P/E Mix
          if (expectedQuantity > 0) {
            List<String> items;
            CompletableFuture<List<String>> newItems;

            // Depending on piece format get already existing items and send requests to create missing items
            if (pieceFormat == ELECTRONIC) {
              items = getElectronicItems(compPOL, existingItems);
              newItems = createMissingElectronicItems(compPOL, holdingId, expectedQuantity - items.size());
            } else {
              items = getPhysicalItems(compPOL, existingItems);
              newItems = createMissingPhysicalItems(compPOL, holdingId, expectedQuantity - items.size());
            }

            // Build piece records once new items are created
            pieces.add(newItems.thenApply(createdItemIds -> {
              List<String> itemIds = ListUtils.union(createdItemIds, items);
              logger.debug("Building {} {} piece(s) for PO Line with id={}", itemIds.size(), pieceFormat, polId);
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
          .thenApply(results -> results
            .stream()
            .flatMap(List::stream)
            .collect(toList())
          );
        }
      );
  }

  private List<String> getPhysicalItems(CompositePoLine compPOL, List<JsonObject> existingItems) {
    return getItemsByMaterialType(existingItems,compPOL.getPhysical().getMaterialType());
  }

  private List<String> getElectronicItems(CompositePoLine compPOL, List<JsonObject> existingItems) {
    return getItemsByMaterialType(existingItems, compPOL.getEresource().getMaterialType());
  }

  private List<String> getItemsByMaterialType(List<JsonObject> existingItems, String materialTypeId) {
    return existingItems
      .stream()
      .filter(item -> materialTypeId.equals(item.getString(ITEM_MATERIAL_TYPE_ID)))
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
  private CompletableFuture<String> getInstanceRecord(CompositePoLine compPOL) {
    // proceed with new Instance Record creation if no productId is provided
    if (!isProductIdsExist(compPOL)) {
      return createInstanceRecord(compPOL);
    }

    String query = compPOL.getDetails().getProductIds().stream()
      .map(this::buildProductIdQuery)
      .collect(joining(" or "));

    // query contains special characters so must be encoded before submitting
    String endpoint = buildLookupEndpoint(INSTANCES, encodeQuery(query, logger), lang);
    return handleGetRequest(endpoint, httpClient, ctx, okapiHeaders, logger)
      .thenCompose(instances -> {
        if (!instances.getJsonArray(INSTANCES).isEmpty()) {
          return completedFuture(extractId(getFirstObjectFromResponse(instances, INSTANCES)));
        }
        return createInstanceRecord(compPOL);
      });
  }

  /**
   * Creates Instance Record in Inventory and returns its Id.
   *
   * @param compPOL PO line to create Instance Record for
   * @return id of newly created Instance Record
   */
  private CompletableFuture<String> createInstanceRecord(CompositePoLine compPOL) {
    JsonObject lookupObj = new JsonObject();
    CompletableFuture<Void> instanceTypeFuture = getEntryId(INSTANCE_TYPES, MISSING_INSTANCE_TYPE)
      .thenAccept(lookupObj::mergeIn);

    CompletableFuture<Void> statusFuture = getEntryId(INSTANCE_STATUSES, MISSING_INSTANCE_STATUS)
      .thenAccept(lookupObj::mergeIn);

    CompletableFuture<Void> contributorNameTypeIdFuture = verifyContributorNameTypesExist(compPOL.getContributors());

    return allOf(ctx, instanceTypeFuture, statusFuture, contributorNameTypeIdFuture)
      .thenApply(v -> buildInstanceRecordJsonObject(compPOL, lookupObj))
      .thenCompose(instanceRecJson -> createRecordInStorage(instanceRecJson, String.format(CREATE_INSTANCE_ENDPOINT, lang)));
  }

  private CompletableFuture<Void> verifyContributorNameTypesExist(List<Contributor> contributors) {
    List<String> ids = contributors.stream()
      .map(contributor -> contributor.getContributorNameTypeId().toLowerCase())
      .distinct()
      .collect(toList());

    return getContributorNameTypes(ids)
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

  private CompletableFuture<List<JsonObject>> getContributorNameTypes(List<String> ids) {
    return collectResultsOnSuccess(StreamEx
      .ofSubLists(ids, MAX_IDS_FOR_GET_RQ)
      .map(this::getContributorNameTypeByIds)
      .toList())
      .thenApply(lists -> StreamEx.of(lists).toFlatList(contributorNameTypes -> contributorNameTypes));
  }

  private CompletableFuture<List<JsonObject>> getContributorNameTypeByIds(List<String> ids) {
    String query = encodeQuery(convertIdsToCqlQuery(ids), logger);
    String endpoint = buildLookupEndpoint(CONTRIBUTOR_NAME_TYPES, ids.size(), query, lang);
    return handleGetRequest(endpoint, httpClient, ctx, okapiHeaders, logger)
      .thenApply(entries -> entries.getJsonArray(CONTRIBUTOR_NAME_TYPES).stream()
        .map(json -> ((JsonObject) json))
        .collect(Collectors.toList())
      )
      .exceptionally(e -> {
        logger.error("The issue happened getting contributor name types", e);
        throw new CompletionException(e.getCause());
      });
  }

  private CompletableFuture<JsonObject> getEntryId(String entryType, ErrorCodes errorCode) {
    CompletableFuture<JsonObject> future = new VertxCompletableFuture<>();
    getAndCache(entryType)
      .thenAccept(future::complete)
      .exceptionally(throwable -> {
        getEntryTypeValue(entryType)
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

  private String buildProductIdQuery(ProductId productId) {
    return String.format(
        "(identifiers adj \"\\\"identifierTypeId\\\": \\\"%s\\\"\" " + "and identifiers adj \"\\\"value\\\": \\\"%s\\\"\")",
        productId.getProductIdType(), productId.getProductId());
  }

  private JsonObject buildInstanceRecordJsonObject(CompositePoLine compPOL, JsonObject lookupObj) {
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

  /**
   * Search for items which might be already created for the PO line
   * @param compPOL PO line to retrieve Item Records for
   * @param holdingId holding uuid from the inventory
   * @param expectedQuantity expected quantity of the items for combination of the holding and PO Line uuid's from the inventory
   * @return future with list of item id's
   */
  private CompletableFuture<List<JsonObject>> searchForExistingItems(CompositePoLine compPOL, String holdingId, int expectedQuantity) {
    String query = encodeQuery(String.format(LOOKUP_ITEM_STOR_QUERY, compPOL.getId(), holdingId), logger);
    String endpoint = String.format(LOOKUP_ITEM_STOR_ENDPOINT, query, expectedQuantity, lang);
    return handleGetRequest(endpoint, httpClient, ctx, okapiHeaders, logger)
      .thenApply(itemsCollection -> {
        List<JsonObject> items = extractItems(itemsCollection);
        logger.debug("{} existing items found out of {} for PO Line with '{}' id", items.size(), expectedQuantity, compPOL.getId());
        return items;
      });
  }

  /**
   * Validates if the json object contains items and returns items as list of JsonObject elements
   * @param itemEntries {@link JsonObject} representing item storage response
   * @return list of the item records as JsonObject elements
   */
  private List<JsonObject> extractItems(JsonObject itemEntries) {
    return Optional.ofNullable(itemEntries.getJsonArray(ITEMS))
                   .map(items -> items.stream()
                                      .map(item -> (JsonObject) item)
                                      .collect(toList()))
                   .orElseGet(Collections::emptyList);
  }

/**
   * Creates Items in the inventory based on the PO line data.
   *
   * @param compPOL PO line to create Instance Record for
   * @param holdingId holding id
   * @param quantity expected number of items to create
   * @return id of newly created Instance Record
   */
  private CompletableFuture<List<String>> createMissingElectronicItems(CompositePoLine compPOL, String holdingId, int quantity) {
    if (quantity > 0) {
      return buildElectronicItemRecordJsonObject(compPOL, holdingId)
        .thenCompose(itemData -> {
          logger.debug("Posting {} electronic item(s) for PO Line with '{}' id", quantity, compPOL.getId());
          return createItemRecords(itemData, quantity);
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
  private CompletableFuture<List<String>> createMissingPhysicalItems(CompositePoLine compPOL, String holdingId, int quantity) {
    if (quantity > 0) {
      return buildPhysicalItemRecordJsonObject(compPOL, holdingId)
        .thenCompose(itemData -> {
          logger.debug("Posting {} physical item(s) for PO Line with '{}' id", quantity, compPOL.getId());
          return createItemRecords(itemData, quantity);
        });
    } else {
      return completedFuture(Collections.emptyList());
    }
  }

  /**
   * Wait for item creation requests completion and filter failed items if any
   * @param itemRecord item record to be created
   * @param expectedCount count of the items to be created
   * @return completable future with list of item id's
   */
  private CompletableFuture<List<String>> createItemRecords(JsonObject itemRecord, int expectedCount) {
    List<CompletableFuture<String>> futures = new ArrayList<>(expectedCount);
    for (int i = 0; i < expectedCount; i++) {
      futures.add(createItemInInventory(itemRecord));
    }

    return collectResultsOnSuccess(futures);
  }

  /**
   * Creates new entry in the inventory storage based on the PO line data.
   *
   * @param itemData json to post
   * @return id of newly created entity Record
   */
  private CompletableFuture<String> createItemInInventory(JsonObject itemData) {
    return createRecordInStorage(itemData, String.format(CREATE_ITEM_STOR_ENDPOINT, lang))
      // In case item creation failed, return null instead of id
      .exceptionally(throwable -> {
        addProcessingError(ITEM_CREATION_FAILED.toError());
        return null;
      });
  }

  /**
   * Builds JsonObject representing inventory item minimal data. The schema is located directly in 'mod-inventory-storage' module.
   *
   * @param compPOL   PO line to create Item Records for
   * @param holdingId holding uuid from the inventory
   * @return item data to be used as request body for POST operation
   */
  private CompletableFuture<JsonObject> buildBaseItemRecordJsonObject(CompositePoLine compPOL, String holdingId) {
    return getLoanTypeId()
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
  private CompletableFuture<JsonObject> buildElectronicItemRecordJsonObject(CompositePoLine compPOL, String holdingId) {
    return buildBaseItemRecordJsonObject(compPOL, holdingId)
      .thenApply(itemRecord -> itemRecord.put(ITEM_MATERIAL_TYPE_ID, compPOL.getEresource().getMaterialType()));
  }

  /**
   * Builds JsonObject representing inventory item minimal data. The schema is located directly in 'mod-inventory-storage' module.
   *
   * @param compPOL   PO line to create Item Records for
   * @param holdingId holding uuid from the inventory
   * @return item data to be used as request body for POST operation
   */
  private CompletableFuture<JsonObject> buildPhysicalItemRecordJsonObject(CompositePoLine compPOL, String holdingId) {
    return buildBaseItemRecordJsonObject(compPOL, holdingId)
      .thenApply(itemRecord -> itemRecord.put(ITEM_MATERIAL_TYPE_ID, compPOL.getPhysical().getMaterialType()));
  }


  String extractId(JsonObject json) {
    return json.getString(ID);
  }

  private CompletableFuture<String> getLoanTypeId() {
    return getEntryId(LOAN_TYPES, MISSING_LOAN_TYPE)
      .thenApply(jsonObject -> jsonObject.getString(LOAN_TYPES));
  }

  /**
   * Accepts response with collection of the elements and tries to extract the first one.
   * In case the response is incorrect or empty, the {@link CompletionException} will be thrown
   * @param response     {@link JsonObject} representing service response which should contain array of objects
   * @param propertyName name of the property which holds array of objects
   * @return the first element of the array
   */
  private JsonObject getFirstObjectFromResponse(JsonObject response, String propertyName) {
    return Optional.ofNullable(response.getJsonArray(propertyName))
                   .flatMap(items -> items.stream().findFirst())
                   .map(item -> (JsonObject) item)
                   .orElseThrow(() -> new CompletionException(new InventoryException(String.format("No records of '%s' can be found", propertyName))));
  }

  /**
   * Caches id's in Vert.X Context and returns it by tenantId.entryType.key.
   *
   * @param entryType name of object whose id we want to get from cache
   *
   * @return value from cache
   */
  private CompletableFuture<JsonObject> getAndCache(String entryType) {
    return getEntryTypeValue(entryType)
      .thenCompose(key -> {
        String tenantSpecificKey = buildTenantSpecificKey(key, entryType);
        JsonObject response = ctx.get(tenantSpecificKey);
        if(response == null) {
          String endpoint = buildLookupEndpoint(entryType, encodeQuery(key, logger), lang);
          return handleGetRequest(endpoint, httpClient, ctx, okapiHeaders, logger)
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

  private String buildTenantSpecificKey(String key, String entryType) {
    String tenantId = TenantTool.tenantId(okapiHeaders);
    return String.format(TENANT_SPECIFIC_KEY_FORMAT, tenantId, entryType, key);
  }

  /**
   * Loads configuration and gets tenant specific value
   * @param entryType type of the entry
   * @return tenant specific value or system default one
   */
  private CompletableFuture<String> getEntryTypeValue(String entryType) {
    return getTenantConfiguration()
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

  public CompletableFuture<String> getProductTypeUUID(String identifierType) {
    String endpoint = String.format("/identifier-types?limit=1&query=name==%s&lang=%s", identifierType, lang);
    return handleGetRequest(endpoint, httpClient, ctx, okapiHeaders, logger)
      .thenCompose(identifierTypes -> completedFuture(extractId(getFirstObjectFromResponse(identifierTypes, IDENTIFIER_TYPES))));
  }

  public CompletableFuture<String> convertToISBN13(String isbn) {
    String convertEndpoint = String.format("/isbn/convertTo13?isbn=%s&lang=%s", isbn, lang);
    return handleGetRequest(convertEndpoint, httpClient, ctx, okapiHeaders, logger).thenApply(json -> json.getString("isbn"))
      .exceptionally(throwable -> {
        logger.error("Can't convert {} to isbn13", isbn);
        List<Parameter> parameters = Collections.singletonList(new Parameter().withKey("isbn").withValue(isbn));
        throw new HttpException(400, ISBN_NOT_VALID.toError().withParameters(parameters));
      });
  }
}
