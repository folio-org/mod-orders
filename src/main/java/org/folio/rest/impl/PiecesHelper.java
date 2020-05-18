package org.folio.rest.impl;

import static java.util.Collections.singletonList;
import static java.util.concurrent.CompletableFuture.completedFuture;
import static java.util.stream.Collectors.joining;
import static java.util.stream.Collectors.toList;
import static me.escoffier.vertx.completablefuture.VertxCompletableFuture.allOf;
import static org.apache.commons.collections4.CollectionUtils.isNotEmpty;
import static org.folio.orders.utils.ErrorCodes.MISSING_INSTANCE_STATUS;
import static org.folio.orders.utils.ErrorCodes.MISSING_INSTANCE_TYPE;
import static org.folio.orders.utils.HelperUtils.URL_WITH_LANG_PARAM;
import static org.folio.orders.utils.HelperUtils.collectResultsOnSuccess;
import static org.folio.orders.utils.HelperUtils.encodeQuery;
import static org.folio.orders.utils.HelperUtils.getPoLineById;
import static org.folio.orders.utils.HelperUtils.getPurchaseOrderById;
import static org.folio.orders.utils.HelperUtils.handleDeleteRequest;
import static org.folio.orders.utils.HelperUtils.handleGetRequest;
import static org.folio.orders.utils.HelperUtils.handlePutRequest;
import static org.folio.orders.utils.HelperUtils.isItemsUpdateRequired;
import static org.folio.orders.utils.ProtectedOperationType.DELETE;
import static org.folio.orders.utils.ResourcePathResolver.PIECES;
import static org.folio.orders.utils.ResourcePathResolver.resourceByIdPath;
import static org.folio.orders.utils.ResourcePathResolver.resourcesPath;
import static org.folio.rest.impl.InventoryHelper.CONTRIBUTOR_NAME;
import static org.folio.rest.impl.InventoryHelper.CONTRIBUTOR_NAME_TYPE_ID;
import static org.folio.rest.impl.InventoryHelper.INSTANCE_CONTRIBUTORS;
import static org.folio.rest.impl.InventoryHelper.INSTANCE_DATE_OF_PUBLICATION;
import static org.folio.rest.impl.InventoryHelper.INSTANCE_EDITIONS;
import static org.folio.rest.impl.InventoryHelper.INSTANCE_IDENTIFIERS;
import static org.folio.rest.impl.InventoryHelper.INSTANCE_IDENTIFIER_TYPE_ID;
import static org.folio.rest.impl.InventoryHelper.INSTANCE_IDENTIFIER_TYPE_VALUE;
import static org.folio.rest.impl.InventoryHelper.INSTANCE_PUBLICATION;
import static org.folio.rest.impl.InventoryHelper.INSTANCE_PUBLISHER;
import static org.folio.rest.impl.InventoryHelper.INSTANCE_SOURCE;
import static org.folio.rest.impl.InventoryHelper.INSTANCE_STATUSES;
import static org.folio.rest.impl.InventoryHelper.INSTANCE_STATUS_ID;
import static org.folio.rest.impl.InventoryHelper.INSTANCE_TITLE;
import static org.folio.rest.impl.InventoryHelper.INSTANCE_TYPES;
import static org.folio.rest.impl.InventoryHelper.INSTANCE_TYPE_ID;
import static org.folio.rest.impl.InventoryHelper.SOURCE_FOLIO;

import java.util.ArrayList;
import java.util.Collections;
import java.util.List;
import java.util.Map;
import java.util.concurrent.CompletableFuture;
import java.util.concurrent.CompletionException;
import java.util.concurrent.ExecutionException;

import javax.ws.rs.core.Response;

import org.apache.commons.collections4.CollectionUtils;
import org.folio.orders.events.handlers.MessageAddress;
import org.folio.orders.rest.exceptions.HttpException;
import org.folio.orders.utils.ErrorCodes;
import org.folio.orders.utils.HelperUtils;
import org.folio.orders.utils.ProtectedOperationType;
import org.folio.rest.jaxrs.model.CompositePoLine;
import org.folio.rest.jaxrs.model.CompositePurchaseOrder;
import org.folio.rest.jaxrs.model.Location;
import org.folio.rest.jaxrs.model.Piece;
import org.folio.rest.jaxrs.model.Piece.ReceivingStatus;
import org.folio.rest.jaxrs.model.Title;
import org.jetbrains.annotations.NotNull;

import io.vertx.core.Context;
import io.vertx.core.json.JsonArray;
import io.vertx.core.json.JsonObject;
import me.escoffier.vertx.completablefuture.VertxCompletableFuture;

public class PiecesHelper extends AbstractHelper {
  private static final String DELETE_PIECE_BY_ID = resourceByIdPath(PIECES, "%s") + "?lang=%s";
  private static final String CREATE_INSTANCE_ENDPOINT = "/inventory/instances?lang=%s";
  private static final String INSTANCES = "instances";

  private ProtectionHelper protectionHelper;
  private InventoryHelper inventoryHelper;
  private TitlesHelper titlesHelper;

  public PiecesHelper(Map<String, String> okapiHeaders, Context ctx, String lang) {
    super(okapiHeaders, ctx, lang);
    protectionHelper = new ProtectionHelper(httpClient, okapiHeaders, ctx, lang);
    inventoryHelper = new InventoryHelper(okapiHeaders, ctx, lang);
    titlesHelper = new TitlesHelper(okapiHeaders, ctx, lang);
  }

  public CompletableFuture<Piece> createPiece(Piece piece) {
      return getCompositeOrderByPoLineId(piece.getPoLineId())
        .thenCompose(order ->
          protectionHelper.isOperationRestricted(order.getAcqUnitIds(), ProtectedOperationType.CREATE)
                          .thenApply(v -> order)
        )
        .thenCompose(order -> updateInventory(order.getCompositePoLines().get(0), piece))
        .thenCompose(v -> createRecordInStorage(JsonObject.mapFrom(piece), resourcesPath(PIECES))
        .thenApply(piece::withId));
  }

  // Flow to update piece
  // 1. Before update, get piece by id from storage and store receiving status
  // 2. Update piece with new content and complete future
  // 3. Create a message and check if receivingStatus is not consistent with storage; if yes - send a message to event bus
  public CompletableFuture<Void> updatePieceRecord(Piece piece) {
    CompletableFuture<Void> future = new VertxCompletableFuture<>(ctx);
    getCompositeOrderByPoLineId(piece.getPoLineId())
      .thenCompose(order -> protectionHelper.isOperationRestricted(order.getAcqUnitIds(), ProtectedOperationType.UPDATE))
      .thenCompose(v -> inventoryHelper.updateItemWithPoLineId(piece.getItemId(), piece.getPoLineId()))
      .thenAccept(vVoid ->
        getPieceById(piece.getId()).thenAccept(pieceStorage -> {
          ReceivingStatus receivingStatusStorage = pieceStorage.getReceivingStatus();

          handlePutRequest(resourceByIdPath(PIECES, piece.getId()), JsonObject.mapFrom(piece), httpClient, ctx, okapiHeaders, logger)
            .thenAccept(future::complete)
            .thenAccept(afterUpdate -> {

              JsonObject messageToEventBus = new JsonObject();
              messageToEventBus.put("poLineIdUpdate", piece.getPoLineId());

              ReceivingStatus receivingStatusUpdate = piece.getReceivingStatus();
              logger.debug("receivingStatusStorage -- " + receivingStatusStorage);
              logger.debug("receivingStatusUpdate -- " + receivingStatusUpdate);

              if (receivingStatusStorage.compareTo(receivingStatusUpdate) != 0) {
                receiptConsistencyPiecePoLine(messageToEventBus);
              }
            })
            .exceptionally(e -> {
              logger.error("Error updating piece by id to storage {}", piece.getId(), e);
              future.completeExceptionally(e);
              return null;
            });
        })
          .exceptionally(e -> {
            logger.error("Error getting piece by id from storage {}", piece.getId(), e);
            future.completeExceptionally(e);
            return null;
          })
    )
      .exceptionally(t -> {
        logger.error("User with id={} is forbidden to update piece with id={}", t.getCause(), getCurrentUserId(), piece.getId());
        future.completeExceptionally(t);
        return null;
    });
    return future;
  }

  public CompletableFuture<Piece> getPieceById(String pieceId) {
    String endpoint = String.format(URL_WITH_LANG_PARAM, resourceByIdPath(PIECES, pieceId), lang);
    return handleGetRequest(endpoint, httpClient, ctx, okapiHeaders, logger)
      .thenApply(jsonPiece -> jsonPiece.mapTo(Piece.class));
  }

  private void receiptConsistencyPiecePoLine(JsonObject jsonObj) {
    logger.debug("Sending event to verify receipt status");

    sendEvent(MessageAddress.RECEIPT_STATUS, jsonObj);

    logger.debug("Event to verify receipt status - sent");
  }

  public CompletableFuture<Void> deletePiece(String id) {
    return getPieceById(id)
      .thenCompose(piece -> getCompositeOrderByPoLineId(piece.getPoLineId()))
      .thenCompose(purchaseOrder -> protectionHelper.isOperationRestricted(purchaseOrder.getAcqUnitIds(), DELETE))
      .thenCompose(aVoid -> handleDeleteRequest(String.format(DELETE_PIECE_BY_ID, id, lang), httpClient, ctx, okapiHeaders, logger));
  }


  public CompletableFuture<CompositePurchaseOrder> getCompositeOrderByPoLineId(String poLineId) {
    return getPoLineById(poLineId, lang, httpClient, ctx, okapiHeaders, logger)
      .thenApply(json -> json.mapTo(CompositePoLine.class))
      .thenCompose(poLine ->
        getCompositePurchaseOrder(poLine.getPurchaseOrderId())
        .thenApply(purchaseOrder -> purchaseOrder.withCompositePoLines(Collections.singletonList(poLine)))
      );
  }


  public CompletableFuture<CompositePurchaseOrder> getCompositePurchaseOrder(String purchaseOrderId) {
    return getPurchaseOrderById(purchaseOrderId, lang, httpClient, ctx, okapiHeaders, logger)
      .thenApply(HelperUtils::convertToCompositePurchaseOrder)
      .exceptionally(t -> {
        Throwable cause = t.getCause();
        // The case when specified order does not exist
        if (cause instanceof HttpException && ((HttpException) cause).getCode() == Response.Status.NOT_FOUND.getStatusCode()) {
          throw new HttpException(422, ErrorCodes.ORDER_NOT_FOUND);
        }
        throw t instanceof CompletionException ? (CompletionException) t : new CompletionException(cause);
      });
  }

  /**
   * Creates Inventory records associated with given PO line and updates PO line with corresponding links.
   *
   * @param compPOL Composite PO line to update Inventory for
   * @return CompletableFuture with void.
   */
  private CompletableFuture<Piece> updateInventory(CompositePoLine compPOL, Piece piece) {
    if (Boolean.TRUE.equals(compPOL.getIsPackage())) {
      return titlesHelper.getTitle(piece.getTitleId())
        .thenCompose(this::handleInstanceRecord)
        .thenCompose(title -> titlesHelper.updateTitle(title)
          .thenApply(json -> title))
        .thenCompose(title -> handleHoldingsAndItemsRecords(compPOL, title.getInstanceId(), piece.getLocationId()))
        .thenApply(pieces -> piece.withItemId(pieces.get(0).getItemId()));
    }
    return CompletableFuture.completedFuture(piece);
  }

  public CompletableFuture<Title> handleInstanceRecord(Title title) {
    if (title.getInstanceId() != null) {
      return CompletableFuture.completedFuture(title);
    } else {
      return getInstanceRecord(title).thenApply(title::withInstanceId);
    }
  }

  private CompletableFuture<String> getInstanceRecord(Title title) {
    // proceed with new Instance Record creation if no productId is provided
    if (!CollectionUtils.isNotEmpty(title.getProductIds())) {
      return createInstanceRecord(title);
    }

    String query = title.getProductIds().stream()
      .map(productId -> inventoryHelper.buildProductIdQuery(productId))
      .collect(joining(" or "));

    // query contains special characters so must be encoded before submitting
    String endpoint = inventoryHelper.buildLookupEndpoint(INSTANCES, encodeQuery(query, logger), lang);
    return handleGetRequest(endpoint, httpClient, ctx, okapiHeaders, logger)
      .thenCompose(instances -> {
        if (!instances.getJsonArray(INSTANCES).isEmpty()) {
          return completedFuture(extractId(inventoryHelper.getFirstObjectFromResponse(instances, INSTANCES)));
        }
        return createInstanceRecord(title);
      });
  }

  private String extractId(JsonObject json) {
    return json.getString(ID);
  }

  private CompletableFuture<String> createInstanceRecord( Title title) {
    JsonObject lookupObj = new JsonObject();
    CompletableFuture<Void> instanceTypeFuture = inventoryHelper.getEntryId(INSTANCE_TYPES, MISSING_INSTANCE_TYPE)
      .thenAccept(lookupObj::mergeIn);

    CompletableFuture<Void> statusFuture = inventoryHelper.getEntryId(INSTANCE_STATUSES, MISSING_INSTANCE_STATUS)
      .thenAccept(lookupObj::mergeIn);

    CompletableFuture<Void> contributorNameTypeIdFuture = inventoryHelper.verifyContributorNameTypesExist(title.getContributors());

    return allOf(ctx, instanceTypeFuture, statusFuture, contributorNameTypeIdFuture)
      .thenApply(v -> buildInstanceRecordJsonObject(title, lookupObj))
      .thenCompose(instanceRecJson -> createRecordInStorage(instanceRecJson, String.format(CREATE_INSTANCE_ENDPOINT, lang)));
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

    if(isNotEmpty(title.getContributors())) {
      List<JsonObject> contributors = title.getContributors().stream().map(compPolContributor -> {
        JsonObject invContributor = new JsonObject();
        invContributor.put(CONTRIBUTOR_NAME_TYPE_ID, compPolContributor.getContributorNameTypeId());
        invContributor.put(CONTRIBUTOR_NAME, compPolContributor.getContributor());
        return invContributor;
      }).collect(toList());
      instance.put(INSTANCE_CONTRIBUTORS, contributors);
    }

    if (CollectionUtils.isNotEmpty(title.getProductIds())) {
      List<JsonObject> identifiers =
        title
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

  private CompletableFuture<List<org.folio.rest.acq.model.Piece>> handleHoldingsAndItemsRecords(CompositePoLine compPOL
    , String instanceId, String locationId) {

    CompletableFuture<List<org.folio.rest.acq.model.Piece>> pieceFutures = new CompletableFuture<>();
    // Group all locations by location id because the holding should be unique for different locations
    if (HelperUtils.isHoldingsUpdateRequired(compPOL.getEresource(), compPOL.getPhysical())) {
          // Search for or create a new holdings record and then create items for it if required
          inventoryHelper.getOrCreateHoldingsRecord(instanceId, locationId)
            .thenCompose(holdingId -> {
                // Items are not going to be created when create inventory is "Instance, Holding"
                boolean isItemsUpdateRequired = isItemsUpdateRequired(compPOL);
                if (isItemsUpdateRequired) {
                  List<Location> locations = cloneLocations(compPOL, locationId);
                  inventoryHelper.handleItemRecords(compPOL, holdingId, locations)
                                 .thenAccept(pieceFutures::complete);
                } else {
                  pieceFutures.complete(Collections.emptyList());
                }
                return null;
              }
            );
    }
    return pieceFutures;
  }

  @NotNull
  private List<Location> cloneLocations(CompositePoLine compPOL, String locationId) {
    return compPOL.getLocations().stream()
                  .map(location ->  JsonObject.mapFrom(location).mapTo(Location.class))
                  .peek(location -> location.setLocationId(locationId))
                  .collect(toList());
  }
}
