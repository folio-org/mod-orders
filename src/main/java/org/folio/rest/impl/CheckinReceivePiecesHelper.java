package org.folio.rest.impl;

import static me.escoffier.vertx.completablefuture.VertxCompletableFuture.completedFuture;
import static me.escoffier.vertx.completablefuture.VertxCompletableFuture.allOf;
import static org.apache.commons.lang3.StringUtils.EMPTY;
import static org.folio.orders.utils.ErrorCodes.*;
import static org.folio.orders.utils.HelperUtils.collectResultsOnSuccess;
import static org.folio.orders.utils.HelperUtils.encodeQuery;
import static org.folio.orders.utils.HelperUtils.handleGetRequest;
import static org.folio.orders.utils.HelperUtils.handlePutRequest;
import static org.folio.orders.utils.ResourcePathResolver.PIECES;
import static org.folio.orders.utils.ResourcePathResolver.PO_LINES;
import static org.folio.orders.utils.ResourcePathResolver.resourceByIdPath;
import static org.folio.orders.utils.ResourcePathResolver.resourcesPath;
import static org.folio.rest.jaxrs.model.PoLine.ReceiptStatus.FULLY_RECEIVED;
import static org.folio.rest.jaxrs.model.PoLine.ReceiptStatus.PARTIALLY_RECEIVED;
import static org.folio.rest.jaxrs.model.PoLine.ReceiptStatus.AWAITING_RECEIPT;
import static org.apache.commons.lang3.ObjectUtils.defaultIfNull;

import io.vertx.core.Context;
import io.vertx.core.json.JsonArray;
import io.vertx.core.json.JsonObject;
import java.util.*;
import java.util.concurrent.CompletableFuture;
import java.util.stream.Collectors;

import one.util.streamex.EntryStream;
import one.util.streamex.StreamEx;
import org.apache.commons.lang3.StringUtils;
import org.folio.orders.events.handlers.MessageAddress;
import org.folio.orders.utils.HelperUtils;
import org.folio.rest.acq.model.Piece;
import org.folio.rest.acq.model.Piece.ReceivingStatus;
import org.folio.rest.acq.model.PieceCollection;
import org.folio.rest.jaxrs.model.Error;
import org.folio.rest.jaxrs.model.*;
import org.folio.rest.jaxrs.model.PoLine.ReceiptStatus;
import org.folio.rest.tools.client.interfaces.HttpClientInterface;

import java.util.*;
import java.util.concurrent.CompletableFuture;

import static java.util.concurrent.CompletableFuture.completedFuture;
import static me.escoffier.vertx.completablefuture.VertxCompletableFuture.allOf;
import static org.apache.commons.lang3.ObjectUtils.defaultIfNull;
import static org.apache.commons.lang3.StringUtils.EMPTY;
import static org.folio.orders.utils.ErrorCodes.*;
import static org.folio.orders.utils.HelperUtils.*;
import static org.folio.orders.utils.ResourcePathResolver.*;
import static org.folio.rest.jaxrs.model.PoLine.ReceiptStatus.*;

public abstract class CheckinReceivePiecesHelper<T> extends AbstractHelper {

  static final int MAX_IDS_FOR_GET_RQ = 15;
  private static final String PIECES_WITH_QUERY_ENDPOINT = resourcesPath(PIECES) + "?limit=%d&lang=%s&query=%s";
  private static final String PIECES_BY_POL_ID_AND_STATUS_QUERY = "poLineId==%s and receivingStatus==%s";
  public static final String HOLDINGS_GET_ENDPOINT = "/holdings-storage/holdings/%s?lang=%s";
  Map<String, Map<String, T>> piecesByLineId;
  final InventoryHelper inventoryHelper;
  Map<String, Map<String, Error>> processingErrors;
  private final PurchaseOrderLineHelper poLineHelper;
  private List<PoLine> poLineList;

  CheckinReceivePiecesHelper(HttpClientInterface httpClient, Map<String, String> okapiHeaders, Context ctx,
      String lang) {
    super(httpClient, okapiHeaders, ctx, lang);
    processingErrors = new HashMap<>();
    inventoryHelper = new InventoryHelper(httpClient, okapiHeaders, ctx, lang);
    poLineHelper = new PurchaseOrderLineHelper(httpClient, okapiHeaders, ctx, lang);
  }

  /**
   * Retrieves piece records from storage based on request data
   *
   * @return {@link CompletableFuture} which holds map with PO line id as key
   *         and list of corresponding pieces as value
   */
  CompletableFuture<Map<String, List<Piece>>> retrievePieceRecords(Map<String, Map<String, T>> piecesByLineId) {
    Map<String, List<Piece>> piecesByPoLine = new HashMap<>();
    this.piecesByLineId = piecesByLineId;
    // Split all piece id's by maximum number of id's for get query
    CompletableFuture[] futures = StreamEx
      .ofSubLists(getPieceIds(), MAX_IDS_FOR_GET_RQ)
      // Send get request for each CQL query
      .map(ids -> getPiecesByIds(ids, piecesByPoLine))
      .toArray(CompletableFuture.class);

    // Wait for all pieces to be retrieved and complete resulting future
    return allOf(ctx, futures)
      .thenApply(v -> {
        if (logger.isDebugEnabled()) {
          int poLinesQty = piecesByPoLine.size();
          int piecesQty = StreamEx.ofValues(piecesByPoLine)
            .mapToInt(List::size)
            .sum();
          logger.debug("{} piece record(s) retrieved from storage for {} PO line(s)", piecesQty, poLinesQty);
        }
        return piecesByPoLine;
      });
  }

  /**
   * Gets all piece id's based on request data
   *
   * @return extract all piece id's
   */
  private List<String> getPieceIds() {
    return StreamEx.ofValues(piecesByLineId)
      .map(Map::keySet)
      .toFlatList(ids -> ids);
  }

  private CompletableFuture<Void> getPiecesByIds(List<String> ids, Map<String, List<Piece>> piecesByPoLine) {
    // Transform piece id's to CQL query
    String query = HelperUtils.convertIdsToCqlQuery(ids);
    String endpoint = String.format(PIECES_WITH_QUERY_ENDPOINT, ids.size(), lang, encodeQuery(query, logger));
    return handleGetRequest(endpoint, httpClient, ctx, okapiHeaders, logger)
      .thenAccept(pieceJson -> {
        List<Piece> pieces = pieceJson.mapTo(PieceCollection.class).getPieces();
        pieces.forEach(piece -> addPieceIfValid(piece, piecesByPoLine));
        checkIfAllPiecesFound(ids, pieces);
      })
      .exceptionally(e -> {
        logger.error("Error fetching piece records", e);
        ids.forEach(pieceId -> addError(getPoLineIdByPieceId(pieceId), pieceId, PIECE_NOT_RETRIEVED.toError()));
        return null;
      });
  }

  /**
   * Validates if the piece corresponds to PO Line specified in the request and
   * can be received/checked-in. If all checks pass, adds to map
   *
   * @param piece
   *          {@link Piece} piece to validate and add to map
   * @param piecesByPoLine
   *          map with PO line id as a key and list of corresponding pieces as a
   *          value
   */
  private void addPieceIfValid(Piece piece, Map<String, List<Piece>> piecesByPoLine) {
    String poLineId = piece.getPoLineId();
    String pieceId = piece.getId();

    // Validate if the piece actually corresponds to PO line specified in the
    // request
    if (piecesByLineId.containsKey(poLineId) && piecesByLineId.get(poLineId).containsKey(pieceId)) {
      // Validate if the piece is not yet received
      if (piece.getReceivingStatus() == ReceivingStatus.EXPECTED || isRevertToOnOrder(piece)) {
        piecesByPoLine.computeIfAbsent(poLineId, v -> new ArrayList<>())
          .add(piece);
      } else {
        addError(poLineId, pieceId, PIECE_ALREADY_RECEIVED.toError());
      }
    } else {
      addError(getPoLineIdByPieceId(pieceId), pieceId, PIECE_POL_MISMATCH.toError());
    }
  }

  /**
   * Verifies if the current status of the piece record is "Received" and the
   * client would like to roll-back to Expected
   *
   * @param piece
   *          piece record to asses
   * @return {@code true} if piece record is already received and has to be
   *         rolled-back to Expected
   */
  abstract boolean isRevertToOnOrder(Piece piece);

  /**
   * @param pieceId
   *          piece id
   * @return PO Line id corresponding to passed pieceId from request data
   */
  private String getPoLineIdByPieceId(String pieceId) {
    return StreamEx
      .ofKeys(piecesByLineId, values -> values.containsKey(pieceId))
      .findFirst()
      .orElse(EMPTY);
  }

  /**
   * Checks if all expected piece records found in the storage. If any are
   * missing, adds corresponding error
   *
   * @param pieceIds
   *          list of expected piece id's
   * @param pieces
   *          found pieces
   */
  private void checkIfAllPiecesFound(List<String> pieceIds, List<Piece> pieces) {
    // Handle the case when for some reason some pieces are not found
    if (pieces.size() < pieceIds.size()) {
      List<String> foundPieces = StreamEx.of(pieces).map(Piece::getId).toList();

      pieceIds.stream()
        .filter(pieceId -> !foundPieces.contains(pieceId))
        .forEach(pieceId -> addError(getPoLineIdByPieceId(pieceId), pieceId, PIECE_NOT_FOUND.toError()));
    }
  }

  PoLine searchPoLineById(List<PoLine> poLines, String polineId) {
    for (PoLine  poline: poLines){
      if (poline.getId().equals(polineId)){
        return poline;
      }
    } return null;
  }

  CompletableFuture<Boolean> updateHoldingsAndItems(JsonObject item, Piece piece, PoLine poLine, String locationId) {
    if (holdingUpdateOnCheckinReceiveRequired(piece, locationId, poLine)) {
      return inventoryHelper.getOrCreateHoldingsRecord(poLine.getInstanceId(), locationId)
        .thenCompose(holdingId -> {
          String endpoint = String.format(HOLDINGS_GET_ENDPOINT, holdingId, lang);
          return handleGetRequest(endpoint, httpClient, ctx, okapiHeaders, logger);
        })
        .thenCompose(holding -> {
          item.put("holdingsRecordId", holding.getString(ID));
          return receiveInventoryItemAndUpdatePiece(item, piece);
        })
        .exceptionally(t -> false);
    } else {
      return receiveInventoryItemAndUpdatePiece(item, piece);
    }
  }

  boolean holdingUpdateOnCheckinReceiveRequired(Piece piece, String locationId, PoLine poLine) {
    return isHoldingsUpdateRequired(poLine.getEresource(), poLine.getPhysical())
      && StringUtils.isNotEmpty(locationId)
      && !locationId.equals(piece.getLocationId());
  }

  /**
   * Retrieves item records from inventory storage
   *
   * @param piecesWithItems
   *          map with item id as a key and piece record as a value
   * @return future with list of item records
   */
  CompletableFuture<List<JsonObject>> getItemRecords(Map<String, Piece> piecesWithItems) {
    // Split all id's by maximum number of id's for get query
    List<CompletableFuture<List<JsonObject>>> futures = StreamEx
      .ofSubLists(new ArrayList<>(piecesWithItems.keySet()), MAX_IDS_FOR_GET_RQ)
      // Get item records from Inventory storage
      .map(ids -> getItemRecordsByIds(ids, piecesWithItems))
      .toList();

    return collectResultsOnSuccess(futures)
      .thenApply(lists -> StreamEx.of(lists).toFlatList(jsonObjects -> jsonObjects));
  }

  /**
   * Returns list of item records for specified id's.
   *
   * @param ids
   *          List of item id's
   * @param piecesWithItems
   *          map with item id as a key and piece record as a value
   * @return future with list of item records
   */
  private CompletableFuture<List<JsonObject>> getItemRecordsByIds(List<String> ids,
      Map<String, Piece> piecesWithItems) {
    return inventoryHelper.getItemRecordsByIds(ids)
      .thenApply(items -> {
        checkIfAllItemsFound(ids, items, piecesWithItems);
        return items;
      })
      .exceptionally(e -> {
        logger.error("The issue happened getting item records");
        ids.forEach(id -> {
          Piece piece = piecesWithItems.get(id);
          addError(piece.getPoLineId(), piece.getId(), ITEM_NOT_RETRIEVED.toError());
        });
        return Collections.emptyList();
      });
  }

  /**
   * Checks if all expected piece records found in the storage. If any is
   * missing, adds corresponding error
   *
   * @param expectedItemIds
   *          list of expected item id's
   * @param piecesWithItems
   *          map with item id as a key and piece record as a value
   * @param items
   *          found item records
   */
  private void checkIfAllItemsFound(List<String> expectedItemIds, List<JsonObject> items,
      Map<String, Piece> piecesWithItems) {
    // Handle the case when for some reason some items are not found
    if (items.size() < expectedItemIds.size()) {
      List<String> foundItemIds = StreamEx.of(items).map(inventoryHelper::extractId).toList();

      expectedItemIds.stream()
        .filter(id -> !foundItemIds.contains(id))
        .forEach(itemId -> {
          Piece piece = piecesWithItems.get(itemId);
          addError(piece.getPoLineId(), piece.getId(), ITEM_NOT_FOUND.toError());
        });
    }
  }

  /**
   * Collect all piece records with non-empty item ids. The result is a map with
   * item id as a key and piece record as a value
   *
   * @param piecesGroupedByPoLine
   *          map with PO line id as key and list of corresponding pieces as
   *          value
   * @return map with item id as key and piece record as a value
   */
  Map<String, Piece> collectPiecesWithItemId(Map<String, List<Piece>> piecesGroupedByPoLine) {
    return StreamEx
      .ofValues(piecesGroupedByPoLine)
      .flatMap(List::stream)
      .filter(piece -> StringUtils.isNotEmpty(piece.getItemId()))
      .toMap(Piece::getItemId, piece -> piece);
  }

  /**
   * @param item
   *          inventory item
   * @param piece
   *          piece associated with the item
   * @return future indicating if the item update is successful.
   */
  abstract CompletableFuture<Boolean> receiveInventoryItemAndUpdatePiece(JsonObject item, Piece piece);

  /**
   * Updates piece records with receiving details which do not have associated
   * item
   *
   * @param piecesGroupedByPoLine
   *          map with PO line id as key and list of corresponding pieces as
   *          value
   * @return updated map passed as a parameter
   */
  abstract Map<String, List<Piece>> updatePieceRecordsWithoutItems(Map<String, List<Piece>> piecesGroupedByPoLine);

  /**
   * Stores updated piece records with receiving details into storage.
   *
   * @param piecesGroupedByPoLine
   *          map with PO line id as key and list of corresponding pieces as
   *          value
   * @return map passed as a parameter
   */
  CompletableFuture<Map<String, List<Piece>>> storeUpdatedPieceRecords(Map<String, List<Piece>> piecesGroupedByPoLine) {
    // Collect all piece records which marked as ready to be received and update
    // storage
    CompletableFuture[] futures = StreamEx
      .ofValues(piecesGroupedByPoLine)
      .flatMap(List::stream)
      .filter(this::isSuccessfullyProcessedPiece)
      .map(this::storeUpdatedPieceRecord)
      .toArray(new CompletableFuture[0]);

    return allOf(ctx, futures)
      .thenApply(v -> piecesGroupedByPoLine);
  }

  /**
   * Sends request to update piece record with receiving/check-in details in the storage.
   * In case of an error updating the piece, this is collected to return in
   * the response to client
   *
   * @param piece
   *          {@link Piece} with receiving information
   */
  private CompletableFuture<Void> storeUpdatedPieceRecord(Piece piece) {
    String pieceId = piece.getId();
    return handlePutRequest(resourceByIdPath(PIECES, pieceId), JsonObject.mapFrom(piece), httpClient, ctx, okapiHeaders,
        logger)
          .exceptionally(e -> {
            addError(getPoLineIdByPieceId(pieceId), pieceId, PIECE_UPDATE_FAILED.toError());
            return null;
          });
  }

  /**
   * Stores updated piece records with receiving/check-in details into storage.
   *
   * @param piecesGroupedByPoLine
   *          map with PO line id as key and list of corresponding pieces as
   *          value
   * @return map passed as a parameter
   */
  CompletableFuture<Map<String, List<Piece>>> updatePoLinesStatus(Map<String, List<Piece>> piecesGroupedByPoLine) {
    List<String> poLineIdsForUpdatedPieces = getPoLineIdsForUpdatedPieces(piecesGroupedByPoLine);
    // Once all PO Lines are retrieved from storage check if receipt status
    // requires update and persist in storage
    return getPoLines(poLineIdsForUpdatedPieces)
      .thenCompose(poLines -> {
        // Calculate expected status for each PO Line and update with new one if
        // required
        List<CompletableFuture<String>> futures = new ArrayList<>();
        for (PoLine poLine : poLines) {
          List<Piece> successfullyProcessedPieces = getSuccessfullyProcessedPieces(poLine.getId(), piecesGroupedByPoLine);
          futures.add(calculatePoLineReceiptStatus(poLine, successfullyProcessedPieces)
                .thenCompose(status -> updatePoLineReceiptStatus(poLine, status)));
        }

        return collectResultsOnSuccess(futures)
          .thenAccept(updatedPoLines -> {
            logger.debug("{} out of {} PO Line(s) updated with new status", updatedPoLines.size(), piecesGroupedByPoLine.size());

            // Send event to check order status for successfully processed PO Lines
            updateOrderStatus(StreamEx
              .of(poLines)
              // Leave only successfully updated PO Lines
              .filter(line -> updatedPoLines.contains(line.getId()))
              .toList());
          });
      })
      .thenApply(ok -> piecesGroupedByPoLine);
  }

  private void updateOrderStatus(List<PoLine> poLines) {
    if (!poLines.isEmpty()) {
      logger.debug("Sending event to verify order status");

      // Collect order ids which should be processed
      List<String> poIds = StreamEx
        .of(poLines)
        .map(PoLine::getPurchaseOrderId)
        .distinct()
        .toList();

      sendEvent(MessageAddress.ORDER_STATUS, new JsonObject().put(ORDER_IDS, new JsonArray(poIds)));

      logger.debug("Event to verify order status - sent");
    }
  }

  private List<String> getPoLineIdsForUpdatedPieces(Map<String, List<Piece>> piecesGroupedByPoLine) {
    return EntryStream
      .of(piecesGroupedByPoLine)
      .filter(entry -> entry.getValue()
        .stream()
        // Check if there is at least one piece record which processed
        // successfully
        .anyMatch(this::isSuccessfullyProcessedPiece))
      .keys()
      .toList();
  }

  private CompletableFuture<List<PoLine>> getPoLines(List<String> poLineIds) {
    if(poLineList == null) {
      return collectResultsOnSuccess(StreamEx
          .ofSubLists(poLineIds, MAX_IDS_FOR_GET_RQ)
          // Transform piece id's to CQL query
          .map(HelperUtils::convertIdsToCqlQuery)
          // Send get request for each CQL query
          .map(this::getPoLinesByQuery)
          .toList())
        .thenApply(lists -> StreamEx.of(lists).toFlatList(poLines -> poLines))
        .thenApply(list -> {
          poLineList = list;
          return list;
        });
    } else {
      return completedFuture(poLineList
        .stream()
        .filter(poLine -> poLineIds.contains(poLine.getId()))
        .collect(Collectors.toList()));
    }
  }

  private CompletableFuture<List<PoLine>> getPoLinesByQuery(String query) {
    return poLineHelper.getPoLines(MAX_IDS_FOR_GET_RQ, 0, query)
      .thenApply(PoLineCollection::getPoLines)
      .exceptionally(e -> {
        logger.error("The issue happened getting PO Lines", e);
        return null;
      });
  }

  private List<Piece> getSuccessfullyProcessedPieces(String poLineId, Map<String, List<Piece>> piecesGroupedByPoLine) {
    return StreamEx.of(piecesGroupedByPoLine.get(poLineId))
      .filter(this::isSuccessfullyProcessedPiece)
      .toList();
  }

  /**
   * @param poLine
   *          PO Line record from storage
   * @param pieces
   *          list of pieces
   * @return resulting PO Line status
   */
  private CompletableFuture<ReceiptStatus> calculatePoLineReceiptStatus(PoLine poLine, List<Piece> pieces) {
    // Search for pieces with Expected status
    return pieces.isEmpty()
      // No successfully pieces processed - receipt status unchanged
      ? completedFuture(poLine.getReceiptStatus())
      : getPiecesQuantityByPoLineAndStatus(poLine.getId(), ReceivingStatus.EXPECTED)
        // Calculate receipt status
        .thenCompose(expectedQty -> calculatePoLineReceiptStatus(expectedQty, poLine, pieces))
        .exceptionally(e -> {
          logger.error("The expected receipt status for PO Line '{}' cannot be calculated", e, poLine.getId());
          return null;
      });
  }

  /**
   * Returns "Fully Received" status if quantity of expected piece records is
   * zero, otherwise checks how many received pieces. If quantity of received
   * piece records is zero, returns "Awaiting Receipt" status, otherwise -
   * "Partially Received"
   *
   * @param expectedPiecesQuantity
   *          expected piece records quantity
   * @param poLine
   *          PO Line record representation from storage
   * @return completable future holding calculated PO Line's receipt status
   */
  private CompletableFuture<ReceiptStatus> calculatePoLineReceiptStatus(int expectedPiecesQuantity, PoLine poLine,
      List<Piece> pieces) {
    // Fully Received:If receiving and there is no expected piece remaining
    if (!isCheckin(poLine) && expectedPiecesQuantity == 0) {
      return CompletableFuture.completedFuture(FULLY_RECEIVED);
    }
    // Partially Received: In case there is at least one successfully received
    // piece
    if (StreamEx.of(pieces).anyMatch(piece -> ReceivingStatus.RECEIVED == piece.getReceivingStatus())) {
      return CompletableFuture.completedFuture(PARTIALLY_RECEIVED);
    }
    // Pieces were rolled-back to Expected. In this case we have to check if
    // there is any Received piece in the storage
    return getPiecesQuantityByPoLineAndStatus(poLine.getId(), ReceivingStatus.RECEIVED)
      .thenApply(receivedQty -> receivedQty == 0 ? AWAITING_RECEIPT : PARTIALLY_RECEIVED);
  }

  boolean isCheckin(PoLine poLine) {
    return defaultIfNull(poLine.getCheckinItems(), false);
  }

  private CompletableFuture<Integer> getPiecesQuantityByPoLineAndStatus(String poLineId,
      ReceivingStatus receivingStatus) {
    String query = String.format(PIECES_BY_POL_ID_AND_STATUS_QUERY, poLineId, receivingStatus.value());
    // Limit to 0 because only total number is important
    String endpoint = String.format(PIECES_WITH_QUERY_ENDPOINT, 0, lang, encodeQuery(query, logger));
    // Search for pieces with Expected status
    return handleGetRequest(endpoint, httpClient, ctx, okapiHeaders, logger)
      // Return total records quantity
      .thenApply(json -> json.mapTo(PieceCollection.class).getTotalRecords());
  }

  private CompletableFuture<String> updatePoLineReceiptStatus(PoLine poLine, ReceiptStatus status) {
    if (status == null || poLine.getReceiptStatus() == status) {
      return completedFuture(null);
    }

    // Update receipt date and receipt status
    if (status == FULLY_RECEIVED) {
      poLine.setReceiptDate(new Date());
    } else if (isCheckin(poLine) && poLine.getReceiptStatus().equals(ReceiptStatus.AWAITING_RECEIPT)
        && status == ReceiptStatus.PARTIALLY_RECEIVED) {
      // if checking in, set the receipt date only for the first piece
      poLine.setReceiptDate(new Date());
    } else {
      poLine.setReceiptDate(null);
    }

    poLine.setReceiptStatus(status);

    // Update PO Line in storage
    return handlePutRequest(resourceByIdPath(PO_LINES, poLine.getId()), JsonObject.mapFrom(poLine), httpClient, ctx,
        okapiHeaders, logger)
      .thenApply(v -> poLine.getId())
      .exceptionally(e -> {
        logger.error("The PO Line '{}' cannot be updated with new receipt status", e, poLine.getId());
        return null;
      });
  }

  /**
   * Checks if there is processing error for particular piece record
   * @param piece piece record to get error for
   * @return {@code true} if there is an error associated with piece record
   */
  private boolean isSuccessfullyProcessedPiece(Piece piece) {
    return getError(piece.getPoLineId(), piece.getId()) == null;
  }

  /**
   * Gets processing error for particular piece record
   * @param polId PO Line id
   * @param pieceId piece id
   * @return error object if presents or null
   */
  private Error getError(String polId, String pieceId) {
    return Optional.ofNullable(processingErrors.get(polId))
     .map(errors -> errors.get(pieceId))
     .orElse(null);
  }

  void addError(String polId, String pieceId, Error error) {
    processingErrors.computeIfAbsent(polId, k -> new HashMap<>())
      .put(pieceId, error);
  }

  public void calculateProcessingErrors(String poLineId, ReceivingResult result,
      Map<String, Piece> processedPiecesForPoLine, Map<String, Integer> resultCounts, String pieceId) {
    // Calculate processing status
    ProcessingStatus status = new ProcessingStatus();
    Error error = getError(poLineId, pieceId);
    if (processedPiecesForPoLine.get(pieceId) != null && error == null) {
      status.setType(ProcessingStatus.Type.SUCCESS);
      resultCounts.merge(ProcessingStatus.Type.SUCCESS.toString(), 1, Integer::sum);
    } else {
      status.setType(ProcessingStatus.Type.FAILURE);
      status.setError(error);
      resultCounts.merge(ProcessingStatus.Type.FAILURE.toString(), 1, Integer::sum);
    }

    ReceivingItemResult itemResult = new ReceivingItemResult();
    itemResult.setPieceId(pieceId);
    itemResult.setProcessingStatus(status);
    result.getReceivingItemResults().add(itemResult);
  }

  /**
   * Filter by locationId presence for items/pieces related to POLine.
   *
   * @return {@link CompletableFuture} which holds map with PO line id as key
   *         and list of corresponding pieces as value
   */
  CompletableFuture<Map<String, List<Piece>>> filterMissingLocations(Map<String, List<Piece>> piecesRecords) {
    return getPoLines(StreamEx.ofKeys(piecesRecords).toList())
      .thenApply(poLines -> {
        for(PoLine poLine : poLines) {
          piecesRecords.get(poLine.getId()).removeIf(piece -> isMissingLocation(poLine, piece));
        }
        return piecesRecords;
      });
  }

  private boolean isMissingLocation(PoLine poLine, Piece piece) {
    // Check if locationId doesn't presented in piece from request and retrieved from storage
    // Corresponding piece from collection
    if (getLocationId(piece) == null && !isRevertToOnOrder(piece)) {
      if (piece.getFormat() == Piece.Format.ELECTRONIC) {
        // Check E-Resource
        if (poLine.getEresource() != null && poLine.getEresource().getCreateInventory() != Eresource.CreateInventory.NONE
          && poLine.getEresource().getCreateInventory() != Eresource.CreateInventory.INSTANCE) {
          addError(poLine.getId(), piece.getId(), LOC_NOT_PROVIDED.toError());
          return true;
        }
      } else {
        // Check Physical
        if (poLine.getPhysical() != null && poLine.getPhysical().getCreateInventory() != Physical.CreateInventory.NONE
          && poLine.getPhysical().getCreateInventory() != Physical.CreateInventory.INSTANCE) {
          addError(poLine.getId(), piece.getId(), LOC_NOT_PROVIDED.toError());
          return true;
        }
      }
    }
    return false;
  }

  abstract String getLocationId(Piece piece);

}
