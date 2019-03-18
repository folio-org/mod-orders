package org.folio.rest.impl;

import static java.util.concurrent.CompletableFuture.completedFuture;
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

import io.vertx.core.Context;
import io.vertx.core.json.JsonObject;
import java.util.*;
import java.util.concurrent.CompletableFuture;
import one.util.streamex.EntryStream;
import one.util.streamex.StreamEx;
import org.apache.commons.lang3.StringUtils;
import org.folio.orders.utils.HelperUtils;
import org.folio.rest.acq.model.Piece;
import org.folio.rest.acq.model.PieceCollection;
import org.folio.rest.jaxrs.model.CheckInPiece;
import org.folio.rest.jaxrs.model.Error;
import org.folio.rest.jaxrs.model.PoLine;
import org.folio.rest.jaxrs.model.PoLineCollection;
import org.folio.rest.jaxrs.model.ReceivedItem;
import org.folio.rest.jaxrs.model.PoLine.ReceiptStatus;
import org.folio.rest.acq.model.Piece.ReceivingStatus;
import org.folio.rest.tools.client.interfaces.HttpClientInterface;

public class CheckInRecievePiecesHelper<T> extends AbstractHelper {

  static final int MAX_IDS_FOR_GET_RQ = 15;
  private static final String PIECES_WITH_QUERY_ENDPOINT = resourcesPath(PIECES) + "?limit=%d&lang=%s&query=%s";
  private static final String PIECES_BY_POL_ID_AND_STATUS_QUERY = "poLineId==%s and receivingStatus==%s";
  Map<String, Map<String, T>> piecesFromclass;
  private final InventoryHelper inventoryHelper;
  Map<String, Map<String, Error>> processingErrors;
  private final PurchaseOrderLineHelper poLineHelper;
  private boolean isReceiving = false;
  private boolean isCheckIn = false;

  CheckInRecievePiecesHelper(HttpClientInterface httpClient, Map<String, String> okapiHeaders, Context ctx, String lang,
      Map<String, Map<String, T>> p, Map<String, Map<String, Error>> processingError) {
    super(httpClient, okapiHeaders, ctx, lang);
    piecesFromclass = p;
    inventoryHelper = new InventoryHelper(httpClient, okapiHeaders, ctx, lang);
    processingErrors = processingError;
    poLineHelper = new PurchaseOrderLineHelper(httpClient, okapiHeaders, ctx, lang);
    isReceiving= StreamEx.ofValues(piecesFromclass).allMatch(piece-> piece.values().iterator().next().getClass().isAssignableFrom(ReceivedItem.class));
    isCheckIn= StreamEx.ofValues(piecesFromclass).allMatch(piece-> piece.values().iterator().next().getClass().isAssignableFrom(CheckInPiece.class));
  }

  /**
   * Retrieves piece records from storage based on request data
   * 
   * @return {@link CompletableFuture} which holds map with PO line id as key
   *         and list of corresponding pieces as value
   */
  CompletableFuture<Map<String, List<Piece>>> retrievePieceRecords() {
    Map<String, List<Piece>> piecesByPoLine = new HashMap<>();

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
    return StreamEx.ofValues(piecesFromclass)
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
        logger.error("The issue happened getting piece records", e);
        ids.forEach(pieceId -> addError(getPoLineIdByPieceId(pieceId), pieceId, PIECE_NOT_RETRIEVED.toError()));
        return null;
      });
  }

  /**
   * Validates if the piece corresponds to PO Line specified in the request and
   * can be received. If all checks pass, adds to map
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
    if (piecesFromclass.containsKey(poLineId) && piecesFromclass.get(poLineId).containsKey(pieceId)) {
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
  private boolean isRevertToOnOrder(Piece piece) {
    if (isReceiving) {
      return piece.getReceivingStatus() == ReceivingStatus.RECEIVED
          && inventoryHelper
            .isOnOrderItemStatus((ReceivedItem) piecesFromclass.get(piece.getPoLineId()).get(piece.getId()));
    }else if(isCheckIn) {
      return piece.getReceivingStatus() == ReceivingStatus.RECEIVED
          && inventoryHelper
            .isOnOrderPieceStatus((CheckInPiece) piecesFromclass.get(piece.getPoLineId()).get(piece.getId()));
    }
    return false;

  }

  /**
   * @param pieceId
   *          piece id
   * @return PO Line id corresponding to passed pieceId from request data
   */
  private String getPoLineIdByPieceId(String pieceId) {
    return StreamEx
      .ofKeys(piecesFromclass, values -> values.containsKey(pieceId))
      .findFirst()
      .orElse(EMPTY);
  }

  /**
   * Checks if all expected piece records found in the storage. If any is
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

  /**
   * Updates items in the inventory storage with receiving details if any. On
   * success updates corresponding records as received
   * 
   * @return {@link CompletableFuture} which holds map with PO line id as key
   *         and list of corresponding pieces as value
   */
  CompletableFuture<Map<String, List<Piece>>> updateInventoryItems(Map<String, List<Piece>> piecesGroupedByPoLine) {
    // Collect all piece records with non-empty item ids. The result is a map
    // with item id as a key and piece record as a value
    Map<String, Piece> piecesWithItems = collectPiecesWithItemId(piecesGroupedByPoLine);

    // If there are no pieces with ItemId, continue
    if (piecesWithItems.isEmpty()) {
      return completedFuture(piecesGroupedByPoLine);
    }

    return getItemRecords(piecesWithItems)
      .thenCompose(items -> {
        List<CompletableFuture<Boolean>> futuresForItemUpdates = new ArrayList<>();
        for (JsonObject item : items) {
          String itemId = item.getString(ID);
          Piece piece = piecesWithItems.get(itemId);
          futuresForItemUpdates.add(receiveInventoryItemAndUpdatePiece(item, piece));
        }

        return collectResultsOnSuccess(futuresForItemUpdates)
          .thenApply(results -> {
            if (logger.isDebugEnabled()) {
              long successQty = results.stream()
                .filter(result -> result)
                .count();
              logger.debug("{} out of {} inventory item(s) successfully updated", successQty, results.size());
            }
            return piecesGroupedByPoLine;
          });
      });
  }

  /**
   * Retrieves item records from inventory storage
   * 
   * @param piecesWithItems
   *          map with item id as a key and piece record as a value
   * @return future with list of item records
   */
  private CompletableFuture<List<JsonObject>> getItemRecords(Map<String, Piece> piecesWithItems) {
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
  private Map<String, Piece> collectPiecesWithItemId(Map<String, List<Piece>> piecesGroupedByPoLine) {
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
  private CompletableFuture<Boolean> receiveInventoryItemAndUpdatePiece(JsonObject item, Piece piece) {

    if (isReceiving) {

      ReceivedItem receivedItem = (ReceivedItem) piecesFromclass.get(piece.getPoLineId())
        .get(piece.getId());
      return inventoryHelper
        // Update item records with receiving information and send updates to
        // Inventory
        .receiveItem(item, receivedItem)
        // Update Piece record object with receiving details if item updated
        // successfully
        .thenApply(v -> {
          updatePieceWithReceivingInfo(piece);
          return true;
        })
        // Add processing error if item failed to be updated
        .exceptionally(e -> {
          logger.error("Item associated with piece '{}' cannot be updated", piece.getId());
          addError(piece.getPoLineId(), piece.getId(), ITEM_UPDATE_FAILED.toError());
          return false;
        });
    } else if(isCheckIn){
      CheckInPiece checkinPiece = (CheckInPiece) piecesFromclass.get(piece.getPoLineId())
          .get(piece.getId());
        return inventoryHelper
          // Update item records with checkIn information and send updates to
          // Inventory
          .checkinItem(item, checkinPiece)
          // Update Piece record object with checkIn details if item updated
          // successfully
          .thenApply(v -> {
            updatePieceWithCheckinInfo(piece);
            return true;
          })
          // Add processing error if item failed to be updated
          .exceptionally(e -> {
            logger.error("Item associated with piece '{}' cannot be updated", piece.getId());
            addError(piece.getPoLineId(), piece.getId(), ITEM_UPDATE_FAILED.toError());
            return false;
          });
      
    }
    return null;
  }

  /**
   * Updates piece record with receiving information
   * 
   * @param piece
   *          piece record to be updated with receiving info
   */
  private void updatePieceWithReceivingInfo(Piece piece) {
    // Get ReceivedItem corresponding to piece record
    ReceivedItem receivedItem = (ReceivedItem) piecesFromclass.get(piece.getPoLineId())
      .get(piece.getId());

    if (StringUtils.isNotEmpty(receivedItem.getCaption())) {
      piece.setCaption(receivedItem.getCaption());
    }
    if (StringUtils.isNotEmpty(receivedItem.getComment())) {
      piece.setComment(receivedItem.getComment());
    }
    if (StringUtils.isNotEmpty(receivedItem.getLocationId())) {
      piece.setLocationId(receivedItem.getLocationId());
    }

    // Piece record might be received or rolled-back to Expected
    if (inventoryHelper.isOnOrderItemStatus(receivedItem)) {
      piece.setReceivedDate(null);
      piece.setReceivingStatus(ReceivingStatus.EXPECTED);
    } else {
      piece.setReceivedDate(new Date());
      piece.setReceivingStatus(ReceivingStatus.RECEIVED);
    }
  }
  
  private void updatePieceWithCheckinInfo(Piece piece) {
    // Get ReceivedItem corresponding to piece record
    CheckInPiece checkinPiece = (CheckInPiece) piecesFromclass.get(piece.getPoLineId())
      .get(piece.getId());

    if (StringUtils.isNotEmpty(checkinPiece.getCaption())) {
      piece.setCaption(checkinPiece.getCaption());
    }
    if (StringUtils.isNotEmpty(checkinPiece.getComment())) {
      piece.setComment(checkinPiece.getComment());
    }
    if (StringUtils.isNotEmpty(checkinPiece.getLocationId())) {
      piece.setLocationId(checkinPiece.getLocationId());
    }

    // Piece record might be received or rolled-back to Expected
    if (inventoryHelper.isOnOrderPieceStatus(checkinPiece)) {
      piece.setReceivedDate(null);
      piece.setReceivingStatus(ReceivingStatus.EXPECTED);
    } else {
      piece.setReceivedDate(new Date());
      piece.setReceivingStatus(ReceivingStatus.RECEIVED);
    }
  }

  /**
   * Updates piece records with receiving details which do not have associated
   * item
   * 
   * @param piecesGroupedByPoLine
   *          map with PO line id as key and list of corresponding pieces as
   *          value
   * @return updated map passed as a parameter
   */
  Map<String, List<Piece>> updatePieceRecordsWithoutItems(Map<String, List<Piece>> piecesGroupedByPoLine) {
    // Collect all piece records without item id and update with receiving
    // information.
    if(isReceiving) {
      StreamEx.ofValues(piecesGroupedByPoLine)
      .flatMap(List::stream)
      .filter(piece -> StringUtils.isEmpty(piece.getItemId()))
      .forEach(this::updatePieceWithReceivingInfo);
    } else if(isCheckIn) {
      StreamEx.ofValues(piecesGroupedByPoLine)
      .flatMap(List::stream)
      .filter(piece -> StringUtils.isEmpty(piece.getItemId()))
      .forEach(this::updatePieceWithCheckinInfo);
    }
   

    return piecesGroupedByPoLine;
  }

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
      .filter(piece -> getError(piece.getPoLineId(), piece.getId()) == null)
      .map(this::storeUpdatedPieceRecord)
      .toArray(new CompletableFuture[0]);

    return allOf(ctx, futures)
      .thenApply(v -> piecesGroupedByPoLine);
  }

  /**
   * Sends request to update piece record with receiving details in the storage.
   * In case error happens updating the piece, this is collected to return in
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
   * Stores updated piece records with receiving details into storage.
   * 
   * @param piecesGroupedByPoLine
   *          map with PO line id as key and list of corresponding pieces as
   *          value
   * @return map passed as a parameter
   */
  CompletableFuture<Map<String, List<Piece>>> updatePoLinesStatus(Map<String, List<Piece>> piecesGroupedByPoLine) {
    // Get all PO Line id's which potentially require receipt status update
    List<String> poLineIdsForUpdatedPieces = getPoLineIdsForUpdatedPieces(piecesGroupedByPoLine);

    // Get all PO Lines from storage which potentially require receipt status
    // update
    List<CompletableFuture<List<PoLine>>> polFutures = StreamEx
      .ofSubLists(poLineIdsForUpdatedPieces, MAX_IDS_FOR_GET_RQ)
      // Transform piece id's to CQL query
      .map(HelperUtils::convertIdsToCqlQuery)
      // Send get request for each CQL query
      .map(this::getPoLinesByQuery)
      .toList();

    // Once all PO Lines are retrieved from storage check if receipt status
    // requires update and persist in storage
    return collectResultsOnSuccess(polFutures)
      .thenApply(lists -> StreamEx.of(lists).toFlatList(poLines -> poLines))
      .thenCompose(poLines -> {
        // Calculate expected status for each PO Line and update with new one if
        // required
        List<CompletableFuture<Void>> futures = new ArrayList<>();
        for (PoLine poLine : poLines) {
          futures.add(calculatePoLineReceiptStatus(poLine,
              getSuccessfullyProcessedPieces(poLine.getId(), piecesGroupedByPoLine))
                .thenCompose(status -> updatePoLineReceiptStatus(poLine, status)));
        }
        return collectResultsOnSuccess(futures)
          .thenAccept(result -> logger.debug("{} out of {} PO Line(s) updated with new status", result.size(),
              poLineIdsForUpdatedPieces.size()));
      })
      .thenApply(v -> piecesGroupedByPoLine);
  }

  private List<String> getPoLineIdsForUpdatedPieces(Map<String, List<Piece>> piecesGroupedByPoLine) {
    return EntryStream
      .of(piecesGroupedByPoLine)
      .filter(entry -> entry.getValue()
        .stream()
        // Check if there is at least one piece record which processed
        // successfully
        .anyMatch(piece -> getError(entry.getKey(), piece.getId()) == null))
      .keys()
      .toList();
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
      .filter(piece -> getError(poLineId, piece.getId()) == null)
      .toList();
  }

  /**
   * @param poLine
   *          PO Line record from storage
   * @param pieces
   *          list of pieces
   * @return
   */
  private CompletableFuture<ReceiptStatus> calculatePoLineReceiptStatus(PoLine poLine, List<Piece> pieces) {
    // Search for pieces with Expected status
    return getPiecesQuantityByPoLineAndStatus(poLine.getId(), ReceivingStatus.EXPECTED)
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
    // Fully Received: In case there is no any expected piece remaining
    if (isReceiving && expectedPiecesQuantity == 0) {
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

  private CompletableFuture<Void> updatePoLineReceiptStatus(PoLine poLine, ReceiptStatus status) {
    if (status == null || poLine.getReceiptStatus() == status) {
      return completedFuture(null);
    }

    // Update receipt status and date (if fully received)
    poLine.setReceiptStatus(status);
    if (status == FULLY_RECEIVED) {
      poLine.setReceiptDate(new Date());
    } else if (isCheckIn && status == ReceiptStatus.PARTIALLY_RECEIVED) {
      // set the date when the first piece is checked In
      poLine.setReceiptDate(new Date());
    } else {
      poLine.setReceiptDate(null);
    }

    // Update PO Line in storage
    return handlePutRequest(resourceByIdPath(PO_LINES, poLine.getId()), JsonObject.mapFrom(poLine), httpClient, ctx,
        okapiHeaders, logger)
          .exceptionally(e -> {
            logger.error("The PO Line '{}' cannot be updated with new receipt status", e, poLine.getId());
            return null;
          });
  }

  private Error getError(String polId, String pieceId) {
    return processingErrors.computeIfAbsent(polId, k -> Collections.emptyMap())
      .get(pieceId);
  }

  private void addError(String polId, String pieceId, Error error) {
    processingErrors.computeIfAbsent(polId, k -> new HashMap<>())
      .put(pieceId, error);
  }

}
