package org.folio.rest.impl;

import io.vertx.core.Context;
import io.vertx.core.json.JsonObject;
import me.escoffier.vertx.completablefuture.VertxCompletableFuture;
import one.util.streamex.StreamEx;
import org.apache.commons.lang3.StringUtils;
import org.folio.rest.acq.model.Piece;
import org.folio.rest.acq.model.Piece.ReceivingStatus;
import org.folio.rest.jaxrs.model.*;

import java.util.*;
import java.util.concurrent.CompletableFuture;

import static java.util.stream.Collectors.*;
import static org.folio.orders.utils.ErrorCodes.ITEM_UPDATE_FAILED;
import static org.folio.orders.utils.HelperUtils.buildQuery;
import static org.folio.orders.utils.HelperUtils.handleGetRequest;
import static org.folio.orders.utils.ResourcePathResolver.RECEIVING_HISTORY;
import static org.folio.orders.utils.ResourcePathResolver.resourcesPath;

public class ReceivingHelper extends CheckinReceivePiecesHelper<ReceivedItem> {

  private static final String GET_RECEIVING_HISTORY_BY_QUERY = resourcesPath(RECEIVING_HISTORY) + SEARCH_PARAMS;

  /**
   * Map with PO line id as a key and value is map with piece id as a key and {@link ReceivedItem} as a value
   */
  private final Map<String, Map<String, ReceivedItem>> receivingItems;


  ReceivingHelper(ReceivingCollection receivingCollection, Map<String, String> okapiHeaders, Context ctx, String lang) {
    super(getHttpClient(okapiHeaders), okapiHeaders, ctx, lang);

    // Convert request to map representation
    receivingItems = groupReceivedItemsByPoLineId(receivingCollection);

    // Logging quantity of the piece records to be received
    if (logger.isDebugEnabled()) {
      int poLinesQty = receivingItems.size();
      int piecesQty = StreamEx.ofValues(receivingItems)
                               .mapToInt(Map::size)
                               .sum();
      logger.debug("{} piece record(s) are going to be received for {} PO line(s)", piecesQty, poLinesQty);
    }
  }

  ReceivingHelper(Map<String, String> okapiHeaders, Context ctx, String lang) {
    super(getHttpClient(okapiHeaders), okapiHeaders, ctx, lang);
    receivingItems = null;
  }

  CompletableFuture<ReceivingResults> receiveItems(ReceivingCollection receivingCollection) {
    Map<String, Map<String, String>> pieceLocationsGroupedByPoLine = groupLocationsByPoLineIdOnReceiving(receivingCollection);
    // 1. Get piece records from storage
    return this.retrievePieceRecords(receivingItems)
      // 2. Filter locationId
      .thenCompose(this::filterMissingLocations)
      // 3. Update items in the Inventory if required
      .thenCompose(pieces -> updateInventoryItems(pieceLocationsGroupedByPoLine, pieces))
      // 4. Update piece records with receiving details which do not have associated item
      .thenApply(this::updatePieceRecordsWithoutItems)
      // 5. Update received piece records in the storage
      .thenCompose(this::storeUpdatedPieceRecords)
      // 6. Update PO Line status
      .thenCompose(this::updatePoLinesStatus)
      // 7. Return results to the client
      .thenApply(piecesGroupedByPoLine -> prepareResponseBody(receivingCollection, piecesGroupedByPoLine));
  }

  private Map<String, Map<String, String>> groupLocationsByPoLineIdOnReceiving(ReceivingCollection receivingCollection) {
    return StreamEx
      .of(receivingCollection.getToBeReceived())
      .groupingBy(ToBeReceived::getPoLineId,
        mapping(ToBeReceived::getReceivedItems,
          collectingAndThen(toList(),
            lists -> StreamEx.of(lists)
              .flatMap(List::stream)
              .filter(receivedItem -> receivedItem.getLocationId() != null)
              .toMap(ReceivedItem::getPieceId, ReceivedItem::getLocationId))));
  }

  CompletableFuture<ReceivingHistoryCollection> getReceivingHistory(int limit, int offset, String query) {
    CompletableFuture<ReceivingHistoryCollection> future = new VertxCompletableFuture<>(ctx);

    try {
      AcquisitionsUnitsHelper acqUnitsHelper = new AcquisitionsUnitsHelper(httpClient, okapiHeaders, ctx, lang);
      acqUnitsHelper.buildAcqUnitsCqlExprToSearchRecords()
        .thenCompose(acqUnitsCqlExpr -> {
          String queryParam = buildQuery(StringUtils.isEmpty(query) ? acqUnitsCqlExpr : acqUnitsCqlExpr + " and " + query, logger);
          String endpoint = String.format(GET_RECEIVING_HISTORY_BY_QUERY, limit, offset, queryParam, lang);
          return handleGetRequest(endpoint, httpClient, ctx, okapiHeaders, logger)
            .thenAccept(jsonReceivingHistory -> future.complete(jsonReceivingHistory.mapTo(ReceivingHistoryCollection.class)));
        })
        .exceptionally(t -> {
          logger.error("Error happened retrieving receiving history", t);
          future.completeExceptionally(t.getCause());
          return null;
        });
    } catch (Exception e) {
      future.completeExceptionally(e);
    }

    return future;
  }

  private ReceivingResults prepareResponseBody(ReceivingCollection receivingCollection, Map<String, List<Piece>> piecesGroupedByPoLine) {
    ReceivingResults results = new ReceivingResults();
    results.setTotalRecords(receivingCollection.getTotalRecords());
    for (ToBeReceived toBeReceived : receivingCollection.getToBeReceived()) {
      String poLineId = toBeReceived.getPoLineId();
      ReceivingResult result = new ReceivingResult();
      results.getReceivingResults().add(result);

      // Get all processed piece records for PO Line
      Map<String, Piece> processedPiecesForPoLine = StreamEx
        .of(piecesGroupedByPoLine.getOrDefault(poLineId, Collections.emptyList()))
        .toMap(Piece::getId, piece -> piece);

      Map<String, Integer> resultCounts = new HashMap<>();
      resultCounts.put(ProcessingStatus.Type.SUCCESS.toString(), 0);
      resultCounts.put(ProcessingStatus.Type.FAILURE.toString(), 0);

      for (ReceivedItem receivedItem : toBeReceived.getReceivedItems()) {
        String pieceId = receivedItem.getPieceId();
        calculateProcessingErrors(poLineId, result, processedPiecesForPoLine, resultCounts, pieceId);
      }

      result.withPoLineId(poLineId)
            .withProcessedSuccessfully(resultCounts.get(ProcessingStatus.Type.SUCCESS.toString()))
            .withProcessedWithError(resultCounts.get(ProcessingStatus.Type.FAILURE.toString()));
    }

    return results;
  }

  /**
   * Converts {@link ReceivingCollection} to map with PO line id as a key and value is map with piece id as a key
   * and {@link ReceivedItem} as a value
   * @param receivingCollection {@link ReceivingCollection} object
   * @return map with PO line id as a key and value is map with piece id as a key and {@link ReceivedItem} as a value
   */
  private Map<String, Map<String, ReceivedItem>> groupReceivedItemsByPoLineId(ReceivingCollection receivingCollection) {
    return StreamEx
      .of(receivingCollection.getToBeReceived())
      .groupingBy(ToBeReceived::getPoLineId,
        mapping(ToBeReceived::getReceivedItems,
          collectingAndThen(toList(),
            lists -> StreamEx.of(lists)
              .flatMap(List::stream)
              .toMap(ReceivedItem::getPieceId, receivedItem -> receivedItem))));
  }

  @Override
  boolean isRevertToOnOrder(Piece piece) {
    return piece.getReceivingStatus() == ReceivingStatus.RECEIVED
        && inventoryHelper
          .isOnOrderItemStatus(piecesByLineId.get(piece.getPoLineId()).get(piece.getId()));
  }

  @Override
  CompletableFuture<Boolean> receiveInventoryItemAndUpdatePiece(JsonObject item, Piece piece) {
    ReceivedItem receivedItem = piecesByLineId.get(piece.getPoLineId())
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
  }

  @Override
  Map<String, List<Piece>> updatePieceRecordsWithoutItems(Map<String, List<Piece>> piecesGroupedByPoLine) {
    StreamEx.ofValues(piecesGroupedByPoLine)
      .flatMap(List::stream)
      .filter(piece -> StringUtils.isEmpty(piece.getItemId()))
      .forEach(this::updatePieceWithReceivingInfo);

    return piecesGroupedByPoLine;
  }

  /**
   * Updates piece record with receiving information
   *
   * @param piece
   *          piece record to be updated with receiving info
   */
  private void updatePieceWithReceivingInfo(Piece piece) {
    // Get ReceivedItem corresponding to piece record
    ReceivedItem receivedItem = piecesByLineId.get(piece.getPoLineId())
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

  @Override
  String getLocationId(Piece piece) {
    return receivingItems.get(piece.getPoLineId()).get(piece.getId()).getLocationId();
  }

}
