package org.folio.rest.impl;

import io.vertx.core.Context;
import io.vertx.core.json.JsonObject;
import one.util.streamex.StreamEx;
import org.apache.commons.lang3.StringUtils;
import org.folio.orders.utils.HelperUtils;
import org.folio.rest.acq.model.Piece;
import org.folio.rest.acq.model.Piece.ReceivingStatus;
import org.folio.rest.jaxrs.model.*;

import java.util.*;
import java.util.concurrent.CompletableFuture;

import static java.util.concurrent.CompletableFuture.completedFuture;
import static java.util.stream.Collectors.*;
import static org.folio.orders.utils.ErrorCodes.ITEM_UPDATE_FAILED;

public class CheckinHelper extends CheckinReceivePiecesHelper<CheckInPiece> {

  /**
   * Map with PO line id as a key and value is map with piece id as a key and
   * {@link CheckInPiece} as a value
   */
  private final Map<String, Map<String, CheckInPiece>> checkinPieces;

  CheckinHelper(CheckinCollection checkinCollection, Map<String, String> okapiHeaders,
                Context ctx, String lang) {
    super(getHttpClient(okapiHeaders), okapiHeaders, ctx, lang);
    // Convert request to map representation
    checkinPieces = groupCheckinPiecesByPoLineId(checkinCollection);

    // Logging quantity of the piece records to be checked in
    if (logger.isDebugEnabled()) {
      int poLinesQty = checkinPieces.size();
      int piecesQty = StreamEx.ofValues(checkinPieces)
        .mapToInt(Map::size)
        .sum();
      logger.debug("{} piece record(s) are going to be checkedIn for {} PO line(s)", piecesQty, poLinesQty);
    }
  }

  CompletableFuture<ReceivingResults> checkinPieces(CheckinCollection checkinCollection) {

    // 1. Get piece records from storage
    return retrievePieceRecords(checkinPieces)
      // 2. Filter locationId
      .thenCompose(this::filterMissingLocations)
      // 3. Update items in the Inventory if required
      .thenCompose(pieces -> updateInventoryItemsOnCheckin(checkinPieces, pieces))
      // 4. Update piece records with checkIn details which do not have
      // associated item
      .thenApply(this::updatePieceRecordsWithoutItems)
      // 5. Update received piece records in the storage
      .thenCompose(this::storeUpdatedPieceRecords)
      // 6. Update PO Line status
      .thenCompose(this::updatePoLinesStatus)
      // 7. Return results to the client
      .thenApply(piecesGroupedByPoLine -> prepareResponseBody(checkinCollection, piecesGroupedByPoLine));
  }

  /**
   * Updates items in the inventory storage with check-in details if any. On
   * success updates corresponding records as received
   *
   * @return {@link CompletableFuture} which holds map with PO line id as key
   * and list of corresponding pieces as value
   */
  CompletableFuture<Map<String, List<Piece>>> updateInventoryItemsOnCheckin(Map<String, Map<String, CheckInPiece>> checkinCollection, Map<String, List<Piece>> piecesGroupedByPoLine) {
    // Collect all piece records with non-empty item ids. The result is a map
    // with item id as a key and piece record as a value
    Map<String, Piece> piecesWithItems = collectPiecesWithItemId(piecesGroupedByPoLine);
    List<String> polineIds = new ArrayList<>(piecesGroupedByPoLine.keySet());

    // If there are no pieces with ItemId, continue
    if (piecesWithItems.isEmpty()) {
      return completedFuture(piecesGroupedByPoLine);
    }

    return getItemRecords(piecesWithItems)
      .thenCombine(getPoLines(polineIds), (items, poLines) -> {
        List<CompletableFuture<Boolean>> futuresForItemUpdates = new ArrayList<>();
        for (JsonObject item : items) {
          String itemId = item.getString(ID);
          Piece piece = piecesWithItems.get(itemId);

          PoLine poLine = searchPoLineById(poLines, piece.getPoLineId());
          CheckInPiece checkInPiece = checkinCollection.get(poLine.getId()).get(piece.getId());

          futuresForItemUpdates.add(updateHoldingsAndItems(item, piece, poLine, checkInPiece.getLocationId()));
        }
        return futuresForItemUpdates;

      })
      .thenCompose(HelperUtils::collectResultsOnSuccess)
      .thenApply(results -> {
        if (logger.isDebugEnabled()) {
          long successQty = results.stream()
            .filter(result -> result)
            .count();
          logger.debug("{} out of {} inventory item(s) successfully updated", successQty, results.size());
        }
        return piecesGroupedByPoLine;
      });
  }

  private ReceivingResults prepareResponseBody(CheckinCollection checkinCollection,
                                               Map<String, List<Piece>> piecesGroupedByPoLine) {
    ReceivingResults results = new ReceivingResults();
    results.setTotalRecords(checkinCollection.getTotalRecords());
    for (ToBeCheckedIn toBeCheckedIn : checkinCollection.getToBeCheckedIn()) {
      String poLineId = toBeCheckedIn.getPoLineId();
      ReceivingResult result = new ReceivingResult();
      results.getReceivingResults().add(result);

      // Get all processed piece records for PO Line
      Map<String, Piece> processedPiecesForPoLine = StreamEx
        .of(piecesGroupedByPoLine.getOrDefault(poLineId, Collections.emptyList()))
        .toMap(Piece::getId, piece -> piece);

      Map<String, Integer> resultCounts = new HashMap<>();
      resultCounts.put(ProcessingStatus.Type.SUCCESS.toString(), 0);
      resultCounts.put(ProcessingStatus.Type.FAILURE.toString(), 0);
      for (CheckInPiece checkinPiece : toBeCheckedIn.getCheckInPieces()) {
        String pieceId = checkinPiece.getId();

        calculateProcessingErrors(poLineId, result, processedPiecesForPoLine, resultCounts, pieceId);
      }

      result.withPoLineId(poLineId)
        .withProcessedSuccessfully(resultCounts.get(ProcessingStatus.Type.SUCCESS.toString()))
        .withProcessedWithError(resultCounts.get(ProcessingStatus.Type.FAILURE.toString()));
    }

    return results;
  }

  /**
   * Converts {@link CheckinCollection} to map with PO line id as a key and
   * value is map with piece id as a key and {@link CheckInPiece} as a value
   *
   * @param checkinCollection {@link CheckinCollection} object
   * @return map with PO line id as a key and value is map with piece id as a
   * key and {@link CheckInPiece} as a value
   */
  private Map<String, Map<String, CheckInPiece>> groupCheckinPiecesByPoLineId(CheckinCollection checkinCollection) {
    return StreamEx
      .of(checkinCollection.getToBeCheckedIn())
      .groupingBy(ToBeCheckedIn::getPoLineId,
        mapping(ToBeCheckedIn::getCheckInPieces,
          collectingAndThen(toList(),
            lists -> StreamEx.of(lists)
              .flatMap(List::stream)
              .toMap(CheckInPiece::getId, checkInPiece -> checkInPiece))));
  }

  @Override
  boolean isRevertToOnOrder(Piece piece) {
    return piece.getReceivingStatus() == ReceivingStatus.RECEIVED
      && inventoryHelper
      .isOnOrderPieceStatus(piecesByLineId.get(piece.getPoLineId()).get(piece.getId()));
  }

  @Override
  CompletableFuture<Boolean> receiveInventoryItemAndUpdatePiece(JsonObject item, Piece piece) {

    CheckInPiece checkinPiece = piecesByLineId.get(piece.getPoLineId())
      .get(piece.getId());
    return inventoryHelper
      // Update item records with check-in information and send updates to
      // Inventory
      .checkinItem(item, checkinPiece)
      // Update Piece record object with check-in details if item updated
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

  private void updatePieceWithCheckinInfo(Piece piece) {
    // Get checkinPiece corresponding to piece record
    CheckInPiece checkinPiece = piecesByLineId.get(piece.getPoLineId())
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

  @Override
  Map<String, List<Piece>> updatePieceRecordsWithoutItems(Map<String, List<Piece>> piecesGroupedByPoLine) {
    StreamEx.ofValues(piecesGroupedByPoLine)
      .flatMap(List::stream)
      .filter(piece -> StringUtils.isEmpty(piece.getItemId()))
      .forEach(this::updatePieceWithCheckinInfo);

    return piecesGroupedByPoLine;
  }

  @Override
  String getLocationId(Piece piece) {
    return checkinPieces.get(piece.getPoLineId()).get(piece.getId()).getLocationId();
  }
}
