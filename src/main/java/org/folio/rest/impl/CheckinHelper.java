package org.folio.rest.impl;

import static java.util.stream.Collectors.collectingAndThen;
import static java.util.stream.Collectors.mapping;
import static java.util.stream.Collectors.toList;

import io.vertx.core.Context;
import java.util.Collections;
import java.util.HashMap;
import java.util.List;
import java.util.Map;
import java.util.concurrent.CompletableFuture;
import one.util.streamex.StreamEx;
import org.folio.rest.acq.model.Piece;
import org.folio.rest.jaxrs.model.*;
import org.folio.rest.jaxrs.model.Error;

public class CheckinHelper extends AbstractHelper {


  private final Map<String, Map<String, Error>> processingErrors = new HashMap<>();
  /**
   * Map with PO line id as a key and value is map with piece id as a key and
   * {@link CheckInPiece} as a value
   */
  private final Map<String, Map<String, CheckInPiece>> checkinPieces;
  private final CheckinReceivePiecesHelper<CheckInPiece> piecesHelper;

  CheckinHelper(CheckinCollection checkinCollection, Map<String, String> okapiHeaders,
      Context ctx, String lang) {
    super(getHttpClient(okapiHeaders), okapiHeaders, ctx, lang);
    // Convert request to map representation
    checkinPieces = groupcheckinPiecesByPoLineId(checkinCollection);
    piecesHelper = new CheckinReceivePiecesHelper<>(httpClient, okapiHeaders, ctx, lang, checkinPieces, processingErrors);

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
    return piecesHelper.retrievePieceRecords()
      // 2. Update items in the Inventory if required
      .thenCompose(piecesHelper::updateInventoryItems)
      // 3. Update piece records with checkIn details which do not have
      // associated item
      .thenApply(piecesHelper::updatePieceRecordsWithoutItems)
      // 4. Update received piece records in the storage
      .thenCompose(piecesHelper::storeUpdatedPieceRecords)
      // 5. Update PO Line status
      .thenCompose(piecesHelper::updatePoLinesStatus)
      // 6. Return results to the client
      .thenApply(piecesGroupedByPoLine -> prepareResponseBody(checkinCollection, piecesGroupedByPoLine));
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
      for (CheckInPiece checkinPiece : toBeCheckedIn.getCheckInPieces()) {
        String pieceId = checkinPiece.getId();

        piecesHelper.calculateProcessingErrors(poLineId, result, processedPiecesForPoLine, resultCounts, pieceId);
      }

      result.withPoLineId(poLineId)
            .withProcessedSuccessfully(resultCounts.get("succeded"))
            .withProcessedWithError(resultCounts.get("failed"));
    }

    return results;
  }

  
  /**
   * Converts {@link CheckinCollection} to map with PO line id as a key and value is map with piece id as a key
   * and {@link CheckInPiece} as a value
   * @param checkinCollection {@link CheckinCollection} object
   * @return map with PO line id as a key and value is map with piece id as a key and {@link CheckInPiece} as a value
   */
  private Map<String, Map<String, CheckInPiece>> groupcheckinPiecesByPoLineId(CheckinCollection checkinCollection) {
    return StreamEx
      .of(checkinCollection.getToBeCheckedIn())
      .groupingBy(ToBeCheckedIn::getPoLineId,
        mapping(ToBeCheckedIn::getCheckInPieces,
          collectingAndThen(toList(),
            lists -> StreamEx.of(lists)
              .flatMap(List::stream)
              .toMap(CheckInPiece::getId, checkInPiece -> checkInPiece))));
  }
    
    
  private Error getError(String polId, String pieceId) {
    return processingErrors.computeIfAbsent(polId, k -> Collections.emptyMap())
      .get(pieceId);
  }

}
