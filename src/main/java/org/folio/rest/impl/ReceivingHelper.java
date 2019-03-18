package org.folio.rest.impl;

import static java.util.stream.Collectors.collectingAndThen;
import static java.util.stream.Collectors.mapping;
import static java.util.stream.Collectors.toList;
import static org.folio.orders.utils.HelperUtils.getEndpointWithQuery;
import static org.folio.orders.utils.HelperUtils.handleGetRequest;
import static org.folio.orders.utils.ResourcePathResolver.RECEIVING_HISTORY;
import static org.folio.orders.utils.ResourcePathResolver.resourcesPath;

import io.vertx.core.Context;
import java.util.Collections;
import java.util.HashMap;
import java.util.List;
import java.util.Map;
import java.util.concurrent.CompletableFuture;
import me.escoffier.vertx.completablefuture.VertxCompletableFuture;
import one.util.streamex.StreamEx;
import org.folio.rest.acq.model.Piece;
import org.folio.rest.jaxrs.model.*;
import org.folio.rest.jaxrs.model.Error;

public class ReceivingHelper extends AbstractHelper {

  private static final String GET_RECEIVING_HISTORY_BY_QUERY = resourcesPath(RECEIVING_HISTORY) + "?limit=%s&offset=%s%s&lang=%s";

  /**
   * Map with PO line id as a key and value is map with piece id as a key and {@link Error} as a value
   */
  private final Map<String, Map<String, Error>> processingErrors = new HashMap<>();
  /**
   * Map with PO line id as a key and value is map with piece id as a key and {@link ReceivedItem} as a value
   */
  private final Map<String, Map<String, ReceivedItem>> receivingItems;


  private final CheckInRecievePiecesHelper<ReceivedItem> piecesHelper;


  ReceivingHelper(ReceivingCollection receivingCollection, Map<String, String> okapiHeaders, Context ctx, String lang) {
    super(okapiHeaders, ctx, lang);

    // Convert request to map representation
    receivingItems = groupReceivedItemsByPoLineId(receivingCollection);
    piecesHelper = new CheckInRecievePiecesHelper<>(httpClient, okapiHeaders, ctx, lang, receivingItems, processingErrors);

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
    super(okapiHeaders, ctx, lang);
    receivingItems = null;
    piecesHelper = null;
  }

  CompletableFuture<ReceivingResults> receiveItems(ReceivingCollection receivingCollection) {

    // 1. Get piece records from storage
    return piecesHelper.retrievePieceRecords()
      // 2. Update items in the Inventory if required
      .thenCompose(piecesHelper::updateInventoryItems)
      // 3. Update piece records with receiving details which do not have associated item
      .thenApply(piecesHelper::updatePieceRecordsWithoutItems)
      // 4. Update received piece records in the storage
      .thenCompose(piecesHelper::storeUpdatedPieceRecords)
      // 5. Update PO Line status
      .thenCompose(piecesHelper::updatePoLinesStatus)
      // 6. Return results to the client
      .thenApply(piecesGroupedByPoLine -> prepareResponseBody(receivingCollection, piecesGroupedByPoLine));
  }

  CompletableFuture<ReceivingHistoryCollection> getReceivingHistory(int limit, int offset, String query) {
    CompletableFuture<ReceivingHistoryCollection> future = new VertxCompletableFuture<>(ctx);

    try {
      String queryParam = getEndpointWithQuery(query, logger);
      String endpoint = String.format(GET_RECEIVING_HISTORY_BY_QUERY, limit, offset, queryParam, lang);
      handleGetRequest(endpoint, httpClient, ctx, okapiHeaders, logger)
        .thenAccept(jsonReceivingHistory -> future.complete(jsonReceivingHistory.mapTo(ReceivingHistoryCollection.class)))
        .exceptionally(t -> {
          logger.error("Error retrieving receiving history", t);
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

      int succeded = 0;
      int failed = 0;
      for (ReceivedItem receivedItem : toBeReceived.getReceivedItems()) {
        String pieceId = receivedItem.getPieceId();

        // Calculate processing status
        ProcessingStatus status = new ProcessingStatus();
        if (processedPiecesForPoLine.get(pieceId) != null && getError(poLineId, pieceId) == null) {
          status.setType(ProcessingStatus.Type.SUCCESS);
          succeded++;
        } else {
          status.setType(ProcessingStatus.Type.FAILURE);
          status.setError(getError(poLineId, pieceId));
          failed++;
        }

        ReceivingItemResult itemResult = new ReceivingItemResult();
        itemResult.setPieceId(pieceId);
        itemResult.setProcessingStatus(status);
        result.getReceivingItemResults().add(itemResult);
      }

      result.withPoLineId(poLineId)
            .withProcessedSuccessfully(succeded)
            .withProcessedWithError(failed);
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
  
  private Error getError(String polId, String pieceId) {
    return processingErrors.computeIfAbsent(polId, k -> Collections.emptyMap())
                           .get(pieceId);
  }
}
