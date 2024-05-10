package org.folio.helper;

import io.vertx.core.Context;
import io.vertx.core.Future;
import io.vertx.core.json.JsonObject;
import one.util.streamex.StreamEx;
import org.folio.orders.utils.PoLineCommonUtil;
import org.folio.rest.core.models.RequestContext;
import org.folio.rest.jaxrs.model.BindPiecesCollection;
import org.folio.rest.jaxrs.model.Piece;
import org.folio.rest.jaxrs.model.ProcessingStatus;
import org.folio.rest.jaxrs.model.ReceivingResult;
import org.folio.rest.jaxrs.model.ReceivingResults;
import org.folio.rest.jaxrs.model.Title;

import java.util.Collections;
import java.util.Date;
import java.util.HashMap;
import java.util.List;
import java.util.Map;
import java.util.stream.Collectors;

import static org.folio.service.inventory.InventoryItemManager.ITEM_STATUS;

public class BindHelper extends CheckinReceivePiecesHelper<BindPiecesCollection> {

  public BindHelper(BindPiecesCollection bindPiecesCollection,
                    Map<String, String> okapiHeaders, Context ctx) {
    super(okapiHeaders, ctx);
    piecesByLineId = groupBindPieceByPoLineId(bindPiecesCollection);
    if (logger.isDebugEnabled()) {
      int poLineQty = piecesByLineId.size();
      int piecesQty = StreamEx.ofValues(piecesByLineId)
        .mapToInt(Map::size)
        .sum();
      logger.debug("{} piece records(s) are going to be bound for {} PO line(s)", piecesQty, poLineQty);
    }
  }

  private Map<String, Map<String, BindPiecesCollection>> groupBindPieceByPoLineId(BindPiecesCollection bindPiecesCollection) {
    String poLineId = bindPiecesCollection.getPoLineId();
    Map<String, BindPiecesCollection> bindPieceMap = bindPiecesCollection.getBindPieceIds().stream()
      .collect(Collectors.toMap(
        bindPieceId -> bindPieceId,
        bindPieceId -> bindPiecesCollection
      ));

    return Map.of(poLineId, bindPieceMap);
  }

  public Future<ReceivingResults> bindPieces(BindPiecesCollection bindPiecesCollection, RequestContext requestContext) {
    return removeForbiddenEntities(requestContext)
      .compose(vVoid -> processBindPieces(bindPiecesCollection, requestContext));
  }

  private Future<ReceivingResults> processBindPieces(BindPiecesCollection bindPiecesCollection, RequestContext requestContext) {
    // 1. Get piece records from storage
    return retrievePieceRecords(requestContext)
      // 2. Update piece isBound flag
      .map(this::updatePieceRecords)
      // 3. Update currently associated items
      .map(piecesGroupedByPoLine -> updateItemStatus(piecesGroupedByPoLine, requestContext))
      // 4. Crate item for pieces with specific fields
      .compose(piecesGroupedByPoLine -> createItemForPiece(piecesGroupedByPoLine, bindPiecesCollection, requestContext))
      // 5. Update received piece records in the storage
      .compose(piecesGroupedByPoLine -> storeUpdatedPieceRecords(piecesGroupedByPoLine, requestContext))
      // 6. Update Title with new bind items
      .map(piecesGroupedByPoLine -> updateTitleWithBindItems(piecesGroupedByPoLine, requestContext))
      // 7. Return results to the client
      .map(piecesGroupedByPoLine -> prepareResponseBody(piecesGroupedByPoLine, bindPiecesCollection));
  }

  private Map<String, List<Piece>> updatePieceRecords(Map<String, List<Piece>> piecesGroupedByPoLine) {
    StreamEx.ofValues(piecesGroupedByPoLine)
      .flatMap(List::stream)
      .forEach(piece -> piece.setIsBound(true));

    return piecesGroupedByPoLine;
  }

  private Map<String, List<Piece>> updateItemStatus(Map<String, List<Piece>> piecesGroupedByPoLine,
                                                            RequestContext requestContext) {
    List<String> itemIds = piecesGroupedByPoLine.values().stream().flatMap(List::stream)
        .map(Piece::getItemId).toList();
    inventoryItemManager.getItemRecordsByIds(itemIds, requestContext)
      .compose(items -> {
        items.forEach(item ->
          item.put(ITEM_STATUS, new JsonObject().put("date", new Date()).put("name", "Unavailable"))
        );
        return inventoryItemManager.updateItemRecords(items, requestContext);
      });
    return piecesGroupedByPoLine;
  }

  private Future<Map<String, List<Piece>>> createItemForPiece(Map<String, List<Piece>> piecesGroupedByPoLine,
                                                      BindPiecesCollection bindPiecesCollection,
                                                      RequestContext requestContext) {
    String poLineId = bindPiecesCollection.getPoLineId();
    logger.debug("createItemForPiece:: Trying to get poLine by id '{}'", poLineId);
    return purchaseOrderLineService.getOrderLineById(poLineId, requestContext)
      .map(PoLineCommonUtil::convertToCompositePoLine)
      .compose(compPOL ->
        inventoryItemManager.createBindItem(compPOL, bindPiecesCollection.getBindItem(), requestContext))
      .map(itemId -> {
          piecesGroupedByPoLine.get(poLineId).forEach(piece -> piece.setItemId(itemId));
          return piecesGroupedByPoLine;
        }
      );
  }

  private Map<String, List<Piece>> updateTitleWithBindItems(Map<String, List<Piece>> piecesByPoLineIds,
                                                            RequestContext requestContext) {
    piecesByPoLineIds.forEach((poLineId, pieces) -> {
      List<String> itemIds = pieces.stream().map(Piece::getItemId).distinct().toList();
      titlesService.getTitlesByPoLineIds(List.of(poLineId), requestContext)
        .map(titles -> {
            if (titles.containsKey(poLineId) && !titles.get(poLineId).isEmpty()) {
              Title title = titles.get(poLineId).get(0).withBindItemIds(itemIds);
              titlesService.saveTitle(title, requestContext);
            }
            return titles;
          }
        );
    });
    return piecesByPoLineIds;
  }

  private ReceivingResults prepareResponseBody(Map<String, List<Piece>> piecesGroupedByPoLine,
                                               BindPiecesCollection bindPiecesCollection) {
    String poLineId = bindPiecesCollection.getPoLineId();

    // Get all processed piece records for PO Line
    Map<String, Piece> processedPiecesForPoLine = StreamEx
      .of(piecesGroupedByPoLine.getOrDefault(poLineId, Collections.emptyList()))
      .toMap(Piece::getId, piece -> piece);

    Map<String, Integer> resultCounts = new HashMap<>();
    resultCounts.put(ProcessingStatus.Type.SUCCESS.toString(), 0);
    resultCounts.put(ProcessingStatus.Type.FAILURE.toString(), 0);
    ReceivingResult result = new ReceivingResult();
    for (String pieceId : bindPiecesCollection.getBindPieceIds()) {
      calculateProcessingErrors(poLineId, result, processedPiecesForPoLine, resultCounts, pieceId);
    }

    result.withPoLineId(poLineId)
      .withProcessedSuccessfully(resultCounts.get(ProcessingStatus.Type.SUCCESS.toString()))
      .withProcessedWithError(resultCounts.get(ProcessingStatus.Type.FAILURE.toString()));
    return new ReceivingResults()
      .withTotalRecords(1)
      .withReceivingResults(List.of(result));
  }

  @Override
  protected boolean isRevertToOnOrder(Piece piece) {
    return false;
  }

  @Override
  protected Future<Boolean> receiveInventoryItemAndUpdatePiece(JsonObject item, Piece piece, RequestContext requestContext) {
    return null;
  }

  @Override
  protected Map<String, List<Piece>> updatePieceRecordsWithoutItems(Map<String, List<Piece>> piecesGroupedByPoLine) {
    return Map.of();
  }

  @Override
  protected String getHoldingId(Piece piece) {
    return "";
  }

  @Override
  protected String getLocationId(Piece piece) {
    return "";
  }
}
