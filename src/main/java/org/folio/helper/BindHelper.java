package org.folio.helper;

import io.vertx.core.Context;
import io.vertx.core.Future;
import io.vertx.core.json.JsonObject;
import one.util.streamex.StreamEx;
import org.apache.commons.lang3.StringUtils;
import org.folio.orders.utils.PoLineCommonUtil;
import org.folio.rest.core.models.RequestContext;
import org.folio.rest.jaxrs.model.BindPiecesCollection;
import org.folio.rest.jaxrs.model.Piece;
import org.folio.rest.jaxrs.model.ProcessingStatus;
import org.folio.rest.jaxrs.model.ReceivingResult;
import org.folio.rest.jaxrs.model.ReceivingResults;
import org.folio.rest.jaxrs.model.Title;
import org.folio.rest.jaxrs.model.ToBeBound;

import java.util.Collections;
import java.util.HashMap;
import java.util.List;
import java.util.Map;
import java.util.stream.Collectors;

public class BindHelper extends CheckinReceivePiecesHelper<ToBeBound> {

  public BindHelper(BindPiecesCollection bindPiecesCollection,
                    Map<String, String> okapiHeaders, Context ctx) {
    super(okapiHeaders, ctx);
    piecesByLineId = groupBindPieceByPoLineId(bindPiecesCollection);
    if (logger.isDebugEnabled()) {
      int poLineQty = piecesByLineId.size();
      int piecesQty = StreamEx.ofValues(piecesByLineId)
        .mapToInt(Map::size)
        .sum();
      logger.debug("{} piece records(s) are going to be bind for {} PO line(s)", piecesQty, poLineQty);
    }
  }

  private Map<String, Map<String, ToBeBound>> groupBindPieceByPoLineId(BindPiecesCollection bindPiecesCollection) {
    ToBeBound toBeBound = bindPiecesCollection.getToBeBound();
    String poLineId = toBeBound.getPoLineId();
    Map<String, ToBeBound> bindPieceMap = toBeBound.getBindPieceIds().stream()
      .collect(Collectors.toMap(
        bindPieceId -> bindPieceId,
        bindPieceId -> toBeBound
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
      // 4. Crate item for pieces with specific fields
      .compose(piecesGroupedByPoLine -> createItemForPiece(piecesGroupedByPoLine, bindPiecesCollection, requestContext))
      // 3. Update received piece records in the storage
      .compose(piecesGroupedByPoLine -> storeUpdatedPieceRecords(piecesGroupedByPoLine, requestContext))
      // 4. Update Title with new bind items
      .map(piecesByPoLineIds -> updateTitleWithBindItems(piecesByPoLineIds, requestContext))
      // 5. Return results to the client
      .map(piecesGroupedByPoLine -> prepareResponseBody(bindPiecesCollection, piecesGroupedByPoLine));
  }

  private Map<String, List<Piece>> updatePieceRecords(Map<String, List<Piece>> piecesGroupedByPoLine) {
    StreamEx.ofValues(piecesGroupedByPoLine)
      .flatMap(List::stream)
      .forEach(piece -> piece.setIsBound(true));

    return piecesGroupedByPoLine;
  }

  private Future<Map<String, List<Piece>>> createItemForPiece(Map<String, List<Piece>> piecesGroupedByPoLine,
                                                      BindPiecesCollection bindPiecesCollection,
                                                      RequestContext requestContext) {
    String poLineId = bindPiecesCollection.getToBeBound().getPoLineId();
    logger.debug("createItemForPiece:: Trying to get poLine by id '{}'", poLineId);
    return purchaseOrderLineService.getOrderLineById(poLineId, requestContext)
      .map(PoLineCommonUtil::convertToCompositePoLine)
      .compose(compPOL ->
        inventoryItemManager.createBindItem(compPOL, bindPiecesCollection.getToBeBound().getBindItem(), requestContext)
      )
      .map(itemId -> {
          piecesGroupedByPoLine.get(poLineId).forEach(piece -> piece.withItemId(itemId));
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

  private ReceivingResults prepareResponseBody(BindPiecesCollection bindPiecesCollection,
                                               Map<String, List<Piece>> piecesGroupedByPoLine) {
    ReceivingResults results = new ReceivingResults();
    results.setTotalRecords(1);
    var toBeBound = bindPiecesCollection.getToBeBound();
    String poLineId = toBeBound.getPoLineId();
    ReceivingResult result = new ReceivingResult();
    results.getReceivingResults().add(result);

    // Get all processed piece records for PO Line
    Map<String, Piece> processedPiecesForPoLine = StreamEx
      .of(piecesGroupedByPoLine.getOrDefault(poLineId, Collections.emptyList()))
      .toMap(Piece::getId, piece -> piece);

    Map<String, Integer> resultCounts = new HashMap<>();
    resultCounts.put(ProcessingStatus.Type.SUCCESS.toString(), 0);
    resultCounts.put(ProcessingStatus.Type.FAILURE.toString(), 0);
    for (String pieceId : toBeBound.getBindPieceIds()) {
      calculateProcessingErrors(poLineId, result, processedPiecesForPoLine, resultCounts, pieceId);
    }

    result.withPoLineId(poLineId)
      .withProcessedSuccessfully(resultCounts.get(ProcessingStatus.Type.SUCCESS.toString()))
      .withProcessedWithError(resultCounts.get(ProcessingStatus.Type.FAILURE.toString()));
    return results;
  }

  @Override
  protected boolean isRevertToOnOrder(Piece piece) {
    return false;
  }

  @Override
  protected Future<Boolean> receiveInventoryItemAndUpdatePiece(JsonObject item, Piece piece, RequestContext requestContext) {
    return Future.succeededFuture(false);
  }

  @Override
  protected Map<String, List<Piece>> updatePieceRecordsWithoutItems(Map<String, List<Piece>> piecesGroupedByPoLine) {
    return Collections.emptyMap();
  }

  @Override
  protected String getHoldingId(Piece piece) {
    return StringUtils.EMPTY;
  }

  @Override
  protected String getLocationId(Piece piece) {
    return StringUtils.EMPTY;
  }
}
