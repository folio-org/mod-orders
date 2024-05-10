package org.folio.helper;

import io.vertx.core.Context;
import io.vertx.core.Future;
import io.vertx.core.json.JsonObject;
import one.util.streamex.StreamEx;
import org.folio.models.ItemFields;
import org.folio.orders.utils.PoLineCommonUtil;
import org.folio.rest.core.models.RequestContext;
import org.folio.rest.jaxrs.model.BindPiecesCollection;
import org.folio.rest.jaxrs.model.Piece;
import org.folio.rest.jaxrs.model.ProcessingStatus;
import org.folio.rest.jaxrs.model.ReceivedItem;
import org.folio.rest.jaxrs.model.ReceivingResult;
import org.folio.rest.jaxrs.model.ReceivingResults;
import org.folio.rest.jaxrs.model.Title;

import java.util.ArrayList;
import java.util.Collections;
import java.util.Date;
import java.util.HashMap;
import java.util.List;
import java.util.Map;
import java.util.stream.Collectors;


public class BindHelper extends CheckinReceivePiecesHelper<BindPiecesCollection> {

  private static final String TITLE_BY_POLINE_QUERY = "id==%s";
  public BindHelper(BindPiecesCollection bindPiecesCollection,
                    Map<String, String> okapiHeaders, Context ctx) {
    super(okapiHeaders, ctx);
    piecesByLineId = groupBindPieceByPoLineId(bindPiecesCollection);
    logger.debug("{} piece records(s) are going to be bound for '{}' PO line",
        bindPiecesCollection.getPoLineId(), bindPiecesCollection.getBindPieceIds().size());
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
    logger.debug("updatePieceRecords:: Updating the piece records to set isBound flag as TRUE");
    piecesGroupedByPoLine.values().stream()
      .flatMap(List::stream)
      .forEach(piece -> piece.setIsBound(true));
    return piecesGroupedByPoLine;
  }

  private Map<String, List<Piece>> updateItemStatus(Map<String, List<Piece>> piecesGroupedByPoLine,
                                                            RequestContext requestContext) {
    logger.debug("updateItemStatus:: Updating previous item status to 'Unavailable'");
    List<String> itemIds = piecesGroupedByPoLine.values()
      .stream().flatMap(List::stream)
      .map(Piece::getItemId).toList();
    inventoryItemManager.getItemRecordsByIds(itemIds, requestContext)
      .compose(items -> {
        items.forEach(item -> {
            logger.info("updateItemStatus:: '{}' item status set to 'Unavailable'", item.getString(ItemFields.ID.value()));
            item.put(ItemFields.STATUS.value(), new JsonObject()
              .put(ItemFields.STATUS_DATE.value(), new Date())
              .put(ItemFields.STATUS_NAME.value(), ReceivedItem.ItemStatus.UNAVAILABLE));
          }
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
      titlesService.getTitlesByQuery(String.format(TITLE_BY_POLINE_QUERY, poLineId), requestContext)
        .map(titles -> updateTitle(titles, itemIds, requestContext));
    });
    return piecesByPoLineIds;
  }

  private Future<Void> updateTitle(List<Title> titles, List<String> itemIds, RequestContext requestContext) {
    if (titles.isEmpty() || titles.get(0) == null) {
      return Future.succeededFuture();
    }
    var title = titles.get(0);
    List<String> existingBindItemIds = title.getBindItemIds() != null ? title.getBindItemIds() : new ArrayList<>();
    existingBindItemIds.addAll(itemIds);
    title = title.withBindItemIds(existingBindItemIds);
    return titlesService.saveTitle(title, requestContext);
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
