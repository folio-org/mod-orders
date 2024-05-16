package org.folio.helper;

import io.vertx.core.Context;
import io.vertx.core.Future;
import io.vertx.core.json.JsonArray;
import io.vertx.core.json.JsonObject;
import one.util.streamex.StreamEx;
import org.apache.commons.collections4.CollectionUtils;
import org.apache.commons.lang3.StringUtils;
import org.folio.orders.events.handlers.MessageAddress;
import org.folio.rest.core.RestClient;
import org.folio.rest.core.models.RequestContext;
import org.folio.rest.core.models.RequestEntry;
import org.folio.rest.jaxrs.model.Piece;
import org.folio.rest.jaxrs.model.PoLine;
import org.folio.rest.jaxrs.model.ProcessingStatus;
import org.folio.rest.jaxrs.model.ReceivedItem;
import org.folio.rest.jaxrs.model.ReceivingCollection;
import org.folio.rest.jaxrs.model.ReceivingHistoryCollection;
import org.folio.rest.jaxrs.model.ReceivingResult;
import org.folio.rest.jaxrs.model.ReceivingResults;
import org.folio.rest.jaxrs.model.ToBeReceived;
import org.folio.service.ProtectionService;
import org.springframework.beans.factory.annotation.Autowired;

import java.util.Date;
import java.util.List;
import java.util.Map;

import static java.util.stream.Collectors.collectingAndThen;
import static java.util.stream.Collectors.mapping;
import static java.util.stream.Collectors.toList;
import static org.folio.orders.utils.ResourcePathResolver.RECEIVING_HISTORY;
import static org.folio.orders.utils.ResourcePathResolver.resourcesPath;
import static org.folio.service.inventory.InventoryItemManager.COPY_NUMBER;
import static org.folio.service.inventory.InventoryItemManager.ITEM_BARCODE;
import static org.folio.service.inventory.InventoryItemManager.ITEM_CHRONOLOGY;
import static org.folio.service.inventory.InventoryItemManager.ITEM_DISPLAY_SUMMARY;
import static org.folio.service.inventory.InventoryItemManager.ITEM_ENUMERATION;
import static org.folio.service.inventory.InventoryItemManager.ITEM_LEVEL_CALL_NUMBER;
import static org.folio.service.inventory.InventoryItemManager.ITEM_STATUS;
import static org.folio.service.inventory.InventoryItemManager.ITEM_STATUS_NAME;

public class ReceivingHelper extends CheckinReceivePiecesHelper<ReceivedItem> {

  private static final String GET_RECEIVING_HISTORY_BY_QUERY = resourcesPath(RECEIVING_HISTORY);

  @Autowired
  private ProtectionService protectionService;

  public ReceivingHelper(ReceivingCollection receivingCollection, Map<String, String> okapiHeaders, Context ctx) {
    super(okapiHeaders, ctx);
    // Convert request to map representation
    piecesByLineId = groupReceivedItemsByPoLineId(receivingCollection);

    // Logging quantity of the piece records to be received
    if (logger.isDebugEnabled()) {
      int poLinesQty = piecesByLineId.size();
      int piecesQty = StreamEx.ofValues(piecesByLineId)
        .mapToInt(Map::size)
        .sum();
      logger.debug("{} piece record(s) are going to be received for {} PO line(s)", piecesQty, poLinesQty);
    }
  }

  public ReceivingHelper(Map<String, String> okapiHeaders, Context ctx) {
    super(okapiHeaders, ctx);
    piecesByLineId = null;
  }

  public Future<ReceivingResults> receiveItems(ReceivingCollection receivingCollection, RequestContext requestContext) {
    return removeForbiddenEntities(requestContext)
      .compose(vVoid -> processReceiveItems(receivingCollection, requestContext));
  }

  private Future<ReceivingResults> processReceiveItems(ReceivingCollection receivingCollection, RequestContext requestContext) {
    // 1. Get piece records from storage
    return retrievePieceRecords(requestContext)
      // 2. Filter locationId
      .compose(piecesByPoLineIds -> filterMissingLocations(piecesByPoLineIds, requestContext))
      // 3. Update items in the Inventory if required
      .compose(pieces -> updateInventoryItemsAndHoldings(pieces, requestContext))
      // 4. Update piece records with receiving details which do not have associated item
      .map(this::updatePieceRecordsWithoutItems)
      // 5. Update received piece records in the storage
      .compose(piecesByPoLineIds -> storeUpdatedPieceRecords(piecesByPoLineIds, requestContext))
      // 6. Update PO Line status
      .compose(piecesByPoLineIds -> updateOrderAndPoLinesStatus(piecesByPoLineIds, requestContext))
      // 7. Return results to the client
      .map(piecesGroupedByPoLine -> prepareResponseBody(receivingCollection, piecesGroupedByPoLine));
  }

  private Future<Map<String, List<Piece>>> updateOrderAndPoLinesStatus(Map<String, List<Piece>> piecesGroupedByPoLine, RequestContext requestContext) {
    return updateOrderAndPoLinesStatus(
      piecesGroupedByPoLine,
      requestContext,
      poLines -> updateOrderStatus(poLines, requestContext)
    );
  }

  private void updateOrderStatus(List<PoLine> poLines, RequestContext requestContext) {
    if (CollectionUtils.isEmpty(poLines)) {
      logger.info("updateOrderStatus::poLines empty, returning");
      return;
    }
    logger.debug("updateOrderStatus::Sending event to verify order status");

    // Collect order ids which should be processed
    List<JsonObject> poIds = StreamEx
      .of(poLines)
      .map(PoLine::getPurchaseOrderId)
      .distinct()
      .map(orderId -> new JsonObject().put(ORDER_ID, orderId))
      .toList();

    sendMessage(MessageAddress.RECEIVE_ORDER_STATUS_UPDATE, new JsonArray(poIds), requestContext);

    logger.debug("updateOrderStatus::Event to verify order status - sent");
  }

  public Future<ReceivingHistoryCollection> getReceivingHistory(int limit, int offset, String query,
                                                                RequestContext requestContext) {
    return protectionService.getQueryWithAcqUnitsCheck(StringUtils.EMPTY, query, requestContext)
      .compose(finalQuery -> {
        RequestEntry rq = new RequestEntry(GET_RECEIVING_HISTORY_BY_QUERY)
          .withLimit(limit)
          .withOffset(offset)
          .withQuery(finalQuery);
        return new RestClient().get(rq, ReceivingHistoryCollection.class, requestContext);
      })
      .onFailure(t -> logger.error("Error happened retrieving receiving history", t));
  }

  private ReceivingResults prepareResponseBody(ReceivingCollection receivingCollection, Map<String, List<Piece>> piecesGroupedByPoLine) {
    ReceivingResults results = new ReceivingResults();
    results.setTotalRecords(receivingCollection.getTotalRecords());
    for (ToBeReceived toBeReceived : receivingCollection.getToBeReceived()) {
      String poLineId = toBeReceived.getPoLineId();
      ReceivingResult result = new ReceivingResult();
      results.getReceivingResults().add(result);

      // Get all processed piece records for PO Line
      Map<String, Piece> processedPiecesForPoLine = getProcessedPiecesForPoLine(poLineId, piecesGroupedByPoLine);

      Map<String, Integer> resultCounts = getEmptyResultCounts();

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
   *
   * @param receivingCollection {@link ReceivingCollection} object
   * @return map with PO line id as a key and value is map with piece id as a key and {@link ReceivedItem} as a value
   */
  private Map<String, Map<String, ReceivedItem>> groupReceivedItemsByPoLineId(ReceivingCollection receivingCollection) {
    return StreamEx
      .of(receivingCollection.getToBeReceived())
      .distinct()
      .groupingBy(ToBeReceived::getPoLineId,
        mapping(ToBeReceived::getReceivedItems,
          collectingAndThen(toList(),
            lists -> StreamEx.of(lists)
              .flatMap(List::stream)
              .toMap(ReceivedItem::getPieceId, receivedItem -> receivedItem))));
  }

  @Override
  protected Future<Boolean> receiveInventoryItemAndUpdatePiece(JsonObject item, Piece piece, RequestContext locationContext) {
    ReceivedItem receivedItem = getByPiece(piece);
    return receiveItem(item, receivedItem, locationContext)
      // Update Piece record object with receiving details if item updated
      // successfully
      .map(v -> {
        updatePieceWithReceivingInfo(piece);
        return true;
      })
      // Add processing error if item failed to be updated
      .otherwise(e -> {
        addErrorForUpdatingItem(piece, e);
        return false;
      });
  }

  @Override
  protected Map<String, List<Piece>> updatePieceRecordsWithoutItems(Map<String, List<Piece>> piecesGroupedByPoLine) {
    extractAllPieces(piecesGroupedByPoLine)
      .filter(piece -> StringUtils.isEmpty(piece.getItemId()))
      .forEach(this::updatePieceWithReceivingInfo);

    return piecesGroupedByPoLine;
  }

  /**
   * Updates piece record with receiving information
   *
   * @param piece piece record to be updated with receiving info
   */
  private void updatePieceWithReceivingInfo(Piece piece) {
    ReceivedItem receivedItem = getByPiece(piece);

    piece.setDisplaySummary(receivedItem.getDisplaySummary());
    piece.setComment(receivedItem.getComment());
    if (StringUtils.isNotEmpty(receivedItem.getLocationId())) {
      piece.setLocationId(receivedItem.getLocationId());
    }
    if (StringUtils.isNotEmpty(receivedItem.getHoldingId())) {
      piece.setHoldingId(receivedItem.getHoldingId());
    }
    piece.setEnumeration(receivedItem.getEnumeration());
    piece.setChronology(receivedItem.getChronology());
    piece.setCopyNumber(receivedItem.getCopyNumber());
    piece.setDisplayOnHolding(receivedItem.getDisplayOnHolding());
    // Piece record might be received or rolled-back to Expected
    if (isOnOrderItemStatus(receivedItem)) {
      piece.setReceivedDate(null);
      piece.setReceivingStatus(Piece.ReceivingStatus.EXPECTED);
    } else {
      piece.setReceivedDate(new Date());
      piece.setReceivingStatus(Piece.ReceivingStatus.RECEIVED);
    }
  }

  /**
   * Returns list of item records for specified id's.
   *
   * @param itemRecord   item record
   * @param receivedItem item details specified by user upon receiving flow
   * @return future with list of item records
   */
  private Future<Void> receiveItem(JsonObject itemRecord, ReceivedItem receivedItem, RequestContext locationContext) {
    // Update item record with receiving details
    itemRecord.put(ITEM_STATUS, new JsonObject().put(ITEM_STATUS_NAME, receivedItem.getItemStatus().value()));

    if (StringUtils.isNotEmpty(receivedItem.getDisplaySummary())) {
      itemRecord.put(ITEM_DISPLAY_SUMMARY, receivedItem.getDisplaySummary());
    }
    if (StringUtils.isNotEmpty(receivedItem.getEnumeration())) {
      itemRecord.put(ITEM_ENUMERATION, receivedItem.getEnumeration());
    }
    if (StringUtils.isNotEmpty(receivedItem.getCopyNumber())) {
      itemRecord.put(COPY_NUMBER, receivedItem.getCopyNumber());
    }
    if (StringUtils.isNotEmpty(receivedItem.getChronology())) {
      itemRecord.put(ITEM_CHRONOLOGY, receivedItem.getChronology());
    }
    if (StringUtils.isNotEmpty(receivedItem.getBarcode())) {
      itemRecord.put(ITEM_BARCODE, receivedItem.getBarcode());
    }
    if (StringUtils.isNotEmpty(receivedItem.getCallNumber())) {
      itemRecord.put(ITEM_LEVEL_CALL_NUMBER, receivedItem.getCallNumber());
    }

    return inventoryItemManager.updateItem(itemRecord, locationContext);
  }

  @Override
  protected String getLocationId(Piece piece) {
    return getByPiece(piece).getLocationId();
  }

  @Override
  protected String getHoldingId(Piece piece) {
    return getByPiece(piece).getHoldingId();
  }

  @Override
  protected boolean isRevertToOnOrder(Piece piece) {
    return piece.getReceivingStatus() == Piece.ReceivingStatus.RECEIVED && isOnOrderItemStatus(getByPiece(piece));
  }

  private boolean isOnOrderItemStatus(ReceivedItem receivedItem) {
    return receivedItem.getItemStatus() == ReceivedItem.ItemStatus.ON_ORDER;
  }

}
