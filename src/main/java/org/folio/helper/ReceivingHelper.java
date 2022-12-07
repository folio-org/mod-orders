package org.folio.helper;

import static java.util.stream.Collectors.collectingAndThen;
import static java.util.stream.Collectors.mapping;
import static java.util.stream.Collectors.toList;
import static org.folio.orders.utils.HelperUtils.collectResultsOnSuccess;
import static org.folio.orders.utils.HelperUtils.combineCqlExpressions;
import static org.folio.orders.utils.ResourcePathResolver.RECEIVING_HISTORY;
import static org.folio.orders.utils.ResourcePathResolver.resourcesPath;
import static org.folio.rest.core.exceptions.ErrorCodes.ITEM_UPDATE_FAILED;
import static org.folio.service.inventory.InventoryManager.ITEM_BARCODE;
import static org.folio.service.inventory.InventoryManager.ITEM_CHRONOLOGY;
import static org.folio.service.inventory.InventoryManager.ITEM_ENUMERATION;
import static org.folio.service.inventory.InventoryManager.ITEM_LEVEL_CALL_NUMBER;
import static org.folio.service.inventory.InventoryManager.ITEM_STATUS;
import static org.folio.service.inventory.InventoryManager.ITEM_STATUS_NAME;

import java.util.ArrayList;
import java.util.Collections;
import java.util.Date;
import java.util.HashMap;
import java.util.List;
import java.util.Map;
import java.util.Optional;

import org.apache.commons.lang3.StringUtils;
import org.apache.logging.log4j.LogManager;
import org.apache.logging.log4j.Logger;
import org.folio.orders.events.handlers.MessageAddress;
import org.folio.orders.utils.HelperUtils;
import org.folio.rest.core.RestClient;
import org.folio.rest.core.models.RequestContext;
import org.folio.rest.core.models.RequestEntry;
import org.folio.rest.jaxrs.model.Location;
import org.folio.rest.jaxrs.model.Piece;
import org.folio.rest.jaxrs.model.PoLine;
import org.folio.rest.jaxrs.model.ProcessingStatus;
import org.folio.rest.jaxrs.model.ReceivedItem;
import org.folio.rest.jaxrs.model.ReceivingCollection;
import org.folio.rest.jaxrs.model.ReceivingHistoryCollection;
import org.folio.rest.jaxrs.model.ReceivingResult;
import org.folio.rest.jaxrs.model.ReceivingResults;
import org.folio.rest.jaxrs.model.ToBeReceived;
import org.folio.service.AcquisitionsUnitsService;
import org.springframework.beans.factory.annotation.Autowired;

import io.vertx.core.Context;
import io.vertx.core.Future;
import io.vertx.core.json.JsonArray;
import io.vertx.core.json.JsonObject;
import one.util.streamex.StreamEx;

public class ReceivingHelper extends CheckinReceivePiecesHelper<ReceivedItem> {
  private static final Logger logger = LogManager.getLogger(ReceivingHelper.class);
  private static final String GET_RECEIVING_HISTORY_BY_QUERY = resourcesPath(RECEIVING_HISTORY);
  /**
   * Map with PO line id as a key and value is map with piece id as a key and {@link ReceivedItem} as a value
   */
  private final Map<String, Map<String, ReceivedItem>> receivingItems;
  @Autowired
  private AcquisitionsUnitsService acquisitionsUnitsService;

  public ReceivingHelper(ReceivingCollection receivingCollection, Map<String, String> okapiHeaders, Context ctx) {
    super(okapiHeaders, ctx);
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

  public ReceivingHelper(Map<String, String> okapiHeaders, Context ctx) {
    super(okapiHeaders, ctx);
    receivingItems = null;
  }

  public Future<ReceivingResults>   receiveItems(ReceivingCollection receivingCollection, RequestContext requestContext) {
    return getPoLines(new ArrayList<>(receivingItems.keySet()), requestContext)
      .compose(poLines -> removeForbiddenEntities(poLines, receivingItems, requestContext))
      .compose(vVoid -> processReceiveItems(receivingCollection, requestContext));
  }

  private Future<ReceivingResults> processReceiveItems(ReceivingCollection receivingCollection, RequestContext requestContext) {
    Map<String, Map<String, Location>> pieceLocationsGroupedByPoLine = groupLocationsByPoLineIdOnReceiving(receivingCollection);
    // 1. Get piece records from storage
    return this.retrievePieceRecords(receivingItems, requestContext)
      // 2. Filter locationId
      .compose(piecesByPoLineIds -> filterMissingLocations(piecesByPoLineIds, requestContext))
      // 3. Update items in the Inventory if required
      .compose(pieces -> updateInventoryItemsAndHoldings(pieceLocationsGroupedByPoLine, pieces, requestContext))
      // 4. Update piece records with receiving details which do not have associated item
      .map(this::updatePieceRecordsWithoutItems)
      // 5. Update received piece records in the storage
      .compose(piecesByPoLineIds -> storeUpdatedPieceRecords(piecesByPoLineIds, requestContext))
      // 6. Update PO Line status
      .compose(piecesByPoLineIds -> updatePoLinesStatus(piecesByPoLineIds, requestContext))
      // 7. Return results to the client
      .map(piecesGroupedByPoLine -> prepareResponseBody(receivingCollection, piecesGroupedByPoLine));
  }

  /**
   * Stores updated piece records with receiving/check-in details into storage.
   *
   * @param piecesGroupedByPoLine
   *          map with PO line id as key and list of corresponding pieces as
   *          value
   * @return map passed as a parameter
   */
  protected Future<Map<String, List<Piece>>> updatePoLinesStatus(Map<String, List<Piece>> piecesGroupedByPoLine, RequestContext requestContext) {
    if (piecesGroupedByPoLine.isEmpty()) {
      return Future.succeededFuture(piecesGroupedByPoLine);
    } else {
      List<String> poLineIdsForUpdatedPieces = getPoLineIdsForUpdatedPieces(piecesGroupedByPoLine);
      // Once all PO Lines are retrieved from storage check if receipt status
      // requires update and persist in storage
      return getPoLines(poLineIdsForUpdatedPieces, requestContext).compose(poLines -> {
        // Calculate expected status for each PO Line and update with new one if required
        // Skip status update if PO line status is Ongoing
        List<Future<String>> futures = new ArrayList<>();
        for (PoLine poLine : poLines) {
          if (!poLine.getPaymentStatus().equals(PoLine.PaymentStatus.ONGOING)) {
            List<Piece> successfullyProcessedPieces = getSuccessfullyProcessedPieces(poLine.getId(), piecesGroupedByPoLine);
            futures.add(calculatePoLineReceiptStatus(poLine, successfullyProcessedPieces, requestContext)
              .compose(status -> purchaseOrderLineService.updatePoLineReceiptStatus(poLine, status, requestContext)));
          }
        }

        return collectResultsOnSuccess(futures).map(updatedPoLines -> {
          logger.debug("{} out of {} PO Line(s) updated with new status", updatedPoLines.size(), piecesGroupedByPoLine.size());

          // Send event to check order status for successfully processed PO Lines
          updateOrderStatus(StreamEx.of(poLines)
            // Leave only successfully updated PO Lines
            .filter(line -> updatedPoLines.contains(line.getId()))
            .toList(), requestContext);
          return null;
        });
      })
        .map(ok -> piecesGroupedByPoLine);
    }
  }

  private void updateOrderStatus(List<PoLine> poLines, RequestContext requestContext) {
    if (!poLines.isEmpty()) {
      logger.debug("Sending event to verify order status");

      // Collect order ids which should be processed
      List<JsonObject> poIds = StreamEx
        .of(poLines)
        .map(PoLine::getPurchaseOrderId)
        .distinct()
        .map(orderId -> new JsonObject().put(ORDER_ID, orderId))
        .toList();

      JsonObject messageContent = new JsonObject();
      messageContent.put(OKAPI_HEADERS, okapiHeaders);
      messageContent.put(EVENT_PAYLOAD, new JsonArray(poIds));
      HelperUtils.sendEvent(MessageAddress.RECEIVE_ORDER_STATUS_UPDATE, messageContent, requestContext);

      logger.debug("Event to verify order status - sent");
    }
  }

  private Map<String, Map<String, Location>> groupLocationsByPoLineIdOnReceiving(ReceivingCollection receivingCollection) {
    return StreamEx
      .of(receivingCollection.getToBeReceived())
      .distinct()
      .groupingBy(ToBeReceived::getPoLineId,
        mapping(ToBeReceived::getReceivedItems,
          collectingAndThen(toList(),
            lists -> StreamEx.of(lists)
              .flatMap(List::stream)
              .toMap(ReceivedItem::getPieceId, checkInPiece  ->
                new Location().withHoldingId(checkInPiece.getHoldingId()).withLocationId(checkInPiece.getLocationId())
              ))));
  }

  public Future<ReceivingHistoryCollection> getReceivingHistory(int limit, int offset, String query,
      RequestContext requestContext) {
    return acquisitionsUnitsService.buildAcqUnitsCqlExprToSearchRecords(StringUtils.EMPTY, requestContext)
      .compose(acqUnitsCqlExpr -> {
        String cql = StringUtils.isEmpty(query) ? acqUnitsCqlExpr : combineCqlExpressions("and", acqUnitsCqlExpr, query);
        RequestEntry rq = new RequestEntry(GET_RECEIVING_HISTORY_BY_QUERY)
          .withLimit(limit)
          .withOffset(offset)
          .withQuery(cql);
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
      .distinct()
      .groupingBy(ToBeReceived::getPoLineId,
        mapping(ToBeReceived::getReceivedItems,
          collectingAndThen(toList(),
            lists -> StreamEx.of(lists)
              .flatMap(List::stream)
              .toMap(ReceivedItem::getPieceId, receivedItem -> receivedItem))));
  }

  @Override
  protected boolean isRevertToOnOrder(Piece piece) {
    return piece.getReceivingStatus() == Piece.ReceivingStatus.RECEIVED
        && inventoryManager
          .isOnOrderItemStatus(piecesByLineId.get(piece.getPoLineId()).get(piece.getId()));
  }


  @Override
  protected Future<Boolean> receiveInventoryItemAndUpdatePiece(JsonObject item, Piece piece, RequestContext requestContext) {
    ReceivedItem receivedItem = piecesByLineId.get(piece.getPoLineId())
      .get(piece.getId());
    return receiveItem(item, receivedItem, requestContext)
      // Update Piece record object with receiving details if item updated
      // successfully
      .map(v -> {
        updatePieceWithReceivingInfo(piece);
        return true;
      })
      // Add processing error if item failed to be updated
       .otherwise(e -> {
        logger.error("Item associated with piece '{}' cannot be updated", piece.getId());
        addError(piece.getPoLineId(), piece.getId(), ITEM_UPDATE_FAILED.toError());
        return false;
      });
  }

  @Override
  protected Map<String, List<Piece>> updatePieceRecordsWithoutItems(Map<String, List<Piece>> piecesGroupedByPoLine) {
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

    piece.setCaption(receivedItem.getCaption());
    piece.setComment(receivedItem.getComment());
    if (StringUtils.isNotEmpty(receivedItem.getLocationId())) {
      piece.setLocationId(receivedItem.getLocationId());
    }
    if (StringUtils.isNotEmpty(receivedItem.getHoldingId())) {
      piece.setHoldingId(receivedItem.getHoldingId());
    }
    piece.setEnumeration(receivedItem.getEnumeration());
    piece.setChronology(receivedItem.getChronology());
    piece.setDisplayOnHolding(receivedItem.getDisplayOnHolding());
    // Piece record might be received or rolled-back to Expected
    if (inventoryManager.isOnOrderItemStatus(receivedItem)) {
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
  private Future<Void> receiveItem(JsonObject itemRecord, ReceivedItem receivedItem, RequestContext requestContext) {
    // Update item record with receiving details
    itemRecord.put(ITEM_STATUS, new JsonObject().put(ITEM_STATUS_NAME, receivedItem.getItemStatus().value()));
    if (StringUtils.isNotEmpty(receivedItem.getBarcode())) {
      itemRecord.put(ITEM_BARCODE, receivedItem.getBarcode());
    }
    if (StringUtils.isNotEmpty(receivedItem.getCallNumber())) {
      itemRecord.put(ITEM_LEVEL_CALL_NUMBER, receivedItem.getCallNumber());
    }
    Optional.ofNullable(receivedItem.getEnumeration())
      .ifPresentOrElse(enumeration -> itemRecord.put(ITEM_ENUMERATION, enumeration), () -> itemRecord.remove(ITEM_ENUMERATION));
    Optional.ofNullable(receivedItem.getChronology())
      .ifPresentOrElse(chronology -> itemRecord.put(ITEM_CHRONOLOGY, chronology), () -> itemRecord.remove(ITEM_CHRONOLOGY));
    return inventoryManager.updateItem(itemRecord, requestContext);
  }

  @Override
  protected String getLocationId(Piece piece) {
    return receivingItems.get(piece.getPoLineId()).get(piece.getId()).getLocationId();
  }

  @Override
  protected String getHoldingId(Piece piece) {
    return receivingItems.get(piece.getPoLineId()).get(piece.getId()).getHoldingId();
  }
}
