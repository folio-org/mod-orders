package org.folio.helper;

import io.vertx.core.Context;
import io.vertx.core.Future;
import io.vertx.core.Promise;
import io.vertx.core.json.JsonArray;
import io.vertx.core.json.JsonObject;
import one.util.streamex.StreamEx;
import org.apache.commons.collections4.CollectionUtils;
import org.apache.commons.lang3.StringUtils;
import org.apache.commons.lang3.tuple.Pair;
import org.folio.orders.events.handlers.MessageAddress;
import org.folio.orders.utils.PoLineCommonUtil;
import org.folio.rest.core.models.RequestContext;
import org.folio.rest.jaxrs.model.CheckInPiece;
import org.folio.rest.jaxrs.model.CheckinCollection;
import org.folio.rest.jaxrs.model.Piece;
import org.folio.rest.jaxrs.model.PoLine;
import org.folio.rest.jaxrs.model.ProcessingStatus;
import org.folio.rest.jaxrs.model.ReceivingResult;
import org.folio.rest.jaxrs.model.ReceivingResults;
import org.folio.rest.jaxrs.model.ToBeCheckedIn;

import java.util.ArrayList;
import java.util.Collection;
import java.util.Collections;
import java.util.Date;
import java.util.HashMap;
import java.util.List;
import java.util.Map;

import static java.util.stream.Collectors.collectingAndThen;
import static java.util.stream.Collectors.groupingBy;
import static java.util.stream.Collectors.mapping;
import static java.util.stream.Collectors.toList;
import static org.folio.orders.utils.HelperUtils.collectResultsOnSuccess;
import static org.folio.service.inventory.InventoryItemManager.COPY_NUMBER;
import static org.folio.service.inventory.InventoryItemManager.ITEM_ACCESSION_NUMBER;
import static org.folio.service.inventory.InventoryItemManager.ITEM_BARCODE;
import static org.folio.service.inventory.InventoryItemManager.ITEM_CHRONOLOGY;
import static org.folio.service.inventory.InventoryItemManager.ITEM_DISCOVERY_SUPPRESS;
import static org.folio.service.inventory.InventoryItemManager.ITEM_DISPLAY_SUMMARY;
import static org.folio.service.inventory.InventoryItemManager.ITEM_ENUMERATION;
import static org.folio.service.inventory.InventoryItemManager.ITEM_LEVEL_CALL_NUMBER;
import static org.folio.service.inventory.InventoryItemManager.ITEM_STATUS;
import static org.folio.service.inventory.InventoryItemManager.ITEM_STATUS_NAME;

public class CheckinHelper extends CheckinReceivePiecesHelper<CheckInPiece> {

  public static final String IS_ITEM_ORDER_CLOSED_PRESENT = "isItemOrderClosedPresent";

  public CheckinHelper(CheckinCollection checkinCollection, Map<String, String> okapiHeaders,
                       Context ctx) {
    super(okapiHeaders, ctx);
    // Convert request to map representation
    CheckinCollection checkinCollectionClone = JsonObject.mapFrom(checkinCollection).mapTo(CheckinCollection.class);
    piecesByLineId = groupCheckinPiecesByPoLineId(checkinCollectionClone);
    updateCheckInPiecesStatus();
    // Logging quantity of the piece records to be checked in
    if (logger.isDebugEnabled()) {
      int poLinesQty = piecesByLineId.size();
      int piecesQty = StreamEx.ofValues(piecesByLineId)
        .mapToInt(Map::size)
        .sum();
      logger.debug("{} piece record(s) are going to be checkedIn for {} PO line(s)", piecesQty, poLinesQty);
    }
  }

  public Future<ReceivingResults> checkinPieces(CheckinCollection checkinCollection, RequestContext requestContext) {
    return removeForbiddenEntities(requestContext)
      .compose(vVoid -> processCheckInPieces(checkinCollection, requestContext));
  }

  private Future<ReceivingResults> processCheckInPieces(CheckinCollection checkinCollection, RequestContext requestContext) {
    // 1. Get piece records from storage
    return createItemsWithPieceUpdate(checkinCollection, requestContext)
      // 2. Filter locationId
      .compose(piecesByPoLineIds -> filterMissingLocations(piecesByPoLineIds, requestContext))
      // 3. Update items in the Inventory if required
      .compose(pieces -> updateInventoryItemsAndHoldings(pieces, requestContext))
      // 4. Update piece records with checkIn details which do not have
      // associated item
      .map(this::updatePieceRecordsWithoutItems)
      // 5. Update received piece records in the storage
      .compose(piecesGroupedByPoLine -> storeUpdatedPieceRecords(piecesGroupedByPoLine, requestContext))
      // 6. Update PO Line status
      .compose(piecesGroupedByPoLine -> updateOrderAndPoLinesStatus(piecesGroupedByPoLine, checkinCollection, requestContext))
      // 7. Return results to the client
      .map(piecesGroupedByPoLine -> prepareResponseBody(checkinCollection, piecesGroupedByPoLine));
  }

  private Future<Map<String, List<Piece>>> createItemsWithPieceUpdate(CheckinCollection checkinCollection, RequestContext requestContext) {
    Map<String, List<CheckInPiece>> poLineIdVsCheckInPiece = getItemCreateNeededCheckinPieces(checkinCollection);
    List<Future<Pair<String, Piece>>> futures = new ArrayList<>();
    return retrievePieceRecords(requestContext)
      .map(piecesGroupedByPoLine -> {
        piecesGroupedByPoLine.forEach((poLineId, pieces) -> poLineIdVsCheckInPiece.get(poLineId)
          .forEach(checkInPiece -> pieces.forEach(piece -> {
            if (checkInPiece.getId().equals(piece.getId()) && Boolean.TRUE.equals(checkInPiece.getCreateItem())) {
              futures.add(purchaseOrderLineService.getOrderLineById(poLineId, requestContext)
                .map(PoLineCommonUtil::convertToCompositePoLine)
                .compose(compPOL -> pieceCreateFlowInventoryManager.processInventory(compPOL, piece,
                  checkInPiece.getCreateItem(), requestContext))
                .map(aVoid -> Pair.of(poLineId, piece)));
            } else {
              futures.add(Future.succeededFuture(Pair.of(poLineId, piece)));
            }
          })));
        return null;
      })
      .compose(v -> collectResultsOnSuccess(futures).map(poLineIdVsPieceList -> StreamEx.of(poLineIdVsPieceList)
        .distinct()
        .groupingBy(Pair::getKey, mapping(Pair::getValue, collectingAndThen(toList(), lists -> StreamEx.of(lists)
          .collect(toList()))))));
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
      Map<String, Piece> processedPiecesForPoLine = getProcessedPiecesForPoLine(poLineId, piecesGroupedByPoLine);

      Map<String, Integer> resultCounts = getEmptyResultCounts();
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
      .distinct()
      .groupingBy(ToBeCheckedIn::getPoLineId,
        mapping(ToBeCheckedIn::getCheckInPieces,
          collectingAndThen(toList(),
            lists -> StreamEx.of(lists)
              .flatMap(List::stream)
              .toMap(CheckInPiece::getId, checkInPiece -> checkInPiece))));
  }

  public Map<String, List<CheckInPiece>> getItemCreateNeededCheckinPieces(CheckinCollection checkinCollection) {
    return StreamEx
      .of(checkinCollection.getToBeCheckedIn())
      .distinct()
      .groupingBy(ToBeCheckedIn::getPoLineId,
        mapping(ToBeCheckedIn::getCheckInPieces,
          collectingAndThen(toList(),
            lists -> StreamEx.of(lists)
              .flatMap(List::stream)
              .collect(toList()))));
  }

  @Override
  protected Future<Boolean> receiveInventoryItemAndUpdatePiece(JsonObject item, Piece piece, RequestContext locationContext) {
    Promise<Boolean> promise = Promise.promise();
    CheckInPiece checkinPiece = getByPiece(piece);
    checkinItem(item, checkinPiece, locationContext)
      // Update Piece record object with check-in details if item updated
      // successfully
      .map(v -> {
        updatePieceWithCheckinInfo(piece);
        promise.complete(true);
        return true;
      })
      // Add processing error if item failed to be updated
      .onFailure(e -> {
        addErrorForUpdatingItem(piece, e);
        promise.complete(false);
      });
    return promise.future();
  }

  private void updatePieceWithCheckinInfo(Piece piece) {
    CheckInPiece checkinPiece = getByPiece(piece);

    piece.setDisplaySummary(checkinPiece.getDisplaySummary());
    piece.setComment(checkinPiece.getComment());

    if (StringUtils.isNotEmpty(checkinPiece.getLocationId())) {
      piece.setLocationId(checkinPiece.getLocationId());
    }
    if (StringUtils.isNotEmpty(checkinPiece.getHoldingId())) {
      piece.setHoldingId(checkinPiece.getHoldingId());
    }
    piece.setEnumeration(checkinPiece.getEnumeration());
    piece.setChronology(checkinPiece.getChronology());
    piece.setCopyNumber(checkinPiece.getCopyNumber());
    piece.setAccessionNumber(checkinPiece.getAccessionNumber());
    piece.setDisplayOnHolding(checkinPiece.getDisplayOnHolding());
    piece.setDiscoverySuppress(checkinPiece.getDiscoverySuppress());
    piece.setSupplement(checkinPiece.getSupplement());
    piece.setBarcode(checkinPiece.getBarcode());
    piece.setReceiptDate(checkinPiece.getReceiptDate());
    piece.setCallNumber(checkinPiece.getCallNumber());
    // Piece record might be received or rolled-back to Expected
    if (isOnOrderPieceStatus(checkinPiece)) {
      piece.setReceivedDate(null);
      piece.setReceivingStatus(Piece.ReceivingStatus.EXPECTED);
    } else {
      piece.setReceivedDate(new Date());
      piece.setReceivingStatus(Piece.ReceivingStatus.RECEIVED);
    }
  }

  @Override
  protected Map<String, List<Piece>> updatePieceRecordsWithoutItems(Map<String, List<Piece>> piecesGroupedByPoLine) {
    extractAllPieces(piecesGroupedByPoLine)
      .filter(piece -> StringUtils.isEmpty(piece.getItemId()))
      .forEach(this::updatePieceWithCheckinInfo);

    return piecesGroupedByPoLine;
  }

  private void updateCheckInPiecesStatus() {
    piecesByLineId.values()
      .stream()
      .map(Map::values)
      .flatMap(Collection::stream)
      .forEach(checkInPiece -> {
        if (CheckInPiece.ItemStatus.ORDER_CLOSED.equals(checkInPiece.getItemStatus())) {
          checkInPiece.setItemStatus(CheckInPiece.ItemStatus.IN_PROCESS);
        }
      });
  }

  private Future<Map<String, List<Piece>>> updateOrderAndPoLinesStatus(Map<String, List<Piece>> piecesGroupedByPoLine,
                                                                       CheckinCollection checkinCollection, RequestContext requestContext) {
    return updateOrderAndPoLinesStatus(
      piecesGroupedByPoLine,
      requestContext,
      poLines -> updateOrderStatus(poLines, checkinCollection, requestContext)
    );
  }

  private void updateOrderStatus(List<PoLine> poLines, CheckinCollection checkinCollection, RequestContext requestContext) {
    if (CollectionUtils.isEmpty(poLines)) {
      logger.info("updateOrderStatus::poLines empty, returning");
      return;
    }
    logger.debug("updateOrderStatus::sending event to verify order status");

    Map<String, Boolean> orderClosedStatusesMap = groupCheckinPiecesByPoLineId(checkinCollection, poLines);
    List<JsonObject> orderClosedStatusesJsonList = orderClosedStatusesMap.entrySet().stream()
      .map(entry -> new JsonObject()
        .put(ORDER_ID, entry.getKey())
        .put(IS_ITEM_ORDER_CLOSED_PRESENT, entry.getValue()))
      .collect(toList());

    sendMessage(MessageAddress.CHECKIN_ORDER_STATUS_UPDATE, new JsonArray(orderClosedStatusesJsonList), requestContext);

    logger.debug("updateOrderStatus::Event to verify order status - sent");
  }

  private Map<String, Boolean> groupCheckinPiecesByPoLineId(CheckinCollection checkinCollection, List<PoLine> poLines) {
    Map<String, Map<String, CheckInPiece>> poLineCheckInPieces = groupCheckinPiecesByPoLineId(checkinCollection);
    Map<String, List<PoLine>> orderIdOrderLineMap = poLines.stream().distinct().collect(groupingBy(PoLine::getPurchaseOrderId));

    Map<String, Boolean> orderClosedStatusesMap = new HashMap<>();
    orderIdOrderLineMap.forEach((orderId, orderPoLines) ->
      orderPoLines.forEach(orderPoLine -> {
        boolean isItemOrderClosedPresent =
          poLineCheckInPieces.get(orderPoLine.getId()).values()
            .stream()
            .anyMatch(piece -> piece.getItemStatus() == CheckInPiece.ItemStatus.ORDER_CLOSED);
        orderClosedStatusesMap.put(orderId, isItemOrderClosedPresent);
      }));
    return orderClosedStatusesMap;
  }

  private Future<Void> checkinItem(JsonObject itemRecord, CheckInPiece checkinPiece, RequestContext locationContext) {

    // Update item record with checkIn details
    itemRecord.put(ITEM_STATUS, new JsonObject().put(ITEM_STATUS_NAME, checkinPiece.getItemStatus().value()));

    if (StringUtils.isNotEmpty(checkinPiece.getDisplaySummary())) {
      itemRecord.put(ITEM_DISPLAY_SUMMARY, checkinPiece.getDisplaySummary());
    }
    if (StringUtils.isNotEmpty(checkinPiece.getEnumeration())) {
      itemRecord.put(ITEM_ENUMERATION, checkinPiece.getEnumeration());
    }
    if (StringUtils.isNotEmpty(checkinPiece.getCopyNumber())) {
      itemRecord.put(COPY_NUMBER, checkinPiece.getCopyNumber());
    }
    if (StringUtils.isNotEmpty(checkinPiece.getChronology())) {
      itemRecord.put(ITEM_CHRONOLOGY, checkinPiece.getChronology());
    }
    if (StringUtils.isNotEmpty(checkinPiece.getBarcode())) {
      itemRecord.put(ITEM_BARCODE, checkinPiece.getBarcode());
    }
    if (StringUtils.isNotEmpty(checkinPiece.getAccessionNumber())) {
      itemRecord.put(ITEM_ACCESSION_NUMBER, checkinPiece.getAccessionNumber());
    }
    if (StringUtils.isNotEmpty(checkinPiece.getCallNumber())) {
      itemRecord.put(ITEM_LEVEL_CALL_NUMBER, checkinPiece.getCallNumber());
    }
    if (checkinPiece.getDiscoverySuppress() != null) {
      itemRecord.put(ITEM_DISCOVERY_SUPPRESS, checkinPiece.getDiscoverySuppress());
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
    return piece.getReceivingStatus() == Piece.ReceivingStatus.RECEIVED && isOnOrderPieceStatus(getByPiece(piece));
  }

  private boolean isOnOrderPieceStatus(CheckInPiece checkinPiece) {
    return checkinPiece.getItemStatus() == CheckInPiece.ItemStatus.ON_ORDER;
  }

}
