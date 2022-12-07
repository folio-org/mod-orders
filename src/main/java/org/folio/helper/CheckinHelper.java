package org.folio.helper;

import static java.util.stream.Collectors.collectingAndThen;
import static java.util.stream.Collectors.groupingBy;
import static java.util.stream.Collectors.mapping;
import static java.util.stream.Collectors.toList;
import static org.folio.orders.utils.HelperUtils.collectResultsOnSuccess;
import static org.folio.rest.core.exceptions.ErrorCodes.ITEM_UPDATE_FAILED;
import static org.folio.service.inventory.InventoryManager.ITEM_BARCODE;
import static org.folio.service.inventory.InventoryManager.ITEM_CHRONOLOGY;
import static org.folio.service.inventory.InventoryManager.ITEM_DISCOVERY_SUPPRESS;
import static org.folio.service.inventory.InventoryManager.ITEM_ENUMERATION;
import static org.folio.service.inventory.InventoryManager.ITEM_LEVEL_CALL_NUMBER;
import static org.folio.service.inventory.InventoryManager.ITEM_STATUS;
import static org.folio.service.inventory.InventoryManager.ITEM_STATUS_NAME;

import java.util.ArrayList;
import java.util.Collection;
import java.util.Collections;
import java.util.Date;
import java.util.HashMap;
import java.util.List;
import java.util.Map;
import java.util.Optional;

import org.apache.commons.lang3.StringUtils;
import org.apache.commons.lang3.tuple.Pair;
import org.apache.logging.log4j.LogManager;
import org.apache.logging.log4j.Logger;
import org.folio.orders.events.handlers.MessageAddress;
import org.folio.orders.utils.HelperUtils;
import org.folio.orders.utils.PoLineCommonUtil;
import org.folio.rest.core.models.RequestContext;
import org.folio.rest.jaxrs.model.CheckInPiece;
import org.folio.rest.jaxrs.model.CheckinCollection;
import org.folio.rest.jaxrs.model.Location;
import org.folio.rest.jaxrs.model.Piece;
import org.folio.rest.jaxrs.model.PoLine;
import org.folio.rest.jaxrs.model.ProcessingStatus;
import org.folio.rest.jaxrs.model.ReceivingResult;
import org.folio.rest.jaxrs.model.ReceivingResults;
import org.folio.rest.jaxrs.model.ToBeCheckedIn;

import io.vertx.core.Context;
import io.vertx.core.Future;
import io.vertx.core.Promise;
import io.vertx.core.json.JsonArray;
import io.vertx.core.json.JsonObject;
import one.util.streamex.StreamEx;

public class CheckinHelper extends CheckinReceivePiecesHelper<CheckInPiece> {
  private static final Logger logger = LogManager.getLogger(CheckinHelper.class);
  public static final String IS_ITEM_ORDER_CLOSED_PRESENT = "isItemOrderClosedPresent";
  /**
   * Map with PO line id as a key and value is map with piece id as a key and
   * {@link CheckInPiece} as a value
   */
  private final Map<String, Map<String, CheckInPiece>> checkinPieces;

  public CheckinHelper(CheckinCollection checkinCollection, Map<String, String> okapiHeaders,
                Context ctx) {
    super(okapiHeaders, ctx);
    // Convert request to map representation
    CheckinCollection checkinCollectionClone = JsonObject.mapFrom(checkinCollection).mapTo(CheckinCollection.class);
    checkinPieces = groupCheckinPiecesByPoLineId(checkinCollectionClone);
    updateCheckInPiecesStatus();
    // Logging quantity of the piece records to be checked in
    if (logger.isDebugEnabled()) {
      int poLinesQty = checkinPieces.size();
      int piecesQty = StreamEx.ofValues(checkinPieces)
        .mapToInt(Map::size)
        .sum();
      logger.debug("{} piece record(s) are going to be checkedIn for {} PO line(s)", piecesQty, poLinesQty);
    }
  }

  public Future<ReceivingResults> checkinPieces(CheckinCollection checkinCollection, RequestContext requestContext) {
    return getPoLines(new ArrayList<>(checkinPieces.keySet()), requestContext)
      .compose(poLines -> removeForbiddenEntities(poLines, checkinPieces, requestContext))
      .compose(vVoid -> processCheckInPieces(checkinCollection, requestContext));
  }

  private Future<ReceivingResults> processCheckInPieces(CheckinCollection checkinCollection, RequestContext requestContext) {
    Map<String, Map<String, Location>> pieceLocationsGroupedByPoLine = groupLocationsByPoLineIdOnCheckin(checkinCollection);
     // 1. Get piece records from storage
    return createItemsWithPieceUpdate(checkinCollection, requestContext)
      // 2. Filter locationId
      .compose(piecesByPoLineIds -> filterMissingLocations(piecesByPoLineIds, requestContext))
      // 3. Update items in the Inventory if required
      .compose(pieces -> updateInventoryItemsAndHoldings(pieceLocationsGroupedByPoLine, pieces, requestContext))
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
    return retrievePieceRecords(checkinPieces, requestContext)
      .map(piecesGroupedByPoLine -> { piecesGroupedByPoLine.forEach((poLineId, pieces) -> poLineIdVsCheckInPiece.get(poLineId)
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
        return null;})
      .compose(v -> collectResultsOnSuccess(futures).map(poLineIdVsPieceList -> StreamEx.of(poLineIdVsPieceList)
        .distinct()
        .groupingBy(Pair::getKey, mapping(Pair::getValue, collectingAndThen(toList(), lists -> StreamEx.of(lists)
          .collect(toList()))))));
  }

  private Map<String, Map<String, Location>> groupLocationsByPoLineIdOnCheckin(CheckinCollection checkinCollection) {
    return StreamEx
      .of(checkinCollection.getToBeCheckedIn())
      .distinct()
      .groupingBy(ToBeCheckedIn::getPoLineId,
        mapping(ToBeCheckedIn::getCheckInPieces,
          collectingAndThen(toList(),
            lists -> StreamEx.of(lists)
              .flatMap(List::stream)
              .toMap(CheckInPiece::getId, checkInPiece  ->
                new Location().withHoldingId(checkInPiece.getHoldingId()).withLocationId(checkInPiece.getLocationId())
              )
          )
        )
      );
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
  protected boolean isRevertToOnOrder(Piece piece) {
    return piece.getReceivingStatus() == Piece.ReceivingStatus.RECEIVED
      && inventoryManager.isOnOrderPieceStatus(piecesByLineId.get(piece.getPoLineId()).get(piece.getId()));
  }

  @Override
  protected Future<Boolean> receiveInventoryItemAndUpdatePiece(JsonObject item, Piece piece, RequestContext requestContext) {
    Promise<Boolean> promise = Promise.promise();
    CheckInPiece checkinPiece = piecesByLineId.get(piece.getPoLineId())
      .get(piece.getId());
    checkinItem(item, checkinPiece, requestContext)
      // Update Piece record object with check-in details if item updated
      // successfully
      .map(v -> {
        updatePieceWithCheckinInfo(piece);
        promise.complete(true);
        return true;
      })
      // Add processing error if item failed to be updated
       .onFailure(e -> {
        logger.error("Item associated with piece '{}' cannot be updated", piece.getId());
        addError(piece.getPoLineId(), piece.getId(), ITEM_UPDATE_FAILED.toError());
         promise.complete(false);
      });
    return promise.future();
  }

  private void updatePieceWithCheckinInfo(Piece piece) {
    // Get checkinPiece corresponding to piece record
    CheckInPiece checkinPiece = piecesByLineId.get(piece.getPoLineId())
      .get(piece.getId());

    piece.setCaption(checkinPiece.getCaption());
    piece.setComment(checkinPiece.getComment());

    if (StringUtils.isNotEmpty(checkinPiece.getLocationId())) {
      piece.setLocationId(checkinPiece.getLocationId());
    }
    if (StringUtils.isNotEmpty(checkinPiece.getHoldingId())) {
      piece.setHoldingId(checkinPiece.getHoldingId());
    }
    piece.setEnumeration(checkinPiece.getEnumeration());
    piece.setChronology(checkinPiece.getChronology());
    piece.setDisplayOnHolding(checkinPiece.getDisplayOnHolding());
    piece.setDiscoverySuppress(checkinPiece.getDiscoverySuppress());
    piece.setSupplement(checkinPiece.getSupplement());
    // Piece record might be received or rolled-back to Expected
    if (inventoryManager.isOnOrderPieceStatus(checkinPiece)) {
      piece.setReceivedDate(null);
      piece.setReceivingStatus(Piece.ReceivingStatus.EXPECTED);
    } else {
      piece.setReceivedDate(new Date());
      piece.setReceivingStatus(Piece.ReceivingStatus.RECEIVED);
    }
  }

  @Override
  protected Map<String, List<Piece>> updatePieceRecordsWithoutItems(Map<String, List<Piece>> piecesGroupedByPoLine) {
    StreamEx.ofValues(piecesGroupedByPoLine)
      .flatMap(List::stream)
      .filter(piece -> StringUtils.isEmpty(piece.getItemId()))
      .forEach(this::updatePieceWithCheckinInfo);

    return piecesGroupedByPoLine;
  }

  private void updateCheckInPiecesStatus() {
    checkinPieces.values()
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
        List<PoLine> successPoLines = StreamEx.of(poLines)
          .filter(line -> updatedPoLines.contains(line.getId()))
          .toList();
        updateOrderStatus(successPoLines, checkinCollection, requestContext);
        return null;
      });
    })
      .map(ok -> piecesGroupedByPoLine);
  }

  private void updateOrderStatus(List<PoLine> poLines, CheckinCollection checkinCollection, RequestContext requestContext) {
    if (!poLines.isEmpty()) {
      logger.debug("Sending event to verify order status");

      Map<String, Boolean> orderClosedStatusesMap = groupCheckinPiecesByPoLineId(checkinCollection, poLines);
      List<JsonObject> orderClosedStatusesJsonList = orderClosedStatusesMap.entrySet().stream()
        .map(entry -> new JsonObject().put(ORDER_ID, entry.getKey())
                                      .put(IS_ITEM_ORDER_CLOSED_PRESENT, entry.getValue()))
        .collect(toList());

      JsonObject messageContent = new JsonObject();
      messageContent.put(OKAPI_HEADERS, okapiHeaders);
      messageContent.put(EVENT_PAYLOAD, new JsonArray(orderClosedStatusesJsonList));
      HelperUtils.sendEvent(MessageAddress.CHECKIN_ORDER_STATUS_UPDATE, messageContent, requestContext);
      logger.debug("Event to verify order status - sent");
    }
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
                           .filter(checkinPiece -> CheckInPiece.ItemStatus.ORDER_CLOSED.equals(checkinPiece.getItemStatus()))
                           .map(CheckInPiece::getItemStatus)
                           .findAny()
                           .isPresent();
      orderClosedStatusesMap.put(orderId, isItemOrderClosedPresent);
    }));
    return orderClosedStatusesMap;
  }

  private Future<Void> checkinItem(JsonObject itemRecord, CheckInPiece checkinPiece, RequestContext requestContext) {

    // Update item record with checkIn details
    itemRecord.put(ITEM_STATUS, new JsonObject().put(ITEM_STATUS_NAME, checkinPiece.getItemStatus().value()));
    if (StringUtils.isNotEmpty(checkinPiece.getBarcode())) {
      itemRecord.put(ITEM_BARCODE, checkinPiece.getBarcode());
    }
    if (StringUtils.isNotEmpty(checkinPiece.getCallNumber())) {
      itemRecord.put(ITEM_LEVEL_CALL_NUMBER, checkinPiece.getCallNumber());
    }
    Optional.ofNullable(checkinPiece.getEnumeration())
      .ifPresentOrElse(enumeration -> itemRecord.put(ITEM_ENUMERATION, enumeration), () -> itemRecord.remove(ITEM_ENUMERATION));
    Optional.ofNullable(checkinPiece.getChronology())
      .ifPresentOrElse(chronology -> itemRecord.put(ITEM_CHRONOLOGY, chronology), () -> itemRecord.remove(ITEM_CHRONOLOGY));
    Optional.ofNullable(checkinPiece.getDiscoverySuppress())
      .ifPresentOrElse(discSup -> itemRecord.put(ITEM_DISCOVERY_SUPPRESS, discSup), () -> itemRecord.remove(ITEM_DISCOVERY_SUPPRESS));

    return inventoryManager.updateItem(itemRecord, requestContext);
  }

  @Override
  protected String getLocationId(Piece piece) {
    return checkinPieces.get(piece.getPoLineId()).get(piece.getId()).getLocationId();
  }

  @Override
  protected String getHoldingId(Piece piece) {
    return checkinPieces.get(piece.getPoLineId()).get(piece.getId()).getHoldingId();
  }
}
