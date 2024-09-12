package org.folio.helper;

import io.vertx.core.Context;
import io.vertx.core.Future;
import io.vertx.core.Promise;
import io.vertx.core.json.JsonArray;
import io.vertx.core.json.JsonObject;
import java.util.Objects;
import one.util.streamex.StreamEx;
import org.apache.commons.collections4.CollectionUtils;
import org.apache.commons.lang3.StringUtils;
import org.apache.commons.lang3.tuple.Pair;
import org.folio.models.pieces.PiecesHolder;
import org.folio.orders.events.handlers.MessageAddress;
import org.folio.rest.core.models.RequestContext;
import org.folio.rest.jaxrs.model.CheckInPiece;
import org.folio.rest.jaxrs.model.CheckinCollection;
import org.folio.rest.jaxrs.model.Piece;
import org.folio.rest.jaxrs.model.PoLine;
import org.folio.rest.jaxrs.model.ProcessingStatus;
import org.folio.rest.jaxrs.model.ReceivingResult;
import org.folio.rest.jaxrs.model.ReceivingResults;
import org.folio.rest.jaxrs.model.ToBeCheckedIn;
import org.folio.service.inventory.InventoryUtils;

import java.util.ArrayList;
import java.util.Collection;
import java.util.Date;
import java.util.HashMap;
import java.util.List;
import java.util.Map;

import static java.util.stream.Collectors.collectingAndThen;
import static java.util.stream.Collectors.groupingBy;
import static java.util.stream.Collectors.mapping;
import static java.util.stream.Collectors.toList;
import static org.folio.orders.utils.HelperUtils.collectResultsOnSuccess;

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
      .compose(voidResult -> processCheckInPieces(checkinCollection, requestContext));
  }

  private Future<ReceivingResults> processCheckInPieces(CheckinCollection checkinCollection, RequestContext requestContext) {
    PiecesHolder holder = new PiecesHolder();
    // 1. Get purchase order and poLine from storage
    return findAndSetPurchaseOrderPoLinePair(extractPoLineId(checkinCollection), holder, requestContext)
      // 2. Get piece records from storage
      .compose(voidResult -> createItemsWithPieceUpdate(checkinCollection, holder, requestContext))
      // 3. Filter locationId
      .compose(piecesByPoLineIds -> filterMissingLocations(piecesByPoLineIds, requestContext))
      // 4. Update items in the Inventory if required
      .compose(pieces -> updateInventoryItemsAndHoldings(pieces, holder, requestContext))
      // 5. Update piece records with checkIn details which do not have
      // associated item
      .map(this::updatePieceRecordsWithoutItems)
      // 6. Update received piece records in the storage
      .compose(piecesGroupedByPoLine -> storeUpdatedPieceRecords(piecesGroupedByPoLine, requestContext))
      // 7. Update PO Line status
      .compose(piecesGroupedByPoLine -> updateOrderAndPoLinesStatus(holder.getPiecesFromStorage(), piecesGroupedByPoLine, checkinCollection, requestContext))
      // 8. Return results to the client
      .map(piecesGroupedByPoLine -> prepareResponseBody(checkinCollection, piecesGroupedByPoLine));
  }

  private Future<Map<String, List<Piece>>> createItemsWithPieceUpdate(CheckinCollection checkinCollection, PiecesHolder holder, RequestContext requestContext) {
    var poLineIdCheckInPieceMap = getItemCreateNeededCheckinPieces(checkinCollection);
    var pieceFutures = new ArrayList<Future<PiecesHolder.PiecePoLineDto>>();
    return retrievePieceRecords(requestContext)
      .map(piecesFromStorage -> {
        holder.withPiecesFromStorage(piecesFromStorage);
        piecesFromStorage.forEach((poLineId, pieces) -> poLineIdCheckInPieceMap.get(poLineId)
          .forEach(checkInPiece -> pieces.forEach(piece -> {
            var srcTenantId = piece.getReceivingTenantId();
            var dstTenantId = checkInPiece.getReceivingTenantId();
            var purchaseOrder = holder.getPurchaseOrderPoLinePair().getKey();
            var poLine = holder.getPurchaseOrderPoLinePair().getValue();
            if (checkInPiece.getId().equals(piece.getId()) && Boolean.TRUE.equals(checkInPiece.getCreateItem())) {
              pieceFutures.add(pieceCreateFlowInventoryManager.processInventory(purchaseOrder, poLine, piece, checkInPiece.getCreateItem(), requestContext)
                .map(voidResult -> new PiecesHolder.PiecePoLineDto(poLineId, piece)));
            } else if (checkInPiece.getId().equals(piece.getId()) && InventoryUtils.allowItemRecreate(srcTenantId, dstTenantId) && Objects.nonNull(piece.getItemId())) {
              pieceFutures.add(Future.succeededFuture(new PiecesHolder.PiecePoLineDto(poLine, piece, checkInPiece)));
            } else {
              pieceFutures.add(Future.succeededFuture(new PiecesHolder.PiecePoLineDto(poLineId, piece)));
            }
          })));
        return null;
      })
      .compose(v -> collectResultsOnSuccess(pieceFutures)
        .map(piecePoLineDtoList -> {
          prepareItemsToRecreate(holder, piecePoLineDtoList);
          return createPoLineIdPiecePair(piecePoLineDtoList);
        }));
  }

  private void prepareItemsToRecreate(PiecesHolder holder, List<PiecesHolder.PiecePoLineDto> piecePoLineDtoList) {
    holder.withItemsToRecreate(piecePoLineDtoList.stream()
      .filter(PiecesHolder.PiecePoLineDto::isRecreateItem)
      .collect(groupingBy(PiecesHolder.PiecePoLineDto::getPoLineId)));
  }

  private static Map<String, List<Piece>> createPoLineIdPiecePair(List<PiecesHolder.PiecePoLineDto> piecePoLineDtoList) {
    return StreamEx.of(piecePoLineDtoList)
      .map(dto -> Pair.of(dto.getPoLineId(), dto.getPieceFromStorage())).distinct()
      .groupingBy(Pair::getKey, mapping(Pair::getValue, collectingAndThen(toList(), lists -> StreamEx.of(lists).toList())));
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

      Map<ProcessingStatus.Type, Integer> resultCounts = getEmptyResultCounts();
      for (CheckInPiece checkinPiece : toBeCheckedIn.getCheckInPieces()) {
        String pieceId = checkinPiece.getId();

        calculateProcessingErrors(poLineId, result, processedPiecesForPoLine, resultCounts, pieceId);
      }

      result.withPoLineId(poLineId)
        .withProcessedSuccessfully(resultCounts.get(ProcessingStatus.Type.SUCCESS))
        .withProcessedWithError(resultCounts.get(ProcessingStatus.Type.FAILURE));
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
  protected Future<Boolean> receiveInventoryItemAndUpdatePiece(PiecesHolder holder, JsonObject item, Piece piece, RequestContext locationContext) {
    Promise<Boolean> promise = Promise.promise();
    CheckInPiece checkinPiece = getByPiece(piece);
    InventoryUtils.updateItemWithCheckinPieceFields(item, checkinPiece);
    inventoryItemManager.updateItem(item, locationContext)
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

    if (StringUtils.isNotEmpty(checkinPiece.getReceivingTenantId())) {
      piece.setReceivingTenantId(checkinPiece.getReceivingTenantId());
    }
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

  private Future<Map<String, List<Piece>>> updateOrderAndPoLinesStatus(Map<String, List<Piece>> piecesFromStorage,
                                                                       Map<String, List<Piece>> piecesGroupedByPoLine,
                                                                       CheckinCollection checkinCollection, RequestContext requestContext) {
    return updateOrderAndPoLinesStatus(
      piecesFromStorage,
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

  @Override
  protected String getLocationId(Piece piece) {
    return getByPiece(piece).getLocationId();
  }

  @Override
  protected String getHoldingId(Piece piece) {
    return getByPiece(piece).getHoldingId();
  }

  @Override
  protected String getReceivingTenantId(Piece piece) {
    return getByPiece(piece).getReceivingTenantId();
  }

  @Override
  protected boolean isRevertToOnOrder(Piece piece) {
    return piece.getReceivingStatus() == Piece.ReceivingStatus.RECEIVED && isOnOrderPieceStatus(getByPiece(piece));
  }

  private boolean isOnOrderPieceStatus(CheckInPiece checkinPiece) {
    return checkinPiece.getItemStatus() == CheckInPiece.ItemStatus.ON_ORDER;
  }

}
