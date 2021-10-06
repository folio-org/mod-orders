package org.folio.helper;

import static java.util.stream.Collectors.collectingAndThen;
import static java.util.stream.Collectors.groupingBy;
import static java.util.stream.Collectors.mapping;
import static java.util.stream.Collectors.toList;
import static org.folio.orders.utils.HelperUtils.collectResultsOnSuccess;
import static org.folio.orders.utils.HelperUtils.updatePoLineReceiptStatus;
import static org.folio.rest.core.exceptions.ErrorCodes.ITEM_UPDATE_FAILED;

import java.util.ArrayList;
import java.util.Collection;
import java.util.Collections;
import java.util.Date;
import java.util.HashMap;
import java.util.List;
import java.util.Map;
import java.util.Optional;
import java.util.concurrent.CompletableFuture;
import java.util.concurrent.CompletionStage;

import org.apache.commons.lang3.StringUtils;
import org.apache.commons.lang3.tuple.Pair;
import org.apache.logging.log4j.LogManager;
import org.apache.logging.log4j.Logger;
import org.folio.orders.events.handlers.MessageAddress;
import org.folio.orders.utils.PoLineCommonUtil;
import org.folio.rest.core.models.RequestContext;
import org.folio.rest.jaxrs.model.CheckInPiece;
import org.folio.rest.jaxrs.model.CheckinCollection;
import org.folio.rest.jaxrs.model.CompositePoLine;
import org.folio.rest.jaxrs.model.Location;
import org.folio.rest.jaxrs.model.Piece;
import org.folio.rest.jaxrs.model.PoLine;
import org.folio.rest.jaxrs.model.ProcessingStatus;
import org.folio.rest.jaxrs.model.ReceivingResult;
import org.folio.rest.jaxrs.model.ReceivingResults;
import org.folio.rest.jaxrs.model.ToBeCheckedIn;
import org.folio.service.pieces.flows.create.PieceCreateFlowValidator;

import io.vertx.core.Context;
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
                Context ctx, String lang) {
    super(getHttpClient(okapiHeaders), okapiHeaders, ctx, lang);
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

  public CompletableFuture<ReceivingResults> checkinPieces(CheckinCollection checkinCollection, RequestContext requestContext) {
    return getPoLines(new ArrayList<>(checkinPieces.keySet()), requestContext)
      .thenCompose(poLines -> removeForbiddenEntities(poLines, checkinPieces))
      .thenCompose(vVoid -> processCheckInPieces(checkinCollection, requestContext));
  }

  private CompletionStage<ReceivingResults> processCheckInPieces(CheckinCollection checkinCollection, RequestContext requestContext) {
    Map<String, Map<String, Location>> pieceLocationsGroupedByPoLine = groupLocationsByPoLineIdOnCheckin(checkinCollection);
    //Should be used in next stories MODORDERS-535
    //Map<String, Map<String, String>> pieceHoldingsGroupedByPoLine = groupHoldingsByPoLineIdOnCheckin(checkinCollection);
    // 1. Get piece records from storage
    return createItemsWithPieceUpdate(checkinCollection, requestContext)
      // 2. Filter locationId
      .thenCompose(piecesByPoLineIds -> filterMissingLocations(piecesByPoLineIds, requestContext))
      // 3. Update items in the Inventory if required
      .thenCompose(pieces -> updateInventoryItemsAndHoldings(pieceLocationsGroupedByPoLine, pieces, requestContext))
      // 4. Update piece records with checkIn details which do not have
      // associated item
      .thenApply(this::updatePieceRecordsWithoutItems)
      // 5. Update received piece records in the storage
      .thenCompose(this::storeUpdatedPieceRecords)
      // 6. Update PO Line status
      .thenCompose(piecesGroupedByPoLine -> updateOrderAndPoLinesStatus(piecesGroupedByPoLine, checkinCollection, requestContext))
      // 7. Return results to the client
      .thenApply(piecesGroupedByPoLine -> prepareResponseBody(checkinCollection, piecesGroupedByPoLine));
  }

  private CompletableFuture<Map<String, List<Piece>>> createItemsWithPieceUpdate(CheckinCollection checkinCollection, RequestContext requestContext) {
    Map<String, List<CheckInPiece>> poLineIdVsCheckInPiece = getItemCreateNeededCheckinPieces(checkinCollection);
    List<CompletableFuture<Pair<String, Piece>>> futures = new ArrayList<>();
    return retrievePieceRecords(checkinPieces, requestContext)
      .thenAccept(piecesGroupedByPoLine -> {
        piecesGroupedByPoLine.forEach((poLineId, pieces) -> poLineIdVsCheckInPiece.get(poLineId)
          .forEach(checkInPiece -> {
              pieces.forEach(piece -> {
                if (checkInPiece.getId().equals(piece.getId()) && Boolean.TRUE.equals(checkInPiece.getCreateItem())) {
                  futures.add(purchaseOrderLineService.getOrderLineById(poLineId, requestContext)
                                  .thenApply(PoLineCommonUtil::convertToCompositePoLine)
                                  .thenCompose(compPOL -> handleItem(compPOL, piece, requestContext))
                                  .thenAccept(itemId -> Optional.ofNullable(itemId).ifPresent(piece::withItemId))
                                  .thenApply(aVoid -> Pair.of(poLineId, piece)));
                } else {
                  futures.add(CompletableFuture.completedFuture(Pair.of(poLineId, piece)));
                }
              });
        }));
    })
    .thenCompose(v -> collectResultsOnSuccess(futures).thenApply(poLineIdVsPieceList -> {
      logger.debug("{} pieces updated with item", poLineIdVsPieceList.size());
      return StreamEx.of(poLineIdVsPieceList)
        .distinct()
        .groupingBy(Pair::getKey, mapping(Pair::getValue, collectingAndThen(toList(),
          lists -> StreamEx.of(lists).collect(toList()))));
    }));
  }

  private CompletableFuture<String> handleItem(CompositePoLine compPOL, Piece piece, RequestContext requestContext) {
    if (PieceCreateFlowValidator.isCreateItemForPiecePossible(piece, compPOL)) {
       return pieceCreateFlowInventoryManager.createItemRecord(compPOL, piece.getHoldingId(), requestContext);
    }
    return CompletableFuture.completedFuture(null);
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

  private Map<String, List<CheckInPiece>> getItemCreateNeededCheckinPieces(CheckinCollection checkinCollection) {
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
  protected CompletableFuture<Boolean> receiveInventoryItemAndUpdatePiece(JsonObject item, Piece piece, RequestContext requestContext) {

    CheckInPiece checkinPiece = piecesByLineId.get(piece.getPoLineId())
      .get(piece.getId());
    return inventoryManager
      // Update item records with check-in information and send updates to
      // Inventory
      .checkinItem(item, checkinPiece, requestContext)
      // Update Piece record object with check-in details if item updated
      // successfully
      .thenApply(v -> {
        updatePieceWithCheckinInfo(piece);
        return true;
      })
      // Add processing error if item failed to be updated
      .exceptionally(e -> {
        logger.error("Item associated with piece '{}' cannot be updated", piece.getId());
        addError(piece.getPoLineId(), piece.getId(), ITEM_UPDATE_FAILED.toError());
        return false;
      });
  }

  private void updatePieceWithCheckinInfo(Piece piece) {
    // Get checkinPiece corresponding to piece record
    CheckInPiece checkinPiece = piecesByLineId.get(piece.getPoLineId())
      .get(piece.getId());

    if (StringUtils.isNotEmpty(checkinPiece.getCaption())) {
      piece.setCaption(checkinPiece.getCaption());
    }
    if (StringUtils.isNotEmpty(checkinPiece.getComment())) {
      piece.setComment(checkinPiece.getComment());
    }
    if (StringUtils.isNotEmpty(checkinPiece.getLocationId())) {
      piece.setLocationId(checkinPiece.getLocationId());
    }
    if (StringUtils.isNotEmpty(checkinPiece.getHoldingId())) {
      piece.setHoldingId(checkinPiece.getHoldingId());
    }
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

  private CompletableFuture<Map<String, List<Piece>>> updateOrderAndPoLinesStatus(Map<String, List<Piece>> piecesGroupedByPoLine,
                            CheckinCollection checkinCollection, RequestContext requestContext) {
    List<String> poLineIdsForUpdatedPieces = getPoLineIdsForUpdatedPieces(piecesGroupedByPoLine);
    // Once all PO Lines are retrieved from storage check if receipt status
    // requires update and persist in storage
    return getPoLines(poLineIdsForUpdatedPieces, requestContext).thenCompose(poLines -> {
      // Calculate expected status for each PO Line and update with new one if required
      // Skip status update if PO line status is Ongoing
      List<CompletableFuture<String>> futures = new ArrayList<>();
      for (PoLine poLine : poLines) {
        if (!poLine.getPaymentStatus().equals(PoLine.PaymentStatus.ONGOING)) {
          List<Piece> successfullyProcessedPieces = getSuccessfullyProcessedPieces(poLine.getId(), piecesGroupedByPoLine);
          futures.add(calculatePoLineReceiptStatus(poLine, successfullyProcessedPieces)
            .thenCompose(status -> updatePoLineReceiptStatus(poLine, status, httpClient, okapiHeaders, logger)));
        }
      }

      return collectResultsOnSuccess(futures).thenAccept(updatedPoLines -> {
        logger.debug("{} out of {} PO Line(s) updated with new status", updatedPoLines.size(), piecesGroupedByPoLine.size());

        // Send event to check order status for successfully processed PO Lines
        List<PoLine> successPoLines = StreamEx.of(poLines)
          .filter(line -> updatedPoLines.contains(line.getId()))
          .toList();
        updateOrderStatus(successPoLines, checkinCollection);
      });
    })
      .thenApply(ok -> piecesGroupedByPoLine);
  }

  private void updateOrderStatus(List<PoLine> poLines, CheckinCollection checkinCollection) {
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
      sendEvent(MessageAddress.CHECKIN_ORDER_STATUS_UPDATE, messageContent);
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

  @Override
  protected String getLocationId(Piece piece) {
    return checkinPieces.get(piece.getPoLineId()).get(piece.getId()).getLocationId();
  }

  @Override
  protected String getHoldingId(Piece piece) {
    return checkinPieces.get(piece.getPoLineId()).get(piece.getId()).getHoldingId();
  }
}
