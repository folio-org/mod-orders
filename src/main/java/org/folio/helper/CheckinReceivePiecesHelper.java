package org.folio.helper;

import io.vertx.core.Context;
import io.vertx.core.Future;
import io.vertx.core.json.JsonObject;
import one.util.streamex.EntryStream;
import one.util.streamex.StreamEx;
import org.apache.commons.collections4.CollectionUtils;
import org.apache.commons.collections4.MapUtils;
import org.apache.commons.lang3.StringUtils;
import org.apache.commons.lang3.tuple.Pair;
import org.folio.models.PoLineLocationsPair;
import org.folio.models.pieces.PieceUpdateHolder;
import org.folio.models.pieces.PiecesHolder;
import org.folio.okapi.common.GenericCompositeFuture;
import org.folio.orders.events.handlers.MessageAddress;
import org.folio.orders.utils.HelperUtils;
import org.folio.orders.utils.PoLineCommonUtil;
import org.folio.orders.utils.ProtectedOperationType;
import org.folio.orders.utils.RequestContextUtil;
import org.folio.rest.core.RestClient;
import org.folio.rest.core.models.RequestContext;
import org.folio.rest.jaxrs.model.CheckinCollection;
import org.folio.rest.jaxrs.model.PoLine;
import org.folio.rest.jaxrs.model.CompositePurchaseOrder;
import org.folio.rest.jaxrs.model.Eresource;
import org.folio.rest.jaxrs.model.Error;
import org.folio.rest.jaxrs.model.Location;
import org.folio.rest.jaxrs.model.Parameter;
import org.folio.rest.jaxrs.model.Physical;
import org.folio.rest.jaxrs.model.Piece;
import org.folio.rest.jaxrs.model.Piece.ReceivingStatus;
import org.folio.rest.jaxrs.model.PieceCollection;
import org.folio.rest.jaxrs.model.PoLine.ReceiptStatus;
import org.folio.rest.jaxrs.model.ProcessingStatus;
import org.folio.rest.jaxrs.model.ReceivingCollection;
import org.folio.rest.jaxrs.model.ReceivingItemResult;
import org.folio.rest.jaxrs.model.ReceivingResult;
import org.folio.rest.jaxrs.model.Title;
import org.folio.rest.jaxrs.model.ToBeCheckedIn;
import org.folio.rest.jaxrs.model.ToBeReceived;
import org.folio.rest.tools.utils.TenantTool;
import org.folio.service.ProtectionService;
import org.folio.service.inventory.InventoryHoldingManager;
import org.folio.service.inventory.InventoryInstanceManager;
import org.folio.service.inventory.InventoryItemManager;
import org.folio.service.inventory.InventoryUtils;
import org.folio.service.orders.PurchaseOrderLineService;
import org.folio.service.orders.PurchaseOrderStorageService;
import org.folio.service.pieces.ItemRecreateInventoryService;
import org.folio.service.pieces.PieceStorageService;
import org.folio.service.pieces.PieceUtil;
import org.folio.service.pieces.flows.create.PieceCreateFlowInventoryManager;
import org.folio.service.pieces.flows.update.PieceUpdateFlowPoLineService;
import org.folio.service.titles.TitlesService;
import org.springframework.beans.factory.annotation.Autowired;

import java.util.ArrayList;
import java.util.Collection;
import java.util.Collections;
import java.util.Date;
import java.util.HashMap;
import java.util.HashSet;
import java.util.List;
import java.util.Map;
import java.util.Objects;
import java.util.Optional;
import java.util.Set;
import java.util.function.Consumer;
import java.util.function.Function;
import java.util.stream.Collectors;

import static java.util.stream.Collectors.mapping;
import static java.util.stream.Collectors.toList;
import static org.apache.commons.lang3.StringUtils.EMPTY;
import static org.folio.orders.utils.HelperUtils.collectResultsOnSuccess;
import static org.folio.orders.utils.ResourcePathResolver.PIECES_STORAGE;
import static org.folio.orders.utils.ResourcePathResolver.resourceByIdPath;
import static org.folio.rest.RestConstants.MAX_IDS_FOR_GET_RQ_15;
import static org.folio.rest.core.exceptions.ErrorCodes.BARCODE_IS_NOT_UNIQUE;
import static org.folio.rest.core.exceptions.ErrorCodes.ITEM_NOT_RETRIEVED;
import static org.folio.rest.core.exceptions.ErrorCodes.ITEM_UPDATE_FAILED;
import static org.folio.rest.core.exceptions.ErrorCodes.LOC_NOT_PROVIDED;
import static org.folio.rest.core.exceptions.ErrorCodes.PIECE_ALREADY_RECEIVED;
import static org.folio.rest.core.exceptions.ErrorCodes.PIECE_NOT_FOUND;
import static org.folio.rest.core.exceptions.ErrorCodes.PIECE_NOT_RETRIEVED;
import static org.folio.rest.core.exceptions.ErrorCodes.PIECE_POL_MISMATCH;
import static org.folio.rest.core.exceptions.ErrorCodes.PIECE_UPDATE_FAILED;
import static org.folio.rest.core.exceptions.ErrorCodes.USER_HAS_NO_PERMISSIONS;
import static org.folio.rest.jaxrs.model.Piece.ReceivingStatus.CLAIM_DELAYED;
import static org.folio.rest.jaxrs.model.Piece.ReceivingStatus.CLAIM_SENT;
import static org.folio.rest.jaxrs.model.Piece.ReceivingStatus.EXPECTED;
import static org.folio.rest.jaxrs.model.Piece.ReceivingStatus.LATE;
import static org.folio.rest.jaxrs.model.Piece.ReceivingStatus.RECEIVED;
import static org.folio.rest.jaxrs.model.Piece.ReceivingStatus.UNRECEIVABLE;
import static org.folio.rest.jaxrs.model.PoLine.ReceiptStatus.AWAITING_RECEIPT;
import static org.folio.rest.jaxrs.model.PoLine.ReceiptStatus.FULLY_RECEIVED;
import static org.folio.rest.jaxrs.model.PoLine.ReceiptStatus.PARTIALLY_RECEIVED;
import static org.folio.service.inventory.InventoryItemManager.ITEM_HOLDINGS_RECORD_ID;

public abstract class CheckinReceivePiecesHelper<T> extends BaseHelper {

  public static final List<ReceivingStatus> RECEIVED_STATUSES = List.of(RECEIVED, UNRECEIVABLE);
  public static final List<ReceivingStatus> EXPECTED_STATUSES = List.of(EXPECTED, CLAIM_DELAYED, CLAIM_SENT, LATE);
  private static final String BARCODE_NOT_UNIQUE_MESSAGE = "Barcode must be unique";
  protected Map<String, Map<String, T>> piecesByLineId;
  private final Map<String, Map<String, Error>> processingErrors;
  private final Set<String> processedHoldingsParams;
  private final Map<String, String> processedHoldings;
  @Autowired
  private ProtectionService protectionService;
  @Autowired
  protected TitlesService titlesService;
  @Autowired
  protected InventoryHoldingManager inventoryHoldingManager;
  @Autowired
  protected InventoryItemManager inventoryItemManager;
  @Autowired
  protected InventoryInstanceManager inventoryInstanceManager;
  @Autowired
  protected PieceStorageService pieceStorageService;
  @Autowired
  protected PurchaseOrderLineService purchaseOrderLineService;
  @Autowired
  protected PieceCreateFlowInventoryManager pieceCreateFlowInventoryManager;
  @Autowired
  protected PieceUpdateFlowPoLineService pieceUpdateFlowPoLineService;
  @Autowired
  private ItemRecreateInventoryService itemRecreateInventoryService;
  @Autowired
  protected PurchaseOrderStorageService purchaseOrderStorageService;

  private final RestClient restClient;

  private List<PoLine> poLineList;


  protected CheckinReceivePiecesHelper(Map<String, String> okapiHeaders, Context ctx) {
    super(okapiHeaders, ctx);
    this.restClient = new RestClient();
    processedHoldingsParams = new HashSet<>();
    processedHoldings = new HashMap<>();
    processingErrors = new HashMap<>();
  }

  public static String extractPoLineId(Object collection) {
    Optional<String> poLineOptional;
    if (collection instanceof CheckinCollection checkinCollection) {
      poLineOptional = checkinCollection.getToBeCheckedIn().stream().map(ToBeCheckedIn::getPoLineId).findFirst();
    } else if (collection instanceof ReceivingCollection receivingCollection)  {
      poLineOptional = receivingCollection.getToBeReceived().stream().map(ToBeReceived::getPoLineId).findFirst();
    } else {
      throw new IllegalStateException("Unsupported checkIn or receiving model");
    }
    return poLineOptional.orElseThrow(() -> new IllegalStateException("PoLineId cannot be extracted from a checkIn/receiving model"));
  }

  /**
   * Find and set the associated purchase order and its order line by order line id
   * @param poLineId Order line id from the checkIn/receiving model
   * @param holder PieceHolder, the destination where this pair will be saved
   * @param requestContext The request context that holds the headers needed proper query resolution
   * @return A pair consistigng of a purchase order and its order line
   */
  public Future<Void> findAndSetPurchaseOrderPoLinePair(String poLineId, PiecesHolder holder, RequestContext requestContext) {
    return purchaseOrderLineService.getOrderLineById(poLineId, requestContext)
      .compose(poLine -> purchaseOrderStorageService.getPurchaseOrderByIdAsJson(poLine.getPurchaseOrderId(), requestContext)
        .map(HelperUtils::convertToCompositePurchaseOrder)
        .map(purchaseOrder -> {
          logger.info("findAndSetPurchaseOrderPoLinePair:: Found purchase order & poLine, order id: {}, poLineId: {}",
            purchaseOrder.getId(), poLine.getId());
          holder.withPurchaseOrderPoLinePair(Pair.of(purchaseOrder, poLine));
          return null;
        }));
  }

  //-------------------------------------------------------------------------------------
  /*
  retrievePieceRecords
   */

  public Future<Map<String, List<Piece>>> retrievePieceRecords(RequestContext requestContext) {
    Map<String, List<Piece>> piecesByPoLine = new HashMap<>();
    // Split all piece id's by maximum number of id's for get query
    List<Future<Void>> futures = StreamEx
      .ofSubLists(getPieceIds(), MAX_IDS_FOR_GET_RQ_15)
      // Send get request for each CQL query
      .map(ids -> getPiecesByIds(ids, piecesByPoLine, requestContext))
      .collect(Collectors.toList());

    // Wait for all pieces to be retrieved and complete resulting future
    return GenericCompositeFuture.join(futures)
      .map(v -> {
        if (logger.isDebugEnabled()) {
          int poLinesQty = piecesByPoLine.size();
          int piecesQty = StreamEx.ofValues(piecesByPoLine)
            .mapToInt(List::size)
            .sum();
          logger.debug("{} piece record(s) retrieved from storage for {} PO line(s)", piecesQty, poLinesQty);
        }
        return piecesByPoLine;
      });
  }

  private Future<Void> getPiecesByIds(List<String> ids, Map<String, List<Piece>> piecesByPoLine, RequestContext requestContext) {
    return pieceStorageService.getPiecesByIds(ids, requestContext)
      .map(pieces -> {
        pieces.forEach(piece -> addPieceIfValid(piece, piecesByPoLine));
        checkIfAllPiecesFound(ids, pieces);
        return null;
      })
      .otherwise(e -> {
        logger.error("Error fetching piece records", e);
        ids.forEach(pieceId -> addError(getPoLineIdByPieceId(pieceId), pieceId, PIECE_NOT_RETRIEVED.toError()));
        return null;
      })
      .mapEmpty();
  }

  /**
   * Validates if the piece corresponds to PO Line specified in the request and
   * can be received/checked-in. If all checks pass, adds to map
   *
   * @param piece          {@link Piece} piece to validate and add to map
   * @param piecesByPoLine map with PO line id as a key and list of corresponding pieces as a
   *                       value
   */
  private void addPieceIfValid(Piece piece, Map<String, List<Piece>> piecesByPoLine) {
    String poLineId = piece.getPoLineId();
    String pieceId = piece.getId();

    // Validate if the piece actually corresponds to PO line specified in the
    // request
    if (!piecesByLineId.containsKey(poLineId) || !piecesByLineId.get(poLineId).containsKey(pieceId)) {
      addError(getPoLineIdByPieceId(pieceId), pieceId, PIECE_POL_MISMATCH.toError());
      return;
    }
    // Validate if the piece is not yet received
    if (piece.getReceivingStatus() == ReceivingStatus.RECEIVED && !isRevertToOnOrder(piece)) {
      addError(poLineId, pieceId, PIECE_ALREADY_RECEIVED.toError());
      return;
    }
    piecesByPoLine.computeIfAbsent(poLineId, v -> new ArrayList<>()).add(piece);
  }

  /**
   * Checks if all expected piece records found in the storage. If any are
   * missing, adds corresponding error
   *
   * @param pieceIds list of expected piece id's
   * @param pieces   found pieces
   */
  private void checkIfAllPiecesFound(List<String> pieceIds, List<Piece> pieces) {
    // Handle the case when for some reason some pieces are not found
    if (pieces.size() < pieceIds.size()) {
      List<String> foundPieces = StreamEx.of(pieces).map(Piece::getId).toList();

      pieceIds.stream()
        .filter(pieceId -> !foundPieces.contains(pieceId))
        .forEach(pieceId -> addError(getPoLineIdByPieceId(pieceId), pieceId, PIECE_NOT_FOUND.toError()));
    }
  }

  /**
   * Stores updated piece records with receiving details into storage.
   *
   * @param piecesGroupedByPoLine map with PO line id as key and list of corresponding pieces as
   *                              value
   * @return map passed as a parameter
   */
  protected Future<Map<String, List<Piece>>> storeUpdatedPieceRecords(Map<String, List<Piece>> piecesGroupedByPoLine, RequestContext requestContext) {
    return GenericCompositeFuture.join(
        extractAllPieces(piecesGroupedByPoLine)
          .filter(this::isSuccessfullyProcessedPiece)
          .map(piece -> piece.withStatusUpdatedDate(new Date()))
          .map(piece -> storeUpdatedPieceRecord(piece, requestContext))
          .toList())
      .map(v -> piecesGroupedByPoLine);
  }

  /**
   * Sends request to update piece record with receiving/check-in details in the storage.
   * In case of an error updating the piece, this is collected to return in
   * the response to client
   *
   * @param piece {@link Piece} with receiving information
   */
  private Future<Void> storeUpdatedPieceRecord(Piece piece, RequestContext requestContext) {
    String pieceId = piece.getId();
    return restClient.put(resourceByIdPath(PIECES_STORAGE, pieceId), JsonObject.mapFrom(piece), requestContext)
      .otherwise(e -> {
        addError(getPoLineIdByPieceId(pieceId), pieceId, PIECE_UPDATE_FAILED.toError());
        return null;
      });
  }

  private String getPoLineIdByPieceId(String pieceId) {
    return StreamEx
      .ofKeys(piecesByLineId, values -> values.containsKey(pieceId))
      .findFirst()
      .orElse(EMPTY);
  }

  //-------------------------------------------------------------------------------------
  /*
  updateOrderAndPoLinesStatus
   */

  protected Future<Map<String, List<Piece>>> updateOrderAndPoLinesStatus(Map<String, List<Piece>> piecesFromStorage,
                                                                         Map<String, List<Piece>> piecesGroupedByPoLine,
                                                                         RequestContext requestContext,
                                                                         Consumer<List<PoLine>> updateOrderStatus) {
    if (MapUtils.isEmpty(piecesGroupedByPoLine)) {
      return Future.succeededFuture(piecesGroupedByPoLine);
    }
    List<String> poLineIdsForUpdatedPieces = getPoLineIdsForUpdatedPieces(piecesGroupedByPoLine);
    // Once all PO Lines are retrieved from storage check if receipt status
    // requires update and persist in storage
    return getPoLines(poLineIdsForUpdatedPieces, requestContext).compose(poLines -> {
      // Calculate expected status and po line details for each PO Line and update with new one if required
      List<Future<PoLineLocationsPair>> poLinesToUpdate = new ArrayList<>();
      for (PoLine poLine : poLines) {
        List<Piece> successfullyProcessedPieces = getSuccessfullyProcessedPieces(poLine.getId(), piecesGroupedByPoLine);
        if (CollectionUtils.isEmpty(successfullyProcessedPieces)) {
          logger.info("updateOrderAndPoLinesStatus:: No pieces processed - nothing to update for POL with {}", poLine.getId());
          continue;
        }

        // Method call getPiecesByPoLine() is required to retrieve all pieces for given POL by id as
        // piecesFromStorage only contains the processed pieces which is not enough for correct Receipt Status calculation
        Future<PoLineLocationsPair> poLineToUpdateFuture = getPiecesByPoLine(poLine.getId(), requestContext)
          .map(pieces -> updateRelatedPoLineDetails(poLine, piecesFromStorage.get(poLine.getId()), pieces, successfullyProcessedPieces));

        poLinesToUpdate.add(poLineToUpdateFuture);
      }

      if (CollectionUtils.isEmpty(poLinesToUpdate)) {
        logger.info("updateOrderAndPoLinesStatus:: No POL found for update");
        return Future.succeededFuture(piecesGroupedByPoLine);
      }

      return saveOrderLinesBatch(piecesGroupedByPoLine, requestContext, updateOrderStatus, poLines, poLinesToUpdate);
    })
    .map(ok -> piecesGroupedByPoLine);
  }

  private Future<Object> saveOrderLinesBatch(Map<String, List<Piece>> piecesGroupedByPoLine, RequestContext requestContext,
                                             Consumer<List<PoLine>> updateOrderStatus, List<PoLine> poLines,
                                             List<Future<PoLineLocationsPair>> poLinesToUpdate) {
    return collectResultsOnSuccess(poLinesToUpdate)
      .map(updatedPoLines -> purchaseOrderLineService.saveOrderLinesWithLocations(updatedPoLines, requestContext).compose(voidResult -> {
        logger.info("saveOrderLinesBatch:: {} out of {} POL updated with new status in batch", poLines.size(), piecesGroupedByPoLine.size());

        List<PoLine> notCancelledOrOngoingPoLines = updatedPoLines.stream()
          .map(PoLineLocationsPair::getPoLine)
          .filter(poLine -> !PoLineCommonUtil.isCancelledOrOngoingStatus(poLine))
          .toList();
        updateOrderStatus.accept(notCancelledOrOngoingPoLines);
        return null;
      }));
  }

  private List<String> getPoLineIdsForUpdatedPieces(Map<String, List<Piece>> piecesGroupedByPoLine) {
    return EntryStream
      .of(piecesGroupedByPoLine)
      .filter(entry -> entry.getValue()
        .stream()
        .anyMatch(this::isSuccessfullyProcessedPiece))
      .keys()
      .toList();
  }

  private List<Piece> getSuccessfullyProcessedPieces(String poLineId, Map<String, List<Piece>> piecesGroupedByPoLine) {
    return StreamEx.of(piecesGroupedByPoLine.get(poLineId))
      .filter(this::isSuccessfullyProcessedPiece)
      .toList();
  }

  private Future<List<Piece>> getPiecesByPoLine(String poLineId, RequestContext requestContext) {
    String query = String.format("poLineId==%s", poLineId);
    return pieceStorageService.getAllPieces(query, requestContext)
      .map(PieceCollection::getPieces);
  }

  private PoLineLocationsPair updateRelatedPoLineDetails(PoLine poLine,
                                            List<Piece> piecesFromStorage,
                                            List<Piece> byPoLine,
                                            List<Piece> successfullyProcessed) {
    if (PoLineCommonUtil.isCancelledOrOngoingStatus(poLine)) {
      logger.info("updateRelatedPoLineDetails:: Skipping updating POL '{}' status for CANCELLED or ONGOING po lines", poLine.getId());
    } else {
      ReceiptStatus receiptStatus = calculatePoLineReceiptStatus(byPoLine, successfullyProcessed, poLine);
      purchaseOrderLineService.updatePoLineReceiptStatusWithoutSave(poLine, receiptStatus);
    }

    // the same check as in PieceUpdateFlowManager::updatePoLine
    if (Boolean.TRUE.equals(poLine.getIsPackage()) || Boolean.TRUE.equals(poLine.getCheckinItems())) {
      logger.info("updateRelatedPoLineDetails:: Skipping updating POL {} if it package or has independent receiving flow", poLine.getId());
      return PoLineLocationsPair.of(poLine, poLine.getLocations());
    }

    short updatedLocations = 0;
    Set<Location> locations = new HashSet<>();
    for (Piece pieceFromStorage: piecesFromStorage) {
      locations.addAll(PieceUtil.findOrderPieceLineLocation(pieceFromStorage, poLine));
    }

    for (Piece pieceToUpdate : successfullyProcessed) {
      Optional<Piece> relatedStoragePiece = piecesFromStorage.stream()
        .filter(piece -> piece.getId().equals(pieceToUpdate.getId()))
        .findFirst();
      if (relatedStoragePiece.isEmpty()) {
        continue;
      }
      PieceUpdateHolder holder = new PieceUpdateHolder();
      holder.withPieceFromStorage(relatedStoragePiece.get());
      holder.withPieceToUpdate(pieceToUpdate);
      holder.withPoLineOnly(poLine);
      pieceUpdateFlowPoLineService.updatePoLineWithoutSave(holder);
      updatedLocations++;
    }

    return updatedLocations > 0
      ? PoLineLocationsPair.of(poLine, locations.stream().toList())
      : PoLineLocationsPair.of(poLine, poLine.getLocations());
  }

  /**
   * Returns "Fully Received" status if quantity of expected piece records is
   * zero, otherwise checks how many received pieces. If quantity of received
   * piece records is zero, returns "Awaiting Receipt" status, otherwise -
   * "Partially Received"
   *
   * @param piecesByPoLine               all piece records obtained by PO line id
   * @param piecesSuccessfullyProcessed  pieces that were successfully processed
   * @param poLine                       PO Line record representation from storage
   * @return completable future holding calculated PO Line's receipt status
   */
  private ReceiptStatus calculatePoLineReceiptStatus(List<Piece> piecesByPoLine,
                                                     List<Piece> piecesSuccessfullyProcessed,
                                                     PoLine poLine) {
    logger.info("calculatePoLineReceiptStatus:: Calculating receipt status for POL, id: {}, checkInItems: {}, processed pieces: {}, pieces from storage: {}",
      poLine.getId(), poLine.getCheckinItems(), piecesSuccessfullyProcessed.size(), piecesByPoLine.size());
    piecesSuccessfullyProcessed.forEach(v -> logger.info("calculatePoLineReceiptStatus:: Processed Piece, id: {}, status: {}", v.getId(), v.getReceivingStatus()));
    piecesByPoLine.forEach(v -> logger.info("calculatePoLineReceiptStatus:: Piece from storage, id: {}, status: {}", v.getId(), v.getReceivingStatus()));
    long expectedPiecesQuantity = piecesByPoLine.stream()
      .filter(piece -> EXPECTED_STATUSES.contains(piece.getReceivingStatus()))
      .count();
    long receivedPiecesQuantity = piecesByPoLine.stream()
      .filter(piece -> RECEIVED_STATUSES.contains(piece.getReceivingStatus()))
      .count();
    logger.info("calculatePoLineReceiptStatus:: Expected pieces: {}, Received pieces: {}", expectedPiecesQuantity, receivedPiecesQuantity);
    // Fully Received: If receiving and there is no expected piece remaining
    if (!poLine.getCheckinItems().equals(Boolean.TRUE) && expectedPiecesQuantity == 0) {
      return FULLY_RECEIVED;
    }
    // Partially Received: In case there is at least one successfully received piece
    if (StreamEx.of(piecesSuccessfullyProcessed).anyMatch(piece -> RECEIVED_STATUSES.contains(piece.getReceivingStatus()))) {
      return PARTIALLY_RECEIVED;
    }
    // If pieces were rolled-back to Expected we check if there is any Received piece in the storage
    return receivedPiecesQuantity == 0 ? AWAITING_RECEIPT : PARTIALLY_RECEIVED;
  }

  //-------------------------------------------------------------------------------------
  /*
  filterMissingLocations
   */

  /**
   * Filter by locationId presence for items/pieces related to POLine.
   *
   * @return {@link Future} which holds map with PO line id as key
   * and list of corresponding pieces as value
   */
  Future<Map<String, List<Piece>>> filterMissingLocations(Map<String, List<Piece>> piecesRecords, RequestContext requestContext) {
    return getPoLines(StreamEx.ofKeys(piecesRecords).toList(), requestContext)
      .map(poLines -> {
        for (PoLine poLine : poLines) {
          piecesRecords.get(poLine.getId()).removeIf(piece -> isMissingLocation(poLine, piece));
        }
        return piecesRecords;
      });
  }

  private boolean isMissingLocation(PoLine poLine, Piece piece) {
    if (getLocationId(piece) != null || getHoldingId(piece) != null || isRevertToOnOrder(piece)) {
      return false;
    }
    if (piece.getFormat() == Piece.Format.ELECTRONIC) {
      // Check E-Resource
      if (poLine.getEresource() != null && poLine.getEresource().getCreateInventory() != Eresource.CreateInventory.NONE
        && poLine.getEresource().getCreateInventory() != Eresource.CreateInventory.INSTANCE) {
        addError(poLine.getId(), piece.getId(), LOC_NOT_PROVIDED.toError());
        return true;
      }
    } else {
      // Check Physical
      if (poLine.getPhysical() != null && poLine.getPhysical().getCreateInventory() != Physical.CreateInventory.NONE
        && poLine.getPhysical().getCreateInventory() != Physical.CreateInventory.INSTANCE) {
        addError(poLine.getId(), piece.getId(), LOC_NOT_PROVIDED.toError());
        return true;
      }
    }
    return false;
  }

  protected abstract String getReceivingTenantId(Piece piece);

  protected abstract String getHoldingId(Piece piece);

  protected abstract String getLocationId(Piece piece);

  //-------------------------------------------------------------------------------------
  /*
  updateInventoryItemsAndHoldings
   */

  private static class PoLineAndTitleById {
    Map<String, PoLine> poLineById;
    Map<String, Title> titleById;
  }

  /**
   * Updates items in the inventory storage with check-in/receiving details if any. On
   * success updates corresponding records as received
   *
   * @return {@link Future} which holds map with PO line id as key
   * and list of corresponding pieces as value
   */
  protected Future<Map<String, List<Piece>>> updateInventoryItemsAndHoldings(Map<String, List<Piece>> piecesGroupedByPoLine,
                                                                             PiecesHolder holder,
                                                                             RequestContext requestContext) {
    Map<String, Piece> piecesByItemId = StreamEx.ofValues(piecesGroupedByPoLine)
      .flatMap(List::stream)
      .filter(piece -> StringUtils.isNotEmpty(piece.getItemId()))
      .toMap(Piece::getItemId, piece -> piece);

    List<String> poLineIds = new ArrayList<>(piecesGroupedByPoLine.keySet());

    return getPoLineAndTitleById(poLineIds, requestContext)
      .compose(poLineAndTitleById -> processHoldingsUpdate(piecesGroupedByPoLine, poLineAndTitleById, requestContext)
        .compose(voidResult -> recreateItemRecords(piecesGroupedByPoLine, holder, requestContext))
        .compose(voidResult -> getItemRecords(piecesGroupedByPoLine, piecesByItemId, requestContext))
        .compose(items -> processItemsUpdate(piecesGroupedByPoLine, holder, piecesByItemId, items, poLineAndTitleById, requestContext))
      );
  }

  protected Future<Void> recreateItemRecords(Map<String, List<Piece>> piecesGroupedByPoLine,
                                             PiecesHolder holder, RequestContext requestContext) {
    if (Objects.isNull(holder.getItemsToRecreate()) || holder.getItemsToRecreate().isEmpty()) {
      return Future.succeededFuture();
    }

    var purchaseOrderFutures = piecesGroupedByPoLine.keySet().stream()
      .collect(Collectors.toMap(poLineId -> poLineId, poLineId -> purchaseOrderStorageService.getCompositeOrderByPoLineId(poLineId, requestContext)
        .recover(throwable -> {
          logger.warn("recreateItemRecords:: Composite Purchase order can't be found for poLineId: {}", poLineId);
          return Future.succeededFuture(null);
        })
      ));

    return GenericCompositeFuture.join(new ArrayList<>(purchaseOrderFutures.values())).compose(purchaseOrdersFuture -> {
      var purchaseOrderMap = purchaseOrderFutures.entrySet().stream().collect(Collectors.toMap(Map.Entry::getKey, entry -> entry.getValue().result()));
      var futures = new ArrayList<Future<String>>();
      piecesGroupedByPoLine.keySet().stream()
        .map(poLineId -> holder.getItemsToRecreate().get(poLineId))
        .forEach(list -> processItemToRecreateList(requestContext, list, purchaseOrderMap, futures));
      return collectResultsOnSuccess(futures)
        .map(itemIdsRecreated -> {
          itemIdsRecreated.forEach(itemIdRecreated -> logger.info("recreateItemRecords:: Recreated an item in another tenant, itemId: {}", itemIdRecreated));
          return null;
        })
        .compose(v -> Future.succeededFuture());
    });
  }

  private void processItemToRecreateList(RequestContext requestContext,
                                         List<PiecesHolder.PiecePoLineDto> itemToRecreateList,
                                         Map<String, CompositePurchaseOrder> purchaseOrderMap, ArrayList<Future<String>> futures) {
    itemToRecreateList.forEach(itemToRecreate -> {
      var piece = itemToRecreate.getPieceFromStorage();
      var srcConfig = InventoryUtils.constructItemRecreateConfig(piece.getReceivingTenantId(), requestContext, true);
      var dstConfig = InventoryUtils.constructItemRecreateConfig(itemToRecreate.getCheckInPiece().getReceivingTenantId(), requestContext, false);
      if (InventoryUtils.allowItemRecreate(srcConfig.tenantId(), dstConfig.tenantId())) {
        logger.info("recreateItemRecords:: Recreating an item in another tenant, pieceId: {}, itemId: {}, locationId:{}, holdingId: {}, tenant transfer: {}->{}",
          piece.getId(), piece.getItemId(), piece.getLocationId(), piece.getHoldingId(), srcConfig.tenantId(), dstConfig.tenantId());
        var compOrder = purchaseOrderMap.get(itemToRecreate.getPoLineId());
        var poLine = itemToRecreate.getPoLine();
        futures.add(itemRecreateInventoryService.recreateItemInDestinationTenant(compOrder, poLine, piece, srcConfig.context(), dstConfig.context()));
      }
    });
  }

  /**
   * Retrieves the PO lines and associated titles.
   * Also checks all po lines have at least one title, and not more for non-packages.
   *
   * @param poLineIds      List of po lines ids
   * @param requestContext Used to initiate more requests
   * @return An object with 2 maps: poLineById (with po lines) and titleById
   */
  private Future<PoLineAndTitleById> getPoLineAndTitleById(List<String> poLineIds, RequestContext requestContext) {
    return getPoLines(poLineIds, requestContext)
      .compose(poLines -> {
        List<String> ids = poLines.stream().map(PoLine::getId).collect(Collectors.toList());
        return titlesService.getTitlesByPoLineIds(ids, requestContext)
          .map(titles -> {
            Map<String, PoLine> poLineById = poLines.stream()
              .collect(Collectors.toMap(PoLine::getId, Function.identity()));
            HelperUtils.verifyTitles(titles, poLineById);
            PoLineAndTitleById result = new PoLineAndTitleById();
            result.poLineById = poLineById;
            result.titleById = titles.values().stream().flatMap(Collection::stream)
              .collect(Collectors.toMap(Title::getId, Function.identity()));
            return result;
          });
      });
  }

  private Future<Void> processHoldingsUpdate(Map<String, List<Piece>> piecesGroupedByPoLine,
                                             PoLineAndTitleById poLinesAndTitlesById,
                                             RequestContext requestContext) {
    List<Future<Boolean>> futuresForHoldingsUpdates = new ArrayList<>();
    extractAllPieces(piecesGroupedByPoLine)
      .forEach(piece -> {
        PoLine poLine = poLinesAndTitlesById.poLineById.get(piece.getPoLineId());
        if (poLine == null) {
          logger.error("POLine associated with piece '{}' cannot be found", piece.getId());
          addError(piece.getPoLineId(), piece.getId(), ITEM_UPDATE_FAILED.toError());
          return;
        }
        Title title = poLinesAndTitlesById.titleById.get(piece.getTitleId());
        if (title == null) {
          logger.error("Piece with id {} : title with id {} was not found within the po line titles.",
            piece.getId(), piece.getTitleId());
          addError(piece.getPoLineId(), piece.getId(), ITEM_UPDATE_FAILED.toError());
          return;
        }

        boolean updateRequired = holdingUpdateOnCheckinReceiveRequired(piece, poLine);
        logger.info("processHoldingsUpdate:: Updating piece holding, pieceId: {}, itemId: {}, locationId: {}, holdingId: {}, updateHoldingRequired: {}",
          piece.getId(), piece.getItemId(), getLocationId(piece), getHoldingId(piece), updateRequired);
        if (updateRequired) {
          futuresForHoldingsUpdates.add(createHoldingsForChangedLocations(piece, title.getInstanceId(), requestContext));
        }
      });

    return GenericCompositeFuture.join(futuresForHoldingsUpdates)
      .onComplete(results -> {
        if (logger.isDebugEnabled()) {
          long successQty = results.result().list().stream()
            .filter(Objects::isNull)
            .count();
          logger.debug("{} out of {} holdings successfully processed", successQty, results.result().size());
        }
      })
      .mapEmpty();
  }

  private Future<Boolean> createHoldingsForChangedLocations(Piece piece, String instanceId, RequestContext requestContext) {
    if (!ifHoldingNotProcessed(piece.getId()) || isRevertToOnOrder(piece)) {
      return Future.succeededFuture(true);
    }
    Location location = new Location().withLocationId(getLocationId(piece)).withHoldingId(getHoldingId(piece));
    RequestContext locationContext = RequestContextUtil.createContextWithNewTenantId(requestContext, getReceivingTenantId(piece));
    return inventoryInstanceManager.createShadowInstanceIfNeeded(instanceId, locationContext)
      .compose(instance -> {
        // Does not create a holding for every piece so will be
        // called conditionally only for cases without an affiliation change
        logger.info("createHoldingsForChangedLocations:: Creating shadow instance and holding if needed, pieceId: {}, itemId: {}, locationId: {}, holdingId: {}",
          piece.getId(), piece.getItemId(), piece.getLocationId(), piece.getHoldingId());
        return inventoryHoldingManager.getOrCreateHoldingsRecord(instanceId, location, locationContext);
      })
      .compose(createdHoldingId -> {
        if (Objects.nonNull(createdHoldingId)) {
          processedHoldings.put(piece.getId(), createdHoldingId);
          piece.setHoldingId(createdHoldingId);
          logger.info("createHoldingsForChangedLocations:: Saving newly created or found holding, pieceId: {}, itemId: {}, locationId: {}, old holdingId: {}, new holdingId: {}",
            piece.getId(), piece.getItemId(), piece.getLocationId(), piece.getHoldingId(), createdHoldingId);
        }
        return Future.succeededFuture(true);
      })
      .onFailure(t -> {
        String msg = Optional.ofNullable(piece.getLocationId())
          .map(pieceLocation -> "locationId: " + pieceLocation)
          .orElse("holdingId: " + piece.getHoldingId());
        logger.error("createHoldingsForChangedLocations:: Cannot create holding for specified piece, {}", msg, t);
        addError(piece.getPoLineId(), piece.getId(), ITEM_UPDATE_FAILED.toError());
      });
  }

  private boolean ifHoldingNotProcessed(String key) {
    return processedHoldingsParams.add(key);
  }

  /**
   * Retrieves item records from inventory storage for
   *
   * @param piecesByItemId map with item id as a key and piece record as a value
   * @return future with list of item records
   */
  private Future<List<JsonObject>> getItemRecords(Map<String, List<Piece>> piecesGroupedByPoLine,
                                                  Map<String, Piece> piecesByItemId,
                                                  RequestContext requestContext) {
    // Split all id lists by maximum number of id's for get query
    return collectResultsOnSuccess(
      mapTenantIdsToItemIds(piecesGroupedByPoLine, requestContext).entrySet().stream()
        .flatMap(entry ->
          StreamEx.ofSubLists(entry.getValue(), MAX_IDS_FOR_GET_RQ_15)
            .map(ids -> {
              var locationContext = RequestContextUtil.createContextWithNewTenantId(requestContext, entry.getKey());
              return getItemRecordsByIds(ids, piecesByItemId, locationContext);
            })
        )
        .toList())
      .map(lists -> StreamEx.of(lists).toFlatList(items -> items));
  }

  /**
   * Returns list of item records for specified id's.
   *
   * @param ids             List of item id's
   * @param piecesWithItems map with item id as a key and piece record as a value
   * @return future with list of item records
   */
  private Future<List<JsonObject>> getItemRecordsByIds(List<String> ids, Map<String, Piece> piecesWithItems, RequestContext requestContext) {
    return inventoryItemManager.getItemRecordsByIds(ids, requestContext)
      .map(items -> {
        checkIfAllItemsFound(ids, items, piecesWithItems);
        return items;
      })
      .otherwise(e -> {
        logger.error("The issue happened getting item records");
        ids.forEach(id -> {
          Piece piece = piecesWithItems.get(id);
          addError(piece.getPoLineId(), piece.getId(), ITEM_NOT_RETRIEVED.toError());
        });
        return Collections.emptyList();
      });
  }

  /**
   * Checks if all expected piece records found in the storage. If any is
   * missing, remove from piece reference to item and exclude piece from {@param piecesWithItems}
   *
   * @param expectedItemIds list of expected item id's
   * @param piecesWithItems map with item id as a key and piece record as a value
   * @param items           found item records
   */
  private void checkIfAllItemsFound(List<String> expectedItemIds, List<JsonObject> items,
                                    Map<String, Piece> piecesWithItems) {
    // Handle the case when for some reason some items are not found
    if (items.size() < expectedItemIds.size()) {
      List<String> foundItemIds = StreamEx.of(items).map(HelperUtils::extractId).toList();

      expectedItemIds.stream()
        .filter(id -> !foundItemIds.contains(id))
        .forEach(itemId -> {
          Piece piece = piecesWithItems.get(itemId);
          piece.setItemId(null);
          piecesWithItems.remove(itemId);
        });
    }
  }

  private Future<Map<String, List<Piece>>> processItemsUpdate(Map<String, List<Piece>> piecesGroupedByPoLine,
                                                              PiecesHolder holder,
                                                              Map<String, Piece> piecesByItemId,
                                                              List<JsonObject> items,
                                                              PoLineAndTitleById poLinesAndTitlesById,
                                                              RequestContext requestContext) {
    List<Future<Boolean>> futuresForItemsUpdates = new ArrayList<>();

    if (piecesByItemId.isEmpty()) {
      return Future.succeededFuture(piecesGroupedByPoLine);
    }

    for (JsonObject item : items) {
      Piece piece = piecesByItemId.get(item.getString(ID));
      PoLine poLine = poLinesAndTitlesById.poLineById.get(piece.getPoLineId());
      Title title = poLinesAndTitlesById.titleById.get(piece.getTitleId());
      if (Objects.isNull(poLine) || Objects.isNull(title)) {
        continue;
      }

      if (holdingUpdateOnCheckinReceiveRequired(piece, poLine) && !isRevertToOnOrder(piece)) {
        if (processedHoldings.containsKey(piece.getId())) {
          String holdingId = processedHoldings.get(piece.getId());
          item.put(ITEM_HOLDINGS_RECORD_ID, holdingId);
          logger.info("processItemsUpdate:: Item was processed, pieceId: {}, itemId: {}, old holdingId: {}, new holdingId: {}", piece.getId(), piece.getItemId(), piece.getHoldingId(), holdingId);
        } else {
          logger.info("processItemsUpdate:: Item processing is not required, pieceId: {}, itemId: {}", piece.getId(), piece.getItemId());
        }
      }
      RequestContext locationContext = RequestContextUtil.createContextWithNewTenantId(requestContext, getReceivingTenantId(piece));
      futuresForItemsUpdates.add(receiveInventoryItemAndUpdatePiece(holder, item, piece, locationContext));
    }
    return collectResultsOnSuccess(futuresForItemsUpdates).map(results -> {
      if (logger.isDebugEnabled()) {
        long successQty = results.stream().filter(result -> result).count();
        logger.debug("{} out of {} inventory item(s) successfully updated", successQty, results.size());
      }
      return piecesGroupedByPoLine;
    });
  }

  /**
   * @param item  inventory item
   * @param piece piece associated with the item
   * @return future indicating if the item update is successful.
   */
  protected abstract Future<Boolean> receiveInventoryItemAndUpdatePiece(PiecesHolder holder, JsonObject item, Piece piece, RequestContext locationContext);

  private boolean holdingUpdateOnCheckinReceiveRequired(Piece piece, PoLine poLine) {
    boolean isHoldingUpdateRequired;
    if (piece.getFormat() == Piece.Format.ELECTRONIC) {
      isHoldingUpdateRequired = PoLineCommonUtil.isHoldingUpdateRequiredForEresource(poLine);
    } else {
      isHoldingUpdateRequired = PoLineCommonUtil.isHoldingUpdateRequiredForPhysical(poLine);
    }
    return isHoldingUpdateRequired && (StringUtils.isNotEmpty(getLocationId(piece)) || StringUtils.isNotEmpty(getHoldingId(piece)));
  }

  //-------------------------------------------------------------------------------------
  /*
  Other methods
   */

  protected Future<Void> removeForbiddenEntities(RequestContext requestContext) {
    return titlesService.getTitlesByPieceIds(getPieceIds(), requestContext)
      .compose(titles -> CollectionUtils.isNotEmpty(titles) ? GenericCompositeFuture
        .join(getListOfRestrictionCheckingFutures(titles, requestContext))
        .mapEmpty() : Future.succeededFuture());
  }

  private List<Future<?>> getListOfRestrictionCheckingFutures(List<Title> titles, RequestContext requestContext) {
    return titles.stream()
      .map(title -> protectionService.isOperationRestricted(title.getAcqUnitIds(), ProtectedOperationType.UPDATE, requestContext)
        .recover(t -> {
          // get map of pieceId and to be processed piece details from multimap
          Map<String, T> pieceMap = piecesByLineId.get(title.getPoLineId());
          return pieceStorageService.getPiecesByIds(new ArrayList<>(pieceMap.keySet()), requestContext)
            .map(pieceList -> {
              pieceList.stream().filter(piece -> Objects.equals(piece.getTitleId(), title.getId()))
                .forEach(piece -> {
                  // remove piece which protected by title to exclude from further processing
                  pieceMap.remove(piece.getId());
                  addError(piece.getPoLineId(), piece.getId(), USER_HAS_NO_PERMISSIONS.toError());
                });
              return null;
            });
        }))
      .collect(Collectors.toList());
  }

  protected void sendMessage(MessageAddress messageAddress, Object value, RequestContext requestContext) {
    JsonObject messageContent = new JsonObject();
    messageContent.put(OKAPI_HEADERS, okapiHeaders);
    messageContent.put(EVENT_PAYLOAD, value);
    HelperUtils.sendEvent(messageAddress, messageContent, requestContext);
  }

  protected T getByPiece(Piece piece) {
    return piecesByLineId.get(piece.getPoLineId()).get(piece.getId());
  }

  /**
   * Updates piece records with receiving details which do not have associated
   * item
   *
   * @param piecesGroupedByPoLine map with PO line id as key and list of corresponding pieces as
   *                              value
   * @return updated map passed as a parameter
   */
  protected abstract Map<String, List<Piece>> updatePieceRecordsWithoutItems(Map<String, List<Piece>> piecesGroupedByPoLine);

  /**
   * Verifies if the current status of the piece record is "Received" and the
   * client would like to roll-back to Expected
   *
   * @param piece piece record to asses
   * @return {@code true} if piece record is already received and has to be
   * rolled-back to Expected
   */
  protected abstract boolean isRevertToOnOrder(Piece piece);

  private Future<List<PoLine>> getPoLines(List<String> poLineIds, RequestContext requestContext) {
    if (poLineList == null) {
      return purchaseOrderLineService.getOrderLinesByIds(poLineIds, requestContext)
        .map(list -> {
          poLineList = list;
          return list;
        });
    } else {
      return Future.succeededFuture(poLineList.stream()
        .filter(poLine -> poLineIds.contains(poLine.getId()))
        .collect(Collectors.toList()));
    }
  }

  /**
   * Gets all piece id's based on request data
   *
   * @return extract all piece id's
   */
  private List<String> getPieceIds() {
    return StreamEx.ofValues(piecesByLineId)
      .map(Map::keySet)
      .toFlatList(ids -> ids);
  }

  protected Map<String, Piece> getProcessedPiecesForPoLine(String poLineId, Map<String, List<Piece>> piecesGroupedByPoLine) {
    return StreamEx
      .of(piecesGroupedByPoLine.getOrDefault(poLineId, Collections.emptyList()))
      .toMap(Piece::getId, piece -> piece);
  }

  protected Map<ProcessingStatus.Type, Integer> getEmptyResultCounts() {
    Map<ProcessingStatus.Type, Integer> resultCounts = new HashMap<>();
    resultCounts.put(ProcessingStatus.Type.SUCCESS, 0);
    resultCounts.put(ProcessingStatus.Type.FAILURE, 0);
    return resultCounts;
  }

  protected StreamEx<Piece> extractAllPieces(Map<String, List<Piece>> piecesGroupedByPoLine) {
    return StreamEx.ofValues(piecesGroupedByPoLine).flatMap(List::stream);
  }

  protected Map<String, List<String>> mapTenantIdsToItemIds(Map<String, List<Piece>> piecesGroupedByPoLine, RequestContext requestContext) {
    return extractAllPieces(piecesGroupedByPoLine)
      .filter(piece -> StringUtils.isNotEmpty(piece.getItemId()))
      .groupingBy(piece -> Optional.ofNullable(getReceivingTenantId(piece))
          .orElse(TenantTool.tenantId(requestContext.getHeaders())),
        mapping(Piece::getItemId, toList()));
  }

  //-------------------------------------------------------------------------------------
  /*
  Errors
   */

  protected void addErrorForUpdatingItem(Piece piece, Throwable e) {
    if (StringUtils.isNotBlank(e.getMessage()) && e.getMessage().contains(BARCODE_NOT_UNIQUE_MESSAGE)) {
      logger.error("The barcode associate with item '{}' is not unique, it cannot be updated", piece.getId());
      addError(piece.getPoLineId(), piece.getId(), BARCODE_IS_NOT_UNIQUE.toError());
    } else {
      logger.error("Item associated with piece '{}' cannot be updated", piece.getId(), e);
      var causeParam = new Parameter().withKey("cause").withValue(e.getMessage());
      addError(piece.getPoLineId(), piece.getId(), ITEM_UPDATE_FAILED.toError().withParameters(List.of(causeParam)));
    }
  }

  /**
   * Checks if there is processing error for particular piece record
   *
   * @param piece piece record to get error for
   * @return {@code true} if there is an error associated with piece record
   */
  private boolean isSuccessfullyProcessedPiece(Piece piece) {
    return getError(piece.getPoLineId(), piece.getId()) == null;
  }

  /**
   * Gets processing error for particular piece record
   *
   * @param polId   PO Line id
   * @param pieceId piece id
   * @return error object if presents or null
   */
  protected Error getError(String polId, String pieceId) {
    return Optional.ofNullable(processingErrors.get(polId))
      .map(errors -> errors.get(pieceId))
      .orElse(null);
  }

  private void addError(String polId, String pieceId, Error error) {
    processingErrors.computeIfAbsent(polId, k -> new HashMap<>())
      .put(pieceId, error);
  }

  public void calculateProcessingErrors(String poLineId, ReceivingResult result,
                                        Map<String, Piece> processedPiecesForPoLine,
                                        Map<ProcessingStatus.Type, Integer> resultCounts, String pieceId) {
    // Calculate processing status
    ProcessingStatus status = new ProcessingStatus();
    Error error = getError(poLineId, pieceId);
    if (processedPiecesForPoLine.get(pieceId) != null && error == null) {
      status.setType(ProcessingStatus.Type.SUCCESS);
      resultCounts.merge(ProcessingStatus.Type.SUCCESS, 1, Integer::sum);
    } else {
      status.setType(ProcessingStatus.Type.FAILURE);
      status.setError(error);
      resultCounts.merge(ProcessingStatus.Type.FAILURE, 1, Integer::sum);
    }

    ReceivingItemResult itemResult = new ReceivingItemResult();
    itemResult.setPieceId(pieceId);
    itemResult.setProcessingStatus(status);
    result.getReceivingItemResults().add(itemResult);
  }

}
