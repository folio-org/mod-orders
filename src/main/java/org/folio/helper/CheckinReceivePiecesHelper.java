package org.folio.helper;

import io.vertx.core.Context;
import io.vertx.core.Future;
import io.vertx.core.json.JsonObject;
import one.util.streamex.EntryStream;
import one.util.streamex.StreamEx;
import org.apache.commons.collections4.CollectionUtils;
import org.apache.commons.collections4.MapUtils;
import org.apache.commons.lang3.StringUtils;
import org.folio.okapi.common.GenericCompositeFuture;
import org.folio.orders.events.handlers.MessageAddress;
import org.folio.orders.utils.HelperUtils;
import org.folio.orders.utils.PoLineCommonUtil;
import org.folio.orders.utils.ProtectedOperationType;
import org.folio.orders.utils.RequestContextUtil;
import org.folio.rest.core.RestClient;
import org.folio.rest.core.models.RequestContext;
import org.folio.rest.jaxrs.model.CompositePoLine;
import org.folio.rest.jaxrs.model.Eresource;
import org.folio.rest.jaxrs.model.Error;
import org.folio.rest.jaxrs.model.Location;
import org.folio.rest.jaxrs.model.Parameter;
import org.folio.rest.jaxrs.model.Physical;
import org.folio.rest.jaxrs.model.Piece;
import org.folio.rest.jaxrs.model.Piece.ReceivingStatus;
import org.folio.rest.jaxrs.model.PieceCollection;
import org.folio.rest.jaxrs.model.PoLine;
import org.folio.rest.jaxrs.model.PoLine.ReceiptStatus;
import org.folio.rest.jaxrs.model.ProcessingStatus;
import org.folio.rest.jaxrs.model.ReceivingItemResult;
import org.folio.rest.jaxrs.model.ReceivingResult;
import org.folio.rest.jaxrs.model.Title;
import org.folio.rest.tools.utils.TenantTool;
import org.folio.service.ProtectionService;
import org.folio.service.inventory.InventoryHoldingManager;
import org.folio.service.inventory.InventoryInstanceManager;
import org.folio.service.inventory.InventoryItemManager;
import org.folio.service.orders.PurchaseOrderLineService;
import org.folio.service.pieces.PieceStorageService;
import org.folio.service.pieces.flows.create.PieceCreateFlowInventoryManager;
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
import static org.folio.rest.jaxrs.model.PoLine.ReceiptStatus.CANCELLED;
import static org.folio.rest.jaxrs.model.PoLine.ReceiptStatus.FULLY_RECEIVED;
import static org.folio.rest.jaxrs.model.PoLine.ReceiptStatus.ONGOING;
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
  private final RestClient restClient;

  private List<PoLine> poLineList;


  protected CheckinReceivePiecesHelper(Map<String, String> okapiHeaders, Context ctx) {
    super(okapiHeaders, ctx);
    this.restClient = new RestClient();
    processedHoldingsParams = new HashSet<>();
    processedHoldings = new HashMap<>();
    processingErrors = new HashMap<>();
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

  protected Future<Map<String, List<Piece>>> updateOrderAndPoLinesStatus(Map<String, List<Piece>> piecesGroupedByPoLine,
                                                                         RequestContext requestContext,
                                                                         Consumer<List<PoLine>> updateOrderStatus) {
    if (MapUtils.isEmpty(piecesGroupedByPoLine)) {
      return Future.succeededFuture(piecesGroupedByPoLine);
    }
    List<String> poLineIdsForUpdatedPieces = getPoLineIdsForUpdatedPieces(piecesGroupedByPoLine);
    // Once all PO Lines are retrieved from storage check if receipt status
    // requires update and persist in storage
    return getPoLines(poLineIdsForUpdatedPieces, requestContext).compose(poLines -> {
        // Calculate expected status for each PO Line and update with new one if required
        // Skip status update if PO line status is Ongoing
        List<Future<String>> futures = new ArrayList<>();
        for (PoLine poLine : poLines) {
          if (poLine.getReceiptStatus() == CANCELLED || poLine.getReceiptStatus() == ONGOING) {
            continue;
          }
          List<Piece> successfullyProcessedPieces = getSuccessfullyProcessedPieces(poLine.getId(), piecesGroupedByPoLine);
          futures.add(calculatePoLineReceiptStatus(poLine, successfullyProcessedPieces, requestContext)
            .compose(status -> purchaseOrderLineService.updatePoLineReceiptStatus(poLine, status, requestContext)));
        }

        return collectResultsOnSuccess(futures).map(updatedPoLines -> {
          logger.debug("{} out of {} PO Line(s) updated with new status", updatedPoLines.size(), piecesGroupedByPoLine.size());

          List<PoLine> successPoLines = StreamEx.of(poLines)
            .filter(line -> updatedPoLines.contains(line.getId()))
            .toList();
          updateOrderStatus.accept(successPoLines);
          return null;
        });
      })
      .map(ok -> piecesGroupedByPoLine);
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

  private Future<ReceiptStatus> calculatePoLineReceiptStatus(PoLine poLine, List<Piece> pieces, RequestContext requestContext) {

    if (CollectionUtils.isEmpty(pieces)) {
      logger.info("No pieces processed - receipt status unchanged for PO Line '{}'", poLine.getId());
      return Future.succeededFuture(poLine.getReceiptStatus());
    }

    return getPiecesByPoLine(poLine.getId(), requestContext)
      .compose(byPoLine -> calculatePoLineReceiptStatus(byPoLine, poLine, pieces))
      .onFailure(e -> logger.error("The expected receipt status for PO Line '{}' cannot be calculated", poLine.getId(), e));
  }

  private Future<List<Piece>> getPiecesByPoLine(String poLineId, RequestContext requestContext) {
    String query = String.format("poLineId==%s", poLineId);
    return pieceStorageService.getPieces(Integer.MAX_VALUE, 0, query, requestContext)
      .map(PieceCollection::getPieces);
  }

  /**
   * Returns "Fully Received" status if quantity of expected piece records is
   * zero, otherwise checks how many received pieces. If quantity of received
   * piece records is zero, returns "Awaiting Receipt" status, otherwise -
   * "Partially Received"
   *
   * @param byPoLine all piece records obtained by PO line id
   * @param poLine   PO Line record representation from storage
   * @return completable future holding calculated PO Line's receipt status
   */
  private Future<ReceiptStatus> calculatePoLineReceiptStatus(List<Piece> byPoLine, PoLine poLine, List<Piece> pieces) {
    long expectedPiecesQuantity = byPoLine.stream()
      .filter(piece -> EXPECTED_STATUSES.contains(piece.getReceivingStatus()))
      .count();
    // Fully Received: If receiving and there is no expected piece remaining
    if (!poLine.getCheckinItems().equals(Boolean.TRUE) && expectedPiecesQuantity == 0) {
      return Future.succeededFuture(FULLY_RECEIVED);
    }
    // Partially Received: In case there is at least one successfully received piece
    if (StreamEx.of(pieces).anyMatch(piece -> RECEIVED_STATUSES.contains(piece.getReceivingStatus()))) {
      return Future.succeededFuture(PARTIALLY_RECEIVED);
    }
    // Pieces were rolled-back to Expected. In this case we have to check if
    // there is any Received piece in the storage
    long receivedQty = byPoLine.stream()
      .filter(piece -> RECEIVED_STATUSES.contains(piece.getReceivingStatus()))
      .count();
    return Future.succeededFuture(receivedQty == 0 ? AWAITING_RECEIPT : PARTIALLY_RECEIVED);
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
    Map<String, CompositePoLine> poLineById;
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
                                                                             RequestContext requestContext) {
    Map<String, Piece> piecesByItemId = StreamEx.ofValues(piecesGroupedByPoLine)
      .flatMap(List::stream)
      .filter(piece -> StringUtils.isNotEmpty(piece.getItemId()))
      .toMap(Piece::getItemId, piece -> piece);

    List<String> poLineIds = new ArrayList<>(piecesGroupedByPoLine.keySet());

    return getPoLineAndTitleById(poLineIds, requestContext)
      .compose(poLineAndTitleById -> processHoldingsUpdate(piecesGroupedByPoLine, poLineAndTitleById, requestContext)
        .compose(v -> getItemRecords(piecesGroupedByPoLine, piecesByItemId, requestContext))
        .compose(items -> processItemsUpdate(piecesGroupedByPoLine, piecesByItemId, items, poLineAndTitleById, requestContext))
      );
  }

  /**
   * Retrieves the PO lines and associated titles.
   * Also checks all po lines have at least one title, and not more for non-packages.
   *
   * @param poLineIds      List of po lines ids
   * @param requestContext Used to initiate more requests
   * @return An object with 2 maps: poLineById (with composite po lines) and titleById
   */
  private Future<PoLineAndTitleById> getPoLineAndTitleById(List<String> poLineIds, RequestContext requestContext) {
    return getPoLines(poLineIds, requestContext)
      .compose(poLines -> {
        List<String> ids = poLines.stream().map(PoLine::getId).collect(Collectors.toList());
        return titlesService.getTitlesByPoLineIds(ids, requestContext)
          .map(titles -> {
            List<CompositePoLine> compositePoLines = poLines.stream()
              .map(PoLineCommonUtil::convertToCompositePoLine).toList();
            Map<String, CompositePoLine> poLineById = compositePoLines.stream()
              .collect(Collectors.toMap(CompositePoLine::getId, Function.identity()));
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
        CompositePoLine poLine = poLinesAndTitlesById.poLineById.get(piece.getPoLineId());
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

        if (holdingUpdateOnCheckinReceiveRequired(piece, poLine)) {
          futuresForHoldingsUpdates.add(
            createHoldingsForChangedLocations(piece, title.getInstanceId(), requestContext)
          );
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
    String holdingKey = buildProcessedHoldingKey(piece, instanceId);
    if (!ifHoldingNotProcessed(holdingKey) || isRevertToOnOrder(piece)) {
      return Future.succeededFuture(true);
    }

    String receivingTenantId = getReceivingTenantId(piece);
    String locationId = getLocationId(piece);
    String holdingId = getHoldingId(piece);
    logger.info("createHoldingsForChangedLocations:: receivingTenantId: {} locationId: {} holdingId: {}",
      receivingTenantId, locationId, holdingId);
    Location location = new Location().withLocationId(locationId).withHoldingId(holdingId);
    var locationContext = RequestContextUtil.createContextWithNewTenantId(requestContext, receivingTenantId);
    return inventoryInstanceManager.createShadowInstanceIfNeeded(instanceId, locationContext)
      .compose(instance -> inventoryHoldingManager.getOrCreateHoldingsRecord(instanceId, location, locationContext))
      .compose(createdHoldingId -> {
        processedHoldings.put(holdingKey, createdHoldingId);
        piece.setHoldingId(createdHoldingId);
        return Future.succeededFuture(true);
      })
      .onFailure(t -> {
        String msg = Optional.ofNullable(piece.getLocationId())
          .map(pieceLocation -> "location : " + pieceLocation)
          .orElse("holding : " + piece.getHoldingId());
        logger.error("Cannot create holding for specified piece {}", msg);
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
        .flatMap(entry -> StreamEx.ofSubLists(entry.getValue(), MAX_IDS_FOR_GET_RQ_15)
          .map(ids -> {
            var locationContext = RequestContextUtil.createContextWithNewTenantId(requestContext, entry.getKey());

            return getItemRecordsByIds(ids, piecesByItemId, locationContext);
          }))
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
                                                              Map<String, Piece> piecesByItemId,
                                                              List<JsonObject> items,
                                                              PoLineAndTitleById poLinesAndTitlesById,
                                                              RequestContext requestContext) {
    List<Future<Boolean>> futuresForItemsUpdates = new ArrayList<>();

    if (piecesByItemId.isEmpty()) {
      return Future.succeededFuture(piecesGroupedByPoLine);
    }

    for (JsonObject item : items) {
      String itemId = item.getString(ID);
      Piece piece = piecesByItemId.get(itemId);

      CompositePoLine poLine = poLinesAndTitlesById.poLineById.get(piece.getPoLineId());
      if (poLine == null)
        // TODO: remove 'continue'
        continue;
      Title title = poLinesAndTitlesById.titleById.get(piece.getTitleId());
      if (title == null)
        continue;

      // holdingUpdateOnCheckinReceiveRequired
      if (holdingUpdateOnCheckinReceiveRequired(piece, poLine) && !isRevertToOnOrder(piece)) {
        String holdingKey = buildProcessedHoldingKey(piece, title.getInstanceId());
        String holdingId = processedHoldings.get(holdingKey);
        item.put(ITEM_HOLDINGS_RECORD_ID, holdingId);
      }
      var locationContext = RequestContextUtil.createContextWithNewTenantId(requestContext, getReceivingTenantId(piece));

      futuresForItemsUpdates.add(receiveInventoryItemAndUpdatePiece(item, piece, locationContext));
    }
    return collectResultsOnSuccess(futuresForItemsUpdates).map(results -> {
      if (logger.isDebugEnabled()) {
        long successQty = results.stream()
          .filter(result -> result)
          .count();
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
  protected abstract Future<Boolean> receiveInventoryItemAndUpdatePiece(JsonObject item, Piece piece, RequestContext locationContext);

  private boolean holdingUpdateOnCheckinReceiveRequired(Piece piece, CompositePoLine poLine) {
    boolean isHoldingUpdateRequired;
    if (piece.getFormat() == Piece.Format.ELECTRONIC) {
      isHoldingUpdateRequired = PoLineCommonUtil.isHoldingUpdateRequiredForEresource(poLine);
    } else {
      isHoldingUpdateRequired = PoLineCommonUtil.isHoldingUpdateRequiredForPhysical(poLine);
    }
    String locationId = getLocationId(piece);
    String holdingId = getHoldingId(piece);
    return isHoldingUpdateRequired && (StringUtils.isNotEmpty(locationId) || StringUtils.isNotEmpty(holdingId));
  }

  private String buildProcessedHoldingKey(Piece piece, String instanceId) {
    String locationId = getLocationId(piece);
    String holdingId = getHoldingId(piece);
    return Optional.ofNullable(locationId).orElse(holdingId) + instanceId;
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
