package org.folio.helper;

import static java.util.concurrent.CompletableFuture.completedFuture;
import static java.util.stream.Collectors.groupingBy;
import static org.apache.commons.lang3.StringUtils.EMPTY;
import static org.folio.helper.PurchaseOrderHelper.GET_PURCHASE_ORDERS;
import static org.folio.orders.utils.ErrorCodes.ITEM_NOT_RETRIEVED;
import static org.folio.orders.utils.ErrorCodes.ITEM_UPDATE_FAILED;
import static org.folio.orders.utils.ErrorCodes.LOC_NOT_PROVIDED;
import static org.folio.orders.utils.ErrorCodes.PIECE_ALREADY_RECEIVED;
import static org.folio.orders.utils.ErrorCodes.PIECE_NOT_FOUND;
import static org.folio.orders.utils.ErrorCodes.PIECE_NOT_RETRIEVED;
import static org.folio.orders.utils.ErrorCodes.PIECE_POL_MISMATCH;
import static org.folio.orders.utils.ErrorCodes.PIECE_UPDATE_FAILED;
import static org.folio.orders.utils.ErrorCodes.USER_HAS_NO_PERMISSIONS;
import static org.folio.orders.utils.HelperUtils.buildQuery;
import static org.folio.orders.utils.HelperUtils.collectResultsOnSuccess;
import static org.folio.orders.utils.HelperUtils.convertIdsToCqlQuery;
import static org.folio.orders.utils.HelperUtils.encodeQuery;
import static org.folio.orders.utils.HelperUtils.handleGetRequest;
import static org.folio.orders.utils.HelperUtils.handlePutRequest;
import static org.folio.orders.utils.ResourcePathResolver.PIECES;
import static org.folio.orders.utils.ResourcePathResolver.resourceByIdPath;
import static org.folio.orders.utils.ResourcePathResolver.resourcesPath;
import static org.folio.rest.jaxrs.model.PoLine.ReceiptStatus.AWAITING_RECEIPT;
import static org.folio.rest.jaxrs.model.PoLine.ReceiptStatus.FULLY_RECEIVED;
import static org.folio.rest.jaxrs.model.PoLine.ReceiptStatus.PARTIALLY_RECEIVED;
import static org.folio.service.inventory.InventoryManager.ITEM_HOLDINGS_RECORD_ID;

import java.util.ArrayList;
import java.util.Collections;
import java.util.HashMap;
import java.util.HashSet;
import java.util.List;
import java.util.Map;
import java.util.Optional;
import java.util.Set;
import java.util.concurrent.CompletableFuture;
import java.util.stream.Collectors;

import org.apache.commons.lang3.StringUtils;
import org.folio.orders.utils.HelperUtils;
import org.folio.orders.utils.PoLineCommonUtil;
import org.folio.orders.utils.ProtectedOperationType;
import org.folio.rest.core.models.RequestContext;
import org.folio.rest.jaxrs.model.CompositePoLine;
import org.folio.rest.jaxrs.model.Eresource;
import org.folio.rest.jaxrs.model.Error;
import org.folio.rest.jaxrs.model.Physical;
import org.folio.rest.jaxrs.model.Piece;
import org.folio.rest.jaxrs.model.PieceCollection;
import org.folio.rest.jaxrs.model.PoLine;
import org.folio.rest.jaxrs.model.PoLine.ReceiptStatus;
import org.folio.rest.jaxrs.model.PoLineCollection;
import org.folio.rest.jaxrs.model.ProcessingStatus;
import org.folio.rest.jaxrs.model.PurchaseOrder;
import org.folio.rest.jaxrs.model.PurchaseOrderCollection;
import org.folio.rest.jaxrs.model.ReceivingItemResult;
import org.folio.rest.jaxrs.model.ReceivingResult;
import org.folio.rest.jaxrs.model.Title;
import org.folio.rest.tools.client.interfaces.HttpClientInterface;
import org.folio.service.ProtectionService;
import org.folio.service.inventory.InventoryManager;
import org.folio.service.titles.TitlesService;
import org.springframework.beans.factory.annotation.Autowired;

import io.vertx.core.Context;
import io.vertx.core.json.JsonObject;
import one.util.streamex.EntryStream;
import one.util.streamex.StreamEx;

public abstract class CheckinReceivePiecesHelper<T> extends AbstractHelper {

  private static final String PIECES_WITH_QUERY_ENDPOINT = resourcesPath(PIECES) + "?limit=%d&lang=%s&query=%s";
  private static final String PIECES_BY_POL_ID_AND_STATUS_QUERY = "poLineId==%s and receivingStatus==%s";
  Map<String, Map<String, T>> piecesByLineId;
  Map<String, Map<String, Error>> processingErrors;
  Set<String> processedHoldingsParams;
  Map<String, String> processedHoldings;
  private final PurchaseOrderLineHelper poLineHelper;
  @Autowired
  private  ProtectionService protectionService;
  @Autowired
  protected TitlesService titlesService;
  @Autowired
  protected InventoryManager inventoryManager;

  private List<PoLine> poLineList;


  public CheckinReceivePiecesHelper(HttpClientInterface httpClient, Map<String, String> okapiHeaders, Context ctx, String lang) {
    super(httpClient, okapiHeaders, ctx, lang);
    processedHoldingsParams = new HashSet<>();
    processedHoldings = new HashMap<>();
    processingErrors = new HashMap<>();
    poLineHelper = new PurchaseOrderLineHelper(httpClient, okapiHeaders, ctx, lang);
  }

  /**
   * Retrieves piece records from storage based on request data
   *
   * @return {@link CompletableFuture} which holds map with PO line id as key
   *         and list of corresponding pieces as value
   */
  CompletableFuture<Map<String, List<Piece>>> retrievePieceRecords(Map<String, Map<String, T>> piecesByLineId,
                                                                   RequestContext requestContext) {
    Map<String, List<Piece>> piecesByPoLine = new HashMap<>();
    this.piecesByLineId = piecesByLineId;
    // Split all piece id's by maximum number of id's for get query
    CompletableFuture<?>[] futures = StreamEx
      .ofSubLists(getPieceIds(), MAX_IDS_FOR_GET_RQ)
      // Send get request for each CQL query
      .map(ids -> getPiecesByIds(ids, piecesByPoLine, requestContext))
      .toArray(CompletableFuture.class);

    // Wait for all pieces to be retrieved and complete resulting future
    return CompletableFuture.allOf(futures)
      .thenApply(v -> {
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

  private CompletableFuture<Void> getPiecesByIds(List<String> ids, Map<String, List<Piece>> piecesByPoLine, RequestContext requestContext) {
    // Transform piece id's to CQL query
    String query = convertIdsToCqlQuery(ids);
    String endpoint = String.format(PIECES_WITH_QUERY_ENDPOINT, ids.size(), lang, encodeQuery(query, logger));
    return handleGetRequest(endpoint, httpClient, okapiHeaders, logger)
      .thenAccept(pieceJson -> {
        List<Piece> pieces = pieceJson.mapTo(PieceCollection.class).getPieces();
        pieces.forEach(piece -> addPieceIfValid(piece, piecesByPoLine, requestContext));
        checkIfAllPiecesFound(ids, pieces);
      })
      .exceptionally(e -> {
        logger.error("Error fetching piece records", e);
        ids.forEach(pieceId -> addError(getPoLineIdByPieceId(pieceId), pieceId, PIECE_NOT_RETRIEVED.toError()));
        return null;
      });
  }

  /**
   * Validates if the piece corresponds to PO Line specified in the request and
   * can be received/checked-in. If all checks pass, adds to map
   *
   * @param piece
   *          {@link Piece} piece to validate and add to map
   * @param piecesByPoLine
   *          map with PO line id as a key and list of corresponding pieces as a
   *          value
   */
  private void addPieceIfValid(Piece piece, Map<String, List<Piece>> piecesByPoLine, RequestContext requestContext) {
    String poLineId = piece.getPoLineId();
    String pieceId = piece.getId();

    // Validate if the piece actually corresponds to PO line specified in the
    // request
    if (piecesByLineId.containsKey(poLineId) && piecesByLineId.get(poLineId).containsKey(pieceId)) {
      // Validate if the piece is not yet received
      if (piece.getReceivingStatus() == Piece.ReceivingStatus.EXPECTED || isRevertToOnOrder(piece)) {
        piecesByPoLine.computeIfAbsent(poLineId, v -> new ArrayList<>())
          .add(piece);
      } else {
        addError(poLineId, pieceId, PIECE_ALREADY_RECEIVED.toError());
      }
    } else {
      addError(getPoLineIdByPieceId(pieceId), pieceId, PIECE_POL_MISMATCH.toError());
    }
  }

  /**
   * Verifies if the current status of the piece record is "Received" and the
   * client would like to roll-back to Expected
   *
   * @param piece
   *          piece record to asses
   * @return {@code true} if piece record is already received and has to be
   *         rolled-back to Expected
   */
  abstract boolean isRevertToOnOrder(Piece piece);

  /**
   * @param pieceId
   *          piece id
   * @return PO Line id corresponding to passed pieceId from request data
   */
  private String getPoLineIdByPieceId(String pieceId) {
    return StreamEx
      .ofKeys(piecesByLineId, values -> values.containsKey(pieceId))
      .findFirst()
      .orElse(EMPTY);
  }

  /**
   * Checks if all expected piece records found in the storage. If any are
   * missing, adds corresponding error
   *
   * @param pieceIds
   *          list of expected piece id's
   * @param pieces
   *          found pieces
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

  private CompositePoLine searchPoLineById(List<CompositePoLine> poLines, Piece piece) {
    for (CompositePoLine poline : poLines) {
      if (poline.getId().equals(piece.getPoLineId())) {
        return poline;
      }
    }
    logger.error("POLine associated with piece '{}' cannot be found", piece.getId());
    addError(piece.getPoLineId(), piece.getId(), ITEM_UPDATE_FAILED.toError());
    return null;
  }

  private CompletableFuture<Boolean> createHoldingsForChangedLocations(Piece piece, CompositePoLine poLine, String receivedPieceLocationId,
                                                                       RequestContext requestContext) {
    if (ifHoldingNotProcessed(receivedPieceLocationId, poLine.getInstanceId()) && !isRevertToOnOrder(piece)) {

      return inventoryManager.getOrCreateHoldingsRecord(poLine.getInstanceId(), receivedPieceLocationId, requestContext)
        .thenCompose(holdingId -> {
          processedHoldings.put(receivedPieceLocationId + poLine.getInstanceId(), holdingId);
          return completedFuture(true);
        })
        .exceptionally(t -> {
          logger.error("Cannot create holding for specified piece location {}", piece.getLocationId());
          addError(piece.getPoLineId(), piece.getId(), ITEM_UPDATE_FAILED.toError());
          return false;
        });
    } else {
      return completedFuture(true);
    }
  }

  private boolean ifHoldingNotProcessed(String locationId, String instanceId) {
    return processedHoldingsParams.add(locationId + instanceId);
  }

  private boolean holdingUpdateOnCheckinReceiveRequired(Piece piece, String locationId, CompositePoLine poLine) {
    boolean isHoldingUpdateRequired;
    if (piece.getFormat() == Piece.Format.ELECTRONIC) {
      isHoldingUpdateRequired = PoLineCommonUtil.isHoldingUpdateRequiredForEresource(poLine.getEresource());
    } else {
      isHoldingUpdateRequired = PoLineCommonUtil.isHoldingUpdateRequiredForPhysical(poLine.getPhysical());
    }
    return isHoldingUpdateRequired
      && StringUtils.isNotEmpty(locationId);
  }

  /**
   * Retrieves item records from inventory storage
   *
   * @param piecesWithItems
   *          map with item id as a key and piece record as a value
   * @return future with list of item records
   */
  private CompletableFuture<List<JsonObject>> getItemRecords(Map<String, Piece> piecesWithItems, RequestContext requestContext) {
    // Split all id's by maximum number of id's for get query
    List<CompletableFuture<List<JsonObject>>> futures = StreamEx
      .ofSubLists(new ArrayList<>(piecesWithItems.keySet()), MAX_IDS_FOR_GET_RQ)
      // Get item records from Inventory storage
      .map(ids -> getItemRecordsByIds(ids, piecesWithItems, requestContext))
      .toList();

    return collectResultsOnSuccess(futures)
      .thenApply(lists -> StreamEx.of(lists).toFlatList(jsonObjects -> jsonObjects));
  }

  /**
   * Returns list of item records for specified id's.
   *
   * @param ids
   *          List of item id's
   * @param piecesWithItems
   *          map with item id as a key and piece record as a value
   * @return future with list of item records
   */
  private CompletableFuture<List<JsonObject>> getItemRecordsByIds(List<String> ids, Map<String, Piece> piecesWithItems,
                                                                  RequestContext requestContext) {
    return inventoryManager.getItemRecordsByIds(ids, requestContext)
      .thenApply(items -> {
        checkIfAllItemsFound(ids, items, piecesWithItems);
        return items;
      })
      .exceptionally(e -> {
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
   * @param expectedItemIds
   *          list of expected item id's
   * @param piecesWithItems
   *          map with item id as a key and piece record as a value
   * @param items
   *          found item records
   */
  private void checkIfAllItemsFound(List<String> expectedItemIds, List<JsonObject> items,
      Map<String, Piece> piecesWithItems) {
    // Handle the case when for some reason some items are not found
    if (items.size() < expectedItemIds.size()) {
      List<String> foundItemIds = StreamEx.of(items).map(inventoryManager::extractId).toList();

      expectedItemIds.stream()
        .filter(id -> !foundItemIds.contains(id))
        .forEach(itemId -> {
          Piece piece = piecesWithItems.get(itemId);
          piece.setItemId(null);
          piecesWithItems.remove(itemId);
        });
    }
  }

  /**
   * Collect all piece records with non-empty item ids. The result is a map with
   * item id as a key and piece record as a value
   *
   * @param piecesGroupedByPoLine
   *          map with PO line id as key and list of corresponding pieces as
   *          value
   * @return map with item id as key and piece record as a value
   */
  private Map<String, Piece> collectPiecesWithItemId(Map<String, List<Piece>> piecesGroupedByPoLine) {
    return StreamEx
      .ofValues(piecesGroupedByPoLine)
      .flatMap(List::stream)
      .filter(piece -> StringUtils.isNotEmpty(piece.getItemId()))
      .toMap(Piece::getItemId, piece -> piece);
  }

  /**
   * @param item
   *          inventory item
   * @param piece
   *          piece associated with the item
   * @return future indicating if the item update is successful.
   */
  abstract CompletableFuture<Boolean> receiveInventoryItemAndUpdatePiece(JsonObject item, Piece piece, RequestContext requestContext);

  /**
   * Updates piece records with receiving details which do not have associated
   * item
   *
   * @param piecesGroupedByPoLine
   *          map with PO line id as key and list of corresponding pieces as
   *          value
   * @return updated map passed as a parameter
   */
  abstract Map<String, List<Piece>> updatePieceRecordsWithoutItems(Map<String, List<Piece>> piecesGroupedByPoLine);

  /**
   * Stores updated piece records with receiving details into storage.
   *
   * @param piecesGroupedByPoLine
   *          map with PO line id as key and list of corresponding pieces as
   *          value
   * @return map passed as a parameter
   */
  CompletableFuture<Map<String, List<Piece>>> storeUpdatedPieceRecords(Map<String, List<Piece>> piecesGroupedByPoLine) {
    // Collect all piece records which marked as ready to be received and update
    // storage
    CompletableFuture<?>[] futures = StreamEx
      .ofValues(piecesGroupedByPoLine)
      .flatMap(List::stream)
      .filter(this::isSuccessfullyProcessedPiece)
      .map(this::storeUpdatedPieceRecord)
      .toArray(new CompletableFuture[0]);

    return CompletableFuture.allOf(futures)
      .thenApply(v -> piecesGroupedByPoLine);
  }

  /**
   * Sends request to update piece record with receiving/check-in details in the storage.
   * In case of an error updating the piece, this is collected to return in
   * the response to client
   *
   * @param piece
   *          {@link Piece} with receiving information
   */
  private CompletableFuture<Void> storeUpdatedPieceRecord(Piece piece) {
    String pieceId = piece.getId();
    return handlePutRequest(resourceByIdPath(PIECES, pieceId), JsonObject.mapFrom(piece), httpClient, okapiHeaders,
        logger)
          .exceptionally(e -> {
            addError(getPoLineIdByPieceId(pieceId), pieceId, PIECE_UPDATE_FAILED.toError());
            return null;
          });
  }


  protected List<String> getPoLineIdsForUpdatedPieces(Map<String, List<Piece>> piecesGroupedByPoLine) {
    return EntryStream
      .of(piecesGroupedByPoLine)
      .filter(entry -> entry.getValue()
        .stream()
        // Check if there is at least one piece record which processed
        // successfully
        .anyMatch(this::isSuccessfullyProcessedPiece))
      .keys()
      .toList();
  }

  public CompletableFuture<List<PoLine>> getPoLines(List<String> poLineIds) {
    if(poLineList == null) {
      return collectResultsOnSuccess(StreamEx
        .ofSubLists(poLineIds, MAX_IDS_FOR_GET_RQ)
        // Transform piece id's to CQL query
        .map(HelperUtils::convertIdsToCqlQuery)
        // Send get request for each CQL query
        .map(this::getPoLinesByQuery)
        .toList())
        .thenApply(lists -> StreamEx.of(lists).toFlatList(poLines -> poLines))
        .thenApply(list -> {
          poLineList = list;
          return list;
        });
    } else {
      return completedFuture(poLineList
        .stream()
        .filter(poLine -> poLineIds.contains(poLine.getId()))
        .collect(Collectors.toList()));
    }
  }

  public CompletableFuture<List<CompositePoLine>> getCompositePoLines(List<String> poLineIds, RequestContext requestContext) {
    return getPoLines(poLineIds)
      .thenCompose(poLines -> {
        List<String> ids = poLines.stream().filter(poLine -> !poLine.getIsPackage()).map(PoLine::getId).collect(Collectors.toList());
        return titlesService.getTitlesByPoLineIds(ids, requestContext)
          .thenApply(lineIdTitles -> HelperUtils.verifyNonPackageTitles(lineIdTitles, ids))
          .thenApply(titles -> populateInstanceId(poLines, titles));
      });
  }

  private List<CompositePoLine> populateInstanceId(List<PoLine> poLines, Map<String, List<Title>> titles) {
    return poLines.stream().map(poLine -> convertToCompositePoLine(poLine, titles)).collect(Collectors.toList());
  }

  protected CompositePoLine convertToCompositePoLine(PoLine poLine, Map<String, List<Title>> titles) {
    poLine.setAlerts(null);
    poLine.setReportingCodes(null);
    JsonObject jsonLine = JsonObject.mapFrom(poLine);
    Title title = titles.get(poLine.getId()).get(0);
    return jsonLine.mapTo(CompositePoLine.class).withInstanceId(title.getInstanceId()).withTitleOrPackage(title.getTitle());
  }

  private CompletableFuture<List<PoLine>> getPoLinesByQuery(String query) {
    return poLineHelper.getPoLines(MAX_IDS_FOR_GET_RQ, 0, query)
      .thenApply(PoLineCollection::getPoLines)
      .exceptionally(e -> {
        logger.error("The issue happened getting PO Lines", e);
        return null;
      });
  }

  protected List<Piece> getSuccessfullyProcessedPieces(String poLineId, Map<String, List<Piece>> piecesGroupedByPoLine) {
    return StreamEx.of(piecesGroupedByPoLine.get(poLineId))
      .filter(this::isSuccessfullyProcessedPiece)
      .toList();
  }

  /**
   * @param poLine
   *          PO Line record from storage
   * @param pieces
   *          list of pieces
   * @return resulting PO Line status
   */
  protected CompletableFuture<ReceiptStatus> calculatePoLineReceiptStatus(PoLine poLine, List<Piece> pieces) {
    // Search for pieces with Expected status
    return pieces.isEmpty()
      // No successfully pieces processed - receipt status unchanged
      ? completedFuture(poLine.getReceiptStatus())
      : getPiecesQuantityByPoLineAndStatus(poLine.getId(), Piece.ReceivingStatus.EXPECTED)
        // Calculate receipt status
        .thenCompose(expectedQty -> calculatePoLineReceiptStatus(expectedQty, poLine, pieces))
        .exceptionally(e -> {
          logger.error("The expected receipt status for PO Line '{}' cannot be calculated", poLine.getId(), e);
          return null;
      });
  }

  /**
   * Returns "Fully Received" status if quantity of expected piece records is
   * zero, otherwise checks how many received pieces. If quantity of received
   * piece records is zero, returns "Awaiting Receipt" status, otherwise -
   * "Partially Received"
   *
   * @param expectedPiecesQuantity
   *          expected piece records quantity
   * @param poLine
   *          PO Line record representation from storage
   * @return completable future holding calculated PO Line's receipt status
   */
  private CompletableFuture<ReceiptStatus> calculatePoLineReceiptStatus(int expectedPiecesQuantity, PoLine poLine,
      List<Piece> pieces) {
    // Fully Received:If receiving and there is no expected piece remaining
    if (!poLine.getCheckinItems().equals(Boolean.TRUE) && expectedPiecesQuantity == 0) {
      return CompletableFuture.completedFuture(FULLY_RECEIVED);
    }
    // Partially Received: In case there is at least one successfully received
    // piece
    if (StreamEx.of(pieces).anyMatch(piece -> Piece.ReceivingStatus.RECEIVED == piece.getReceivingStatus())) {
      return CompletableFuture.completedFuture(PARTIALLY_RECEIVED);
    }
    // Pieces were rolled-back to Expected. In this case we have to check if
    // there is any Received piece in the storage
    return getPiecesQuantityByPoLineAndStatus(poLine.getId(), Piece.ReceivingStatus.RECEIVED)
      .thenApply(receivedQty -> receivedQty == 0 ? AWAITING_RECEIPT : PARTIALLY_RECEIVED);
  }

  public CompletableFuture<Integer> getPiecesQuantityByPoLineAndStatus(String poLineId,
      Piece.ReceivingStatus receivingStatus) {
    String query = String.format(PIECES_BY_POL_ID_AND_STATUS_QUERY, poLineId, receivingStatus.value());
    // Limit to 0 because only total number is important
    String endpoint = String.format(PIECES_WITH_QUERY_ENDPOINT, 0, lang, encodeQuery(query, logger));
    // Search for pieces with Expected status
    return handleGetRequest(endpoint, httpClient, okapiHeaders, logger)
      // Return total records quantity
      .thenApply(json -> json.mapTo(PieceCollection.class).getTotalRecords());
  }

  /**
   * Checks if there is processing error for particular piece record
   * @param piece piece record to get error for
   * @return {@code true} if there is an error associated with piece record
   */
  private boolean isSuccessfullyProcessedPiece(Piece piece) {
    return getError(piece.getPoLineId(), piece.getId()) == null;
  }

  /**
   * Gets processing error for particular piece record
   * @param polId PO Line id
   * @param pieceId piece id
   * @return error object if presents or null
   */
  private Error getError(String polId, String pieceId) {
    return Optional.ofNullable(processingErrors.get(polId))
     .map(errors -> errors.get(pieceId))
     .orElse(null);
  }

  void addError(String polId, String pieceId, Error error) {
    processingErrors.computeIfAbsent(polId, k -> new HashMap<>())
      .put(pieceId, error);
  }

  public void calculateProcessingErrors(String poLineId, ReceivingResult result,
      Map<String, Piece> processedPiecesForPoLine, Map<String, Integer> resultCounts, String pieceId) {
    // Calculate processing status
    ProcessingStatus status = new ProcessingStatus();
    Error error = getError(poLineId, pieceId);
    if (processedPiecesForPoLine.get(pieceId) != null && error == null) {
      status.setType(ProcessingStatus.Type.SUCCESS);
      resultCounts.merge(ProcessingStatus.Type.SUCCESS.toString(), 1, Integer::sum);
    } else {
      status.setType(ProcessingStatus.Type.FAILURE);
      status.setError(error);
      resultCounts.merge(ProcessingStatus.Type.FAILURE.toString(), 1, Integer::sum);
    }

    ReceivingItemResult itemResult = new ReceivingItemResult();
    itemResult.setPieceId(pieceId);
    itemResult.setProcessingStatus(status);
    result.getReceivingItemResults().add(itemResult);
  }

  /**
   * Filter by locationId presence for items/pieces related to POLine.
   *
   * @return {@link CompletableFuture} which holds map with PO line id as key
   *         and list of corresponding pieces as value
   */
  CompletableFuture<Map<String, List<Piece>>> filterMissingLocations(Map<String, List<Piece>> piecesRecords) {
    return getPoLines(StreamEx.ofKeys(piecesRecords).toList())
      .thenApply(poLines -> {
        for(PoLine poLine : poLines) {
          piecesRecords.get(poLine.getId()).removeIf(piece -> isMissingLocation(poLine, piece));
        }
        return piecesRecords;
      });
  }

  private boolean isMissingLocation(PoLine poLine, Piece piece) {
    // Check if locationId doesn't presented in piece from request and retrieved from storage
    // Corresponding piece from collection
    if (getLocationId(piece) == null && !isRevertToOnOrder(piece)) {
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
    }
    return false;
  }

  abstract String getLocationId(Piece piece);

  /**
   * Updates items in the inventory storage with check-in/receiving details if any. On
   * success updates corresponding records as received
   *
   * @return {@link CompletableFuture} which holds map with PO line id as key
   *         and list of corresponding pieces as value
   */
  CompletableFuture<Map<String, List<Piece>>> updateInventoryItems(Map<String, Map<String, String>> pieceLocationsGroupedByPoLine,
      Map<String, List<Piece>> piecesGroupedByPoLine, RequestContext requestContext) {
    // Collect all piece records with non-empty item ids. The result is a map
    // with item id as a key and piece record as a value
    Map<String, Piece> piecesWithItems = collectPiecesWithItemId(piecesGroupedByPoLine);
    List<String> poLineIds = new ArrayList<>(piecesGroupedByPoLine.keySet());

    return getCompositePoLines(poLineIds, requestContext)
      .thenCompose(poLines -> processHoldingsUpdate(pieceLocationsGroupedByPoLine, piecesGroupedByPoLine, poLines, requestContext)
        .thenCompose(v -> getItemRecords(piecesWithItems, requestContext))
        .thenCompose(items -> processItemsUpdate(pieceLocationsGroupedByPoLine, piecesGroupedByPoLine, items, poLines, requestContext)));
  }

  private CompletableFuture<Map<String, List<Piece>>> processItemsUpdate(
      Map<String, Map<String, String>> pieceLocationsGroupedByPoLine, Map<String, List<Piece>> piecesGroupedByPoLine,
      List<JsonObject> items, List<CompositePoLine> poLines, RequestContext requestContext) {
    List<CompletableFuture<Boolean>> futuresForItemsUpdates = new ArrayList<>();
    Map<String, Piece> piecesWithItems = collectPiecesWithItemId(piecesGroupedByPoLine);

    // If there are no pieces with ItemId, continue
    if (piecesWithItems.isEmpty()) {
      return completedFuture(piecesGroupedByPoLine);
    }

    for (JsonObject item : items) {
      String itemId = item.getString(ID);
      Piece piece = piecesWithItems.get(itemId);

      CompositePoLine poLine = searchPoLineById(poLines, piece);
      if (poLine == null)
        continue;

      String pieceLocation = pieceLocationsGroupedByPoLine.get(poLine.getId())
        .get(piece.getId());
      if (holdingUpdateOnCheckinReceiveRequired(piece, pieceLocation, poLine) && !isRevertToOnOrder(piece)) {
        String holdingId = processedHoldings.get(pieceLocation + poLine.getInstanceId());
        item.put(ITEM_HOLDINGS_RECORD_ID, holdingId);
      }
      futuresForItemsUpdates.add(receiveInventoryItemAndUpdatePiece(item, piece, requestContext));
    }
    return collectResultsOnSuccess(futuresForItemsUpdates).thenApply(results -> {
      if (logger.isDebugEnabled()) {
        long successQty = results.stream()
          .filter(result -> result)
          .count();
        logger.debug("{} out of {} inventory item(s) successfully updated", successQty, results.size());
      }
      return piecesGroupedByPoLine;
    });
  }

  private CompletableFuture<Void> processHoldingsUpdate(Map<String, Map<String, String>> pieceLocationsGroupedByPoLine,
      Map<String, List<Piece>> piecesGroupedByPoLine, List<CompositePoLine> poLines, RequestContext requestContext) {
    List<CompletableFuture<Boolean>> futuresForHoldingsUpdates = new ArrayList<>();
    StreamEx.ofValues(piecesGroupedByPoLine)
      .flatMap(List::stream)
      .forEach(piece -> {
        CompositePoLine poLine = searchPoLineById(poLines, piece);
        String receivedPieceLocationId = pieceLocationsGroupedByPoLine.get(poLine.getId()).get(piece.getId());

        if (holdingUpdateOnCheckinReceiveRequired(piece, receivedPieceLocationId, poLine)) {
          futuresForHoldingsUpdates.add(createHoldingsForChangedLocations(piece, poLine, receivedPieceLocationId, requestContext));
        }
      });

    return collectResultsOnSuccess(futuresForHoldingsUpdates).thenAccept(results -> {
      if (logger.isDebugEnabled()) {
        long successQty = results.stream()
          .filter(result -> result)
          .count();
        logger.debug("{} out of {} holdings successfully processed", successQty, results.size());
      }
    });
  }

  private CompletableFuture<?>[] getListOfRestrictionCheckingFutures(List<PurchaseOrder> orders,  Map<String, List<PoLine>> poLinesGroupedByOrderId,
                                                                     Map<String, Map<String, T>> pieces) {
    return orders.stream().map(order -> protectionService.isOperationRestricted(order.getAcqUnitIds(), ProtectedOperationType.UPDATE, getRequestContext())
      .exceptionally(t -> {
        for (PoLine line : poLinesGroupedByOrderId.get(order.getId())) {
          for (String pieceId : pieces.remove(line.getId()).keySet()) {
            addError(line.getId(), pieceId, USER_HAS_NO_PERMISSIONS.toError());
          }
        }
        return null;
      })).toArray(CompletableFuture[]::new);
  }

  CompletableFuture<Void> removeForbiddenEntities(List<PoLine> poLines, Map<String, Map<String, T>> pieces) {
    if(!poLines.isEmpty()) {
      Map<String, List<PoLine>> poLinesGroupedByOrderId = poLines.stream().collect(groupingBy(PoLine::getPurchaseOrderId));
      String query = buildQuery(convertIdsToCqlQuery(poLinesGroupedByOrderId.keySet()), logger);
      String url = String.format(GET_PURCHASE_ORDERS, poLinesGroupedByOrderId.size(), 0, query, lang);
      return handleGetRequest(url, httpClient, okapiHeaders, logger)
        .thenCompose(json -> {
          List<PurchaseOrder> orders = json.mapTo(PurchaseOrderCollection.class).getPurchaseOrders();
          return CompletableFuture.allOf(getListOfRestrictionCheckingFutures(orders, poLinesGroupedByOrderId, pieces));
        });
    } else {
      return CompletableFuture.completedFuture(null);
    }
  }
}
