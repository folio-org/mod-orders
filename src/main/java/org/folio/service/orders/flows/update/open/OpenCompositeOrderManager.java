package org.folio.service.orders.flows.update.open;

import static java.util.concurrent.CompletableFuture.completedFuture;
import static java.util.stream.Collectors.collectingAndThen;
import static java.util.stream.Collectors.groupingBy;
import static java.util.stream.Collectors.summingInt;
import static java.util.stream.Collectors.toList;
import static org.folio.orders.utils.HelperUtils.calculateInventoryItemsQuantity;
import static org.folio.orders.utils.HelperUtils.calculatePiecesQuantityWithoutLocation;
import static org.folio.orders.utils.HelperUtils.collectResultsOnSuccess;
import static org.folio.orders.utils.PoLineCommonUtil.groupLocationsByHoldingId;
import static org.folio.orders.utils.PoLineCommonUtil.groupLocationsByLocationId;
import static org.folio.orders.utils.ResourcePathResolver.ALERTS;
import static org.folio.orders.utils.ResourcePathResolver.REPORTING_CODES;
import static org.folio.rest.jaxrs.model.CompositePurchaseOrder.WorkflowStatus.OPEN;

import java.util.ArrayList;
import java.util.Collections;
import java.util.Date;
import java.util.List;
import java.util.Map;
import java.util.Objects;
import java.util.Optional;
import java.util.concurrent.CompletableFuture;
import java.util.concurrent.CompletionException;
import java.util.stream.Collectors;

import org.apache.commons.collections4.CollectionUtils;
import org.apache.commons.lang3.StringUtils;
import org.apache.logging.log4j.LogManager;
import org.apache.logging.log4j.Logger;
import org.folio.completablefuture.AsyncUtil;
import org.folio.completablefuture.FolioVertxCompletableFuture;
import org.folio.helper.PurchaseOrderLineHelper;
import org.folio.orders.events.handlers.MessageAddress;
import org.folio.orders.utils.HelperUtils;
import org.folio.orders.utils.PoLineCommonUtil;
import org.folio.orders.utils.ProtectedOperationType;
import org.folio.rest.core.exceptions.InventoryException;
import org.folio.rest.core.models.RequestContext;
import org.folio.rest.jaxrs.model.Alert;
import org.folio.rest.jaxrs.model.CompositePoLine;
import org.folio.rest.jaxrs.model.CompositePurchaseOrder;
import org.folio.rest.jaxrs.model.Location;
import org.folio.rest.jaxrs.model.Piece;
import org.folio.rest.jaxrs.model.PoLine;
import org.folio.rest.jaxrs.model.ReportingCode;
import org.folio.rest.jaxrs.model.Title;
import org.folio.service.ProtectionService;
import org.folio.service.finance.transaction.EncumbranceWorkflowStrategy;
import org.folio.service.finance.transaction.EncumbranceWorkflowStrategyFactory;
import org.folio.service.inventory.InventoryManager;
import org.folio.service.orders.OrderWorkflowType;
import org.folio.service.orders.PurchaseOrderLineService;
import org.folio.service.orders.PurchaseOrderStorageService;
import org.folio.service.pieces.PieceChangeReceiptStatusPublisher;
import org.folio.service.pieces.PieceStorageService;
import org.folio.service.titles.TitlesService;

import io.vertx.core.json.JsonObject;
import one.util.streamex.StreamEx;

public class OpenCompositeOrderManager {
  private static final Logger logger = LogManager.getLogger(OpenCompositeOrderManager.class);

  private final PurchaseOrderStorageService purchaseOrderStorageService;
  private final PurchaseOrderLineService purchaseOrderLineService;
  private final PurchaseOrderLineHelper purchaseOrderLineHelper;
  private final EncumbranceWorkflowStrategyFactory encumbranceWorkflowStrategyFactory;
  private final InventoryManager inventoryManager;
  private final PieceStorageService pieceStorageService;
  private final ProtectionService protectionService;
  private final PieceChangeReceiptStatusPublisher receiptStatusPublisher;
  private final TitlesService titlesService;
  private final OpenCompositeOrderInventoryService openCompositeOrderInventoryService;
  private final OpenCompositeOrderFlowValidator openCompositeOrderFlowValidator;

  public OpenCompositeOrderManager(PurchaseOrderLineService purchaseOrderLineService, EncumbranceWorkflowStrategyFactory encumbranceWorkflowStrategyFactory, InventoryManager inventoryManager,
    PieceStorageService pieceStorageService, PurchaseOrderStorageService purchaseOrderStorageService, ProtectionService protectionService,
    PieceChangeReceiptStatusPublisher receiptStatusPublisher, TitlesService titlesService, OpenCompositeOrderInventoryService openCompositeOrderInventoryService,
    OpenCompositeOrderFlowValidator openCompositeOrderFlowValidator, PurchaseOrderLineHelper purchaseOrderLineHelper) {
    this.purchaseOrderLineService = purchaseOrderLineService;
    this.encumbranceWorkflowStrategyFactory = encumbranceWorkflowStrategyFactory;
    this.inventoryManager = inventoryManager;
    this.pieceStorageService = pieceStorageService;
    this.purchaseOrderStorageService = purchaseOrderStorageService;
    this.protectionService = protectionService;
    this.receiptStatusPublisher = receiptStatusPublisher;
    this.titlesService = titlesService;
    this.openCompositeOrderInventoryService = openCompositeOrderInventoryService;
    this.openCompositeOrderFlowValidator = openCompositeOrderFlowValidator;
    this.purchaseOrderLineHelper = purchaseOrderLineHelper;
  }

  /**
   * Handles transition of given order to OPEN status.
   *
   * @param compPO Purchase Order to open
   * @return CompletableFuture that indicates when transition is completed
   */
  public CompletableFuture<Void> process(CompositePurchaseOrder compPO, CompositePurchaseOrder poFromStorage, RequestContext requestContext) {
      return AsyncUtil.executeBlocking(requestContext.getContext(), false, () -> updateIncomingOrder(compPO, poFromStorage))
        .thenCompose(aVoid -> openCompositeOrderFlowValidator.validate(compPO, poFromStorage, requestContext))
        .thenCompose(aCompPO -> titlesService.fetchNonPackageTitles(compPO, requestContext))
        .thenCompose(linesIdTitles -> {
          populateInstanceId(linesIdTitles, compPO.getCompositePoLines());
          return processInventory(linesIdTitles, compPO, requestContext);
        })
        .thenCompose(v -> finishProcessingEncumbrancesForOpenOrder(compPO, poFromStorage, requestContext))
        .thenAccept(ok -> changePoLineStatuses(compPO))
        .thenCompose(ok -> openOrderUpdatePoLinesSummary(compPO.getCompositePoLines(), requestContext));
  }

  public CompletableFuture<Void> openOrderUpdatePoLinesSummary(List<CompositePoLine> compositePoLines, RequestContext requestContext) {
    return CompletableFuture.allOf( compositePoLines.stream()
      .map(this::openOrderRemoveLocationId)
      .map(this::openOrderConvertToPoLine)
      .map(line -> purchaseOrderLineService.saveOrderLine(line, requestContext))
      .toArray(CompletableFuture[]::new));
  }

  public PoLine openOrderConvertToPoLine(CompositePoLine compPoLine) {
    JsonObject pol = JsonObject.mapFrom(compPoLine);
    pol.remove(ALERTS);
    pol.remove(REPORTING_CODES);
    PoLine poLine = pol.mapTo(PoLine.class);
    poLine.setAlerts(compPoLine.getAlerts().stream().map(Alert::getId).collect(toList()));
    poLine.setReportingCodes(compPoLine.getReportingCodes().stream().map(ReportingCode::getId).collect(toList()));
    return poLine;
  }


  /**
   * Creates Inventory records associated with given PO line and updates PO line with corresponding links.
   *
   * @param compPOL Composite PO line to update Inventory for
   * @return CompletableFuture with void.
   */
 public CompletableFuture<Void> openOrderUpdateInventory(CompositePoLine compPOL, String titleId, RequestContext requestContext) {
    if (Boolean.TRUE.equals(compPOL.getIsPackage())) {
      return completedFuture(null);
    }

    if (PoLineCommonUtil.isInventoryUpdateNotRequired(compPOL)) {
      // don't create pieces, if no inventory updates and receiving not required
      if (PoLineCommonUtil.isReceiptNotRequired(compPOL.getReceiptStatus())) {
        return completedFuture(null);
      }
      // do not create pieces in case of check-in flow
      if (compPOL.getCheckinItems() != null && compPOL.getCheckinItems()) {
        return completedFuture(null);
      }
      return openOrderCreatePieces(compPOL, titleId, Collections.emptyList(), requestContext).thenRun(
        () -> logger.info("Create pieces for PO Line with '{}' id where inventory updates are not required", compPOL.getId()));
    }

    return inventoryManager.handleInstanceRecord(compPOL, requestContext)
      .thenCompose(compPOLWithInstanceId -> openCompositeOrderInventoryService.handleHoldingsAndItemsRecords(compPOLWithInstanceId, requestContext))
      .thenCompose(piecesWithItemId -> {
        // do not create pieces in case of check-in flow
        if (compPOL.getCheckinItems() != null && compPOL.getCheckinItems()) {
          return completedFuture(null);
        }
        if (PoLineCommonUtil.isReceiptNotRequired(compPOL.getReceiptStatus())) {
          return completedFuture(null);
        }
        //create pieces only if receiving is required
        return openOrderCreatePieces(compPOL, titleId, piecesWithItemId, requestContext);
      });
  }

  /**
   * Creates pieces that are not yet in storage
   *
   * @param compPOL PO line to create Pieces Records for
   * @param expectedPiecesWithItem expected Pieces to create with created associated Items records
   * @return void future
   */
  public CompletableFuture<Void> openOrderCreatePieces(CompositePoLine compPOL, String titleId, List<Piece> expectedPiecesWithItem,
                                                        RequestContext requestContext) {
    int createdItemsQuantity = expectedPiecesWithItem.size();
    logger.info("Get pieces by poLine ID");
    return pieceStorageService.getPiecesByPoLineId(compPOL, requestContext)
      .thenCompose(existingPieces -> {
        List<Piece> piecesToCreate;
        List<Piece> piecesWithLocationToProcess = createPiecesByLocationId(compPOL, expectedPiecesWithItem, existingPieces);
        List<Piece> piecesWithHoldingToProcess = createPiecesByHoldingId(compPOL, expectedPiecesWithItem, existingPieces);

        List<Piece> onlyLocationChangedPieces = getPiecesWithChangedLocation(compPOL, piecesWithLocationToProcess, existingPieces);
        if ((onlyLocationChangedPieces.size() == piecesWithLocationToProcess.size()) && CollectionUtils.isNotEmpty(onlyLocationChangedPieces)) {
          return FolioVertxCompletableFuture.allOf(requestContext.getContext(), onlyLocationChangedPieces.stream()
            .map(piece -> openOrderUpdatePieceRecord(piece, requestContext)).toArray(CompletableFuture[]::new));
        } else {
          piecesToCreate = new ArrayList<>(piecesWithLocationToProcess);
          piecesToCreate.addAll(piecesWithHoldingToProcess);
        }
        piecesToCreate.addAll(createPiecesWithoutLocationId(compPOL, existingPieces));
        piecesToCreate.forEach(piece -> piece.setTitleId(titleId));
        logger.info("Trying to create pieces");
        List<CompletableFuture<Piece>> piecesToCreateFutures = new ArrayList<>();
        piecesToCreate.forEach(piece ->
          piecesToCreateFutures.add(openOrderCreatePiece(piece, requestContext))
        );
        return collectResultsOnSuccess(piecesToCreateFutures)
          .thenAccept(result -> logger.info("Number of created pieces: " + result.size()))
          .exceptionally(th -> {
            logger.error("Piece creation error");
            return null;
          });
      })
      .thenAccept(v -> validateItemsCreation(compPOL, createdItemsQuantity));
  }

  public CompletableFuture<Piece> openOrderCreatePiece(Piece piece, RequestContext requestContext) {
    logger.info("createPiece start");
    return purchaseOrderStorageService.getCompositeOrderByPoLineId(piece.getPoLineId(), requestContext)
      .thenCompose(order -> protectionService.isOperationRestricted(order.getAcqUnitIds(), ProtectedOperationType.CREATE, requestContext)
        .thenApply(v -> order))
      .thenCompose(order -> openOrderUpdateInventory(order.getCompositePoLines().get(0), piece, requestContext))
      .thenCompose(v -> pieceStorageService.insertPiece(piece, requestContext));
  }

  /**
   * Creates Inventory records associated with given PO line and updates PO line with corresponding links.
   *
   * @param compPOL Composite PO line to update Inventory for
   * @return CompletableFuture with void.
   */
  public CompletableFuture<Void> openOrderUpdateInventory(CompositePoLine compPOL, Piece piece, RequestContext requestContext) {
    if (Boolean.TRUE.equals(compPOL.getIsPackage())) {
      return titlesService.getTitleById(piece.getTitleId(), requestContext)
        .thenCompose(title -> inventoryManager.handleInstanceRecord(title, requestContext))
        .thenCompose(title -> titlesService.saveTitle(title, requestContext).thenApply(json -> title))
        .thenCompose(title ->
        {
          if (piece.getHoldingId() != null) {
            return completedFuture(piece.getHoldingId());
          }
          return openCompositeOrderInventoryService.handleHoldingsRecord(compPOL, new Location().withLocationId(piece.getLocationId()), title.getInstanceId(), requestContext)
            .thenApply(holdingId -> {
              piece.setLocationId(null);
              piece.setHoldingId(holdingId);
              return holdingId;
            });
        })
        .thenCompose(holdingId -> {
          if (PoLineCommonUtil.isItemsUpdateRequired(compPOL)) {
            return openCompositeOrderInventoryService.createItemRecord(compPOL, holdingId, requestContext);
          }
          return completedFuture(null);
        })
        .thenAccept(itemId -> Optional.ofNullable(itemId).ifPresent(piece::withItemId));
    }
    else
    {
      return inventoryManager.updateItemWithPoLineId(piece.getItemId(), piece.getPoLineId(), requestContext);
    }
  }

  // Flow to update piece
  // 1. Before update, get piece by id from storage and store receiving status
  // 2. Update piece with new content and complete future
  // 3. Create a message and check if receivingStatus is not consistent with storage; if yes - send a message to event bus
  public CompletableFuture<Void> openOrderUpdatePieceRecord(Piece piece, RequestContext requestContext) {
    CompletableFuture<Void> future = new CompletableFuture<>();
    purchaseOrderStorageService.getCompositeOrderByPoLineId(piece.getPoLineId(), requestContext)
      .thenCompose(order -> protectionService.isOperationRestricted(order.getAcqUnitIds(), ProtectedOperationType.UPDATE, requestContext))
      .thenCompose(v -> inventoryManager.updateItemWithPoLineId(piece.getItemId(), piece.getPoLineId(), requestContext))
      .thenAccept(vVoid ->
        pieceStorageService.getPieceById(piece.getId(), requestContext).thenAccept(pieceStorage -> {
            Piece.ReceivingStatus receivingStatusStorage = pieceStorage.getReceivingStatus();
            pieceStorageService.updatePiece(piece, requestContext)
              .thenAccept(future::complete)
              .thenAccept(afterUpdate -> {

                JsonObject messageToEventBus = new JsonObject();
                messageToEventBus.put("poLineIdUpdate", piece.getPoLineId());

                Piece.ReceivingStatus receivingStatusUpdate = piece.getReceivingStatus();
                logger.debug("receivingStatusStorage -- {}", receivingStatusStorage);
                logger.debug("receivingStatusUpdate -- {}", receivingStatusUpdate);

                if (receivingStatusStorage.compareTo(receivingStatusUpdate) != 0) {
                  receiptStatusPublisher.sendEvent(MessageAddress.RECEIPT_STATUS, messageToEventBus, requestContext);
                }
              })
              .exceptionally(e -> {
                logger.error("Error updating piece by id to storage {}", piece.getId(), e);
                future.completeExceptionally(e);
                return null;
              });
          })
          .exceptionally(e -> {
            logger.error("Error getting piece by id from storage {}", piece.getId(), e);
            future.completeExceptionally(e);
            return null;
          })
      )
      .exceptionally(t -> {
        logger.error("User to update piece with id={}", piece.getId(), t.getCause());
        future.completeExceptionally(t);
        return null;
      });
    return future;
  }

  public List<Piece> createPiecesByLocationId(CompositePoLine compPOL, List<Piece> expectedPiecesWithItem, List<Piece> existingPieces) {
    List<Piece> piecesToCreate = new ArrayList<>();
    // For each location collect pieces that need to be created.
    groupLocationsByLocationId(compPOL)
      .forEach((lineLocationId, lineLocations) -> {
        List<Piece> filteredExistingPieces = filterByLocationId(existingPieces, lineLocationId);
        List<Piece> createdPiecesWithItem = processPiecesWithItemAndLocationId(expectedPiecesWithItem, filteredExistingPieces, lineLocationId);
        piecesToCreate.addAll(createdPiecesWithItem);
        List<Piece> piecesWithoutItem = processPiecesWithoutItemAndLocationId(compPOL, filteredExistingPieces, lineLocationId, lineLocations);
        piecesToCreate.addAll(piecesWithoutItem);
      });
    return piecesToCreate;
  }

  public List<Piece> getPiecesWithChangedLocation(CompositePoLine compPOL, List<Piece> needProcessPieces, List<Piece> existingPieces) {
    Map<String, Map<Piece.Format, Integer>> existingPieceMap = numOfPiecesByFormatAndLocationId(existingPieces, compPOL.getId());
    Map<String, Map<Piece.Format, Integer>> needProcessPiecesMap = numOfPiecesByFormatAndLocationId(needProcessPieces, compPOL.getId());

    List<Piece> piecesForLocationUpdate = new ArrayList<>();
    for (Map.Entry<String, Map<Piece.Format, Integer>> entry : existingPieceMap.entrySet()) {
      String existingPieceLocationId = entry.getKey();
      Map<Piece.Format, Integer> existingPieceQtyMap = entry.getValue();
      for (Map.Entry<Piece.Format, Integer> existPieceFormatQty : existingPieceQtyMap.entrySet()) {
        Map<Piece.Format, Integer> pieceLocationMap = needProcessPiecesMap.get(existingPieceLocationId);
        if (pieceLocationMap == null) {
          needProcessPiecesMap.forEach((newLocationId, value) -> {
            Integer pieceQty = value.get(existPieceFormatQty.getKey());
            if (pieceQty != null && pieceQty.equals(existPieceFormatQty.getValue())) {
              List<Piece> piecesWithUpdatedLocation = existingPieces.stream()
                .filter(piece -> existingPieceLocationId.equals(piece.getLocationId())
                  && existPieceFormatQty.getKey() == piece.getFormat())
                .map(piece -> piece.withLocationId(newLocationId))
                .collect(Collectors.toList());
              piecesForLocationUpdate.addAll(piecesWithUpdatedLocation);
            }
          });
        }
      }
    }
    return piecesForLocationUpdate;
  }

  public List<Piece> createPiecesWithoutLocationId(CompositePoLine compPOL, List<Piece> existingPieces) {
    List<Piece> piecesToCreate = new ArrayList<>();
    Map<Piece.Format, Integer> expectedQuantitiesWithoutLocation = calculatePiecesQuantityWithoutLocation(compPOL);
    Map<Piece.Format, Integer> existingPiecesQuantities = calculateQuantityOfExistingPiecesWithoutLocation(existingPieces);
    expectedQuantitiesWithoutLocation.forEach((format, expectedQty) -> {
      int remainingPiecesQuantity = expectedQty - existingPiecesQuantities.getOrDefault(format, 0);
      if (remainingPiecesQuantity > 0) {
        for (int i = 0; i < remainingPiecesQuantity; i++) {
          piecesToCreate.add(new Piece().withFormat(format).withPoLineId(compPOL.getId()));
        }
      }
    });
    return piecesToCreate;
  }

  public static Map<String, Map<Piece.Format, Integer>> numOfPiecesByFormatAndLocationId(List<Piece> pieces, String poLineId) {
    return pieces.stream()
      .filter(piece -> Objects.nonNull(piece.getPoLineId())
        && Objects.nonNull(piece.getLocationId())
        && piece.getPoLineId().equals(poLineId))
      .collect(groupingBy(Piece::getLocationId, groupingBy(Piece::getFormat, summingInt(q -> 1))));
  }


  private Map<Piece.Format, Integer> calculateQuantityOfExistingPiecesWithoutItem(List<Piece> pieces) {
    return StreamEx.of(pieces)
      .filter(piece -> StringUtils.isEmpty(piece.getItemId()))
      .groupingBy(Piece::getFormat, collectingAndThen(toList(), List::size));
  }

  private Map<Piece.Format, Integer> calculateQuantityOfExistingPiecesWithoutLocation(List<Piece> pieces) {
    return StreamEx.of(pieces)
      .filter(piece -> StringUtils.isEmpty(piece.getLocationId()))
      .groupingBy(Piece::getFormat, collectingAndThen(toList(), List::size));
  }

  private List<Piece> filterByLocationId(List<Piece> pieces, String locationId) {
    return pieces.stream()
      .filter(piece -> locationId.equals(piece.getLocationId()))
      .collect(Collectors.toList());
  }

  private List<Piece> filterByHoldingId(List<Piece> pieces, String holdingId) {
    return pieces.stream()
      .filter(piece -> holdingId.equals(piece.getHoldingId()))
      .collect(Collectors.toList());
  }

  private void validateItemsCreation(CompositePoLine compPOL, int itemsSize) {
    int expectedItemsQuantity = calculateInventoryItemsQuantity(compPOL);
    if (itemsSize != expectedItemsQuantity) {
      String message = String.format("Error creating items for PO Line with '%s' id. Expected %d but %d created",
        compPOL.getId(), expectedItemsQuantity, itemsSize);
      throw new InventoryException(message);
    }
  }

  private CompletableFuture<Void> processInventory(Map<String, List<Title>> lineIdsTitles, CompositePurchaseOrder compPO,
    RequestContext requestContext) {
    return FolioVertxCompletableFuture.allOf(requestContext.getContext(),
      compPO.getCompositePoLines()
        .stream()
        .map(poLine -> openOrderUpdateInventory(poLine, getFirstTitleIdIfExist(lineIdsTitles, poLine), requestContext))
        .toArray(CompletableFuture[]::new)
    );
  }

  private String getFirstTitleIdIfExist(Map<String, List<Title>> lineIdsTitles, CompositePoLine poLine) {
    return Optional.ofNullable(lineIdsTitles.get(poLine.getId()))
      .map(titles -> titles.get(0))
      .map(Title::getId)
      .orElse(null);
  }

  private List<Piece> createPiecesByHoldingId(CompositePoLine compPOL, List<Piece> expectedPiecesWithItem, List<Piece> existingPieces) {
    List<Piece> piecesToCreate = new ArrayList<>();
    // For each location collect pieces that need to be created.
    groupLocationsByHoldingId(compPOL)
      .forEach((existingPieceHoldingId, existingPieceLocations) -> {
        List<Piece> filteredExistingPieces = filterByHoldingId(existingPieces, existingPieceHoldingId);
        List<Piece> createdPiecesWithItem = processPiecesWithItemAndHoldingId(expectedPiecesWithItem, filteredExistingPieces, existingPieceHoldingId);
        piecesToCreate.addAll(createdPiecesWithItem);
        List<Piece> piecesWithoutItem = processPiecesWithoutItemAndHoldingId(compPOL, filteredExistingPieces, existingPieceHoldingId, existingPieceLocations);
        piecesToCreate.addAll(piecesWithoutItem);
      });
    return piecesToCreate;
  }



  private List<Piece> processPiecesWithoutItemAndLocationId(CompositePoLine compPOL, List<Piece> existedPieces, String existingPieceLocationId, List<Location> existingPieceLocations) {
    List<Piece> piecesToCreate = new ArrayList<>();
    Map<Piece.Format, Integer> expectedQuantitiesWithoutItem = HelperUtils.calculatePiecesWithoutItemIdQuantity(compPOL, existingPieceLocations);
    Map<Piece.Format, Integer> existedQuantityWithoutItem = calculateQuantityOfExistingPiecesWithoutItem(existedPieces);
    expectedQuantitiesWithoutItem.forEach((format, expectedQty) -> {
      int remainingPiecesQuantity = expectedQty - existedQuantityWithoutItem.getOrDefault(format, 0);
      if (remainingPiecesQuantity > 0) {
        for (int i = 0; i < remainingPiecesQuantity; i++) {
          piecesToCreate.add(new Piece().withFormat(format).withLocationId(existingPieceLocationId).withPoLineId(compPOL.getId()));
        }
      }
    });
    return piecesToCreate;
  }

  private List<Piece> processPiecesWithoutItemAndHoldingId(CompositePoLine compPOL, List<Piece> existedPieces, String existingPieceHoldingId, List<Location> existingPieceLocations) {
    List<Piece> piecesToCreate = new ArrayList<>();
    Map<Piece.Format, Integer> expectedQuantitiesWithoutItem = HelperUtils.calculatePiecesWithoutItemIdQuantity(compPOL, existingPieceLocations);
    Map<Piece.Format, Integer> existedQuantityWithoutItem = calculateQuantityOfExistingPiecesWithoutItem(existedPieces);
    expectedQuantitiesWithoutItem.forEach((format, expectedQty) -> {
      int remainingPiecesQuantity = expectedQty - existedQuantityWithoutItem.getOrDefault(format, 0);
      if (remainingPiecesQuantity > 0) {
        for (int i = 0; i < remainingPiecesQuantity; i++) {
          piecesToCreate.add(new Piece().withFormat(format).withHoldingId(existingPieceHoldingId).withPoLineId(compPOL.getId()));
        }
      }
    });
    return piecesToCreate;
  }

  private List<Piece> processPiecesWithItemAndLocationId(List<Piece> piecesWithItem, List<Piece> existedPieces, String existingPieceLocationId) {
    List<Piece> expectedPiecesWithItem = filterByLocationId(piecesWithItem, existingPieceLocationId);
    return collectMissingPiecesWithItem(expectedPiecesWithItem, existedPieces);
  }

  private List<Piece> processPiecesWithItemAndHoldingId(List<Piece> piecesWithItem, List<Piece> existedPieces, String existingPieceLocationId) {
    List<Piece> expectedPiecesWithItem = filterByHoldingId(piecesWithItem, existingPieceLocationId);
    return collectMissingPiecesWithItem(expectedPiecesWithItem, existedPieces);
  }

  /**
   * Find pieces for which created items, but which are not yet in the storage.
   *
   * @param piecesWithItem pieces for which created items
   * @param existingPieces pieces from storage
   * @return List of Pieces with itemId that are not in storage.
   */
  private List<Piece> collectMissingPiecesWithItem(List<Piece> piecesWithItem, List<Piece> existingPieces) {
    return piecesWithItem.stream()
      .filter(pieceWithItem -> existingPieces.stream()
        .noneMatch(existingPiece -> pieceWithItem.getItemId().equals(existingPiece.getItemId())))
      .collect(Collectors.toList());
  }

  private CompositePoLine openOrderRemoveLocationId(CompositePoLine compositePoLine) {
    compositePoLine.getLocations().forEach(location ->
    {
      if (location.getHoldingId() != null) {
        location.setLocationId(null);
      }
    });
    return compositePoLine;
  }


  private void populateInstanceId(Map<String, List<Title>> lineIdsTitles, List<CompositePoLine> lines) {
    getNonPackageLines(lines).forEach(line -> {
      if (lineIdsTitles.containsKey(line.getId())) {
        line.setInstanceId(lineIdsTitles.get(line.getId()).get(0).getInstanceId());
      }
    });
  }

  private void changePoLineStatuses(CompositePurchaseOrder compPO) {
    compPO.getCompositePoLines().forEach(poLine -> {
      changeReceiptStatus(compPO, poLine);
      changePaymentStatus(compPO, poLine);
    });
  }

  private void changePaymentStatus(CompositePurchaseOrder compPO, CompositePoLine poLine) {
    if (compPO.getOrderType().equals(CompositePurchaseOrder.OrderType.ONGOING)) {
      poLine.setPaymentStatus(CompositePoLine.PaymentStatus.ONGOING);
    }
    else if (poLine.getPaymentStatus() == CompositePoLine.PaymentStatus.PENDING) {
      poLine.setPaymentStatus(CompositePoLine.PaymentStatus.AWAITING_PAYMENT);
    }
  }

  private void changeReceiptStatus(CompositePurchaseOrder compPO, CompositePoLine poLine) {
    if (compPO.getOrderType().equals(CompositePurchaseOrder.OrderType.ONGOING)) {
      poLine.setReceiptStatus(CompositePoLine.ReceiptStatus.ONGOING);
    }
    else if (poLine.getReceiptStatus() == CompositePoLine.ReceiptStatus.PENDING) {
      poLine.setReceiptStatus(CompositePoLine.ReceiptStatus.AWAITING_RECEIPT);
    }
  }

  private List<CompositePoLine> getNonPackageLines(List<CompositePoLine> compositePoLines) {
    return compositePoLines.stream().filter(line -> !line.getIsPackage()).collect(toList());
  }

  private CompletableFuture<Void> finishProcessingEncumbrancesForOpenOrder(CompositePurchaseOrder compPO, CompositePurchaseOrder poFromStorage,
                            RequestContext requestContext) {
    EncumbranceWorkflowStrategy strategy = encumbranceWorkflowStrategyFactory.getStrategy(OrderWorkflowType.PENDING_TO_OPEN);
    return strategy.processEncumbrances(compPO, poFromStorage, requestContext)
      .handle((v, t) -> {
        if (t == null)
          return CompletableFuture.completedFuture(v);
        // There was an error when processing the encumbrances despite the previous validations.
        // Order lines should be saved to avoid leaving an open order with locationId instead of holdingId.
        return openOrderUpdatePoLinesSummary(compPO.getCompositePoLines(), requestContext)
          .handle((vv, tt) -> {
            throw new CompletionException(t.getCause());
          });
      })
      .thenCompose(v -> v) // wait for future returned by handle
      .thenApply(v -> null);
  }

  private void updateIncomingOrder(CompositePurchaseOrder compPO, CompositePurchaseOrder poFromStorage) {
    compPO.setWorkflowStatus(OPEN);
    compPO.setDateOrdered(new Date());
    if (CollectionUtils.isEmpty(compPO.getCompositePoLines())) {
      CompositePurchaseOrder clonedPoFromStorage = JsonObject.mapFrom(poFromStorage).mapTo(CompositePurchaseOrder.class);
      compPO.setCompositePoLines(clonedPoFromStorage.getCompositePoLines());
    }
    compPO.getCompositePoLines().forEach(poLine -> PoLineCommonUtil.updateLocationsQuantity(poLine.getLocations()));
  }
}
