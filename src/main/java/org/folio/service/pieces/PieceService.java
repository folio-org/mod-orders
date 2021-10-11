package org.folio.service.pieces;

import static java.util.concurrent.CompletableFuture.allOf;
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

import java.util.ArrayList;
import java.util.List;
import java.util.Map;
import java.util.Objects;
import java.util.concurrent.CompletableFuture;
import java.util.concurrent.CompletionException;
import java.util.stream.Collectors;

import javax.ws.rs.core.Response;

import org.apache.commons.lang3.StringUtils;
import org.apache.logging.log4j.LogManager;
import org.apache.logging.log4j.Logger;
import org.folio.orders.events.handlers.MessageAddress;
import org.folio.orders.utils.HelperUtils;
import org.folio.orders.utils.ProtectedOperationType;
import org.folio.rest.core.exceptions.ErrorCodes;
import org.folio.rest.core.exceptions.HttpException;
import org.folio.rest.core.exceptions.InventoryException;
import org.folio.rest.core.models.RequestContext;
import org.folio.rest.jaxrs.model.CompositePoLine;
import org.folio.rest.jaxrs.model.CompositePurchaseOrder;
import org.folio.rest.jaxrs.model.Location;
import org.folio.rest.jaxrs.model.Piece;
import org.folio.rest.jaxrs.model.Piece.ReceivingStatus;
import org.folio.service.ProtectionService;
import org.folio.service.inventory.InventoryManager;
import org.folio.service.orders.PurchaseOrderLineService;
import org.folio.service.orders.PurchaseOrderService;

import io.vertx.core.json.JsonObject;
import one.util.streamex.StreamEx;

public class PieceService {
  private static final Logger logger = LogManager.getLogger(PieceService.class);

  private final PieceStorageService pieceStorageService;
  private final ProtectionService protectionService;
  private final PurchaseOrderService purchaseOrderService;
  private final PurchaseOrderLineService purchaseOrderLineService;
  private final InventoryManager inventoryManager;
  private final PieceChangeReceiptStatusPublisher receiptStatusPublisher;
  private final PieceUpdateInventoryService pieceUpdateInventoryService;

  public PieceService(PieceStorageService pieceStorageService, ProtectionService protectionService,
                      PurchaseOrderLineService purchaseOrderLineService,
                      InventoryManager inventoryManager, PieceChangeReceiptStatusPublisher receiptStatusPublisher,
                      PurchaseOrderService purchaseOrderService, PieceUpdateInventoryService pieceUpdateInventoryService) {
    this.pieceStorageService = pieceStorageService;
    this.protectionService = protectionService;
    this.purchaseOrderLineService = purchaseOrderLineService;
    this.inventoryManager = inventoryManager;
    this.receiptStatusPublisher = receiptStatusPublisher;
    this.purchaseOrderService = purchaseOrderService;
    this.pieceUpdateInventoryService = pieceUpdateInventoryService;
  }

  public CompletableFuture<Piece> openOrderCreatePiece(Piece piece, RequestContext requestContext) {
     logger.info("createPiece start");
      return getCompositeOrderByPoLineId(piece.getPoLineId(), requestContext)
        .thenCompose(order -> protectionService.isOperationRestricted(order.getAcqUnitIds(), ProtectedOperationType.CREATE, requestContext)
                                               .thenApply(v -> order))
        .thenCompose(order -> pieceUpdateInventoryService.updateInventory(order.getCompositePoLines().get(0), piece, requestContext))
        .thenCompose(v -> pieceStorageService.insertPiece(piece, requestContext));
  }

  // Flow to update piece
  // 1. Before update, get piece by id from storage and store receiving status
  // 2. Update piece with new content and complete future
  // 3. Create a message and check if receivingStatus is not consistent with storage; if yes - send a message to event bus
  public CompletableFuture<Void> openOrderUpdatePieceRecord(Piece piece, RequestContext requestContext) {
    CompletableFuture<Void> future = new CompletableFuture<>();
    getOrderByPoLineId(piece.getPoLineId(), requestContext)
      .thenCompose(order -> protectionService.isOperationRestricted(order.getAcqUnitIds(), ProtectedOperationType.UPDATE, requestContext))
      .thenCompose(v -> inventoryManager.updateItemWithPoLineId(piece.getItemId(), piece.getPoLineId(), requestContext))
      .thenAccept(vVoid ->
        pieceStorageService.getPieceById(piece.getId(), requestContext).thenAccept(pieceStorage -> {
          ReceivingStatus receivingStatusStorage = pieceStorage.getReceivingStatus();
            pieceStorageService.updatePiece(piece, requestContext)
            .thenAccept(future::complete)
            .thenAccept(afterUpdate -> {

              JsonObject messageToEventBus = new JsonObject();
              messageToEventBus.put("poLineIdUpdate", piece.getPoLineId());

              ReceivingStatus receivingStatusUpdate = piece.getReceivingStatus();
              logger.debug("receivingStatusStorage -- {}", receivingStatusStorage);
              logger.debug("receivingStatusUpdate -- {}", receivingStatusUpdate);

              if (receivingStatusStorage.compareTo(receivingStatusUpdate) != 0) {
                receiptConsistencyPiecePoLine(messageToEventBus, requestContext);
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

  public CompletableFuture<CompositePurchaseOrder> getOrderByPoLineId(String poLineId, RequestContext requestContext) {
    return purchaseOrderLineService.getOrderLineById(poLineId, requestContext)
      .thenCompose(poLine -> purchaseOrderService.getCompositeOrderById(poLine.getPurchaseOrderId(), requestContext));
  }

  private CompletableFuture<CompositePurchaseOrder> getCompositeOrderByPoLineId(String poLineId, RequestContext requestContext) {
    return purchaseOrderLineService.getOrderLineById(poLineId, requestContext)
                                  .thenCompose(poLine -> getCompositePurchaseOrder(poLine.getPurchaseOrderId(), requestContext));
  }

  public CompletableFuture<CompositePurchaseOrder> getCompositePurchaseOrder(String purchaseOrderId, RequestContext requestContext) {
    return purchaseOrderService.getCompositeOrderById(purchaseOrderId, requestContext)
      .exceptionally(t -> {
        Throwable cause = t.getCause();
        // The case when specified order does not exist
        if (cause instanceof HttpException && ((HttpException) cause).getCode() == Response.Status.NOT_FOUND.getStatusCode()) {
          throw new HttpException(404, ErrorCodes.ORDER_NOT_FOUND);
        }
        throw t instanceof CompletionException ? (CompletionException) t : new CompletionException(cause);
      });
  }

  /**
   * Creates pieces that are not yet in storage
   *
   * @param compPOL PO line to create Pieces Records for
   * @param expectedPiecesWithItem expected Pieces to create with created associated Items records
   * @return void future
   */
  public CompletableFuture<Void> openOrderCreatePieces(CompositePoLine compPOL, String titleId,
                                               List<Piece> expectedPiecesWithItem, boolean isOpenOrderFlow, RequestContext requestContext) {
    int createdItemsQuantity = expectedPiecesWithItem.size();
    // do not create pieces in case of check-in flow
    if (compPOL.getCheckinItems() != null && compPOL.getCheckinItems()) {
      return completedFuture(null);
    }
    logger.info("Get pieces by poLine ID");
    return pieceStorageService.getPiecesByPoLineId(compPOL, requestContext)
      .thenCompose(existingPieces -> {
        List<Piece> piecesToCreate;
        List<Piece> piecesWithLocationToProcess = createPiecesByLocationId(compPOL, expectedPiecesWithItem, existingPieces);
        List<Piece> piecesWithHoldingToProcess = createPiecesByHoldingId(compPOL, expectedPiecesWithItem, existingPieces);

        List<Piece> onlyLocationChangedPieces = getPiecesWithChangedLocation(compPOL, piecesWithLocationToProcess, existingPieces);
        if ((onlyLocationChangedPieces.size() == piecesWithLocationToProcess.size()) && !isOpenOrderFlow) {
          return allOf(onlyLocationChangedPieces.stream()
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

  public List<Piece> createPiecesByLocationId(CompositePoLine compPOL, List<Piece> expectedPiecesWithItem,
    List<Piece> existingPieces) {
    List<Piece> piecesToCreate = new ArrayList<>();
    // For each location collect pieces that need to be created.
    groupLocationsByLocationId(compPOL)
      .forEach((existingPieceLocationId, existingPieceLocations) -> {
        List<Piece> filteredExistingPieces = filterByLocationId(existingPieces, existingPieceLocationId);
        List<Piece> createdPiecesWithItem = processPiecesWithLocationAndItem(expectedPiecesWithItem, filteredExistingPieces, existingPieceLocationId);
        piecesToCreate.addAll(createdPiecesWithItem);
        List<Piece> piecesWithoutItem = processPiecesWithoutItemAndLocationId(compPOL, filteredExistingPieces, existingPieceLocationId, existingPieceLocations);
        piecesToCreate.addAll(piecesWithoutItem);
      });
    return piecesToCreate;
  }

  private List<Piece> createPiecesByHoldingId(CompositePoLine compPOL, List<Piece> expectedPiecesWithItem, List<Piece> existingPieces) {
    List<Piece> piecesToCreate = new ArrayList<>();
    // For each location collect pieces that need to be created.
    groupLocationsByHoldingId(compPOL)
      .forEach((existingPieceHoldingId, existingPieceLocations) -> {
        List<Piece> filteredExistingPieces = filterByHoldingId(existingPieces, existingPieceHoldingId);
        List<Piece> createdPiecesWithItem = processPiecesWithHoldingAndItem(expectedPiecesWithItem, filteredExistingPieces, existingPieceHoldingId);
        piecesToCreate.addAll(createdPiecesWithItem);
        List<Piece> piecesWithoutItem = processPiecesWithoutItemAndHoldingId(compPOL, filteredExistingPieces, existingPieceHoldingId, existingPieceLocations);
        piecesToCreate.addAll(piecesWithoutItem);
      });
    return piecesToCreate;
  }

  public List<Piece> getPiecesWithChangedLocation(CompositePoLine compPOL, List<Piece> needProcessPieces,
    List<Piece> existingPieces) {
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

  private List<Piece> processPiecesWithLocationAndItem(List<Piece> piecesWithItem, List<Piece> existedPieces, String existingPieceLocationId) {
    List<Piece> expectedPiecesWithItem = filterByLocationId(piecesWithItem, existingPieceLocationId);
    return collectMissingPiecesWithItem(expectedPiecesWithItem, existedPieces);
  }

  private List<Piece> processPiecesWithHoldingAndItem(List<Piece> piecesWithItem, List<Piece> existedPieces, String existingPieceLocationId) {
    List<Piece> expectedPiecesWithItem = filterByHoldingId(piecesWithItem, existingPieceLocationId);
    return collectMissingPiecesWithItem(expectedPiecesWithItem, existedPieces);
  }

  private void validateItemsCreation(CompositePoLine compPOL, int itemsSize) {
    int expectedItemsQuantity = calculateInventoryItemsQuantity(compPOL);
    if (itemsSize != expectedItemsQuantity) {
      String message = String.format("Error creating items for PO Line with '%s' id. Expected %d but %d created",
        compPOL.getId(), expectedItemsQuantity, itemsSize);
      throw new InventoryException(message);
    }
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

  public static Map<String, Map<Piece.Format, Integer>> numOfPiecesByFormatAndLocationId(List<Piece> pieces, String poLineId) {
    return pieces.stream()
      .filter(piece -> Objects.nonNull(piece.getPoLineId())
        && Objects.nonNull(piece.getLocationId())
        && piece.getPoLineId().equals(poLineId))
      .collect(groupingBy(Piece::getLocationId, groupingBy(Piece::getFormat, summingInt(q -> 1))));
  }

  public void receiptConsistencyPiecePoLine(JsonObject jsonObj, RequestContext requestContext) {
    logger.debug("Sending event to verify receipt status");

    receiptStatusPublisher.sendEvent(MessageAddress.RECEIPT_STATUS, jsonObj, requestContext);

    logger.debug("Event to verify receipt status - sent");
  }
}
