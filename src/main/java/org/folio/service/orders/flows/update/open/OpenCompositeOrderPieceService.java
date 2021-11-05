package org.folio.service.orders.flows.update.open;

import static java.util.concurrent.CompletableFuture.completedFuture;
import static java.util.stream.Collectors.toList;
import static org.folio.orders.utils.HelperUtils.calculateInventoryItemsQuantity;
import static org.folio.orders.utils.HelperUtils.collectResultsOnSuccess;

import java.util.ArrayList;
import java.util.List;
import java.util.Optional;
import java.util.concurrent.CompletableFuture;
import java.util.concurrent.CompletionException;

import org.apache.commons.collections4.CollectionUtils;
import org.apache.logging.log4j.LogManager;
import org.apache.logging.log4j.Logger;
import org.folio.models.pieces.OpenOrderPieceHolder;
import org.folio.orders.events.handlers.MessageAddress;
import org.folio.orders.utils.PoLineCommonUtil;
import org.folio.orders.utils.ProtectedOperationType;
import org.folio.rest.core.exceptions.InventoryException;
import org.folio.rest.core.models.RequestContext;
import org.folio.rest.jaxrs.model.CompositePoLine;
import org.folio.rest.jaxrs.model.Location;
import org.folio.rest.jaxrs.model.Piece;
import org.folio.service.ProtectionService;
import org.folio.service.inventory.InventoryManager;
import org.folio.service.orders.PurchaseOrderStorageService;
import org.folio.service.pieces.PieceChangeReceiptStatusPublisher;
import org.folio.service.pieces.PieceStorageService;
import org.folio.service.titles.TitlesService;

import io.vertx.core.json.JsonObject;

public class OpenCompositeOrderPieceService {
  private static final Logger logger = LogManager.getLogger(OpenCompositeOrderPieceService.class);

  private final InventoryManager inventoryManager;
  private final PieceStorageService pieceStorageService;
  private final PieceChangeReceiptStatusPublisher receiptStatusPublisher;
  private final PurchaseOrderStorageService purchaseOrderStorageService;
  private final ProtectionService protectionService;
  private final OpenCompositeOrderHolderBuilder openCompositeOrderHolderBuilder;
  private final TitlesService titlesService;

  public OpenCompositeOrderPieceService(PurchaseOrderStorageService purchaseOrderStorageService,
        PieceStorageService pieceStorageService, ProtectionService protectionService, PieceChangeReceiptStatusPublisher receiptStatusPublisher,
        InventoryManager inventoryManager, TitlesService titlesService, OpenCompositeOrderHolderBuilder openCompositeOrderHolderBuilder) {
    this.purchaseOrderStorageService = purchaseOrderStorageService;
    this.pieceStorageService = pieceStorageService;
    this.protectionService = protectionService;
    this.receiptStatusPublisher = receiptStatusPublisher;
    this.inventoryManager = inventoryManager;
    this.titlesService = titlesService;
    this.openCompositeOrderHolderBuilder = openCompositeOrderHolderBuilder;
  }

  /**
   * Creates pieces that are not yet in storage
   *
   * @param compPOL PO line to create Pieces Records for
   * @param expectedPiecesWithItem expected Pieces to create with created associated Items records
   * @return void future
   */
  public CompletableFuture<List<Piece>> handlePieces(CompositePoLine compPOL, String titleId, List<Piece> expectedPiecesWithItem,
                                              RequestContext requestContext) {
    logger.debug("Get pieces by poLine ID");
    return openCompositeOrderHolderBuilder.buildHolder(compPOL, titleId, expectedPiecesWithItem, requestContext)
      .thenCompose(holder -> {
        if (CollectionUtils.isNotEmpty(holder.getPiecesWithLocationToProcess()) &&
                       (holder.getPiecesWithChangedLocation().size() == holder.getPiecesWithLocationToProcess().size())) {
          return updatePieces(holder, requestContext);
        } else {
          return createPieces(holder, requestContext);
        }
      })
      .thenApply(pieces -> {
        int createdItemsQuantity = expectedPiecesWithItem.size();
        validateItemsCreation(compPOL, createdItemsQuantity);
        return pieces;
      });
  }

  private CompletableFuture<List<Piece>> updatePieces(OpenOrderPieceHolder holder, RequestContext requestContext) {
    List<CompletableFuture<Piece>> updateFutures = holder.getPiecesWithChangedLocation().stream()
      .map(piece -> updatePieceRecord(piece, requestContext).thenApply(v -> piece))
      .collect(toList());
    return collectResultsOnSuccess(updateFutures);
  }

  private CompletableFuture<List<Piece>> createPieces(OpenOrderPieceHolder holder, RequestContext requestContext) {
    List<Piece> piecesToCreate = new ArrayList<>(holder.getPiecesWithLocationToProcess());
    piecesToCreate.addAll(holder.getPiecesWithHoldingToProcess());
    piecesToCreate.addAll(holder.getPiecesWithoutLocationId());
    piecesToCreate.forEach(piece -> piece.setTitleId(holder.getTitleId()));
    logger.debug("Trying to create pieces");
    List<CompletableFuture<Piece>> piecesToCreateFutures = new ArrayList<>();
    piecesToCreate.forEach(piece ->
      piecesToCreateFutures.add(createPiece(piece, requestContext))
    );
    return collectResultsOnSuccess(piecesToCreateFutures)
      .exceptionally(th -> {
        logger.error("Piece creation error");
        throw new CompletionException("Piece creation error", th);
      });
  }

  public CompletableFuture<Piece> createPiece(Piece piece, RequestContext requestContext) {
    logger.debug("createPiece start");
    return purchaseOrderStorageService.getCompositeOrderByPoLineId(piece.getPoLineId(), requestContext)
      .thenCompose(order -> protectionService.isOperationRestricted(order.getAcqUnitIds(), ProtectedOperationType.CREATE, requestContext)
        .thenApply(v -> order))
      .thenCompose(order -> openOrderUpdateInventory(order.getCompositePoLines().get(0), piece, requestContext))
      .thenCompose(v -> pieceStorageService.insertPiece(piece, requestContext));
  }

  public CompletableFuture<Void> updatePieceRecord(Piece piece, RequestContext requestContext) {
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
          return inventoryManager.handleHoldingsRecord(compPOL, new Location().withLocationId(piece.getLocationId()), title.getInstanceId(), requestContext)
            .thenApply(holdingId -> {
              piece.setLocationId(null);
              piece.setHoldingId(holdingId);
              return holdingId;
            });
        })
        .thenCompose(holdingId -> {
          if (PoLineCommonUtil.isItemsUpdateRequired(compPOL)) {
            return inventoryManager.openOrderCreateItemRecord(compPOL, holdingId, requestContext);
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


  private void validateItemsCreation(CompositePoLine compPOL, int itemsSize) {
    int expectedItemsQuantity = calculateInventoryItemsQuantity(compPOL);
    if (itemsSize != expectedItemsQuantity) {
      String message = String.format("Error creating items for PO Line with '%s' id. Expected %d but %d created",
        compPOL.getId(), expectedItemsQuantity, itemsSize);
      throw new InventoryException(message);
    }
  }
}
