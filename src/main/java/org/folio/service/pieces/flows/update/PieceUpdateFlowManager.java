package org.folio.service.pieces.flows.update;

import static org.folio.orders.utils.ProtectedOperationType.UPDATE;

import java.util.concurrent.CompletableFuture;

import org.apache.logging.log4j.LogManager;
import org.apache.logging.log4j.Logger;
import org.folio.completablefuture.FolioVertxCompletableFuture;
import org.folio.models.pieces.PieceUpdateHolder;
import org.folio.rest.core.models.RequestContext;
import org.folio.rest.jaxrs.model.Piece;
import org.folio.service.ProtectionService;
import org.folio.service.pieces.PieceService;
import org.folio.service.pieces.PieceStorageService;
import org.folio.service.pieces.flows.BasePieceFlowHolderBuilder;

import io.vertx.core.json.JsonObject;

public class PieceUpdateFlowManager {
  private static final Logger logger = LogManager.getLogger(PieceUpdateFlowManager.class);

  private final PieceStorageService pieceStorageService;
  private final PieceService pieceService;
  private final ProtectionService protectionService;
  private final PieceUpdateFlowPoLineService updatePoLineService;
  private final PieceUpdateFlowInventoryManager pieceUpdateFlowInventoryManager;
  private final BasePieceFlowHolderBuilder basePieceFlowHolderBuilder;

  public PieceUpdateFlowManager(PieceStorageService pieceStorageService, PieceService pieceService, ProtectionService protectionService,
                  PieceUpdateFlowPoLineService updatePoLineService, PieceUpdateFlowInventoryManager pieceUpdateFlowInventoryManager,
                  BasePieceFlowHolderBuilder basePieceFlowHolderBuilder) {
    this.pieceStorageService = pieceStorageService;
    this.pieceService = pieceService;
    this.protectionService = protectionService;
    this.updatePoLineService = updatePoLineService;
    this.pieceUpdateFlowInventoryManager = pieceUpdateFlowInventoryManager;
    this.basePieceFlowHolderBuilder = basePieceFlowHolderBuilder;
  }
  // Flow to update piece
  // 1. Before update, get piece by id from storage and store receiving status
  // 2. Update piece with new content and complete future
  // 3. Create a message and check if receivingStatus is not consistent with storage; if yes - send a message to event bus
  public CompletableFuture<Void> updatePiece(Piece pieceToUpdate, boolean createItem, boolean deleteHolding, RequestContext requestContext) {
    CompletableFuture<Void> future = new FolioVertxCompletableFuture<>(requestContext.getContext());
    PieceUpdateHolder holder = new PieceUpdateHolder().withPieceToUpdate(pieceToUpdate).withCreateItem(createItem)
                                                      .withDeleteHolding(deleteHolding);
    pieceStorageService.getPieceById(pieceToUpdate.getId(), requestContext)
      .thenAccept(holder::withPieceFromStorage)
      .thenCompose(aHolder -> basePieceFlowHolderBuilder.updateHolderWithOrderInformation(holder, requestContext))
      .thenCompose(purchaseOrder -> protectionService.isOperationRestricted(holder.getOriginPurchaseOrder().getAcqUnitIds(), UPDATE, requestContext))
      .thenCompose(v -> pieceUpdateFlowInventoryManager.processInventory(holder, requestContext))
      .thenCompose(vVoid -> updatePoLine(holder, requestContext))
      .thenAccept(afterUpdate -> {
        JsonObject messageToEventBus = new JsonObject();
        messageToEventBus.put("poLineIdUpdate", holder.getPieceToUpdate().getPoLineId());
        Piece.ReceivingStatus receivingStatusStorage = holder.getPieceFromStorage().getReceivingStatus();
        Piece.ReceivingStatus receivingStatusUpdate = holder.getPieceToUpdate().getReceivingStatus();
        logger.debug("receivingStatusStorage -- {}", receivingStatusStorage);
        logger.debug("receivingStatusUpdate -- {}", receivingStatusUpdate);
        if (receivingStatusStorage.compareTo(receivingStatusUpdate) != 0) {
          pieceService.receiptConsistencyPiecePoLine(messageToEventBus, requestContext);
        }
      })
      .thenCompose(aVoid -> pieceStorageService.updatePiece(holder.getPieceToUpdate(), requestContext))
      .thenAccept(future::complete)
      .exceptionally(t -> {
        logger.error("User to update piece with id={}", holder.getPieceToUpdate().getId(), t.getCause());
        future.completeExceptionally(t);
        return null;
      });
    return future;
  }

  protected CompletableFuture<Void> updatePoLine(PieceUpdateHolder holder, RequestContext requestContext) {
    if (!Boolean.TRUE.equals(holder.getOriginPoLine().getIsPackage()) && !Boolean.TRUE.equals(holder.getOriginPoLine().getCheckinItems())) {
      return updatePoLineService.updatePoLine(holder, requestContext);
    }
    return CompletableFuture.completedFuture(null);
  }

}
