package org.folio.service.pieces.flows.update;

import static java.util.concurrent.CompletableFuture.completedFuture;
import static org.folio.orders.utils.ProtectedOperationType.UPDATE;

import java.util.concurrent.CompletableFuture;

import org.apache.logging.log4j.LogManager;
import org.apache.logging.log4j.Logger;
import org.folio.completablefuture.FolioVertxCompletableFuture;
import org.folio.models.pieces.PieceUpdateHolder;
import org.folio.rest.core.models.RequestContext;
import org.folio.rest.jaxrs.model.Piece;
import org.folio.service.ProtectionService;
import org.folio.service.finance.transaction.ReceivingEncumbranceStrategy;
import org.folio.service.inventory.InventoryManager;
import org.folio.service.orders.PurchaseOrderLineService;
import org.folio.service.orders.PurchaseOrderService;
import org.folio.service.pieces.PieceService;
import org.folio.service.pieces.PieceStorageService;
import org.folio.service.pieces.flows.PieceFlowUpdatePoLineKey;
import org.folio.service.pieces.flows.PieceFlowUpdatePoLineStrategyResolver;

import io.vertx.core.json.JsonObject;

public class PieceUpdateFlowManager {
  private static final Logger logger = LogManager.getLogger(PieceUpdateFlowManager.class);

  private final PieceStorageService pieceStorageService;
  private final PieceService pieceService;
  private final ProtectionService protectionService;
  private final PurchaseOrderService purchaseOrderService;
  private final PurchaseOrderLineService purchaseOrderLineService;
  private final InventoryManager inventoryManager;
  private final ReceivingEncumbranceStrategy receivingEncumbranceStrategy;
  private final PieceFlowUpdatePoLineStrategyResolver pieceFlowUpdatePoLineStrategyResolver;

  public PieceUpdateFlowManager(PieceStorageService pieceStorageService, PieceService pieceService, ProtectionService protectionService,
    PurchaseOrderService purchaseOrderService, PurchaseOrderLineService purchaseOrderLineService, InventoryManager inventoryManager,
    ReceivingEncumbranceStrategy receivingEncumbranceStrategy, PieceFlowUpdatePoLineStrategyResolver pieceFlowUpdatePoLineStrategyResolver) {
    this.pieceStorageService = pieceStorageService;
    this.pieceService = pieceService;
    this.protectionService = protectionService;
    this.purchaseOrderService = purchaseOrderService;
    this.purchaseOrderLineService = purchaseOrderLineService;
    this.inventoryManager = inventoryManager;
    this.receivingEncumbranceStrategy = receivingEncumbranceStrategy;
    this.pieceFlowUpdatePoLineStrategyResolver = pieceFlowUpdatePoLineStrategyResolver;
  }
  // Flow to update piece
  // 1. Before update, get piece by id from storage and store receiving status
  // 2. Update piece with new content and complete future
  // 3. Create a message and check if receivingStatus is not consistent with storage; if yes - send a message to event bus
  public CompletableFuture<Void> updatePiece(Piece pieceToUpdate, boolean createItem, RequestContext requestContext) {
    CompletableFuture<Void> future = new FolioVertxCompletableFuture<>(requestContext.getContext());
    PieceUpdateHolder holder = new PieceUpdateHolder(pieceToUpdate, createItem);
    pieceStorageService.getPieceById(pieceToUpdate.getId(), requestContext)
      .thenCompose(pieceFromStorage -> purchaseOrderLineService.getOrderLineById(pieceToUpdate.getPoLineId(), requestContext)
        .thenCompose(poLine -> purchaseOrderService.getPurchaseOrderById(poLine.getPurchaseOrderId(), requestContext)
          .thenAccept(purchaseOrder -> holder.shallowCopy(new PieceUpdateHolder(purchaseOrder, poLine).withPieceFromStorage(pieceFromStorage)))
        ))
      .thenCompose(purchaseOrder -> protectionService.isOperationRestricted(holder.getOriginPurchaseOrder().getAcqUnitIds(), UPDATE, requestContext))
      .thenCompose(v -> inventoryManager.updateItemWithPoLineId(holder.getPieceToUpdate().getItemId(), holder.getPieceToUpdate().getPoLineId(), requestContext))
      .thenAccept(vVoid ->
        pieceStorageService.getPieceById(holder.getPieceToUpdate().getId(), requestContext).thenAccept(pieceStorage -> {
            Piece.ReceivingStatus receivingStatusStorage = pieceStorage.getReceivingStatus();
            pieceStorageService.updatePiece(holder.getPieceToUpdate(), requestContext)
              .thenAccept(aVoid -> updatePoLine(holder, requestContext))
              .thenAccept(future::complete)
              .thenAccept(afterUpdate -> {

                JsonObject messageToEventBus = new JsonObject();
                messageToEventBus.put("poLineIdUpdate", holder.getPieceToUpdate().getPoLineId());

                Piece.ReceivingStatus receivingStatusUpdate = holder.getPieceToUpdate().getReceivingStatus();
                logger.debug("receivingStatusStorage -- {}", receivingStatusStorage);
                logger.debug("receivingStatusUpdate -- {}", receivingStatusUpdate);

                if (receivingStatusStorage.compareTo(receivingStatusUpdate) != 0) {
                  pieceService.receiptConsistencyPiecePoLine(messageToEventBus, requestContext);
                }
              })
              .exceptionally(e -> {
                logger.error("Error updating piece by id to storage {}", holder.getPieceToUpdate().getId(), e);
                future.completeExceptionally(e);
                return null;
              });
          })
          .exceptionally(e -> {
            logger.error("Error getting piece by id from storage {}", holder.getPieceToUpdate().getId(), e);
            future.completeExceptionally(e);
            return null;
          })
      )
      .exceptionally(t -> {
        logger.error("User to update piece with id={}", holder.getPieceToUpdate().getId(), t.getCause());
        future.completeExceptionally(t);
        return null;
      });
    return future;
  }

  private CompletableFuture<Void> updatePoLine(PieceUpdateHolder holder, RequestContext requestContext) {
    if (!Boolean.TRUE.equals(holder.getOriginPoLine().getIsPackage()) && !Boolean.TRUE.equals(holder.getOriginPoLine().getCheckinItems()) ) {
      return FolioVertxCompletableFuture.from(requestContext.getContext(), completedFuture(poLineUpdateQuantity(holder))
        .thenCompose(aHolder -> receivingEncumbranceStrategy.processEncumbrances(holder.getPurchaseOrderToSave(),
          holder.getPurchaseOrderToSave(), requestContext))
        .thenAccept(v -> purchaseOrderLineService.updateOrderLine(holder.getPoLineToSave(), requestContext)));
    }
    return CompletableFuture.completedFuture(null);
  }

  private PieceUpdateHolder poLineUpdateQuantity(PieceUpdateHolder holder) {
    PieceFlowUpdatePoLineKey key = new PieceFlowUpdatePoLineKey().withIsPackage(holder.getPoLineToSave().getIsPackage())
      .withOrderWorkFlowStatus(holder.getPurchaseOrderToSave().getWorkflowStatus())
      .withPieceFlowType(PieceFlowUpdatePoLineKey.PieceFlowType.PIECE_UPDATE_FLOW);
    pieceFlowUpdatePoLineStrategyResolver.resolve(key).ifPresent(strategy -> {
      strategy.updateQuantity(1, holder.getPieceToUpdate(), holder.getPoLineToSave());
    });
    return holder;
  }
}
