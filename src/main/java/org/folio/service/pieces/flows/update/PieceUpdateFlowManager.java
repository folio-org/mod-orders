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
  private final ReceivingEncumbranceStrategy receivingEncumbranceStrategy;
  private final PieceFlowUpdatePoLineStrategyResolver pieceFlowUpdatePoLineStrategyResolver;
  private final PieceUpdateFlowInventoryManager pieceUpdateFlowInventoryManager;

  public PieceUpdateFlowManager(PieceStorageService pieceStorageService, PieceService pieceService, ProtectionService protectionService,
    PurchaseOrderService purchaseOrderService, PurchaseOrderLineService purchaseOrderLineService,
    ReceivingEncumbranceStrategy receivingEncumbranceStrategy,
    PieceFlowUpdatePoLineStrategyResolver pieceFlowUpdatePoLineStrategyResolver,
    PieceUpdateFlowInventoryManager pieceUpdateFlowInventoryManager) {
    this.pieceStorageService = pieceStorageService;
    this.pieceService = pieceService;
    this.protectionService = protectionService;
    this.purchaseOrderService = purchaseOrderService;
    this.purchaseOrderLineService = purchaseOrderLineService;
    this.receivingEncumbranceStrategy = receivingEncumbranceStrategy;
    this.pieceFlowUpdatePoLineStrategyResolver = pieceFlowUpdatePoLineStrategyResolver;
    this.pieceUpdateFlowInventoryManager = pieceUpdateFlowInventoryManager;
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
          .thenAccept(purchaseOrder -> holder.shallowCopy(new PieceUpdateHolder(purchaseOrder, poLine)).withPieceFromStorage(pieceFromStorage))
        ))
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

  private CompletableFuture<Void> updatePoLine(PieceUpdateHolder holder, RequestContext requestContext) {
    if (!Boolean.TRUE.equals(holder.getOriginPoLine().getIsPackage()) &&
            !Boolean.TRUE.equals(holder.getOriginPoLine().getCheckinItems()) &&
                      isLocationUpdated(holder)) {
      return FolioVertxCompletableFuture.from(requestContext.getContext(), completedFuture(poLineUpdateQuantity(holder))
        .thenCompose(aHolder -> receivingEncumbranceStrategy.processEncumbrances(holder.getPurchaseOrderToSave(),
                                              holder.getPurchaseOrderToSave(), requestContext))
        .thenAccept(v -> purchaseOrderLineService.updateOrderLine(holder.getPoLineToSave(), requestContext)));
    }
    return CompletableFuture.completedFuture(null);
  }

  private PieceUpdateHolder poLineUpdateQuantity(PieceUpdateHolder holder) {
    PieceFlowUpdatePoLineKey keyDelete = new PieceFlowUpdatePoLineKey().withIsPackage(holder.getPoLineToSave().getIsPackage())
      .withOrderWorkFlowStatus(holder.getPurchaseOrderToSave().getWorkflowStatus())
      .withPieceFlowType(PieceFlowUpdatePoLineKey.PieceFlowType.PIECE_DELETE_FLOW);
    pieceFlowUpdatePoLineStrategyResolver.resolve(keyDelete).ifPresent(strategy -> {
      strategy.updateQuantity(1, holder.getPieceFromStorage(), holder.getPoLineToSave());
    });

    PieceFlowUpdatePoLineKey keyAdd = new PieceFlowUpdatePoLineKey().withIsPackage(holder.getPoLineToSave().getIsPackage())
      .withOrderWorkFlowStatus(holder.getPurchaseOrderToSave().getWorkflowStatus())
      .withPieceFlowType(PieceFlowUpdatePoLineKey.PieceFlowType.PIECE_CREATE_FLOW);
    pieceFlowUpdatePoLineStrategyResolver.resolve(keyAdd).ifPresent(strategy -> {
      strategy.updateQuantity(1, holder.getPieceToUpdate(), holder.getPoLineToSave());
    });
    return holder;
  }

  private boolean isLocationUpdated(PieceUpdateHolder pieceUpdateHolder) {
    Piece pieceToUpdate = pieceUpdateHolder.getPieceToUpdate();
    Piece pieceFromStorage = pieceUpdateHolder.getPieceFromStorage();
    return (pieceToUpdate.getLocationId() != null  && pieceFromStorage.getHoldingId() != null) ||
           (pieceToUpdate.getHoldingId() != null && pieceFromStorage.getHoldingId() != null &&
                          !pieceToUpdate.getHoldingId().equals(pieceFromStorage.getHoldingId())) ||
           (pieceToUpdate.getLocationId() != null  && pieceFromStorage.getLocationId() != null &&
                          !pieceToUpdate.getLocationId().equals(pieceFromStorage.getLocationId()));
  }
}
