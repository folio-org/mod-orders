package org.folio.service.pieces.flows.delete;

import static org.folio.orders.utils.ProtectedOperationType.DELETE;

import java.util.concurrent.CompletableFuture;
import java.util.concurrent.CompletionException;

import org.apache.commons.lang3.StringUtils;
import org.folio.models.pieces.PieceCreationHolder;
import org.folio.models.pieces.PieceDeletionHolder;
import org.folio.rest.core.exceptions.ErrorCodes;
import org.folio.rest.core.exceptions.HttpException;
import org.folio.rest.core.models.RequestContext;
import org.folio.rest.jaxrs.model.Piece;
import org.folio.service.ProtectionService;
import org.folio.service.finance.transaction.ReceivingEncumbranceStrategy;
import org.folio.service.inventory.InventoryManager;
import org.folio.service.orders.PurchaseOrderLineService;
import org.folio.service.orders.PurchaseOrderService;
import org.folio.service.pieces.PieceStorageService;
import org.folio.service.pieces.flows.PieceFlowUpdatePoLineKey;
import org.folio.service.pieces.flows.PieceFlowUpdatePoLineStrategyResolver;
import org.folio.service.pieces.flows.create.PieceFlowUpdatePoLineStrategies;

public class PieceDeleteFlowManager {
  private final PieceStorageService pieceStorageService;
  private final ProtectionService protectionService;
  private final PurchaseOrderService purchaseOrderService;
  private final PurchaseOrderLineService purchaseOrderLineService;
  private final InventoryManager inventoryManager;
  private final ReceivingEncumbranceStrategy receivingEncumbranceStrategy;
  private final PieceFlowUpdatePoLineStrategyResolver pieceFlowUpdatePoLineStrategyResolver;

  public PieceDeleteFlowManager(PieceStorageService pieceStorageService, ProtectionService protectionService,
    PurchaseOrderService purchaseOrderService, PurchaseOrderLineService purchaseOrderLineService, InventoryManager inventoryManager,
    ReceivingEncumbranceStrategy receivingEncumbranceStrategy, PieceFlowUpdatePoLineStrategyResolver pieceFlowUpdatePoLineStrategyResolver) {
    this.pieceStorageService = pieceStorageService;
    this.protectionService = protectionService;
    this.purchaseOrderService = purchaseOrderService;
    this.purchaseOrderLineService = purchaseOrderLineService;
    this.inventoryManager = inventoryManager;
    this.receivingEncumbranceStrategy = receivingEncumbranceStrategy;
    this.pieceFlowUpdatePoLineStrategyResolver = pieceFlowUpdatePoLineStrategyResolver;
  }

  public CompletableFuture<Void> deletePieceWithItem(String pieceId, RequestContext requestContext) {
    PieceDeletionHolder holder = new PieceDeletionHolder();
    return pieceStorageService.getPieceById(pieceId, requestContext)
      .thenCompose(piece -> purchaseOrderLineService.getOrderLineById(piece.getPoLineId(), requestContext)
        .thenCompose(poLine -> purchaseOrderService.getPurchaseOrderById(poLine.getPurchaseOrderId(), requestContext)
          .thenAccept(purchaseOrder -> holder.shallowCopy(new PieceDeletionHolder(purchaseOrder, poLine).withPieceToDelete(piece)))
       ))
      .thenCompose(purchaseOrder -> protectionService.isOperationRestricted(holder.getOriginPurchaseOrder().getAcqUnitIds(), DELETE, requestContext))
      .thenCompose(vVoid -> canDeletePiece(holder.getPieceToDelete(), requestContext))
      .thenCompose(aVoid -> pieceStorageService.deletePiece(pieceId, requestContext))
      .thenCompose(aVoid -> deletePieceConnectedItem(holder.getPieceToDelete(), requestContext))
      .thenAccept(compPoLine -> PieceFlowUpdatePoLineStrategies.DELETE.updateQuantity(1, holder.getPieceToDelete(), holder.getPoLineToSave()))
      .thenCompose(v -> receivingEncumbranceStrategy.processEncumbrances(holder.getPurchaseOrderToSave(), holder.getOriginPurchaseOrder(), requestContext))
      .thenAccept(v -> purchaseOrderLineService.updateOrderLine(holder.getPoLineToSave(), requestContext));
  }

  private void poLineUpdateQuantity(PieceCreationHolder holder) {
    PieceFlowUpdatePoLineKey key = new PieceFlowUpdatePoLineKey().withIsPackage(holder.getPoLineToSave().getIsPackage())
      .withOrderWorkFlowStatus(holder.getPurchaseOrderToSave().getWorkflowStatus())
      .withPieceFlowType(PieceFlowUpdatePoLineKey.PieceFlowType.PIECE_DELETE_FLOW);
    pieceFlowUpdatePoLineStrategyResolver.resolve(key)
      .ifPresent(strategy -> strategy.updateQuantity(1, holder.getPieceToCreate(), holder.getPoLineToSave()));
  }

  private CompletableFuture<Void> canDeletePiece(Piece piece, RequestContext requestContext) {
    return inventoryManager.getNumberOfRequestsByItemId(piece.getItemId(), requestContext)
      .thenAccept(numOfRequests -> {
        if (numOfRequests > 0) {
          throw new HttpException(422, ErrorCodes.REQUEST_FOUND.toError());
        }
      });
  }

  private CompletableFuture<Void> deletePieceConnectedItem(Piece piece, RequestContext requestContext) {
    if (StringUtils.isNotEmpty(piece.getItemId())) {
      // Attempt to delete item
      return inventoryManager.deleteItem(piece.getItemId(), requestContext)
        .exceptionally(t -> {
          // Skip error processing if item has already deleted
          if (t instanceof HttpException && ((HttpException) t).getCode() == 404) {
            return null;
          } else {
            throw new CompletionException(t);
          }
        });
    } else {
      return CompletableFuture.completedFuture(null);
    }
  }

}
