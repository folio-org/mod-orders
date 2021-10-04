package org.folio.service.pieces.flows.create;

import static java.util.concurrent.CompletableFuture.completedFuture;

import java.util.concurrent.CompletableFuture;

import org.apache.logging.log4j.LogManager;
import org.apache.logging.log4j.Logger;
import org.folio.completablefuture.FolioVertxCompletableFuture;
import org.folio.models.pieces.PieceCreationHolder;
import org.folio.orders.utils.ProtectedOperationType;
import org.folio.rest.core.models.RequestContext;
import org.folio.rest.jaxrs.model.Piece;
import org.folio.service.ProtectionService;
import org.folio.service.finance.transaction.ReceivingEncumbranceStrategy;
import org.folio.service.orders.PurchaseOrderLineService;
import org.folio.service.orders.PurchaseOrderService;
import org.folio.service.pieces.PieceStorageService;
import org.folio.service.pieces.flows.PieceFlowUpdatePoLineKey;
import org.folio.service.pieces.flows.PieceFlowUpdatePoLineStrategyResolver;

public class PieceCreateFlowManager {
  private static final Logger logger = LogManager.getLogger(PieceCreateFlowManager.class);

  private final PieceStorageService pieceStorageService;
  private final PurchaseOrderLineService purchaseOrderLineService;
  private final PurchaseOrderService purchaseOrderService;
  private final ProtectionService protectionService;
  private final ReceivingEncumbranceStrategy receivingEncumbranceStrategy;
  private final PieceCreateFlowInventoryManager pieceCreateFlowInventoryManager;
  private final PieceFlowUpdatePoLineStrategyResolver pieceFlowUpdatePoLineStrategyResolver;
  private final PieceCreateFlowValidator pieceCreateFlowValidator;

  public PieceCreateFlowManager(PieceStorageService pieceStorageService, PurchaseOrderLineService purchaseOrderLineService,
        PurchaseOrderService purchaseOrderService, ProtectionService protectionService,
        ReceivingEncumbranceStrategy receivingEncumbranceStrategy, PieceCreateFlowInventoryManager pieceCreateFlowInventoryManager,
        PieceFlowUpdatePoLineStrategyResolver pieceFlowUpdatePoLineStrategyResolver, PieceCreateFlowValidator pieceCreateFlowValidator) {
    this.pieceStorageService = pieceStorageService;
    this.purchaseOrderLineService = purchaseOrderLineService;
    this.purchaseOrderService = purchaseOrderService;
    this.protectionService = protectionService;
    this.receivingEncumbranceStrategy = receivingEncumbranceStrategy;
    this.pieceCreateFlowInventoryManager = pieceCreateFlowInventoryManager;
    this.pieceFlowUpdatePoLineStrategyResolver = pieceFlowUpdatePoLineStrategyResolver;
    this.pieceCreateFlowValidator = pieceCreateFlowValidator;
  }

  public CompletableFuture<Piece> createPiece(Piece piece, boolean createItem, RequestContext requestContext) {
    logger.info("manual createPiece start");
    PieceCreationHolder holder = new PieceCreationHolder(piece, createItem);
    return purchaseOrderLineService.getOrderLineById(piece.getPoLineId(), requestContext)
      .thenCompose(poLine -> purchaseOrderService.getPurchaseOrderById(poLine.getPurchaseOrderId(), requestContext)
                                .thenAccept(purchaseOrder -> holder.shallowCopy(new PieceCreationHolder(purchaseOrder, poLine)))
      )
      .thenAccept(v -> pieceCreateFlowValidator.isCreatePieceRequestValid(holder))
      .thenCompose(order -> protectionService.isOperationRestricted(holder.getOriginPurchaseOrder().getAcqUnitIds(),
                                                                      ProtectedOperationType.CREATE, requestContext))
      .thenCompose(v -> pieceCreateFlowInventoryManager.processInventory(holder, requestContext))
      .thenAccept(compPoLine -> updatePoLine(holder, requestContext))
      .thenCompose(v -> pieceStorageService.insertPiece(piece, requestContext));
  }

  private CompletableFuture<Void> updatePoLine(PieceCreationHolder holder, RequestContext requestContext) {
    if (!Boolean.TRUE.equals(holder.getOriginPoLine().getIsPackage()) && !Boolean.TRUE.equals(holder.getOriginPoLine().getCheckinItems()) ) {
      return FolioVertxCompletableFuture.from(requestContext.getContext(), completedFuture(poLineUpdateQuantity(holder))
                          .thenCompose(aHolder -> receivingEncumbranceStrategy.processEncumbrances(holder.getPurchaseOrderToSave(),
                            holder.getOriginPurchaseOrder(), requestContext))
                          .thenAccept(v -> purchaseOrderLineService.updateOrderLine(holder.getPoLineToSave(), requestContext)));
    }
    return CompletableFuture.completedFuture(null);
  }

  private PieceCreationHolder poLineUpdateQuantity(PieceCreationHolder holder) {
    PieceFlowUpdatePoLineKey key = new PieceFlowUpdatePoLineKey().withIsPackage(holder.getPoLineToSave().getIsPackage())
        .withOrderWorkFlowStatus(holder.getPurchaseOrderToSave().getWorkflowStatus())
        .withPieceFlowType(PieceFlowUpdatePoLineKey.PieceFlowType.PIECE_CREATE_FLOW);
    pieceFlowUpdatePoLineStrategyResolver.resolve(key).ifPresent(strategy ->
                    strategy.updateQuantity(1, holder.getPieceToCreate(), holder.getPoLineToSave()));
    return holder;
  }

}
