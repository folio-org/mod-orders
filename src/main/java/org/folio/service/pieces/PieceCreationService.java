package org.folio.service.pieces;

import org.apache.logging.log4j.LogManager;
import org.apache.logging.log4j.Logger;
import org.folio.models.pieces.PieceCreationHolder;
import org.folio.orders.utils.ProtectedOperationType;
import org.folio.rest.core.models.RequestContext;
import org.folio.rest.jaxrs.model.Piece;
import org.folio.service.ProtectionService;
import org.folio.service.finance.transaction.ReceivingEncumbranceStrategy;
import org.folio.service.orders.PurchaseOrderLineService;
import org.folio.service.orders.PurchaseOrderService;

import java.util.concurrent.CompletableFuture;

public class PieceCreationService {
  private static final Logger logger = LogManager.getLogger(PieceCreationService.class);

  private final PieceStorageService pieceStorageService;
  private final PurchaseOrderLineService purchaseOrderLineService;
  private final PurchaseOrderService purchaseOrderService;
  private final ProtectionService protectionService;
  private final ReceivingEncumbranceStrategy receivingEncumbranceStrategy;

  public PieceCreationService(PieceStorageService pieceStorageService, PurchaseOrderLineService purchaseOrderLineService,
    PurchaseOrderService purchaseOrderService, ProtectionService protectionService,
    ReceivingEncumbranceStrategy receivingEncumbranceStrategy) {
    this.pieceStorageService = pieceStorageService;
    this.purchaseOrderLineService = purchaseOrderLineService;
    this.purchaseOrderService = purchaseOrderService;
    this.protectionService = protectionService;
    this.receivingEncumbranceStrategy = receivingEncumbranceStrategy;
  }

  public CompletableFuture<Piece> createPiece(Piece piece, RequestContext requestContext) {
    logger.info("manual createPiece start");
    PieceCreationHolder holder = new PieceCreationHolder(piece);
    return purchaseOrderLineService.getOrderLineById(piece.getPoLineId(), requestContext)
      .thenCompose(poLine -> purchaseOrderService.getPurchaseOrderById(poLine.getPurchaseOrderId(), requestContext)
        .thenAccept(purchaseOrder -> {
          holder.shallowCopy(new PieceCreationHolder(purchaseOrder, poLine));
        }))
      .thenCompose(order -> protectionService.isOperationRestricted(holder.getOriginPurchaseOrder().getAcqUnitIds(),
        ProtectedOperationType.CREATE, requestContext))
   //   .thenCompose(compPoLine -> updatePoLineLocationAndCostQuantity(piece, holder.getPoLineToSave(), requestContext))
      .thenCompose(v -> receivingEncumbranceStrategy.processEncumbrances(holder.getPurchaseOrderToSave(), holder.getOriginPurchaseOrder(), requestContext))
      .thenAccept(v -> purchaseOrderLineService.updateOrderLine(holder.getPoLineToSave(), requestContext))
  //    .thenCompose(v -> updateInventory(holder.getPoLineToSave(), piece, requestContext))
      .thenCompose(v -> pieceStorageService.insertPiece(piece, requestContext));
  }

}
