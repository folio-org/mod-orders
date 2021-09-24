package org.folio.service.pieces;

import static org.folio.rest.core.exceptions.ErrorCodes.PIECE_HOLDING_REFERENCE_IS_NOT_ALLOWED_ERROR;

import java.util.ArrayList;
import java.util.List;
import java.util.Optional;
import java.util.concurrent.CompletableFuture;

import org.apache.logging.log4j.LogManager;
import org.apache.logging.log4j.Logger;
import org.folio.models.pieces.PieceCreationHolder;
import org.folio.orders.utils.ProtectedOperationType;
import org.folio.rest.RestConstants;
import org.folio.rest.core.exceptions.HttpException;
import org.folio.rest.core.models.RequestContext;
import org.folio.rest.jaxrs.model.CompositePurchaseOrder;
import org.folio.rest.jaxrs.model.Error;
import org.folio.rest.jaxrs.model.Errors;
import org.folio.rest.jaxrs.model.Piece;
import org.folio.service.ProtectionService;
import org.folio.service.finance.transaction.ReceivingEncumbranceStrategy;
import org.folio.service.orders.PurchaseOrderLineService;
import org.folio.service.orders.PurchaseOrderService;
import org.folio.service.pieces.validators.PieceValidatorUtil;

public class PieceCreateFlowManager {
  private static final Logger logger = LogManager.getLogger(PieceCreateFlowManager.class);

  private final PieceStorageService pieceStorageService;
  private final PurchaseOrderLineService purchaseOrderLineService;
  private final PurchaseOrderService purchaseOrderService;
  private final ProtectionService protectionService;
  private final ReceivingEncumbranceStrategy receivingEncumbranceStrategy;
  private final PieceCreateFlowInventoryManager pieceCreateFlowInventoryManager;


  public PieceCreateFlowManager(PieceStorageService pieceStorageService, PurchaseOrderLineService purchaseOrderLineService,
    PurchaseOrderService purchaseOrderService, ProtectionService protectionService,
    ReceivingEncumbranceStrategy receivingEncumbranceStrategy, PieceCreateFlowInventoryManager pieceCreateFlowInventoryManager) {
    this.pieceStorageService = pieceStorageService;
    this.purchaseOrderLineService = purchaseOrderLineService;
    this.purchaseOrderService = purchaseOrderService;
    this.protectionService = protectionService;
    this.receivingEncumbranceStrategy = receivingEncumbranceStrategy;
    this.pieceCreateFlowInventoryManager = pieceCreateFlowInventoryManager;
  }

  public CompletableFuture<Piece> createPiece(Piece piece, boolean createItem, RequestContext requestContext) {
    logger.info("manual createPiece start");
    PieceCreationHolder holder = new PieceCreationHolder(piece);
    return purchaseOrderLineService.getOrderLineById(piece.getPoLineId(), requestContext)
      .thenCompose(poLine -> purchaseOrderService.getPurchaseOrderById(poLine.getPurchaseOrderId(), requestContext)
                                .thenAccept(purchaseOrder -> holder.shallowCopy(new PieceCreationHolder(purchaseOrder, poLine)))
      )
      .thenAccept(v -> isIncomingPieceValid(holder))
      .thenCompose(order -> protectionService.isOperationRestricted(holder.getOriginPurchaseOrder().getAcqUnitIds(),
        ProtectedOperationType.CREATE, requestContext))
      .thenAccept(compPoLine -> PieceFlowUpdatePoLineStrategies.ADD.updateQuantity(1, piece, holder.getPoLineToSave()))
      .thenCompose(v -> receivingEncumbranceStrategy.processEncumbrances(holder.getPurchaseOrderToSave(), holder.getOriginPurchaseOrder(), requestContext))
      .thenAccept(v -> purchaseOrderLineService.updateOrderLine(holder.getPoLineToSave(), requestContext))
      .thenCompose(v -> pieceCreateFlowInventoryManager.updateInventory(holder.getPoLineToSave(), piece, createItem, requestContext))
      .thenCompose(v -> pieceStorageService.insertPiece(piece, requestContext));
  }

  private void isIncomingPieceValid(PieceCreationHolder holder) {
    CompositePurchaseOrder originCompPO = holder.getOriginPurchaseOrder();
    Piece pieceToCreate = holder.getPieceToCreate();
    List<Error> errors = Optional.ofNullable(PieceValidatorUtil.validatePieceLocation(pieceToCreate)).orElse(new ArrayList<>());
    if (originCompPO.getWorkflowStatus() == CompositePurchaseOrder.WorkflowStatus.PENDING) {
      if (pieceToCreate.getHoldingId() != null) {
        errors.add(PIECE_HOLDING_REFERENCE_IS_NOT_ALLOWED_ERROR.toError());
      }
    }
    throw new HttpException(RestConstants.BAD_REQUEST, new Errors().withErrors(errors));
  }
}
