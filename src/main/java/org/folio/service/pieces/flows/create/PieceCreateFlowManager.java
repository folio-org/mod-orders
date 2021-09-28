package org.folio.service.pieces.flows.create;

import java.util.ArrayList;
import java.util.List;
import java.util.Optional;
import java.util.concurrent.CompletableFuture;

import org.apache.commons.collections4.CollectionUtils;
import org.apache.commons.collections4.ListUtils;
import org.apache.logging.log4j.LogManager;
import org.apache.logging.log4j.Logger;
import org.folio.models.pieces.PieceCreationHolder;
import org.folio.orders.utils.ProtectedOperationType;
import org.folio.rest.RestConstants;
import org.folio.rest.core.exceptions.HttpException;
import org.folio.rest.core.models.RequestContext;
import org.folio.rest.jaxrs.model.CompositePoLine;
import org.folio.rest.jaxrs.model.Error;
import org.folio.rest.jaxrs.model.Errors;
import org.folio.rest.jaxrs.model.Piece;
import org.folio.service.ProtectionService;
import org.folio.service.finance.transaction.ReceivingEncumbranceStrategy;
import org.folio.service.orders.PurchaseOrderLineService;
import org.folio.service.orders.PurchaseOrderService;
import org.folio.service.pieces.PieceStorageService;
import org.folio.service.pieces.flows.PieceFlowUpdatePoLineKey;
import org.folio.service.pieces.flows.PieceFlowUpdatePoLineStrategyResolver;
import org.folio.service.pieces.validators.PieceValidatorUtil;

public class PieceCreateFlowManager {
  private static final Logger logger = LogManager.getLogger(PieceCreateFlowManager.class);

  private final PieceStorageService pieceStorageService;
  private final PurchaseOrderLineService purchaseOrderLineService;
  private final PurchaseOrderService purchaseOrderService;
  private final ProtectionService protectionService;
  private final ReceivingEncumbranceStrategy receivingEncumbranceStrategy;
  private final PieceCreateFlowInventoryManager pieceCreateFlowInventoryManager;
  private final PieceFlowUpdatePoLineStrategyResolver pieceFlowUpdatePoLineStrategyResolver;

  public PieceCreateFlowManager(PieceStorageService pieceStorageService, PurchaseOrderLineService purchaseOrderLineService,
    PurchaseOrderService purchaseOrderService, ProtectionService protectionService,
    ReceivingEncumbranceStrategy receivingEncumbranceStrategy, PieceCreateFlowInventoryManager pieceCreateFlowInventoryManager,
    PieceFlowUpdatePoLineStrategyResolver pieceFlowUpdatePoLineStrategyResolver) {
    this.pieceStorageService = pieceStorageService;
    this.purchaseOrderLineService = purchaseOrderLineService;
    this.purchaseOrderService = purchaseOrderService;
    this.protectionService = protectionService;
    this.receivingEncumbranceStrategy = receivingEncumbranceStrategy;
    this.pieceCreateFlowInventoryManager = pieceCreateFlowInventoryManager;
    this.pieceFlowUpdatePoLineStrategyResolver = pieceFlowUpdatePoLineStrategyResolver;
  }

  public CompletableFuture<Piece> createPiece(Piece piece, boolean createItem, RequestContext requestContext) {
    logger.info("manual createPiece start");
    PieceCreationHolder holder = new PieceCreationHolder(piece, createItem);
    return purchaseOrderLineService.getOrderLineById(piece.getPoLineId(), requestContext)
      .thenCompose(poLine -> purchaseOrderService.getPurchaseOrderById(poLine.getPurchaseOrderId(), requestContext)
                                .thenAccept(purchaseOrder -> holder.shallowCopy(new PieceCreationHolder(purchaseOrder, poLine)))
      )
      .thenAccept(v -> isIncomingPieceValid(holder))
      .thenCompose(order -> protectionService.isOperationRestricted(holder.getOriginPurchaseOrder().getAcqUnitIds(),
                                                                      ProtectedOperationType.CREATE, requestContext))
      .thenCompose(v -> pieceCreateFlowInventoryManager.updateInventory(holder, requestContext))
      .thenAccept(compPoLine -> poLineUpdateQuantity(holder))
      .thenAccept(v -> purchaseOrderLineService.updateOrderLine(holder.getPoLineToSave(), requestContext))
      .thenCompose(v -> receivingEncumbranceStrategy.processEncumbrances(holder.getPurchaseOrderToSave(), holder.getOriginPurchaseOrder(), requestContext))
      .thenCompose(v -> pieceStorageService.insertPiece(piece, requestContext))
      .thenApply(v -> holder.getPieceToCreate());
  }

  private void poLineUpdateQuantity(PieceCreationHolder holder) {

    PieceFlowUpdatePoLineKey key = new PieceFlowUpdatePoLineKey().withIsPackage(holder.getPoLineToSave().getIsPackage())
                                                      .withOrderWorkFlowStatus(holder.getPurchaseOrderToSave().getWorkflowStatus())
                                                      .withPieceFlowType(PieceFlowUpdatePoLineKey.PieceFlowType.PIECE_CREATE_FLOW);
    pieceFlowUpdatePoLineStrategyResolver.resolve(key)
                        .ifPresent(strategy -> strategy.updateQuantity(1, holder.getPieceToCreate(), holder.getPoLineToSave()));
  }

  private void isIncomingPieceValid(PieceCreationHolder holder) {
    Piece pieceToCreate = holder.getPieceToCreate();
    CompositePoLine originPoLine = holder.getOriginPoLine();
    List<Error> pieceLocationErrors = Optional.ofNullable(PieceValidatorUtil.validatePieceLocation(pieceToCreate)).orElse(new ArrayList<>());
    List<Error> pieceFormatErrors = Optional.ofNullable(PieceValidatorUtil.validatePieceFormat(pieceToCreate, originPoLine)).orElse(new ArrayList<>());
    List<Error> combinedErrors = ListUtils.union(pieceFormatErrors, pieceLocationErrors);
    if (CollectionUtils.isNotEmpty(combinedErrors)) {
      throw new HttpException(RestConstants.BAD_REQUEST, new Errors().withErrors(combinedErrors).withTotalRecords(combinedErrors.size()));
    }
  }

}
