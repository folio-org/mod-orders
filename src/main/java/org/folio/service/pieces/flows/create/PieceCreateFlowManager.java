package org.folio.service.pieces.flows.create;

import org.apache.logging.log4j.LogManager;
import org.apache.logging.log4j.Logger;
import org.folio.models.pieces.PieceCreationHolder;
import org.folio.orders.utils.ProtectedOperationType;
import org.folio.rest.core.models.RequestContext;
import org.folio.rest.jaxrs.model.Piece;
import org.folio.service.ProtectionService;
import org.folio.service.pieces.PieceStorageService;
import org.folio.service.pieces.flows.BasePieceFlowHolderBuilder;
import org.folio.service.pieces.flows.DefaultPieceFlowsValidator;

import io.vertx.core.Future;

public class PieceCreateFlowManager {
  private static final Logger logger = LogManager.getLogger(PieceCreateFlowManager.class);

  private final PieceStorageService pieceStorageService;
  private final ProtectionService protectionService;
  private final PieceCreateFlowInventoryManager pieceCreateFlowInventoryManager;
  private final DefaultPieceFlowsValidator defaultPieceFlowsValidator;
  private final PieceCreateFlowPoLineService pieceCreateFlowPoLineService;
  private final BasePieceFlowHolderBuilder basePieceFlowHolderBuilder;

  public PieceCreateFlowManager(PieceStorageService pieceStorageService, ProtectionService protectionService,
        PieceCreateFlowInventoryManager pieceCreateFlowInventoryManager, DefaultPieceFlowsValidator defaultPieceFlowsValidator,
        PieceCreateFlowPoLineService pieceCreateFlowPoLineService, BasePieceFlowHolderBuilder basePieceFlowHolderBuilder) {
    this.pieceStorageService = pieceStorageService;
    this.protectionService = protectionService;
    this.pieceCreateFlowInventoryManager = pieceCreateFlowInventoryManager;
    this.pieceCreateFlowPoLineService = pieceCreateFlowPoLineService;
    this.defaultPieceFlowsValidator = defaultPieceFlowsValidator;
    this.basePieceFlowHolderBuilder = basePieceFlowHolderBuilder;
  }

  public Future<Piece> createPiece(Piece pieceToCreate, boolean createItem, RequestContext requestContext) {
    logger.info("manual createPiece start");
    PieceCreationHolder holder = new PieceCreationHolder().withPieceToCreate(pieceToCreate).withCreateItem(createItem);
    return basePieceFlowHolderBuilder.updateHolderWithOrderInformation(holder, requestContext)
      .compose(aHolder -> basePieceFlowHolderBuilder.updateHolderWithTitleInformation(holder, requestContext))
      .map(v -> {defaultPieceFlowsValidator.isPieceRequestValid(pieceToCreate, holder.getOriginPoLine(), createItem); return null;})
      .compose(order -> protectionService.isOperationRestricted(holder.getTitle().getAcqUnitIds(), ProtectedOperationType.CREATE, requestContext))
      .compose(v -> pieceCreateFlowInventoryManager.processInventory(holder.getPurchaseOrderToSave(), holder.getPoLineToSave(), holder.getPieceToCreate(), holder.isCreateItem(), requestContext))
      .compose(compPoLine -> updatePoLine(holder, requestContext))
      .compose(v -> pieceStorageService.insertPiece(pieceToCreate, requestContext));
  }

  protected Future<Void> updatePoLine(PieceCreationHolder holder, RequestContext requestContext) {
    if (!Boolean.TRUE.equals(holder.getOriginPoLine().getIsPackage()) && !Boolean.TRUE.equals(holder.getOriginPoLine().getCheckinItems())) {
      return pieceCreateFlowPoLineService.updatePoLine(holder, requestContext);
    }
    return Future.succeededFuture();
  }
}
