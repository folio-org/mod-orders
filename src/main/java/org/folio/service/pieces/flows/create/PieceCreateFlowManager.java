package org.folio.service.pieces.flows.create;

import java.util.concurrent.CompletableFuture;

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

  public CompletableFuture<Piece> createPiece(Piece pieceToCreate, boolean createItem, RequestContext requestContext) {
    logger.info("manual createPiece start");
    PieceCreationHolder holder = new PieceCreationHolder().withPieceToCreate(pieceToCreate).withCreateItem(createItem);
    return basePieceFlowHolderBuilder.updateHolderWithOrderInformation(holder, requestContext)
      .thenAccept(v -> defaultPieceFlowsValidator.isPieceRequestValid(pieceToCreate, holder.getOriginPoLine(), createItem))
      .thenCompose(order -> protectionService.isOperationRestricted(holder.getOriginPurchaseOrder().getAcqUnitIds(),
                                                                      ProtectedOperationType.CREATE, requestContext))
      .thenCompose(v -> pieceCreateFlowInventoryManager.processInventory(holder.getOriginPoLine(), holder.getPieceToCreate(),
                                                                holder.isCreateItem(), requestContext))
      .thenAccept(compPoLine -> updatePoLine(holder, requestContext))
      .thenCompose(v -> pieceStorageService.insertPiece(pieceToCreate, requestContext));
  }

  protected CompletableFuture<Void> updatePoLine(PieceCreationHolder holder, RequestContext requestContext) {
    if (!Boolean.TRUE.equals(holder.getOriginPoLine().getIsPackage()) && !Boolean.TRUE.equals(holder.getOriginPoLine().getCheckinItems())) {
      return pieceCreateFlowPoLineService.updatePoLine(holder, requestContext);
    }
    return CompletableFuture.completedFuture(null);
  }
}
