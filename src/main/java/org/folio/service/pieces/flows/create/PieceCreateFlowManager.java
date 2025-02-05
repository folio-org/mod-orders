package org.folio.service.pieces.flows.create;

import static org.folio.orders.utils.FutureUtils.asFuture;
import static org.folio.orders.utils.HelperUtils.collectResultsOnSuccess;

import java.util.List;
import java.util.stream.Collectors;

import io.vertx.core.Future;
import org.apache.logging.log4j.LogManager;
import org.apache.logging.log4j.Logger;
import org.folio.models.pieces.PieceBatchCreationHolder;
import org.folio.models.pieces.PieceCreationHolder;
import org.folio.orders.utils.ProtectedOperationType;
import org.folio.rest.core.models.RequestContext;
import org.folio.rest.jaxrs.model.Piece;
import org.folio.rest.jaxrs.model.PieceCollection;
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

  public Future<Piece> createPiece(Piece pieceToCreate, boolean createItem, RequestContext requestContext) {
    logger.info("createPiece:: manual createPiece start, poLineId: {}, receivingTenantId: {}",
      pieceToCreate.getPoLineId(), pieceToCreate.getReceivingTenantId());
    PieceCreationHolder holder = new PieceCreationHolder().withPieceToCreate(pieceToCreate).withCreateItem(createItem);
    return basePieceFlowHolderBuilder.updateHolderWithOrderInformation(holder, requestContext)
      .compose(v -> basePieceFlowHolderBuilder.updateHolderWithTitleInformation(holder, requestContext))
      .compose(v -> asFuture(() -> defaultPieceFlowsValidator.isPieceRequestValid(pieceToCreate, holder.getOriginPurchaseOrder(), holder.getOriginPoLine(), createItem)))
      .compose(v -> protectionService.isOperationRestricted(holder.getTitle().getAcqUnitIds(), ProtectedOperationType.CREATE, requestContext))
      .compose(v -> processInventory(holder, requestContext))
      .compose(v -> updatePoLine(holder, requestContext))
      .compose(v -> pieceStorageService.insertPiece(pieceToCreate, requestContext));
  }

  public Future<PieceCollection> createPieces(PieceCollection pieceCollection, boolean createItem, RequestContext requestContext) {
    logger.info("createPieces:: Trying to create '{}' pieces", pieceCollection.getPieces().size());
    if (pieceCollection.getPieces().isEmpty()) {
      logger.info("createPieces:: No pieces to create");
      return Future.succeededFuture();
    }
    var holder = new PieceBatchCreationHolder().withPieceToCreate(pieceCollection).withCreateItem(createItem);
    return basePieceFlowHolderBuilder.updateHolderWithOrderInformation(holder, requestContext)
      .compose(v -> basePieceFlowHolderBuilder.updateHolderWithTitleInformation(holder, requestContext))
      .compose(v -> asFuture(() -> defaultPieceFlowsValidator.isPieceBatchRequestValid(holder.getPiecesToCreate(), holder.getOriginPurchaseOrder(), holder.getOriginPoLine(), createItem)))
      .compose(v -> protectionService.isOperationRestricted(holder.getTitle().getAcqUnitIds(), ProtectedOperationType.CREATE, requestContext))
      .compose(v -> processInventory(holder, requestContext))
      .compose(v -> updatePoLine(holder, requestContext))
      .compose(v -> pieceStorageService.insertPieceBatch(holder.getPiecesToCreate(), requestContext));
  }

  private Future<Void> processInventory(PieceCreationHolder holder, RequestContext requestContext) {
    return pieceCreateFlowInventoryManager.processInventory(holder.getPurchaseOrderToSave(),
      holder.getPoLineToSave(), holder.getPieceToCreate(), holder.isCreateItem(), requestContext);
  }

  private Future<Void> processInventory(PieceBatchCreationHolder holder, RequestContext requestContext) {
    var futures = holder.getPiecesToCreate().stream()
      .map(piece -> pieceCreateFlowInventoryManager.processInventory(holder.getPurchaseOrderToSave(),
        holder.getPoLineToSave(), piece, holder.isCreateItem(), requestContext))
      .toList();
    return collectResultsOnSuccess(futures).mapEmpty();
  }

  protected Future<Void> updatePoLine(PieceBatchCreationHolder holder, RequestContext requestContext) {
    var pieceCreationHolderList = createPieceCreationHolderList(holder);
    var futures = pieceCreationHolderList.stream()
      .map(pieceCreationHolder -> updatePoLine(pieceCreationHolder, requestContext))
      .toList();
    return collectResultsOnSuccess(futures).mapEmpty();
  }

  private List<PieceCreationHolder> createPieceCreationHolderList(PieceBatchCreationHolder holder) {
    return holder.getPiecesToCreate().stream()
      .map(piece -> createPieceCreationHolder(piece, holder))
      .toList();
  }

  private PieceCreationHolder createPieceCreationHolder(Piece piece, PieceBatchCreationHolder holder) {
    var pieceCreationHolder = new PieceCreationHolder()
      .withPieceToCreate(piece)
      .withCreateItem(holder.isCreateItem());
    pieceCreationHolder
      .withTitleInformation(holder.getTitle())
      .withPoLineOnly(holder.getPoLineToSave())
      .withOrderInformation(holder.getPurchaseOrderToSave());
    return pieceCreationHolder;
  }

  protected Future<Void> updatePoLine(PieceCreationHolder holder, RequestContext requestContext) {
    if (!Boolean.TRUE.equals(holder.getOriginPoLine().getIsPackage()) && !Boolean.TRUE.equals(holder.getOriginPoLine().getCheckinItems())) {
      return pieceCreateFlowPoLineService.updatePoLine(holder, requestContext);
    }
    return Future.succeededFuture();
  }
}
