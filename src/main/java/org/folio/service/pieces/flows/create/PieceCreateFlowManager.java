package org.folio.service.pieces.flows.create;

import static org.folio.orders.utils.FutureUtils.asFuture;
import static org.folio.orders.utils.HelperUtils.collectResultsOnSuccess;

import java.util.List;

import io.vertx.core.Future;
import lombok.RequiredArgsConstructor;
import lombok.extern.log4j.Log4j2;
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
import org.folio.service.titles.TitlesService;

@Log4j2
@RequiredArgsConstructor
public class PieceCreateFlowManager {

  private final PieceStorageService pieceStorageService;
  private final TitlesService titlesService;
  private final ProtectionService protectionService;
  private final PieceCreateFlowInventoryManager pieceCreateFlowInventoryManager;
  private final DefaultPieceFlowsValidator defaultPieceFlowsValidator;
  private final PieceCreateFlowPoLineService pieceCreateFlowPoLineService;
  private final BasePieceFlowHolderBuilder basePieceFlowHolderBuilder;

  public Future<Piece> createPiece(Piece pieceToCreate, boolean createItem, RequestContext requestContext) {
    log.info("createPiece:: manual createPiece start, poLineId: {}, receivingTenantId: {}",
      pieceToCreate.getPoLineId(), pieceToCreate.getReceivingTenantId());
    PieceCreationHolder holder = new PieceCreationHolder().withPieceToCreate(pieceToCreate).withCreateItem(createItem);
    return basePieceFlowHolderBuilder.updateHolderWithOrderInformation(holder, requestContext)
      .compose(v -> basePieceFlowHolderBuilder.updateHolderWithTitleInformation(holder, requestContext))
      .compose(v -> asFuture(() -> defaultPieceFlowsValidator.isPieceRequestValid(pieceToCreate, holder.getOriginPurchaseOrder(), holder.getOriginPoLine(), holder.getTitle(), createItem)))
      .compose(v -> protectionService.isOperationRestricted(holder.getTitle().getAcqUnitIds(), ProtectedOperationType.CREATE, requestContext))
      .compose(v -> processInventory(holder, requestContext))
      .compose(v -> updatePoLine(holder, requestContext))
      .compose(v -> titlesService.generateNextSequenceNumbers(List.of(holder.getPieceToCreate()), holder.getTitle(), requestContext))
      .compose(pieces -> pieceStorageService.insertPiece(pieces.getFirst(), requestContext));
  }

  public Future<PieceCollection> createPieces(PieceCollection pieceCollection, boolean createItem, RequestContext requestContext) {
    log.info("createPieces:: Trying to create '{}' pieces", pieceCollection.getPieces().size());
    if (pieceCollection.getPieces().isEmpty()) {
      log.info("createPieces:: No pieces to create");
      return Future.succeededFuture();
    }
    var holder = new PieceBatchCreationHolder().withPieceToCreate(pieceCollection).withCreateItem(createItem);
    return basePieceFlowHolderBuilder.updateHolderWithOrderInformation(holder, requestContext)
      .compose(v -> basePieceFlowHolderBuilder.updateHolderWithTitleInformation(holder, requestContext))
      .compose(v -> asFuture(() -> defaultPieceFlowsValidator.isPieceBatchRequestValid(holder.getPiecesToCreate(), holder.getOriginPurchaseOrder(), holder.getOriginPoLine(), holder.getTitle(), createItem)))
      .compose(v -> protectionService.isOperationRestricted(holder.getTitle().getAcqUnitIds(), ProtectedOperationType.CREATE, requestContext))
      .compose(v -> processInventory(holder, requestContext))
      .compose(v -> updatePoLine(holder, requestContext))
      .compose(v -> titlesService.generateNextSequenceNumbers(holder.getPiecesToCreate(), holder.getTitle(), requestContext))
      .compose(pieces -> pieceStorageService.insertPiecesBatch(pieces, requestContext));
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
