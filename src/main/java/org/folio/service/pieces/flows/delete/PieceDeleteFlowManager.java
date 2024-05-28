package org.folio.service.pieces.flows.delete;

import static org.folio.orders.utils.ProtectedOperationType.DELETE;

import static org.folio.service.inventory.InventoryItemManager.ITEM_STATUS;
import static org.folio.service.inventory.InventoryItemManager.ITEM_STATUS_NAME;
import static org.folio.service.orders.utils.HelperUtils.collectResultsOnSuccess;

import org.apache.logging.log4j.LogManager;
import org.apache.logging.log4j.Logger;
import org.folio.models.pieces.PieceDeletionHolder;
import org.folio.rest.RestConstants;
import org.folio.rest.core.exceptions.ErrorCodes;
import org.folio.rest.core.exceptions.HttpException;
import org.folio.rest.core.models.RequestContext;

import org.folio.rest.jaxrs.model.Error;
import org.folio.rest.jaxrs.model.Errors;
import org.folio.rest.jaxrs.model.Piece;
import org.folio.rest.jaxrs.model.PieceCollection;
import org.folio.service.ProtectionService;
import org.folio.service.inventory.InventoryItemManager;
import org.folio.service.ProtectionService;
import org.folio.service.pieces.PieceStorageService;
import org.folio.service.pieces.flows.BasePieceFlowHolderBuilder;

import io.vertx.core.Future;

public class PieceDeleteFlowManager {

  private static final Logger logger = LogManager.getLogger(PieceDeleteFlowManager.class);

  private final PieceDeleteFlowInventoryManager pieceDeleteFlowInventoryManager;
  private final PieceStorageService pieceStorageService;
  private final ProtectionService protectionService;
  private final PieceDeleteFlowPoLineService pieceDeleteFlowPoLineService;
  private final BasePieceFlowHolderBuilder basePieceFlowHolderBuilder;
  private final CirculationRequestsRetriever circulationRequestsRetriever;

  public PieceDeleteFlowManager(PieceDeleteFlowInventoryManager pieceDeleteFlowInventoryManager,
                                PieceStorageService pieceStorageService,
                                ProtectionService protectionService,
                                PieceDeleteFlowPoLineService pieceDeleteFlowPoLineService,
                                BasePieceFlowHolderBuilder basePieceFlowHolderBuilder,
                                CirculationRequestsRetriever circulationRequestsRetriever) {
    this.pieceDeleteFlowInventoryManager = pieceDeleteFlowInventoryManager;
    this.pieceStorageService = pieceStorageService;
    this.protectionService = protectionService;
    this.pieceDeleteFlowPoLineService = pieceDeleteFlowPoLineService;
    this.basePieceFlowHolderBuilder = basePieceFlowHolderBuilder;
    this.circulationRequestsRetriever = circulationRequestsRetriever;
  }

  public Future<Void> deletePiece(String pieceId, boolean deleteHolding, RequestContext requestContext) {
    PieceDeletionHolder holder = new PieceDeletionHolder().withDeleteHolding(deleteHolding);

    return pieceStorageService.getPieceById(pieceId, requestContext)
      .map(holder::withPieceToDelete)
      .compose(aHolder -> basePieceFlowHolderBuilder.updateHolderWithOrderInformation(holder, requestContext))
      .compose(aVoid -> basePieceFlowHolderBuilder.updateHolderWithTitleInformation(holder, requestContext))
      .compose(aVoid -> protectionService.isOperationRestricted(holder.getTitle().getAcqUnitIds(), DELETE, requestContext))
      .compose(aVoid -> isDeletePieceRequestValid(holder, requestContext))
      .compose(aVoid -> pieceDeleteFlowInventoryManager.processInventory(holder, requestContext))
      .compose(pair -> updatePoLine(holder, requestContext))
      .compose(aVoid -> pieceStorageService.deletePiece(holder.getPieceToDelete().getId(), true, requestContext));
  }

  private Future<Void> isDeletePieceRequestValid(PieceDeletionHolder holder, RequestContext requestContext) {
    var piece = holder.getPieceToDelete();
    if (piece.getItemId() == null) {
      return Future.succeededFuture();
    }

    return circulationRequestsRetriever.getNumberOfRequestsByItemId(piece.getItemId(), requestContext)
      .compose(totalRequests -> {
        if (totalRequests != null && totalRequests > 0) {
          logger.error("isDeletePieceRequestValid:: {} Request(s) were found for the given item {} when deleting piece {}",
            totalRequests, piece.getItemId(), piece.getId());
          throw new HttpException(RestConstants.VALIDATION_ERROR, ErrorCodes.REQUEST_FOUND.toError());
        }
        return Future.succeededFuture();
      })
      .mapEmpty();
  }

  protected Future<Void> updatePoLine(PieceDeletionHolder holder, RequestContext requestContext) {
    var comPOL = holder.getOriginPoLine();
    return Boolean.TRUE.equals(comPOL.getIsPackage()) || Boolean.TRUE.equals(comPOL.getCheckinItems())
      ? Future.succeededFuture()
      : pieceDeleteFlowPoLineService.updatePoLine(holder, requestContext);
  }

  private Future<JsonObject> getOnOrderItemForPiece(Piece piece, RequestContext requestContext) {
    if (StringUtils.isNotEmpty(piece.getItemId())) {
      return inventoryItemManager.getItemRecordById(piece.getItemId(), true, requestContext)
        .map(item -> {
          boolean isOnOrderItem = isItemWithStatus(item, ItemStatus.ON_ORDER.value());
          if (isOnOrderItem) {
            return item;
          }
          return null;
        });
    } else {
      return Future.succeededFuture();
    }
  }

  public Future<List<Void>> batchDeletePiece (PieceCollection entity, RequestContext requestContext) {
    List <String> ids = new ArrayList<>();
    entity.getPieces().stream().forEach(v -> ids.add(v.getId()));
    List<Future<PieceDeletionHolder>> deletionHolders = ids.stream()
      .map(pieceId -> {
        PieceDeletionHolder holder = new PieceDeletionHolder().withDeleteHolding(true);
        return pieceStorageService.getPieceById(pieceId, requestContext)
          .map(pieceToDelete -> {
            holder.withPieceToDelete(pieceToDelete);
            return holder;
          });
      })
      .toList();

    return collectResultsOnSuccess(deletionHolders)
      .compose(holders -> {
        List<Future<Void>> deleteFutures = holders.stream()
          .map(holder -> pieceStorageService.deletePiece(holder.getPieceToDelete().getId(), true, requestContext))
          .toList();

        return collectResultsOnSuccess(deleteFutures);
      });
  }
}
