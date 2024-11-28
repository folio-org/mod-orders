package org.folio.service.pieces.flows.update;

import static org.folio.orders.utils.ProtectedOperationType.UPDATE;
import static org.folio.service.orders.utils.StatusUtils.calculatePoLineReceiptStatus;
import static org.folio.service.pieces.PieceUtil.updatePieceStatus;

import java.util.List;
import java.util.stream.Collectors;

import lombok.extern.log4j.Log4j2;
import org.folio.models.pieces.BasePieceFlowHolder;
import org.folio.models.pieces.PieceBatchStatusUpdateHolder;
import org.folio.models.pieces.PieceUpdateHolder;
import org.folio.okapi.common.GenericCompositeFuture;
import org.folio.orders.utils.HelperUtils;
import org.folio.orders.utils.PoLineCommonUtil;
import org.folio.orders.utils.ProtectedOperationType;
import org.folio.rest.core.models.RequestContext;
import org.folio.rest.jaxrs.model.CompositePoLine;
import org.folio.rest.jaxrs.model.CompositePurchaseOrder.OrderType;
import org.folio.rest.jaxrs.model.CompositePurchaseOrder.WorkflowStatus;
import org.folio.rest.jaxrs.model.Location;
import org.folio.rest.jaxrs.model.Piece;
import org.folio.rest.jaxrs.model.PieceBatchStatusCollection;
import org.folio.service.ProtectionService;
import org.folio.service.orders.PurchaseOrderLineService;
import org.folio.service.pieces.PieceService;
import org.folio.service.pieces.PieceStorageService;
import org.folio.service.pieces.PieceUtil;
import org.folio.service.pieces.flows.BasePieceFlowHolderBuilder;
import org.folio.service.pieces.flows.DefaultPieceFlowsValidator;

import io.vertx.core.Future;
import org.folio.service.titles.TitlesService;

@Log4j2
public class PieceUpdateFlowManager {

  private final PieceStorageService pieceStorageService;
  private final PieceService pieceService;
  private final TitlesService titlesService;
  private final ProtectionService protectionService;
  private final PieceUpdateFlowPoLineService updatePoLineService;
  private final PieceUpdateFlowInventoryManager pieceUpdateFlowInventoryManager;
  private final BasePieceFlowHolderBuilder basePieceFlowHolderBuilder;
  private final DefaultPieceFlowsValidator defaultPieceFlowsValidator;
  private final PurchaseOrderLineService purchaseOrderLineService;

  public PieceUpdateFlowManager(PieceStorageService pieceStorageService, PieceService pieceService, TitlesService titlesService, ProtectionService protectionService,
                                PieceUpdateFlowPoLineService updatePoLineService, PieceUpdateFlowInventoryManager pieceUpdateFlowInventoryManager,
                                BasePieceFlowHolderBuilder basePieceFlowHolderBuilder, DefaultPieceFlowsValidator defaultPieceFlowsValidator,
                                PurchaseOrderLineService purchaseOrderLineService) {
    this.pieceStorageService = pieceStorageService;
    this.pieceService = pieceService;
    this.titlesService = titlesService;
    this.protectionService = protectionService;
    this.updatePoLineService = updatePoLineService;
    this.pieceUpdateFlowInventoryManager = pieceUpdateFlowInventoryManager;
    this.basePieceFlowHolderBuilder = basePieceFlowHolderBuilder;
    this.defaultPieceFlowsValidator = defaultPieceFlowsValidator;
    this.purchaseOrderLineService = purchaseOrderLineService;
  }

  // Flow to update piece
  // 1. Before update, get piece by id from storage and store receiving status
  // 2. Update piece with new content and complete future
  // 3. Create a message and check if receivingStatus is not consistent with storage; if yes - send a message to event bus
  public Future<Void> updatePiece(Piece pieceToUpdate, boolean createItem, boolean deleteHolding, RequestContext requestContext) {
    PieceUpdateHolder holder = new PieceUpdateHolder()
      .withPieceToUpdate(pieceToUpdate)
      .withCreateItem(createItem)
      .withDeleteHolding(deleteHolding);

    return pieceStorageService.getPieceById(pieceToUpdate.getId(), requestContext)
      .map(holder::withPieceFromStorage)
      .compose(aHolder -> basePieceFlowHolderBuilder.updateHolderWithOrderInformation(holder, requestContext))
      .compose(aHolder -> basePieceFlowHolderBuilder.updateHolderWithTitleInformation(holder, requestContext))
      .map(v -> {
        defaultPieceFlowsValidator.isPieceRequestValid(pieceToUpdate, holder.getOriginPoLine(), createItem);
        return null;
      })
      .compose(title -> protectionService.isOperationRestricted(holder.getTitle().getAcqUnitIds(), UPDATE, requestContext))
      .compose(v -> pieceUpdateFlowInventoryManager.processInventory(holder, requestContext))
      .compose(v -> updatePoLine(holder, requestContext))
      .map(v -> updatePieceStatus(holder.getPieceToUpdate(), holder.getPieceFromStorage().getReceivingStatus(), holder.getPieceToUpdate().getReceivingStatus()))
      .compose(verifyReceiptStatus -> pieceStorageService.updatePiece(holder.getPieceToUpdate(), requestContext).map(verifyReceiptStatus))
      .compose(verifyReceiptStatus -> {
        if (Boolean.TRUE.equals(verifyReceiptStatus)) {
          pieceService.receiptConsistencyPiecePoLine(holder.getPieceToUpdate().getPoLineId(), requestContext);
        }
        return Future.succeededFuture();
      })
      .onFailure(t -> log.error("User to update piece with id={}", holder.getPieceToUpdate().getId(), t))
      .mapEmpty();
  }

  public Future<Void> updatePiecesStatuses(List<String> pieceIds, PieceBatchStatusCollection.ReceivingStatus receivingStatus, RequestContext requestContext) {
    var newStatus = Piece.ReceivingStatus.fromValue(receivingStatus.value());
    return pieceStorageService.getPiecesByIds(pieceIds, requestContext)
      .compose(pieces -> isOperationRestricted(pieces, requestContext))
      .map(pieces -> pieces.stream().collect(Collectors.groupingBy(Piece::getPoLineId)))
      .map(piecesByPoLineId -> piecesByPoLineId.entrySet().stream()
        .map(entry -> new PieceBatchStatusUpdateHolder(newStatus, entry.getValue(), entry.getKey()))
        .map(holder -> basePieceFlowHolderBuilder.updateHolderWithOrderInformation(holder, requestContext)
          .compose(v -> updatePoLine(holder, requestContext))
          .compose(v -> updatePiecesStatusesByPoLine(holder, requestContext)))
        .toList())
      .compose(HelperUtils::collectResultsOnSuccess)
      .onSuccess(v -> log.info("Pieces statuses are updated for pieceIds: {} to status: {}", pieceIds, receivingStatus))
      .onFailure(t -> log.error("Failed to update pieces statuses for pieceIds: {} to status: {}", pieceIds, receivingStatus, t))
      .mapEmpty();
  }

  protected Future<Void> updatePoLine(PieceUpdateHolder holder, RequestContext requestContext) {
    return updatePoLine(holder, List.of(holder.getPieceToUpdate()), requestContext)
      .compose(v -> !Boolean.TRUE.equals(holder.getOriginPoLine().getIsPackage()) && !Boolean.TRUE.equals(holder.getOriginPoLine().getCheckinItems())
        ? updatePoLineService.updatePoLine(holder, requestContext)
        : Future.succeededFuture());
  }

  protected Future<Void> updatePoLine(PieceBatchStatusUpdateHolder holder, RequestContext requestContext) {
    return updatePoLine(holder, holder.getPieces(), requestContext);
  }

  private <T extends BasePieceFlowHolder> Future<Void> updatePoLine(T holder, List<Piece> piecesToUpdate, RequestContext requestContext) {
    var originPurchaseOrder = holder.getOriginPurchaseOrder();
    if (originPurchaseOrder.getOrderType() != OrderType.ONE_TIME || originPurchaseOrder.getWorkflowStatus() != WorkflowStatus.OPEN) {
      return Future.succeededFuture();
    }

    var originPoLine = holder.getOriginPoLine();
    var poLineToSave = holder.getPoLineToSave();
    var pieceIds = piecesToUpdate.stream().map(Piece::getId).toList();
    return pieceStorageService.getPiecesByLineId(originPoLine.getId(), requestContext)
      .compose(pieces -> {
        if (PoLineCommonUtil.isCancelledOrOngoingStatus(PoLineCommonUtil.convertToPoLine(poLineToSave))) {
          log.info("updatePoLine:: Skip updating PoLine: '{}' with status: '{}'", poLineToSave.getId(), poLineToSave.getReceiptStatus());
        } else {
          var newStatus = calculatePoLineReceiptStatus(poLineToSave.getId(), pieces, piecesToUpdate);
          poLineToSave.setReceiptStatus(CompositePoLine.ReceiptStatus.fromValue(newStatus.value()));
        }
        var locations = getPieceLocations(piecesToUpdate, poLineToSave);
        return purchaseOrderLineService.saveOrderLine(poLineToSave, locations, requestContext);
      })
      .onSuccess(v -> log.info("updatePoLine:: PoLine with id: '{}' is updated for pieceIds: {}", originPoLine.getId(), pieceIds))
      .onFailure(t -> log.error("Failed to update PO line with id: '{}' for pieceIds: {}", originPoLine.getId(), pieceIds, t));
  }

  private Future<Boolean> updatePiecesStatusesByPoLine(PieceBatchStatusUpdateHolder holder, RequestContext requestContext) {
    var isAnyPiecesUpdated = holder.getPieces().stream().anyMatch(piece -> updatePieceStatus(piece, piece.getReceivingStatus(), holder.getReceivingStatus()));
    var updates = holder.getPieces().stream().map(piece -> pieceStorageService.updatePiece(piece, requestContext)).toList();
    return HelperUtils.collectResultsOnSuccess(updates)
      .map(v -> {
        if (isAnyPiecesUpdated) {
          pieceService.receiptConsistencyPiecePoLine(holder.getPoLineId(), requestContext);
        }
        return isAnyPiecesUpdated;
      });
  }

  private List<Location> getPieceLocations(List<Piece> pieces, CompositePoLine poLine) {
    return pieces.stream()
      .flatMap(pieceToUpdate -> PieceUtil.findOrderPieceLineLocation(pieceToUpdate, poLine).stream())
      .toList();
  }

  protected Future<List<Piece>> isOperationRestricted(List<Piece> pieces, RequestContext requestContext) {
    var pieceIds = pieces.stream().map(Piece::getId).toList();
    return titlesService.getTitlesByPieceIds(pieceIds, requestContext)
      .map(titles -> titles.stream()
        .map(title -> protectionService.isOperationRestricted(title.getAcqUnitIds(), ProtectedOperationType.UPDATE, requestContext))
        .toList())
      .map(GenericCompositeFuture::all)
      .map(pieces);
  }

}
