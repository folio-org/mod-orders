package org.folio.service.pieces.flows.delete;

import static org.folio.orders.utils.PoLineCommonUtil.getOverallCostQuantity;
import static org.folio.orders.utils.ProtectedOperationType.DELETE;
import static org.folio.orders.utils.RequestContextUtil.createContextWithNewTenantId;

import java.util.List;

import org.apache.logging.log4j.LogManager;
import org.apache.logging.log4j.Logger;
import org.folio.models.pieces.PieceDeletionHolder;
import org.folio.rest.RestConstants;
import org.folio.rest.core.exceptions.ErrorCodes;
import org.folio.rest.core.exceptions.HttpException;
import org.folio.rest.core.models.RequestContext;
import org.folio.rest.jaxrs.model.CompositePurchaseOrder;
import org.folio.rest.jaxrs.model.Parameter;
import org.folio.service.CirculationRequestsRetriever;
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
      .compose(aVoid -> isAllowedToDeletePiece(holder, requestContext))
      .compose(aVoid -> isDeletePieceRequestValid(holder, requestContext))
      .compose(aVoid -> pieceDeleteFlowInventoryManager.processInventory(holder, requestContext))
      .compose(pair -> updatePoLine(holder, requestContext))
      .compose(aVoid -> pieceStorageService.deletePiece(holder.getPieceToDelete().getId(), true, requestContext));
  }

  private Future<Void> isAllowedToDeletePiece(PieceDeletionHolder holder, RequestContext requestContext) {
    var poLineCheckItems = holder.getOriginPoLine().getCheckinItems();
    var orderStatus = holder.getOriginPurchaseOrder().getWorkflowStatus();
    var overallCostQuantity = getOverallCostQuantity(holder.getOriginPoLine());
    if (!poLineCheckItems && CompositePurchaseOrder.WorkflowStatus.CLOSED.equals(orderStatus) && overallCostQuantity == 1) {
      var params = List.of(new Parameter().withKey("WorkFlowStatus").withValue(orderStatus.value()),
        new Parameter().withKey("OverallCostQuantity").withValue(String.valueOf(overallCostQuantity)));
      var error = ErrorCodes.LAST_PIECE.toError().withParameters(params);
      logger.error("isAllowedToDeletePiece:: {}", error.getMessage());
      throw new HttpException(RestConstants.VALIDATION_ERROR, error);
    }
    return Future.succeededFuture();
  }

  private Future<Void> isDeletePieceRequestValid(PieceDeletionHolder holder, RequestContext requestContext) {
    var piece = holder.getPieceToDelete();
    if (piece.getItemId() == null) {
      return Future.succeededFuture();
    }

    var locationContext = createContextWithNewTenantId(requestContext, holder.getPieceToDelete().getReceivingTenantId());
    return circulationRequestsRetriever.getNumberOfRequestsByItemId(piece.getItemId(), locationContext)
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

}
