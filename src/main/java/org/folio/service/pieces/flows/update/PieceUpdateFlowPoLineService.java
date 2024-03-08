package org.folio.service.pieces.flows.update;

import java.util.List;

import org.apache.commons.collections4.CollectionUtils;
import org.folio.models.pieces.PieceCreationHolder;
import org.folio.models.pieces.PieceDeletionHolder;
import org.folio.models.pieces.PieceUpdateHolder;
import org.folio.rest.core.models.RequestContext;
import org.folio.rest.jaxrs.model.CompositePoLine;
import org.folio.rest.jaxrs.model.Location;
import org.folio.rest.jaxrs.model.Piece;
import org.folio.service.finance.transaction.ReceivingEncumbranceStrategy;
import org.folio.service.orders.PurchaseOrderLineService;
import org.folio.service.orders.PurchaseOrderStorageService;
import org.folio.service.pieces.PieceUtil;
import org.folio.service.pieces.flows.BasePieceFlowUpdatePoLineService;
import org.folio.service.pieces.flows.create.PieceCreateFlowPoLineService;
import org.folio.service.pieces.flows.delete.PieceDeleteFlowPoLineService;

import io.vertx.core.Future;

public class PieceUpdateFlowPoLineService extends BasePieceFlowUpdatePoLineService<PieceUpdateHolder> {
  private PieceCreateFlowPoLineService pieceCreateFlowPoLineService;
  private PieceDeleteFlowPoLineService pieceDeleteFlowPoLineService;

  public PieceUpdateFlowPoLineService(PurchaseOrderStorageService purchaseOrderStorageService, PurchaseOrderLineService purchaseOrderLineService,
              ReceivingEncumbranceStrategy receivingEncumbranceStrategy, PieceCreateFlowPoLineService pieceCreateFlowPoLineService,
              PieceDeleteFlowPoLineService pieceDeleteFlowPoLineService) {
    super(purchaseOrderStorageService, purchaseOrderLineService, receivingEncumbranceStrategy);
    this.pieceCreateFlowPoLineService = pieceCreateFlowPoLineService;
    this.pieceDeleteFlowPoLineService = pieceDeleteFlowPoLineService;
  }

  @Override
  public Future<Void> updatePoLine(PieceUpdateHolder holder, RequestContext requestContext) {
    boolean isLineUpdated = poLineUpdateQuantity(holder);
    if (isLineUpdated) {
      updateEstimatedPrice(holder.getPoLineToSave());
      return purchaseOrderLineService.saveOrderLine(holder.getPoLineToSave(), requestContext);
    } else {
      return Future.succeededFuture();
    }
  }

  @Override
  public boolean poLineUpdateQuantity(PieceUpdateHolder pieceUpdateHolder) {
    CompositePoLine lineToSave = pieceUpdateHolder.getPoLineToSave();
    Piece pieceToUpdate = pieceUpdateHolder.getPieceToUpdate();
    Piece pieceFromStorage = pieceUpdateHolder.getPieceFromStorage();
    List<Location> locationsToUpdate = PieceUtil.findOrderPieceLineLocation(pieceFromStorage, lineToSave);
    if (CollectionUtils.isNotEmpty(locationsToUpdate)) {
      PieceDeletionHolder pieceDeletionHolder = new PieceDeletionHolder().withPieceToDelete(pieceFromStorage);
      pieceDeletionHolder.withOrderInformation(pieceUpdateHolder.getPurchaseOrderToSave());
      boolean isDecreased = pieceDeleteFlowPoLineService.poLineUpdateQuantity(pieceDeletionHolder);
      if (isDecreased) {
        PieceCreationHolder pieceCreationHolder = new PieceCreationHolder().withPieceToCreate(pieceToUpdate);
        pieceCreationHolder.withOrderInformation(pieceDeletionHolder.getPurchaseOrderToSave());
        pieceCreateFlowPoLineService.poLineUpdateQuantity(pieceCreationHolder);
        pieceUpdateHolder.withOrderInformation(pieceCreationHolder.getPurchaseOrderToSave());
        return true;
      }
    }
    return false;
  }
}
