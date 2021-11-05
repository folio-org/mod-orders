package org.folio.service.pieces.flows.update;

import java.util.List;
import java.util.concurrent.CompletableFuture;

import org.apache.commons.collections4.CollectionUtils;
import org.folio.completablefuture.FolioVertxCompletableFuture;
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
  public CompletableFuture<Void> updatePoLine(PieceUpdateHolder holder, RequestContext requestContext) {
    FolioVertxCompletableFuture<Void> future = new FolioVertxCompletableFuture<>(requestContext.getContext());
    try {
      Boolean isLineUpdated = poLineUpdateQuantity(holder);
      if (Boolean.TRUE.equals(isLineUpdated)) {
        purchaseOrderLineService.saveOrderLine(holder.getPoLineToSave(), requestContext)
                                .thenAccept(aVoid -> future.complete(null))
                                .exceptionally(ex -> {
                                  future.completeExceptionally(ex);
                                  return null;
                                });
      }
      return CompletableFuture.completedFuture(null);
    } catch (Exception e) {
      future.completeExceptionally(e);
    }
    return future;
  }

  @Override
  public Boolean poLineUpdateQuantity(PieceUpdateHolder pieceUpdateHolder) {
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
        return Boolean.TRUE;
      }
    }
    return Boolean.FALSE;
  }
}
