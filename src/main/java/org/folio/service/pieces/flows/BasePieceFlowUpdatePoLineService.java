package org.folio.service.pieces.flows;

import static org.folio.orders.utils.HelperUtils.calculateEstimatedPrice;

import java.util.concurrent.CompletableFuture;

import org.folio.models.pieces.BasePieceFlowHolder;
import org.folio.rest.core.models.RequestContext;
import org.folio.rest.jaxrs.model.CompositePoLine;
import org.folio.rest.jaxrs.model.Cost;
import org.folio.rest.jaxrs.model.Piece;
import org.folio.service.finance.transaction.ReceivingEncumbranceStrategy;
import org.folio.service.orders.PurchaseOrderLineService;
import org.folio.service.orders.PurchaseOrderStorageService;
import org.folio.service.pieces.validators.PieceValidatorUtil;

public abstract class BasePieceFlowUpdatePoLineService<T extends BasePieceFlowHolder> implements PoLineUpdateQuantityService<T> {
  protected final PurchaseOrderStorageService purchaseOrderStorageService;
  protected final PurchaseOrderLineService purchaseOrderLineService;
  protected final ReceivingEncumbranceStrategy receivingEncumbranceStrategy;

  public BasePieceFlowUpdatePoLineService(PurchaseOrderStorageService purchaseOrderStorageService, PurchaseOrderLineService purchaseOrderLineService,
        ReceivingEncumbranceStrategy receivingEncumbranceStrategy) {
    this.purchaseOrderStorageService = purchaseOrderStorageService;
    this.purchaseOrderLineService = purchaseOrderLineService;
    this.receivingEncumbranceStrategy = receivingEncumbranceStrategy;
  }

  public CompletableFuture<Void> updatePoLine(T holder, RequestContext requestContext) {
    Boolean isLineUpdated = poLineUpdateQuantity(holder);
    if (Boolean.TRUE.equals(isLineUpdated)) {
      return receivingEncumbranceStrategy.processEncumbrances(holder.getPurchaseOrderToSave(), holder.getPurchaseOrderToSave(), requestContext)
                                .thenAccept(aVoid -> updateEstimatedPrice(holder.getPoLineToSave()))
                                .thenAccept(v -> purchaseOrderLineService.saveOrderLine(holder.getPoLineToSave(), requestContext));
    }
    return CompletableFuture.completedFuture(null);
  }

  protected CompositePoLine updateEstimatedPrice(CompositePoLine compPoLine) {
    Cost cost = compPoLine.getCost();
    cost.setPoLineEstimatedPrice(calculateEstimatedPrice(cost).getNumber().doubleValue());
    return compPoLine;
  }

  protected boolean isLocationUpdateRequired(Piece piece, CompositePoLine lineToSave) {
    return (piece.getHoldingId() != null || piece.getLocationId() != null) ||
                      PieceValidatorUtil.isLocationRequired(piece.getFormat(), lineToSave);
  }
}
