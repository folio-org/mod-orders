package org.folio.service.pieces.flows;

import static org.folio.orders.utils.HelperUtils.calculateEstimatedPrice;

import org.folio.models.pieces.BasePieceFlowHolder;
import org.folio.rest.core.models.RequestContext;
import org.folio.rest.jaxrs.model.PoLine;
import org.folio.rest.jaxrs.model.Cost;
import org.folio.rest.jaxrs.model.Location;
import org.folio.rest.jaxrs.model.Piece;
import org.folio.service.finance.transaction.ReceivingEncumbranceStrategy;
import org.folio.service.orders.PurchaseOrderLineService;
import org.folio.service.orders.PurchaseOrderStorageService;
import org.folio.service.pieces.validators.PieceValidatorUtil;

import io.vertx.core.Future;

import java.util.List;

public abstract class BasePieceFlowUpdatePoLineService<T extends BasePieceFlowHolder> implements PoLineUpdateQuantityService<T> {
  protected final PurchaseOrderStorageService purchaseOrderStorageService;
  protected final PurchaseOrderLineService purchaseOrderLineService;
  protected final ReceivingEncumbranceStrategy receivingEncumbranceStrategy;

  protected BasePieceFlowUpdatePoLineService(PurchaseOrderStorageService purchaseOrderStorageService, PurchaseOrderLineService purchaseOrderLineService,
        ReceivingEncumbranceStrategy receivingEncumbranceStrategy) {
    this.purchaseOrderStorageService = purchaseOrderStorageService;
    this.purchaseOrderLineService = purchaseOrderLineService;
    this.receivingEncumbranceStrategy = receivingEncumbranceStrategy;
  }

  public Future<Void> updatePoLine(T holder, RequestContext requestContext) {
    boolean isLineUpdated = poLineUpdateQuantity(holder);
    if (isLineUpdated) {
      return receivingEncumbranceStrategy
        .processEncumbrances(holder.getPurchaseOrderToSave(), holder.getPurchaseOrderToSave(), requestContext)
        .map(aVoid -> {
          updateEstimatedPrice(holder.getPoLineToSave());
          return null;
        })
        .compose(v -> purchaseOrderLineService.saveOrderLine(holder.getPoLineToSave(), getPieceLocations(holder), requestContext));
    }
    return Future.succeededFuture();
  }

  protected abstract List<Location> getPieceLocations(T holder);

  protected PoLine updateEstimatedPrice(PoLine poLine) {
    Cost cost = poLine.getCost();
    cost.setPoLineEstimatedPrice(calculateEstimatedPrice(cost).getNumber().doubleValue());
    return poLine;
  }

  protected boolean isLocationUpdateRequired(Piece piece, PoLine lineToSave) {
    return (piece.getHoldingId() != null || piece.getLocationId() != null) ||
                      PieceValidatorUtil.isLocationRequired(piece.getFormat(), lineToSave);
  }
}
