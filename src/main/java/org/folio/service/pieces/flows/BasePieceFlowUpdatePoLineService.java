package org.folio.service.pieces.flows;

import static org.folio.orders.utils.HelperUtils.calculateEstimatedPrice;

import org.folio.models.pieces.BasePieceFlowHolder;
import org.folio.rest.core.models.RequestContext;
import org.folio.rest.jaxrs.model.PoLine;
import org.folio.rest.jaxrs.model.Cost;
import org.folio.rest.jaxrs.model.Piece;
import org.folio.rest.jaxrs.model.acq.Location;
import org.folio.service.finance.transaction.ReceivingEncumbranceStrategy;
import org.folio.service.orders.PurchaseOrderLineService;
import org.folio.service.orders.PurchaseOrderStorageService;
import org.folio.service.pieces.validators.PieceValidatorUtil;

import io.vertx.core.Future;

import java.util.List;
import java.util.Objects;

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
    if (skipUpdatingPoLine(holder.getOriginPoLine())) {
      return Future.succeededFuture();
    }
    boolean isLineUpdated = poLineUpdateCost(holder);
    holder.setLineUpdated(isLineUpdated);
    if (isLineUpdated) {
      return receivingEncumbranceStrategy
        .processEncumbrances(holder.getPurchaseOrderToSave(), holder.getPurchaseOrderToSave(), requestContext)
        .map(aVoid -> {
          updateEstimatedPrice(holder.getPoLineToSave());
          return null;
        });
    }
    return Future.succeededFuture();
  }

  public Future<Void> updateLocationsAndSavePoLine(T holder, RequestContext requestContext) {
    if (skipUpdatingPoLine(holder.getOriginPoLine())) {
      return Future.succeededFuture();
    }
    boolean lineUpdated2 = poLineUpdateLocations(holder);
    holder.setLineUpdated(holder.getLineUpdated() || lineUpdated2 ||
      !Objects.equals(holder.getOriginPoLine().getInstanceId(), holder.getPoLineToSave().getInstanceId()));
    if (holder.getLineUpdated()) {
      return purchaseOrderLineService.saveOrderLine(holder.getPoLineToSave(), getPieceLocations(holder), requestContext);
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

  private boolean skipUpdatingPoLine(PoLine poLine) {
    return Boolean.TRUE.equals(poLine.getIsPackage()) || Boolean.TRUE.equals(poLine.getCheckinItems());
  }
}
