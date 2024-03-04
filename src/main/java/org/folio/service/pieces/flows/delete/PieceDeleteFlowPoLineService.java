package org.folio.service.pieces.flows.delete;

import java.util.ArrayList;
import java.util.List;

import org.apache.commons.collections4.CollectionUtils;
import org.folio.models.pieces.PieceDeletionHolder;
import org.folio.rest.jaxrs.model.CompositePoLine;
import org.folio.rest.jaxrs.model.Cost;
import org.folio.rest.jaxrs.model.Location;
import org.folio.rest.jaxrs.model.Piece;
import org.folio.service.finance.transaction.ReceivingEncumbranceStrategy;
import org.folio.service.orders.PurchaseOrderLineService;
import org.folio.service.orders.PurchaseOrderStorageService;
import org.folio.service.pieces.PieceUtil;
import org.folio.service.pieces.flows.BasePieceFlowUpdatePoLineService;

public class PieceDeleteFlowPoLineService extends BasePieceFlowUpdatePoLineService<PieceDeletionHolder> {

  public PieceDeleteFlowPoLineService(PurchaseOrderStorageService purchaseOrderStorageService, PurchaseOrderLineService purchaseOrderLineService,
                                ReceivingEncumbranceStrategy receivingEncumbranceStrategy) {
    super(purchaseOrderStorageService, purchaseOrderLineService, receivingEncumbranceStrategy);
  }

  @Override
  public boolean poLineUpdateQuantity(PieceDeletionHolder holder) {
    CompositePoLine lineToSave = holder.getPoLineToSave();
    Piece piece = holder.getPieceToDelete();
    final int qty = 1;
    List<Location> locationsToUpdate = PieceUtil.findOrderPieceLineLocation(piece, lineToSave);
    List<Location> locationToDelete = new ArrayList<>();
    if (CollectionUtils.isNotEmpty(locationsToUpdate)) {
      Location loc = locationsToUpdate.get(0);
      Cost cost = lineToSave.getCost();
      if (piece.getFormat() == Piece.Format.ELECTRONIC) {
        loc.setQuantityElectronic(loc.getQuantityElectronic() - qty);
        loc.setQuantity(loc.getQuantity() - qty);
        cost.setQuantityElectronic(cost.getQuantityElectronic() - qty);
        if (loc.getQuantityElectronic() == 0) {
          loc.setQuantityElectronic(null);
        }
      } else {
        loc.setQuantityPhysical(loc.getQuantityPhysical() - qty);
        loc.setQuantity(loc.getQuantity() - qty);
        cost.setQuantityPhysical(cost.getQuantityPhysical() - qty);
        if (loc.getQuantityPhysical() == 0) {
          loc.setQuantityPhysical(null);
        }
      }
      if (loc.getQuantity() != null && loc.getQuantity() == 0) {
        locationToDelete.add(loc);
      }
      lineToSave.getLocations().removeAll(locationToDelete);
      return true;
    } else if (!isLocationUpdateRequired(piece, lineToSave)) {
      Cost cost = lineToSave.getCost();
      if (piece.getFormat() == Piece.Format.ELECTRONIC) {
        if (cost.getQuantityElectronic() != null) {
          int prevQty = cost.getQuantityElectronic();
          if (prevQty > 0) {
            cost.setQuantityElectronic(prevQty - qty);
            return true;
          }
        }
      } else {
        if (cost.getQuantityPhysical() != null) {
          int prevQty = cost.getQuantityPhysical();
          if (prevQty > 0) {
            cost.setQuantityPhysical(prevQty - qty);
            return true;
          }
        }
      }
    }
    return false;
  }
}
