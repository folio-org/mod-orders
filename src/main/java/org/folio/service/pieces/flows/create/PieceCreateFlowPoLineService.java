package org.folio.service.pieces.flows.create;

import java.util.List;
import java.util.Objects;
import java.util.Optional;

import org.apache.commons.collections4.CollectionUtils;
import org.folio.models.pieces.PieceCreationHolder;
import org.folio.rest.jaxrs.model.CompositePoLine;
import org.folio.rest.jaxrs.model.Cost;
import org.folio.rest.jaxrs.model.Location;
import org.folio.rest.jaxrs.model.Piece;
import org.folio.service.finance.transaction.ReceivingEncumbranceStrategy;
import org.folio.service.orders.PurchaseOrderLineService;
import org.folio.service.orders.PurchaseOrderStorageService;
import org.folio.service.pieces.PieceUtil;
import org.folio.service.pieces.flows.BasePieceFlowUpdatePoLineService;

public class PieceCreateFlowPoLineService extends BasePieceFlowUpdatePoLineService<PieceCreationHolder> {

  public PieceCreateFlowPoLineService(PurchaseOrderStorageService purchaseOrderStorageService, PurchaseOrderLineService purchaseOrderLineService,
    ReceivingEncumbranceStrategy receivingEncumbranceStrategy) {
    super(purchaseOrderStorageService, purchaseOrderLineService, receivingEncumbranceStrategy);
  }

  @Override
  public boolean poLineUpdateQuantity(PieceCreationHolder holder) {
    CompositePoLine lineToSave = holder.getPoLineToSave();
    Piece piece = holder.getPieceToCreate();
    final int qty = 1;
    List<Location> locationsToUpdate = PieceUtil.findOrderPieceLineLocation(piece, lineToSave);
    if (CollectionUtils.isNotEmpty(locationsToUpdate)) {
      Location loc = locationsToUpdate.get(0);
      Cost cost = lineToSave.getCost();
      if (Objects.nonNull(piece.getReceivingStatus())) {
        loc.setTenantId(piece.getReceivingTenantId());
      }
      if (piece.getFormat() == Piece.Format.ELECTRONIC) {
        Integer prevLocQty = Optional.ofNullable(loc.getQuantityElectronic()).orElse(0);
        loc.setQuantityElectronic(prevLocQty + qty);
        loc.setQuantity(loc.getQuantity() + qty);
        Integer prevCostQty = Optional.ofNullable(cost.getQuantityElectronic()).orElse(0);
        cost.setQuantityElectronic(prevCostQty + qty);
      } else {
        Integer prevLocQty = Optional.ofNullable(loc.getQuantityPhysical()).orElse(0);
        loc.setQuantityPhysical(prevLocQty + qty);
        loc.setQuantity(loc.getQuantity() + qty);
        Integer prevCostQty = Optional.ofNullable(cost.getQuantityPhysical()).orElse(0);
        cost.setQuantityPhysical(prevCostQty + qty);
      }
    } else if (isLocationUpdateRequired(piece, lineToSave)) {
      Location locationToAdd = new Location().withLocationId(piece.getLocationId()).withHoldingId(piece.getHoldingId())
        .withQuantity(qty);
      Cost cost = lineToSave.getCost();
      if (Objects.nonNull(piece.getReceivingStatus())) {
        locationToAdd.setTenantId(piece.getReceivingTenantId());
      }
      if (piece.getFormat() == Piece.Format.ELECTRONIC) {
        locationToAdd.withQuantityElectronic(qty);
        Integer prevQty = Optional.ofNullable(cost.getQuantityElectronic()).orElse(0);
        cost.setQuantityElectronic(prevQty + qty);
      } else {
        locationToAdd.withQuantityPhysical(qty);
        Integer prevQty = Optional.ofNullable(cost.getQuantityPhysical()).orElse(0);
        cost.setQuantityPhysical(prevQty + qty);
      }
      List<Location> locations = lineToSave.getLocations();
      locations.add(locationToAdd);
    } else {
      Cost cost = lineToSave.getCost();
      if (piece.getFormat() == Piece.Format.ELECTRONIC) {
        Integer prevQty = Optional.ofNullable(cost.getQuantityElectronic()).orElse(0);
        cost.setQuantityElectronic(prevQty + qty);
      } else {
        Integer prevQty = Optional.ofNullable(cost.getQuantityPhysical()).orElse(0);
        cost.setQuantityPhysical(prevQty + qty);
      }
    }
    return true;
  }
}
