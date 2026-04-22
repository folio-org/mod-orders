package org.folio.service.pieces.flows.create;

import java.util.List;
import java.util.Objects;
import java.util.Optional;

import io.vertx.core.Future;
import org.apache.commons.collections4.CollectionUtils;
import org.folio.models.pieces.PieceBatchCreationHolder;
import org.folio.models.pieces.PieceCreationHolder;
import org.folio.rest.core.models.RequestContext;
import org.folio.rest.jaxrs.model.PoLine;
import org.folio.rest.jaxrs.model.Cost;
import org.folio.rest.jaxrs.model.Piece;
import org.folio.rest.jaxrs.model.acq.Location;
import org.folio.service.finance.transaction.ReceivingEncumbranceStrategy;
import org.folio.service.orders.PurchaseOrderLineService;
import org.folio.service.orders.PurchaseOrderStorageService;
import org.folio.service.pieces.PieceUtil;
import org.folio.service.pieces.flows.BasePieceFlowUpdatePoLineService;

import static org.folio.orders.utils.HelperUtils.chainCall;

public class PieceCreateFlowPoLineService extends BasePieceFlowUpdatePoLineService<PieceCreationHolder> {

  public PieceCreateFlowPoLineService(PurchaseOrderStorageService purchaseOrderStorageService, PurchaseOrderLineService purchaseOrderLineService,
    ReceivingEncumbranceStrategy receivingEncumbranceStrategy) {
    super(purchaseOrderStorageService, purchaseOrderLineService, receivingEncumbranceStrategy);
  }

  @Override
  protected List<Location> getPieceLocations(PieceCreationHolder holder) {
    return PieceUtil.findOrderPieceLineLocation(holder.getPieceToCreate(), holder.getPoLineToSave());
  }

  @Override
  public boolean poLineUpdateCost(PieceCreationHolder holder) {
    PoLine lineToSave = holder.getPoLineToSave();
    Piece piece = holder.getPieceToCreate();
    Cost cost = lineToSave.getCost();
    if (piece.getFormat() == Piece.Format.ELECTRONIC) {
      Integer prevQty = Optional.ofNullable(cost.getQuantityElectronic()).orElse(0);
      cost.setQuantityElectronic(prevQty + 1);
    } else {
      Integer prevQty = Optional.ofNullable(cost.getQuantityPhysical()).orElse(0);
      cost.setQuantityPhysical(prevQty + 1);
    }
    return true;
  }

  @Override
  public boolean poLineUpdateLocations(PieceCreationHolder holder) {
    PoLine lineToSave = holder.getPoLineToSave();
    Piece piece = holder.getPieceToCreate();
    final int qty = 1;
    List<Location> locationsToUpdate = PieceUtil.findOrderPieceLineLocation(piece, lineToSave);
    if (CollectionUtils.isNotEmpty(locationsToUpdate)) {
      Location loc = locationsToUpdate.get(0);
      if (Objects.nonNull(piece.getReceivingTenantId())) {
        loc.setTenantId(piece.getReceivingTenantId());
      }
      if (piece.getFormat() == Piece.Format.ELECTRONIC) {
        Integer prevLocQty = Optional.ofNullable(loc.getQuantityElectronic()).orElse(0);
        loc.setQuantityElectronic(prevLocQty + qty);
        loc.setQuantity(loc.getQuantity() + qty);
      } else {
        Integer prevLocQty = Optional.ofNullable(loc.getQuantityPhysical()).orElse(0);
        loc.setQuantityPhysical(prevLocQty + qty);
        loc.setQuantity(loc.getQuantity() + qty);
      }
    } else if (isLocationUpdateRequired(piece, lineToSave)) {
      Location locationToAdd = new Location().withQuantity(qty);
      if (piece.getHoldingId() != null) {
        locationToAdd = locationToAdd.withHoldingId(piece.getHoldingId());
      } else {
        locationToAdd = locationToAdd.withLocationId(piece.getLocationId());
      }
      if (Objects.nonNull(piece.getReceivingTenantId())) {
        locationToAdd.setTenantId(piece.getReceivingTenantId());
      }
      if (piece.getFormat() == Piece.Format.ELECTRONIC) {
        locationToAdd.withQuantityElectronic(qty);
      } else {
        locationToAdd.withQuantityPhysical(qty);
      }
      List<Location> locations = lineToSave.getLocations();
      locations.add(locationToAdd);
    }
    return true;
  }

  public Future<Void> updatePoLineBatch(PieceBatchCreationHolder holder, RequestContext requestContext) {
    return chainCall(holder.getPiecesToCreate(), piece ->
      updatePoLineCostAndProcessEncumbrances(createPieceCreationHolder(piece, holder), requestContext));
  }

  public Future<Void> updateLocationsAndSavePoLineBatch(PieceBatchCreationHolder holder, RequestContext requestContext) {
    var pieceCreationHolderList = createPieceCreationHolderList(holder);
    for (var pieceCreationHolder : pieceCreationHolderList) {
      poLineUpdateLocations(pieceCreationHolder);
    }
    return purchaseOrderLineService.saveOrderLine(holder.getPoLineToSave(), getPieceLocations(holder), requestContext);
  }

  private PieceCreationHolder createPieceCreationHolder(Piece piece, PieceBatchCreationHolder holder) {
    var pieceCreationHolder = new PieceCreationHolder()
      .withPieceToCreate(piece)
      .withCreateItem(holder.isCreateItem());
    pieceCreationHolder
      .withTitleInformation(holder.getTitle())
      .withOrderInformation(holder.getOriginPurchaseOrder(), holder.getPurchaseOrderToSave());
    return pieceCreationHolder;
  }

  private List<PieceCreationHolder> createPieceCreationHolderList(PieceBatchCreationHolder holder) {
    return holder.getPiecesToCreate().stream()
      .map(piece -> createPieceCreationHolder(piece, holder))
      .toList();
  }

  private List<Location> getPieceLocations(PieceBatchCreationHolder holder) {
    return PieceUtil.findLocationsUsingPieceList(holder.getPiecesToCreate(), holder.getPoLineToSave());
  }

}
