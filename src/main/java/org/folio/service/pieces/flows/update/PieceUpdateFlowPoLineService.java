package org.folio.service.pieces.flows.update;

import java.util.List;
import java.util.Optional;

import org.apache.commons.collections4.CollectionUtils;
import org.folio.models.pieces.PieceCreationHolder;
import org.folio.models.pieces.PieceDeletionHolder;
import org.folio.models.pieces.PieceUpdateHolder;
import org.folio.rest.jaxrs.model.Cost;
import org.folio.rest.jaxrs.model.PoLine;
import org.folio.rest.jaxrs.model.Piece;
import org.folio.rest.jaxrs.model.acq.Location;
import org.folio.service.finance.transaction.ReceivingEncumbranceStrategy;
import org.folio.service.orders.PurchaseOrderLineService;
import org.folio.service.orders.PurchaseOrderStorageService;
import org.folio.service.pieces.PieceUtil;
import org.folio.service.pieces.flows.BasePieceFlowUpdatePoLineService;
import org.folio.service.pieces.flows.create.PieceCreateFlowPoLineService;
import org.folio.service.pieces.flows.delete.PieceDeleteFlowPoLineService;

public class PieceUpdateFlowPoLineService extends BasePieceFlowUpdatePoLineService<PieceUpdateHolder> {
  private final PieceCreateFlowPoLineService pieceCreateFlowPoLineService;
  private final PieceDeleteFlowPoLineService pieceDeleteFlowPoLineService;

  public PieceUpdateFlowPoLineService(PurchaseOrderStorageService purchaseOrderStorageService, PurchaseOrderLineService purchaseOrderLineService,
              ReceivingEncumbranceStrategy receivingEncumbranceStrategy, PieceCreateFlowPoLineService pieceCreateFlowPoLineService,
              PieceDeleteFlowPoLineService pieceDeleteFlowPoLineService) {
    super(purchaseOrderStorageService, purchaseOrderLineService, receivingEncumbranceStrategy);
    this.pieceCreateFlowPoLineService = pieceCreateFlowPoLineService;
    this.pieceDeleteFlowPoLineService = pieceDeleteFlowPoLineService;
  }

  @Override
  protected List<Location> getPieceLocations(PieceUpdateHolder holder) {
    return PieceUtil.findOrderPieceLineLocation(holder.getPieceToUpdate(), holder.getPoLineToSave());
  }

  @Override
  public boolean poLineUpdateCost(PieceUpdateHolder holder) {
    // Handle a change of piece format, which could affect the cost
    Piece pieceFromStorage = holder.getPieceFromStorage();
    Piece pieceToUpdate = holder.getPieceToUpdate();
    if (pieceFromStorage.getFormat() == pieceToUpdate.getFormat()) {
      return false;
    }
    PoLine lineToSave = holder.getPoLineToSave();
    Cost cost = lineToSave.getCost();
    if (pieceFromStorage.getFormat() == Piece.Format.ELECTRONIC) {
      int prevQty = Optional.ofNullable(cost.getQuantityElectronic()).orElse(0);
      if (prevQty > 0) {
        cost.setQuantityElectronic(prevQty - 1);
      }
    } else {
      int prevQty = Optional.ofNullable(cost.getQuantityPhysical()).orElse(0);
      if (prevQty > 0) {
        cost.setQuantityPhysical(prevQty - 1);
      }
    }
    if (pieceToUpdate.getFormat() == Piece.Format.ELECTRONIC) {
      int prevQty = Optional.ofNullable(cost.getQuantityElectronic()).orElse(0);
      cost.setQuantityElectronic(prevQty + 1);
    } else {
      int prevQty = Optional.ofNullable(cost.getQuantityPhysical()).orElse(0);
      cost.setQuantityPhysical(prevQty + 1);
    }
    return true;
  }

  @Override
  public boolean poLineUpdateLocations(PieceUpdateHolder pieceUpdateHolder) {
    PoLine lineToSave = pieceUpdateHolder.getPoLineToSave();
    Piece pieceToUpdate = pieceUpdateHolder.getPieceToUpdate();
    Piece pieceFromStorage = pieceUpdateHolder.getPieceFromStorage();
    List<Location> locationsToUpdate = PieceUtil.findOrderPieceLineLocation(pieceFromStorage, lineToSave);
    if (CollectionUtils.isNotEmpty(locationsToUpdate)) {
      PieceDeletionHolder pieceDeletionHolder = new PieceDeletionHolder().withPieceToDelete(pieceFromStorage);
      if (pieceUpdateHolder.getPurchaseOrderToSave() != null) {
        pieceDeletionHolder.withOrderInformation(pieceUpdateHolder.getPurchaseOrderToSave());
      } else {
        pieceDeletionHolder.withPoLineOnly(lineToSave);
      }
      boolean isDecreased = pieceDeleteFlowPoLineService.poLineUpdateLocations(pieceDeletionHolder);
      if (isDecreased) {
        PieceCreationHolder pieceCreationHolder = new PieceCreationHolder().withPieceToCreate(pieceToUpdate);
        if (pieceDeletionHolder.getPurchaseOrderToSave() != null) {
          pieceCreationHolder.withOrderInformation(pieceDeletionHolder.getPurchaseOrderToSave());
        } else {
          pieceCreationHolder.withPoLineOnly(lineToSave);
        }
        pieceCreateFlowPoLineService.poLineUpdateLocations(pieceCreationHolder);
        if (pieceCreationHolder.getPurchaseOrderToSave() != null) {
          pieceUpdateHolder.withOrderInformation(pieceCreationHolder.getPurchaseOrderToSave());
        } else {
          pieceUpdateHolder.withPoLineOnly(lineToSave);
        }
        return true;
      }
    }
    return false;
  }

  public void updatePoLineWithoutSave(PieceUpdateHolder holder) {
    if (poLineUpdateCost(holder)) {
      updateEstimatedPrice(holder.getPoLineToSave());
    }
    poLineUpdateLocations(holder);
  }
}
