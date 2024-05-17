package org.folio.service.pieces.flows.create;

import java.util.Optional;

import org.apache.commons.lang3.BooleanUtils;
import org.folio.orders.utils.PoLineCommonUtil;
import org.folio.rest.core.models.RequestContext;
import org.folio.rest.jaxrs.model.CompositePoLine;
import org.folio.rest.jaxrs.model.Location;
import org.folio.rest.jaxrs.model.Piece;
import org.folio.service.inventory.InventoryHoldingManager;
import org.folio.service.pieces.PieceUpdateInventoryService;
import org.folio.service.pieces.flows.DefaultPieceFlowsValidator;
import org.folio.service.titles.TitlesService;

import io.vertx.core.Future;

public class PieceCreateFlowInventoryManager {

  private final TitlesService titlesService;
  private final PieceUpdateInventoryService pieceUpdateInventoryService;
  private final InventoryHoldingManager inventoryHoldingManager;

  public PieceCreateFlowInventoryManager(TitlesService titlesService,
                                         PieceUpdateInventoryService pieceUpdateInventoryService,
                                         InventoryHoldingManager inventoryHoldingManager) {
    this.titlesService = titlesService;
    this.pieceUpdateInventoryService = pieceUpdateInventoryService;
    this.inventoryHoldingManager = inventoryHoldingManager;
  }

  public Future<Void> processInventory(CompositePoLine compPOL, Piece piece, boolean createItem, RequestContext requestContext) {
    return updateInventoryForPoLine(compPOL, piece, requestContext)
      .compose(instanceId -> handleHolding(compPOL, piece, instanceId, requestContext))
      .compose(holdingId -> handleItem(compPOL, createItem, piece, requestContext))
      .map(itemId -> Optional.ofNullable(itemId).map(piece::withItemId))
      .mapEmpty();
  }

  private Future<String> updateInventoryForPoLine(CompositePoLine compPOL, Piece piece, RequestContext requestContext) {
    if (BooleanUtils.isNotTrue(compPOL.getIsPackage())) {
      return Optional.ofNullable(getPoLineInstanceId(compPOL))
        .orElse(titlesService.updateTitleWithInstance(piece.getTitleId(), requestContext))
        .map(instanceId -> compPOL.withInstanceId(instanceId).getInstanceId());
    }
    return titlesService.updateTitleWithInstance(piece.getTitleId(), requestContext);
  }

  private Future<String> getPoLineInstanceId(CompositePoLine compPOL) {
    return compPOL.getInstanceId() != null || PoLineCommonUtil.isInventoryUpdateNotRequired(compPOL)
      ? Future.succeededFuture(compPOL.getInstanceId())
      : null;
  }

  private Future<Location> handleHolding(CompositePoLine compPOL, Piece piece, String instanceId, RequestContext requestContext) {
    if (piece.getHoldingId() != null) {
      return Future.succeededFuture(new Location().withHoldingId(piece.getHoldingId()));
    }

    var location = new Location().withLocationId(piece.getLocationId());
    if (instanceId == null || !DefaultPieceFlowsValidator.isCreateHoldingForPiecePossible(piece, compPOL)) {
      return Future.succeededFuture(location);
    }
    return inventoryHoldingManager.createHoldingAndReturnId(instanceId, piece.getLocationId(), requestContext)
      .map(holdingId -> {
        if (holdingId != null) {
          piece.withLocationId(null).setHoldingId(holdingId);
          location.withLocationId(null).setHoldingId(holdingId);
        }
        return location;
      });
  }

  private Future<String> handleItem(CompositePoLine compPOL, boolean createItem, Piece piece, RequestContext requestContext) {
    return piece.getItemId() != null || !createItem || piece.getHoldingId() == null
      ? Future.succeededFuture(piece.getItemId())
      : pieceUpdateInventoryService.manualPieceFlowCreateItemRecord(piece, compPOL, requestContext);
  }

}
