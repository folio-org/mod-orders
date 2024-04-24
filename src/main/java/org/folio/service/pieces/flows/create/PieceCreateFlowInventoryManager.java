package org.folio.service.pieces.flows.create;

import java.util.Optional;

import org.folio.orders.utils.PoLineCommonUtil;
import org.folio.rest.core.models.RequestContext;
import org.folio.rest.jaxrs.model.CompositePoLine;
import org.folio.rest.jaxrs.model.Location;
import org.folio.rest.jaxrs.model.Piece;
import org.folio.rest.jaxrs.model.Title;
import org.folio.service.inventory.InventoryHoldingManager;
import org.folio.service.pieces.PieceUpdateInventoryService;
import org.folio.service.pieces.flows.DefaultPieceFlowsValidator;
import org.folio.service.titles.TitleInstanceService;
import org.folio.service.titles.TitlesService;

import io.vertx.core.Future;

public class PieceCreateFlowInventoryManager {

  private final TitlesService titlesService;
  private final TitleInstanceService titleInstanceService;
  private final PieceUpdateInventoryService pieceUpdateInventoryService;
  private final InventoryHoldingManager inventoryHoldingManager;

  public PieceCreateFlowInventoryManager(TitlesService titlesService,
                                         TitleInstanceService titleInstanceService,
                                         PieceUpdateInventoryService pieceUpdateInventoryService,
                                         InventoryHoldingManager inventoryHoldingManager) {
    this.titlesService = titlesService;
    this.titleInstanceService = titleInstanceService;
    this.pieceUpdateInventoryService = pieceUpdateInventoryService;
    this.inventoryHoldingManager = inventoryHoldingManager;
  }

  public Future<Void> processInventory(CompositePoLine compPOL,  Piece piece,  boolean createItem, RequestContext requestContext) {
    if (Boolean.TRUE.equals(compPOL.getIsPackage())) {
      return packagePoLineUpdateInventory(compPOL, piece, createItem, requestContext);
    }
    else
    {
       return nonPackagePoLineUpdateInventory(compPOL, piece, createItem, requestContext);
    }
  }

  private Future<Void> nonPackagePoLineUpdateInventory(CompositePoLine compPOL, Piece piece, boolean createItem, RequestContext requestContext) {
    return nonPackageUpdateTitleWithInstance(compPOL, piece.getTitleId(), requestContext)
      .compose(instanceId -> handleHolding(compPOL, piece, instanceId, requestContext))
      .compose(holdingId -> handleItem(compPOL, createItem, piece, requestContext))
      .map(itemId -> {
        Optional.ofNullable(itemId).ifPresent(piece::withItemId);
        return null;
      })
      .mapEmpty();
  }

  private Future<Void> packagePoLineUpdateInventory(CompositePoLine compPOL, Piece piece, boolean createItem, RequestContext requestContext) {
    return titlesService.getTitleById(piece.getTitleId(), requestContext)
      .compose(title -> packageUpdateTitleWithInstance(title, requestContext))
      .compose(instanceId -> handleHolding(compPOL, piece, instanceId, requestContext))
      .compose(holdingId -> handleItem(compPOL, createItem, piece, requestContext))
      .map(itemId -> {
        Optional.ofNullable(itemId).ifPresent(piece::withItemId);
        return null;
      })
      .mapEmpty();
  }

  private Future<Location> handleHolding(CompositePoLine compPOL, Piece piece, String instanceId, RequestContext requestContext) {
    if (piece.getHoldingId() != null) {
      return Future.succeededFuture(new Location().withHoldingId(piece.getHoldingId()));
    }
    if (instanceId != null && DefaultPieceFlowsValidator.isCreateHoldingForPiecePossible(piece, compPOL)) {
      Location location = new Location().withLocationId(piece.getLocationId());
      return inventoryHoldingManager.getOrCreateHoldingsRecord(instanceId, location, requestContext)
        .map(holdingId -> {
          if(holdingId != null) {
            piece.setLocationId(null);
            piece.setHoldingId(holdingId);
            location.setLocationId(null);
            location.setHoldingId(holdingId);
          }
          return location;
        });
    }
    return Future.succeededFuture(new Location().withLocationId(piece.getLocationId()));
  }

  private Future<String> handleItem(CompositePoLine compPOL, boolean createItem, Piece piece, RequestContext requestContext) {
    if (piece.getItemId() != null) {
      return Future.succeededFuture(piece.getItemId());
    }
    if (createItem &&  piece.getHoldingId() != null) {
        return pieceUpdateInventoryService.manualPieceFlowCreateItemRecord(piece, compPOL, requestContext);
    }
    return Future.succeededFuture();
  }


  private Future<String> nonPackageUpdateTitleWithInstance(CompositePoLine poLine, String titleId, RequestContext requestContext) {
    if (poLine.getInstanceId() != null || PoLineCommonUtil.isInventoryUpdateNotRequired(poLine)) {
      return Future.succeededFuture(poLine.getInstanceId());
    }
    return titlesService.getTitleById(titleId, requestContext)
      .compose(title -> titleInstanceService.createTitleInstance(title, requestContext))
      .map(instanceId -> poLine.withInstanceId(instanceId).getInstanceId());
  }

  private Future<String> packageUpdateTitleWithInstance(Title title, RequestContext requestContext) {
    return titleInstanceService.createTitleInstance(title, requestContext);
  }
}
