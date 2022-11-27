package org.folio.service.pieces.flows.create;

import java.util.Optional;

import org.apache.logging.log4j.LogManager;
import org.apache.logging.log4j.Logger;
import org.folio.orders.utils.PoLineCommonUtil;
import org.folio.rest.core.models.RequestContext;
import org.folio.rest.jaxrs.model.CompositePoLine;
import org.folio.rest.jaxrs.model.Location;
import org.folio.rest.jaxrs.model.Piece;
import org.folio.rest.jaxrs.model.Title;
import org.folio.service.inventory.InventoryManager;
import org.folio.service.pieces.PieceUpdateInventoryService;
import org.folio.service.pieces.flows.DefaultPieceFlowsValidator;
import org.folio.service.titles.TitlesService;

import io.vertx.core.Future;

public class PieceCreateFlowInventoryManager {
  private static final Logger logger = LogManager.getLogger(PieceCreateFlowInventoryManager.class);

  private final TitlesService titlesService;
  private final PieceUpdateInventoryService pieceUpdateInventoryService;
  private final InventoryManager inventoryManager;

  public PieceCreateFlowInventoryManager(TitlesService titlesService,  PieceUpdateInventoryService pieceUpdateInventoryService,
                                         InventoryManager inventoryManager) {
    this.titlesService = titlesService;
    this.pieceUpdateInventoryService = pieceUpdateInventoryService;
    this.inventoryManager = inventoryManager;
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
      .compose(title -> handleHolding(compPOL, piece, title.getInstanceId(), requestContext))
      .compose(holdingId -> handleItem(compPOL, createItem, piece, requestContext))
      .onSuccess(itemId -> Optional.ofNullable(itemId).ifPresent(piece::withItemId))
      .mapEmpty();
  }

  private Future<Location> handleHolding(CompositePoLine compPOL, Piece piece, String instanceId, RequestContext requestContext) {
    if (piece.getHoldingId() != null) {
      return Future.succeededFuture(new Location().withHoldingId(piece.getHoldingId()));
    }
    if (instanceId != null && DefaultPieceFlowsValidator.isCreateHoldingForPiecePossible(piece, compPOL)) {
      Location location = new Location().withLocationId(piece.getLocationId());
      return inventoryManager.getOrCreateHoldingsRecord(instanceId, location, requestContext)
                    .map(holdingId -> {
                          Optional.ofNullable(holdingId).ifPresent(holdingIdP -> {
                            piece.setLocationId(null);
                            piece.setHoldingId(holdingId);
                            location.setLocationId(null);
                            location.setHoldingId(holdingId);
                          });
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
    if (poLine.getInstanceId() == null && !PoLineCommonUtil.isInventoryUpdateNotRequired(poLine)) {
      return titlesService.getTitleById(titleId, requestContext)
        .compose(title -> {
          if (title.getInstanceId() == null) {
            return createTitleInstance(title, requestContext);
          }
          return Future.succeededFuture(title.getInstanceId());
        })
        .map(instanceId -> poLine.withInstanceId(instanceId).getInstanceId());
    }
    return Future.succeededFuture(poLine.getInstanceId());
  }

  private Future<Title> packageUpdateTitleWithInstance(Title title, RequestContext requestContext) {
    if (title.getInstanceId() != null) {
      return Future.succeededFuture(title);
    } else {
      return inventoryManager.getOrCreateInstanceRecord(title, requestContext)
        .map(title::withInstanceId)
        .compose(titleWithInstanceId -> titlesService.saveTitle(titleWithInstanceId, requestContext)
          .map(json -> title));
    }
  }

  private Future<String> createTitleInstance(Title title, RequestContext requestContext) {
    return inventoryManager.getOrCreateInstanceRecord(title, requestContext)
      .map(title::withInstanceId)
      .compose(titleWithInstanceId ->
        titlesService.saveTitle(titleWithInstanceId, requestContext)
          .map(aVoid -> title.getInstanceId())
      );
  }
}
