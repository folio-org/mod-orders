package org.folio.service.pieces.flows.create;

import static java.util.concurrent.CompletableFuture.completedFuture;

import java.util.Optional;
import java.util.concurrent.CompletableFuture;

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
import org.folio.service.titles.TitlesService;

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

  public CompletableFuture<Void> processInventory(CompositePoLine compPOL,  Piece piece,  boolean createItem,
                                                  RequestContext requestContext) {
    if (Boolean.TRUE.equals(compPOL.getIsPackage())) {
      return packagePoLineUpdateInventory(compPOL, piece, createItem, requestContext);
    }
    else
    {
      return nonPackagePoLineUpdateInventory(compPOL, piece, createItem, requestContext);
    }
  }

  private CompletableFuture<Void> nonPackagePoLineUpdateInventory(CompositePoLine compPOL, Piece piece, boolean createItem,
                                                                  RequestContext requestContext) {
    return nonPackageUpdateTitleWithInstance(compPOL, piece.getTitleId(), requestContext)
      .thenCompose(instanceId -> handleHolding(compPOL, piece, instanceId, requestContext))
      .thenCompose(holdingId -> handleItem(compPOL, createItem, piece, requestContext))
      .thenAccept(itemId -> Optional.ofNullable(itemId).ifPresent(piece::withItemId));
  }

  private CompletableFuture<Void> packagePoLineUpdateInventory(CompositePoLine compPOL, Piece piece, boolean createItem,
                                                                RequestContext requestContext) {
    return titlesService.getTitleById(piece.getTitleId(), requestContext)
      .thenCompose(title -> packageUpdateTitleWithInstance(title, requestContext))
      .thenCompose(title -> handleHolding(compPOL, piece, title.getInstanceId(), requestContext))
      .thenCompose(holdingId -> handleItem(compPOL, createItem, piece, requestContext))
      .thenAccept(itemId -> Optional.ofNullable(itemId).ifPresent(piece::withItemId));
  }

  private CompletableFuture<Location> handleHolding(CompositePoLine compPOL, Piece piece, String instanceId, RequestContext requestContext) {
    if (piece.getHoldingId() != null) {
      return completedFuture(new Location().withHoldingId(piece.getHoldingId()));
    }
    if (instanceId != null && PieceCreateFlowValidator.isCreateHoldingForPiecePossible(piece, compPOL)) {
      Location location = new Location().withLocationId(piece.getLocationId());
      return inventoryManager.getOrCreateHoldingsRecord(instanceId, location, requestContext).thenApply(holdingId -> {
        Optional.ofNullable(holdingId).ifPresent(holdingIdP -> {
          piece.setLocationId(null);
          piece.setHoldingId(holdingId);
          location.setLocationId(null);
          location.setHoldingId(holdingId);
        });
        return location;
      });
    }
    return completedFuture(new Location().withLocationId(piece.getLocationId()));
  }

  private CompletableFuture<String> handleItem(CompositePoLine compPOL, boolean createItem, Piece piece, RequestContext requestContext) {
    if (piece.getItemId() != null) {
      return completedFuture(piece.getItemId());
    }
    if (createItem && PieceCreateFlowValidator.isCreateItemForPiecePossible(piece, compPOL) && piece.getHoldingId() != null) {
        return pieceUpdateInventoryService.createItemRecord(compPOL, piece.getHoldingId(), requestContext);
    }
    return CompletableFuture.completedFuture(null);
  }


  private CompletableFuture<String> nonPackageUpdateTitleWithInstance(CompositePoLine poLine, String titleId, RequestContext requestContext) {
    if (poLine.getInstanceId() == null && !PoLineCommonUtil.isInventoryUpdateNotRequired(poLine)) {
      return titlesService.getTitleById(titleId, requestContext)
        .thenCompose(title -> {
          if (title.getInstanceId() == null) {
            return createTitleInstance(title, requestContext);
          }
          return completedFuture(title.getInstanceId());
        })
        .thenApply(instanceId -> poLine.withInstanceId(instanceId).getInstanceId());
    }
    return completedFuture(poLine.getInstanceId());
  }

  private CompletableFuture<Title> packageUpdateTitleWithInstance(Title title, RequestContext requestContext) {
    if (title.getInstanceId() != null) {
      return CompletableFuture.completedFuture(title);
    } else {
      return pieceUpdateInventoryService.getOrCreateInstanceRecord(title, requestContext)
        .thenApply(title::withInstanceId)
        .thenCompose(titleWithInstanceId -> titlesService.saveTitle(titleWithInstanceId, requestContext).thenApply(json -> title));
    }
  }

  private CompletableFuture<String> createTitleInstance(Title title, RequestContext requestContext) {
    return pieceUpdateInventoryService.getOrCreateInstanceRecord(title, requestContext)
      .thenApply(title::withInstanceId)
      .thenCompose(titleWithInstanceId ->
        titlesService.saveTitle(titleWithInstanceId, requestContext).thenApply(aVoid -> title.getInstanceId())
      );
  }
}
