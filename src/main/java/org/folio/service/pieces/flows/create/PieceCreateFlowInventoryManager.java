package org.folio.service.pieces.flows.create;

import static java.util.concurrent.CompletableFuture.completedFuture;

import java.util.Optional;
import java.util.concurrent.CompletableFuture;

import org.folio.models.pieces.PieceCreationHolder;
import org.folio.rest.core.models.RequestContext;
import org.folio.rest.jaxrs.model.CompositePoLine;
import org.folio.rest.jaxrs.model.Location;
import org.folio.rest.jaxrs.model.Piece;
import org.folio.service.inventory.InventoryManager;
import org.folio.service.pieces.PieceUpdateInventoryService;
import org.folio.service.titles.TitlesService;

public class PieceCreateFlowInventoryManager {
  private final TitlesService titlesService;
  private final InventoryManager inventoryManager;
  private final PieceUpdateInventoryService pieceUpdateInventoryService;

  public PieceCreateFlowInventoryManager(TitlesService titlesService, InventoryManager inventoryManager,
    PieceUpdateInventoryService pieceUpdateInventoryService) {
    this.titlesService = titlesService;
    this.inventoryManager = inventoryManager;
    this.pieceUpdateInventoryService = pieceUpdateInventoryService;
  }

  public CompletableFuture<Void> updateInventory(PieceCreationHolder holder, RequestContext requestContext) {
    CompositePoLine compPOL = holder.getOriginPoLine();
    Piece piece = holder.getPieceToCreate();
    boolean createItem = holder.getCreateItem();
    if (Boolean.TRUE.equals(compPOL.getIsPackage())) {
      return packagePoLineUpdateInventory(compPOL, piece, createItem, requestContext);
    }
    else
    {
      return inventoryManager.updateItemWithPoLineId(piece.getItemId(), piece.getPoLineId(), requestContext);
    }
  }

  private CompletableFuture<Void> packagePoLineUpdateInventory(CompositePoLine compPOL, Piece piece, boolean createItem,
                                                                RequestContext requestContext) {
    return titlesService.getTitleById(piece.getTitleId(), requestContext)
      .thenCompose(title -> pieceUpdateInventoryService.handleInstanceRecord(title, requestContext))
      .thenCompose(title -> titlesService.updateTitle(title, requestContext).thenApply(json -> title))
      .thenCompose(title ->
      {
        if (piece.getHoldingId() != null) {
          return completedFuture(piece.getHoldingId());
        }
        Location location = new Location().withLocationId(piece.getLocationId());
        return pieceUpdateInventoryService.handleHoldingsRecord(compPOL, location, title.getInstanceId(), requestContext)
          .thenApply(holdingId -> {
            piece.setLocationId(null);
            piece.setHoldingId(holdingId);
            return holdingId;
          });
      })
      .thenCompose(holdingId -> {
        if (createItem) {
          return pieceUpdateInventoryService.createItemRecord(compPOL, holdingId, requestContext);
        }
        return null;
      })
      .thenAccept(itemId -> Optional.ofNullable(itemId).ifPresent(piece::withItemId));
  }
}
