package org.folio.service.pieces.flows.update;

import static java.util.concurrent.CompletableFuture.completedFuture;

import java.util.concurrent.CompletableFuture;

import org.apache.logging.log4j.LogManager;
import org.apache.logging.log4j.Logger;
import org.folio.models.pieces.PieceUpdateHolder;
import org.folio.rest.core.models.RequestContext;
import org.folio.rest.jaxrs.model.CompositePoLine;
import org.folio.rest.jaxrs.model.Piece;
import org.folio.service.inventory.InventoryManager;
import org.folio.service.pieces.PieceUpdateInventoryService;
import org.folio.service.titles.TitlesService;

public class PieceUpdateFlowInventoryManager {
  private static final Logger logger = LogManager.getLogger(PieceUpdateFlowInventoryManager.class);

  private final TitlesService titlesService;
  private final PieceUpdateInventoryService pieceUpdateInventoryService;
  private final InventoryManager inventoryManager;

  public PieceUpdateFlowInventoryManager(TitlesService titlesService,  PieceUpdateInventoryService pieceUpdateInventoryService,
                                         InventoryManager inventoryManager) {
    this.titlesService = titlesService;
    this.pieceUpdateInventoryService = pieceUpdateInventoryService;
    this.inventoryManager = inventoryManager;
  }

  public CompletableFuture<Void> processInventory(PieceUpdateHolder holder, RequestContext requestContext) {
    return inventoryManager.updateItemWithPoLineId(holder.getPieceToUpdate().getItemId(), holder.getPieceToUpdate().getPoLineId(), requestContext)
      .thenCompose(aVoid -> {
        if (Boolean.TRUE.equals(holder.getOriginPoLine().getIsPackage())) {
          return packagePoLineUpdateInventory(holder.getOriginPoLine(), holder.getPieceToUpdate(), holder.isCreateItem(), requestContext);
        }
        else
        {
          return nonPackagePoLineUpdateInventory(holder.getOriginPoLine(), holder.getPieceToUpdate(), holder.isCreateItem(), requestContext);
        }
      });
  }

  private CompletableFuture<Void> nonPackagePoLineUpdateInventory(CompositePoLine compPOL, Piece piece, boolean createItem,
                                                                  RequestContext requestContext) {
    return completedFuture(null);
  }

  private CompletableFuture<Void> packagePoLineUpdateInventory(CompositePoLine compPOL, Piece piece, boolean createItem,
                                                                RequestContext requestContext) {
    return completedFuture(null);
  }


}
