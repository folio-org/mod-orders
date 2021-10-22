package org.folio.service.orders.flows.open;

import org.apache.logging.log4j.LogManager;
import org.apache.logging.log4j.Logger;
import org.folio.orders.utils.PoLineCommonUtil;
import org.folio.rest.core.models.RequestContext;
import org.folio.rest.jaxrs.model.CompositePoLine;
import org.folio.rest.jaxrs.model.Location;
import org.folio.service.inventory.InventoryManager;
import org.folio.service.pieces.PieceStorageService;
import org.folio.service.titles.TitlesService;

import java.util.concurrent.CompletableFuture;

import static org.folio.rest.jaxrs.model.CompositePoLine.OrderFormat.ELECTRONIC_RESOURCE;

public class OpenCompositeOrderInventoryService {
  private static final Logger logger = LogManager.getLogger(OpenCompositeOrderInventoryService.class);

  private final TitlesService titlesService;
  private final InventoryManager inventoryManager;
  private final PieceStorageService pieceStorageService;

  public OpenCompositeOrderInventoryService(TitlesService titlesService, InventoryManager inventoryManager,
    PieceStorageService pieceStorageService) {
    this.titlesService = titlesService;
    this.inventoryManager = inventoryManager;
    this.pieceStorageService = pieceStorageService;
  }


  /**
   * Return id of created  Item
   */
  public CompletableFuture<String> createItemRecord(CompositePoLine compPOL, String holdingId,
                                                    RequestContext requestContext) {
    final int ITEM_QUANTITY = 1;
    logger.debug("Handling {} items for PO Line and holdings with id={}", ITEM_QUANTITY, holdingId);
    CompletableFuture<String> itemFuture = new CompletableFuture<>();
    try {
      if (PoLineCommonUtil.isItemsUpdateRequired(compPOL)) {
        if (compPOL.getOrderFormat() == ELECTRONIC_RESOURCE) {
          inventoryManager.createMissingElectronicItems(compPOL, holdingId, ITEM_QUANTITY, requestContext)
            .thenApply(idS -> itemFuture.complete(idS.get(0)))
            .exceptionally(itemFuture::completeExceptionally);
        } else {
          inventoryManager.createMissingPhysicalItems(compPOL, holdingId, ITEM_QUANTITY, requestContext)
            .thenApply(idS -> itemFuture.complete(idS.get(0)))
            .exceptionally(itemFuture::completeExceptionally);
        }
      }
      else {
        itemFuture.complete(null);
      }
    } catch (Exception e) {
      itemFuture.completeExceptionally(e);
    }
    return itemFuture;
  }

  /**
   * Return id of created  Holding
   */
  public CompletableFuture<String> handleHoldingsRecord(final CompositePoLine compPOL, Location location, String instanceId,
    RequestContext requestContext) {
    try {
      if (PoLineCommonUtil.isHoldingsUpdateRequired(compPOL)) {
        return inventoryManager.getOrCreateHoldingsRecord(instanceId, location, requestContext);
      } else {
        return CompletableFuture.completedFuture(null);
      }
    }
    catch (Exception e) {
      return CompletableFuture.failedFuture(e);
    }
  }
}
