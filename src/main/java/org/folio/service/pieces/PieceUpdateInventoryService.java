package org.folio.service.pieces;

import org.apache.commons.collections4.CollectionUtils;
import org.apache.logging.log4j.LogManager;
import org.apache.logging.log4j.Logger;
import org.folio.orders.utils.HelperUtils;
import org.folio.orders.utils.PoLineCommonUtil;
import org.folio.rest.core.models.RequestContext;
import org.folio.rest.jaxrs.model.CompositePoLine;
import org.folio.rest.jaxrs.model.Location;
import org.folio.rest.jaxrs.model.Piece;
import org.folio.rest.jaxrs.model.Title;
import org.folio.service.inventory.InventoryManager;
import org.folio.service.titles.TitlesService;

import java.util.Optional;
import java.util.concurrent.CompletableFuture;

import static java.util.concurrent.CompletableFuture.completedFuture;
import static org.folio.orders.utils.HelperUtils.ID;
import static org.folio.rest.jaxrs.model.CompositePoLine.OrderFormat.ELECTRONIC_RESOURCE;

public class PieceUpdateInventoryService {
  private static final Logger logger = LogManager.getLogger(PieceUpdateInventoryService.class);

  private static final String INSTANCES = "instances";

  private final TitlesService titlesService;
  private final InventoryManager inventoryManager;

  public PieceUpdateInventoryService(TitlesService titlesService, InventoryManager inventoryManager) {
    this.titlesService = titlesService;
    this.inventoryManager = inventoryManager;
  }

  /**
   * Creates Inventory records associated with given PO line and updates PO line with corresponding links.
   *
   * @param compPOL Composite PO line to update Inventory for
   * @return CompletableFuture with void.
   */
  public CompletableFuture<Void> updateInventory(CompositePoLine compPOL, Piece piece, RequestContext requestContext) {
    if (Boolean.TRUE.equals(compPOL.getIsPackage())) {
      return titlesService.getTitleById(piece.getTitleId(), requestContext)
        .thenCompose(title -> handleInstanceRecord(title, requestContext))
        .thenCompose(title -> titlesService.saveTitle(title, requestContext).thenApply(json -> title))
        .thenCompose(title ->
        {
          if (piece.getHoldingId() != null) {
            return completedFuture(piece.getHoldingId());
          }
          return handleHoldingsRecord(compPOL, new Location().withLocationId(piece.getLocationId()), title.getInstanceId(), requestContext)
            .thenApply(holdingId -> {
              piece.setLocationId(null);
              piece.setHoldingId(holdingId);
              return holdingId;
            });
        })
        .thenCompose(holdingId -> createItemRecord(compPOL, holdingId, requestContext))
        .thenAccept(itemId -> Optional.ofNullable(itemId).ifPresent(piece::withItemId));
    }
    else
    {
      return inventoryManager.updateItemWithPoLineId(piece.getItemId(), piece.getPoLineId(), requestContext);
    }
  }

  public CompletableFuture<Title> handleInstanceRecord(Title title, RequestContext requestContext) {
    if (title.getInstanceId() != null) {
      return CompletableFuture.completedFuture(title);
    } else {
      return getOrCreateInstanceRecord(title, requestContext).thenApply(title::withInstanceId);
    }
  }

  public CompletableFuture<String> getOrCreateInstanceRecord(Title title, RequestContext requestContext) {
    // proceed with new Instance Record creation if no productId is provided
    if (!CollectionUtils.isNotEmpty(title.getProductIds())) {
      return inventoryManager.createInstanceRecord(title, requestContext);
    }

    return inventoryManager.searchInstancesByProducts(title.getProductIds(), requestContext)
      .thenCompose(instances -> {
        if (!instances.getJsonArray(INSTANCES).isEmpty()) {
          String instanceId = HelperUtils.getFirstObjectFromResponse(instances, INSTANCES).getString(ID);
          return completedFuture(instanceId);
        }
        return inventoryManager.createInstanceRecord(title, requestContext);
      });
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

  /**
   * Return id of created  Item
   */
  public CompletableFuture<String> createItemRecord(CompositePoLine compPOL, String holdingId, RequestContext requestContext) {
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
}
