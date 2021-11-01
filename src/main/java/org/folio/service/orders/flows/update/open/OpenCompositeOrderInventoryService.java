package org.folio.service.orders.flows.update.open;

import org.apache.logging.log4j.LogManager;
import org.apache.logging.log4j.Logger;
import org.folio.completablefuture.FolioVertxCompletableFuture;
import org.folio.orders.utils.PoLineCommonUtil;
import org.folio.rest.core.models.RequestContext;
import org.folio.rest.jaxrs.model.CompositePoLine;
import org.folio.rest.jaxrs.model.Location;
import org.folio.rest.jaxrs.model.Piece;
import org.folio.service.inventory.InventoryManager;
import org.folio.service.pieces.PieceStorageService;
import org.folio.service.titles.TitlesService;

import java.util.ArrayList;
import java.util.Collections;
import java.util.List;
import java.util.concurrent.CompletableFuture;

import static java.util.concurrent.CompletableFuture.completedFuture;
import static java.util.stream.Collectors.toList;
import static org.folio.orders.utils.HelperUtils.collectResultsOnSuccess;
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
    FolioVertxCompletableFuture<String> itemFuture = new FolioVertxCompletableFuture<>(requestContext.getContext());
    try {
      if (compPOL.getOrderFormat() == ELECTRONIC_RESOURCE) {
        inventoryManager.createMissingElectronicItems(compPOL, holdingId, ITEM_QUANTITY, requestContext)
          .thenApply(idS -> itemFuture.complete(idS.get(0)))
          .exceptionally(itemFuture::completeExceptionally);
      } else {
        inventoryManager.createMissingPhysicalItems(compPOL, holdingId, ITEM_QUANTITY, requestContext)
          .thenApply(idS -> itemFuture.complete(idS.get(0)))
          .exceptionally(itemFuture::completeExceptionally);
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

  /**
   * Returns list of pieces with populated item and location id's corresponding to given PO line.
   * Items are either retrieved from Inventory or new ones are created if no corresponding item records exist yet.
   *
   * @param compPOL   PO line to retrieve/create Item Records for. At this step PO Line must contain instance Id
   * @return future with list of pieces with item and location id's
   */
  public CompletableFuture<List<Piece>> handleHoldingsAndItemsRecords(CompositePoLine compPOL, RequestContext requestContext) {
    List<CompletableFuture<List<Piece>>> itemsPerHolding = new ArrayList<>();

    // Group all locations by location id because the holding should be unique for different locations
    if (PoLineCommonUtil.isHoldingsUpdateRequired(compPOL)) {
      compPOL.getLocations().forEach(location -> {
        itemsPerHolding.add(
          // Search for or create a new holdings record and then create items for it if required
          inventoryManager.getOrCreateHoldingsRecord(compPOL.getInstanceId(), location, requestContext)
            .thenCompose(holdingId -> {
                // Items are not going to be created when create inventory is "Instance, Holding"
                exchangeLocationIdWithHoldingId(location, holdingId);
                if (PoLineCommonUtil.isItemsUpdateRequired(compPOL)) {
                  return inventoryManager.handleItemRecords(compPOL, location, requestContext);
                } else {
                  return completedFuture(Collections.emptyList());
                }
              }
            ));
      });
    }
    return collectResultsOnSuccess(itemsPerHolding)
      .thenApply(itemCreated -> itemCreated.stream()
        .flatMap(List::stream)
        .collect(toList())
      );
  }

  private void exchangeLocationIdWithHoldingId(Location location, String holdingId) {
    addHoldingId(List.of(location), holdingId);
    location.setLocationId(null);
  }

  private void addHoldingId(List<Location> polLocations, String holdingId) {
    polLocations.forEach(location -> location.setHoldingId(holdingId));
  }

}
