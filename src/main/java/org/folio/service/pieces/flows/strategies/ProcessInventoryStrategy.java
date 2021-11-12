package org.folio.service.pieces.flows.strategies;

import static java.util.concurrent.CompletableFuture.completedFuture;

import java.util.ArrayList;
import java.util.Collections;
import java.util.List;
import java.util.concurrent.CompletableFuture;

import org.apache.logging.log4j.LogManager;
import org.apache.logging.log4j.Logger;
import org.folio.orders.utils.PoLineCommonUtil;
import org.folio.rest.core.models.RequestContext;
import org.folio.rest.jaxrs.model.CompositePoLine;
import org.folio.rest.jaxrs.model.Location;
import org.folio.rest.jaxrs.model.Piece;
import org.folio.service.inventory.InventoryManager;
import org.folio.service.orders.flows.update.open.OpenCompositeOrderPieceService;

public abstract class ProcessInventoryStrategy {

  private static final Logger logger = LogManager.getLogger(ProcessInventoryStrategy.class);

  /**
   * Returns list of pieces with populated item and location id's corresponding to given PO line.
   * Items are either retrieved from Inventory or new ones are created if no corresponding item records exist yet.
   *
   * @param compPOL PO line to retrieve/create Item Records for. At this step PO Line must contain instance Id
   * @return future with list of pieces with item and location id's
   */
  protected abstract CompletableFuture<List<Piece>> handleHoldingsAndItemsRecords(CompositePoLine compPOL,
                                                                                  InventoryManager inventoryManager,
                                                                                  RequestContext requestContext);

  /**
   * Creates Inventory records associated with given PO line and updates PO line with corresponding links.
   *
   * @param compPOL Composite PO line to update Inventory for
   * @return CompletableFuture with void.
   */
  public CompletableFuture<Void> processInventory(CompositePoLine compPOL, String titleId,
                                                  InventoryManager inventoryManager,
                                                  OpenCompositeOrderPieceService openCompositeOrderPieceService,
                                                  RequestContext requestContext) {
    if (Boolean.TRUE.equals(compPOL.getIsPackage())) {
      return completedFuture(null);
    }

    if (PoLineCommonUtil.isInventoryUpdateNotRequired(compPOL)) {
      return handlePieces(compPOL, titleId, Collections.emptyList(), requestContext, openCompositeOrderPieceService);
    }

    return inventoryManager.openOrderHandleInstance(compPOL, requestContext)
      .thenCompose(compPOLWithInstanceId -> handleHoldingsAndItemsRecords(compPOLWithInstanceId, inventoryManager, requestContext))
      .thenCompose(piecesWithItemId -> handlePieces(compPOL, titleId, piecesWithItemId, requestContext, openCompositeOrderPieceService));
  }

  protected List<CompletableFuture<List<Piece>>> updateHolding(CompositePoLine compPOL, InventoryManager inventoryManager,
                                                               RequestContext requestContext) {
    List<CompletableFuture<List<Piece>>> itemsPerHolding = new ArrayList<>();
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
    return itemsPerHolding;
  }

  private void exchangeLocationIdWithHoldingId(Location location, String holdingId) {
    addHoldingId(List.of(location), holdingId);
    location.setLocationId(null);
  }

  private void addHoldingId(List<Location> polLocations, String holdingId) {
    polLocations.forEach(location -> location.setHoldingId(holdingId));
  }

  private CompletableFuture<Void> handlePieces(CompositePoLine compPOL, String titleId, List<Piece> piecesWithItemId,
                                                 RequestContext requestContext,
                                                 OpenCompositeOrderPieceService openCompositeOrderPieceService) {
    // don't create pieces, if no inventory updates and receiving not required
    if (PoLineCommonUtil.isReceiptNotRequired(compPOL.getReceiptStatus())) {
      return completedFuture(null);
    }
    // do not create pieces in case of check-in flow
    if (compPOL.getCheckinItems() != null && compPOL.getCheckinItems()) {
      return completedFuture(null);
    }
    return openCompositeOrderPieceService.handlePieces(compPOL, titleId, piecesWithItemId, requestContext).thenRun(
      () -> logger.info("Create pieces for PO Line with '{}' id where inventory updates are not required", compPOL.getId()));
  }

}
