package org.folio.service.pieces.flows.strategies;

import org.apache.logging.log4j.LogManager;
import org.apache.logging.log4j.Logger;
import org.folio.orders.utils.PoLineCommonUtil;
import org.folio.rest.core.models.RequestContext;
import org.folio.rest.jaxrs.model.CompositePoLine;
import org.folio.rest.jaxrs.model.Piece;
import org.folio.service.inventory.InventoryManager;
import org.folio.service.orders.flows.update.open.OpenCompositeOrderPieceService;

import java.util.ArrayList;
import java.util.Collections;
import java.util.List;
import java.util.concurrent.CompletableFuture;

import static java.util.concurrent.CompletableFuture.completedFuture;
import static java.util.stream.Collectors.toList;
import static org.folio.orders.utils.HelperUtils.collectResultsOnSuccess;

public class ProcessInventoryElectronicStrategy extends ProcessInventoryStrategy {

  private static final Logger logger = LogManager.getLogger(ProcessInventoryElectronicStrategy.class);

  private final InventoryManager inventoryManager;
  private final OpenCompositeOrderPieceService openCompositeOrderPieceService;

  public ProcessInventoryElectronicStrategy(InventoryManager inventoryManager, OpenCompositeOrderPieceService openCompositeOrderPieceService) {
    this.inventoryManager = inventoryManager;
    this.openCompositeOrderPieceService = openCompositeOrderPieceService;
  }

  public CompletableFuture<Void> processInventory(CompositePoLine compPOL, String titleId,
                                                  boolean isInstanceMatchingDisabled, RequestContext requestContext) {
    if (Boolean.TRUE.equals(compPOL.getIsPackage())) {
      return completedFuture(null);
    }

    if (PoLineCommonUtil.isInventoryUpdateNotRequired(compPOL)) {
      return handlePieces(compPOL, titleId, Collections.emptyList(), isInstanceMatchingDisabled, requestContext);
    }

    return inventoryManager.openOrderHandleInstance(compPOL, isInstanceMatchingDisabled, requestContext)
      .thenCompose(compPOLWithInstanceId -> handleHoldingsAndItemsRecords(compPOLWithInstanceId, requestContext))
      .thenCompose(piecesWithItemId -> handlePieces(compPOL, titleId, piecesWithItemId, isInstanceMatchingDisabled, requestContext));
  }

  protected CompletableFuture<Void> handlePieces(CompositePoLine compPOL, String titleId, List<Piece> piecesWithItemId,
                                              boolean isInstanceMatchingDisabled, RequestContext requestContext) {
    // don't create pieces, if no inventory updates and receiving not required
    if (PoLineCommonUtil.isReceiptNotRequired(compPOL.getReceiptStatus())) {
      return completedFuture(null);
    }
    // do not create pieces in case of check-in flow
    if (compPOL.getCheckinItems() != null && compPOL.getCheckinItems()) {
      return completedFuture(null);
    }
    return openCompositeOrderPieceService.handlePieces(compPOL, titleId, piecesWithItemId, isInstanceMatchingDisabled, requestContext).thenRun(
      () -> logger.info("Create pieces for PO Line with '{}' id where inventory updates are not required", compPOL.getId()));
  }

  public CompletableFuture<List<Piece>> handleHoldingsAndItemsRecords(CompositePoLine compPOL, RequestContext requestContext) {
    List<CompletableFuture<List<Piece>>> itemsPerHolding = new ArrayList<>();

    // Group all locations by location id because the holding should be unique for different locations
    if (PoLineCommonUtil.isHoldingUpdateRequiredForEresource(compPOL)) {
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
}
