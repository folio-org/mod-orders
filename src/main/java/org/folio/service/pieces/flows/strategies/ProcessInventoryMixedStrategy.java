package org.folio.service.pieces.flows.strategies;

import io.vertx.core.json.JsonObject;
import org.apache.logging.log4j.LogManager;
import org.apache.logging.log4j.Logger;
import org.folio.orders.utils.PoLineCommonUtil;
import org.folio.rest.core.models.RequestContext;
import org.folio.rest.jaxrs.model.CompositePoLine;
import org.folio.rest.jaxrs.model.Location;
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
import static org.folio.service.inventory.InventoryManager.HOLDING_PERMANENT_LOCATION_ID;
import static org.folio.service.inventory.InventoryManager.ID;

public class ProcessInventoryMixedStrategy extends ProcessInventoryStrategy {

  private static final Logger logger = LogManager.getLogger(ProcessInventoryMixedStrategy.class);

  private final InventoryManager inventoryManager;
  private final OpenCompositeOrderPieceService openCompositeOrderPieceService;

  public ProcessInventoryMixedStrategy(InventoryManager inventoryManager, OpenCompositeOrderPieceService openCompositeOrderPieceService) {
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
    List<CompletableFuture<Void>> itemsPerHolding = new ArrayList<>();
    compPOL.getLocations().forEach(location -> {
      itemsPerHolding.add(inventoryManager.getOrCreateHoldingsJsonRecord(compPOL.getInstanceId(), location, requestContext)
        .thenAccept(holding -> updateLocationWithHoldingInfo(holding, location))
        .thenAccept(aVoid -> updateLocations(compPOL)));
    });
    return collectResultsOnSuccess(itemsPerHolding)
      .thenCompose(aVoid -> {
          List<CompletableFuture<List<Piece>>> pieceFutures = new ArrayList<>();
          if (PoLineCommonUtil.isItemsUpdateRequired(compPOL)) {
            for (Location location : compPOL.getLocations()) {
              pieceFutures.add(inventoryManager.handleItemRecords(compPOL, location, requestContext));
            }
          } else {
            pieceFutures.add(completedFuture(Collections.emptyList()));
          }
          return collectResultsOnSuccess(pieceFutures)
            .thenApply(itemCreated -> itemCreated.stream()
              .flatMap(List::stream)
              .collect(toList())
            );
        }
      );

  }

  private void updateLocationWithHoldingInfo(JsonObject holding, Location location) {
    if (holding != null && !holding.isEmpty()) {
      String permLocationId = holding.getString(HOLDING_PERMANENT_LOCATION_ID);
      String holdingId = holding.getString(ID);
      location.setHoldingId(holdingId);
      location.setLocationId(permLocationId);
    }
  }

  private void updateLocations(CompositePoLine compPOL) {
    List<Location> locations = new ArrayList<>();
    for (Location location : compPOL.getLocations()) {
      if (location.getQuantityPhysical() != null && location.getQuantityPhysical() > 0) {
        Location physicalLocation = new Location();
        if (PoLineCommonUtil.isHoldingUpdateRequiredForPhysical(compPOL)) {
          physicalLocation.setHoldingId(location.getHoldingId());
        } else {
          physicalLocation.setLocationId(location.getLocationId());
        }
        physicalLocation.setQuantity(location.getQuantityPhysical());
        physicalLocation.setQuantityPhysical(location.getQuantityPhysical());
        locations.add(physicalLocation);
      }

      if (location.getQuantityElectronic() != null && location.getQuantityElectronic() > 0) {
        Location electronicLocation = new Location();
        if (PoLineCommonUtil.isHoldingUpdateRequiredForEresource(compPOL)) {
          electronicLocation.setHoldingId(location.getHoldingId());
        } else {
          electronicLocation.setLocationId(location.getLocationId());
        }
        electronicLocation.setQuantity(location.getQuantityElectronic());
        electronicLocation.setQuantityElectronic(location.getQuantityElectronic());
        locations.add(electronicLocation);
      }
    }
    compPOL.setLocations(null);
    compPOL.setLocations(locations);
  }
}
