package org.folio.service.pieces.flows.strategies;

import org.apache.logging.log4j.LogManager;
import org.apache.logging.log4j.Logger;
import org.folio.orders.utils.PoLineCommonUtil;
import org.folio.rest.core.models.RequestContext;
import org.folio.rest.jaxrs.model.CompositePoLine;
import org.folio.rest.jaxrs.model.Location;
import org.folio.rest.jaxrs.model.Piece;
import org.folio.service.inventory.InventoryManager;
import org.folio.service.orders.flows.update.open.OpenCompositeOrderPieceService;

import java.util.*;
import java.util.concurrent.CompletableFuture;

import static java.util.concurrent.CompletableFuture.completedFuture;
import static java.util.stream.Collectors.toList;
import static org.folio.orders.utils.HelperUtils.collectResultsOnSuccess;

public class MixedStrategy extends ProcessInventoryStrategy {

  private static final Logger logger = LogManager.getLogger(MixedStrategy.class);

  private final InventoryManager inventoryManager;
  private final OpenCompositeOrderPieceService openCompositeOrderPieceService;

  public MixedStrategy(InventoryManager inventoryManager, OpenCompositeOrderPieceService openCompositeOrderPieceService) {
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
    Map<String, List<Location>> splitLocation = splitLocation(compPOL.getLocations());

    if (PoLineCommonUtil.isHoldingUpdateRequiredForPhysical(compPOL)) {
      itemsPerHolding = holdingsUpdateRequired(compPOL,
        splitLocation.get(CompositePoLine.OrderFormat.PHYSICAL_RESOURCE.value()), requestContext);
    }
    if (PoLineCommonUtil.isHoldingUpdateRequiredForEresource(compPOL)) {
      itemsPerHolding = holdingsUpdateRequired(compPOL,
        splitLocation.get(CompositePoLine.OrderFormat.ELECTRONIC_RESOURCE.value()), requestContext);
    }

    return collectResultsOnSuccess(itemsPerHolding)
      .thenApply(itemCreated -> itemCreated.stream()
        .flatMap(List::stream)
        .collect(toList())
      );
  }

  private List<CompletableFuture<List<Piece>>> holdingsUpdateRequired(CompositePoLine compPOL,
                                                                      List<Location> locations, RequestContext requestContext) {

    List<CompletableFuture<List<Piece>>> itemsPerHolding = new ArrayList<>();
    locations.forEach(location -> itemsPerHolding.add(
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
        )));

    return itemsPerHolding;
  }

  private Map<String, List<Location>> splitLocation(List<Location> locations) {
    Map<String, List<Location>> locationMap = new HashMap<>();

    List<Location> physicalLocations = new ArrayList<>();
    List<Location> electronicLocations = new ArrayList<>();

    for (Location location : locations) {

      if (location.getQuantityPhysical() > 0) {
        Location physicalLocation = new Location();
        physicalLocation.setLocationId(location.getLocationId());
        physicalLocation.setHoldingId(location.getHoldingId());
        physicalLocation.setQuantity(location.getQuantityPhysical());
        physicalLocation.setQuantityPhysical(location.getQuantityPhysical());

        physicalLocations.add(physicalLocation);
      }

      if (location.getQuantityElectronic() > 0) {
        Location electronicLocation = new Location();
        electronicLocation.setLocationId(location.getLocationId());
        electronicLocation.setHoldingId(location.getHoldingId());
        electronicLocation.setQuantity(location.getQuantityElectronic());
        electronicLocation.setQuantityElectronic(location.getQuantityElectronic());

        electronicLocations.add(electronicLocation);
      }
    }

    locationMap.put(CompositePoLine.OrderFormat.PHYSICAL_RESOURCE.value(), physicalLocations);
    locationMap.put(CompositePoLine.OrderFormat.ELECTRONIC_RESOURCE.value(), electronicLocations);

    return locationMap;
  }
}
