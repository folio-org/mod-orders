package org.folio.service.pieces.flows.strategies;

import static java.util.stream.Collectors.toList;
import static org.folio.orders.utils.HelperUtils.collectResultsOnSuccess;
import static org.folio.service.inventory.InventoryManager.HOLDING_PERMANENT_LOCATION_ID;
import static org.folio.service.inventory.InventoryManager.ID;

import java.util.ArrayList;
import java.util.Collections;
import java.util.List;

import org.folio.orders.utils.PoLineCommonUtil;
import org.folio.rest.core.models.RequestContext;
import org.folio.rest.jaxrs.model.CompositePoLine;
import org.folio.rest.jaxrs.model.Location;
import org.folio.rest.jaxrs.model.Piece;
import org.folio.service.inventory.InventoryManager;

import io.vertx.core.Future;
import io.vertx.core.json.JsonObject;

public class ProcessInventoryMixedStrategy extends ProcessInventoryStrategy {

  public ProcessInventoryMixedStrategy() {
  }

  public Future<List<Piece>> handleHoldingsAndItemsRecords(CompositePoLine compPOL,
                                                                      InventoryManager inventoryManager,
                                                                      RequestContext requestContext) {
    List<Future<JsonObject>> itemsPerHolding = new ArrayList<>();
    compPOL.getLocations()
      .forEach(location -> itemsPerHolding.add(inventoryManager.getOrCreateHoldingsJsonRecord(compPOL.getInstanceId(), location, requestContext)
        .map(holding -> {
          updateLocationWithHoldingInfo(holding, location);
          return null;
        })));
    return collectResultsOnSuccess(itemsPerHolding)
      .map(aVoid -> {
        updateLocations(compPOL);
        return null;
      })
      .compose(aVoid -> {
          List<Future<List<Piece>>> pieceFutures = new ArrayList<>();
          if (PoLineCommonUtil.isItemsUpdateRequired(compPOL)) {
            for (Location location : compPOL.getLocations()) {
              pieceFutures.add(inventoryManager.handleItemRecords(compPOL, location, requestContext));
            }
          } else {
            pieceFutures.add(Future.succeededFuture(Collections.emptyList()));
          }
          return collectResultsOnSuccess(pieceFutures)
            .map(itemCreated -> itemCreated.stream()
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
