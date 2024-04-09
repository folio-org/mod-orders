package org.folio.service.pieces.flows.strategies;

import static java.util.stream.Collectors.toList;
import static org.folio.orders.utils.HelperUtils.collectResultsOnSuccess;
import static org.folio.orders.utils.PoLineCommonUtil.isHoldingUpdateRequiredForEresource;
import static org.folio.orders.utils.PoLineCommonUtil.isHoldingUpdateRequiredForPhysical;
import static org.folio.service.inventory.InventoryManager.HOLDING_PERMANENT_LOCATION_ID;
import static org.folio.service.inventory.InventoryManager.ID;

import java.util.ArrayList;
import java.util.Collections;
import java.util.List;

import org.folio.orders.utils.PoLineCommonUtil;
import org.folio.rest.core.RestClient;
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
                                                                      RestClient restClient,
                                                                      RequestContext requestContext) {
    List<Future<JsonObject>> itemsPerHolding = updateMixedHolding(compPOL, inventoryManager, restClient, requestContext);
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

  private List<Future<JsonObject>> updateMixedHolding(CompositePoLine compPOL, InventoryManager inventoryManager, RestClient restClient, RequestContext requestContext) {
    List<Future<JsonObject>> itemsPerHolding = new ArrayList<>();
    compPOL.getLocations().forEach(location -> itemsPerHolding.add(
      findHoldingsId(compPOL, location, restClient, requestContext)
        .compose(aVoid -> inventoryManager.getOrCreateHoldingsJsonRecord(compPOL.getEresource(), compPOL.getInstanceId(), location, requestContext)
          .map(holding -> {
            updateLocationWithHoldingInfo(holding, location);
            return null;
          }))
    ));
    return itemsPerHolding;
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
      boolean physicalResource = location.getQuantityPhysical() != null && location.getQuantityPhysical() > 0;
      boolean electronicResource = location.getQuantityElectronic() != null && location.getQuantityElectronic() > 0;
      boolean physicalHolding = physicalResource && isHoldingUpdateRequiredForPhysical(compPOL);
      boolean electronicHolding = electronicResource && isHoldingUpdateRequiredForEresource(compPOL);
      Location newLocation = JsonObject.mapFrom(location).mapTo(Location.class);
      // Physical/electronic locations have to be merged when they have the same holdingId or locationId,
      // but separate otherwise, and they can't have both holdingId and locationId.
      // This will split locations only if one resource type is getting a holdings update but not the other.
      if (electronicHolding && physicalResource && !physicalHolding) {
        Location newElectronicLocation = JsonObject.mapFrom(location).mapTo(Location.class);
        newElectronicLocation.setQuantityPhysical(null);
        newElectronicLocation.setLocationId(null);
        newElectronicLocation.setQuantity(location.getQuantityElectronic());
        locations.add(newElectronicLocation);
        newLocation.setQuantityElectronic(null);
        newLocation.setHoldingId(null);
        newLocation.setQuantity(location.getQuantityPhysical());
      } else if (physicalHolding && electronicResource && !electronicHolding) {
        Location newPhysicalLocation = JsonObject.mapFrom(location).mapTo(Location.class);
        newPhysicalLocation.setQuantityElectronic(null);
        newPhysicalLocation.setLocationId(null);
        newPhysicalLocation.setQuantity(location.getQuantityPhysical());
        locations.add(newPhysicalLocation);
        newLocation.setQuantityPhysical(null);
        newLocation.setHoldingId(null);
        newLocation.setQuantity(location.getQuantityElectronic());
      } else {
        if (physicalHolding || electronicHolding) {
          newLocation.setLocationId(null);
        } else {
          newLocation.setHoldingId(null);
        }
      }
      locations.add(newLocation);
    }
    compPOL.setLocations(locations);
  }
}
