package org.folio.service.pieces.flows.strategies;

import static java.util.stream.Collectors.toList;
import static org.folio.orders.utils.FutureUtils.asFuture;
import static org.folio.orders.utils.HelperUtils.chainCall;
import static org.folio.orders.utils.HelperUtils.collectResultsOnSuccess;
import static org.folio.orders.utils.PoLineCommonUtil.isHoldingUpdateRequiredForEresource;
import static org.folio.orders.utils.PoLineCommonUtil.isHoldingUpdateRequiredForPhysical;
import static org.folio.service.inventory.InventoryItemManager.ID;
import static org.folio.service.inventory.InventoryHoldingManager.HOLDING_PERMANENT_LOCATION_ID;

import java.util.ArrayList;
import java.util.Collections;
import java.util.List;
import java.util.Optional;

import one.util.streamex.StreamEx;
import org.apache.logging.log4j.LogManager;
import org.apache.logging.log4j.Logger;
import org.folio.orders.utils.PoLineCommonUtil;
import org.folio.rest.core.RestClient;
import org.folio.rest.core.models.RequestContext;
import org.folio.rest.jaxrs.model.PoLine;
import org.folio.rest.jaxrs.model.CompositePurchaseOrder;
import org.folio.rest.jaxrs.model.Location;
import org.folio.rest.jaxrs.model.Piece;
import org.folio.service.consortium.ConsortiumConfigurationService;
import org.folio.service.inventory.InventoryHoldingManager;
import org.folio.service.inventory.InventoryItemManager;

import io.vertx.core.Future;
import io.vertx.core.json.JsonObject;

public class ProcessInventoryMixedStrategy extends ProcessInventoryStrategy {
  private static final Logger logger = LogManager.getLogger();

  public ProcessInventoryMixedStrategy(ConsortiumConfigurationService consortiumConfigurationService) {
    super(consortiumConfigurationService);
  }

  public Future<List<Piece>> handleHoldingsAndItemsRecords(CompositePurchaseOrder compPO,
                                                           PoLine poLine,
                                                           InventoryItemManager inventoryItemManager,
                                                           InventoryHoldingManager inventoryHoldingManager,
                                                           RestClient restClient,
                                                           RequestContext requestContext) {
    logger.debug("ProcessInventoryMixedStrategy.handleHoldingsAndItemsRecords poLine.id={}", poLine.getId());
    var itemsPerHolding = updateMixedHolding(poLine, inventoryHoldingManager, restClient, requestContext);
    return collectResultsOnSuccess(itemsPerHolding)
      .map(aVoid -> {
        updateLocations(poLine);
        return null;
      })
      .compose(aVoid -> {
          List<Future<List<Piece>>> pieceFutures = new ArrayList<>();
          if (PoLineCommonUtil.isItemsUpdateRequired(poLine)) {
            for (Location location : poLine.getLocations()) {
              pieceFutures.add(inventoryItemManager.handleItemRecords(compPO, poLine, location, requestContext));
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

  private List<Future<Void>> updateMixedHolding(PoLine poLine, InventoryHoldingManager inventoryHoldingManager,
                                                RestClient restClient, RequestContext requestContext) {
    logger.debug("ProcessInventoryStrategy.updateHolding");
    return StreamEx.of(poLine.getLocations())
      .groupingBy(location -> Optional.ofNullable(location.getTenantId()))
      .values().stream()
      .map(locations -> chainCall(locations, location ->
        updateMixedHolding(poLine, location, inventoryHoldingManager, restClient, requestContext)))
      .toList();
  }

  private Future<Void> updateMixedHolding(PoLine poLine, Location location, InventoryHoldingManager inventoryHoldingManager,
                                          RestClient restClient, RequestContext requestContext) {
    return findHoldingsId(poLine, location, restClient, requestContext)
      .compose(aVoid -> consortiumConfigurationService.cloneRequestContextIfNeeded(requestContext, location))
      .compose(updatedRequestContext -> inventoryHoldingManager.getOrCreateHoldingsJsonRecord(poLine.getEresource(), poLine.getInstanceId(), location, updatedRequestContext))
      .compose(holding -> asFuture(() -> updateLocationWithHoldingInfo(holding, location)));
  }

  private void updateLocationWithHoldingInfo(JsonObject holding, Location location) {
    if (holding != null && !holding.isEmpty()) {
      String permLocationId = holding.getString(HOLDING_PERMANENT_LOCATION_ID);
      String holdingId = holding.getString(ID);
      location.setHoldingId(holdingId);
      location.setLocationId(permLocationId);
    }
  }

  private void updateLocations(PoLine poLine) {
    List<Location> locations = new ArrayList<>();
    for (Location location : poLine.getLocations()) {
      boolean physicalResource = location.getQuantityPhysical() != null && location.getQuantityPhysical() > 0;
      boolean electronicResource = location.getQuantityElectronic() != null && location.getQuantityElectronic() > 0;
      boolean physicalHolding = physicalResource && isHoldingUpdateRequiredForPhysical(poLine);
      boolean electronicHolding = electronicResource && isHoldingUpdateRequiredForEresource(poLine);
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
    poLine.setLocations(locations);
  }
}
