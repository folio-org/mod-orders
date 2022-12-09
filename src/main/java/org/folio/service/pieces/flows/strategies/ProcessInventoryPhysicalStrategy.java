package org.folio.service.pieces.flows.strategies;

import static java.util.stream.Collectors.toList;
import static org.folio.orders.utils.HelperUtils.collectResultsOnSuccess;

import java.util.ArrayList;
import java.util.List;

import org.folio.orders.utils.PoLineCommonUtil;
import org.folio.rest.core.models.RequestContext;
import org.folio.rest.jaxrs.model.CompositePoLine;
import org.folio.rest.jaxrs.model.Piece;
import org.folio.service.inventory.InventoryManager;

import io.vertx.core.Future;

public class ProcessInventoryPhysicalStrategy extends ProcessInventoryStrategy {

  public ProcessInventoryPhysicalStrategy() {
  }

  public Future<List<Piece>> handleHoldingsAndItemsRecords(CompositePoLine compPOL,
                                                                      InventoryManager inventoryManager,
                                                                      RequestContext requestContext) {
    List<Future<List<Piece>>> itemsPerHolding = new ArrayList<>();

    // Group all locations by location id because the holding should be unique for different locations
    if (PoLineCommonUtil.isHoldingUpdateRequiredForPhysical(compPOL)) {
      itemsPerHolding = updateHolding(compPOL, inventoryManager, requestContext);
    }
    return collectResultsOnSuccess(itemsPerHolding)
      .map(itemCreated -> itemCreated.stream()
        .flatMap(List::stream)
        .collect(toList())
      );
  }

}
