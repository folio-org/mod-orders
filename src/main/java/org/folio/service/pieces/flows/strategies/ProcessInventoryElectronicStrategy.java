package org.folio.service.pieces.flows.strategies;

import static java.util.stream.Collectors.toList;
import static org.folio.orders.utils.HelperUtils.collectResultsOnSuccess;

import java.util.ArrayList;
import java.util.List;
import java.util.concurrent.CompletableFuture;

import org.folio.orders.utils.PoLineCommonUtil;
import org.folio.rest.core.models.RequestContext;
import org.folio.rest.jaxrs.model.CompositePoLine;
import org.folio.rest.jaxrs.model.Piece;
import org.folio.service.inventory.InventoryManager;

public class ProcessInventoryElectronicStrategy extends ProcessInventoryStrategy {

  public ProcessInventoryElectronicStrategy() {
  }

  public CompletableFuture<List<Piece>> handleHoldingsAndItemsRecords(CompositePoLine compPOL,
                                                                      InventoryManager inventoryManager,
                                                                      RequestContext requestContext) {
    List<CompletableFuture<List<Piece>>> itemsPerHolding = new ArrayList<>();

    // Group all locations by location id because the holding should be unique for different locations
    if (PoLineCommonUtil.isHoldingUpdateRequiredForEresource(compPOL)) {
      itemsPerHolding = updateHolding(compPOL, inventoryManager, requestContext);
    }
    return collectResultsOnSuccess(itemsPerHolding)
      .thenApply(itemCreated -> itemCreated.stream()
        .flatMap(List::stream)
        .collect(toList())
      );
  }
}
