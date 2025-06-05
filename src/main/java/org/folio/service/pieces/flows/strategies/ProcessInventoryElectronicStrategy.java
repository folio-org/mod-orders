package org.folio.service.pieces.flows.strategies;

import static java.util.stream.Collectors.toList;
import static org.folio.orders.utils.HelperUtils.collectResultsOnSuccess;

import java.util.ArrayList;
import java.util.List;

import org.apache.logging.log4j.LogManager;
import org.apache.logging.log4j.Logger;
import org.folio.orders.utils.PoLineCommonUtil;
import org.folio.rest.core.RestClient;
import org.folio.rest.core.models.RequestContext;
import org.folio.rest.jaxrs.model.PoLine;
import org.folio.rest.jaxrs.model.CompositePurchaseOrder;
import org.folio.rest.jaxrs.model.Piece;
import org.folio.service.consortium.ConsortiumConfigurationService;
import org.folio.service.inventory.InventoryHoldingManager;
import org.folio.service.inventory.InventoryItemManager;

import io.vertx.core.Future;

public class ProcessInventoryElectronicStrategy extends ProcessInventoryStrategy {
  private static final Logger logger = LogManager.getLogger();

  public ProcessInventoryElectronicStrategy(ConsortiumConfigurationService consortiumConfigurationService) {
    super(consortiumConfigurationService);
  }

  public Future<List<Piece>> handleHoldingsAndItemsRecords(CompositePurchaseOrder compPO,
                                                           PoLine poLine,
                                                           InventoryItemManager inventoryItemManager,
                                                           InventoryHoldingManager inventoryHoldingManager,
                                                           RestClient restClient,
                                                           RequestContext requestContext) {
    logger.debug("ProcessInventoryElectronicStrategy.handleHoldingsAndItemsRecords");
    List<Future<List<Piece>>> itemsPerHolding = new ArrayList<>();

    // Group all locations by location id because the holding should be unique for different locations
    if (PoLineCommonUtil.isHoldingUpdateRequiredForEresource(poLine)) {
      itemsPerHolding = updateHolding(compPO, poLine, inventoryItemManager, inventoryHoldingManager, restClient, requestContext);
    }
    return collectResultsOnSuccess(itemsPerHolding)
      .map(itemCreated -> itemCreated.stream()
        .flatMap(List::stream)
        .collect(toList())
      );
  }
}
