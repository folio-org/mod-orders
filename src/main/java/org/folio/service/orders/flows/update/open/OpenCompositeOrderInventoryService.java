package org.folio.service.orders.flows.update.open;

import java.util.ArrayList;
import java.util.List;
import java.util.Map;
import java.util.Optional;

import org.apache.logging.log4j.LogManager;
import org.apache.logging.log4j.Logger;
import org.folio.okapi.common.GenericCompositeFuture;
import org.folio.rest.core.models.RequestContext;
import org.folio.rest.jaxrs.model.CompositePoLine;
import org.folio.rest.jaxrs.model.CompositePurchaseOrder;
import org.folio.rest.jaxrs.model.Title;
import org.folio.service.inventory.InventoryManager;
import org.folio.service.pieces.flows.strategies.ProcessInventoryStrategyResolver;

import io.vertx.core.Future;

public class OpenCompositeOrderInventoryService {
  private static final Logger logger = LogManager.getLogger(OpenCompositeOrderInventoryService.class);

  private final InventoryManager inventoryManager;
  private final OpenCompositeOrderPieceService openCompositeOrderPieceService;
  private final ProcessInventoryStrategyResolver processInventoryStrategyResolver;

  public OpenCompositeOrderInventoryService(InventoryManager inventoryManager,
                                            OpenCompositeOrderPieceService openCompositeOrderPieceService,
                                            ProcessInventoryStrategyResolver processInventoryStrategyResolver) {
    this.inventoryManager = inventoryManager;
    this.openCompositeOrderPieceService = openCompositeOrderPieceService;
    this.processInventoryStrategyResolver = processInventoryStrategyResolver;
  }

  public Future<Void> processInventory(Map<String, List<Title>> lineIdsTitles, CompositePurchaseOrder compPO,
      boolean isInstanceMatchingDisabled, RequestContext requestContext) {
    List<Future<Void>> futures = new ArrayList<>();
    Future<Void> future = Future.succeededFuture();
    for (int idx = 0; idx < compPO.getCompositePoLines().size(); idx++) {
      var poLine = compPO.getCompositePoLines().get(idx);
      if (idx % 2 == 0){
        // TODO: fix future processing
        future = future.compose(v -> processInventory(poLine, getFirstTitleIdIfExist(lineIdsTitles, poLine), isInstanceMatchingDisabled, requestContext));
        futures.add(future);
      } else {
        futures.add(processInventory(poLine, getFirstTitleIdIfExist(lineIdsTitles, poLine), isInstanceMatchingDisabled, requestContext));
      }
    }
    return GenericCompositeFuture.join(new ArrayList<>(futures))
      .mapEmpty();
  }

  public Future<Void> processInventory(CompositePoLine compPOL, String titleId, boolean isInstanceMatchingDisabled,
      RequestContext requestContext) {

    if (logger.isDebugEnabled()) {
      logger.debug("Executing a strategy for: {}", compPOL.getOrderFormat().value());
    }
    return processInventoryStrategyResolver.getHoldingAndItemStrategy(compPOL.getOrderFormat()
      .value())
      .processInventory(compPOL, titleId, isInstanceMatchingDisabled, inventoryManager, openCompositeOrderPieceService, requestContext);
  }

  private String getFirstTitleIdIfExist(Map<String, List<Title>> lineIdsTitles, CompositePoLine poLine) {
    return Optional.ofNullable(lineIdsTitles.get(poLine.getId()))
      .map(titles -> titles.get(0))
      .map(Title::getId)
      .orElse(null);
  }
}
