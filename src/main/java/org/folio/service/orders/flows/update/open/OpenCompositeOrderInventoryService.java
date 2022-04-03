package org.folio.service.orders.flows.update.open;

import java.util.ArrayList;
import java.util.List;
import java.util.Map;
import java.util.Optional;
import java.util.concurrent.CompletableFuture;

import org.apache.logging.log4j.LogManager;
import org.apache.logging.log4j.Logger;
import org.folio.completablefuture.FolioVertxCompletableFuture;
import org.folio.rest.core.models.RequestContext;
import org.folio.rest.jaxrs.model.CompositePoLine;
import org.folio.rest.jaxrs.model.CompositePurchaseOrder;
import org.folio.rest.jaxrs.model.Title;
import org.folio.service.inventory.InventoryManager;
import org.folio.service.pieces.flows.strategies.ProcessInventoryStrategyResolver;

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

  public CompletableFuture<Void> processInventory(Map<String, List<Title>> lineIdsTitles, CompositePurchaseOrder compPO,
      boolean isInstanceMatchingDisabled, RequestContext requestContext) {
    List<CompletableFuture<Void>> futures = new ArrayList<>();
    CompletableFuture<Void> future = CompletableFuture.completedFuture(null);
    for (int idx = 0; idx < compPO.getCompositePoLines().size(); idx++) {
      var poLine = compPO.getCompositePoLines().get(idx);
      if (idx % 2 == 0){
        future = future.thenCompose(v -> processInventory(poLine, getFirstTitleIdIfExist(lineIdsTitles, poLine), isInstanceMatchingDisabled, requestContext));
        futures.add(future);
      } else {
        futures.add(processInventory(poLine, getFirstTitleIdIfExist(lineIdsTitles, poLine), isInstanceMatchingDisabled, requestContext));
      }
    }
    return FolioVertxCompletableFuture.allOf(requestContext.getContext(), futures.toArray(new CompletableFuture[0]));
  }

  public CompletableFuture<Void> processInventory(CompositePoLine compPOL, String titleId,
                                                  boolean isInstanceMatchingDisabled, RequestContext requestContext) {

    logger.debug("Executing a strategy for: " + compPOL.getOrderFormat().value());
    return processInventoryStrategyResolver.getHoldingAndItemStrategy(compPOL.getOrderFormat().value())
      .processInventory(compPOL, titleId, isInstanceMatchingDisabled,
        inventoryManager, openCompositeOrderPieceService, requestContext);
  }

  private String getFirstTitleIdIfExist(Map<String, List<Title>> lineIdsTitles, CompositePoLine poLine) {
    return Optional.ofNullable(lineIdsTitles.get(poLine.getId()))
      .map(titles -> titles.get(0))
      .map(Title::getId)
      .orElse(null);
  }
}
