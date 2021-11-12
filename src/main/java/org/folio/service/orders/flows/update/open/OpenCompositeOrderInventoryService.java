package org.folio.service.orders.flows.update.open;

import org.apache.logging.log4j.LogManager;
import org.apache.logging.log4j.Logger;
import org.folio.completablefuture.FolioVertxCompletableFuture;
import org.folio.rest.core.models.RequestContext;
import org.folio.rest.jaxrs.model.CompositePoLine;
import org.folio.rest.jaxrs.model.CompositePurchaseOrder;
import org.folio.rest.jaxrs.model.Title;
import org.folio.service.inventory.InventoryManager;
import org.folio.service.pieces.PieceStorageService;
import org.folio.service.pieces.flows.strategies.ProcessInventoryStrategyResolver;
import org.folio.service.titles.TitlesService;

import java.util.List;
import java.util.Map;
import java.util.Optional;
import java.util.concurrent.CompletableFuture;

public class OpenCompositeOrderInventoryService {
  private static final Logger logger = LogManager.getLogger(OpenCompositeOrderInventoryService.class);

  private final TitlesService titlesService;
  private final InventoryManager inventoryManager;
  private final PieceStorageService pieceStorageService;
  private final OpenCompositeOrderPieceService openCompositeOrderPieceService;
  private final ProcessInventoryStrategyResolver processInventoryStrategyResolver;

  public OpenCompositeOrderInventoryService(TitlesService titlesService, InventoryManager inventoryManager,
                                            PieceStorageService pieceStorageService, OpenCompositeOrderPieceService openCompositeOrderPieceService, ProcessInventoryStrategyResolver processInventoryStrategyResolver) {
    this.titlesService = titlesService;
    this.inventoryManager = inventoryManager;
    this.pieceStorageService = pieceStorageService;
    this.openCompositeOrderPieceService = openCompositeOrderPieceService;
    this.processInventoryStrategyResolver = processInventoryStrategyResolver;
  }

  public CompletableFuture<Void> processInventory(Map<String, List<Title>> lineIdsTitles, CompositePurchaseOrder compPO,
                                                  boolean isInstanceMatchingDisabled, RequestContext requestContext) {
    return FolioVertxCompletableFuture.allOf(requestContext.getContext(),
      compPO.getCompositePoLines()
        .stream()
        .map(poLine -> processInventory(poLine, getFirstTitleIdIfExist(lineIdsTitles, poLine), isInstanceMatchingDisabled, requestContext))
        .toArray(CompletableFuture[]::new)
    );
  }

  public CompletableFuture<Void> processInventory(CompositePoLine compPOL, String titleId,
                                                  boolean isInstanceMatchingDisabled, RequestContext requestContext) {

    return processInventoryStrategyResolver.getHoldingAndItemStrategy(compPOL.getOrderFormat().value())
      .processInventory(compPOL, titleId, isInstanceMatchingDisabled, requestContext);
  }

  private String getFirstTitleIdIfExist(Map<String, List<Title>> lineIdsTitles, CompositePoLine poLine) {
    return Optional.ofNullable(lineIdsTitles.get(poLine.getId()))
      .map(titles -> titles.get(0))
      .map(Title::getId)
      .orElse(null);
  }
}
