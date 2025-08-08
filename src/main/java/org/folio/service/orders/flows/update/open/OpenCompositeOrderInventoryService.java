package org.folio.service.orders.flows.update.open;

import static org.folio.rest.RestConstants.SEMAPHORE_MAX_ACTIVE_THREADS;

import java.util.List;
import java.util.Map;
import java.util.Optional;

import org.apache.logging.log4j.LogManager;
import org.apache.logging.log4j.Logger;
import org.folio.orders.utils.HelperUtils;
import org.folio.rest.core.RestClient;
import org.folio.rest.core.models.RequestContext;
import org.folio.rest.jaxrs.model.PoLine;
import org.folio.rest.jaxrs.model.CompositePurchaseOrder;
import org.folio.rest.jaxrs.model.Title;
import org.folio.service.inventory.InventoryHoldingManager;
import org.folio.service.inventory.InventoryInstanceManager;
import org.folio.service.inventory.InventoryItemManager;
import org.folio.service.pieces.flows.strategies.ProcessInventoryStrategyResolver;

import io.vertx.core.Future;

public class OpenCompositeOrderInventoryService {
  private static final Logger logger = LogManager.getLogger(OpenCompositeOrderInventoryService.class);

  private final InventoryItemManager inventoryItemManager;
  private final InventoryHoldingManager inventoryHoldingManager;
  private final InventoryInstanceManager inventoryInstanceManager;
  private final OpenCompositeOrderPieceService openCompositeOrderPieceService;
  private final ProcessInventoryStrategyResolver processInventoryStrategyResolver;
  private final RestClient restClient;

  public OpenCompositeOrderInventoryService(InventoryItemManager inventoryItemManager,
                                            InventoryHoldingManager inventoryHoldingManager,
                                            InventoryInstanceManager inventoryInstanceManager,
                                            OpenCompositeOrderPieceService openCompositeOrderPieceService,
                                            ProcessInventoryStrategyResolver processInventoryStrategyResolver,
                                            RestClient restClient) {
    this.inventoryItemManager = inventoryItemManager;
    this.inventoryHoldingManager = inventoryHoldingManager;
    this.inventoryInstanceManager = inventoryInstanceManager;
    this.openCompositeOrderPieceService = openCompositeOrderPieceService;
    this.processInventoryStrategyResolver = processInventoryStrategyResolver;
    this.restClient = restClient;
  }

  public Future<Void> processInventory(Map<String, List<Title>> lineIdsTitles, CompositePurchaseOrder compPO,
      boolean isInstanceMatchingDisabled, RequestContext requestContext) {
    logger.debug("OpenCompositeOrderInventoryService.processInventory compPO.id={}", compPO.getId());
    return HelperUtils.executeWithSemaphores(requestContext.getContext(), SEMAPHORE_MAX_ACTIVE_THREADS, compPO.getPoLines(),
        poLine -> processInventory(compPO, poLine, getFirstTitleIdIfExist(lineIdsTitles, poLine), isInstanceMatchingDisabled, requestContext))
      .mapEmpty();
  }

  public Future<Void> processInventory(CompositePurchaseOrder compPO, PoLine poLine, String titleId,
                                       boolean isInstanceMatchingDisabled, RequestContext requestContext) {
    logger.debug("processInventory:: Executing a strategy for: {}", poLine.getOrderFormat().value());
    return processInventoryStrategyResolver
      .getHoldingAndItemStrategy(poLine.getOrderFormat().value())
      .processInventory(compPO, poLine, titleId, isInstanceMatchingDisabled,
        inventoryItemManager, inventoryHoldingManager, inventoryInstanceManager, openCompositeOrderPieceService, restClient, requestContext);
  }

  private String getFirstTitleIdIfExist(Map<String, List<Title>> lineIdsTitles, PoLine poLine) {
    return Optional.ofNullable(lineIdsTitles.get(poLine.getId()))
      .map(titles -> titles.get(0))
      .map(Title::getId)
      .orElse(null);
  }
}
