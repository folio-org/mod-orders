package org.folio.service.orders.flows.update.open;

import static org.folio.rest.RestConstants.SEMAPHORE_MAX_ACTIVE_THREADS;

import java.util.ArrayList;
import java.util.List;
import java.util.Map;
import java.util.Optional;

import org.apache.commons.collections4.CollectionUtils;
import org.apache.logging.log4j.LogManager;
import org.apache.logging.log4j.Logger;
import org.folio.okapi.common.GenericCompositeFuture;
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
import io.vertxconcurrent.Semaphore;

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
    Semaphore semaphore = new Semaphore(SEMAPHORE_MAX_ACTIVE_THREADS, requestContext.getContext().owner());
    if (CollectionUtils.isEmpty(compPO.getPoLines())) {
      return Future.succeededFuture();
    }
    return requestContext.getContext().owner().<List<Future<Void>>>executeBlocking(event -> {
        List<Future<Void>> futures = new ArrayList<>();
        for (PoLine poLine : compPO.getPoLines()) {
          semaphore.acquire(() -> {
            Future<Void> future = processInventory(compPO, poLine, getFirstTitleIdIfExist(lineIdsTitles, poLine), isInstanceMatchingDisabled, requestContext)
              .onComplete(asyncResult -> semaphore.release());
            futures.add(future);
            if (futures.size() == compPO.getPoLines().size()) {
              event.complete(futures);
            }
          });
        }
      }).compose(futures -> GenericCompositeFuture.all(new ArrayList<>(futures)).mapEmpty());
  }

  public Future<Void> processInventory(CompositePurchaseOrder compPO, PoLine poLine, String titleId,
                                       boolean isInstanceMatchingDisabled, RequestContext requestContext) {
    logger.debug("processInventory:: Executing a strategy for: {}", poLine.getOrderFormat().value());
    return processInventoryStrategyResolver.getHoldingAndItemStrategy(poLine.getOrderFormat().value())
      .processInventory(compPO, poLine, titleId, isInstanceMatchingDisabled, inventoryItemManager, inventoryHoldingManager, inventoryInstanceManager, openCompositeOrderPieceService, restClient, requestContext);
  }

  private String getFirstTitleIdIfExist(Map<String, List<Title>> lineIdsTitles, PoLine poLine) {
    return Optional.ofNullable(lineIdsTitles.get(poLine.getId()))
      .map(List::getFirst)
      .map(Title::getId)
      .orElse(null);
  }
}
