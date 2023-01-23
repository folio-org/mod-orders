package org.folio.service.orders.flows.update.open;

import static org.folio.rest.RestConstants.SEMAPHORE_MAX_ACTIVE_THREADS;

import java.util.ArrayList;
import java.util.List;
import java.util.Map;
import java.util.Optional;

import org.apache.logging.log4j.LogManager;
import org.apache.logging.log4j.Logger;
import org.folio.okapi.common.GenericCompositeFuture;
import org.folio.rest.core.RestClient;
import org.folio.rest.core.models.RequestContext;
import org.folio.rest.jaxrs.model.CompositePoLine;
import org.folio.rest.jaxrs.model.CompositePurchaseOrder;
import org.folio.rest.jaxrs.model.Title;
import org.folio.service.inventory.InventoryManager;
import org.folio.service.pieces.flows.strategies.ProcessInventoryStrategyResolver;

import io.vertx.core.Future;
import io.vertxconcurrent.Semaphore;

public class OpenCompositeOrderInventoryService {
  private static final Logger logger = LogManager.getLogger(OpenCompositeOrderInventoryService.class);

  private final InventoryManager inventoryManager;
  private final OpenCompositeOrderPieceService openCompositeOrderPieceService;
  private final ProcessInventoryStrategyResolver processInventoryStrategyResolver;
  private final RestClient restClient;

  public OpenCompositeOrderInventoryService(InventoryManager inventoryManager,
                                            OpenCompositeOrderPieceService openCompositeOrderPieceService,
                                            ProcessInventoryStrategyResolver processInventoryStrategyResolver,
                                            RestClient restClient) {
    this.inventoryManager = inventoryManager;
    this.openCompositeOrderPieceService = openCompositeOrderPieceService;
    this.processInventoryStrategyResolver = processInventoryStrategyResolver;
    this.restClient = restClient;
  }

  public Future<Void> processInventory(Map<String, List<Title>> lineIdsTitles, CompositePurchaseOrder compPO,
      boolean isInstanceMatchingDisabled, RequestContext requestContext) {
    Semaphore semaphore = new Semaphore(SEMAPHORE_MAX_ACTIVE_THREADS, requestContext.getContext().owner());
    return requestContext.getContext().owner()
      .<List<Future<Void>>>executeBlocking(event -> {
        List<Future<Void>> futures = new ArrayList<>();
        for (CompositePoLine poLine : compPO.getCompositePoLines()) {
          Future<Void> future = processInventory(poLine, getFirstTitleIdIfExist(lineIdsTitles, poLine),isInstanceMatchingDisabled, requestContext);
          futures.add(future);
          semaphore.acquire(() -> future
            .onComplete(asyncResult -> semaphore.release()));
        }
        event.complete(futures);
      })
      .compose(futures -> GenericCompositeFuture.all(new ArrayList<>(futures))
        .mapEmpty());
  }

  public Future<Void> processInventory(CompositePoLine compPOL, String titleId, boolean isInstanceMatchingDisabled,
      RequestContext requestContext) {

    if (logger.isDebugEnabled()) {
      logger.debug("Executing a strategy for: {}", compPOL.getOrderFormat().value());
    }
    return processInventoryStrategyResolver.getHoldingAndItemStrategy(compPOL.getOrderFormat().value())
      .processInventory(compPOL, titleId, isInstanceMatchingDisabled, inventoryManager, openCompositeOrderPieceService, restClient, requestContext);
  }

  private String getFirstTitleIdIfExist(Map<String, List<Title>> lineIdsTitles, CompositePoLine poLine) {
    return Optional.ofNullable(lineIdsTitles.get(poLine.getId()))
      .map(titles -> titles.get(0))
      .map(Title::getId)
      .orElse(null);
  }
}
