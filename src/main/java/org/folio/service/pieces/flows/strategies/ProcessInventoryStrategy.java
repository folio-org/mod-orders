package org.folio.service.pieces.flows.strategies;

import io.vertx.core.Future;
import one.util.streamex.StreamEx;
import org.apache.commons.lang3.ObjectUtils;
import org.apache.commons.lang3.StringUtils;
import org.apache.logging.log4j.LogManager;
import org.apache.logging.log4j.Logger;
import org.folio.orders.utils.PoLineCommonUtil;
import org.folio.orders.utils.StreamUtils;
import org.folio.rest.core.RestClient;
import org.folio.rest.core.models.RequestContext;
import org.folio.rest.core.models.RequestEntry;
import org.folio.rest.jaxrs.model.CompositePurchaseOrder;
import org.folio.rest.jaxrs.model.Location;
import org.folio.rest.jaxrs.model.Piece;
import org.folio.rest.jaxrs.model.PoLine;
import org.folio.service.consortium.ConsortiumConfigurationService;
import org.folio.service.inventory.InventoryHoldingManager;
import org.folio.service.inventory.InventoryInstanceManager;
import org.folio.service.inventory.InventoryItemManager;
import org.folio.service.orders.flows.update.open.OpenCompositeOrderPieceService;

import java.util.Collections;
import java.util.List;
import java.util.Optional;
import java.util.Set;

import static org.folio.orders.utils.FutureUtils.asFuture;
import static org.folio.orders.utils.HelperUtils.chainCallAccumulated;
import static org.folio.service.inventory.InventoryHoldingManager.HOLDINGS_LOOKUP_QUERY;
import static org.folio.service.inventory.InventoryItemManager.ID;
import static org.folio.service.inventory.InventoryUtils.HOLDINGS_RECORDS;
import static org.folio.service.inventory.InventoryUtils.INVENTORY_LOOKUP_ENDPOINTS;

public abstract class ProcessInventoryStrategy {

  private static final Logger logger = LogManager.getLogger(ProcessInventoryStrategy.class);

  protected final ConsortiumConfigurationService consortiumConfigurationService;

  protected ProcessInventoryStrategy(ConsortiumConfigurationService consortiumConfigurationService) {
    this.consortiumConfigurationService = consortiumConfigurationService;
  }

  /**
   * Returns list of pieces with populated item and location id's corresponding to given PO line.
   * Items are either retrieved from Inventory or new ones are created if no corresponding item records exist yet.
   *
   * @param poLine PO line to retrieve/create Item Records for. At this step PO Line must contain instance Id
   * @return future with list of pieces with item and location id's
   */
  protected abstract Future<List<Piece>> handleHoldingsAndItemsRecords(CompositePurchaseOrder comPO,
                                                                       PoLine poLine,
                                                                       InventoryItemManager inventoryItemManager,
                                                                       InventoryHoldingManager inventoryHoldingManager,
                                                                       RestClient restClient,
                                                                       RequestContext requestContext);

  /**
   * Creates Inventory records associated with given PO line and updates PO line with corresponding links.
   *
   * @param poLine PO line to update Inventory for
   * @param createdInstanceIds Set to track newly created instance IDs
   * @return CompletableFuture with void.
   */
  public Future<Void> processInventory(CompositePurchaseOrder compPO,
                                       PoLine poLine, String titleId,
                                       boolean isInstanceMatchingDisabled,
                                       Set<String> createdInstanceIds,
                                       InventoryItemManager inventoryItemManager,
                                       InventoryHoldingManager inventoryHoldingManager,
                                       InventoryInstanceManager inventoryInstanceManager,
                                       OpenCompositeOrderPieceService openCompositeOrderPieceService,
                                       RestClient restClient,
                                       RequestContext requestContext) {
    logger.debug("ProcessInventoryStrategy.processInventory");
    if (Boolean.TRUE.equals(poLine.getIsPackage())) {
      return Future.succeededFuture();
    }

    if (PoLineCommonUtil.isInventoryUpdateNotRequired(poLine)) {
      logger.debug("processInventory:: inventory update not required");
      return handlePieces(poLine, titleId, Collections.emptyList(), isInstanceMatchingDisabled, requestContext,
        openCompositeOrderPieceService);
    }

    return inventoryInstanceManager.openOrderHandleInstance(poLine, isInstanceMatchingDisabled, createdInstanceIds, requestContext)
      .compose(polWithInstanceId -> handleHoldingsAndItemsRecords(compPO, polWithInstanceId,
        inventoryItemManager, inventoryHoldingManager, restClient, requestContext))
      .compose(piecesWithItemId -> handlePieces(poLine, titleId, piecesWithItemId, isInstanceMatchingDisabled,
        requestContext, openCompositeOrderPieceService));
  }

  protected List<Future<List<Piece>>> updateHolding(CompositePurchaseOrder compPO, PoLine poLine,
                                                    InventoryItemManager inventoryItemManager, InventoryHoldingManager inventoryHoldingManager,
                                                    RestClient restClient, RequestContext requestContext) {
    logger.debug("ProcessInventoryStrategy.updateHolding");
    return StreamEx.of(poLine.getLocations())
      .groupingBy(location -> Optional.ofNullable(location.getTenantId()))
      .values().stream()
      .map(locations -> chainCallAccumulated(locations, location ->
          updateHolding(compPO, poLine, location, inventoryItemManager, inventoryHoldingManager, restClient, requestContext))
        .map(StreamUtils::flatMap))
      .toList();
  }

  private Future<List<Piece>> updateHolding(CompositePurchaseOrder compPO, PoLine poLine, Location location,
                                              InventoryItemManager inventoryItemManager, InventoryHoldingManager inventoryHoldingManager,
                                              RestClient restClient, RequestContext requestContext) {
    return findHoldingsId(poLine, location, restClient, requestContext)
      // Search for or create a new holdings record and then create items for it if required
      .compose(v -> consortiumConfigurationService.cloneRequestContextIfNeeded(requestContext, location))
      .compose(updatedRequestContext -> inventoryHoldingManager.getOrCreateHoldingsRecord(poLine.getInstanceId(), location, updatedRequestContext))
      // Items are not going to be created when create inventory is "Instance, Holding"
      .compose(holdingId -> asFuture(() -> exchangeLocationIdWithHoldingId(location, holdingId)))
      .compose(v -> PoLineCommonUtil.isItemsUpdateRequired(poLine)
        ? inventoryItemManager.handleItemRecords(compPO, poLine, location, requestContext)
        : Future.succeededFuture(Collections.emptyList()));
  }

  protected Future<Void> findHoldingsId(PoLine poLine, Location location, RestClient restClient, RequestContext requestContext) {
    if (ObjectUtils.notEqual(PoLine.Source.USER, poLine.getSource()) &&
        StringUtils.isNotBlank(poLine.getInstanceId()) &&
        StringUtils.isNotBlank(location.getLocationId()) &&
        StringUtils.isBlank(location.getHoldingId())) {

      String query = String.format(HOLDINGS_LOOKUP_QUERY, poLine.getInstanceId(), location.getLocationId());
      RequestEntry requestEntry = new RequestEntry(INVENTORY_LOOKUP_ENDPOINTS.get(HOLDINGS_RECORDS))
        .withQuery(query).withOffset(0).withLimit(1);
      return consortiumConfigurationService.cloneRequestContextIfNeeded(requestContext, location)
        .compose(updatedRequestContext -> restClient.getAsJsonObject(requestEntry, updatedRequestContext))
        .compose(holdings -> {
          if (!holdings.getJsonArray(HOLDINGS_RECORDS).isEmpty()) {
            String holdingId = holdings.getJsonArray(HOLDINGS_RECORDS).getJsonObject(0).getString(ID);
            location.setHoldingId(holdingId);
          }
          return Future.succeededFuture();
        });
    }
    return Future.succeededFuture();
  }

  private void exchangeLocationIdWithHoldingId(Location location, String holdingId) {
    addHoldingId(List.of(location), holdingId);
    location.setLocationId(null);
  }

  private void addHoldingId(List<Location> polLocations, String holdingId) {
    polLocations.forEach(location -> location.setHoldingId(holdingId));
  }

  private Future<Void> handlePieces(PoLine poLine, String titleId, List<Piece> piecesWithItemId,
                                    boolean isInstanceMatchingDisabled, RequestContext requestContext,
                                    OpenCompositeOrderPieceService openCompositeOrderPieceService) {
    logger.debug("ProcessInventoryStrategy.handlePieces poLine.id={}", poLine.getId());
    // don't create pieces, if no inventory updates and receiving not required
    if (PoLineCommonUtil.isReceiptNotRequired(poLine.getReceiptStatus())) {
      return Future.succeededFuture();
    }
    // do not create pieces in case of check-in flow
    if (poLine.getCheckinItems() != null && poLine.getCheckinItems()) {
      return Future.succeededFuture();
    }
    return openCompositeOrderPieceService.handlePieces(poLine, titleId, piecesWithItemId, isInstanceMatchingDisabled, requestContext)
      .onSuccess(v -> logger.info("Create pieces for PO Line with '{}' id where inventory updates are not required", poLine.getId()))
      .mapEmpty();
  }

}
