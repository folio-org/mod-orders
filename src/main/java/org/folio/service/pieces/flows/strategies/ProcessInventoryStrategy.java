package org.folio.service.pieces.flows.strategies;

import java.util.ArrayList;
import java.util.Collections;
import java.util.List;

import org.apache.commons.lang3.ObjectUtils;
import org.apache.commons.lang3.StringUtils;
import org.apache.logging.log4j.LogManager;
import org.apache.logging.log4j.Logger;
import org.folio.orders.utils.PoLineCommonUtil;
import org.folio.rest.core.RestClient;
import org.folio.rest.core.models.RequestContext;
import org.folio.rest.core.models.RequestEntry;
import org.folio.rest.jaxrs.model.CompositePoLine;
import org.folio.rest.jaxrs.model.Location;
import org.folio.rest.jaxrs.model.Piece;
import org.folio.service.consortium.ConsortiumConfigurationService;
import org.folio.service.inventory.InventoryHoldingManager;
import org.folio.service.inventory.InventoryInstanceManager;
import org.folio.service.inventory.InventoryItemManager;
import org.folio.service.orders.flows.update.open.OpenCompositeOrderPieceService;

import io.vertx.core.Future;

import static org.folio.orders.utils.RequestContextUtil.cloneRequestContextBasedOnLocation;
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
   * @param compPOL PO line to retrieve/create Item Records for. At this step PO Line must contain instance Id
   * @return future with list of pieces with item and location id's
   */
  protected abstract Future<List<Piece>> handleHoldingsAndItemsRecords(CompositePoLine compPOL,
                                                                       InventoryItemManager inventoryItemManager,
                                                                       InventoryHoldingManager inventoryHoldingManager,
                                                                       RestClient restClient,
                                                                       RequestContext requestContext);

  /**
   * Creates Inventory records associated with given PO line and updates PO line with corresponding links.
   *
   * @param compPOL Composite PO line to update Inventory for
   * @return CompletableFuture with void.
   */
  public Future<Void> processInventory(CompositePoLine compPOL, String titleId,
                                       boolean isInstanceMatchingDisabled,
                                       InventoryItemManager inventoryItemManager,
                                       InventoryHoldingManager inventoryHoldingManager,
                                       InventoryInstanceManager inventoryInstanceManager,
                                       OpenCompositeOrderPieceService openCompositeOrderPieceService,
                                       RestClient restClient,
                                       RequestContext requestContext) {
    if (Boolean.TRUE.equals(compPOL.getIsPackage())) {
      return Future.succeededFuture();
    }

    if (PoLineCommonUtil.isInventoryUpdateNotRequired(compPOL)) {
      return handlePieces(compPOL, titleId, Collections.emptyList(), isInstanceMatchingDisabled, requestContext,
        openCompositeOrderPieceService);
    }

    return inventoryInstanceManager.openOrderHandleInstance(compPOL, isInstanceMatchingDisabled, requestContext)
      .compose(compPOLWithInstanceId -> handleHoldingsAndItemsRecords(
        compPOLWithInstanceId, inventoryItemManager, inventoryHoldingManager, restClient, requestContext))
      .compose(piecesWithItemId -> handlePieces(compPOL, titleId, piecesWithItemId, isInstanceMatchingDisabled,
        requestContext, openCompositeOrderPieceService));
  }

  protected List<Future<List<Piece>>> updateHolding(CompositePoLine compPOL,
                                                    InventoryItemManager inventoryItemManager,
                                                    InventoryHoldingManager inventoryHoldingManager,
                                                    RestClient restClient,
                                                    RequestContext requestContext) {
    List<Future<List<Piece>>> itemsPerHolding = new ArrayList<>();
    compPOL.getLocations().forEach(location -> itemsPerHolding.add(
      findHoldingsId(compPOL, location, restClient, requestContext)
        .compose(aVoid -> {
          // Search for or create a new holdings record and then create items for it if required
          return consortiumConfigurationService.getConsortiumConfiguration(requestContext)
            .map(optionalConfiguration -> optionalConfiguration.map(configuration ->
              cloneRequestContextBasedOnLocation(requestContext, location)).orElse(requestContext))
            .compose(updatedRequestContext -> inventoryHoldingManager.getOrCreateHoldingsRecord(compPOL.getInstanceId(), location, updatedRequestContext))
            .compose(holdingId -> {
              // Items are not going to be created when create inventory is "Instance, Holding"
              exchangeLocationIdWithHoldingId(location, holdingId);
              if (PoLineCommonUtil.isItemsUpdateRequired(compPOL)) {
                return inventoryItemManager.handleItemRecords(compPOL, location, requestContext);
              } else {
                return Future.succeededFuture(Collections.emptyList());
              }
            });
        })
    ));
    return itemsPerHolding;
  }

  protected Future<Void> findHoldingsId(CompositePoLine compPOL, Location location, RestClient restClient, RequestContext requestContext) {
    if (ObjectUtils.notEqual(CompositePoLine.Source.USER, compPOL.getSource()) &&
        StringUtils.isNotBlank(compPOL.getInstanceId()) &&
        StringUtils.isNotBlank(location.getLocationId()) &&
        StringUtils.isBlank(location.getHoldingId())) {

      String query = String.format(HOLDINGS_LOOKUP_QUERY, compPOL.getInstanceId(), location.getLocationId());
      RequestEntry requestEntry = new RequestEntry(INVENTORY_LOOKUP_ENDPOINTS.get(HOLDINGS_RECORDS))
        .withQuery(query).withOffset(0).withLimit(1);
      return consortiumConfigurationService.getConsortiumConfiguration(requestContext)
        .map(optionalConfiguration -> optionalConfiguration.map(configuration ->
          cloneRequestContextBasedOnLocation(requestContext, location)).orElse(requestContext))
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

  private Future<Void> handlePieces(CompositePoLine compPOL, String titleId, List<Piece> piecesWithItemId,
                                                 boolean isInstanceMatchingDisabled, RequestContext requestContext,
                                                 OpenCompositeOrderPieceService openCompositeOrderPieceService) {
    // don't create pieces, if no inventory updates and receiving not required
    if (PoLineCommonUtil.isReceiptNotRequired(compPOL.getReceiptStatus())) {
      return Future.succeededFuture();
    }
    // do not create pieces in case of check-in flow
    if (compPOL.getCheckinItems() != null && compPOL.getCheckinItems()) {
      return Future.succeededFuture();
    }
    return openCompositeOrderPieceService.handlePieces(compPOL, titleId, piecesWithItemId, isInstanceMatchingDisabled, requestContext)
      .onSuccess(v -> logger.info("Create pieces for PO Line with '{}' id where inventory updates are not required", compPOL.getId()))
      .mapEmpty();
  }

}
