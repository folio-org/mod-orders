package org.folio.service.orders.flows.update.open;

import static java.util.concurrent.CompletableFuture.completedFuture;
import static java.util.stream.Collectors.toList;
import static org.folio.orders.utils.HelperUtils.collectResultsOnSuccess;

import java.util.ArrayList;
import java.util.Collections;
import java.util.List;
import java.util.Map;
import java.util.Optional;
import java.util.concurrent.CompletableFuture;

import org.apache.logging.log4j.LogManager;
import org.apache.logging.log4j.Logger;
import org.folio.completablefuture.FolioVertxCompletableFuture;
import org.folio.orders.utils.PoLineCommonUtil;
import org.folio.rest.core.models.RequestContext;
import org.folio.rest.jaxrs.model.CompositePoLine;
import org.folio.rest.jaxrs.model.CompositePurchaseOrder;
import org.folio.rest.jaxrs.model.Location;
import org.folio.rest.jaxrs.model.Piece;
import org.folio.rest.jaxrs.model.Title;
import org.folio.service.inventory.InventoryManager;
import org.folio.service.pieces.PieceStorageService;
import org.folio.service.titles.TitlesService;

public class OpenCompositeOrderInventoryService {
  private static final Logger logger = LogManager.getLogger(OpenCompositeOrderInventoryService.class);

  private final TitlesService titlesService;
  private final InventoryManager inventoryManager;
  private final PieceStorageService pieceStorageService;
  private final OpenCompositeOrderPieceService openCompositeOrderPieceService;

  public OpenCompositeOrderInventoryService(TitlesService titlesService, InventoryManager inventoryManager,
    PieceStorageService pieceStorageService, OpenCompositeOrderPieceService openCompositeOrderPieceService) {
    this.titlesService = titlesService;
    this.inventoryManager = inventoryManager;
    this.pieceStorageService = pieceStorageService;
    this.openCompositeOrderPieceService = openCompositeOrderPieceService;
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

  /**
   * Creates Inventory records associated with given PO line and updates PO line with corresponding links.
   *
   * @param compPOL Composite PO line to update Inventory for
   * @return CompletableFuture with void.
   */
  public CompletableFuture<Void> processInventory(CompositePoLine compPOL, String titleId,
                                                  boolean isInstanceMatchingDisabled, RequestContext requestContext) {
    if (Boolean.TRUE.equals(compPOL.getIsPackage())) {
      return completedFuture(null);
    }

    if (PoLineCommonUtil.isInventoryUpdateNotRequired(compPOL)) {
      return handlePieces(compPOL, titleId, Collections.emptyList(), isInstanceMatchingDisabled, requestContext);
    }

    return inventoryManager.openOrderHandleInstance(compPOL, isInstanceMatchingDisabled, requestContext)
      .thenCompose(compPOLWithInstanceId -> handleHoldingsAndItemsRecords(compPOLWithInstanceId, requestContext))
      .thenCompose(piecesWithItemId -> handlePieces(compPOL, titleId, piecesWithItemId, isInstanceMatchingDisabled, requestContext));
  }

  /**
   * Returns list of pieces with populated item and location id's corresponding to given PO line.
   * Items are either retrieved from Inventory or new ones are created if no corresponding item records exist yet.
   *
   * @param compPOL   PO line to retrieve/create Item Records for. At this step PO Line must contain instance Id
   * @return future with list of pieces with item and location id's
   */
  public CompletableFuture<List<Piece>> handleHoldingsAndItemsRecords(CompositePoLine compPOL, RequestContext requestContext) {
    List<CompletableFuture<List<Piece>>> itemsPerHolding = new ArrayList<>();

    // Group all locations by location id because the holding should be unique for different locations
    if (PoLineCommonUtil.isHoldingsUpdateRequired(compPOL)) {
      compPOL.getLocations().forEach(location -> {
        itemsPerHolding.add(
          // Search for or create a new holdings record and then create items for it if required
          inventoryManager.getOrCreateHoldingsRecord(compPOL.getInstanceId(), location, requestContext)
            .thenCompose(holdingId -> {
                // Items are not going to be created when create inventory is "Instance, Holding"
                exchangeLocationIdWithHoldingId(location, holdingId);
                if (PoLineCommonUtil.isItemsUpdateRequired(compPOL)) {
                  return inventoryManager.handleItemRecords(compPOL, location, requestContext);
                } else {
                  return completedFuture(Collections.emptyList());
                }
              }
            ));
      });
    }
    return collectResultsOnSuccess(itemsPerHolding)
      .thenApply(itemCreated -> itemCreated.stream()
        .flatMap(List::stream)
        .collect(toList())
      );
  }

  private CompletableFuture<Void> handlePieces(CompositePoLine compPOL, String titleId, List<Piece> piecesWithItemId,
                                               boolean isInstanceMatchingDisabled, RequestContext requestContext) {
    // don't create pieces, if no inventory updates and receiving not required
    if (PoLineCommonUtil.isReceiptNotRequired(compPOL.getReceiptStatus())) {
      return completedFuture(null);
    }
    // do not create pieces in case of check-in flow
    if (compPOL.getCheckinItems() != null && compPOL.getCheckinItems()) {
      return completedFuture(null);
    }
    return openCompositeOrderPieceService.handlePieces(compPOL, titleId, piecesWithItemId, isInstanceMatchingDisabled, requestContext).thenRun(
      () -> logger.info("Create pieces for PO Line with '{}' id where inventory updates are not required", compPOL.getId()));
  }

  private void exchangeLocationIdWithHoldingId(Location location, String holdingId) {
    addHoldingId(List.of(location), holdingId);
    location.setLocationId(null);
  }

  private void addHoldingId(List<Location> polLocations, String holdingId) {
    polLocations.forEach(location -> location.setHoldingId(holdingId));
  }

  private String getFirstTitleIdIfExist(Map<String, List<Title>> lineIdsTitles, CompositePoLine poLine) {
    return Optional.ofNullable(lineIdsTitles.get(poLine.getId()))
      .map(titles -> titles.get(0))
      .map(Title::getId)
      .orElse(null);
  }
}
