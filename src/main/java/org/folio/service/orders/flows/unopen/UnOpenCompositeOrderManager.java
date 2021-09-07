package org.folio.service.orders.flows.unopen;

import static java.util.concurrent.CompletableFuture.completedFuture;
import static java.util.stream.Collectors.groupingBy;
import static java.util.stream.Collectors.toList;
import static org.apache.commons.collections4.CollectionUtils.isNotEmpty;
import static org.folio.orders.utils.HelperUtils.collectResultsOnSuccess;
import static org.folio.orders.utils.PoLineCommonUtil.isOnlyInstanceUpdateRequired;
import static org.folio.service.inventory.InventoryManager.HOLDING_PERMANENT_LOCATION_ID;
import static org.folio.service.inventory.InventoryManager.ID;
import static org.folio.service.inventory.InventoryManager.ITEM_EFFECTIVE_LOCATION;
import static org.folio.service.inventory.InventoryManager.ITEM_HOLDINGS_RECORD_ID;
import static org.folio.service.inventory.InventoryManager.ITEM_STATUS;
import static org.folio.service.inventory.InventoryManager.ITEM_STATUS_NAME;

import java.util.ArrayList;
import java.util.Collections;
import java.util.List;
import java.util.Map;
import java.util.Objects;
import java.util.concurrent.CompletableFuture;
import java.util.stream.Collectors;

import org.apache.commons.collections4.CollectionUtils;
import org.apache.commons.lang3.tuple.Pair;
import org.apache.logging.log4j.LogManager;
import org.apache.logging.log4j.Logger;
import org.folio.completablefuture.FolioVertxCompletableFuture;
import org.folio.orders.utils.HelperUtils;
import org.folio.orders.utils.PoLineCommonUtil;
import org.folio.rest.core.models.RequestContext;
import org.folio.rest.jaxrs.model.CompositePoLine;
import org.folio.rest.jaxrs.model.CompositePurchaseOrder;
import org.folio.rest.jaxrs.model.Location;
import org.folio.rest.jaxrs.model.Piece;
import org.folio.service.finance.transaction.EncumbranceWorkflowStrategyFactory;
import org.folio.service.inventory.InventoryManager;
import org.folio.models.ItemStatus;
import org.folio.service.orders.OrderWorkflowType;
import org.folio.service.orders.PurchaseOrderLineService;
import org.folio.service.pieces.PieceStorageService;
import org.folio.service.pieces.PieceService;

import io.vertx.core.json.JsonObject;

public class UnOpenCompositeOrderManager {
  private static final Logger logger = LogManager.getLogger(UnOpenCompositeOrderManager.class);

  private final PurchaseOrderLineService purchaseOrderLineService;
  private final EncumbranceWorkflowStrategyFactory encumbranceWorkflowStrategyFactory;
  private final InventoryManager inventoryManager;
  private final PieceService pieceService;
  private final PieceStorageService pieceStorageService;

  public UnOpenCompositeOrderManager(PurchaseOrderLineService purchaseOrderLineService,
                                     EncumbranceWorkflowStrategyFactory encumbranceWorkflowStrategyFactory,
                                     InventoryManager inventoryManager, PieceService pieceService,
                                     PieceStorageService pieceStorageService) {
    this.purchaseOrderLineService = purchaseOrderLineService;
    this.encumbranceWorkflowStrategyFactory = encumbranceWorkflowStrategyFactory;
    this.inventoryManager = inventoryManager;
    this.pieceService = pieceService;
    this.pieceStorageService = pieceStorageService;
  }


  public CompletableFuture<Void> process(CompositePurchaseOrder compPO, CompositePurchaseOrder poFromStorage, RequestContext requestContext) {
    CompletableFuture<Void> future = new CompletableFuture<>();
    updateAndGetOrderWithLines(compPO, requestContext)
      .thenApply(aVoid -> encumbranceWorkflowStrategyFactory.getStrategy(OrderWorkflowType.OPEN_TO_PENDING))
      .thenCompose(strategy -> strategy.processEncumbrances(compPO, poFromStorage, requestContext))
      .thenAccept(ok -> HelperUtils.makePoLinesPending(compPO.getCompositePoLines()))
      .thenCompose(ok -> updatePoLinesSummary(compPO.getCompositePoLines(), requestContext))
      .thenCompose(ok -> processInventory(compPO.getCompositePoLines(), requestContext))
      .thenAccept(v-> future.complete(null))
      .exceptionally(t -> {
        future.completeExceptionally(t);
        return null;
      });
    return future;
  }

  public CompletableFuture<Void> updatePoLinesSummary(List<CompositePoLine> compositePoLines, RequestContext requestContext) {
    return FolioVertxCompletableFuture.allOf(requestContext.getContext(), compositePoLines.stream()
      .map(HelperUtils::convertToPoLine)
      .map(line -> purchaseOrderLineService.updateOrderLine(line, requestContext))
      .toArray(CompletableFuture[]::new));
  }

  private CompletableFuture<Void> processInventory(List<CompositePoLine> compositePoLines, RequestContext requestContext) {
    return FolioVertxCompletableFuture.allOf(requestContext.getContext(), compositePoLines.stream()
      .map(line -> processInventory(line, requestContext))
      .toArray(CompletableFuture[]::new));
  }

  private CompletableFuture<Void> processInventory(CompositePoLine compPOL, RequestContext rqContext) {
    if (Boolean.TRUE.equals(compPOL.getIsPackage())) {
      return completedFuture(null);
    }
    if (PoLineCommonUtil.inventoryUpdateNotRequired(compPOL) || isOnlyInstanceUpdateRequired(compPOL)) {
      return deleteExpectedPieces(compPOL, rqContext).thenAccept(pieces -> {
        if (logger.isDebugEnabled()) {
          String deletedIds = pieces.stream().map(Piece::getId).collect(Collectors.joining(","));
          logger.debug(String.format("Pieces were removed : %s", deletedIds));
        }
      });
    } else if (PoLineCommonUtil.isItemsUpdateRequired(compPOL)) {
      return processInventoryWithItems(compPOL, rqContext);
    } else if (PoLineCommonUtil.isHoldingsUpdateRequired(compPOL.getEresource(), compPOL.getPhysical())) {
      return processInventoryOnlyWithHolding(compPOL, rqContext);
    }
    return completedFuture(null);
  }

  private CompletableFuture<Void> processInventoryOnlyWithHolding(CompositePoLine compPOL, RequestContext rqContext) {
    return deleteExpectedPieces(compPOL, rqContext)
                .thenCompose(deletedPieces -> {
                  List<String> holdingIds = compPOL.getLocations().stream()
                                    .filter(loc -> Objects.nonNull(loc.getHoldingId()))
                                    .map(Location::getHoldingId).collect(toList());
                  return inventoryManager.getHoldingsByIds(holdingIds, rqContext);
                })
                .thenCompose(holdings -> deleteHoldings(holdings, rqContext))
                .thenAccept(deletedHoldingVsLocationIds -> updateLocations(compPOL, deletedHoldingVsLocationIds))
                .thenAccept(v -> logger.debug("Pieces, Holdings deleted after UnOpen order"));
  }

  private CompletableFuture<List<Piece>> deleteExpectedPieces(CompositePoLine compPOL, RequestContext rqContext) {
    if (!PoLineCommonUtil.isReceiptNotRequired(compPOL.getReceiptStatus()) && Boolean.FALSE.equals(compPOL.getCheckinItems())) {
      return pieceStorageService.getExpectedPiecesByLineId(compPOL.getId(), rqContext)
        .thenCompose(pieceCollection -> {
          if (isNotEmpty(pieceCollection.getPieces())) {
            return pieceService.deletePiecesByIds(pieceCollection.getPieces().stream().map(Piece::getId).collect(toList()), rqContext)
                                .thenApply(v -> pieceCollection.getPieces());
          }
          return completedFuture(Collections.emptyList());
        });
    }
    return completedFuture(Collections.emptyList());
  }

  private CompletableFuture<Void> processInventoryWithItems(CompositePoLine compPOL, RequestContext rqContext) {
    return inventoryManager.getItemsByStatus(List.of(compPOL.getId()), ItemStatus.ON_ORDER.value(), rqContext)
                          .thenCompose(onOrderItems -> {
                            if (isNotEmpty(onOrderItems)) {
                              List<String> itemIds = onOrderItems.stream().map(item -> item.getString(ID)).collect(toList());
                              if (PoLineCommonUtil.isReceiptNotRequired(compPOL.getReceiptStatus())) {
                                return inventoryManager.deleteItems(itemIds,  rqContext)
                                              .thenCompose(deletedItemIds -> deleteHoldingsByItems(onOrderItems, rqContext))
                                              .thenAccept(deletedHoldingVsLocationIds -> updateLocations(compPOL, deletedHoldingVsLocationIds))
                                              .thenAccept(v -> logger.debug("Items and holdings deleted after UnOpen order"));
                              }
                              return pieceStorageService.getExpectedPiecesByLineId(compPOL.getId(), rqContext)
                                .thenCompose(pieceCollection -> {
                                  if (isNotEmpty(pieceCollection.getPieces())) {
                                    return inventoryManager.getItemRecordsByIds(itemIds, rqContext)
                                      .thenApply(items -> getItemsByStatus(items, ItemStatus.ON_ORDER.value()))
                                      .thenCompose(onOrderItemsP -> deletePiecesAndItems(onOrderItemsP, pieceCollection.getPieces(), rqContext))
                                      .thenCompose(deletedItems -> deleteHoldingsByItems(deletedItems, rqContext))
                                      .thenAccept(deletedHoldingVsLocationIds -> updateLocations(compPOL, deletedHoldingVsLocationIds))
                                      .thenAccept(v -> logger.debug("Pieces, Items, Holdings deleted after UnOpen order"));
                                  }
                                  return completedFuture(null);
                                });
                            }
                            return completedFuture(null);
                          });
  }

  private void updateLocations(CompositePoLine compPOL, List<Pair<String, String>> deletedHoldingVsLocationIds) {
    if (CollectionUtils.isNotEmpty(deletedHoldingVsLocationIds)) {
      Map<String, List<Location>> holdingIdVsLocations = compPOL.getLocations().stream().collect(groupingBy(Location::getHoldingId));
      deletedHoldingVsLocationIds.forEach(holdingIdVsLocationId -> {
        String holdingId = holdingIdVsLocationId.getKey();
        String locationId = holdingIdVsLocationId.getValue();
        List<Location> locations = holdingIdVsLocations.get(holdingId);
        if (CollectionUtils.isNotEmpty(locations)) {
          locations.forEach(location -> {
            location.setHoldingId(null);
            location.setLocationId(locationId);
          });
        }
      });
    }
  }


  private CompletableFuture<List<Pair<String, String>>> deleteHoldingsByItems(List<JsonObject> deletedItems, RequestContext rqContext) {
    List<CompletableFuture<Pair<String, String>>> deletedHoldingIds = new ArrayList<>(deletedItems.size());
    deletedItems.forEach(deletedItem -> {
      String holdingId = deletedItem.getString(ITEM_HOLDINGS_RECORD_ID);
      String effectiveLocationId = deletedItem.getJsonObject(ITEM_EFFECTIVE_LOCATION).getString(ID);
      if (holdingId != null) {
        deletedHoldingIds.add(inventoryManager.getItemsByHoldingId(holdingId, rqContext)
          .thenCompose(items -> {
            if (items.isEmpty()) {
              return inventoryManager.deleteHolding(holdingId, rqContext).thenApply(v -> Pair.of(holdingId, effectiveLocationId));
            }
            return CompletableFuture.completedFuture(null);
          }));
      }
    });
    return collectResultsOnSuccess(deletedHoldingIds)
      .thenApply(resultDeletedHoldingVsLocationIds -> resultDeletedHoldingVsLocationIds.stream()
                                                        .filter(pair -> Objects.nonNull(pair.getKey())).collect(toList()))
      .thenApply(resultDeletedHoldingVsLocationIds -> {
        if (logger.isDebugEnabled()) {
          String deletedIds = resultDeletedHoldingVsLocationIds.stream().map(Pair::getKey).collect(Collectors.joining(","));
          logger.debug(String.format("Holdings were removed : %s", deletedIds));
        }
        return resultDeletedHoldingVsLocationIds;
      });
  }

  private CompletableFuture<List<Pair<String, String>>> deleteHoldings(List<JsonObject> holdings, RequestContext rqContext) {
    if (CollectionUtils.isNotEmpty(holdings)) {
      List<CompletableFuture<Pair<String, String>>> deletedHoldingIds = new ArrayList<>(holdings.size());
      holdings.forEach(holding -> {
        String holdingId = holding.getString(ID);
        String permanentLocationId = holding.getString(HOLDING_PERMANENT_LOCATION_ID);
        if (holdingId != null) {
          deletedHoldingIds.add(inventoryManager.getItemsByHoldingId(holdingId, rqContext)
            .thenCompose(items -> {
              if (items.isEmpty()) {
                return inventoryManager.deleteHolding(holdingId, rqContext).thenApply(v -> Pair.of(holdingId, permanentLocationId));
              }
              return CompletableFuture.completedFuture(null);
            }));
        }
      });
      return collectResultsOnSuccess(deletedHoldingIds)
        .thenApply(resultDeletedHoldingVsLocationIds -> resultDeletedHoldingVsLocationIds.stream()
          .filter(pair -> Objects.nonNull(pair.getKey())).collect(toList()))
        .thenApply(resultDeletedHoldingVsLocationIds -> {
          if (logger.isDebugEnabled()) {
            String deletedIds = resultDeletedHoldingVsLocationIds.stream().map(Pair::getKey).collect(Collectors.joining(","));
            logger.debug(String.format("Holdings were removed : %s", deletedIds));
          }
          return resultDeletedHoldingVsLocationIds;
        });
    }
    return completedFuture(Collections.emptyList());
  }

  private CompletableFuture<List<JsonObject>> deletePiecesAndItems(List<JsonObject> onOrderItems, List<Piece> pieces, RequestContext rqContext) {
    List<CompletableFuture<JsonObject>> deletedItems = new ArrayList<>(onOrderItems.size());
    Map<String, List<Piece>> itemIdVsPiece = pieces.stream().collect(groupingBy(Piece::getItemId));
    onOrderItems.forEach(onOrderItem -> {
      List<Piece> itemPieces = itemIdVsPiece.get(onOrderItem.getString(ID));
      if (CollectionUtils.isNotEmpty(itemPieces)) {
        itemPieces.forEach(piece -> deletedItems.add(pieceService.deletePieceWithItem(piece.getId(), true, rqContext)
          .thenApply(v -> onOrderItem)));
      }
    });
    return collectResultsOnSuccess(deletedItems).thenApply(resultDeletedItems -> {
      if (logger.isDebugEnabled()) {
        String deletedIds = resultDeletedItems.stream().map(item -> item.getString(ID)).collect(Collectors.joining(","));
        logger.debug(String.format("Item were removed : %s", deletedIds));
      }
      return resultDeletedItems;
    });
  }

  private List<JsonObject> getItemsByStatus(List<JsonObject> items, String status) {
    return items.stream()
      .filter(item -> status.equalsIgnoreCase(item.getJsonObject(ITEM_STATUS).getString(ITEM_STATUS_NAME)))
      .collect(toList());
  }

  private CompletableFuture<CompositePurchaseOrder> updateAndGetOrderWithLines(CompositePurchaseOrder compPO, RequestContext requestContext) {
    if (CollectionUtils.isEmpty(compPO.getCompositePoLines())) {
      return purchaseOrderLineService.getCompositePoLinesByOrderId(compPO.getId(), requestContext)
        .thenApply(poLines -> {
          PoLineCommonUtil.sortPoLinesByPoLineNumber(poLines);
          return compPO.withCompositePoLines(poLines);
        })
        .thenApply(v -> compPO);
    } else {
      return completedFuture(compPO);
    }
  }
}
