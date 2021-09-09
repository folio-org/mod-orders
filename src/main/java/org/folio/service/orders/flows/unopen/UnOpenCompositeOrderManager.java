package org.folio.service.orders.flows.unopen;

import static java.util.concurrent.CompletableFuture.completedFuture;
import static java.util.stream.Collectors.groupingBy;
import static java.util.stream.Collectors.toList;
import static org.apache.commons.collections4.CollectionUtils.isNotEmpty;
import static org.folio.orders.utils.HelperUtils.collectResultsOnSuccess;
import static org.folio.orders.utils.PoLineCommonUtil.isOnlyInstanceUpdateRequired;
import static org.folio.orders.utils.ProtectedOperationType.DELETE;
import static org.folio.service.inventory.InventoryManager.HOLDING_PERMANENT_LOCATION_ID;
import static org.folio.service.inventory.InventoryManager.ID;
import static org.folio.service.inventory.InventoryManager.ITEM_EFFECTIVE_LOCATION;
import static org.folio.service.inventory.InventoryManager.ITEM_HOLDINGS_RECORD_ID;
import static org.folio.service.inventory.InventoryManager.ITEM_STATUS;
import static org.folio.service.inventory.InventoryManager.ITEM_STATUS_NAME;
import static org.folio.service.pieces.PieceFlowUpdatePoLineUtil.updatePoLineLocationAndCostQuantity;

import java.util.ArrayList;
import java.util.Collections;
import java.util.List;
import java.util.Map;
import java.util.Objects;
import java.util.Optional;
import java.util.concurrent.CompletableFuture;
import java.util.concurrent.CompletionException;
import java.util.stream.Collectors;

import org.apache.commons.collections4.CollectionUtils;
import org.apache.commons.lang3.StringUtils;
import org.apache.commons.lang3.tuple.Pair;
import org.apache.logging.log4j.LogManager;
import org.apache.logging.log4j.Logger;
import org.folio.completablefuture.FolioVertxCompletableFuture;
import org.folio.models.pieces.PieceDeletionHolder;
import org.folio.orders.rest.exceptions.HttpException;
import org.folio.orders.utils.ErrorCodes;
import org.folio.orders.utils.HelperUtils;
import org.folio.orders.utils.PoLineCommonUtil;
import org.folio.rest.core.models.RequestContext;
import org.folio.rest.jaxrs.model.CompositePoLine;
import org.folio.rest.jaxrs.model.CompositePurchaseOrder;
import org.folio.rest.jaxrs.model.Location;
import org.folio.rest.jaxrs.model.Piece;
import org.folio.service.ProtectionService;
import org.folio.service.finance.transaction.EncumbranceWorkflowStrategyFactory;
import org.folio.service.inventory.InventoryManager;
import org.folio.models.ItemStatus;
import org.folio.service.orders.OrderWorkflowType;
import org.folio.service.orders.PurchaseOrderLineService;
import org.folio.service.orders.PurchaseOrderService;
import org.folio.service.pieces.PieceDeletionFlowManager;
import org.folio.service.pieces.PieceFlowUpdatePoLineUtil;
import org.folio.service.pieces.PieceStorageService;
import org.folio.service.pieces.PieceService;

import io.vertx.core.json.JsonObject;

public class UnOpenCompositeOrderManager {
  private static final Logger logger = LogManager.getLogger(UnOpenCompositeOrderManager.class);

  private final PurchaseOrderService purchaseOrderService;
  private final PurchaseOrderLineService purchaseOrderLineService;
  private final EncumbranceWorkflowStrategyFactory encumbranceWorkflowStrategyFactory;
  private final InventoryManager inventoryManager;
  private final PieceStorageService pieceStorageService;
  private final PieceDeletionFlowManager pieceDeletionFlowManager;
  private final ProtectionService protectionService;

  public UnOpenCompositeOrderManager(PurchaseOrderLineService purchaseOrderLineService,
                                      EncumbranceWorkflowStrategyFactory encumbranceWorkflowStrategyFactory,
                                      InventoryManager inventoryManager, PieceStorageService pieceStorageService,
                                      PieceDeletionFlowManager pieceDeletionFlowManager, PurchaseOrderService purchaseOrderService,
                                      ProtectionService protectionService) {
    this.purchaseOrderLineService = purchaseOrderLineService;
    this.encumbranceWorkflowStrategyFactory = encumbranceWorkflowStrategyFactory;
    this.inventoryManager = inventoryManager;
    this.pieceStorageService = pieceStorageService;
    this.pieceDeletionFlowManager = pieceDeletionFlowManager;
    this.purchaseOrderService = purchaseOrderService;
    this.protectionService = protectionService;
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
            return pieceStorageService.deletePiecesByIds(pieceCollection.getPieces().stream().map(Piece::getId).collect(toList()), rqContext)
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
    var holdingIdVsItemMap = deletedItems.stream()
                                        .collect(groupingBy(item -> Optional.ofNullable(item.getString(ITEM_HOLDINGS_RECORD_ID)) ));
    holdingIdVsItemMap.forEach((optionalHoldingId, holdingDeletedItems) -> {
      if (optionalHoldingId.isPresent()) {
        String holdingId = optionalHoldingId.get();
        String effectiveLocationId = holdingDeletedItems.get(0).getJsonObject(ITEM_EFFECTIVE_LOCATION).getString(ID);
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
    Map<Optional<String>, List<Piece>> itemIdVsPiece = pieces.stream().collect(groupingBy(piece -> Optional.ofNullable(piece.getItemId())));
    onOrderItems.forEach(onOrderItem -> {
      List<Piece> piecesWithItem = itemIdVsPiece.get(Optional.ofNullable(onOrderItem.getString(ID)));
      if (CollectionUtils.isNotEmpty(piecesWithItem)) {
        piecesWithItem.forEach(piece -> deletedItems.add(deletePieceWithItem(piece.getId(), rqContext)
          .thenApply(v -> onOrderItem)));
      }
      List<Piece> piecesWithoutItem = itemIdVsPiece.get(Optional.empty());
      if (CollectionUtils.isNotEmpty(piecesWithoutItem)) {
        List<String> pieceIds = piecesWithoutItem.stream().map(Piece::getId).collect(toList());
        piecesWithItem.forEach(piece -> deletedItems.add(pieceStorageService.deletePiecesByIds(pieceIds, rqContext)
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

  public CompletableFuture<Void> deletePieceWithItem(String pieceId, RequestContext requestContext) {
    PieceDeletionHolder holder = new PieceDeletionHolder();
    return pieceStorageService.getPieceById(pieceId, requestContext)
      .thenCompose(piece -> purchaseOrderLineService.getOrderLineById(piece.getPoLineId(), requestContext)
        .thenCompose(poLine -> purchaseOrderService.getPurchaseOrderById(poLine.getPurchaseOrderId(), requestContext)
          .thenAccept(purchaseOrder -> holder.shallowCopy(new PieceDeletionHolder(purchaseOrder, poLine).withPieceToDelete(piece)))
        ))
      .thenCompose(purchaseOrder -> protectionService.isOperationRestricted(holder.getOriginPurchaseOrder().getAcqUnitIds(), DELETE, requestContext))
      .thenCompose(vVoid -> canDeletePiece(holder.getPieceToDelete(), requestContext))
      .thenCompose(aVoid -> pieceStorageService.deletePiece(pieceId, requestContext))
      .thenCompose(aVoid -> deletePieceConnectedItem(holder.getPieceToDelete(), requestContext));
  }

  private CompletableFuture<Void> deletePieceConnectedItem(Piece piece, RequestContext requestContext) {
    if (StringUtils.isNotEmpty(piece.getItemId())) {
      // Attempt to delete item
      return inventoryManager.deleteItem(piece.getItemId(), requestContext)
        .exceptionally(t -> {
          // Skip error processing if item has already deleted
          if (t instanceof HttpException && ((HttpException) t).getCode() == 404) {
            return null;
          } else {
            throw new CompletionException(t);
          }
        });
    } else {
      return CompletableFuture.completedFuture(null);
    }
  }

  private CompletableFuture<Void> canDeletePiece(Piece piece, RequestContext requestContext) {
    return inventoryManager.getNumberOfRequestsByItemId(piece.getItemId(), requestContext)
      .thenAccept(numOfRequests -> {
        if (numOfRequests > 0) {
          throw new HttpException(422, ErrorCodes.REQUEST_FOUND.toError());
        }
      });
  }

}
