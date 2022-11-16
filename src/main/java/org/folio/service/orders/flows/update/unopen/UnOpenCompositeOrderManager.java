package org.folio.service.orders.flows.update.unopen;

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

import java.util.ArrayList;
import java.util.Collections;
import java.util.List;
import java.util.Map;
import java.util.Objects;
import java.util.Optional;
import java.util.concurrent.CompletionException;
import java.util.stream.Collectors;

import org.apache.commons.collections4.CollectionUtils;
import org.apache.commons.lang3.StringUtils;
import org.apache.commons.lang3.tuple.Pair;
import org.apache.logging.log4j.LogManager;
import org.apache.logging.log4j.Logger;
import org.folio.models.ItemStatus;
import org.folio.models.pieces.PieceDeletionHolder;
import org.folio.okapi.common.GenericCompositeFuture;
import org.folio.orders.utils.HelperUtils;
import org.folio.orders.utils.PoLineCommonUtil;
import org.folio.rest.core.exceptions.ErrorCodes;
import org.folio.rest.core.exceptions.HttpException;
import org.folio.rest.core.models.RequestContext;
import org.folio.rest.jaxrs.model.CompositePoLine;
import org.folio.rest.jaxrs.model.CompositePurchaseOrder;
import org.folio.rest.jaxrs.model.Location;
import org.folio.rest.jaxrs.model.Piece;
import org.folio.service.ProtectionService;
import org.folio.service.finance.transaction.EncumbranceWorkflowStrategyFactory;
import org.folio.service.inventory.InventoryManager;
import org.folio.service.orders.OrderWorkflowType;
import org.folio.service.orders.PurchaseOrderLineService;
import org.folio.service.orders.PurchaseOrderStorageService;
import org.folio.service.pieces.PieceStorageService;

import io.vertx.core.Future;
import io.vertx.core.json.JsonObject;

public class UnOpenCompositeOrderManager {
  private static final Logger logger = LogManager.getLogger(UnOpenCompositeOrderManager.class);

  private final PurchaseOrderStorageService purchaseOrderStorageService;
  private final PurchaseOrderLineService purchaseOrderLineService;
  private final EncumbranceWorkflowStrategyFactory encumbranceWorkflowStrategyFactory;
  private final InventoryManager inventoryManager;
  private final PieceStorageService pieceStorageService;
  private final ProtectionService protectionService;

  public UnOpenCompositeOrderManager(PurchaseOrderLineService purchaseOrderLineService,
                                      EncumbranceWorkflowStrategyFactory encumbranceWorkflowStrategyFactory,
                                      InventoryManager inventoryManager, PieceStorageService pieceStorageService,
                                      PurchaseOrderStorageService purchaseOrderStorageService,
                                      ProtectionService protectionService) {
    this.purchaseOrderLineService = purchaseOrderLineService;
    this.encumbranceWorkflowStrategyFactory = encumbranceWorkflowStrategyFactory;
    this.inventoryManager = inventoryManager;
    this.pieceStorageService = pieceStorageService;
    this.purchaseOrderStorageService = purchaseOrderStorageService;
    this.protectionService = protectionService;
  }


  public Future<Void> process(CompositePurchaseOrder compPO, CompositePurchaseOrder poFromStorage, RequestContext requestContext) {
    return updateAndGetOrderWithLines(compPO, requestContext)
      .map(aVoid -> encumbranceWorkflowStrategyFactory.getStrategy(OrderWorkflowType.OPEN_TO_PENDING))
      .compose(strategy -> strategy.processEncumbrances(compPO, poFromStorage, requestContext))
      .onSuccess(ok -> PoLineCommonUtil.makePoLinesPending(compPO.getCompositePoLines()))
      .compose(ok -> updatePoLinesSummary(compPO.getCompositePoLines(), requestContext))
      .compose(ok -> processInventory(compPO.getCompositePoLines(), requestContext));

  }

  public Future<Void> updatePoLinesSummary(List<CompositePoLine> compositePoLines, RequestContext requestContext) {
    return GenericCompositeFuture.all(compositePoLines.stream()
      .map(HelperUtils::convertToPoLine)
      .map(line -> purchaseOrderLineService.saveOrderLine(line, requestContext))
      .collect(toList()))
      .map(ok -> null);
  }

  private Future<Void> processInventory(List<CompositePoLine> compositePoLines, RequestContext requestContext) {
    return GenericCompositeFuture.all(compositePoLines.stream()
      .map(line -> processInventory(line, requestContext))
        .collect(toList()))
      .mapEmpty();
  }

  private Future<Void> processInventory(CompositePoLine compPOL, RequestContext requestContext) {
    if (Boolean.TRUE.equals(compPOL.getIsPackage())) {
      return Future.succeededFuture();
    }
    if (PoLineCommonUtil.isInventoryUpdateNotRequired(compPOL) || isOnlyInstanceUpdateRequired(compPOL)) {
      return deleteExpectedPieces(compPOL, requestContext).onSuccess(pieces -> {
        if (logger.isDebugEnabled()) {
          String deletedIds = pieces.stream().map(Piece::getId).collect(Collectors.joining(","));
          logger.debug(String.format("Pieces were removed : %s", deletedIds));
        }
      })
        .mapEmpty();
    } else if (PoLineCommonUtil.isItemsUpdateRequired(compPOL)) {
      return processInventoryWithItems(compPOL, requestContext);
    } else if (PoLineCommonUtil.isHoldingsUpdateRequired(compPOL)) {
      return processInventoryOnlyWithHolding(compPOL, requestContext);
    }
    return Future.succeededFuture();
  }

  private Future<Void> processInventoryOnlyWithHolding(CompositePoLine compPOL, RequestContext requestContext) {
    return deleteExpectedPieces(compPOL, requestContext)
                .compose(deletedPieces -> {
                  List<String> holdingIds = compPOL.getLocations().stream()
                                    .filter(loc -> Objects.nonNull(loc.getHoldingId()))
                                    .map(Location::getHoldingId).collect(toList());
                  return inventoryManager.getHoldingsByIds(holdingIds, requestContext);
                })
                .compose(holdings -> deleteHoldings(holdings, requestContext))
                .onSuccess(deletedHoldingVsLocationIds -> updateLocations(compPOL, deletedHoldingVsLocationIds))
                .onSuccess(v -> logger.debug("Pieces, Holdings deleted after UnOpen order"))
      .mapEmpty();
  }

  private Future<List<Piece>> deleteExpectedPieces(CompositePoLine compPOL, RequestContext requestContext) {
    if (!PoLineCommonUtil.isReceiptNotRequired(compPOL.getReceiptStatus()) && Boolean.FALSE.equals(compPOL.getCheckinItems())) {
      return pieceStorageService.getExpectedPiecesByLineId(compPOL.getId(), requestContext)
        .compose(pieceCollection -> {
          if (isNotEmpty(pieceCollection.getPieces())) {
            return pieceStorageService.deletePiecesByIds(pieceCollection.getPieces().stream().map(Piece::getId).collect(toList()), requestContext)
                                .map(v -> pieceCollection.getPieces());
          }
          return Future.succeededFuture(Collections.emptyList());
        });
    }
    return Future.succeededFuture(Collections.emptyList());
  }

  private Future<Void> processInventoryWithItems(CompositePoLine compPOL, RequestContext requestContext) {
    return inventoryManager.getItemsByPoLineIdsAndStatus(List.of(compPOL.getId()), ItemStatus.ON_ORDER.value(), requestContext)
                          .compose(onOrderItems -> {
                            if (isNotEmpty(onOrderItems)) {
                              List<String> itemIds = onOrderItems.stream().map(item -> item.getString(ID)).collect(toList());
                              if (PoLineCommonUtil.isReceiptNotRequired(compPOL.getReceiptStatus())) {
                                return inventoryManager.deleteItems(itemIds,  false, requestContext)
                                              .compose(deletedItemIds -> deleteHoldingsByItems(onOrderItems, requestContext))
                                              .onSuccess(deletedHoldingVsLocationIds -> updateLocations(compPOL, deletedHoldingVsLocationIds))
                                              .onSuccess(v -> logger.debug("Items and holdings deleted after UnOpen order"))
                                  .mapEmpty();
                              }
                              return pieceStorageService.getExpectedPiecesByLineId(compPOL.getId(), requestContext)
                                .compose(pieceCollection -> {
                                  if (isNotEmpty(pieceCollection.getPieces())) {
                                    return inventoryManager.getItemRecordsByIds(itemIds, requestContext)
                                      .map(items -> getItemsByStatus(items, ItemStatus.ON_ORDER.value()))
                                      .compose(onOrderItemsP -> deletePiecesAndItems(onOrderItemsP, pieceCollection.getPieces(), requestContext))
                                      .compose(deletedItems -> deleteHoldingsByItems(deletedItems, requestContext))
                                      .onSuccess(deletedHoldingVsLocationIds -> updateLocations(compPOL, deletedHoldingVsLocationIds))
                                      .onSuccess(v -> logger.debug("Pieces, Items, Holdings deleted after UnOpen order"))
                                      .mapEmpty();
                                  }
                                  return Future.succeededFuture();
                                });
                            }
                            return Future.succeededFuture();
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


  private Future<List<Pair<String, String>>> deleteHoldingsByItems(List<JsonObject> deletedItems, RequestContext requestContext) {
    List<Future<Pair<String, String>>> deletedHoldingIds = new ArrayList<>(deletedItems.size());
    var holdingIdVsItemMap = deletedItems.stream()
                                        .collect(groupingBy(item -> Optional.ofNullable(item.getString(ITEM_HOLDINGS_RECORD_ID)) ));
    holdingIdVsItemMap.forEach((optionalHoldingId, holdingDeletedItems) -> {
      if (optionalHoldingId.isPresent()) {
        String holdingId = optionalHoldingId.get();
        String effectiveLocationId = holdingDeletedItems.get(0).getJsonObject(ITEM_EFFECTIVE_LOCATION).getString(ID);
        deletedHoldingIds.add(inventoryManager.getItemsByHoldingId(holdingId, requestContext)
          .compose(items -> {
            if (items.isEmpty()) {
              return inventoryManager.deleteHoldingById(holdingId, true, requestContext)
                                     .map(v -> Pair.of(holdingId, effectiveLocationId));
            }
            return Future.succeededFuture();
          }));
      }
    });
    return collectResultsOnSuccess(deletedHoldingIds)
      .map(resultDeletedHoldingVsLocationIds -> resultDeletedHoldingVsLocationIds.stream()
        .filter(pair -> Objects.nonNull(pair.getKey()))
        .collect(toList()))
      .map(resultDeletedHoldingVsLocationIds -> {
        if (logger.isDebugEnabled()) {
          String deletedIds = resultDeletedHoldingVsLocationIds.stream()
            .map(Pair::getKey)
            .collect(Collectors.joining(","));
          logger.debug(String.format("Holdings were removed : %s", deletedIds));
        }
        return resultDeletedHoldingVsLocationIds;
      });
  }

  private Future<List<Pair<String, String>>> deleteHoldings(List<JsonObject> holdings, RequestContext requestContext) {
    if (CollectionUtils.isNotEmpty(holdings)) {
      List<Future<Pair<String, String>>> deletedHoldingIds = new ArrayList<>(holdings.size());
      holdings.forEach(holding -> {
        String holdingId = holding.getString(ID);
        String permanentLocationId = holding.getString(HOLDING_PERMANENT_LOCATION_ID);
        if (holdingId != null) {
          deletedHoldingIds.add(inventoryManager.getItemsByHoldingId(holdingId, requestContext)
            .compose(items -> {
              if (items.isEmpty()) {
                return inventoryManager.deleteHoldingById(holdingId, true, requestContext)
                  .map(v -> Pair.of(holdingId, permanentLocationId));
              }
              return Future.succeededFuture();
            }));
        }
      });
      return collectResultsOnSuccess(deletedHoldingIds)
        .map(resultDeletedHoldingVsLocationIds -> resultDeletedHoldingVsLocationIds.stream()
          .filter(pair -> Objects.nonNull(pair.getKey())).collect(toList()))
        .map(resultDeletedHoldingVsLocationIds -> {
          if (logger.isDebugEnabled()) {
            String deletedIds = resultDeletedHoldingVsLocationIds.stream().map(Pair::getKey).collect(Collectors.joining(","));
            logger.debug(String.format("Holdings were removed : %s", deletedIds));
          }
          return resultDeletedHoldingVsLocationIds;
        });
    }
    return Future.succeededFuture(Collections.emptyList());
  }

  private Future<List<JsonObject>> deletePiecesAndItems(List<JsonObject> onOrderItems, List<Piece> pieces, RequestContext requestContext) {
    List<Future<JsonObject>> deletedItems = new ArrayList<>(onOrderItems.size());
    Map<Optional<String>, List<Piece>> itemIdVsPiece = pieces.stream().collect(groupingBy(piece -> Optional.ofNullable(piece.getItemId())));
    onOrderItems.forEach(onOrderItem -> {
      List<Piece> piecesWithItem = itemIdVsPiece.get(Optional.ofNullable(onOrderItem.getString(ID)));
      if (CollectionUtils.isNotEmpty(piecesWithItem)) {
        piecesWithItem.forEach(piece -> deletedItems.add(deletePieceWithItem(piece.getId(), requestContext)
          .map(v -> onOrderItem)));
      }
      List<Piece> piecesWithoutItem = itemIdVsPiece.get(Optional.empty());
      if (CollectionUtils.isNotEmpty(piecesWithoutItem)) {
        List<String> pieceIds = piecesWithoutItem.stream().map(Piece::getId).collect(toList());
        piecesWithItem.forEach(piece -> deletedItems.add(pieceStorageService.deletePiecesByIds(pieceIds, requestContext)
          .map(v -> onOrderItem)));
      }
    });
    return collectResultsOnSuccess(deletedItems).map(resultDeletedItems -> {
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

  private Future<CompositePurchaseOrder> updateAndGetOrderWithLines(CompositePurchaseOrder compPO, RequestContext requestContext) {
    if (CollectionUtils.isEmpty(compPO.getCompositePoLines())) {
      return purchaseOrderLineService.getCompositePoLinesByOrderId(compPO.getId(), requestContext)
        .map(poLines -> {
          PoLineCommonUtil.sortPoLinesByPoLineNumber(poLines);
          return compPO.withCompositePoLines(poLines);
        })
        .map(v -> compPO);
    } else {
      return Future.succeededFuture(compPO);
    }
  }

  public Future<Void> deletePieceWithItem(String pieceId, RequestContext requestContext) {
    PieceDeletionHolder holder = new PieceDeletionHolder().withDeleteHolding(true);
    return pieceStorageService.getPieceById(pieceId, requestContext)
      .onSuccess(holder::setPieceToDelete)
      .compose(aVoid -> purchaseOrderLineService.getOrderLineById(holder.getPieceToDelete().getPoLineId(), requestContext)
        .compose(poLine -> purchaseOrderStorageService.getPurchaseOrderById(poLine.getPurchaseOrderId(), requestContext)
          .onSuccess(purchaseOrder -> holder.withOrderInformation(purchaseOrder, poLine))
        ))
      .compose(purchaseOrder -> protectionService.isOperationRestricted(holder.getOriginPurchaseOrder().getAcqUnitIds(), DELETE, requestContext))
      .compose(vVoid -> canDeletePieceWithItem(holder.getPieceToDelete(), requestContext))
      .compose(aVoid -> pieceStorageService.deletePiece(pieceId, requestContext))
      .compose(aVoid -> deletePieceConnectedItem(holder.getPieceToDelete(), requestContext));
  }

  private Future<Void> deletePieceConnectedItem(Piece piece, RequestContext requestContext) {
    if (StringUtils.isNotEmpty(piece.getItemId())) {
      // Attempt to delete item
      return inventoryManager.deleteItem(piece.getItemId(), true, requestContext)
         .onFailure(t -> {
          // Skip error processing if item has already deleted
          if (t instanceof HttpException || ((HttpException) t).getCode() == 404) {
          } else {
            throw new CompletionException(t);
          }
        });
    } else {
      return Future.succeededFuture();
    }
  }

  private Future<Void> canDeletePieceWithItem(Piece piece, RequestContext requestContext) {
    return inventoryManager.getNumberOfRequestsByItemId(piece.getItemId(), requestContext)
      .map(numOfRequests -> {
        if (numOfRequests != null && numOfRequests > 0) {
          throw new HttpException(422, ErrorCodes.REQUEST_FOUND.toError());
        }
        return null;
      });
  }

}
