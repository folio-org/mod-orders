package org.folio.service.orders.flows.update.unopen;

import static java.util.stream.Collectors.groupingBy;
import static java.util.stream.Collectors.toList;
import static org.folio.orders.utils.HelperUtils.collectResultsOnSuccess;
import static org.folio.orders.utils.ProtectedOperationType.DELETE;
import static org.folio.orders.utils.RequestContextUtil.createContextWithNewTenantId;
import static org.folio.service.inventory.InventoryHoldingManager.HOLDING_PERMANENT_LOCATION_ID;
import static org.folio.service.inventory.InventoryItemManager.ID;
import static org.folio.service.inventory.InventoryItemManager.ITEM_EFFECTIVE_LOCATION;
import static org.folio.service.inventory.InventoryItemManager.ITEM_HOLDINGS_RECORD_ID;
import static org.folio.service.inventory.InventoryItemManager.ITEM_STATUS;
import static org.folio.service.inventory.InventoryItemManager.ITEM_STATUS_NAME;

import java.util.ArrayList;
import java.util.EnumSet;
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
import org.folio.orders.utils.PoLineCommonUtil;
import org.folio.rest.core.exceptions.ErrorCodes;
import org.folio.rest.core.exceptions.HttpException;
import org.folio.rest.core.models.RequestContext;
import org.folio.rest.jaxrs.model.PoLine;
import org.folio.rest.jaxrs.model.CompositePurchaseOrder;
import org.folio.rest.jaxrs.model.Location;
import org.folio.rest.jaxrs.model.Piece;
import org.folio.service.CirculationRequestsRetriever;
import org.folio.service.ProtectionService;
import org.folio.service.finance.transaction.EncumbranceWorkflowStrategyFactory;
import org.folio.service.inventory.InventoryHoldingManager;
import org.folio.service.inventory.InventoryItemManager;
import org.folio.service.orders.OrderWorkflowType;
import org.folio.service.orders.PurchaseOrderLineService;
import org.folio.service.orders.PurchaseOrderStorageService;
import org.folio.service.pieces.PieceStorageService;

import io.vertx.core.Future;
import io.vertx.core.json.JsonObject;

public class UnOpenCompositeOrderManager {
  private static final Logger logger = LogManager.getLogger(UnOpenCompositeOrderManager.class);
  private static final EnumSet<PoLine.PaymentStatus> SUPPORTED_PAYMENT_STATUSES =
    EnumSet.of(PoLine.PaymentStatus.AWAITING_PAYMENT, PoLine.PaymentStatus.ONGOING);
  private static final EnumSet<PoLine.ReceiptStatus> SUPPORTED_RECEIPT_STATUSES =
    EnumSet.of(PoLine.ReceiptStatus.AWAITING_RECEIPT, PoLine.ReceiptStatus.ONGOING);

  private final PurchaseOrderStorageService purchaseOrderStorageService;
  private final PurchaseOrderLineService purchaseOrderLineService;
  private final EncumbranceWorkflowStrategyFactory encumbranceWorkflowStrategyFactory;
  private final InventoryItemManager inventoryItemManager;
  private final InventoryHoldingManager inventoryHoldingManager;
  private final PieceStorageService pieceStorageService;
  private final ProtectionService protectionService;
  private final CirculationRequestsRetriever circulationRequestsRetriever;

  public UnOpenCompositeOrderManager(PurchaseOrderLineService purchaseOrderLineService,
                                     EncumbranceWorkflowStrategyFactory encumbranceWorkflowStrategyFactory,
                                     InventoryItemManager inventoryItemManager,
                                     InventoryHoldingManager inventoryHoldingManager,
                                     PieceStorageService pieceStorageService,
                                     PurchaseOrderStorageService purchaseOrderStorageService,
                                     ProtectionService protectionService,
                                     CirculationRequestsRetriever circulationRequestsRetriever) {
    this.purchaseOrderLineService = purchaseOrderLineService;
    this.encumbranceWorkflowStrategyFactory = encumbranceWorkflowStrategyFactory;
    this.inventoryItemManager = inventoryItemManager;
    this.inventoryHoldingManager = inventoryHoldingManager;
    this.pieceStorageService = pieceStorageService;
    this.purchaseOrderStorageService = purchaseOrderStorageService;
    this.protectionService = protectionService;
    this.circulationRequestsRetriever = circulationRequestsRetriever;
  }


  public Future<Void> process(CompositePurchaseOrder compPO, CompositePurchaseOrder poFromStorage, boolean deleteHoldings, RequestContext requestContext) {
    return updateAndGetOrderWithLines(compPO, requestContext)
      .map(aVoid -> encumbranceWorkflowStrategyFactory.getStrategy(OrderWorkflowType.OPEN_TO_PENDING))
      .compose(strategy -> strategy.processEncumbrances(compPO, poFromStorage, requestContext))
      .compose(ok -> processInventory(compPO.getPoLines(), deleteHoldings, requestContext))
      .map(ok -> {
        compPO.getPoLines().forEach(this::makePoLinePending);
        return null;
      })
      .compose(ok -> updatePoLinesSummary(compPO.getPoLines(), requestContext));

  }

  private Future<CompositePurchaseOrder> updateAndGetOrderWithLines(CompositePurchaseOrder compPO, RequestContext requestContext) {
    if (CollectionUtils.isNotEmpty(compPO.getPoLines())) {
      return Future.succeededFuture(compPO);
    }
    return purchaseOrderLineService.getPoLinesByOrderId(compPO.getId(), requestContext)
      .map(poLines -> {
        PoLineCommonUtil.sortPoLinesByPoLineNumber(poLines);
        return compPO.withPoLines(poLines);
      })
      .map(v -> compPO);
  }

  private void makePoLinePending(PoLine poLine) {
    if (SUPPORTED_PAYMENT_STATUSES.contains(poLine.getPaymentStatus())) {
      poLine.setPaymentStatus(PoLine.PaymentStatus.PENDING);
    }
    if (SUPPORTED_RECEIPT_STATUSES.contains(poLine.getReceiptStatus())) {
      poLine.setReceiptStatus(PoLine.ReceiptStatus.PENDING);
    }
  }

  private Future<Void> updatePoLinesSummary(List<PoLine> poLines, RequestContext requestContext) {
    return GenericCompositeFuture.join(poLines.stream()
        .map(line -> purchaseOrderLineService.saveOrderLine(line, requestContext))
        .toList())
      .map(ok -> null);
  }

  public Future<Void> rollbackInventory(CompositePurchaseOrder compPO, RequestContext requestContext) {
    return processInventory(compPO.getPoLines(), true, requestContext);
  }

  private Future<Void> processInventory(List<PoLine> poLines, boolean deleteHoldings, RequestContext requestContext) {
    return GenericCompositeFuture.join(
        poLines
          .stream()
          .map(line -> processInventory(line, deleteHoldings, requestContext))
          .toList()
      )
      .mapEmpty();
  }

  /**
   * Processes inventory.
   * <p>
   * PO Line checkinItems has the following values: (synchronized - false, independent - true)
   * deleteHoldings == true && workflow synchronized => delete holdings and items
   * deleteHoldings == false && workflow synchronized => delete items
   * deleteHoldings == true && workflow independent => delete holdings
   * deleteHoldings == false && workflow independent => do nothing
   *
   * @param poLine         the purchase order
   * @param deleteHoldings delete holdings flag
   * @param requestContext the request context
   * @return future with void
   */
  private Future<Void> processInventory(PoLine poLine, boolean deleteHoldings, RequestContext requestContext) {
    if (Boolean.TRUE.equals(poLine.getIsPackage())) {
      return Future.succeededFuture();
    }
    if (PoLineCommonUtil.isInventoryUpdateNotRequired(poLine) || PoLineCommonUtil.isOnlyInstanceUpdateRequired(poLine)) {
      return deleteExpectedPieces(poLine, requestContext).onSuccess(pieces -> {
          if (logger.isDebugEnabled()) {
            String deletedIds = pieces.stream().map(Piece::getId).collect(Collectors.joining(","));
            logger.debug("Pieces were removed: {}", deletedIds);
          }
        })
        .mapEmpty();
    }
    if (PoLineCommonUtil.isItemsUpdateRequired(poLine)) {
      if (poLine.getCheckinItems()) { // independent workflow
        return (deleteHoldings)
          ? processInventoryOnlyWithHolding(poLine, requestContext)
          : Future.succeededFuture();
      } else { // synchronized workflow
        return (deleteHoldings)
          ? processInventoryHoldingWithItems(poLine, requestContext)
          : processInventoryOnlyWithItems(poLine, requestContext);
      }
    }
    if (PoLineCommonUtil.isHoldingsUpdateRequired(poLine)) {
      if (poLine.getCheckinItems()) { // independent workflow
        return (deleteHoldings)
          ? processInventoryOnlyWithHolding(poLine, requestContext)
          : Future.succeededFuture();
      } else { // synchronized workflow
        return (deleteHoldings)
          ? processInventoryOnlyWithHolding(poLine, requestContext)
          : deleteExpectedPieces(poLine, requestContext).mapEmpty();
      }
    }

    return Future.succeededFuture();
  }


  private Future<Void> processInventoryOnlyWithHolding(PoLine poLine, RequestContext requestContext) {
    return deleteExpectedPieces(poLine, requestContext)
      .compose(deletedPieces -> {
        var deleteHoldingsVsLocations = inventoryHoldingManager.getHoldingsByLocationTenants(poLine, requestContext)
          .entrySet()
          .stream()
          .map(entry -> entry.getValue().compose(holdings -> deleteHoldings(entry.getKey(), holdings, requestContext)))
          .toList();
        return GenericCompositeFuture.all(deleteHoldingsVsLocations).map(ar -> {
          var deletedHoldingVsLocationIds = deleteHoldingsVsLocations.stream()
            .map(Future::result)
            .flatMap(List::stream)
            .toList();
          updateLocations(poLine, deletedHoldingVsLocationIds);
          return null;
        });
      })
      .onSuccess(v -> logger.debug("Pieces, Holdings deleted after UnOpen order"))
      .mapEmpty();
  }

  private Future<List<Pair<String, String>>> deleteHoldings(String tenantId, List<JsonObject> holdings, RequestContext requestContext) {
    if (CollectionUtils.isEmpty(holdings)) {
      return Future.succeededFuture(List.of());
    }
    RequestContext newContext = createContextWithNewTenantId(requestContext, tenantId);
    List<Future<Pair<String, String>>> deletedHoldingIds = new ArrayList<>(holdings.size());
    holdings.forEach(holding -> {
      String holdingId = holding.getString(ID);
      String permanentLocationId = holding.getString(HOLDING_PERMANENT_LOCATION_ID);
      if (holdingId != null) {
        deletedHoldingIds.add(inventoryItemManager.getItemsByHoldingId(holdingId, newContext)
          .compose(items -> {
            if (items.isEmpty()) {
              logger.info("deleteHoldings:: Deleting holdings, holdingId: {}", holdingId);
              return inventoryHoldingManager.deleteHoldingById(holdingId, true, newContext)
                .map(v -> Pair.of(holdingId, permanentLocationId));
            } else {
              logger.info("deleteHoldings:: Cannot delete holdings with items, holdingId: {}, items: {}", holdingId, items.size());
            }
            return Future.succeededFuture();
          }));
      }
    });
    return collectResultsOnSuccess(deletedHoldingIds)
      .map(resultDeletedHoldingVsLocationIds -> resultDeletedHoldingVsLocationIds.stream()
        .filter(pair -> Objects.nonNull(pair) && Objects.nonNull(pair.getKey())).collect(toList()))
      .map(resultDeletedHoldingVsLocationIds -> {
        if (logger.isDebugEnabled()) {
          String deletedIds = resultDeletedHoldingVsLocationIds.stream().map(Pair::getKey).collect(Collectors.joining(","));
          logger.debug("Holdings were removed: {}", deletedIds);
        }
        return resultDeletedHoldingVsLocationIds;
      });
  }


  private Future<Void> processInventoryOnlyWithItems(PoLine poLine, RequestContext requestContext) {
    return deleteExpectedPieces(poLine, requestContext)
      .compose(deletedPieces ->
        GenericCompositeFuture.all(
          PoLineCommonUtil.getTenantsFromLocations(poLine)
            .stream()
            .map(tenantId -> processInventoryOnlyWithItemsForTenant(poLine, createContextWithNewTenantId(requestContext, tenantId)))
            .toList()
        )
      ).mapEmpty();
  }

  private Future<?> processInventoryOnlyWithItemsForTenant(PoLine poLine, RequestContext requestContext) {
    return inventoryItemManager.getItemsByPoLineIdsAndStatus(List.of(poLine.getId()), ItemStatus.ON_ORDER.value(), requestContext)
      .compose(onOrderItems -> {
        if (CollectionUtils.isEmpty(onOrderItems)) {
          return Future.succeededFuture();
        }
        List<String> itemIds = onOrderItems.stream().map(item -> item.getString(ID)).toList();
        return inventoryItemManager.deleteItems(itemIds, false, requestContext);
      })
      .onSuccess(v -> logger.info("Items and pieces deleted after Un-Open order with id: {}", poLine.getId()))
      .onFailure(e -> logger.error("Items and pieces deletion failed after Un-Open order with id: {}", poLine.getId(), e));
  }


  private Future<Void> processInventoryHoldingWithItems(PoLine poLine, RequestContext requestContext) {
    return GenericCompositeFuture.all(
      PoLineCommonUtil.getTenantsFromLocations(poLine)
        .stream()
        .map(tenantId -> processInventoryHoldingWithItemsForTenant(poLine, requestContext, createContextWithNewTenantId(requestContext, tenantId)))
        .toList()
    ).mapEmpty();
  }

  private Future<Void> processInventoryHoldingWithItemsForTenant(PoLine poLine,
                                                                 RequestContext centralTenantContext,
                                                                 RequestContext locationContext) {
    return inventoryItemManager.getItemsByPoLineIdsAndStatus(List.of(poLine.getId()), ItemStatus.ON_ORDER.value(), locationContext)
      .compose(onOrderItems -> {
        if (CollectionUtils.isEmpty(onOrderItems)) {
          return Future.succeededFuture();
        }
        List<String> itemIds = onOrderItems.stream().map(item -> item.getString(ID)).toList();
        if (PoLineCommonUtil.isReceiptNotRequired(poLine.getReceiptStatus())) {
          return inventoryItemManager.deleteItems(itemIds, false, locationContext)
            .compose(deletedItemIds -> deleteHoldingsByItems(onOrderItems, locationContext))
            .map(deletedHoldingVsLocationIds -> {
              updateLocations(poLine, deletedHoldingVsLocationIds);
              return null;
            })
            .onSuccess(v -> logger.debug("Items and holdings deleted after UnOpen order"))
            .mapEmpty();
        }
        return pieceStorageService.getExpectedPiecesByLineId(poLine.getId(), centralTenantContext)
          .compose(pieceCollection -> {
            if (CollectionUtils.isEmpty(pieceCollection.getPieces())) {
              return Future.succeededFuture();
            }
            return inventoryItemManager.getItemRecordsByIds(itemIds, locationContext)
              .map(items -> filterItemsByStatus(items, ItemStatus.ON_ORDER.value()))
              .compose(onOrderItemsP -> deletePiecesAndItems(onOrderItemsP, pieceCollection.getPieces(), centralTenantContext, locationContext))
              .compose(deletedItems -> deleteHoldingsByItems(deletedItems, locationContext))
              .map(deletedHoldingVsLocationIds -> {
                updateLocations(poLine, deletedHoldingVsLocationIds);
                return null;
              })
              .onSuccess(v -> logger.debug("Pieces, Items, Holdings deleted after UnOpen order"))
              .onFailure(e -> logger.error("Pieces, Items, Holdings failed to be deleted after UnOpen order for order id: {}",
                poLine.getId(), e))
              .mapEmpty();
          });
      });
  }

  private Future<List<Pair<String, String>>> deleteHoldingsByItems(List<JsonObject> deletedItems, RequestContext requestContext) {
    List<Future<Pair<String, String>>> deletedHoldingIds = new ArrayList<>(deletedItems.size());
    var holdingIdVsItemMap = deletedItems.stream()
      .collect(groupingBy(item -> Optional.ofNullable(item.getString(ITEM_HOLDINGS_RECORD_ID))));
    holdingIdVsItemMap.forEach((optionalHoldingId, holdingDeletedItems) -> {
      if (optionalHoldingId.isEmpty()) {
        return;
      }
      String holdingId = optionalHoldingId.get();
      String effectiveLocationId = holdingDeletedItems.get(0).getJsonObject(ITEM_EFFECTIVE_LOCATION).getString(ID);
      deletedHoldingIds.add(inventoryItemManager.getItemsByHoldingId(holdingId, requestContext)
        .compose(items -> {
          if (items.isEmpty()) {
            logger.info("deleteHoldingsByItems:: Deleting holdings, holdingId: {}", holdingId);
            return inventoryHoldingManager.deleteHoldingById(holdingId, true, requestContext)
              .map(v -> Pair.of(holdingId, effectiveLocationId));
          } else {
            logger.info("deleteHoldingsByItems:: Cannot delete holdings with items, holdingId: {}, items: {}", holdingId, items.size());
          }
          return Future.succeededFuture();
        }));
    });
    return collectResultsOnSuccess(deletedHoldingIds)
      .map(resultDeletedHoldingVsLocationIds -> resultDeletedHoldingVsLocationIds.stream()
        .filter(pair -> Objects.nonNull(pair) && Objects.nonNull(pair.getKey()))
        .collect(toList()))
      .map(resultDeletedHoldingVsLocationIds -> {
        if (logger.isDebugEnabled()) {
          String deletedIds = resultDeletedHoldingVsLocationIds.stream()
            .map(Pair::getKey)
            .collect(Collectors.joining(","));
          logger.debug("Holdings were removed: {}", deletedIds);
        }
        return resultDeletedHoldingVsLocationIds;
      });
  }

  private Future<List<JsonObject>> deletePiecesAndItems(List<JsonObject> onOrderItems,
                                                        List<Piece> pieces,
                                                        RequestContext centralTenantContext,
                                                        RequestContext locationContext) {
    List<Future<JsonObject>> deletedItems = new ArrayList<>(onOrderItems.size());
    Map<Optional<String>, List<Piece>> itemIdVsPiece = pieces.stream().collect(groupingBy(piece -> Optional.ofNullable(piece.getItemId())));
    onOrderItems.forEach(onOrderItem -> {
      List<Piece> piecesWithItem = itemIdVsPiece.get(Optional.ofNullable(onOrderItem.getString(ID)));
      if (CollectionUtils.isNotEmpty(piecesWithItem)) {
        piecesWithItem.forEach(piece -> deletedItems.add(deletePieceWithItem(piece.getId(), centralTenantContext, locationContext)
          .map(v -> onOrderItem)));
      }
      List<Piece> piecesWithoutItem = itemIdVsPiece.get(Optional.empty());
      if (CollectionUtils.isNotEmpty(piecesWithoutItem)) {
        List<String> pieceIds = piecesWithoutItem.stream().map(Piece::getId).collect(toList());
        piecesWithItem.forEach(piece -> deletedItems.add(pieceStorageService.deletePiecesByIds(pieceIds, centralTenantContext)
          .map(v -> onOrderItem)));
      }
    });
    return collectResultsOnSuccess(deletedItems).map(resultDeletedItems -> {
      if (logger.isDebugEnabled()) {
        String deletedIds = resultDeletedItems.stream().map(item -> item.getString(ID)).collect(Collectors.joining(","));
        logger.debug("Item were removed: {}", deletedIds);
      }
      return resultDeletedItems;
    });
  }

  private Future<Void> deletePieceWithItem(String pieceId, RequestContext requestContext, RequestContext itemContext) {
    PieceDeletionHolder holder = new PieceDeletionHolder().withDeleteHolding(true);
    return pieceStorageService.getPieceById(pieceId, requestContext)
      .map(piece -> {
        holder.setPieceToDelete(piece);
        return null;
      })
      .compose(aVoid -> purchaseOrderLineService.getOrderLineById(holder.getPieceToDelete().getPoLineId(), requestContext))
      .compose(poLine -> purchaseOrderStorageService.getPurchaseOrderById(poLine.getPurchaseOrderId(), requestContext)
        .map(purchaseOrder -> {
          holder.withOrderInformation(purchaseOrder, poLine);
          return null;
        })
      )
      .compose(aVoid -> protectionService.isOperationRestricted(holder.getOriginPurchaseOrder().getAcqUnitIds(), DELETE, requestContext))
      .compose(vVoid -> canDeletePieceWithItem(holder.getPieceToDelete(), requestContext))
      .compose(aVoid -> pieceStorageService.deletePiece(pieceId, requestContext))
      .compose(aVoid -> deletePieceConnectedItem(holder.getPieceToDelete(), itemContext));
  }

  private List<JsonObject> filterItemsByStatus(List<JsonObject> items, String status) {
    return items.stream()
      .filter(item -> status.equalsIgnoreCase(item.getJsonObject(ITEM_STATUS).getString(ITEM_STATUS_NAME)))
      .toList();
  }

  private Future<Void> deletePieceConnectedItem(Piece piece, RequestContext requestContext) {
    if (StringUtils.isEmpty(piece.getItemId())) {
      return Future.succeededFuture();
    }
    return inventoryItemManager.deleteItem(piece.getItemId(), true, requestContext)
      .recover(t -> {
        if (t instanceof HttpException httpException && httpException.getCode() == 404) {
          return null;
        }
        throw new CompletionException(t);
      });
  }

  private Future<Void> canDeletePieceWithItem(Piece piece, RequestContext requestContext) {
    return circulationRequestsRetriever.getNumberOfRequestsByItemId(piece.getItemId(), requestContext)
      .map(numOfRequests -> {
        if (numOfRequests != null && numOfRequests > 0) {
          throw new HttpException(422, ErrorCodes.REQUEST_FOUND.toError());
        }
        return null;
      });
  }


  private Future<List<Piece>> deleteExpectedPieces(PoLine poLine, RequestContext requestContext) {
    if (PoLineCommonUtil.isReceiptNotRequired(poLine.getReceiptStatus()) || Boolean.TRUE.equals(poLine.getCheckinItems())) {
      logger.info("Receipt is not required or independent receiving flow is used, skipping deleting pieces, poLineId: {}", poLine.getId());
      return Future.succeededFuture(List.of());
    }
    return pieceStorageService.getExpectedPiecesByLineId(poLine.getId(), requestContext)
      .compose(pieceCollection -> {
        if (CollectionUtils.isEmpty(pieceCollection.getPieces())) {
          return Future.succeededFuture(List.of());
        }
        return pieceStorageService.deletePiecesByIds(pieceCollection.getPieces().stream().map(Piece::getId).collect(toList()), requestContext)
          .map(v -> pieceCollection.getPieces());
      });
  }

  private void updateLocations(PoLine poLine, List<Pair<String, String>> deletedHoldingVsLocationIds) {
    if (CollectionUtils.isEmpty(deletedHoldingVsLocationIds)) {
      return;
    }
    Map<String, List<Location>> holdingIdVsLocations = poLine.getLocations().stream().filter(entity -> Objects.nonNull(entity.getHoldingId()))
      .collect(groupingBy(Location::getHoldingId));
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
