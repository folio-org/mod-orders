package org.folio.service.orders.flows.update.unopen;

import static org.folio.orders.utils.FutureUtils.asFuture;
import static org.folio.orders.utils.HelperUtils.collectResultsOnSuccess;
import static org.folio.orders.utils.RequestContextUtil.createContextWithNewTenantId;
import static org.folio.orders.utils.StreamUtils.map;
import static org.folio.service.inventory.InventoryItemManager.ID;

import java.util.ArrayList;
import java.util.EnumSet;
import java.util.HashMap;
import java.util.List;
import java.util.Map;
import java.util.Objects;
import java.util.Set;
import java.util.function.Function;

import lombok.RequiredArgsConstructor;
import lombok.extern.log4j.Log4j2;
import one.util.streamex.StreamEx;
import org.apache.commons.collections4.CollectionUtils;
import org.apache.commons.lang3.tuple.Pair;
import org.folio.models.ItemStatus;
import org.folio.models.HoldingDataExclusionConfig;
import org.folio.models.HoldingDataExclusionMode;
import org.folio.orders.utils.HelperUtils;
import org.folio.orders.utils.PoLineCommonUtil;
import org.folio.rest.core.exceptions.ErrorCodes;
import org.folio.rest.core.exceptions.HttpException;
import org.folio.rest.core.models.RequestContext;
import org.folio.rest.jaxrs.model.PoLine;
import org.folio.rest.jaxrs.model.CompositePurchaseOrder;
import org.folio.rest.jaxrs.model.Piece;
import org.folio.rest.jaxrs.model.acq.Location;
import org.folio.service.CirculationRequestsRetriever;
import org.folio.service.HoldingDeletionService;
import org.folio.service.finance.transaction.EncumbranceWorkflowStrategyFactory;
import org.folio.service.inventory.InventoryHoldingManager;
import org.folio.service.inventory.InventoryItemManager;
import org.folio.service.orders.OrderWorkflowType;
import org.folio.service.orders.PurchaseOrderLineService;
import org.folio.service.pieces.PieceStorageService;

import io.vertx.core.Future;
import io.vertx.core.json.JsonObject;

@Log4j2
@RequiredArgsConstructor
public class UnOpenCompositeOrderManager {

  private static final EnumSet<PoLine.PaymentStatus> SUPPORTED_PAYMENT_STATUSES =
    EnumSet.of(PoLine.PaymentStatus.AWAITING_PAYMENT, PoLine.PaymentStatus.ONGOING);
  private static final EnumSet<PoLine.ReceiptStatus> SUPPORTED_RECEIPT_STATUSES =
    EnumSet.of(PoLine.ReceiptStatus.AWAITING_RECEIPT, PoLine.ReceiptStatus.ONGOING);

  private final PurchaseOrderLineService purchaseOrderLineService;
  private final EncumbranceWorkflowStrategyFactory encumbranceWorkflowStrategyFactory;
  private final InventoryItemManager inventoryItemManager;
  private final InventoryHoldingManager inventoryHoldingManager;
  private final PieceStorageService pieceStorageService;
  private final CirculationRequestsRetriever circulationRequestsRetriever;
  private final HoldingDeletionService holdingDeletionService;

  public Future<Void> process(CompositePurchaseOrder compPO, CompositePurchaseOrder poFromStorage, boolean deleteHoldings, RequestContext requestContext) {
    if (CollectionUtils.isEmpty(compPO.getPoLines())) {
      compPO.setPoLines(poFromStorage.getPoLines());
    }
    var strategy = encumbranceWorkflowStrategyFactory.getStrategy(OrderWorkflowType.OPEN_TO_PENDING);
    return strategy.processEncumbrances(compPO, poFromStorage, requestContext)
      .compose(ok -> asFuture(() -> compPO.getPoLines().forEach(this::makePoLinePending)))
      .compose(ok -> processInventory(compPO.getPoLines(), deleteHoldings, requestContext))
      .compose(ok -> updatePoLinesSummary(compPO.getPoLines(), requestContext))
      .onSuccess(v -> log.info("Unopen order: done processing inventory for po id: {}", compPO.getId()))
      .onFailure(t -> log.error("Unopen order: failed to process inventory for po id: {}", compPO.getId(), t));
  }

  public Future<Void> rollbackInventory(CompositePurchaseOrder compPO, RequestContext requestContext) {
    return processPoLines(compPO.getPoLines(), poLine -> processInventory(poLine, true, requestContext))
      .onSuccess(v -> log.info("rollbackInventory:: successfully rolled back inventory changes, order id={}", compPO.getId()))
      .onFailure(t -> log.error("rollbackInventory:: error when trying to rollback inventory changes, order id={}", compPO.getId(), t));
  }

  public Future<Void> checkRequests(CompositePurchaseOrder compPO, CompositePurchaseOrder poFromStorage, RequestContext requestContext) {
    List<PoLine> poLines = compPO.getPoLines();
    if (CollectionUtils.isEmpty(poLines)) {
      poLines = poFromStorage.getPoLines();
    }
    poLines = poLines.stream()
      .filter(poLine -> !Boolean.TRUE.equals(poLine.getIsPackage()))
      .toList();
    HashMap<String, List<String>> tenantIdToPoLineIds = createTenantIdToPoLineIdsMap(poLines);
    List<Future<Void>> futures = tenantIdToPoLineIds.keySet().stream()
      .map(tenantId -> {
        List<String> poLineIds = tenantIdToPoLineIds.get(tenantId);
        RequestContext tenantRequestContext = createContextWithNewTenantId(requestContext, tenantId);
        return inventoryItemManager.getItemsByPoLineIdsAndStatus(poLineIds, ItemStatus.ON_ORDER.value(), tenantRequestContext)
          .compose(onOrderItems -> {
            if (CollectionUtils.isEmpty(onOrderItems)) {
              return Future.succeededFuture();
            }
            List<String> itemIds = onOrderItems.stream().map(item -> item.getString(ID)).toList();
            return checkRequestsForItems(itemIds, tenantRequestContext);
          });
      })
      .toList();
    return HelperUtils.executeAllFailFast(futures);
  }

  private HashMap<String, List<String>> createTenantIdToPoLineIdsMap(List<PoLine> poLines) {
    HashMap<String, List<String>> tenantIdToPoLineIds = new HashMap<>();
    poLines.forEach(poLine -> {
      List<String> tenantIds = PoLineCommonUtil.getTenantsFromLocations(poLine);
      tenantIds.forEach(tenantId -> {
        List<String> poLineIds = tenantIdToPoLineIds.get(tenantId);
        if (poLineIds == null) {
          poLineIds = new ArrayList<>();
        }
        poLineIds.add(poLine.getId());
        tenantIdToPoLineIds.put(tenantId, poLineIds);
      });
    });
    return tenantIdToPoLineIds;
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
    return processPoLines(poLines, poLine -> purchaseOrderLineService.saveOrderLine(poLine, requestContext));
  }

  private Future<Void> processInventory(List<PoLine> poLines, boolean deleteHoldings, RequestContext requestContext) {
    return processPoLines(poLines, poLine -> processInventory(poLine, deleteHoldings, requestContext));
  }

  private Future<Void> processPoLines(List<PoLine> poLines, Function<PoLine, Future<?>> poLineProcessor) {
    return Future.join(poLines.stream().map(poLineProcessor::apply).toList()).mapEmpty();
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
      return deleteExpectedPieces(poLine, requestContext);
    }
    Future<Void> future = Future.succeededFuture();
    boolean itemsUpdateRequired = PoLineCommonUtil.isItemsUpdateRequired(poLine);
    boolean holdingsUpdateRequired = PoLineCommonUtil.isHoldingsUpdateRequired(poLine);
    boolean synchronizedWorkflow = !Boolean.TRUE.equals(poLine.getCheckinItems());
    if (itemsUpdateRequired && synchronizedWorkflow) {
      future = future.compose(v -> deleteItemsInAllTenants(poLine, requestContext));
    }
    if (itemsUpdateRequired || holdingsUpdateRequired) {
      if (synchronizedWorkflow || deleteHoldings) {
        future = future.compose(v -> deleteExpectedPieces(poLine, requestContext)).mapEmpty();
      }
      if (deleteHoldings) {
        future = future.compose(v -> deleteHoldingsAndUpdatePoLineLocations(poLine, requestContext));
      }
    }
    return future
      .onSuccess(v -> log.info("processInventory:: done processing inventory for po line id: {}", poLine.getId()))
      .onFailure(e -> log.error("processInventory:: failed to process inventory for po line id: {}", poLine.getId(), e));
  }

  private Future<Void> deleteItemsInAllTenants(PoLine poLine, RequestContext requestContext) {
    var futures = PoLineCommonUtil.getTenantsFromLocations(poLine).stream()
      .map(tenantId -> deleteItemsInOneTenant(poLine, createContextWithNewTenantId(requestContext, tenantId)))
      .toList();
    return Future.all(futures)
    .onSuccess(v -> log.info("deleteItemsInAllTenants:: items deleted for po line id: {}", poLine.getId()))
    .onFailure(t -> log.error("deleteItemsInAllTenants:: failed to delete items for po line id: {}", poLine.getId(), t))
    .mapEmpty();
  }

  private Future<?> deleteItemsInOneTenant(PoLine poLine, RequestContext requestContext) {
    return inventoryItemManager.getItemsByPoLineIdsAndStatus(List.of(poLine.getId()), ItemStatus.ON_ORDER.value(), requestContext)
      .compose(onOrderItems -> {
        if (CollectionUtils.isEmpty(onOrderItems)) {
          return Future.succeededFuture();
        }
        List<String> itemIds = onOrderItems.stream().map(item -> item.getString(ID)).toList();
        return inventoryItemManager.deleteItems(itemIds, false, requestContext);
      });
  }

  private Future<Void> checkRequestsForItems(List<String> itemIds, RequestContext requestContext) {
    return circulationRequestsRetriever.getNumbersOfRequestsByItemIds(itemIds, requestContext)
      .compose(idsAndCounts -> idsAndCounts.values().stream().anyMatch(count -> count > 0)
        ? Future.failedFuture(new HttpException(422, ErrorCodes.REQUEST_FOUND.toError()))
        : Future.succeededFuture());
  }

  private Future<Void> deleteExpectedPieces(PoLine poLine, RequestContext requestContext) {
    if (PoLineCommonUtil.isReceiptNotRequired(poLine.getReceiptStatus()) || Boolean.TRUE.equals(poLine.getCheckinItems())) {
      log.info("deleteExpectedPieces:: receipt is not required or independent receiving flow is used, skipping deleting pieces, poLineId: {}",
        poLine.getId());
      return Future.succeededFuture();
    }
    return pieceStorageService.getExpectedPiecesByLineId(poLine.getId(), requestContext)
      .compose(pieceCollection -> {
        List<Piece> pieces = pieceCollection.getPieces();
        if (CollectionUtils.isEmpty(pieces)) {
          return Future.succeededFuture();
        }
        return pieceStorageService.deletePiecesByIds(pieces.stream().map(Piece::getId).toList(), requestContext);
      })
      .onSuccess(v -> log.info("deleteExpectedPieces:: successfully deleted expected pieces for po line id: {}", poLine.getId()))
      .onFailure(t -> log.error("deleteExpectedPieces:: failed to delete expected pieces for po line id: {}", poLine.getId(), t));
  }

  private Future<Void> deleteHoldingsAndUpdatePoLineLocations(PoLine poLine, RequestContext requestContext) {
    log.info("deleteHoldingsAndUpdatePoLineLocations:: PoLine id: {}, payment status={}, receipt status={}",
      poLine.getId(), poLine.getPaymentStatus(), poLine.getReceiptStatus());
     var futuresOfHoldingIdLocationIdPairs = inventoryHoldingManager.getHoldingsByLocationTenants(poLine, requestContext)
       .entrySet().stream()
       .map(entry -> entry.getValue()
         .compose(holdings -> deleteHoldingsCheckLinkedData(poLine, entry, holdings, requestContext)))
       .toList();
    return Future.all(futuresOfHoldingIdLocationIdPairs)
      .map(ar -> {
        var holdingIdLocationIdPairs = futuresOfHoldingIdLocationIdPairs.stream()
          .map(Future::result)
          .flatMap(List::stream)
          .toList();
        updateLocations(poLine, holdingIdLocationIdPairs);
        return null;
      })
      .onSuccess(v -> log.info("deleteHoldingsAndUpdatePoLineLocations:: successfully deleted holdings for po line id: {}", poLine.getId()))
      .onFailure(t -> log.error("deleteHoldingsAndUpdatePoLineLocations:: failed to delete holdings for po line id: {}", poLine.getId(), t))
      .mapEmpty();
  }

  private Future<List<Pair<String, String>>> deleteHoldingsCheckLinkedData(PoLine poLine, Map.Entry<String, Future<List<JsonObject>>> entry,
                                                                           List<JsonObject> holdings, RequestContext requestContext) {
    var futures = new ArrayList<Future<Pair<String, String>>>();
    for (var holding : holdings) {
      var locationContext = createContextWithNewTenantId(requestContext, entry.getKey());
      var exclusionConfig = new HoldingDataExclusionConfig(HoldingDataExclusionMode.PURCHASE_ORDER_UNOPEN, Set.of(poLine.getId()), isPending(poLine));
      var future = holdingDeletionService.getHoldingLinkedData(holding, exclusionConfig, requestContext, locationContext)
        .compose(deletableHoldings -> holdingDeletionService.deleteHoldingIfPossible(deletableHoldings, locationContext));
      futures.add(future);
    }
    return collectResultsOnSuccess(futures)
      .map(this::filterHoldingAndLocationPairs);
  }

  private Set<String> isPending(PoLine poLine) {
    return poLine.getPaymentStatus() == PoLine.PaymentStatus.PENDING &&
      poLine.getReceiptStatus() == PoLine.ReceiptStatus.PENDING ? Set.of(poLine.getId()) : Set.of();
  }

  private List<Pair<String, String>> filterHoldingAndLocationPairs(List<Pair<String, String>> holdingAndLocationPairs) {
    var validPairs = holdingAndLocationPairs.stream()
      .filter(pair -> Objects.nonNull(pair) && Objects.nonNull(pair.getKey()))
      .toList();
    if (log.isDebugEnabled()) {
      log.debug("Holdings were removed: {}", map(validPairs, Pair::getKey));
    }
    return validPairs;
  }

  private void updateLocations(PoLine poLine, List<Pair<String, String>> deletedHoldingVsLocationIds) {
    if (CollectionUtils.isEmpty(deletedHoldingVsLocationIds)) {
      return;
    }
    Map<String, List<Location>> holdingIdVsLocations = StreamEx.of(poLine.getLocations())
      .filter(entity -> Objects.nonNull(entity.getHoldingId()))
      .groupingBy(Location::getHoldingId);
    deletedHoldingVsLocationIds.forEach(holdingIdVsLocationId -> {
      String holdingId = holdingIdVsLocationId.getKey();
      String locationId = holdingIdVsLocationId.getValue();
      holdingIdVsLocations.getOrDefault(holdingId, List.of())
        .forEach(location -> location.withHoldingId(null).withLocationId(locationId));
    });
  }
}
