package org.folio.service.pieces;

import static org.folio.orders.utils.FutureUtils.asFuture;
import static org.folio.orders.utils.FutureUtils.unwrap;
import static org.folio.orders.utils.HelperUtils.collectResultsOnSuccess;
import static org.folio.orders.utils.RequestContextUtil.createContextWithNewTenantId;
import static org.folio.service.inventory.InventoryHoldingManager.HOLDING_PERMANENT_LOCATION_ID;
import static org.folio.service.inventory.InventoryHoldingManager.ID;

import java.util.ArrayList;
import java.util.List;
import java.util.Map;
import java.util.Objects;
import java.util.Optional;
import java.util.Set;
import java.util.stream.Collectors;

import io.vertx.core.CompositeFuture;
import lombok.RequiredArgsConstructor;
import lombok.extern.log4j.Log4j2;
import org.apache.commons.collections4.CollectionUtils;
import org.apache.commons.lang3.tuple.Pair;
import org.folio.rest.core.models.RequestContext;
import org.folio.rest.jaxrs.model.PoLine;
import org.folio.rest.jaxrs.model.CompositePurchaseOrder;
import org.folio.rest.jaxrs.model.Piece;
import org.folio.rest.tools.utils.TenantTool;
import org.folio.service.consortium.ConsortiumUserTenantService;
import org.folio.service.inventory.InventoryHoldingManager;
import org.folio.service.inventory.InventoryItemManager;
import org.folio.service.orders.PurchaseOrderLineService;
import org.folio.service.pieces.flows.DefaultPieceFlowsValidator;

import io.vertx.core.Future;
import io.vertx.core.json.JsonObject;

@Log4j2
@RequiredArgsConstructor
public class PieceUpdateInventoryService {

  private static final int ITEM_QUANTITY = 1;

  private final ConsortiumUserTenantService consortiumUserTenantService;
  private final InventoryItemManager inventoryItemManager;
  private final InventoryHoldingManager inventoryHoldingManager;
  private final PieceStorageService pieceStorageService;
  private final PurchaseOrderLineService purchaseOrderLineService;

  /**
   * Return id of created  Item
   */
  public Future<String> manualPieceFlowCreateItemRecord(Piece piece, CompositePurchaseOrder compPO,
                                                        PoLine poLine, RequestContext requestContext) {
    log.debug("manualPieceFlowCreateItemRecord:: Handling {} items for PO Line and holdings with id={}, receivingTenantId={}",
      ITEM_QUANTITY, piece.getHoldingId(), piece.getReceivingTenantId());
    if (piece.getFormat() == Piece.Format.ELECTRONIC && DefaultPieceFlowsValidator.isCreateItemForElectronicPiecePossible(piece, poLine)) {
      return inventoryItemManager.createMissingElectronicItems(compPO, poLine, piece, ITEM_QUANTITY, requestContext)
        .map(List::getFirst);
    } else if (DefaultPieceFlowsValidator.isCreateItemForNonElectronicPiecePossible(piece, poLine)) {
      return inventoryItemManager.createMissingPhysicalItems(compPO, poLine, piece, ITEM_QUANTITY, requestContext)
        .map(List::getFirst);
    } else {
      log.warn("manualPieceFlowCreateItemRecord:: Creating Item is not possible for piece: {}, poLine: {}",
        piece.getId(), poLine.getId());
      return Future.succeededFuture();
    }
  }

  public Future<Pair<String, String>> deleteHoldingConnectedToPiece(Piece piece, RequestContext requestContext) {
    var pieces = Optional.ofNullable(piece)
      .map(List::of)
      .orElseGet(List::of);
    return deleteHoldingsConnectedToPieces(pieces, requestContext)
      .compose(result -> Optional.ofNullable(result)
        .map(List::getFirst)
        .map(Future::succeededFuture)
        .orElseGet(Future::succeededFuture));
  }

  public Future<List<Pair<String, String>>> deleteHoldingsConnectedToPieces(List<Piece> pieces, RequestContext requestContext) {
    var poLinesToSkip = pieces.stream()
      .map(Piece::getPoLineId)
      .collect(Collectors.toSet());
    var holdingIdsToPieces = PieceUtil.groupPiecesByHoldings(pieces);
    return deleteHoldingsConnectedToPieces(holdingIdsToPieces, Set.of(), poLinesToSkip, requestContext);
  }

  public Future<List<Pair<String, String>>> deleteHoldingsConnectedToPieces(Map<Pair<String, String>, Set<String>> pieceByHoldingIds,
                                                                            Set<String> processedHoldingIds,
                                                                            Set<String> excludePoLineIds,
                                                                            RequestContext requestContext) {
    var holdingIds = pieceByHoldingIds.keySet().stream().map(Pair::getKey).toList();
    log.info("deleteHoldingsConnectedToPieces:: Holdings before exclusion={}, ids to exclude={}", holdingIds, processedHoldingIds);

    var futures = pieceByHoldingIds.entrySet().stream()
      .filter(entry -> !processedHoldingIds.contains(entry.getKey().getKey()))
      .map(entry -> {
        var holdingId = entry.getKey().getKey();
        var receivingTenantId = entry.getKey().getValue();
        var excludePieceIds = entry.getValue();
        var locationContext = createContextWithNewTenantId(requestContext, receivingTenantId);
        return inventoryHoldingManager.getHoldingById(holdingId, true, locationContext)
          .compose(holding -> getHoldingLinkedData(holding, holdingId, excludePoLineIds, excludePieceIds, requestContext, locationContext))
          .compose(deletableHoldings -> deleteHoldingIfPossible(deletableHoldings, holdingId, locationContext));
      }).toList();
    return collectResultsOnSuccess(futures);
  }

  private Future<Pair<Boolean, JsonObject>> getHoldingLinkedData(JsonObject holding, String holdingId,
                                                                 Set<String> excludePoLineIds, Set<String> excludePieceIds,
                                                                 RequestContext requestContext, RequestContext locationContext) {
    if (Objects.isNull(holding) || holding.isEmpty()) {
      return Future.succeededFuture(Pair.of(false, new JsonObject()));
    }

    return consortiumUserTenantService.getUserTenantsIfNeeded(requestContext)
      .compose(userTenants -> prepareLinkedDataFutures(holdingId, requestContext, locationContext, userTenants))
      .compose(linkedData -> {
        var linkedPoLines = linkedData.<List<PoLine>>resultAt(0);
        var linkedPieces = linkedData.<List<Piece>>resultAt(1);
        var linkedItems = linkedData.<List<JsonObject>>resultAt(2);

        var excludeItemIds = linkedPieces.stream()
          .filter(piece -> excludePieceIds.contains(piece.getId()))
          .map(Piece::getItemId)
          .filter(Objects::nonNull)
          .collect(Collectors.toSet());

        var filteredPoLines = excludePoLines(linkedPoLines, excludePoLineIds);
        var filteredPieces = excludePieces(linkedPieces, excludePieceIds);
        var filteredItems = excludeItems(linkedItems, excludeItemIds);

        var isDeletable = CollectionUtils.isEmpty(filteredPoLines)
          && CollectionUtils.isEmpty(filteredPieces)
          && CollectionUtils.isEmpty(filteredItems);

        log.info("getUpdatePossibleForHolding:: holdingId={}, linkedPoLines={}, linkedPieces={}, linkedItems={}, isDeletable={}",
          holdingId, filteredPoLines.size(), filteredPieces.size(), filteredItems.size(), isDeletable);

        return asFuture(() ->
          isDeletable ? Pair.of(true, holding) : Pair.of(false, new JsonObject()));
      });
  }

  private CompositeFuture prepareLinkedDataFutures(String holdingId, RequestContext requestContext,
                                                   RequestContext locationContext, List<String> userTenants) {
    var poLineFutures = new ArrayList<Future<List<PoLine>>>();
    var pieceFutures = new ArrayList<Future<List<Piece>>>();
    if (CollectionUtils.isNotEmpty(userTenants)) {
      userTenants.forEach(tenantId -> {
        log.info("getHoldingLinkedData:: Searching for linked poLines and pieces across tenants in tenant={} by holding id={}",
          tenantId, holdingId);
        // Search for other extraneous PoLines and Pieces that may be linked with the holdingId
        var memberRequestContext = createContextWithNewTenantId(requestContext, tenantId);
        poLineFutures.add(purchaseOrderLineService.getPoLinesByHoldingIds(List.of(holdingId), memberRequestContext));
        pieceFutures.add(pieceStorageService.getPiecesByHoldingId(holdingId, memberRequestContext));
      });
    } else {
      var tenantId = TenantTool.tenantId(requestContext.getHeaders());
      log.info("getHoldingLinkedData:: Searching for linked poLines and pieces in tenant={} by holding id={}",
        tenantId, holdingId);
      poLineFutures.add(purchaseOrderLineService.getPoLinesByHoldingIds(List.of(holdingId), requestContext));
      pieceFutures.add(pieceStorageService.getPiecesByHoldingId(holdingId, requestContext));
    }
    // Unwrap futures by flattening the list of futures
    var combinedPoLineFutures = unwrap(poLineFutures);
    var combinedPieceFutures = unwrap(pieceFutures);
    // Search for all Items by a holdingId using the receivingTenantId on the Piece
    var itemsFuture = inventoryItemManager.getItemsByHoldingId(holdingId, locationContext);

    return Future.all(combinedPoLineFutures, combinedPieceFutures, itemsFuture);
  }

  private Future<Pair<String, String>> deleteHoldingIfPossible(Pair<Boolean, JsonObject> deletableHoldings,
                                                               String holdingId, RequestContext locationContext) {
    var isDeletable = deletableHoldings.getKey();
    var holding = deletableHoldings.getValue();
    log.info("deleteHoldingIfPossible:: isDeletable={}, holdingId={}", isDeletable, holdingId);
    if (Boolean.TRUE.equals(isDeletable) && !holding.isEmpty()) {
      return inventoryHoldingManager.deleteHoldingById(holdingId, true, locationContext)
        .map(v -> Pair.of(holdingId, holding.getString(HOLDING_PERMANENT_LOCATION_ID)));
    }
    return Future.succeededFuture();
  }

  private List<PoLine> excludePoLines(List<PoLine> poLines, Set<String> excludePoLinesIds) {
    log.info("excludePoLines:: PoLines before exclusion={}, ids to exclude={}",
      poLines.stream().map(PoLine::getId).toList(), excludePoLinesIds);
    // Perform filtering only if the PoLine is synchronized
    return poLines.stream()
      .filter(poLine -> poLine.getCheckinItems() || (!poLine.getCheckinItems() && !excludePoLinesIds.contains(poLine.getId())))
      .toList();
  }

  private List<Piece> excludePieces(List<Piece> pieces, Set<String> excludePieceIds) {
    log.info("excludePieces:: Pieces before exclusion={}, ids to exclude={}",
      pieces.stream().map(Piece::getId).toList(), excludePieceIds);
    return pieces.stream()
      .filter(piece -> !excludePieceIds.contains(piece.getId()))
      .toList();
  }

  private List<JsonObject> excludeItems(List<JsonObject> items, Set<String> excludeItemIds) {
    log.info("excludeItems:: Items before exclusion={}, ids to exclude={}",
      items.stream().map(i -> i.getString(ID)).toList(), excludeItemIds);
    return items.stream()
      .filter(item -> !excludeItemIds.contains(item.getString(ID)))
      .toList();
  }
}
