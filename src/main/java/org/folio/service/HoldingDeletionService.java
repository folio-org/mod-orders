package org.folio.service;

import io.vertx.core.CompositeFuture;
import io.vertx.core.Future;
import io.vertx.core.json.JsonObject;
import lombok.RequiredArgsConstructor;
import lombok.extern.log4j.Log4j2;
import org.apache.commons.collections4.CollectionUtils;
import org.apache.commons.lang3.tuple.Pair;
import org.folio.models.HoldingDataExclusionConfig;
import org.folio.rest.core.models.RequestContext;
import org.folio.rest.jaxrs.model.Piece;
import org.folio.rest.jaxrs.model.PoLine;
import org.folio.rest.tools.utils.TenantTool;
import org.folio.service.consortium.ConsortiumUserTenantService;
import org.folio.service.inventory.InventoryHoldingManager;
import org.folio.service.inventory.InventoryItemManager;
import org.folio.service.orders.PurchaseOrderLineService;
import org.folio.service.pieces.PieceStorageService;

import java.util.ArrayList;
import java.util.List;
import java.util.Objects;
import java.util.Set;
import java.util.stream.Collectors;

import static org.folio.orders.utils.FutureUtils.asFuture;
import static org.folio.orders.utils.FutureUtils.unwrap;
import static org.folio.orders.utils.RequestContextUtil.createContextWithNewTenantId;
import static org.folio.service.inventory.InventoryHoldingManager.HOLDING_PERMANENT_LOCATION_ID;
import static org.folio.service.inventory.InventoryHoldingManager.ID;

@Log4j2
@RequiredArgsConstructor
public class HoldingDeletionService {

  private final ConsortiumUserTenantService consortiumUserTenantService;
  private final InventoryHoldingManager inventoryHoldingManager;
  private final InventoryItemManager inventoryItemManager;
  private final PieceStorageService pieceStorageService;
  private final PurchaseOrderLineService purchaseOrderLineService;

  public Future<Pair<Boolean, JsonObject>> getHoldingLinkedData(JsonObject holding,
                                                                HoldingDataExclusionConfig exclusionConfig,
                                                                RequestContext requestContext,
                                                                RequestContext locationContext) {
    if (Objects.isNull(holding) || holding.isEmpty()) {
      return Future.succeededFuture(Pair.of(false, new JsonObject()));
    }

    var holdingId = holding.getString(ID);
    return consortiumUserTenantService.getUserTenantsIfNeeded(requestContext)
      .compose(userTenants -> prepareLinkedDataFutures(holdingId, userTenants, requestContext, locationContext))
      .compose(linkedData -> {
        var linkedPoLines = linkedData.<List<PoLine>>resultAt(0);
        var linkedPieces = linkedData.<List<Piece>>resultAt(1);
        var linkedItems = linkedData.<List<JsonObject>>resultAt(2);

        var excludeItemIds = linkedPieces.stream()
          .filter(piece -> exclusionConfig.excludePieceIds().contains(piece.getId()))
          .map(Piece::getItemId)
          .filter(Objects::nonNull)
          .collect(Collectors.toSet());

        var filteredPoLines = excludePoLines(linkedPoLines, exclusionConfig);
        var filteredPieces = excludePieces(linkedPieces, exclusionConfig.excludePieceIds());
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

  private CompositeFuture prepareLinkedDataFutures(String holdingId,
                                                   List<String> userTenants,
                                                   RequestContext requestContext,
                                                   RequestContext locationContext) {
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

  private List<PoLine> excludePoLines(List<PoLine> poLines, HoldingDataExclusionConfig exclusionConfig) {
    var finalPoLines = poLines.stream()
      .filter(poLine -> switch (exclusionConfig.mode()) {
        case PIECE_RECEIVING ->
          poLine.getCheckinItems() || (!poLine.getCheckinItems() && !exclusionConfig.excludePoLinesIds().contains(poLine.getId()));
        case PURCHASE_ORDER_UNOPEN ->
          (poLine.getCheckinItems() || (!poLine.getCheckinItems() && !exclusionConfig.excludePoLinesIds().contains(poLine.getId())))
          && !exclusionConfig.pendingPoLineIds().contains(poLine.getId());
        case PO_LINE_CHANGE_INSTANCE ->
          !exclusionConfig.excludePoLinesIds().contains(poLine.getId());
      })
      .toList();
    log.info("excludePoLines:: PoLines before exclusion={}, ids to exclude={}, final poLines={}",
      poLines.stream().map(PoLine::getId).toList(), exclusionConfig.excludePoLinesIds(), finalPoLines.stream().map(PoLine::getId).toList());
    return finalPoLines;
  }

  private List<Piece> excludePieces(List<Piece> pieces, Set<String> excludePieceIds) {
    var finalPieces = pieces.stream()
      .filter(piece -> !excludePieceIds.contains(piece.getId()))
      .toList();
    log.info("excludePieces:: Pieces before exclusion={}, ids to exclude={}, final pieces={}",
      pieces.stream().map(Piece::getId).toList(), excludePieceIds, finalPieces.stream().map(Piece::getId).toList());
    return finalPieces;
  }

  private List<JsonObject> excludeItems(List<JsonObject> items, Set<String> excludeItemIds) {
    var finalItems = items.stream()
      .filter(item -> !excludeItemIds.contains(item.getString(ID)))
      .toList();
    log.info("excludeItems:: Items before exclusion={}, ids to exclude={}, final items={}",
      items.stream().map(i -> i.getString(ID)).toList(), excludeItemIds,
      finalItems.stream().map(i -> i.getString(ID)).toList());
    return finalItems;
  }

  public Future<Pair<String, String>> deleteHoldingIfPossible(Pair<Boolean, JsonObject> deletableHoldings,
                                                              RequestContext locationContext) {
    var isDeletable = deletableHoldings.getKey();
    var holding = deletableHoldings.getValue();
    var holdingId = holding.getString(ID);
    log.info("deleteHoldingIfPossible:: isDeletable={}, holdingId={}", isDeletable, holdingId);
    if (Boolean.TRUE.equals(isDeletable) && !holding.isEmpty()) {
      return inventoryHoldingManager.deleteHoldingById(holdingId, true, locationContext)
        .map(v -> Pair.of(holdingId, holding.getString(HOLDING_PERMANENT_LOCATION_ID)));
    }
    return Future.succeededFuture();
  }
}
