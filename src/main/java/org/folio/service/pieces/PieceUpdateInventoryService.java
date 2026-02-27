package org.folio.service.pieces;

import static org.folio.orders.utils.HelperUtils.collectResultsOnSuccess;
import static org.folio.orders.utils.RequestContextUtil.createContextWithNewTenantId;
import static org.folio.service.inventory.InventoryHoldingManager.HOLDING_PERMANENT_LOCATION_ID;
import static org.folio.service.inventory.InventoryHoldingManager.ID;

import java.util.List;
import java.util.Map;
import java.util.Optional;
import java.util.Set;
import java.util.stream.Collectors;

import lombok.RequiredArgsConstructor;
import lombok.extern.log4j.Log4j2;
import org.apache.commons.collections4.CollectionUtils;
import org.apache.commons.lang3.tuple.Pair;
import org.folio.rest.core.models.RequestContext;
import org.folio.rest.jaxrs.model.PoLine;
import org.folio.rest.jaxrs.model.CompositePurchaseOrder;
import org.folio.rest.jaxrs.model.Piece;
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
    return deleteHoldingsConnectedToPieces(holdingIdsToPieces, poLinesToSkip, Set.of(), requestContext);
  }

  public Future<List<Pair<String, String>>> deleteHoldingsConnectedToPieces(Map<Pair<String, String>,
                                                                            Set<String>> holdingIdsToPieces,
                                                                            Set<String> processedHoldingIds,
                                                                            Set<String> poLinesToSkip, RequestContext requestContext) {
    log.info("deleteHoldingsConnectedToPieces:: Excluding processed holdingIds={}", processedHoldingIds);

    var futures = holdingIdsToPieces.entrySet().stream()
      .filter(entry -> !processedHoldingIds.contains(entry.getKey().getKey()))
      .map(entry -> {
        var holdingId = entry.getKey().getKey();
        var receivingTenantId = entry.getKey().getValue();
        var pieceIdsToSkip = entry.getValue();
        var locationContext = createContextWithNewTenantId(requestContext, receivingTenantId);
        return inventoryHoldingManager.getHoldingById(holdingId, true, locationContext)
          .compose(holding -> getHoldingLinkedData(holding, holdingId, poLinesToSkip, pieceIdsToSkip, requestContext, locationContext))
          .compose(deletableHoldings -> deleteHoldingIfPossible(deletableHoldings, holdingId, locationContext));
      }).toList();
    return collectResultsOnSuccess(futures);
  }

  private Future<Pair<Boolean, JsonObject>> getHoldingLinkedData(JsonObject holding, String holdingId, Set<String> poLinesToSkip,
                                                                 Set<String> pieceIdsToSkip, RequestContext requestContext, RequestContext locationContext) {
    if (holding == null || holding.isEmpty()) {
      return Future.succeededFuture(Pair.of(false, new JsonObject()));
    }

    // With ECS look up in central tenant
    var poLinesFuture = purchaseOrderLineService.getPoLinesByHoldingIds(List.of(holdingId), requestContext);
    var piecesFuture = pieceStorageService.getPiecesByHoldingId(holdingId, requestContext);
    // With ECS look up in member tenants
    var itemsFuture = inventoryItemManager.getItemsByHoldingId(holdingId, locationContext);
    return Future.all(poLinesFuture, piecesFuture, itemsFuture)
      .map(linkedData -> {
        var linkedPoLines = linkedData.<List<PoLine>>resultAt(0);
        var linkedPieces = linkedData.<List<Piece>>resultAt(1);
        var linkedItems = linkedData.<List<JsonObject>>resultAt(2);

        var itemIdsToSkip = linkedPieces.stream()
          .filter(piece -> pieceIdsToSkip.contains(piece.getId()))
          .map(Piece::getItemId)
          .collect(Collectors.toSet());

        // Excludes linked entities that are not affected
        var filteredPoLines = excludePoLines(linkedPoLines, poLinesToSkip);
        var filteredPieces = excludePieces(linkedPieces, pieceIdsToSkip);
        var filteredItems = excludeItems(linkedItems, itemIdsToSkip);

        var isDeletable = CollectionUtils.isEmpty(filteredPoLines)
          && CollectionUtils.isEmpty(filteredPieces)
          && CollectionUtils.isEmpty(linkedItems);

        log.info("getUpdatePossibleForHolding:: holdingId={}, filteredPoLines={}, filteredPieces={}, filteredItems={}, isDeletable={}",
          holdingId, filteredPoLines.size(), filteredPieces.size(), filteredItems.size(), isDeletable);

        return isDeletable ? Pair.of(true, holding) : Pair.of(false, new JsonObject());
      });
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

  private List<PoLine> excludePoLines(List<PoLine> poLines, Set<String> poLinesIdsToSkip) {
    log.info("excludePoLines:: PoLines before exclusion={}, ids to exclude={}", poLines, poLinesIdsToSkip);
    return poLines.stream()
      .filter(poLine -> !poLinesIdsToSkip.contains(poLine.getId()))
      .toList();
  }

  private List<Piece> excludePieces(List<Piece> pieces, Set<String> pieceIdsToSkip) {
    log.info("excludePieces:: Pieces before exclusion={}, ids to exclude={}", pieces, pieceIdsToSkip);
    return pieces.stream()
      .filter(piece -> !pieceIdsToSkip.contains(piece.getId()))
      .toList();
  }

  private List<JsonObject> excludeItems(List<JsonObject> items, Set<String> itemIdsToSkip) {
    log.info("excludeItems:: Items before exclusion={}, ids to exclude={}", items, itemIdsToSkip);
    return items.stream()
      .filter(item -> !itemIdsToSkip.contains(item.getString(ID)))
      .toList();
  }
}
