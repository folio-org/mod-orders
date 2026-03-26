package org.folio.service.pieces;

import static org.folio.orders.utils.HelperUtils.collectResultsOnSuccess;
import static org.folio.orders.utils.RequestContextUtil.createContextWithNewTenantId;

import java.util.List;
import java.util.Map;
import java.util.Optional;
import java.util.Set;
import java.util.stream.Collectors;

import lombok.RequiredArgsConstructor;
import lombok.extern.log4j.Log4j2;
import org.apache.commons.lang3.tuple.Pair;
import org.folio.models.HoldingDataExclusionConfig;
import org.folio.models.HoldingDataExclusionMode;
import org.folio.rest.core.models.RequestContext;
import org.folio.rest.jaxrs.model.PoLine;
import org.folio.rest.jaxrs.model.CompositePurchaseOrder;
import org.folio.rest.jaxrs.model.Piece;
import org.folio.service.HoldingDeletionService;
import org.folio.service.inventory.InventoryHoldingManager;
import org.folio.service.inventory.InventoryItemManager;
import org.folio.service.pieces.flows.DefaultPieceFlowsValidator;

import io.vertx.core.Future;

@Log4j2
@RequiredArgsConstructor
public class PieceUpdateInventoryService {

  private static final int ITEM_QUANTITY = 1;

  private final InventoryHoldingManager inventoryHoldingManager;
  private final InventoryItemManager inventoryItemManager;
  private final HoldingDeletionService holdingDeletionService;

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
        var excludePieceIds = entry.getValue();
        var locationContext = createContextWithNewTenantId(requestContext, entry.getKey().getValue());
        var exclusionConfig = new HoldingDataExclusionConfig(HoldingDataExclusionMode.PIECE_RECEIVING, excludePoLineIds, excludePieceIds);
        return inventoryHoldingManager.getHoldingById(holdingId, true, locationContext)
          .compose(holding -> holdingDeletionService.getHoldingLinkedData(holding, exclusionConfig, requestContext, locationContext))
          .compose(deletableHoldings -> holdingDeletionService.deleteHoldingIfPossible(deletableHoldings, locationContext));
      }).toList();
    return collectResultsOnSuccess(futures);
  }
}
