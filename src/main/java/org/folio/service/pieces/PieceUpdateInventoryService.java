package org.folio.service.pieces;

import static org.folio.orders.utils.HelperUtils.collectResultsOnSuccess;
import static org.folio.orders.utils.RequestContextUtil.createContextWithNewTenantId;
import static org.folio.service.inventory.InventoryHoldingManager.HOLDING_PERMANENT_LOCATION_ID;

import java.util.List;
import java.util.Optional;
import java.util.Set;
import java.util.stream.Collectors;

import lombok.RequiredArgsConstructor;
import lombok.extern.log4j.Log4j2;
import one.util.streamex.StreamEx;
import org.apache.commons.collections4.CollectionUtils;
import org.apache.commons.lang3.tuple.Pair;
import org.folio.rest.core.models.RequestContext;
import org.folio.rest.jaxrs.model.PoLine;
import org.folio.rest.jaxrs.model.CompositePurchaseOrder;
import org.folio.rest.jaxrs.model.Piece;
import org.folio.service.inventory.InventoryHoldingManager;
import org.folio.service.inventory.InventoryItemManager;
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
    return deleteHoldingsConnectedToPieces(Optional.ofNullable(piece).map(List::of).orElseGet(List::of), requestContext)
      .compose(result -> Optional.ofNullable(result).map(List::getFirst).map(Future::succeededFuture).orElseGet(Future::succeededFuture));
  }

  public Future<List<Pair<String, String>>> deleteHoldingsConnectedToPieces(List<Piece> pieces, RequestContext requestContext) {
    var viablePieces = StreamEx.of(pieces)
      .filter(piece -> piece != null && piece.getHoldingId() != null)
      .toList();
    return viablePieces.isEmpty()
      ? Future.succeededFuture()
      : collectResultsOnSuccess(doDeleteHolding(viablePieces, requestContext));
  }

  private List<Future<Pair<String, String>>> doDeleteHolding(List<Piece> pieces, RequestContext requestContext) {
    return StreamEx.of(pieces)
      .groupingBy(piece -> Pair.of(piece.getHoldingId(), piece.getReceivingTenantId()), Collectors.mapping(Piece::getId, Collectors.toSet()))
      .entrySet().stream()
      .map(entry -> {
        var holdingId = entry.getKey().getKey();
        var receivingTenantId = entry.getKey().getValue();
        var pieceIds = entry.getValue();
        var locationContext = createContextWithNewTenantId(requestContext, receivingTenantId);
        return inventoryHoldingManager.getHoldingById(holdingId, true, locationContext)
          .compose(holding -> getUpdatePossibleForHolding(holding, holdingId, pieceIds, locationContext, requestContext))
          .compose(isUpdatePossibleVsHolding -> deleteHoldingIfPossible(isUpdatePossibleVsHolding, holdingId, locationContext));
      }).toList();
  }

  private Future<Pair<Boolean, JsonObject>> getUpdatePossibleForHolding(JsonObject holding, String holdingId, Set<String> pieceIds,
                                                                        RequestContext locationContext, RequestContext requestContext) {
    if (holding == null || holding.isEmpty()) {
      return Future.succeededFuture(Pair.of(false, new JsonObject()));
    }
    return pieceStorageService.getPiecesByHoldingId(holdingId, requestContext)
      .compose(pieces -> inventoryItemManager.getItemsByHoldingId(holdingId, locationContext)
        .map(items -> CollectionUtils.isEmpty(filterPiecesToProcess(pieceIds, pieces)) && CollectionUtils.isEmpty(items)
          ? Pair.of(true, holding)
          : Pair.of(false, new JsonObject()))
      );
  }

  private Future<Pair<String, String>> deleteHoldingIfPossible(Pair<Boolean, JsonObject> isUpdatePossibleVsHolding,
                                                               String holdingId, RequestContext locationContext) {
    var isUpdatePossible = isUpdatePossibleVsHolding.getKey();
    var holding = isUpdatePossibleVsHolding.getValue();
    if (isUpdatePossible && !holding.isEmpty()) {
      return inventoryHoldingManager.deleteHoldingById(holdingId, true, locationContext)
        .map(v -> Pair.of(holdingId, holding.getString(HOLDING_PERMANENT_LOCATION_ID)));
    }
    return Future.succeededFuture();
  }

  private List<Piece> filterPiecesToProcess(Set<String> pieceIdsToSkip, List<Piece> pieces) {
    return pieces.stream()
      .filter(piece -> !pieceIdsToSkip.contains(piece.getId()))
      .toList();
  }

}
