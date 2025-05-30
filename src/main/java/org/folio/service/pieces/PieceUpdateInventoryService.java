package org.folio.service.pieces;

import static org.folio.orders.utils.RequestContextUtil.createContextWithNewTenantId;
import static org.folio.service.inventory.InventoryHoldingManager.HOLDING_PERMANENT_LOCATION_ID;

import java.util.List;
import java.util.stream.Collectors;

import org.apache.commons.collections4.CollectionUtils;
import org.apache.commons.lang3.tuple.Pair;
import org.apache.logging.log4j.LogManager;
import org.apache.logging.log4j.Logger;
import org.folio.rest.core.models.RequestContext;
import org.folio.rest.jaxrs.model.PoLine;
import org.folio.rest.jaxrs.model.CompositePurchaseOrder;
import org.folio.rest.jaxrs.model.Piece;
import org.folio.service.inventory.InventoryHoldingManager;
import org.folio.service.inventory.InventoryItemManager;
import org.folio.service.pieces.flows.DefaultPieceFlowsValidator;

import io.vertx.core.Future;
import io.vertx.core.json.JsonObject;

public class PieceUpdateInventoryService {
  private static final Logger logger = LogManager.getLogger(PieceUpdateInventoryService.class);

  private final InventoryItemManager inventoryItemManager;
  private final InventoryHoldingManager inventoryHoldingManager;
  private final PieceStorageService pieceStorageService;

  public PieceUpdateInventoryService(InventoryItemManager inventoryItemManager,
                                     InventoryHoldingManager inventoryHoldingManager,
                                     PieceStorageService pieceStorageService) {
    this.inventoryItemManager = inventoryItemManager;
    this.inventoryHoldingManager = inventoryHoldingManager;
    this.pieceStorageService = pieceStorageService;
  }

  /**
   * Return id of created  Item
   */
  public Future<String> manualPieceFlowCreateItemRecord(Piece piece, CompositePurchaseOrder compPO,
                                                        PoLine poLine, RequestContext requestContext) {
    final int ITEM_QUANTITY = 1;
    logger.debug("manualPieceFlowCreateItemRecord:: Handling {} items for PO Line and holdings with id={}, receivingTenantId={}",
      ITEM_QUANTITY, piece.getHoldingId(), piece.getReceivingTenantId());
    if (piece.getFormat() == Piece.Format.ELECTRONIC && DefaultPieceFlowsValidator.isCreateItemForElectronicPiecePossible(piece, poLine)) {
      return inventoryItemManager.createMissingElectronicItems(compPO, poLine, piece, ITEM_QUANTITY, requestContext)
        .map(idS -> idS.get(0));
    } else if (DefaultPieceFlowsValidator.isCreateItemForNonElectronicPiecePossible(piece, poLine)) {
      return inventoryItemManager.createMissingPhysicalItems(compPO, poLine, piece, ITEM_QUANTITY, requestContext)
        .map(idS -> idS.get(0));
    } else {
      logger.warn("manualPieceFlowCreateItemRecord:: Creating Item is not possible for piece: {}, poLine: {}",
        piece.getId(), poLine.getId());
      return Future.succeededFuture();
    }
  }

  public Future<Pair<String, String>> deleteHoldingConnectedToPiece(Piece piece, RequestContext requestContext) {
      if (piece == null || piece.getHoldingId() == null) {
          return Future.succeededFuture();
      }
      var locationContext = createContextWithNewTenantId(requestContext, piece.getReceivingTenantId());
      String holdingId = piece.getHoldingId();
      return inventoryHoldingManager.getHoldingById(holdingId, true, locationContext)
        .compose(holding -> getUpdatePossibleForHolding(holding, holdingId, piece, locationContext, requestContext))
        .compose(isUpdatePossibleVsHolding -> deleteHoldingIfPossible(isUpdatePossibleVsHolding, holdingId, locationContext));
  }

  private Future<Pair<Boolean, JsonObject>> getUpdatePossibleForHolding(JsonObject holding, String holdingId, Piece piece,
                                                                        RequestContext locationContext, RequestContext requestContext) {
    if (holding == null || holding.isEmpty()) {
      return Future.succeededFuture(Pair.of(false, new JsonObject()));
    }
    return pieceStorageService.getPiecesByHoldingId(holdingId, requestContext)
      .map(pieces -> skipPieceToProcess(piece, pieces))
      .compose(existingPieces -> inventoryItemManager.getItemsByHoldingId(holdingId, locationContext)
        .map(existingItems-> {
          List<Piece> remainingPieces = skipPieceToProcess(piece, existingPieces);
          if (CollectionUtils.isEmpty(remainingPieces) && CollectionUtils.isEmpty(existingItems)) {
            return Pair.of(true, holding);
          }
          return Pair.of(false, new JsonObject());
        })
      );
  }

  private Future<Pair<String, String>> deleteHoldingIfPossible(Pair<Boolean, JsonObject> isUpdatePossibleVsHolding,
                                                               String holdingId, RequestContext locationContext) {
    var isUpdatePossible = isUpdatePossibleVsHolding.getKey();
    var holding = isUpdatePossibleVsHolding.getValue();
    if (isUpdatePossible && !holding.isEmpty()) {
      String permanentLocationId = holding.getString(HOLDING_PERMANENT_LOCATION_ID);
      return inventoryHoldingManager.deleteHoldingById(holdingId, true, locationContext)
        .map(v -> Pair.of(holdingId, permanentLocationId));
    }
    return Future.succeededFuture();
  }

  private List<Piece> skipPieceToProcess(Piece piece, List<Piece> pieces) {
    return pieces.stream().filter(aPiece -> !aPiece.getId().equals(piece.getId())).collect(
      Collectors.toList());
  }
}
