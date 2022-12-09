package org.folio.service.pieces;

import static org.folio.service.inventory.InventoryManager.HOLDING_PERMANENT_LOCATION_ID;

import java.util.List;
import java.util.stream.Collectors;

import org.apache.commons.collections4.CollectionUtils;
import org.apache.commons.lang3.tuple.Pair;
import org.apache.logging.log4j.LogManager;
import org.apache.logging.log4j.Logger;
import org.folio.orders.utils.PoLineCommonUtil;
import org.folio.rest.core.models.RequestContext;
import org.folio.rest.jaxrs.model.CompositePoLine;
import org.folio.rest.jaxrs.model.Location;
import org.folio.rest.jaxrs.model.Piece;
import org.folio.service.inventory.InventoryManager;
import org.folio.service.pieces.flows.DefaultPieceFlowsValidator;

import io.vertx.core.Future;
import io.vertx.core.Promise;
import io.vertx.core.json.JsonObject;

public class PieceUpdateInventoryService {
  private static final Logger logger = LogManager.getLogger(PieceUpdateInventoryService.class);

  private final InventoryManager inventoryManager;
  private final PieceStorageService pieceStorageService;

  public PieceUpdateInventoryService(InventoryManager inventoryManager, PieceStorageService pieceStorageService) {
    this.inventoryManager = inventoryManager;
    this.pieceStorageService = pieceStorageService;
  }

  public Future<String> handleHoldingsRecord(final CompositePoLine compPOL, Location location, String instanceId,
    RequestContext requestContext) {
    try {
      if (PoLineCommonUtil.isHoldingsUpdateRequired(compPOL)) {
        return inventoryManager.getOrCreateHoldingsRecord(instanceId, location, requestContext);
      } else {
        return Future.succeededFuture();
      }
    }
    catch (Exception e) {
      return Future.failedFuture(e);
    }
  }

  /**
   * Return id of created  Item
   */
  public Future<String> manualPieceFlowCreateItemRecord(Piece piece, CompositePoLine compPOL, RequestContext requestContext) {
    final int ITEM_QUANTITY = 1;
    Promise<String> itemFuture = Promise.promise();
    try {
      logger.debug("Handling {} items for PO Line and holdings with id={}", ITEM_QUANTITY, piece.getHoldingId());
        if (piece.getFormat() == Piece.Format.ELECTRONIC && DefaultPieceFlowsValidator.isCreateItemForElectronicPiecePossible(piece, compPOL)) {
          inventoryManager.createMissingElectronicItems(compPOL, piece, ITEM_QUANTITY, requestContext)
            .onSuccess(idS -> itemFuture.complete(idS.get(0)))
            .onFailure(itemFuture::fail);
        } else if (DefaultPieceFlowsValidator.isCreateItemForNonElectronicPiecePossible(piece, compPOL)) {
          inventoryManager.createMissingPhysicalItems(compPOL, piece, ITEM_QUANTITY, requestContext)
            .onSuccess(idS -> itemFuture.complete(idS.get(0)))
            .onFailure(itemFuture::fail);
        }
      else {
        itemFuture.complete(null);
      }
    } catch (Exception e) {
      itemFuture.fail(e);
    }
    return itemFuture.future();
  }

  public Future<Pair<String, String>> deleteHoldingConnectedToPiece(Piece piece, RequestContext requestContext) {
    if (piece != null && piece.getHoldingId() != null) {
      String holdingId = piece.getHoldingId();
      return inventoryManager.getHoldingById(holdingId, true, requestContext)
                  .compose(holding -> {
                      if (holding != null && !holding.isEmpty()) {
                          return pieceStorageService.getPiecesByHoldingId(holdingId, requestContext)
                            .map(pieces -> skipPieceToProcess(piece, pieces))
                                  .compose(existingPieces -> inventoryManager.getItemsByHoldingId(holdingId, requestContext)
                                    .map(existingItems-> {
                                      List<Piece> remainingPieces = skipPieceToProcess(piece, existingPieces);
                                      if (CollectionUtils.isEmpty(remainingPieces) && CollectionUtils.isEmpty(existingItems)) {
                                        return Pair.of(true, holding);
                                      }
                                      return Pair.of(false, new JsonObject());
                                    }));
                      }
                      return Future.succeededFuture(Pair.of(false, new JsonObject()));
                  })
                  .compose(isUpdatePossibleVsHolding -> {
                    if (isUpdatePossibleVsHolding.getKey() && !isUpdatePossibleVsHolding.getValue().isEmpty()) {
                      String permanentLocationId = isUpdatePossibleVsHolding.getValue().getString(HOLDING_PERMANENT_LOCATION_ID);
                      return inventoryManager.deleteHoldingById(holdingId, true, requestContext)
                                              .map(v -> Pair.of(holdingId, permanentLocationId));
                    }
                    return Future.succeededFuture();
                  });
    }
    return Future.succeededFuture();
  }

  private List<Piece> skipPieceToProcess(Piece piece, List<Piece> pieces) {
    return pieces.stream().filter(aPiece -> !aPiece.getId().equals(piece.getId())).collect(
      Collectors.toList());
  }
}
