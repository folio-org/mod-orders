package org.folio.service.pieces;

import static java.util.concurrent.CompletableFuture.completedFuture;
import static org.folio.service.inventory.InventoryManager.HOLDING_PERMANENT_LOCATION_ID;

import java.util.List;
import java.util.concurrent.CompletableFuture;
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
import org.folio.service.titles.TitlesService;

import io.vertx.core.json.JsonObject;

public class PieceUpdateInventoryService {
  private static final Logger logger = LogManager.getLogger(PieceUpdateInventoryService.class);

  private static final String INSTANCES = "instances";

  private final TitlesService titlesService;
  private final InventoryManager inventoryManager;
  private final PieceStorageService pieceStorageService;

  public PieceUpdateInventoryService(TitlesService titlesService, InventoryManager inventoryManager,
                                      PieceStorageService pieceStorageService) {
    this.titlesService = titlesService;
    this.inventoryManager = inventoryManager;
    this.pieceStorageService = pieceStorageService;
  }


//  public CompletableFuture<Title> handleInstanceRecord(Title title, RequestContext requestContext) {
//    if (title.getInstanceId() != null) {
//      return CompletableFuture.completedFuture(title);
//    } else {
//      return getOrCreateInstanceRecord(title, requestContext).thenApply(title::withInstanceId);
//    }
//  }

//  public CompletableFuture<String> getOrCreateInstanceRecord(Title title, RequestContext requestContext) {
//    // proceed with new Instance Record creation if no productId is provided
//    if (!CollectionUtils.isNotEmpty(title.getProductIds())) {
//      return inventoryManager.createInstanceRecord(title, requestContext);
//    }
//
//    return inventoryManager.searchInstancesByProducts(title.getProductIds(), requestContext)
//      .thenCompose(instances -> {
//        if (!instances.getJsonArray(INSTANCES).isEmpty()) {
//          String instanceId = HelperUtils.getFirstObjectFromResponse(instances, INSTANCES).getString(ID);
//          return completedFuture(instanceId);
//        }
//        return inventoryManager.createInstanceRecord(title, requestContext);
//      });
//  }

//  /**
//   * Return id of created  Holding
//   */
  public CompletableFuture<String> handleHoldingsRecord(final CompositePoLine compPOL, Location location, String instanceId,
    RequestContext requestContext) {
    try {
      if (PoLineCommonUtil.isHoldingsUpdateRequired(compPOL)) {
        return inventoryManager.getOrCreateHoldingsRecord(instanceId, location, requestContext);
      } else {
        return CompletableFuture.completedFuture(null);
      }
    }
    catch (Exception e) {
      return CompletableFuture.failedFuture(e);
    }
  }

//  /**
//   * Return id of created  Item
//   */
//  public CompletableFuture<String> openOrderCreateItemRecord(CompositePoLine compPOL, String holdingId, RequestContext requestContext) {
//    final int ITEM_QUANTITY = 1;
//    logger.debug("Handling {} items for PO Line and holdings with id={}", ITEM_QUANTITY, holdingId);
//    CompletableFuture<String> itemFuture = new CompletableFuture<>();
//    try {
//      if (PoLineCommonUtil.isItemsUpdateRequired(compPOL)) {
//        if (compPOL.getOrderFormat() == ELECTRONIC_RESOURCE) {
//          inventoryManager.createMissingElectronicItems(compPOL, holdingId, ITEM_QUANTITY, requestContext)
//            .thenApply(idS -> itemFuture.complete(idS.get(0)))
//            .exceptionally(itemFuture::completeExceptionally);
//        } else {
//          inventoryManager.createMissingPhysicalItems(compPOL, holdingId, ITEM_QUANTITY, requestContext)
//            .thenApply(idS -> itemFuture.complete(idS.get(0)))
//            .exceptionally(itemFuture::completeExceptionally);
//        }
//      }
//      else {
//        itemFuture.complete(null);
//      }
//    } catch (Exception e) {
//      itemFuture.completeExceptionally(e);
//    }
//    return itemFuture;
//  }

  /**
   * Return id of created  Item
   */
  public CompletableFuture<String> manualPieceFlowCreateItemRecord(Piece piece, CompositePoLine compPOL, RequestContext requestContext) {
    final int ITEM_QUANTITY = 1;
    CompletableFuture<String> itemFuture = new CompletableFuture<>();
    try {
      String holdingId = piece.getHoldingId();
      logger.debug("Handling {} items for PO Line and holdings with id={}", ITEM_QUANTITY, holdingId);
        if (piece.getFormat() == Piece.Format.ELECTRONIC && DefaultPieceFlowsValidator.isCreateItemForElectronicPiecePossible(piece, compPOL)) {
          inventoryManager.createMissingElectronicItems(compPOL, holdingId, ITEM_QUANTITY, requestContext)
            .thenApply(idS -> itemFuture.complete(idS.get(0)))
            .exceptionally(itemFuture::completeExceptionally);
        } else if (DefaultPieceFlowsValidator.isCreateItemForNonElectronicPiecePossible(piece, compPOL)) {
          inventoryManager.createMissingPhysicalItems(compPOL, holdingId, ITEM_QUANTITY, requestContext)
            .thenApply(idS -> itemFuture.complete(idS.get(0)))
            .exceptionally(itemFuture::completeExceptionally);
        }
      else {
        itemFuture.complete(null);
      }
    } catch (Exception e) {
      itemFuture.completeExceptionally(e);
    }
    return itemFuture;
  }

  public CompletableFuture<Pair<String, String>> deleteHoldingConnectedToPiece(Piece piece, RequestContext rqContext) {
    if (piece != null && piece.getHoldingId() != null) {
      String holdingId = piece.getHoldingId();
      return inventoryManager.getHoldingById(holdingId, true, rqContext)
                  .thenCompose(holding -> {
                      if (holding != null && !holding.isEmpty()) {
                          return pieceStorageService.getPiecesByHoldingId(holdingId, rqContext)
                            .thenApply(pieces -> skipPieceToProcess(piece, pieces))
                                  .thenCombine(inventoryManager.getItemsByHoldingId(holdingId, rqContext),
                                    (existingPieces, existingItems) -> {
                                      List<Piece> remainingPieces = skipPieceToProcess(piece, existingPieces);
                                      if (CollectionUtils.isEmpty(remainingPieces) && CollectionUtils.isEmpty(existingItems)) {
                                        return Pair.of(true, holding);
                                      }
                                      return Pair.of(false, new JsonObject());
                                    });
                      }
                      return completedFuture(Pair.of(false, new JsonObject()));
                  })
                  .thenCompose(isUpdatePossibleVsHolding -> {
                    if (isUpdatePossibleVsHolding.getKey() && !isUpdatePossibleVsHolding.getValue().isEmpty()) {
                      String permanentLocationId = isUpdatePossibleVsHolding.getValue().getString(HOLDING_PERMANENT_LOCATION_ID);
                      return inventoryManager.deleteHoldingById(holdingId, true, rqContext)
                                              .thenApply(v -> Pair.of(holdingId, permanentLocationId));
                    }
                    return completedFuture(null);
                  });
    }
    return completedFuture(null);
  }

  private List<Piece> skipPieceToProcess(Piece piece, List<Piece> pieces) {
    return pieces.stream().filter(aPiece -> !aPiece.getId().equals(piece.getId())).collect(
      Collectors.toList());
  }
}
