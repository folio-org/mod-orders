package org.folio.service.pieces;

import static org.folio.service.inventory.InventoryHoldingManager.HOLDING_PERMANENT_LOCATION_ID;
import static org.folio.service.inventory.InventoryItemManager.ITEM_STATUS;
import static org.folio.service.inventory.InventoryItemManager.ITEM_STATUS_NAME;

import io.vertx.core.Future;
import io.vertx.core.Promise;
import io.vertx.core.json.JsonObject;
import java.util.List;
import java.util.Objects;
import java.util.Optional;
import java.util.stream.Collectors;
import org.apache.commons.collections4.CollectionUtils;
import org.apache.commons.lang3.StringUtils;
import org.apache.commons.lang3.tuple.Pair;
import org.apache.logging.log4j.LogManager;
import org.apache.logging.log4j.Logger;
import org.folio.models.ItemStatus;
import org.folio.models.pieces.PieceUpdateHolder;
import org.folio.rest.core.models.RequestContext;
import org.folio.rest.jaxrs.model.Piece;
import org.folio.service.inventory.InventoryHoldingManager;
import org.folio.service.inventory.InventoryItemManager;
import org.folio.service.pieces.flows.DefaultPieceFlowsValidator;

public class ItemRecreateInventoryService {

  protected static final int ITEM_QUANTITY = 1;
  private static final Logger logger = LogManager.getLogger(ItemRecreateInventoryService.class);

  private final InventoryItemManager inventoryItemManager;
  private final InventoryHoldingManager inventoryHoldingManager;
  private final PieceStorageService pieceStorageService;

  public ItemRecreateInventoryService(InventoryItemManager inventoryItemManager,
                                      InventoryHoldingManager inventoryHoldingManager,
                                      PieceStorageService pieceStorageService) {
    this.inventoryItemManager = inventoryItemManager;
    this.inventoryHoldingManager = inventoryHoldingManager;
    this.pieceStorageService = pieceStorageService;
  }

  // Return an Id of the recreated Item
  public Future<String> recreateItemInDestinationTenant(PieceUpdateHolder holder, RequestContext srcLocCtx, RequestContext dstLocCtx) {
    // Case: Member Tenant 1 (University) -> Member Tenant 2 (College)
    // Create Item in Member Tenant 2 with the same Item Id
    // Delete Item in Member Tenant 1 by Item Id
    var itemFuture = Promise.<String>promise();
    var piece = holder.getPieceToUpdate();
    var compPol = holder.getPoLineToSave();

    try {
      logger.debug("Handling {} items for PO Line and holdings with id={}", ITEM_QUANTITY, piece.getHoldingId());

      if (piece.getFormat() == Piece.Format.ELECTRONIC && DefaultPieceFlowsValidator.isCreateItemForElectronicPiecePossible(piece, compPol)) {
        inventoryItemManager.recreateMissingElectronicItems(compPol, piece, ITEM_QUANTITY, dstLocCtx)
          .onSuccess(idS -> itemFuture.complete(idS.get(0)))
          .onFailure(itemFuture::fail);
      } else if (DefaultPieceFlowsValidator.isCreateItemForNonElectronicPiecePossible(piece, compPol)) {
        inventoryItemManager.recreateMissingPhysicalItems(compPol, piece, ITEM_QUANTITY, dstLocCtx)
          .onSuccess(idS -> itemFuture.complete(idS.get(0)))
          .onFailure(itemFuture::fail);
      } else {
        itemFuture.complete(null);
      }
    } catch (Exception e) {
      itemFuture.fail(e);
    }

    return itemFuture.future()
      .compose(itemId -> deleteHoldingConnectedToPiece(piece, srcLocCtx).map(pairResult -> itemId))
      .compose(itemId -> deleteItem(piece, srcLocCtx).map(voidResult -> itemId));
  }

  private Future<Void> deleteItem(Piece piece, RequestContext requestContext) {
    return getOnOrderItemForPiece(piece, requestContext)
      .compose(item -> Optional.ofNullable(item)
        .map(itemEntry -> inventoryItemManager.deleteItem(piece.getItemId(), true, requestContext))
        .orElse(Future.succeededFuture()));
  }

  public Future<Pair<String, String>> deleteHoldingConnectedToPiece(Piece piece, RequestContext requestContext) {
    if (Objects.isNull(piece) || Objects.isNull(piece.getHoldingId())) {
      return Future.succeededFuture();
    }

    var holdingId = piece.getHoldingId();

    return inventoryHoldingManager.getHoldingById(holdingId, true, requestContext)
      .compose(holding -> getUpdatePossibleForHolding(holding, holdingId, piece, requestContext))
      .compose(isUpdatePossibleVsHolding -> deleteHoldingIfPossible(isUpdatePossibleVsHolding, holdingId, requestContext));
  }

  private Future<Pair<Boolean, JsonObject>> getUpdatePossibleForHolding(JsonObject holding, String holdingId, Piece piece,
                                                                        RequestContext requestContext) {
    if (Objects.isNull(holding) || holding.isEmpty()) {
      return Future.succeededFuture(Pair.of(false, new JsonObject()));
    }

    return pieceStorageService.getPiecesByHoldingId(holdingId, requestContext)
      .map(pieces -> skipPieceToProcess(piece, pieces))
      .compose(existingPieces -> inventoryItemManager.getItemsByHoldingId(holdingId, requestContext)
        .map(existingItems -> {
          var remainingPieces = skipPieceToProcess(piece, existingPieces);
          if (CollectionUtils.isEmpty(remainingPieces) && CollectionUtils.isEmpty(existingItems)) {
            return Pair.of(true, holding);
          }

          return Pair.of(false, new JsonObject());
        })
      );
  }

  private Future<Pair<String, String>> deleteHoldingIfPossible(Pair<Boolean, JsonObject> isUpdatePossibleVsHolding,
                                                               String holdingId, RequestContext requestContext) {
    var isUpdatePossible = isUpdatePossibleVsHolding.getKey();
    var holding = isUpdatePossibleVsHolding.getValue();
    if (isUpdatePossible && !holding.isEmpty()) {
      var permanentLocationId = holding.getString(HOLDING_PERMANENT_LOCATION_ID);

      return inventoryHoldingManager.deleteHoldingById(holdingId, true, requestContext)
        .map(v -> Pair.of(holdingId, permanentLocationId));
    }
    return Future.succeededFuture();
  }

  private List<Piece> skipPieceToProcess(Piece piece, List<Piece> pieces) {
    return pieces.stream().filter(pieceEntry -> !pieceEntry.getId().equals(piece.getId())).collect(
      Collectors.toList());
  }

  private Future<JsonObject> getOnOrderItemForPiece(Piece piece, RequestContext requestContext) {
    if (StringUtils.isEmpty(piece.getItemId())) {
      return Future.succeededFuture();
    }

    return inventoryItemManager.getItemRecordById(piece.getItemId(), true, requestContext)
      .map(item -> getItemWithStatus(item, ItemStatus.ON_ORDER.value()));
  }

  private JsonObject getItemWithStatus(JsonObject item, String status) {
    return Optional.ofNullable(item)
      .map(itemObj -> itemObj.getJsonObject(ITEM_STATUS))
      .filter(itemStatus -> status.equalsIgnoreCase(itemStatus.getString(ITEM_STATUS_NAME)))
      .orElse(null);
  }
}
