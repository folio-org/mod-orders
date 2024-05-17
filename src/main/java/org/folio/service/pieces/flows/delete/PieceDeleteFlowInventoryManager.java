package org.folio.service.pieces.flows.delete;

import io.vertx.core.Future;
import io.vertx.core.json.JsonObject;
import org.apache.commons.lang3.StringUtils;
import org.apache.commons.lang3.tuple.Pair;
import org.folio.models.ItemStatus;
import org.folio.models.pieces.PieceDeletionHolder;
import org.folio.rest.core.models.RequestContext;
import org.folio.rest.jaxrs.model.Piece;
import org.folio.service.inventory.InventoryItemManager;
import org.folio.service.pieces.PieceUpdateInventoryService;

import java.util.Optional;

import static org.folio.service.inventory.InventoryItemManager.ITEM_STATUS;
import static org.folio.service.inventory.InventoryItemManager.ITEM_STATUS_NAME;

public class PieceDeleteFlowInventoryManager {

  private final InventoryItemManager inventoryItemManager;
  private final PieceUpdateInventoryService pieceUpdateInventoryService;
  public PieceDeleteFlowInventoryManager(InventoryItemManager inventoryItemManager,
                                         PieceUpdateInventoryService pieceUpdateInventoryService) {
    this.inventoryItemManager = inventoryItemManager;
    this.pieceUpdateInventoryService = pieceUpdateInventoryService;
  }

  public Future<Pair<String, String>> processInventory(PieceDeletionHolder holder, RequestContext requestContext) {
    return deleteItem(holder, requestContext)
      .compose(aVoid -> holder.isDeleteHolding()
          ? pieceUpdateInventoryService.deleteHoldingConnectedToPiece(holder.getPieceToDelete(), requestContext)
          : Future.succeededFuture()
      );
  }

  private Future<Void> deleteItem(PieceDeletionHolder holder, RequestContext requestContext) {
    var piece = holder.getPieceToDelete();
    return getOnOrderItemForPiece(piece, requestContext)
      .compose(item -> Optional.ofNullable(item)
        .map(mItem -> inventoryItemManager.deleteItem(piece.getItemId(), true, requestContext))
        .orElse(Future.succeededFuture()));
  }

  private Future<JsonObject> getOnOrderItemForPiece(Piece piece, RequestContext requestContext) {
    return StringUtils.isEmpty(piece.getItemId())
      ? Future.succeededFuture()
      : inventoryItemManager.getItemRecordById(piece.getItemId(), true, requestContext)
          .map(item -> isItemWithStatus(item, ItemStatus.ON_ORDER.value()) ? item : null);
  }

  private boolean isItemWithStatus(JsonObject item, String status) {
    return Optional.ofNullable(item)
      .map(itemObj -> itemObj.getJsonObject(ITEM_STATUS))
      .filter(itemStatus -> status.equalsIgnoreCase(itemStatus.getString(ITEM_STATUS_NAME)))
      .isPresent();
  }

}
