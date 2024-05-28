package org.folio.service.pieces;

import io.vertx.core.Future;
import io.vertx.core.json.JsonObject;
import org.apache.commons.lang3.StringUtils;
import org.folio.models.ItemStatus;
import org.folio.models.pieces.PieceDeletionHolder;
import org.folio.rest.core.models.RequestContext;
import org.folio.rest.jaxrs.model.Piece;
import org.folio.service.inventory.InventoryItemManager;

import java.util.Optional;

import static org.folio.service.inventory.InventoryItemManager.ITEM_STATUS;
import static org.folio.service.inventory.InventoryItemManager.ITEM_STATUS_NAME;

public class PieceDeleteInventoryService {

  private final InventoryItemManager inventoryItemManager;

  public PieceDeleteInventoryService(InventoryItemManager inventoryItemManager) {
    this.inventoryItemManager = inventoryItemManager;
  }

  public Future<Void> deleteItem(PieceDeletionHolder holder, RequestContext requestContext) {
    var piece = holder.getPieceToDelete();
    return getOnOrderItemForPiece(piece, requestContext)
      .compose(item -> Optional.ofNullable(item)
        .map(mItem -> inventoryItemManager.deleteItem(piece.getItemId(), true, requestContext))
        .orElse(Future.succeededFuture()));
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
