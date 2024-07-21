package org.folio.service.pieces;

import static org.folio.service.inventory.InventoryItemManager.ITEM_STATUS;
import static org.folio.service.inventory.InventoryItemManager.ITEM_STATUS_NAME;

import io.vertx.core.Future;
import io.vertx.core.Promise;
import io.vertx.core.json.JsonObject;
import java.util.Optional;
import org.apache.commons.lang3.StringUtils;
import org.apache.logging.log4j.LogManager;
import org.apache.logging.log4j.Logger;
import org.folio.models.ItemStatus;
import org.folio.models.pieces.PieceUpdateHolder;
import org.folio.rest.core.models.RequestContext;
import org.folio.rest.jaxrs.model.Piece;
import org.folio.service.inventory.InventoryItemManager;
import org.folio.service.pieces.flows.DefaultPieceFlowsValidator;

public class ItemRecreateInventoryService {

  private static final Logger logger = LogManager.getLogger(ItemRecreateInventoryService.class);
  private static final int ITEM_QUANTITY = 1;

  private final InventoryItemManager inventoryItemManager;

  public ItemRecreateInventoryService(InventoryItemManager inventoryItemManager) {
    this.inventoryItemManager = inventoryItemManager;
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
      .compose(itemId -> deleteItem(piece, srcLocCtx).map(voidResult -> itemId));
  }

  public Future<Void> deleteItem(Piece piece, RequestContext requestContext) {
    return getOnOrderItemForPiece(piece, requestContext)
      .compose(item -> Optional.ofNullable(item)
        .map(itemEntry -> inventoryItemManager.deleteItem(piece.getItemId(), true, requestContext))
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
