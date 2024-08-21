package org.folio.service.pieces;

import static org.folio.service.inventory.InventoryItemManager.ITEM_STATUS;
import static org.folio.service.inventory.InventoryItemManager.ITEM_STATUS_NAME;

import io.vertx.core.Future;
import io.vertx.core.json.JsonObject;
import java.util.List;
import java.util.NoSuchElementException;
import java.util.Optional;
import org.apache.commons.lang3.StringUtils;
import org.apache.logging.log4j.LogManager;
import org.apache.logging.log4j.Logger;
import org.folio.models.ItemStatus;
import org.folio.rest.core.models.RequestContext;
import org.folio.rest.jaxrs.model.CompositePoLine;
import org.folio.rest.jaxrs.model.CompositePurchaseOrder;
import org.folio.rest.jaxrs.model.Piece;
import org.folio.service.inventory.InventoryItemManager;
import org.folio.service.pieces.flows.DefaultPieceFlowsValidator;

public class ItemRecreateInventoryService {

  protected static final int ITEM_QUANTITY = 1;
  private static final Logger logger = LogManager.getLogger(ItemRecreateInventoryService.class);

  private final InventoryItemManager inventoryItemManager;

  public ItemRecreateInventoryService(InventoryItemManager inventoryItemManager) {
    this.inventoryItemManager = inventoryItemManager;
  }

  // Return an Id of the recreated Item
  public Future<String> recreateItemInDestinationTenant(CompositePurchaseOrder compPO, CompositePoLine compPol,
                                                        Piece piece, RequestContext srcLocCtx, RequestContext dstLocCtx) {
    // Example Case: Member Tenant 1 (University) -> Member Tenant 2 (College)
    // Create Item in Member Tenant 2 with the same Item Id
    // Delete Item in Member Tenant 1 by Item Id
    Future<List<String>> itemFuture;
    if (piece.getFormat() == Piece.Format.ELECTRONIC && DefaultPieceFlowsValidator.isCreateItemForElectronicPiecePossible(piece, compPol)) {
      itemFuture = inventoryItemManager.createMissingElectronicItems(compPO, compPol, piece, ITEM_QUANTITY, dstLocCtx);
    } else if (DefaultPieceFlowsValidator.isCreateItemForNonElectronicPiecePossible(piece, compPol)) {
      itemFuture = inventoryItemManager.createMissingPhysicalItems(compPO, compPol, piece, ITEM_QUANTITY, dstLocCtx);
    } else {
      itemFuture = Future.succeededFuture(List.of());
    }

    return itemFuture
      .map(itemIds -> itemIds.stream().findFirst().orElseThrow(() -> new NoSuchElementException("Recreated Item Id could not be found")))
      .compose(itemId -> deleteItem(piece, srcLocCtx).map(voidResult -> itemId));
  }

  private Future<Void> deleteItem(Piece piece, RequestContext requestContext) {
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
