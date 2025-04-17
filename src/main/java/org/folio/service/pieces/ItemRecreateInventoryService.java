package org.folio.service.pieces;

import io.vertx.core.Future;
import java.util.List;
import java.util.NoSuchElementException;
import java.util.Objects;
import org.apache.commons.lang3.StringUtils;
import org.folio.rest.core.models.RequestContext;
import org.folio.rest.jaxrs.model.PoLine;
import org.folio.rest.jaxrs.model.CompositePurchaseOrder;
import org.folio.rest.jaxrs.model.Piece;
import org.folio.service.inventory.InventoryItemManager;
import org.folio.service.pieces.flows.DefaultPieceFlowsValidator;

public class ItemRecreateInventoryService {

  protected static final int ITEM_QUANTITY = 1;

  private final InventoryItemManager inventoryItemManager;

  public ItemRecreateInventoryService(InventoryItemManager inventoryItemManager) {
    this.inventoryItemManager = inventoryItemManager;
  }

  /**
   * Recreates an item in a destination tenant
   * Example case:
   * Change piece affiliation from member tenant 1 (e.g. university) to member tenant 2 (e.g. college)
   * Performed actions:
   * 1. Find item in member tenant 1 by id
   * 2. If an item is found create an item in member tenant 2 with the same item id
   * 3. Then delete the item in member tenant 1 by item id
   *
   * @param compOrder Composite PO
   * @param poLine POL
   * @param piece Piece
   * @param srcLocCtx Source location context
   * @param dstLocCtx Destination location context
   * @return id of the recreated Item
   */
  public Future<String> recreateItemInDestinationTenant(CompositePurchaseOrder compOrder, PoLine poLine,
                                                        Piece piece, RequestContext srcLocCtx, RequestContext dstLocCtx) {
    if (StringUtils.isEmpty(piece.getItemId())) {
      return Future.succeededFuture();
    }
    return inventoryItemManager.getItemRecordById(piece.getItemId(), true, srcLocCtx)
      .compose(item -> {
        if (Objects.nonNull(item)) {
          if (piece.getFormat() == Piece.Format.ELECTRONIC && DefaultPieceFlowsValidator.isCreateItemForElectronicPiecePossible(piece, poLine)) {
            return inventoryItemManager.createMissingElectronicItems(compOrder, poLine, piece, ITEM_QUANTITY, dstLocCtx);
          } else if (DefaultPieceFlowsValidator.isCreateItemForNonElectronicPiecePossible(piece, poLine)) {
            return inventoryItemManager.createMissingPhysicalItems(compOrder, poLine, piece, ITEM_QUANTITY,  dstLocCtx);
          }
        }
        return Future.succeededFuture(List.of());
      })
      .map(itemIds -> itemIds.stream().findFirst().orElseThrow(() -> new NoSuchElementException("Recreated item id could not be found")))
      .compose(itemId -> inventoryItemManager.deleteItem(piece.getItemId(), true, srcLocCtx).map(v -> itemId));
  }
}
