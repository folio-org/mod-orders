package org.folio.service.pieces.flows.update;

import static org.folio.orders.utils.RequestContextUtil.createContextWithNewTenantId;
import static org.folio.service.inventory.InventoryItemManager.ID;
import static org.folio.service.inventory.InventoryItemManager.ITEM_HOLDINGS_RECORD_ID;
import static org.folio.service.inventory.InventoryItemManager.ITEM_PURCHASE_ORDER_LINE_IDENTIFIER;

import java.util.Optional;

import org.apache.commons.lang3.tuple.Pair;
import org.apache.logging.log4j.LogManager;
import org.apache.logging.log4j.Logger;
import org.folio.models.pieces.PieceUpdateHolder;
import org.folio.orders.utils.PoLineCommonUtil;
import org.folio.rest.core.models.RequestContext;
import org.folio.rest.jaxrs.model.PoLine;
import org.folio.rest.jaxrs.model.Location;
import org.folio.rest.jaxrs.model.Piece;
import org.folio.service.inventory.InventoryHoldingManager;
import org.folio.service.inventory.InventoryItemManager;
import org.folio.service.inventory.InventoryUtils;
import org.folio.service.pieces.ItemRecreateInventoryService;
import org.folio.service.pieces.PieceUpdateInventoryService;
import org.folio.service.pieces.flows.DefaultPieceFlowsValidator;
import org.folio.service.titles.TitlesService;

import io.vertx.core.Future;
import io.vertx.core.json.JsonObject;

public class PieceUpdateFlowInventoryManager {

  private static final Logger logger = LogManager.getLogger(PieceUpdateFlowInventoryManager.class);

  private final TitlesService titlesService;
  private final PieceUpdateInventoryService pieceUpdateInventoryService;
  private final ItemRecreateInventoryService itemRecreateInventoryService;
  private final InventoryItemManager inventoryItemManager;
  private final InventoryHoldingManager inventoryHoldingManager;

  public PieceUpdateFlowInventoryManager(TitlesService titlesService,
                                         PieceUpdateInventoryService pieceUpdateInventoryService,
                                         ItemRecreateInventoryService itemRecreateInventoryService,
                                         InventoryItemManager inventoryItemManager,
                                         InventoryHoldingManager inventoryHoldingManager) {
    this.titlesService = titlesService;
    this.pieceUpdateInventoryService = pieceUpdateInventoryService;
    this.itemRecreateInventoryService = itemRecreateInventoryService;
    this.inventoryItemManager = inventoryItemManager;
    this.inventoryHoldingManager = inventoryHoldingManager;
  }

  public Future<Void> processInventory(PieceUpdateHolder holder, RequestContext requestContext) {
    final var locationContext = createContextWithNewTenantId(requestContext, holder.getPieceToUpdate().getReceivingTenantId());
    Piece pieceToUpdate = holder.getPieceToUpdate();
    return inventoryItemManager.updateItemWithPieceFields(pieceToUpdate, locationContext)
      .compose(v -> updateInventoryForPoLine(holder, locationContext, requestContext)
          .map(holder::withInstanceId)
          .compose(aHolder -> handleHolding(holder, locationContext))
          .compose(holdingId -> handleItem(holder, locationContext))
          .map(itemId -> Optional.ofNullable(itemId).map(holder.getPieceToUpdate()::withItemId))
          .compose(aVoid -> deleteHolding(holder, requestContext))
          .onSuccess(pair -> logger.info("processInventory:: successfully updated inventory for piece with itemId: {}, poLineId: {}, receivingTenantId: {}",
            pieceToUpdate.getItemId(), pieceToUpdate.getPoLineId(), pieceToUpdate.getReceivingTenantId()))
          .onFailure(t -> logger.error("Failed to update inventory for piece with itemId: {}, poLineId: {}, receivingTenantId: {}",
            pieceToUpdate.getItemId(), pieceToUpdate.getPoLineId(), pieceToUpdate.getReceivingTenantId(), t))
          .mapEmpty()
      );
  }

  private Future<String> updateInventoryForPoLine(PieceUpdateHolder holder, RequestContext locationContext, RequestContext requestContext) {
    PoLine poLineToSave = holder.getPoLineToSave();
    Piece pieceToUpdate = holder.getPieceToUpdate();
    if (!Boolean.TRUE.equals(poLineToSave.getIsPackage())) {
      return Optional.ofNullable(getPoLineInstanceId(poLineToSave))
        .orElseGet(() -> titlesService.updateTitleWithInstance(pieceToUpdate.getTitleId(), locationContext, requestContext))
        .map(instanceId -> poLineToSave.withInstanceId(instanceId).getInstanceId());
    }
    return titlesService.updateTitleWithInstance(pieceToUpdate.getTitleId(), locationContext, requestContext);
  }

  private Future<String> getPoLineInstanceId(PoLine poLine) {
    return poLine.getInstanceId() != null || PoLineCommonUtil.isInventoryUpdateNotRequired(poLine)
      ? Future.succeededFuture(poLine.getInstanceId())
      : null;
  }

  private Future<Pair<String, String>> deleteHolding(PieceUpdateHolder holder, RequestContext requestContext) {
    return holder.isDeleteHolding()
      ? pieceUpdateInventoryService.deleteHoldingConnectedToPiece(holder.getPieceFromStorage(), requestContext)
      : Future.succeededFuture();
  }

  private Future<Location> handleHolding(PieceUpdateHolder holder, RequestContext requestContext) {
    var pieceToUpdate = holder.getPieceToUpdate();
    if (pieceToUpdate.getHoldingId() != null) {
      return Future.succeededFuture(new Location().withHoldingId(pieceToUpdate.getHoldingId()));
    }

    var poLineToSave = holder.getPoLineToSave();
    var instanceId = holder.getInstanceId();
    var location = new Location().withLocationId(pieceToUpdate.getLocationId());
    if (instanceId == null || !DefaultPieceFlowsValidator.isCreateHoldingForPiecePossible(pieceToUpdate, poLineToSave)) {
      return Future.succeededFuture(location);
    }
    return inventoryHoldingManager.createHoldingAndReturnId(instanceId, pieceToUpdate.getLocationId(), requestContext)
      .map(holdingId -> {
        if (holdingId != null) {
          pieceToUpdate.withLocationId(null).setHoldingId(holdingId);
          location.withLocationId(null).setHoldingId(holdingId);
        }
        return location;
      });
  }

  private Future<String> handleItem(PieceUpdateHolder holder, RequestContext requestContext) {
    var poLineToSave = holder.getPoLineToSave();
    var pieceToUpdate = holder.getPieceToUpdate();
    if (!DefaultPieceFlowsValidator.isCreateItemForPiecePossible(pieceToUpdate, poLineToSave) || pieceToUpdate.getIsBound()) {
      return Future.succeededFuture();
    }
    var srcConfig = InventoryUtils.constructItemRecreateConfig(holder.getPieceFromStorage().getReceivingTenantId(), requestContext, true);
    var dstConfig = InventoryUtils.constructItemRecreateConfig(pieceToUpdate.getReceivingTenantId(), requestContext, false);
    var itemId = pieceToUpdate.getItemId();
    return inventoryItemManager.getItemRecordById(itemId, true, srcConfig.context())
      .compose(jsonItem -> {
        if (jsonItem != null && !jsonItem.isEmpty()) {
          updateItemWithFields(jsonItem, poLineToSave, pieceToUpdate);
          if (InventoryUtils.allowItemRecreate(srcConfig.tenantId(), dstConfig.tenantId())) {
            logger.info("handleItem:: recreating item by id '{}', srcTenantId: '{}', dstTenantId: '{}'", itemId, srcConfig.tenantId(), dstConfig.tenantId());
            return itemRecreateInventoryService.recreateItemInDestinationTenant(holder.getPurchaseOrderToSave(),
              holder.getPoLineToSave(), holder.getPieceToUpdate(), srcConfig.context(), dstConfig.context());
          } else {
            logger.info("handleItem:: updating item by id '{}'", itemId);
            return inventoryItemManager.updateItem(jsonItem, requestContext).map(v -> jsonItem.getString(ID));
          }
        }
        if (holder.isCreateItem() && pieceToUpdate.getHoldingId() != null) {
          logger.info("handleItem:: creating item by id '{}'", itemId);
          return pieceUpdateInventoryService.manualPieceFlowCreateItemRecord(pieceToUpdate, holder.getPurchaseOrderToSave(), poLineToSave, requestContext);
        } else {
          return Future.succeededFuture();
        }
      });
  }

  private void updateItemWithFields(JsonObject item, PoLine poLine, Piece piece) {
    if (piece.getHoldingId() != null) {
      item.put(ITEM_HOLDINGS_RECORD_ID, piece.getHoldingId());
    }
    item.put(ITEM_PURCHASE_ORDER_LINE_IDENTIFIER, poLine.getId());
  }
}
