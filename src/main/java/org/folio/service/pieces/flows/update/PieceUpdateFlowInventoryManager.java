package org.folio.service.pieces.flows.update;

import static org.folio.service.inventory.InventoryManager.ID;
import static org.folio.service.inventory.InventoryManager.ITEM_HOLDINGS_RECORD_ID;
import static org.folio.service.inventory.InventoryManager.ITEM_PURCHASE_ORDER_LINE_IDENTIFIER;

import java.util.Optional;

import org.apache.commons.lang3.tuple.Pair;
import org.apache.logging.log4j.LogManager;
import org.apache.logging.log4j.Logger;
import org.folio.models.pieces.PieceUpdateHolder;
import org.folio.orders.utils.PoLineCommonUtil;
import org.folio.rest.core.models.RequestContext;
import org.folio.rest.jaxrs.model.CompositePoLine;
import org.folio.rest.jaxrs.model.Location;
import org.folio.rest.jaxrs.model.Piece;
import org.folio.rest.jaxrs.model.Title;
import org.folio.service.inventory.InventoryManager;
import org.folio.service.pieces.PieceUpdateInventoryService;
import org.folio.service.pieces.flows.DefaultPieceFlowsValidator;
import org.folio.service.titles.TitlesService;

import io.vertx.core.Future;
import io.vertx.core.json.JsonObject;

public class PieceUpdateFlowInventoryManager {
  private static final Logger logger = LogManager.getLogger(PieceUpdateFlowInventoryManager.class);
  private static final String UPDATE_INVENTORY_FOR_LINE_DONE = "Update inventory for line done";

  private final TitlesService titlesService;
  private final PieceUpdateInventoryService pieceUpdateInventoryService;
  private final InventoryManager inventoryManager;

  public PieceUpdateFlowInventoryManager(TitlesService titlesService,  PieceUpdateInventoryService pieceUpdateInventoryService,
                                         InventoryManager inventoryManager) {
    this.titlesService = titlesService;
    this.pieceUpdateInventoryService = pieceUpdateInventoryService;
    this.inventoryManager = inventoryManager;
  }

  public Future<Void> processInventory(PieceUpdateHolder holder, RequestContext requestContext) {
    return inventoryManager.updateItemWithPieceFields(holder.getPieceToUpdate(), requestContext)
      .compose(aVoid -> {
        if (Boolean.TRUE.equals(holder.getOriginPoLine().getIsPackage())) {
          return packagePoLineUpdateInventory(holder, requestContext);
        } else {
          return nonPackagePoLineUpdateInventory(holder, requestContext);
        }
      });
  }

  private Future<Void> nonPackagePoLineUpdateInventory(PieceUpdateHolder holder, RequestContext requestContext) {
    return nonPackageUpdateTitleWithInstance(holder, requestContext)
      .onSuccess(holder::withInstanceId)
      .compose(instanceId -> handleHolding(holder, requestContext))
      .compose(holdingId -> handleItem(holder, requestContext))
      .onSuccess(itemId -> Optional.ofNullable(itemId).ifPresent(holder.getPieceToUpdate()::withItemId))
      .compose(aVoid -> deleteHolding(holder, requestContext))
      .onSuccess(pair -> logger.debug(UPDATE_INVENTORY_FOR_LINE_DONE))
      .mapEmpty();
  }

  private Future<Void> packagePoLineUpdateInventory(PieceUpdateHolder holder, RequestContext requestContext) {
    return titlesService.getTitleById(holder.getPieceToUpdate().getTitleId(), requestContext)
      .compose(title -> packageUpdateTitleWithInstance(title, requestContext))
      .onSuccess(title -> holder.withInstanceId(title.getInstanceId()))
      .compose(title -> handleHolding(holder, requestContext))
      .compose(holdingId -> handleItem(holder, requestContext))
      .onSuccess(itemId -> Optional.ofNullable(itemId).ifPresent(holder.getPieceToUpdate()::withItemId))
      .compose(aVoid -> deleteHolding(holder, requestContext))
      .onSuccess(pair -> logger.debug(UPDATE_INVENTORY_FOR_LINE_DONE))
      .mapEmpty();
  }

  private Future<Pair<String, String>> deleteHolding(PieceUpdateHolder holder, RequestContext requestContext) {
    if (holder.isDeleteHolding()) {
      return pieceUpdateInventoryService.deleteHoldingConnectedToPiece(holder.getPieceFromStorage(), requestContext);
    }
    return Future.succeededFuture();
  }

  private Future<Location> handleHolding(PieceUpdateHolder holder, RequestContext requestContext) {
    CompositePoLine poLineToSave = holder.getPoLineToSave();
    Piece pieceToUpdate = holder.getPieceToUpdate();
    if (pieceToUpdate.getHoldingId() != null) {
      return Future.succeededFuture(new Location().withHoldingId(pieceToUpdate.getHoldingId()));
    }
    String instanceId = holder.getInstanceId();
    if (instanceId != null && DefaultPieceFlowsValidator.isCreateHoldingForPiecePossible(pieceToUpdate, poLineToSave)) {
      Location location = new Location().withLocationId(pieceToUpdate.getLocationId());
      return inventoryManager.getOrCreateHoldingsRecord(instanceId, location, requestContext)
        .map(holdingId -> {
          Optional.ofNullable(holdingId).ifPresent(holdingIdP -> {
            pieceToUpdate.setLocationId(null);
            pieceToUpdate.setHoldingId(holdingId);
            location.setLocationId(null);
            location.setHoldingId(holdingId);
          });
          return location;
        });
    }
    return Future.succeededFuture(new Location().withLocationId(pieceToUpdate.getLocationId()));
  }

  private Future<String> handleItem(PieceUpdateHolder holder, RequestContext requestContext) {
    CompositePoLine poLineToSave = holder.getPoLineToSave();
    Piece pieceToUpdate = holder.getPieceToUpdate();
    if (!DefaultPieceFlowsValidator.isCreateItemForPiecePossible(pieceToUpdate, poLineToSave)) {
        return Future.succeededFuture();
    }
    return inventoryManager.getItemRecordById(pieceToUpdate.getItemId(), true, requestContext)
      .compose(jsonItem -> {
        boolean jsonItemFound = jsonItem != null && !jsonItem.isEmpty();
        if (holder.isCreateItem() && !jsonItemFound && pieceToUpdate.getHoldingId() != null) {
          return pieceUpdateInventoryService.manualPieceFlowCreateItemRecord(pieceToUpdate, poLineToSave, requestContext);
        }
        if (jsonItemFound) {
          return updateItemWithFields(jsonItem, poLineToSave, pieceToUpdate)
            .compose(ignored -> inventoryManager.updateItem(jsonItem, requestContext).map(item -> jsonItem.getString(ID)));
        }
        return Future.succeededFuture();
      });
  }

  private Future<Void> updateItemWithFields(JsonObject item, CompositePoLine compPOL, Piece piece) {
    Optional.ofNullable(piece.getHoldingId())
      .ifPresent(pieceHoldingId -> item.put(ITEM_HOLDINGS_RECORD_ID, piece.getHoldingId()));
    item.put(ITEM_PURCHASE_ORDER_LINE_IDENTIFIER, compPOL.getId());
    return Future.succeededFuture();
  }

  private Future<String> nonPackageUpdateTitleWithInstance(PieceUpdateHolder holder, RequestContext requestContext) {
    CompositePoLine poLineToSave = holder.getPoLineToSave();
    Piece pieceToUpdate = holder.getPieceToUpdate();
    if (poLineToSave.getInstanceId() != null || PoLineCommonUtil.isInventoryUpdateNotRequired(poLineToSave)) {
      return Future.succeededFuture(poLineToSave.getInstanceId());
    }
    return titlesService.getTitleById(pieceToUpdate.getTitleId(), requestContext)
      .compose(title -> {
        if (title.getInstanceId() == null) {
          return createTitleInstance(title, requestContext);
        }
        return Future.succeededFuture(title.getInstanceId());
      })
      .map(instanceId -> poLineToSave.withInstanceId(instanceId).getInstanceId());
  }

  private Future<Title> packageUpdateTitleWithInstance(Title title, RequestContext requestContext) {
    if (title.getInstanceId() != null) {
      return Future.succeededFuture(title);
    }
    return inventoryManager.getOrCreateInstanceRecord(title, requestContext)
      .map(title::withInstanceId)
      .compose(titleWithInstanceId -> titlesService.saveTitle(titleWithInstanceId, requestContext))
      .map(v -> title);
  }

  private Future<String> createTitleInstance(Title title, RequestContext requestContext) {
    return inventoryManager.getOrCreateInstanceRecord(title, requestContext)
      .map(title::withInstanceId)
      .compose(titleWithInstanceId -> titlesService.saveTitle(titleWithInstanceId, requestContext))
      .map(aVoid -> title.getInstanceId());
  }
}
