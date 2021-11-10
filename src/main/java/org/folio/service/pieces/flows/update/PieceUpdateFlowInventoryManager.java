package org.folio.service.pieces.flows.update;

import static java.util.concurrent.CompletableFuture.completedFuture;
import static org.folio.service.inventory.InventoryManager.ID;
import static org.folio.service.inventory.InventoryManager.ITEM_HOLDINGS_RECORD_ID;
import static org.folio.service.inventory.InventoryManager.ITEM_PURCHASE_ORDER_LINE_IDENTIFIER;

import java.util.Optional;
import java.util.concurrent.CompletableFuture;

import org.apache.commons.lang3.tuple.Pair;
import org.apache.logging.log4j.LogManager;
import org.apache.logging.log4j.Logger;
import org.folio.completablefuture.FolioVertxCompletableFuture;
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

  public CompletableFuture<Void> processInventory(PieceUpdateHolder holder, RequestContext requestContext) {
    return inventoryManager.updateItemWithPoLineId(holder.getPieceToUpdate().getItemId(), holder.getPieceToUpdate().getPoLineId(), requestContext)
      .thenCompose(aVoid -> {
        if (Boolean.TRUE.equals(holder.getOriginPoLine().getIsPackage())) {
          return packagePoLineUpdateInventory(holder, requestContext);
        }
        else
        {
          return nonPackagePoLineUpdateInventory(holder, requestContext);
        }
      });
  }

  private CompletableFuture<Void> nonPackagePoLineUpdateInventory(PieceUpdateHolder holder, RequestContext requestContext) {
    return nonPackageUpdateTitleWithInstance(holder, requestContext)
      .thenAccept(holder::withInstanceId)
      .thenCompose(instanceId -> handleHolding(holder, requestContext))
      .thenCompose(holdingId -> handleItem(holder, requestContext))
      .thenAccept(itemId -> Optional.ofNullable(itemId).ifPresent(holder.getPieceToUpdate()::withItemId))
      .thenCompose(aVoid -> deleteHolding(holder, requestContext))
      .thenAccept(pair -> logger.debug(UPDATE_INVENTORY_FOR_LINE_DONE));
  }

  private CompletableFuture<Void> packagePoLineUpdateInventory(PieceUpdateHolder holder, RequestContext requestContext) {
    return titlesService.getTitleById(holder.getPieceToUpdate().getTitleId(), requestContext)
      .thenCompose(title -> packageUpdateTitleWithInstance(title, requestContext))
      .thenAccept(title -> holder.withInstanceId(title.getInstanceId()))
      .thenCompose(title -> handleHolding(holder, requestContext))
      .thenCompose(holdingId -> handleItem(holder, requestContext))
      .thenAccept(itemId -> Optional.ofNullable(itemId).ifPresent(holder.getPieceToUpdate()::withItemId))
      .thenCompose(aVoid -> deleteHolding(holder, requestContext))
      .thenAccept(pair -> logger.debug(UPDATE_INVENTORY_FOR_LINE_DONE));
  }

  private CompletableFuture<Pair<String, String>> deleteHolding(PieceUpdateHolder holder, RequestContext requestContext) {
    if (holder.isDeleteHolding()) {
      return pieceUpdateInventoryService.deleteHoldingConnectedToPiece(holder.getPieceFromStorage(), requestContext);
    }
    return completedFuture(null);
  }

  private CompletableFuture<Location> handleHolding(PieceUpdateHolder holder, RequestContext requestContext) {
    CompositePoLine poLineToSave = holder.getPoLineToSave();
    Piece pieceToUpdate = holder.getPieceToUpdate();
    if (pieceToUpdate.getHoldingId() != null) {
      return completedFuture(new Location().withHoldingId(pieceToUpdate.getHoldingId()));
    }
    String instanceId = holder.getInstanceId();
    if (instanceId != null && DefaultPieceFlowsValidator.isCreateHoldingForPiecePossible(pieceToUpdate, poLineToSave)) {
      Location location = new Location().withLocationId(pieceToUpdate.getLocationId());
      return inventoryManager.getOrCreateHoldingsRecord(instanceId, location, requestContext)
                      .thenApply(holdingId -> {
                          Optional.ofNullable(holdingId).ifPresent(holdingIdP -> {
                            pieceToUpdate.setLocationId(null);
                            pieceToUpdate.setHoldingId(holdingId);
                            location.setLocationId(null);
                            location.setHoldingId(holdingId);
                          });
                          return location;
                        });
    }
    return completedFuture(new Location().withLocationId(pieceToUpdate.getLocationId()));
  }

  private CompletableFuture<String> handleItem(PieceUpdateHolder holder, RequestContext requestContext) {
    CompositePoLine poLineToSave = holder.getPoLineToSave();
    Piece pieceToUpdate = holder.getPieceToUpdate();
    if (DefaultPieceFlowsValidator.isCreateItemForPiecePossible(pieceToUpdate, poLineToSave)) {
      return inventoryManager.getItemRecordById(pieceToUpdate.getItemId(), true, requestContext).thenCompose(jsonItem -> {
        if (holder.isCreateItem() && (jsonItem == null || jsonItem.isEmpty()) && pieceToUpdate.getHoldingId() != null) {
          return pieceUpdateInventoryService.manualPieceFlowCreateItemRecord(pieceToUpdate, poLineToSave, requestContext);
        } else if (jsonItem != null && !jsonItem.isEmpty()) {
          return updateItemWithFields(jsonItem, poLineToSave, pieceToUpdate, requestContext).thenCompose(
            aVoid -> inventoryManager.updateItem(jsonItem, requestContext).thenApply(item -> jsonItem.getString(ID)));
        } else {
          return completedFuture(null);
        }
      });
    }
    return completedFuture(null);
  }

  private CompletableFuture<Void> updateItemWithFields(JsonObject item, CompositePoLine compPOL, Piece piece, RequestContext requestContext) {
    return FolioVertxCompletableFuture.from(requestContext.getContext(), completedFuture(item))
      .thenAccept(itemP -> {
        Optional.ofNullable(piece.getHoldingId()).ifPresent(pieceHoldingId -> {
          itemP.put(ITEM_HOLDINGS_RECORD_ID, piece.getHoldingId());
        });
       itemP.put(ITEM_PURCHASE_ORDER_LINE_IDENTIFIER, compPOL.getId());
      });
  }

  private CompletableFuture<String> nonPackageUpdateTitleWithInstance(PieceUpdateHolder holder, RequestContext requestContext) {
    CompositePoLine poLineToSave = holder.getPoLineToSave();
    Piece pieceToUpdate = holder.getPieceToUpdate();
    if (poLineToSave.getInstanceId() == null && !PoLineCommonUtil.isInventoryUpdateNotRequired(poLineToSave)) {
      return titlesService.getTitleById(pieceToUpdate.getTitleId(), requestContext)
        .thenCompose(title -> {
          if (title.getInstanceId() == null) {
            return createTitleInstance(title, requestContext);
          }
          return completedFuture(title.getInstanceId());
        })
        .thenApply(instanceId -> poLineToSave.withInstanceId(instanceId).getInstanceId());
    }
    return completedFuture(poLineToSave.getInstanceId());
  }

  private CompletableFuture<Title> packageUpdateTitleWithInstance(Title title, RequestContext requestContext) {
    if (title.getInstanceId() != null) {
      return CompletableFuture.completedFuture(title);
    } else {
      return inventoryManager.getOrCreateInstanceRecord(title, requestContext)
        .thenApply(title::withInstanceId)
        .thenCompose(titleWithInstanceId -> titlesService.saveTitle(titleWithInstanceId, requestContext).thenApply(json -> title));
    }
  }

  private CompletableFuture<String> createTitleInstance(Title title, RequestContext requestContext) {
    return inventoryManager.getOrCreateInstanceRecord(title, requestContext)
      .thenApply(title::withInstanceId)
      .thenCompose(titleWithInstanceId ->
        titlesService.saveTitle(titleWithInstanceId, requestContext).thenApply(aVoid -> title.getInstanceId())
      );
  }
}
