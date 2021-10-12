package org.folio.service.pieces.flows.update;

import static java.util.concurrent.CompletableFuture.completedFuture;
import static org.folio.rest.jaxrs.model.CompositePoLine.OrderFormat.ELECTRONIC_RESOURCE;
import static org.folio.service.inventory.InventoryManager.HOLDING_PERMANENT_LOCATION_ID;
import static org.folio.service.inventory.InventoryManager.ID;
import static org.folio.service.inventory.InventoryManager.ITEM_HOLDINGS_RECORD_ID;
import static org.folio.service.inventory.InventoryManager.ITEM_PURCHASE_ORDER_LINE_IDENTIFIER;

import java.util.Optional;
import java.util.concurrent.CompletableFuture;

import org.apache.commons.collections4.CollectionUtils;
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
import org.folio.service.pieces.flows.create.PieceCreateFlowValidator;
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
      .thenCompose(aVoid -> deleteOldHolding(holder, requestContext))
      .thenAccept(pair -> logger.debug(UPDATE_INVENTORY_FOR_LINE_DONE));
  }

  private CompletableFuture<Void> packagePoLineUpdateInventory(PieceUpdateHolder holder, RequestContext requestContext) {
    return titlesService.getTitleById(holder.getPieceToUpdate().getTitleId(), requestContext)
      .thenCompose(title -> packageUpdateTitleWithInstance(title, requestContext))
      .thenAccept(title -> holder.withInstanceId(title.getInstanceId()))
      .thenCompose(title -> handleHolding(holder, requestContext))
      .thenCompose(holdingId -> handleItem(holder, requestContext))
      .thenAccept(itemId -> Optional.ofNullable(itemId).ifPresent(holder.getPieceToUpdate()::withItemId))
      .thenCompose(aVoid -> deleteOldHolding(holder, requestContext))
      .thenAccept(pair -> logger.debug(UPDATE_INVENTORY_FOR_LINE_DONE));
  }

  private CompletableFuture<Location> handleHolding(PieceUpdateHolder holder, RequestContext requestContext) {
    CompositePoLine poLineToSave = holder.getPoLineToSave();
    Piece pieceToUpdate = holder.getPieceToUpdate();
    if (pieceToUpdate.getHoldingId() != null) {
      return completedFuture(new Location().withHoldingId(pieceToUpdate.getHoldingId()));
    }
    String instanceId = holder.getInstanceId();
    if (instanceId != null && PieceCreateFlowValidator.isCreateHoldingForPiecePossible(pieceToUpdate, poLineToSave)) {
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
    return inventoryManager.getItemRecordById(pieceToUpdate.getItemId(), true, requestContext)
      .thenCompose(jsonItem -> {
        if (holder.isCreateItem() && pieceToUpdate.getHoldingId() != null && (jsonItem == null || jsonItem.isEmpty()) &&
                          PieceCreateFlowValidator.isCreateItemForPiecePossible(pieceToUpdate, poLineToSave)) {
          return createItemRecord(poLineToSave, pieceToUpdate.getHoldingId(), requestContext);
        } else {
          return updateItemWithFields(jsonItem, poLineToSave, pieceToUpdate, requestContext)
                      .thenCompose(aVoid -> inventoryManager.updateItem(jsonItem, requestContext)
                      .thenApply(item -> jsonItem.getString(ID)));
        }
      });
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

  /**
   * Return id of created  Item
   */
  public CompletableFuture<String> createItemRecord(CompositePoLine compPOL, String holdingId, RequestContext requestContext) {
    final int ITEM_QUANTITY = 1;
    logger.debug("Handling {} items for PO Line and holdings with id={}", ITEM_QUANTITY, holdingId);
    CompletableFuture<String> itemFuture = new CompletableFuture<>();
    try {
      if (compPOL.getOrderFormat() == ELECTRONIC_RESOURCE) {
        inventoryManager.createMissingElectronicItems(compPOL, holdingId, ITEM_QUANTITY, requestContext)
          .thenApply(idS -> itemFuture.complete(idS.get(0)))
          .exceptionally(itemFuture::completeExceptionally);
      } else {
        inventoryManager.createMissingPhysicalItems(compPOL, holdingId, ITEM_QUANTITY, requestContext)
          .thenApply(idS -> itemFuture.complete(idS.get(0)))
          .exceptionally(itemFuture::completeExceptionally);
      }
    } catch (Exception e) {
      itemFuture.completeExceptionally(e);
    }
    return itemFuture;
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
      return pieceUpdateInventoryService.getOrCreateInstanceRecord(title, requestContext)
        .thenApply(title::withInstanceId)
        .thenCompose(titleWithInstanceId -> titlesService.saveTitle(titleWithInstanceId, requestContext).thenApply(json -> title));
    }
  }

  private CompletableFuture<String> createTitleInstance(Title title, RequestContext requestContext) {
    return pieceUpdateInventoryService.getOrCreateInstanceRecord(title, requestContext)
      .thenApply(title::withInstanceId)
      .thenCompose(titleWithInstanceId ->
        titlesService.saveTitle(titleWithInstanceId, requestContext).thenApply(aVoid -> title.getInstanceId())
      );
  }

  private CompletableFuture<Pair<String, String>> deleteOldHolding(PieceUpdateHolder holder, RequestContext rqContext) {
    if (holder.getPieceFromStorage().getHoldingId() != null) {
      String holdingId = holder.getPieceFromStorage().getHoldingId();
      return inventoryManager.getHoldingById(holdingId, true, rqContext)
                  .thenCompose(holding -> {
                    if (holding != null && !holding.isEmpty()) {
                      return inventoryManager.getItemsByHoldingId(holdingId, rqContext)
                        .thenCompose(items -> {
                          if (CollectionUtils.isEmpty(items)) {
                            String permanentLocationId = holding.getString(HOLDING_PERMANENT_LOCATION_ID);
                            return inventoryManager.deleteHoldingById(holdingId, true, rqContext)
                              .thenApply(v -> Pair.of(holdingId, permanentLocationId));
                          }
                          return completedFuture(null);
                        });
                  }
                  return completedFuture(null);
      });
    }
    return completedFuture(null);
  }

}
