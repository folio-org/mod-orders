package org.folio.service.pieces.flows.delete;

import static java.util.concurrent.CompletableFuture.completedFuture;
import static org.folio.orders.utils.ProtectedOperationType.DELETE;
import static org.folio.service.inventory.InventoryManager.ITEM_STATUS;
import static org.folio.service.inventory.InventoryManager.ITEM_STATUS_NAME;

import java.util.ArrayList;
import java.util.List;
import java.util.Optional;
import java.util.concurrent.CompletableFuture;

import org.apache.commons.collections4.CollectionUtils;
import org.apache.commons.lang3.StringUtils;
import org.apache.commons.lang3.tuple.Pair;
import org.apache.logging.log4j.LogManager;
import org.apache.logging.log4j.Logger;
import org.folio.models.ItemStatus;
import org.folio.models.pieces.PieceDeletionHolder;
import org.folio.rest.RestConstants;
import org.folio.rest.core.exceptions.ErrorCodes;
import org.folio.rest.core.exceptions.HttpException;
import org.folio.rest.core.models.RequestContext;
import org.folio.rest.jaxrs.model.Error;
import org.folio.rest.jaxrs.model.Errors;
import org.folio.rest.jaxrs.model.Piece;
import org.folio.service.ProtectionService;
import org.folio.service.inventory.InventoryManager;
import org.folio.service.pieces.PieceStorageService;
import org.folio.service.pieces.PieceUpdateInventoryService;
import org.folio.service.pieces.flows.BasePieceFlowHolderBuilder;

import io.vertx.core.json.JsonObject;

public class PieceDeleteFlowManager {
  private static final Logger logger = LogManager.getLogger(PieceDeleteFlowManager.class);

  private final PieceStorageService pieceStorageService;
  private final ProtectionService protectionService;
  private final InventoryManager inventoryManager;
  private final PieceUpdateInventoryService pieceUpdateInventoryService;
  private final PieceDeleteFlowPoLineService pieceDeleteFlowPoLineService;
  private final BasePieceFlowHolderBuilder basePieceFlowHolderBuilder;

  public PieceDeleteFlowManager(PieceStorageService pieceStorageService, ProtectionService protectionService,
    InventoryManager inventoryManager, PieceUpdateInventoryService pieceUpdateInventoryService,
    PieceDeleteFlowPoLineService pieceDeleteFlowPoLineService, BasePieceFlowHolderBuilder basePieceFlowHolderBuilder) {
    this.pieceStorageService = pieceStorageService;
    this.protectionService = protectionService;
    this.inventoryManager = inventoryManager;
    this.pieceUpdateInventoryService = pieceUpdateInventoryService;
    this.pieceDeleteFlowPoLineService = pieceDeleteFlowPoLineService;
    this.basePieceFlowHolderBuilder = basePieceFlowHolderBuilder;
  }

  public CompletableFuture<Void> deletePiece(String pieceId, boolean deleteHolding, RequestContext requestContext) {
    PieceDeletionHolder holder = new PieceDeletionHolder().withDeleteHolding(deleteHolding);
    return pieceStorageService.getPieceById(pieceId, requestContext)
      .thenAccept(holder::withPieceToDelete)
      .thenCompose(aHolder -> basePieceFlowHolderBuilder.updateHolderWithOrderInformation(holder, requestContext))
      .thenCompose(aVoid -> protectionService.isOperationRestricted(holder.getOriginPurchaseOrder().getAcqUnitIds(), DELETE, requestContext))
      .thenCompose(aVoid -> isDeletePieceRequestValid(holder, requestContext))
      .thenCompose(aVoid -> processInventory(holder, requestContext))
      .thenCompose(pair -> updatePoLine(holder, requestContext))
      .thenCompose(aVoid -> pieceStorageService.deletePiece(holder.getPieceToDelete().getId(), true, requestContext));
  }

  private CompletableFuture<Void> isDeletePieceRequestValid(PieceDeletionHolder holder, RequestContext requestContext) {
    List<Error> combinedErrors = new ArrayList<>();
    if (holder.getPieceToDelete().getItemId() != null) {
      return inventoryManager.getNumberOfRequestsByItemId(holder.getPieceToDelete().getItemId(), requestContext).thenAccept(numOfRequests -> {
        if (numOfRequests != null && numOfRequests > 0) {
          combinedErrors.add(ErrorCodes.REQUEST_FOUND.toError());
        }
      }).thenAccept(numOfRequests -> {
        if (CollectionUtils.isNotEmpty(combinedErrors)) {
          Errors errors = new Errors().withErrors(combinedErrors).withTotalRecords(combinedErrors.size());
          logger.error("Validation error : " + JsonObject.mapFrom(errors).encodePrettily());
          throw new HttpException(RestConstants.VALIDATION_ERROR, errors);
        }
      });
    }
    return completedFuture(null);
  }

  private CompletableFuture<Pair<String, String>> processInventory(PieceDeletionHolder holder, RequestContext rqContext) {
    return deleteItem(holder, rqContext)
               .thenCompose(aVoid -> {
                 if (holder.isDeleteHolding()) {
                   return pieceUpdateInventoryService.deleteHoldingConnectedToPiece(holder.getPieceToDelete(), rqContext);
                 }
                 return completedFuture(null);
               });
  }

  protected CompletableFuture<Void> updatePoLine(PieceDeletionHolder holder, RequestContext requestContext) {
    if (!Boolean.TRUE.equals(holder.getOriginPoLine().getIsPackage()) && !Boolean.TRUE.equals(holder.getOriginPoLine().getCheckinItems())) {
      return  pieceDeleteFlowPoLineService.updatePoLine(holder, requestContext);
    }
    return CompletableFuture.completedFuture(null);
  }

  private CompletableFuture<Void> deleteItem(PieceDeletionHolder holder, RequestContext rqContext) {
    Piece piece = holder.getPieceToDelete();
    if (piece.getItemId() != null) {
      return getOnOrderItemForPiece(piece, rqContext).thenCompose(item -> {
        if (item != null) {
          return inventoryManager.deleteItem(piece.getItemId(), true, rqContext);
        }
        return completedFuture(null);
      });
    }
    return completedFuture(null);
  }

  private boolean isItemWithStatus(JsonObject item, String status) {
    return Optional.ofNullable(item).map(itemP -> item.getJsonObject(ITEM_STATUS))
      .filter(itemStatus -> status.equalsIgnoreCase(itemStatus.getString(ITEM_STATUS_NAME)))
      .isPresent();
  }

  private CompletableFuture<JsonObject> getOnOrderItemForPiece(Piece piece, RequestContext requestContext) {
    if (StringUtils.isNotEmpty(piece.getItemId())) {
      return inventoryManager.getItemRecordById(piece.getItemId(), true, requestContext)
        .thenApply(item -> {
          boolean isOnOrderItem = isItemWithStatus(item, ItemStatus.ON_ORDER.value());
          if (isOnOrderItem) {
            return item;
          }
          return null;
        });
    } else {
      return completedFuture(null);
    }
  }
}
