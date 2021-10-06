package org.folio.service.pieces.flows.delete;

import static java.util.concurrent.CompletableFuture.completedFuture;
import static org.folio.orders.utils.ProtectedOperationType.DELETE;
import static org.folio.service.inventory.InventoryManager.HOLDING_PERMANENT_LOCATION_ID;
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
import org.folio.completablefuture.FolioVertxCompletableFuture;
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
import org.folio.service.finance.transaction.ReceivingEncumbranceStrategy;
import org.folio.service.inventory.InventoryManager;
import org.folio.service.orders.PurchaseOrderLineService;
import org.folio.service.orders.PurchaseOrderService;
import org.folio.service.pieces.PieceStorageService;
import org.folio.service.pieces.flows.PieceFlowUpdatePoLineKey;
import org.folio.service.pieces.flows.PieceFlowUpdatePoLineStrategyResolver;

import io.vertx.core.json.JsonObject;

public class PieceDeleteFlowManager {
  private static final Logger logger = LogManager.getLogger(PieceDeleteFlowManager.class);

  private final PieceStorageService pieceStorageService;
  private final ProtectionService protectionService;
  private final PurchaseOrderService purchaseOrderService;
  private final PurchaseOrderLineService purchaseOrderLineService;
  private final InventoryManager inventoryManager;
  private final ReceivingEncumbranceStrategy receivingEncumbranceStrategy;
  private final PieceFlowUpdatePoLineStrategyResolver pieceFlowUpdatePoLineStrategyResolver;


  public PieceDeleteFlowManager(PieceStorageService pieceStorageService, ProtectionService protectionService,
    PurchaseOrderService purchaseOrderService, PurchaseOrderLineService purchaseOrderLineService, InventoryManager inventoryManager,
    ReceivingEncumbranceStrategy receivingEncumbranceStrategy, PieceFlowUpdatePoLineStrategyResolver pieceFlowUpdatePoLineStrategyResolver) {
    this.pieceStorageService = pieceStorageService;
    this.protectionService = protectionService;
    this.purchaseOrderService = purchaseOrderService;
    this.purchaseOrderLineService = purchaseOrderLineService;
    this.inventoryManager = inventoryManager;
    this.receivingEncumbranceStrategy = receivingEncumbranceStrategy;
    this.pieceFlowUpdatePoLineStrategyResolver = pieceFlowUpdatePoLineStrategyResolver;
  }

  public CompletableFuture<Void> deleteItem(String pieceId, boolean deleteHolding, RequestContext requestContext) {
    PieceDeletionHolder holder = new PieceDeletionHolder(deleteHolding);
    return pieceStorageService.getPieceById(pieceId, requestContext)
      .thenCompose(piece -> purchaseOrderLineService.getOrderLineById(piece.getPoLineId(), requestContext)
        .thenCompose(poLine -> purchaseOrderService.getPurchaseOrderById(poLine.getPurchaseOrderId(), requestContext)
          .thenAccept(purchaseOrder -> holder.shallowCopy(new PieceDeletionHolder(purchaseOrder, poLine).withPieceToDelete(piece)))
        ))
      .thenCompose(purchaseOrder -> protectionService.isOperationRestricted(holder.getOriginPurchaseOrder().getAcqUnitIds(), DELETE, requestContext))
      .thenCompose(vVoid -> isDeletePieceRequestValid(holder, requestContext))
      .thenCompose(aVoid -> processInventory(holder, requestContext))
      .thenCompose(aVoid -> pieceStorageService.deletePiece(holder.getPieceToDelete().getId(), true, requestContext))
      .thenAccept(aVoid -> updatePoLine(holder, requestContext));
  }

  private CompletableFuture<Void> updatePoLine(PieceDeletionHolder holder, RequestContext requestContext) {
    if (!Boolean.TRUE.equals(holder.getOriginPoLine().getIsPackage()) && !Boolean.TRUE.equals(holder.getOriginPoLine().getCheckinItems()) ) {
      return FolioVertxCompletableFuture.from(requestContext.getContext(), completedFuture(poLineUpdateQuantity(holder))
                        .thenCompose(aHolder -> receivingEncumbranceStrategy.processEncumbrances(holder.getPurchaseOrderToSave(),
                                                                              holder.getPurchaseOrderToSave(), requestContext))
                        .thenAccept(v -> purchaseOrderLineService.updateOrderLine(holder.getPoLineToSave(), requestContext)));
    }
    return CompletableFuture.completedFuture(null);
  }

  private PieceDeletionHolder poLineUpdateQuantity(PieceDeletionHolder holder) {
    PieceFlowUpdatePoLineKey key = new PieceFlowUpdatePoLineKey().withIsPackage(holder.getPoLineToSave().getIsPackage())
        .withOrderWorkFlowStatus(holder.getPurchaseOrderToSave().getWorkflowStatus())
        .withPieceFlowType(PieceFlowUpdatePoLineKey.PieceFlowType.PIECE_DELETE_FLOW);
    pieceFlowUpdatePoLineStrategyResolver.resolve(key).ifPresent(strategy -> {
              strategy.updateQuantity(1, holder.getPieceToDelete(), holder.getPoLineToSave());
    });
    return holder;
  }

  private CompletableFuture<Void> isDeletePieceRequestValid(PieceDeletionHolder holder, RequestContext requestContext) {
    List<Error> combinedErrors = new ArrayList<>();
    if (holder.getPieceToDelete().getItemId() != null) {
      return inventoryManager.getNumberOfRequestsByItemId(holder.getPieceToDelete().getItemId(), requestContext).thenAccept(numOfRequests -> {
        if (numOfRequests != null && numOfRequests > 0) {
          combinedErrors.add(ErrorCodes.REQUEST_FOUND.toError());
        }
      }).thenApply(numOfRequests -> {
        if (CollectionUtils.isNotEmpty(combinedErrors)) {
          Errors errors = new Errors().withErrors(combinedErrors).withTotalRecords(combinedErrors.size());
          logger.error("Validation error : " + JsonObject.mapFrom(errors).encodePrettily());
          throw new HttpException(RestConstants.VALIDATION_ERROR, errors);
        }
        return null;
      });
    }
    return completedFuture(null);
  }

  private CompletableFuture<Pair<String, String>> processInventory(PieceDeletionHolder holder, RequestContext rqContext) {
    return deleteItem(holder, rqContext)
               .thenCompose(aVoid -> deleteHolding(holder, rqContext));
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

  private CompletableFuture<Void> deleteHoldingById(String holdingId, RequestContext requestContext) {
    if (StringUtils.isNotEmpty(holdingId)) {
      return inventoryManager.deleteHolding(holdingId, true, requestContext);
    } else {
      return completedFuture(null);
    }
  }

  private CompletableFuture<JsonObject> getHoldingById(String holdingId, RequestContext requestContext) {
    if (StringUtils.isNotEmpty(holdingId)) {
      return inventoryManager.getHoldingById(holdingId, true, requestContext);
    } else {
      return completedFuture(null);
    }
  }

  private CompletableFuture<Pair<String, String>> deleteHolding(PieceDeletionHolder holder, RequestContext rqContext) {
    if (holder.isDeleteHolding() && holder.getPieceToDelete().getHoldingId() != null) {
      String holdingId = holder.getPieceToDelete().getHoldingId();
      return getHoldingById(holdingId, rqContext).thenCompose(holding -> {
        if (holding != null && !holding.isEmpty()) {
          return inventoryManager.getItemsByHoldingId(holdingId, rqContext)
            .thenCompose(items -> {
              if (CollectionUtils.isEmpty(items)) {
                String permanentLocationId = holding.getString(HOLDING_PERMANENT_LOCATION_ID);
                return deleteHoldingById(holdingId, rqContext)
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
