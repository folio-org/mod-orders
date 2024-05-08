package org.folio.service.pieces.flows.delete;

import static org.folio.orders.utils.ProtectedOperationType.DELETE;
import static org.folio.service.inventory.InventoryItemManager.ITEM_STATUS;
import static org.folio.service.inventory.InventoryItemManager.ITEM_STATUS_NAME;
import static org.folio.service.orders.utils.HelperUtils.collectResultsOnSuccess;


import java.util.ArrayList;
import java.util.List;
import java.util.Optional;
import java.util.stream.Collectors;

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
import org.folio.rest.jaxrs.model.PieceCollection;
import org.folio.service.ProtectionService;
import org.folio.service.inventory.InventoryItemManager;
import org.folio.service.CirculationRequestsRetriever;
import org.folio.service.pieces.PieceStorageService;
import org.folio.service.pieces.PieceUpdateInventoryService;
import org.folio.service.pieces.flows.BasePieceFlowHolderBuilder;

import io.vertx.core.Future;
import io.vertx.core.json.JsonObject;

public class PieceDeleteFlowManager {
  private static final Logger logger = LogManager.getLogger(PieceDeleteFlowManager.class);

  private final PieceStorageService pieceStorageService;
  private final ProtectionService protectionService;
  private final InventoryItemManager inventoryItemManager;
  private final PieceUpdateInventoryService pieceUpdateInventoryService;
  private final PieceDeleteFlowPoLineService pieceDeleteFlowPoLineService;
  private final BasePieceFlowHolderBuilder basePieceFlowHolderBuilder;
  private final CirculationRequestsRetriever circulationRequestsRetriever;

  public PieceDeleteFlowManager(PieceStorageService pieceStorageService,
                                ProtectionService protectionService,
                                InventoryItemManager inventoryItemManager,
                                PieceUpdateInventoryService pieceUpdateInventoryService,
                                PieceDeleteFlowPoLineService pieceDeleteFlowPoLineService,
                                BasePieceFlowHolderBuilder basePieceFlowHolderBuilder,
                                CirculationRequestsRetriever circulationRequestsRetriever) {
    this.pieceStorageService = pieceStorageService;
    this.protectionService = protectionService;
    this.inventoryItemManager = inventoryItemManager;
    this.pieceUpdateInventoryService = pieceUpdateInventoryService;
    this.pieceDeleteFlowPoLineService = pieceDeleteFlowPoLineService;
    this.basePieceFlowHolderBuilder = basePieceFlowHolderBuilder;
    this.circulationRequestsRetriever = circulationRequestsRetriever;
  }

  public Future<Void> deletePiece(String pieceId, boolean deleteHolding, RequestContext requestContext) {
    PieceDeletionHolder holder = new PieceDeletionHolder().withDeleteHolding(deleteHolding);

    return pieceStorageService.getPieceById(pieceId, requestContext)
      .map(pieceToDelete -> {
        holder.withPieceToDelete(pieceToDelete); return null;
      })
      .compose(aHolder -> basePieceFlowHolderBuilder.updateHolderWithOrderInformation(holder, requestContext))
      .compose(aHolder -> basePieceFlowHolderBuilder.updateHolderWithTitleInformation(holder, requestContext))
      .compose(aVoid -> protectionService.isOperationRestricted(holder.getTitle().getAcqUnitIds(), DELETE, requestContext))
      .compose(aVoid -> isDeletePieceRequestValid(holder, requestContext))
      .compose(aVoid -> processInventory(holder, requestContext))
      .compose(pair -> updatePoLine(holder, requestContext))
      .compose(aVoid -> pieceStorageService.deletePiece(holder.getPieceToDelete().getId(), true, requestContext));
  }

  private Future<Void> isDeletePieceRequestValid(PieceDeletionHolder holder, RequestContext requestContext) {
    List<Error> combinedErrors = new ArrayList<>();
    if (holder.getPieceToDelete().getItemId() != null) {
      return circulationRequestsRetriever.getNumberOfRequestsByItemId(holder.getPieceToDelete().getItemId(), requestContext)
        .map(numOfRequests -> {
          if (numOfRequests != null && numOfRequests > 0) {
            combinedErrors.add(ErrorCodes.REQUEST_FOUND.toError());
          }
          return null;
        })
        .map(v -> {
          if (CollectionUtils.isNotEmpty(combinedErrors)) {
            Errors errors = new Errors().withErrors(combinedErrors).withTotalRecords(combinedErrors.size());
            logger.error("Validation error : " + JsonObject.mapFrom(errors).encodePrettily());
            throw new HttpException(RestConstants.VALIDATION_ERROR, errors);
          }
          return null;
        })
      .mapEmpty();
    }
    return Future.succeededFuture();
  }

  private Future<Pair<String, String>> processInventory(PieceDeletionHolder holder, RequestContext requestContext) {
    return deleteItem(holder, requestContext)
               .compose(aVoid -> {
                 if (holder.isDeleteHolding()) {
                   return pieceUpdateInventoryService.deleteHoldingConnectedToPiece(holder.getPieceToDelete(), requestContext);
                 }
                 return Future.succeededFuture();
               });
  }

  protected Future<Void> updatePoLine(PieceDeletionHolder holder, RequestContext requestContext) {
    if (!Boolean.TRUE.equals(holder.getOriginPoLine().getIsPackage()) && !Boolean.TRUE.equals(holder.getOriginPoLine().getCheckinItems())) {
      return  pieceDeleteFlowPoLineService.updatePoLine(holder, requestContext);
    }
    return Future.succeededFuture();
  }

  private Future<Void> deleteItem(PieceDeletionHolder holder, RequestContext requestContext) {
    Piece piece = holder.getPieceToDelete();
    if (piece.getItemId() != null) {
      return getOnOrderItemForPiece(piece, requestContext).compose(item -> {
        if (item != null) {
          return inventoryItemManager.deleteItem(piece.getItemId(), true, requestContext);
        }
        return Future.succeededFuture();
      });
    }
    return Future.succeededFuture();
  }

  private boolean isItemWithStatus(JsonObject item, String status) {
    return Optional.ofNullable(item).map(itemP -> item.getJsonObject(ITEM_STATUS))
      .filter(itemStatus -> status.equalsIgnoreCase(itemStatus.getString(ITEM_STATUS_NAME)))
      .isPresent();
  }

  private Future<JsonObject> getOnOrderItemForPiece(Piece piece, RequestContext requestContext) {
    if (StringUtils.isNotEmpty(piece.getItemId())) {
      return inventoryItemManager.getItemRecordById(piece.getItemId(), true, requestContext)
        .map(item -> {
          boolean isOnOrderItem = isItemWithStatus(item, ItemStatus.ON_ORDER.value());
          if (isOnOrderItem) {
            return item;
          }
          return null;
        });
    } else {
      return Future.succeededFuture();
    }
  }

  public Future<List<Void>> batchDeletePiece (PieceCollection entity, RequestContext requestContext) {
    List <String> ids = new ArrayList<>();
    entity.getPieces().stream().forEach(v -> ids.add(v.getId()));
    List<Future<PieceDeletionHolder>> deletionHolders = ids.stream()
      .map(pieceId -> {
        PieceDeletionHolder holder = new PieceDeletionHolder().withDeleteHolding(true);
        return pieceStorageService.getPieceById(pieceId, requestContext)
          .map(pieceToDelete -> {
            holder.withPieceToDelete(pieceToDelete);
            return holder;
          });
      })
      .toList();

    return collectResultsOnSuccess(deletionHolders)
      .compose(holders -> {
        List<Future<Void>> deleteFutures = holders.stream()
          .map(holder -> pieceStorageService.deletePiece(holder.getPieceToDelete().getId(), true, requestContext))
          .toList();

        return collectResultsOnSuccess(deleteFutures);
      });
  }
}
