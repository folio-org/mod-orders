package org.folio.service.orders.flows.update.open;

import static java.util.stream.Collectors.toList;
import static org.folio.orders.utils.HelperUtils.calculateInventoryItemsQuantity;
import static org.folio.orders.utils.HelperUtils.collectResultsOnSuccess;

import java.util.ArrayList;
import java.util.Date;
import java.util.List;
import java.util.Optional;
import java.util.concurrent.CompletionException;

import org.apache.commons.collections4.CollectionUtils;
import org.apache.logging.log4j.LogManager;
import org.apache.logging.log4j.Logger;
import org.folio.models.pieces.OpenOrderPieceHolder;
import org.folio.orders.events.handlers.MessageAddress;
import org.folio.orders.utils.PoLineCommonUtil;
import org.folio.orders.utils.ProtectedOperationType;
import org.folio.rest.core.exceptions.InventoryException;
import org.folio.rest.core.models.RequestContext;
import org.folio.rest.jaxrs.model.CompositePoLine;
import org.folio.rest.jaxrs.model.Location;
import org.folio.rest.jaxrs.model.Piece;
import org.folio.service.ProtectionService;
import org.folio.service.inventory.InventoryManager;
import org.folio.service.orders.PurchaseOrderStorageService;
import org.folio.service.pieces.PieceChangeReceiptStatusPublisher;
import org.folio.service.pieces.PieceStorageService;
import org.folio.service.titles.TitlesService;

import io.vertx.core.Future;
import io.vertx.core.Promise;
import io.vertx.core.json.JsonObject;

public class OpenCompositeOrderPieceService {
  private static final Logger logger = LogManager.getLogger(OpenCompositeOrderPieceService.class);

  private final InventoryManager inventoryManager;
  private final PieceStorageService pieceStorageService;
  private final PieceChangeReceiptStatusPublisher receiptStatusPublisher;
  private final PurchaseOrderStorageService purchaseOrderStorageService;
  private final ProtectionService protectionService;
  private final OpenCompositeOrderHolderBuilder openCompositeOrderHolderBuilder;
  private final TitlesService titlesService;

  public OpenCompositeOrderPieceService(PurchaseOrderStorageService purchaseOrderStorageService,
        PieceStorageService pieceStorageService, ProtectionService protectionService, PieceChangeReceiptStatusPublisher receiptStatusPublisher,
        InventoryManager inventoryManager, TitlesService titlesService, OpenCompositeOrderHolderBuilder openCompositeOrderHolderBuilder) {
    this.purchaseOrderStorageService = purchaseOrderStorageService;
    this.pieceStorageService = pieceStorageService;
    this.protectionService = protectionService;
    this.receiptStatusPublisher = receiptStatusPublisher;
    this.inventoryManager = inventoryManager;
    this.titlesService = titlesService;
    this.openCompositeOrderHolderBuilder = openCompositeOrderHolderBuilder;
  }

  /**
   * Creates pieces that are not yet in storage
   *
   * @param compPOL PO line to create Pieces Records for
   * @param expectedPiecesWithItem expected Pieces to create with created associated Items records
   * @return void future
   */
  public Future<List<Piece>> handlePieces(CompositePoLine compPOL, String titleId, List<Piece> expectedPiecesWithItem,
                                                     boolean isInstanceMatchingDisabled, RequestContext requestContext) {
    logger.debug("Get pieces by poLine ID");
    return openCompositeOrderHolderBuilder.buildHolder(compPOL, titleId, expectedPiecesWithItem, requestContext)
      .compose(holder -> {
        if (CollectionUtils.isNotEmpty(holder.getPiecesWithLocationToProcess()) &&
                       (holder.getPiecesWithChangedLocation().size() == holder.getPiecesWithLocationToProcess().size())) {
          return updatePieces(holder, requestContext);
        } else {
          return createPieces(holder, isInstanceMatchingDisabled, requestContext);
        }
      })
      .map(pieces -> {
        int createdItemsQuantity = expectedPiecesWithItem.size();
        validateItemsCreation(compPOL, createdItemsQuantity);
        return pieces;
      });
  }

  private Future<List<Piece>> updatePieces(OpenOrderPieceHolder holder, RequestContext requestContext) {
    List<Future<Piece>> updateFutures = holder.getPiecesWithChangedLocation().stream()
      .map(piece -> updatePieceRecord(piece, requestContext).map(v -> piece))
      .collect(toList());
    return collectResultsOnSuccess(updateFutures);
  }

  private Future<List<Piece>> createPieces(OpenOrderPieceHolder holder, boolean isInstanceMatchingDisabled, RequestContext requestContext) {
    List<Piece> piecesToCreate = new ArrayList<>(holder.getPiecesWithLocationToProcess());
    piecesToCreate.addAll(holder.getPiecesWithHoldingToProcess());
    piecesToCreate.addAll(holder.getPiecesWithoutLocationId());
    piecesToCreate.forEach(piece -> piece.setTitleId(holder.getTitleId()));
    logger.debug("Trying to create pieces");
    List<Future<Piece>> piecesToCreateFutures = new ArrayList<>();
    piecesToCreate.forEach(piece ->
      piecesToCreateFutures.add(createPiece(piece, isInstanceMatchingDisabled, requestContext))
    );
    return collectResultsOnSuccess(piecesToCreateFutures)
       .recover(th -> {
        logger.error("Piece creation error");
        throw new CompletionException("Piece creation error", th);
      });
  }

  public Future<Piece> createPiece(Piece piece, boolean isInstanceMatchingDisabled, RequestContext requestContext) {
    logger.debug("createPiece start");
    return purchaseOrderStorageService.getCompositeOrderByPoLineId(piece.getPoLineId(), requestContext)
      .compose(order -> protectionService.isOperationRestricted(order.getAcqUnitIds(), ProtectedOperationType.CREATE, requestContext)
        .map(v -> order))
      .compose(order -> openOrderUpdateInventory(order.getCompositePoLines().get(0), piece, isInstanceMatchingDisabled, requestContext))
      .compose(v -> pieceStorageService.insertPiece(piece, requestContext));
  }

  public Future<Void> updatePieceRecord(Piece piece, RequestContext requestContext) {
    Promise<Void> promise = Promise.promise();
    purchaseOrderStorageService.getCompositeOrderByPoLineId(piece.getPoLineId(), requestContext)
      .compose(order -> protectionService.isOperationRestricted(order.getAcqUnitIds(), ProtectedOperationType.UPDATE, requestContext))
      .compose(v -> inventoryManager.updateItemWithPieceFields(piece, requestContext))
      .onSuccess(vVoid ->
        pieceStorageService.getPieceById(piece.getId(), requestContext).onSuccess(pieceStorage -> {
            Piece.ReceivingStatus receivingStatusUpdate = piece.getReceivingStatus();
            Piece.ReceivingStatus receivingStatusStorage = pieceStorage.getReceivingStatus();
            boolean isReceivingStatusChanged = receivingStatusStorage.compareTo(receivingStatusUpdate) != 0;

            if(isReceivingStatusChanged) {
              piece.setStatusUpdatedDate(new Date());
            }

            pieceStorageService.updatePiece(piece, requestContext)
              .onSuccess(ok -> {
                promise.complete();
                JsonObject messageToEventBus = new JsonObject();
                messageToEventBus.put("poLineIdUpdate", piece.getPoLineId());
                logger.debug("receivingStatusStorage -- {}", receivingStatusStorage);
                logger.debug("receivingStatusUpdate -- {}", receivingStatusUpdate);

                if (isReceivingStatusChanged) {
                  receiptStatusPublisher.sendEvent(MessageAddress.RECEIPT_STATUS, messageToEventBus, requestContext);
                }
              })
              .onFailure(e -> {
                logger.error("Error updating piece by id to storage {}", piece.getId(), e);
                promise.fail(e);
              });
          })
          .onFailure(e -> {
            logger.error("Error getting piece by id from storage {}", piece.getId(), e);
            promise.fail(e);
          })
      )
      .onFailure(t -> {
        logger.error("User to update piece with id={}", piece.getId(), t.getCause());
         promise.fail(t);
      });
    return promise.future();
  }

  /**
   * Creates Inventory records associated with given PO line and updates PO line with corresponding links.
   *
   * @param compPOL Composite PO line to update Inventory for
   * @return CompletableFuture with void.
   */
  public Future<Void> openOrderUpdateInventory(CompositePoLine compPOL, Piece piece, boolean isInstanceMatchingDisabled, RequestContext requestContext) {
    if (Boolean.TRUE.equals(compPOL.getIsPackage())) {
      return titlesService.getTitleById(piece.getTitleId(), requestContext)
        .compose(title -> inventoryManager.openOrderHandlePackageLineInstance(title, isInstanceMatchingDisabled, requestContext))
        .compose(title -> titlesService.saveTitle(title, requestContext).map(json -> title))
        .compose(title ->
        {
          if (piece.getHoldingId() != null) {
            return Future.succeededFuture(piece.getHoldingId());
          }
          return inventoryManager.handleHoldingsRecord(compPOL, new Location().withLocationId(piece.getLocationId()), title.getInstanceId(), requestContext)
            .map(holdingId -> {
              piece.setLocationId(null);
              piece.setHoldingId(holdingId);
              return holdingId;
            });
        })
        .compose(holdingId -> {
          if (PoLineCommonUtil.isItemsUpdateRequired(compPOL)) {
            return inventoryManager.openOrderCreateItemRecord(compPOL, holdingId, requestContext);
          }
          return Future.succeededFuture();
        })
        .onSuccess(itemId -> Optional.ofNullable(itemId).ifPresent(piece::withItemId))
        .mapEmpty();
    }
    else
    {
      return inventoryManager.updateItemWithPieceFields(piece, requestContext);
    }
  }


  private void validateItemsCreation(CompositePoLine compPOL, int itemsSize) {
    int expectedItemsQuantity = calculateInventoryItemsQuantity(compPOL);
    if (itemsSize != expectedItemsQuantity) {
      String message = String.format("Error creating items for PO Line with '%s' id. Expected %d but %d created",
        compPOL.getId(), expectedItemsQuantity, itemsSize);
      throw new InventoryException(message);
    }
  }
}
