package org.folio.service.orders.flows.update.open;

import static org.folio.orders.utils.HelperUtils.calculateInventoryItemsQuantity;
import static org.folio.orders.utils.HelperUtils.collectResultsOnSuccess;
import static org.folio.orders.utils.RequestContextUtil.createContextWithNewTenantId;

import java.util.ArrayList;
import java.util.Date;
import java.util.List;
import java.util.Optional;
import java.util.concurrent.CompletionException;

import io.vertx.core.Future;
import io.vertx.core.json.JsonObject;
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
import org.folio.rest.jaxrs.model.CompositePurchaseOrder;
import org.folio.rest.jaxrs.model.Piece;
import org.folio.rest.jaxrs.model.Title;
import org.folio.service.ProtectionService;
import org.folio.service.inventory.InventoryHoldingManager;
import org.folio.service.inventory.InventoryItemManager;
import org.folio.service.orders.PurchaseOrderStorageService;
import org.folio.service.pieces.PieceChangeReceiptStatusPublisher;
import org.folio.service.pieces.PieceStorageService;
import org.folio.service.titles.TitlesService;

public class OpenCompositeOrderPieceService {
  private static final Logger logger = LogManager.getLogger(OpenCompositeOrderPieceService.class);

  private final InventoryItemManager inventoryItemManager;
  private final InventoryHoldingManager inventoryHoldingManager;
  private final PieceStorageService pieceStorageService;
  private final PieceChangeReceiptStatusPublisher receiptStatusPublisher;
  private final PurchaseOrderStorageService purchaseOrderStorageService;
  private final ProtectionService protectionService;
  private final OpenCompositeOrderHolderBuilder openCompositeOrderHolderBuilder;
  private final TitlesService titlesService;

  public OpenCompositeOrderPieceService(PurchaseOrderStorageService purchaseOrderStorageService,
                                        PieceStorageService pieceStorageService,
                                        ProtectionService protectionService,
                                        PieceChangeReceiptStatusPublisher receiptStatusPublisher,
                                        InventoryItemManager inventoryItemManager,
                                        InventoryHoldingManager inventoryHoldingManager,
                                        TitlesService titlesService,
                                        OpenCompositeOrderHolderBuilder openCompositeOrderHolderBuilder) {
    this.purchaseOrderStorageService = purchaseOrderStorageService;
    this.pieceStorageService = pieceStorageService;
    this.protectionService = protectionService;
    this.receiptStatusPublisher = receiptStatusPublisher;
    this.inventoryItemManager = inventoryItemManager;
    this.inventoryHoldingManager = inventoryHoldingManager;
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
    logger.debug("handlePieces:: Get pieces by poLine ID - {}", compPOL.getId());
    return openCompositeOrderHolderBuilder.buildHolder(compPOL, titleId, expectedPiecesWithItem, requestContext)
      .compose(holder -> {
        var piecesWithChangedLocation = holder.getPiecesWithChangedLocation();
        if (CollectionUtils.isEmpty(piecesWithChangedLocation) || piecesWithChangedLocation.size() != holder.getPiecesWithLocationToProcess().size()) {
          return purchaseOrderStorageService.getCompositeOrderById(compPOL.getPurchaseOrderId(), requestContext)
            .compose(order -> createPieces(holder, order, isInstanceMatchingDisabled, requestContext));
        }
        return updatePieces(holder, requestContext);
      })
      .map(pieces -> validateItemsCreationForPieces(pieces, compPOL, expectedPiecesWithItem.size()));
  }

  private Future<List<Piece>> updatePieces(OpenOrderPieceHolder holder, RequestContext requestContext) {
    logger.debug("updatePieces:: Trying to update pieces");
    return collectResultsOnSuccess(holder.getPiecesWithChangedLocation().stream()
      .map(piece -> updatePiece(piece, requestContext))
      .toList());
  }

  private Future<List<Piece>> createPieces(OpenOrderPieceHolder holder, CompositePurchaseOrder order, boolean isInstanceMatchingDisabled, RequestContext requestContext) {
    logger.debug("createPieces:: Trying to create pieces");
    List<Piece> piecesToCreate = new ArrayList<>(holder.getPiecesWithLocationToProcess());
    piecesToCreate.addAll(holder.getPiecesWithHoldingToProcess());
    piecesToCreate.addAll(holder.getPiecesWithoutLocationId());

    var piecesToCreateFutures = piecesToCreate.stream()
      .map(piece -> piece.withTitleId(holder.getTitleId()))
      .map(piece -> createPiece(piece, order, isInstanceMatchingDisabled, requestContext))
      .toList();
    return collectResultsOnSuccess(piecesToCreateFutures)
       .recover(th -> {
         logger.error("Piece creation failed", th);
         throw new CompletionException("Piece creation error", th);
      });
  }

  public Future<Piece> createPiece(Piece piece, CompositePurchaseOrder order, boolean isInstanceMatchingDisabled, RequestContext requestContext) {
    logger.debug("createPiece:: Creating piece - {}", piece.getId());
    return titlesService.getTitleById(piece.getTitleId(), requestContext)
      .compose(title -> protectionService.isOperationRestricted(title.getAcqUnitIds(), ProtectedOperationType.CREATE, requestContext))
      .compose(v -> openOrderUpdateInventory(order, order.getCompositePoLines().get(0), piece, isInstanceMatchingDisabled, requestContext))
      .compose(v -> pieceStorageService.insertPiece(piece, requestContext));
  }

  public Future<Piece> updatePiece(Piece piece, RequestContext requestContext) {
    logger.debug("updatePiece:: Updating piece - {}", piece.getId());
    return titlesService.getTitleById(piece.getTitleId(), requestContext)
      .compose(title -> protectionService.isOperationRestricted(title.getAcqUnitIds(), ProtectedOperationType.UPDATE, requestContext))
      .compose(vVoid -> pieceStorageService.getPieceById(piece.getId(), requestContext))
      .compose(pieceStorage -> inventoryItemManager.updateItemWithPieceFields(pieceStorage, piece, requestContext)
        .compose(aVoid -> {
          var receivingStatusUpdate = piece.getReceivingStatus();
          var receivingStatusStorage = pieceStorage.getReceivingStatus();
          boolean isReceivingStatusChanged = !receivingStatusStorage.equals(receivingStatusUpdate);
          if (isReceivingStatusChanged) {
            piece.setStatusUpdatedDate(new Date());
          }
          return pieceStorageService.updatePiece(piece, requestContext)
            .compose(v -> {
              logger.debug("updatePiece:: receivingStatusStorage - {}, receivingStatusUpdate - {}", receivingStatusStorage, receivingStatusUpdate);
              if (isReceivingStatusChanged) {
                var messageToEventBus = JsonObject.of("poLineIdUpdate", piece.getPoLineId());
                receiptStatusPublisher.sendEvent(MessageAddress.RECEIPT_STATUS, messageToEventBus, requestContext);
              }
              return Future.succeededFuture();
            })
            .onFailure(e -> logger.error("Error updating piece by id to storage {}", piece.getId(), e));
        })
      ).onFailure(e -> logger.error("Error getting piece by id from storage {}", piece.getId(), e))
      .map(v -> piece);
  }

  /**
   * Creates Inventory records associated with given PO line and updates PO line with corresponding links.
   *
   * @param compPOL Composite PO line to update Inventory for
   * @return CompletableFuture with void.
   */
  public Future<Void> openOrderUpdateInventory(CompositePurchaseOrder compPO, CompositePoLine compPOL,
                                               Piece piece, boolean isInstanceMatchingDisabled, RequestContext requestContext) {
    if (!Boolean.TRUE.equals(compPOL.getIsPackage())) {
      return inventoryItemManager.updateItemWithPieceFields(null, piece, requestContext);
    }
    var locationContext = createContextWithNewTenantId(requestContext, piece.getReceivingTenantId());
    return titlesService.getTitleById(piece.getTitleId(), requestContext)
      .compose(title -> titlesService.updateTitleWithInstance(title, isInstanceMatchingDisabled, locationContext, requestContext).map(title::withInstanceId))
      .compose(title -> getOrCreateHolding(compPOL, piece, title, locationContext))
      .compose(holdingId -> updateItemsIfNeeded(compPO, compPOL, holdingId, locationContext))
      .map(itemId -> Optional.ofNullable(itemId).map(piece::withItemId))
      .mapEmpty();
  }

  private Future<String> getOrCreateHolding(CompositePoLine compPOL, Piece piece, Title title, RequestContext locationContext) {
    if (piece.getHoldingId() != null || !PoLineCommonUtil.isHoldingsUpdateRequired(compPOL)) {
      return Future.succeededFuture(piece.getHoldingId());
    }
    return inventoryHoldingManager.createHoldingAndReturnId(title.getInstanceId(), piece.getLocationId(), locationContext)
      .map(holdingId -> piece.withLocationId(null).withHoldingId(holdingId).getHoldingId());
  }

  private Future<String> updateItemsIfNeeded(CompositePurchaseOrder compPO, CompositePoLine compPOL,
                                             String holdingId, RequestContext locationContext) {
    return PoLineCommonUtil.isItemsUpdateRequired(compPOL)
      ? inventoryItemManager.openOrderCreateItemRecord(compPO, compPOL, holdingId, locationContext)
      : Future.succeededFuture();
  }

  private List<Piece> validateItemsCreationForPieces(List<Piece> pieces, CompositePoLine compPOL, int itemsSize) {
    int expectedItemsQuantity = calculateInventoryItemsQuantity(compPOL);
    if (itemsSize == expectedItemsQuantity) {
      return pieces;
    }
    throw new InventoryException(String.format("Error creating items for PO Line with '%s' id. Expected %d but %d created",
      compPOL.getId(), expectedItemsQuantity, itemsSize));
  }
}
