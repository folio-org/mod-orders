package org.folio.service.orders.flows.update.open;

import static org.folio.orders.events.utils.EventUtils.createPoLineUpdateEvent;
import static org.folio.orders.utils.HelperUtils.calculateInventoryItemsQuantity;
import static org.folio.orders.utils.HelperUtils.chainCall;
import static org.folio.orders.utils.HelperUtils.collectResultsOnSuccess;
import static org.folio.orders.utils.RequestContextUtil.createContextWithNewTenantId;
import static org.folio.service.pieces.PieceUtil.updatePieceStatus;

import java.util.ArrayList;
import java.util.List;
import java.util.Optional;
import java.util.concurrent.CompletionException;

import io.vertx.core.Future;
import org.apache.commons.collections4.CollectionUtils;
import org.apache.logging.log4j.LogManager;
import org.apache.logging.log4j.Logger;
import org.folio.models.pieces.OpenOrderPieceHolder;
import org.folio.orders.events.handlers.MessageAddress;
import org.folio.orders.utils.PoLineCommonUtil;
import org.folio.orders.utils.ProtectedOperationType;
import org.folio.rest.core.exceptions.InventoryException;
import org.folio.rest.core.models.RequestContext;
import org.folio.rest.jaxrs.model.PoLine;
import org.folio.rest.jaxrs.model.CompositePurchaseOrder;
import org.folio.rest.jaxrs.model.Piece;
import org.folio.rest.jaxrs.model.PieceCollection;
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
   * @param poLine PO line to create Pieces Records for
   * @param expectedPiecesWithItem expected Pieces to create with created associated Items records
   * @return void future
   */
  public Future<List<Piece>> handlePieces(PoLine poLine, String titleId, List<Piece> expectedPiecesWithItem,
                                          boolean isInstanceMatchingDisabled, RequestContext requestContext) {
    logger.debug("OpenCompositeOrderPieceService.handlePieces:: Get pieces by poLine ID - {}", poLine.getId());
    return openCompositeOrderHolderBuilder.buildHolder(poLine, titleId, expectedPiecesWithItem, requestContext)
      .compose(holder -> {
        var piecesWithChangedLocation = holder.getPiecesWithChangedLocation();
        if (CollectionUtils.isEmpty(piecesWithChangedLocation) || piecesWithChangedLocation.size() != holder.getPiecesWithLocationToProcess().size()) {
          return purchaseOrderStorageService.getCompositeOrderById(poLine.getPurchaseOrderId(), requestContext)
            .compose(order -> createPieces(holder, order, isInstanceMatchingDisabled, requestContext));
        }
        return updatePieces(holder, requestContext);
      })
      .map(pieces -> validateItemsCreationForPieces(pieces, poLine, expectedPiecesWithItem.size()));
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

    List<Piece> preparedPieces = piecesToCreate.stream()
      .map(piece -> piece.withTitleId(holder.getTitleId()))
      .toList();

    // Collect pieces after validation
    List<Future<Piece>> piecesFutures = new ArrayList<>();

    // Use chainCallCollect to sequentially process each piece with openOrderUpdateInventory.
    return chainCall(preparedPieces, piece ->
      openOrderUpdateInventory(piece, order, isInstanceMatchingDisabled, requestContext)
        .map(validatedPiece -> piecesFutures.add(Future.succeededFuture(validatedPiece)))
      )
      .compose(ignored -> collectResultsOnSuccess(piecesFutures))
      .compose(validatedPieces -> {
        logger.info("createPieces:: Passed acq unit validation and open order '{}' inventory update", order.getId());
        // Once all pieces have been updated, insert them in batch.
        return pieceStorageService.insertPiecesBatch(validatedPieces, requestContext)
          .map(PieceCollection::getPieces);
      })
      .recover(th -> {
        logger.error("Piece creation failed", th);
        return Future.failedFuture(new CompletionException("Piece creation error", th));
      });
  }


  private Future<Piece> openOrderUpdateInventory(Piece piece, CompositePurchaseOrder order, boolean isInstanceMatchingDisabled, RequestContext requestContext) {
    logger.debug("openOrderUpdateInventory:: Validating acq unit and updating order '{}' inventory - {}", order.getId(), piece.getId());
    return titlesService.getTitleById(piece.getTitleId(), requestContext)
      .compose(title ->
        protectionService.isOperationRestricted(title.getAcqUnitIds(), ProtectedOperationType.CREATE, requestContext)
      )
      .compose(v ->
        openOrderUpdateInventory(order, order.getPoLines().getFirst(), piece, isInstanceMatchingDisabled, requestContext)
      )
      .map(v -> piece);
  }

  public Future<Piece> updatePiece(Piece piece, RequestContext requestContext) {
    logger.debug("updatePiece:: Updating piece - {}", piece.getId());
    return titlesService.getTitleById(piece.getTitleId(), requestContext)
      .compose(title -> protectionService.isOperationRestricted(title.getAcqUnitIds(), ProtectedOperationType.UPDATE, requestContext))
      .compose(v -> inventoryItemManager.updateItemWithPieceFields(piece, requestContext))
      .compose(vVoid -> pieceStorageService.getPieceById(piece.getId(), requestContext))
      .compose(pieceStorage -> {
        var isReceivingStatusChanged = updatePieceStatus(piece, pieceStorage.getReceivingStatus(), piece.getReceivingStatus());
        return pieceStorageService.updatePiece(piece, requestContext)
          .compose(v -> {
            logger.debug("updatePiece:: Status updated from: {} to {}", pieceStorage.getReceivingStatus(), piece.getReceivingStatus());
            if (isReceivingStatusChanged) {
              receiptStatusPublisher.sendEvent(MessageAddress.RECEIPT_STATUS, createPoLineUpdateEvent(piece.getPoLineId()), requestContext);
            }
            return Future.succeededFuture();
          })
          .onFailure(e -> logger.error("Error updating piece by id to storage {}", piece.getId(), e));
      })
      .onFailure(e -> logger.error("Error getting piece by id from storage {}", piece.getId(), e))
      .map(v -> piece);
  }

  /**
   * Creates Inventory records associated with given PO line and updates PO line with corresponding links.
   *
   * @param poLine PO line to update Inventory for
   * @return CompletableFuture with void.
   */
  public Future<Void> openOrderUpdateInventory(CompositePurchaseOrder compPO, PoLine poLine,
                                               Piece piece, boolean isInstanceMatchingDisabled, RequestContext requestContext) {
    logger.debug("OpenCompositeOrderPieceService.openOrderUpdateInventory poLine.id={}", poLine.getId());
    if (!Boolean.TRUE.equals(poLine.getIsPackage())) {
      return inventoryItemManager.updateItemWithPieceFields(piece, requestContext);
    }
    var locationContext = createContextWithNewTenantId(requestContext, piece.getReceivingTenantId());
    var suppressDiscovery = Optional.ofNullable(poLine.getSuppressInstanceFromDiscovery()).orElse(false);
    return titlesService.getTitleById(piece.getTitleId(), requestContext)
      .compose(title -> titlesService.updateTitleWithInstance(title, isInstanceMatchingDisabled, suppressDiscovery, locationContext, requestContext).map(title::withInstanceId))
      .compose(title -> getOrCreateHolding(poLine, piece, title, locationContext))
      .compose(holdingId -> updateItemsIfNeeded(compPO, poLine, holdingId, locationContext))
      .map(itemId -> Optional.ofNullable(itemId).map(piece::withItemId))
      .mapEmpty();
  }

  private Future<String> getOrCreateHolding(PoLine poLine, Piece piece, Title title, RequestContext locationContext) {
    logger.debug("OpenCompositeOrderPieceService.getOrCreateHolding poLine.id={}", poLine.getId());
    if (piece.getHoldingId() != null || !PoLineCommonUtil.isHoldingsUpdateRequired(poLine)) {
      return Future.succeededFuture(piece.getHoldingId());
    }
    return inventoryHoldingManager.createHoldingAndReturnId(title.getInstanceId(), piece.getLocationId(), locationContext)
      .map(holdingId -> piece.withLocationId(null).withHoldingId(holdingId).getHoldingId());
  }

  private Future<String> updateItemsIfNeeded(CompositePurchaseOrder compPO, PoLine poLine,
                                             String holdingId, RequestContext locationContext) {
    return PoLineCommonUtil.isItemsUpdateRequired(poLine)
      ? inventoryItemManager.openOrderCreateItemRecord(compPO, poLine, holdingId, locationContext)
      : Future.succeededFuture();
  }

  private List<Piece> validateItemsCreationForPieces(List<Piece> pieces, PoLine poLine, int itemsSize) {
    int expectedItemsQuantity = calculateInventoryItemsQuantity(poLine);
    if (itemsSize == expectedItemsQuantity) {
      return pieces;
    }
    throw new InventoryException(String.format("Error creating items for PO Line with '%s' id. Expected %d but %d created",
      poLine.getId(), expectedItemsQuantity, itemsSize));
  }
}
