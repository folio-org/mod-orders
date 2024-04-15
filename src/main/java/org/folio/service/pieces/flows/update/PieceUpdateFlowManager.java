package org.folio.service.pieces.flows.update;

import static org.folio.helper.CheckinReceivePiecesHelper.EXPECTED_STATUSES;
import static org.folio.helper.CheckinReceivePiecesHelper.RECEIVED_STATUSES;
import static org.folio.orders.utils.ProtectedOperationType.UPDATE;
import static org.folio.rest.jaxrs.model.CompositePoLine.ReceiptStatus.AWAITING_RECEIPT;
import static org.folio.rest.jaxrs.model.CompositePoLine.ReceiptStatus.FULLY_RECEIVED;
import static org.folio.rest.jaxrs.model.CompositePoLine.ReceiptStatus.PARTIALLY_RECEIVED;

import java.util.Date;
import java.util.HashMap;
import java.util.List;
import java.util.Map;

import org.apache.logging.log4j.LogManager;
import org.apache.logging.log4j.Logger;
import org.folio.models.pieces.PieceUpdateHolder;
import org.folio.rest.core.models.RequestContext;
import org.folio.rest.jaxrs.model.CompositePoLine;
import org.folio.rest.jaxrs.model.CompositePurchaseOrder;
import org.folio.rest.jaxrs.model.CompositePurchaseOrder.OrderType;
import org.folio.rest.jaxrs.model.CompositePurchaseOrder.WorkflowStatus;
import org.folio.rest.jaxrs.model.Piece;
import org.folio.service.ProtectionService;
import org.folio.service.orders.PurchaseOrderLineService;
import org.folio.service.pieces.PieceService;
import org.folio.service.pieces.PieceStorageService;
import org.folio.service.pieces.flows.BasePieceFlowHolderBuilder;
import org.folio.service.pieces.flows.DefaultPieceFlowsValidator;

import io.vertx.core.Future;
import io.vertx.core.json.JsonObject;

public class PieceUpdateFlowManager {
  private static final Logger logger = LogManager.getLogger(PieceUpdateFlowManager.class);

  private final PieceStorageService pieceStorageService;
  private final PieceService pieceService;
  private final ProtectionService protectionService;
  private final PieceUpdateFlowPoLineService updatePoLineService;
  private final PieceUpdateFlowInventoryManager pieceUpdateFlowInventoryManager;
  private final BasePieceFlowHolderBuilder basePieceFlowHolderBuilder;
  private final DefaultPieceFlowsValidator defaultPieceFlowsValidator;
  private final PurchaseOrderLineService purchaseOrderLineService;

  public PieceUpdateFlowManager(PieceStorageService pieceStorageService, PieceService pieceService, ProtectionService protectionService,
    PieceUpdateFlowPoLineService updatePoLineService, PieceUpdateFlowInventoryManager pieceUpdateFlowInventoryManager,
    BasePieceFlowHolderBuilder basePieceFlowHolderBuilder, DefaultPieceFlowsValidator defaultPieceFlowsValidator,
    PurchaseOrderLineService purchaseOrderLineService) {
    this.pieceStorageService = pieceStorageService;
    this.pieceService = pieceService;
    this.protectionService = protectionService;
    this.updatePoLineService = updatePoLineService;
    this.pieceUpdateFlowInventoryManager = pieceUpdateFlowInventoryManager;
    this.basePieceFlowHolderBuilder = basePieceFlowHolderBuilder;
    this.defaultPieceFlowsValidator = defaultPieceFlowsValidator;
    this.purchaseOrderLineService = purchaseOrderLineService;
  }

  // Flow to update piece
  // 1. Before update, get piece by id from storage and store receiving status
  // 2. Update piece with new content and complete future
  // 3. Create a message and check if receivingStatus is not consistent with storage; if yes - send a message to event bus
  public Future<Void> updatePiece(Piece pieceToUpdate, boolean createItem, boolean deleteHolding, RequestContext requestContext) {
    PieceUpdateHolder holder = new PieceUpdateHolder()
      .withPieceToUpdate(pieceToUpdate)
      .withCreateItem(createItem)
      .withDeleteHolding(deleteHolding);

    return pieceStorageService.getPieceById(pieceToUpdate.getId(), requestContext)
      .map(holder::withPieceFromStorage)
      .compose(aHolder -> basePieceFlowHolderBuilder.updateHolderWithOrderInformation(holder, requestContext))
      .compose(aHolder -> basePieceFlowHolderBuilder.updateHolderWithTitleInformation(holder, requestContext))
      .map(v -> {
        defaultPieceFlowsValidator.isPieceRequestValid(pieceToUpdate, holder.getOriginPoLine(), createItem);
        return null;
      })
      .compose(title -> protectionService.isOperationRestricted(holder.getTitle().getAcqUnitIds(), UPDATE, requestContext))
      .compose(v -> pieceUpdateFlowInventoryManager.processInventory(holder, requestContext))
      .compose(v -> updatePoLine(holder, requestContext))
      .map(v -> {
        Piece.ReceivingStatus receivingStatusStorage = holder.getPieceFromStorage().getReceivingStatus();
        Piece.ReceivingStatus receivingStatusUpdate = holder.getPieceToUpdate().getReceivingStatus();
        logger.debug("receivingStatusStorage -- {}", receivingStatusStorage);
        logger.debug("receivingStatusUpdate -- {}", receivingStatusUpdate);
        if (receivingStatusStorage.compareTo(receivingStatusUpdate) != 0) {
          holder.getPieceToUpdate().setStatusUpdatedDate(new Date());
          return true;
        }
        return false;
      })
      .compose(verifyReceiptStatus -> pieceStorageService.updatePiece(holder.getPieceToUpdate(), requestContext)
        .map(verifyReceiptStatus))
      .map(verifyReceiptStatus -> {
        if (Boolean.TRUE.equals(verifyReceiptStatus)) {
          JsonObject messageToEventBus = new JsonObject();
          messageToEventBus.put("poLineIdUpdate", holder.getPieceToUpdate().getPoLineId());
          pieceService.receiptConsistencyPiecePoLine(messageToEventBus, requestContext);
        }
        return null;
      })
      .onFailure(t -> logger.error("User to update piece with id={}", holder.getPieceToUpdate().getId(), t.getCause()))
      .mapEmpty();
  }

  protected Future<Void> updatePoLine(PieceUpdateHolder holder, RequestContext requestContext) {
    CompositePoLine originPoLine = holder.getOriginPoLine();

    return pieceStorageService.getPiecesByLineId(originPoLine.getId(), requestContext)
      .compose(pieces -> {
        CompositePurchaseOrder order = holder.getOriginPurchaseOrder();
        if (order.getOrderType() != OrderType.ONE_TIME || order.getWorkflowStatus() != WorkflowStatus.OPEN) {
          return Future.succeededFuture();
        }
        List<Piece> piecesToUpdate = List.of(holder.getPieceToUpdate());
        CompositePoLine poLineToSave = holder.getPoLineToSave();
        poLineToSave.setReceiptStatus(calculatePoLineReceiptStatus(originPoLine, pieces, piecesToUpdate));
        return purchaseOrderLineService.saveOrderLine(poLineToSave, requestContext);
      }).compose(v -> {
        if (!Boolean.TRUE.equals(originPoLine.getIsPackage()) &&
          !Boolean.TRUE.equals(originPoLine.getCheckinItems())) {
          return updatePoLineService.updatePoLine(holder, requestContext);
        }
        return Future.succeededFuture();
      });
  }

  CompositePoLine.ReceiptStatus calculatePoLineReceiptStatus(CompositePoLine poLine, List<Piece> fromStorage, List<Piece> toUpdate) {

    // 1. collect all piece statuses
    Map<String, Piece.ReceivingStatus> map = new HashMap<>();
    fromStorage.forEach(piece -> map.put(piece.getId(), piece.getReceivingStatus()));
    toUpdate.forEach(piece -> map.put(piece.getId(), piece.getReceivingStatus()));

    // 2. count received and expected statuses
    long receivedQuantity = map.values().stream().filter(RECEIVED_STATUSES::contains).count();
    long expectedQuantity = map.values().stream().filter(EXPECTED_STATUSES::contains).count();

    logger.info("calculatePoLineReceiptStatus:: POL: {}, received: {}, expected: {}",
      poLine.getId(), receivedQuantity, expectedQuantity);

    return expectedQuantity == 0 ? FULLY_RECEIVED :
      receivedQuantity > 0 ? PARTIALLY_RECEIVED : AWAITING_RECEIPT;
  }

}
