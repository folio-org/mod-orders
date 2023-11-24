package org.folio.service.pieces.flows.update;

import static org.folio.orders.utils.ProtectedOperationType.UPDATE;

import java.util.Date;

import io.vertx.core.Future;
import io.vertx.core.Promise;
import io.vertx.core.json.JsonObject;
import org.apache.logging.log4j.LogManager;
import org.apache.logging.log4j.Logger;
import org.folio.models.pieces.PieceUpdateHolder;
import org.folio.rest.core.models.RequestContext;
import org.folio.rest.jaxrs.model.Piece;
import org.folio.service.ProtectionService;
import org.folio.service.pieces.PieceService;
import org.folio.service.pieces.PieceStorageService;
import org.folio.service.pieces.flows.BasePieceFlowHolderBuilder;
import org.folio.service.pieces.flows.DefaultPieceFlowsValidator;

public class PieceUpdateFlowManager {
  private static final Logger logger = LogManager.getLogger(PieceUpdateFlowManager.class);

  private final PieceStorageService pieceStorageService;
  private final PieceService pieceService;
  private final ProtectionService protectionService;
  private final PieceUpdateFlowPoLineService updatePoLineService;
  private final PieceUpdateFlowInventoryManager pieceUpdateFlowInventoryManager;
  private final BasePieceFlowHolderBuilder basePieceFlowHolderBuilder;
  private final DefaultPieceFlowsValidator defaultPieceFlowsValidator;

  public PieceUpdateFlowManager(PieceStorageService pieceStorageService, PieceService pieceService, ProtectionService protectionService,
    PieceUpdateFlowPoLineService updatePoLineService, PieceUpdateFlowInventoryManager pieceUpdateFlowInventoryManager,
    BasePieceFlowHolderBuilder basePieceFlowHolderBuilder, DefaultPieceFlowsValidator defaultPieceFlowsValidator) {
    this.pieceStorageService = pieceStorageService;
    this.pieceService = pieceService;
    this.protectionService = protectionService;
    this.updatePoLineService = updatePoLineService;
    this.pieceUpdateFlowInventoryManager = pieceUpdateFlowInventoryManager;
    this.basePieceFlowHolderBuilder = basePieceFlowHolderBuilder;
    this.defaultPieceFlowsValidator = defaultPieceFlowsValidator;
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

    Promise<Void> promise = Promise.promise();
    pieceStorageService.getPieceById(pieceToUpdate.getId(), requestContext)
      .onSuccess(holder::withPieceFromStorage)
      .compose(aHolder -> basePieceFlowHolderBuilder.updateHolderWithOrderInformation(holder, requestContext))
      .compose(aHolder -> basePieceFlowHolderBuilder.updateHolderWithTitleInformation(holder, requestContext))
      .map(v -> {
        defaultPieceFlowsValidator.isPieceRequestValid(pieceToUpdate, holder.getOriginPoLine(), createItem);
        return null;
      })
      .compose(title -> protectionService.isOperationRestricted(holder.getTitle().getAcqUnitIds(), UPDATE, requestContext))
      .compose(v -> pieceUpdateFlowInventoryManager.processInventory(holder, requestContext))
      .compose(vVoid -> updatePoLine(holder, requestContext))
      .map(afterUpdate -> {
        JsonObject messageToEventBus = new JsonObject();
        messageToEventBus.put("poLineIdUpdate", holder.getPieceToUpdate().getPoLineId());
        Piece.ReceivingStatus receivingStatusStorage = holder.getPieceFromStorage().getReceivingStatus();
        Piece.ReceivingStatus receivingStatusUpdate = holder.getPieceToUpdate().getReceivingStatus();
        logger.debug("receivingStatusStorage -- {}", receivingStatusStorage);
        logger.debug("receivingStatusUpdate -- {}", receivingStatusUpdate);
        if (receivingStatusStorage.compareTo(receivingStatusUpdate) != 0) {
          holder.getPieceToUpdate().setStatusUpdatedDate(new Date());
          pieceService.receiptConsistencyPiecePoLine(messageToEventBus, requestContext);
        }
        return null;
      })
      .compose(aVoid -> pieceStorageService.updatePiece(holder.getPieceToUpdate(), requestContext))
      .onSuccess(promise::complete)
      .onFailure(t -> {
        logger.error("User to update piece with id={}", holder.getPieceToUpdate().getId(), t.getCause());
        promise.fail(t);
      });
    return promise.future();
  }

  protected Future<Void> updatePoLine(PieceUpdateHolder holder, RequestContext requestContext) {
    if (!Boolean.TRUE.equals(holder.getOriginPoLine().getIsPackage()) && !Boolean.TRUE.equals(holder.getOriginPoLine().getCheckinItems())) {
      return updatePoLineService.updatePoLine(holder, requestContext);
    }
    return Future.succeededFuture();
  }

}
