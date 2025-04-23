package org.folio.service.pieces.flows.create;

import static org.folio.orders.utils.RequestContextUtil.createContextWithNewTenantId;

import java.util.Optional;

import io.vertx.core.Future;
import org.apache.logging.log4j.LogManager;
import org.apache.logging.log4j.Logger;
import org.folio.orders.utils.PoLineCommonUtil;
import org.folio.rest.core.models.RequestContext;
import org.folio.rest.jaxrs.model.PoLine;
import org.folio.rest.jaxrs.model.CompositePurchaseOrder;
import org.folio.rest.jaxrs.model.Location;
import org.folio.rest.jaxrs.model.Piece;
import org.folio.service.inventory.InventoryHoldingManager;
import org.folio.service.pieces.PieceUpdateInventoryService;
import org.folio.service.pieces.flows.DefaultPieceFlowsValidator;
import org.folio.service.titles.TitlesService;

public class PieceCreateFlowInventoryManager {
  private static final Logger logger = LogManager.getLogger(PieceCreateFlowInventoryManager.class);

  private final TitlesService titlesService;
  private final PieceUpdateInventoryService pieceUpdateInventoryService;
  private final InventoryHoldingManager inventoryHoldingManager;

  public PieceCreateFlowInventoryManager(TitlesService titlesService,
                                         PieceUpdateInventoryService pieceUpdateInventoryService,
                                         InventoryHoldingManager inventoryHoldingManager) {
    this.titlesService = titlesService;
    this.pieceUpdateInventoryService = pieceUpdateInventoryService;
    this.inventoryHoldingManager = inventoryHoldingManager;
  }

  public Future<Void> processInventory(CompositePurchaseOrder compPO, PoLine poLine,
                                       Piece piece, boolean createItem, RequestContext requestContext) {
    var locationContext = createContextWithNewTenantId(requestContext, piece.getReceivingTenantId());
    return updateInventoryInstanceForPoLine(poLine, piece, locationContext, requestContext)
      .compose(instanceId -> handleHolding(poLine, piece, instanceId, locationContext))
      .compose(holdingId -> handleItem(compPO, poLine, createItem, piece, locationContext))
      .map(itemId -> Optional.ofNullable(itemId).map(piece::withItemId))
      .onSuccess(optional -> logger.info("processInventory:: successfully created inventory for piece with itemId: {}, poLineId: {}, receivingTenantId: {}",
        piece.getItemId(), piece.getPoLineId(), piece.getReceivingTenantId()))
      .onFailure(t -> logger.error("Failed to create inventory for piece with itemId: {}, poLineId: {}, receivingTenantId: {}",
        piece.getItemId(), piece.getPoLineId(), piece.getReceivingTenantId(), t))
      .mapEmpty();
  }

  private Future<String> updateInventoryInstanceForPoLine(PoLine poLine, Piece piece, RequestContext locationContext, RequestContext requestContext) {
    if (!Boolean.TRUE.equals(poLine.getIsPackage())) {
      return Optional.ofNullable(getPoLineInstanceId(poLine))
        .orElseGet(() -> titlesService.updateTitleWithInstance(piece.getTitleId(), locationContext, requestContext))
        .map(instanceId -> poLine.withInstanceId(instanceId).getInstanceId());
    }
    return titlesService.updateTitleWithInstance(piece.getTitleId(), locationContext, requestContext);
  }

  private Future<String> getPoLineInstanceId(PoLine poLine) {
    return poLine.getInstanceId() != null || PoLineCommonUtil.isInventoryUpdateNotRequired(poLine)
      ? Future.succeededFuture(poLine.getInstanceId())
      : null;
  }

  private Future<Location> handleHolding(PoLine poLine, Piece piece, String instanceId, RequestContext requestContext) {
    if (piece.getHoldingId() != null) {
      return Future.succeededFuture(new Location().withHoldingId(piece.getHoldingId()));
    }

    var location = new Location().withLocationId(piece.getLocationId());
    if (instanceId == null || !DefaultPieceFlowsValidator.isCreateHoldingForPiecePossible(piece, poLine)) {
      return Future.succeededFuture(location);
    }
    return inventoryHoldingManager.createHoldingAndReturnId(instanceId, piece.getLocationId(), requestContext)
      .map(holdingId -> {
        if (holdingId != null) {
          piece.withLocationId(null).setHoldingId(holdingId);
          location.withLocationId(null).setHoldingId(holdingId);
        }
        return location;
      });
  }

  private Future<String> handleItem(CompositePurchaseOrder compPO, PoLine poLine, boolean createItem,
                                    Piece piece, RequestContext requestContext) {
    return piece.getItemId() != null || !createItem || piece.getHoldingId() == null
      ? Future.succeededFuture(piece.getItemId())
      : pieceUpdateInventoryService.manualPieceFlowCreateItemRecord(piece, compPO, poLine, requestContext);
  }

}
