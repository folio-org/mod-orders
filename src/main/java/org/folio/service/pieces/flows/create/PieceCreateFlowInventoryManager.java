package org.folio.service.pieces.flows.create;

import static java.util.concurrent.CompletableFuture.completedFuture;
import static org.folio.rest.core.exceptions.ErrorCodes.CREATE_HOLDING_WITHOUT_INSTANCE_ERROR;

import java.util.Optional;
import java.util.concurrent.CompletableFuture;

import org.apache.logging.log4j.LogManager;
import org.apache.logging.log4j.Logger;
import org.folio.models.pieces.PieceCreationHolder;
import org.folio.orders.utils.PoLineCommonUtil;
import org.folio.rest.RestConstants;
import org.folio.rest.core.exceptions.HttpException;
import org.folio.rest.core.models.RequestContext;
import org.folio.rest.jaxrs.model.CompositePoLine;
import org.folio.rest.jaxrs.model.Location;
import org.folio.rest.jaxrs.model.Piece;
import org.folio.rest.jaxrs.model.Title;
import org.folio.service.inventory.InventoryManager;
import org.folio.service.pieces.PieceUpdateInventoryService;
import org.folio.service.titles.TitlesService;

public class PieceCreateFlowInventoryManager {
  private static final Logger logger = LogManager.getLogger(PieceCreateFlowInventoryManager.class);

  private final TitlesService titlesService;
  private final PieceUpdateInventoryService pieceUpdateInventoryService;

  public PieceCreateFlowInventoryManager(TitlesService titlesService,  PieceUpdateInventoryService pieceUpdateInventoryService) {
    this.titlesService = titlesService;
    this.pieceUpdateInventoryService = pieceUpdateInventoryService;
  }

  public CompletableFuture<Void> updateInventory(PieceCreationHolder holder, RequestContext requestContext) {
    CompositePoLine compPOL = holder.getOriginPoLine();
    Piece piece = holder.getPieceToCreate();
    boolean createItem = holder.getCreateItem();
    if (Boolean.TRUE.equals(compPOL.getIsPackage())) {
      return packagePoLineUpdateInventory(compPOL, piece, createItem, requestContext);
    }
    else
    {
      return nonPackagePoLineUpdateInventory(compPOL, piece, createItem, requestContext);
    }
  }

  private CompletableFuture<Void> nonPackagePoLineUpdateInventory(CompositePoLine compPOL, Piece piece, boolean createItem,
                                                                  RequestContext requestContext) {
    return titlesService.getTitleById(piece.getTitleId(), requestContext)
      .thenCompose(title -> nonPackageUpdateTitleWithInstance(compPOL, title, requestContext))
      .thenCompose(title -> handleHolding(compPOL, piece, title.getInstanceId(), requestContext))
      .thenCompose(holdingId -> handleItem(compPOL, createItem, holdingId, requestContext))
      .thenAccept(itemId -> Optional.ofNullable(itemId).ifPresent(piece::withItemId));
  }

  private CompletableFuture<Void> packagePoLineUpdateInventory(CompositePoLine compPOL, Piece piece, boolean createItem,
                                                                RequestContext requestContext) {
    return titlesService.getTitleById(piece.getTitleId(), requestContext)
      .thenCompose(title -> packageUpdateTitleWithInstance(title, requestContext))
      .thenCompose(title -> handleHolding(compPOL, piece, title.getInstanceId(), requestContext))
      .thenCompose(holdingId -> handleItem(compPOL, createItem, holdingId, requestContext))
      .thenAccept(itemId -> Optional.ofNullable(itemId).ifPresent(piece::withItemId));
  }

  private CompletableFuture<String> handleHolding(CompositePoLine compPOL, Piece piece, String instanceId, RequestContext requestContext) {
    if (piece.getHoldingId() != null) {
      return completedFuture(piece.getHoldingId());
    }
    if (instanceId == null) {
      logger.error(CREATE_HOLDING_WITHOUT_INSTANCE_ERROR.getDescription());
      return CompletableFuture.failedFuture(new HttpException(RestConstants.VALIDATION_ERROR, CREATE_HOLDING_WITHOUT_INSTANCE_ERROR));
    }
    Location location = new Location().withLocationId(piece.getLocationId());
    return pieceUpdateInventoryService.handleHoldingsRecord(compPOL, location, instanceId, requestContext)
      .thenApply(holdingId -> {
        Optional.ofNullable(holdingId).ifPresent(holdingIdP -> {
          piece.setLocationId(null);
          piece.setHoldingId(holdingId);
        });
        return holdingId;
      });
  }

  private CompletableFuture<String> handleItem(CompositePoLine compPOL, boolean createItem, String holdingId, RequestContext requestContext) {
      if (createItem) {
        if (holdingId == null) {
          logger.error(CREATE_HOLDING_WITHOUT_INSTANCE_ERROR.getDescription());
          return CompletableFuture.failedFuture(new HttpException(RestConstants.VALIDATION_ERROR, CREATE_HOLDING_WITHOUT_INSTANCE_ERROR));
        }
        return pieceUpdateInventoryService.createItemRecord(compPOL, holdingId, requestContext);
      }
      return CompletableFuture.completedFuture(null);
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

  private CompletableFuture<Title> nonPackageUpdateTitleWithInstance(CompositePoLine poLine, Title title, RequestContext requestContext) {
    if (title.getInstanceId() != null || !PoLineCommonUtil.isOnlyInstanceUpdateRequired(poLine) ||
                      !PoLineCommonUtil.isHoldingsUpdateRequired(poLine.getEresource(), poLine.getPhysical())  ) {
      return CompletableFuture.completedFuture(title);
    } else {
      return pieceUpdateInventoryService.getOrCreateInstanceRecord(title, requestContext)
        .thenApply(title::withInstanceId)
        .thenCompose(titleWithInstanceId -> titlesService.saveTitle(titleWithInstanceId, requestContext).thenApply(json -> title));
    }
  }
}
