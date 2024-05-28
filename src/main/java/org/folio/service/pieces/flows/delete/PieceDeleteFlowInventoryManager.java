package org.folio.service.pieces.flows.delete;

import io.vertx.core.Future;
import org.apache.commons.lang3.tuple.Pair;
import org.folio.models.pieces.PieceDeletionHolder;
import org.folio.rest.core.models.RequestContext;
import org.folio.service.pieces.PieceDeleteInventoryService;
import org.folio.service.pieces.PieceUpdateInventoryService;

import static org.folio.orders.utils.RequestContextUtil.createContextWithNewTenantId;

public class PieceDeleteFlowInventoryManager {

  private final PieceDeleteInventoryService pieceDeleteInventoryService;
  private final PieceUpdateInventoryService pieceUpdateInventoryService;
  public PieceDeleteFlowInventoryManager(PieceDeleteInventoryService pieceDeleteInventoryService,
                                         PieceUpdateInventoryService pieceUpdateInventoryService) {
    this.pieceDeleteInventoryService = pieceDeleteInventoryService;
    this.pieceUpdateInventoryService = pieceUpdateInventoryService;
  }

  public Future<Pair<String, String>> processInventory(PieceDeletionHolder holder, RequestContext requestContext) {
    var locationContext = createContextWithNewTenantId(requestContext, holder.getPieceToDelete().getReceivingTenantId());
    return pieceDeleteInventoryService.deleteItem(holder, locationContext)
      .compose(aVoid -> holder.isDeleteHolding()
        ? pieceUpdateInventoryService.deleteHoldingConnectedToPiece(holder.getPieceToDelete(), locationContext)
        : Future.succeededFuture()
      );
  }
}
