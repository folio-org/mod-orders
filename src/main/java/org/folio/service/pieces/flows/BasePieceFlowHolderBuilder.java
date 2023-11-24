package org.folio.service.pieces.flows;

import io.vertx.core.Future;
import org.folio.models.pieces.BasePieceFlowHolder;
import org.folio.rest.core.models.RequestContext;
import org.folio.service.orders.PurchaseOrderLineService;
import org.folio.service.orders.PurchaseOrderStorageService;
import org.folio.service.titles.TitlesService;

public class BasePieceFlowHolderBuilder {
  protected final PurchaseOrderStorageService purchaseOrderStorageService;
  protected final PurchaseOrderLineService purchaseOrderLineService;
  protected final TitlesService titlesService;

  public BasePieceFlowHolderBuilder(PurchaseOrderStorageService purchaseOrderStorageService,
                                    PurchaseOrderLineService purchaseOrderLineService, TitlesService titlesService) {
    this.purchaseOrderStorageService = purchaseOrderStorageService;
    this.purchaseOrderLineService = purchaseOrderLineService;
    this.titlesService = titlesService;
  }

  public Future<Void> updateHolderWithOrderInformation(BasePieceFlowHolder holder, RequestContext requestContext) {
    return purchaseOrderLineService.getOrderLineById(holder.getOrderLineId(), requestContext)
      .compose(poLine -> purchaseOrderStorageService.getPurchaseOrderById(poLine.getPurchaseOrderId(), requestContext)
        .map(purchaseOrder -> {
          holder.withOrderInformation(purchaseOrder, poLine);
          return null;
        }));
  }

  public Future<Void> updateHolderWithTitleInformation(BasePieceFlowHolder holder, RequestContext requestContext) {
    return titlesService.getTitleById(holder.getTitleId(), requestContext)
      .map(title -> {
        holder.withTitleInformation(title);
        return null;
      });
  }
}
