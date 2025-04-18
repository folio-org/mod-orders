package org.folio.models.pieces;

import java.util.List;

import org.folio.orders.utils.HelperUtils;
import org.folio.rest.jaxrs.model.PoLine;
import org.folio.rest.jaxrs.model.CompositePurchaseOrder;
import org.folio.rest.jaxrs.model.PoLine;
import org.folio.rest.jaxrs.model.PurchaseOrder;
import org.folio.rest.jaxrs.model.Title;

public abstract class BasePieceFlowHolder {
  private CompositePurchaseOrder originPurchaseOrder;
  private CompositePurchaseOrder purchaseOrderToSave;

  private PoLine compositePoLineToSave;
  private Title title;

  public BasePieceFlowHolder() {

  }

  public BasePieceFlowHolder withOrderInformation(CompositePurchaseOrder originPurchaseOrder) {
    this.originPurchaseOrder = originPurchaseOrder;
    this.purchaseOrderToSave = HelperUtils.clone(CompositePurchaseOrder.class, originPurchaseOrder);
    return this;
  }

  public BasePieceFlowHolder withOrderInformation(PurchaseOrder originPurchaseOrder, PoLine originPoLine) {
    this.originPurchaseOrder = HelperUtils.convertToCompositePurchaseOrder(originPurchaseOrder, List.of(originPoLine));
    this.purchaseOrderToSave = HelperUtils.clone(CompositePurchaseOrder.class, this.originPurchaseOrder);
    return this;
  }

  public BasePieceFlowHolder withPoLineOnly(PoLine compositePoLineToSave) {
    this.compositePoLineToSave = compositePoLineToSave;
    return this;
  }

  public BasePieceFlowHolder withTitleInformation(Title title) {
    this.title = title;
    return this;
  }

  public CompositePurchaseOrder getOriginPurchaseOrder() {
    return originPurchaseOrder;
  }

  public CompositePurchaseOrder getPurchaseOrderToSave() {
    return purchaseOrderToSave;
  }

  public PoLine getOriginPoLine() {
    return originPurchaseOrder.getPoLines().get(0);
  }

  public PoLine getPoLineToSave() {
    if (this.compositePoLineToSave != null) {
      return this.compositePoLineToSave;
    }
    return purchaseOrderToSave.getPoLines().get(0);
  }

  public abstract String getOrderLineId();

  public abstract String getTitleId();

  public Title getTitle() {
    return title;
  }

}
