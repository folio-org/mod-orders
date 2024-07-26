package org.folio.models.pieces;

import java.util.List;

import org.folio.orders.utils.HelperUtils;
import org.folio.rest.jaxrs.model.CompositePoLine;
import org.folio.rest.jaxrs.model.CompositePurchaseOrder;
import org.folio.rest.jaxrs.model.PoLine;
import org.folio.rest.jaxrs.model.PurchaseOrder;
import org.folio.rest.jaxrs.model.Title;

public abstract class BasePieceFlowHolder {
  private CompositePurchaseOrder originPurchaseOrder;
  private CompositePurchaseOrder purchaseOrderToSave;

  private CompositePoLine compositePoLineToSaveOnly;
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

  public BasePieceFlowHolder withPoLineOnly(CompositePoLine compositePoLineToSave) {
    this.compositePoLineToSaveOnly = compositePoLineToSave;
    return this;
  }

  public void withTitleInformation(Title title) {
    this.title = title;
  }

  public CompositePurchaseOrder getOriginPurchaseOrder() {
    return originPurchaseOrder;
  }

  public CompositePurchaseOrder getPurchaseOrderToSave() {
    return purchaseOrderToSave;
  }

  public CompositePoLine getOriginPoLine() {
    return originPurchaseOrder.getCompositePoLines().get(0);
  }

  public CompositePoLine getPoLineToSave() {
    if (this.compositePoLineToSaveOnly != null) {
      return this.compositePoLineToSaveOnly;
    }
    return purchaseOrderToSave.getCompositePoLines().get(0);
  }

  public abstract String getOrderLineId();

  public abstract String getTitleId();

  public Title getTitle() {
    return title;
  }

}
