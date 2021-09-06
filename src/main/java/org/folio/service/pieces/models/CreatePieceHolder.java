package org.folio.service.pieces.models;

import java.util.List;

import org.folio.orders.utils.HelperUtils;
import org.folio.rest.jaxrs.model.CompositePoLine;
import org.folio.rest.jaxrs.model.CompositePurchaseOrder;
import org.folio.rest.jaxrs.model.PoLine;
import org.folio.rest.jaxrs.model.PurchaseOrder;

public class CreatePieceHolder {
  private CompositePurchaseOrder originPurchaseOrder;
  private CompositePurchaseOrder purchaseOrderToSave;

  public CreatePieceHolder() {

  }

  public CreatePieceHolder(CompositePurchaseOrder originPurchaseOrder) {
    this.originPurchaseOrder = originPurchaseOrder;
    this.purchaseOrderToSave = HelperUtils.clone(CompositePurchaseOrder.class, originPurchaseOrder);
  }

  public CreatePieceHolder(PurchaseOrder originPurchaseOrder, PoLine originPoLine) {
    this.originPurchaseOrder = HelperUtils.convertToCompositePurchaseOrder(originPurchaseOrder, List.of(originPoLine));
    this.purchaseOrderToSave = HelperUtils.clone(CompositePurchaseOrder.class, this.originPurchaseOrder);
  }

  public void shallowCopy(CreatePieceHolder sourceCreatePieceHolder) {
    this.originPurchaseOrder = sourceCreatePieceHolder.getOriginPurchaseOrder();
    this.purchaseOrderToSave = sourceCreatePieceHolder.getPurchaseOrderToSave();
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
    return purchaseOrderToSave.getCompositePoLines().get(0);
  }
}
