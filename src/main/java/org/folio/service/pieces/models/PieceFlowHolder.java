package org.folio.service.pieces.models;

import org.folio.orders.utils.HelperUtils;
import org.folio.orders.utils.PoLineCommonUtil;
import org.folio.rest.jaxrs.model.CompositePoLine;
import org.folio.rest.jaxrs.model.CompositePurchaseOrder;
import org.folio.rest.jaxrs.model.PoLine;
import org.folio.rest.jaxrs.model.PurchaseOrder;

import java.util.Collections;

public class PieceFlowHolder {
  private CompositePurchaseOrder purchaseOrder;
  private CompositePoLine lineToSave;
  private CompositePoLine originLine;

  public PieceFlowHolder() {

  }

  public PieceFlowHolder(PoLine originLine) {
    this.originLine = PoLineCommonUtil.convertToCompositePoLine(originLine);
    this.lineToSave = this.originLine;
  }

  public PieceFlowHolder(CompositePoLine originLine) {
    this.originLine = originLine;
    this.lineToSave = originLine;
  }

  public CompositePurchaseOrder getPurchaseOrder() {
    return purchaseOrder;
  }

  public PieceFlowHolder withPurchaseOrder(CompositePurchaseOrder compositePurchaseOrder) {
    this.purchaseOrder = compositePurchaseOrder;
    return this;
  }

  public PieceFlowHolder withPurchaseOrder(PurchaseOrder purchaseOrder) {
    this.purchaseOrder = HelperUtils.convertToCompositePurchaseOrder(purchaseOrder, Collections.emptyList());
    return this;
  }

  public CompositePoLine getLineToSave() {
    return lineToSave;
  }

  public PieceFlowHolder withLineToSave(CompositePoLine lineToSave) {
    this.lineToSave = lineToSave;
    return this;
  }

  public PieceFlowHolder withLineToSave(PoLine lineToSave) {
    this.lineToSave = PoLineCommonUtil.convertToCompositePoLine(lineToSave);
    return this;
  }

  public CompositePoLine getOriginLine() {
    return originLine;
  }

  public PieceFlowHolder withOriginLine(PoLine originLine) {
    this.originLine = PoLineCommonUtil.convertToCompositePoLine(originLine);
    return this;
  }

  public PieceFlowHolder withOriginLine(CompositePoLine originLine) {
    this.originLine = originLine;
    return this;
  }
}
