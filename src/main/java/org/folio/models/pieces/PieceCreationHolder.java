package org.folio.models.pieces;

import java.util.List;

import org.folio.orders.utils.HelperUtils;
import org.folio.rest.jaxrs.model.CompositePoLine;
import org.folio.rest.jaxrs.model.CompositePurchaseOrder;
import org.folio.rest.jaxrs.model.Piece;
import org.folio.rest.jaxrs.model.PoLine;
import org.folio.rest.jaxrs.model.PurchaseOrder;

public class PieceCreationHolder {
  private Piece pieceToCreate;
  private CompositePurchaseOrder originPurchaseOrder;
  private CompositePurchaseOrder purchaseOrderToSave;
  private boolean createItem;

  public PieceCreationHolder(Piece pieceToCreate, boolean createItem) {
    this.pieceToCreate = pieceToCreate;
    this.createItem = createItem;
  }

  public PieceCreationHolder(CompositePurchaseOrder originPurchaseOrder) {
    this.originPurchaseOrder = originPurchaseOrder;
    this.purchaseOrderToSave = HelperUtils.clone(CompositePurchaseOrder.class, originPurchaseOrder);
  }

  public PieceCreationHolder(PurchaseOrder originPurchaseOrder, PoLine originPoLine) {
    this.originPurchaseOrder = HelperUtils.convertToCompositePurchaseOrder(originPurchaseOrder, List.of(originPoLine));
    this.purchaseOrderToSave = HelperUtils.clone(CompositePurchaseOrder.class, this.originPurchaseOrder);
  }

  public void shallowCopy(PieceCreationHolder sourcePieceCreationHolder) {
    this.originPurchaseOrder = sourcePieceCreationHolder.getOriginPurchaseOrder();
    this.purchaseOrderToSave = sourcePieceCreationHolder.getPurchaseOrderToSave();
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

  public Piece getPieceToCreate() {
    return pieceToCreate;
  }

  public boolean getCreateItem() {
    return createItem;
  }
}
