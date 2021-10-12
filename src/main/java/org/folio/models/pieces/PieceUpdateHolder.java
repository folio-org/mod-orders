package org.folio.models.pieces;

import org.folio.orders.utils.HelperUtils;
import org.folio.rest.jaxrs.model.CompositePoLine;
import org.folio.rest.jaxrs.model.CompositePurchaseOrder;
import org.folio.rest.jaxrs.model.Piece;
import org.folio.rest.jaxrs.model.PoLine;
import org.folio.rest.jaxrs.model.PurchaseOrder;

import java.util.List;

public class PieceUpdateHolder {
  private boolean createItem;
  private String instanceId;
  private Piece pieceToUpdate;
  private Piece pieceFromStorage;
  private CompositePurchaseOrder originPurchaseOrder;
  private CompositePurchaseOrder purchaseOrderToSave;

  public PieceUpdateHolder(Piece pieceToUpdate, boolean createItem) {
    this.pieceToUpdate = pieceToUpdate;
    this.createItem = createItem;
  }

  public PieceUpdateHolder(CompositePurchaseOrder originPurchaseOrder) {
    this.originPurchaseOrder = originPurchaseOrder;
    this.purchaseOrderToSave = HelperUtils.clone(CompositePurchaseOrder.class, originPurchaseOrder);
  }

  public PieceUpdateHolder(PurchaseOrder originPurchaseOrder, PoLine originPoLine) {
    this.originPurchaseOrder = HelperUtils.convertToCompositePurchaseOrder(originPurchaseOrder, List.of(originPoLine));
    this.purchaseOrderToSave = HelperUtils.clone(CompositePurchaseOrder.class, this.originPurchaseOrder);
  }

  public PieceUpdateHolder(PurchaseOrder originPurchaseOrder, PoLine originPoLine, boolean createItem) {
    this.originPurchaseOrder = HelperUtils.convertToCompositePurchaseOrder(originPurchaseOrder, List.of(originPoLine));
    this.purchaseOrderToSave = HelperUtils.clone(CompositePurchaseOrder.class, this.originPurchaseOrder);
    this.createItem = createItem;
  }

  public PieceUpdateHolder shallowCopy(PieceUpdateHolder sourcePieceUpdateHolder) {
    this.originPurchaseOrder = sourcePieceUpdateHolder.getOriginPurchaseOrder();
    this.purchaseOrderToSave = sourcePieceUpdateHolder.getPurchaseOrderToSave();
    return this;
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

  public Piece getPieceToUpdate() {
    return pieceToUpdate;
  }

  public Piece getPieceFromStorage() {
    return pieceFromStorage;
  }

  public PieceUpdateHolder withPieceFromStorage(Piece pieceFromStorage) {
    this.pieceFromStorage = pieceFromStorage;
    return this;
  }

  public String getInstanceId() {
    return this.instanceId;
  }

  public PieceUpdateHolder withInstanceId(String instanceId) {
    this.instanceId = instanceId;
    return this;
  }

  public boolean isCreateItem() {
    return createItem;
  }

}
