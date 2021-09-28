package org.folio.service.pieces.flows;

import org.folio.rest.jaxrs.model.CompositePurchaseOrder;
import org.folio.rest.jaxrs.model.PurchaseOrder;

import java.util.Objects;

public class PieceFlowUpdatePoLineKey {
  private String orderWorkFlowStatus;
  private PieceFlowType pieceFlowType;
  private boolean isPackage;

  public PieceFlowUpdatePoLineKey() {

  }

  public PieceFlowUpdatePoLineKey withIsPackage(boolean isPackage) {
    this.isPackage = isPackage;
    return this;
  }

  public PieceFlowUpdatePoLineKey withOrderWorkFlowStatus(PurchaseOrder.WorkflowStatus workflowStatus) {
    this.orderWorkFlowStatus = workflowStatus.value();
    return this;
  }

  public PieceFlowUpdatePoLineKey withOrderWorkFlowStatus(CompositePurchaseOrder.WorkflowStatus workflowStatus) {
    this.orderWorkFlowStatus = workflowStatus.value();
    return this;
  }

  public PieceFlowUpdatePoLineKey withPieceFlowType(PieceFlowType pieceFlowType) {
    this.pieceFlowType = pieceFlowType;
    return this;
  }

  @Override public boolean equals(Object o) {
    if (this == o)
      return true;
    if (o == null || getClass() != o.getClass())
      return false;
    PieceFlowUpdatePoLineKey that = (PieceFlowUpdatePoLineKey) o;
    return isPackage == that.isPackage && Objects.equals(orderWorkFlowStatus,
      that.orderWorkFlowStatus) && pieceFlowType == that.pieceFlowType;
  }

  @Override public int hashCode() {
    return Objects.hash(orderWorkFlowStatus, pieceFlowType, isPackage);
  }

  public enum PieceFlowType {
    PIECE_CREATE_FLOW, PIECE_UPDATE_FLOW, PIECE_DELETE_FLOW
  }
}
