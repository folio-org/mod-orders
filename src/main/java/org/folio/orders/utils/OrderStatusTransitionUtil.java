package org.folio.orders.utils;

import static org.folio.rest.jaxrs.model.CompositePurchaseOrder.WorkflowStatus.CLOSED;
import static org.folio.rest.jaxrs.model.CompositePurchaseOrder.WorkflowStatus.OPEN;
import static org.folio.rest.jaxrs.model.CompositePurchaseOrder.WorkflowStatus.PENDING;

import java.util.Objects;

import org.folio.rest.jaxrs.model.CompositePurchaseOrder;
import org.folio.rest.jaxrs.model.PurchaseOrder;

public final class OrderStatusTransitionUtil {

  private OrderStatusTransitionUtil() {

  }

  public static boolean isOrderClosing(PurchaseOrder.WorkflowStatus newStatus, String initialStatus) {
    return newStatus == PurchaseOrder.WorkflowStatus.CLOSED && !Objects.equals(newStatus.value(), initialStatus);
  }

  public static boolean isOrderReopening(PurchaseOrder.WorkflowStatus newStatus, String initialOrderStatus) {
    return newStatus == PurchaseOrder.WorkflowStatus.OPEN && CLOSED.value().equals(initialOrderStatus);
  }

  public static boolean isTransitionToApproved(CompositePurchaseOrder poFromStorage, CompositePurchaseOrder compPO) {
    return !poFromStorage.getApproved() && compPO.getApproved();
  }

  public static boolean isTransitionToOpen(CompositePurchaseOrder poFromStorage, CompositePurchaseOrder compPO) {
    return poFromStorage.getWorkflowStatus() == PENDING && compPO.getWorkflowStatus() == OPEN;
  }

  public static boolean isTransitionToPending(CompositePurchaseOrder poFromStorage, CompositePurchaseOrder compPO) {
    return poFromStorage.getWorkflowStatus() == OPEN && compPO.getWorkflowStatus() == PENDING;
  }
}
