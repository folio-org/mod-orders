package org.folio.service.orders.utils;

import org.apache.commons.lang3.StringUtils;
import org.folio.rest.jaxrs.model.CloseReason;
import org.folio.rest.jaxrs.model.CompositePoLine;
import org.folio.rest.jaxrs.model.PoLine;
import org.folio.rest.jaxrs.model.PoLine.PaymentStatus;
import org.folio.rest.jaxrs.model.PoLine.ReceiptStatus;
import org.folio.rest.jaxrs.model.PurchaseOrder;

import java.util.List;
import java.util.Set;

import static org.folio.orders.utils.HelperUtils.REASON_CANCELLED;
import static org.folio.orders.utils.HelperUtils.REASON_COMPLETE;

public class StatusUtils {

  private static final Set<String> resolutionPaymentStatus = Set.of(PaymentStatus.CANCELLED.value(), PaymentStatus.PAYMENT_NOT_REQUIRED.value(), PaymentStatus.FULLY_PAID.value());
  private static final Set<String> resolutionReceiptStatus = Set.of(ReceiptStatus.CANCELLED.value(), ReceiptStatus.RECEIPT_NOT_REQUIRED.value(), ReceiptStatus.FULLY_RECEIVED.value());


//  private static boolean isCompositeOrderClosed(CompositePurchaseOrder order) {
//    return order.getWorkflowStatus() == CompositePurchaseOrder.WorkflowStatus.CLOSED;
//  }

  public static boolean isStatusChanged(CompositePoLine compOrderLine, PoLine lineFromStorage) {
    return !StringUtils.equals(lineFromStorage.getReceiptStatus().value(), compOrderLine.getReceiptStatus().value()) ||
      !StringUtils.equals(lineFromStorage.getPaymentStatus().value(), compOrderLine.getPaymentStatus().value());
  }

//  public static boolean shouldUpdateOrderStatus(CompositePurchaseOrder compositePurchaseOrder, CompositePoLine compOrderLine, PoLine lineFromStorage) {
//    return !isCompositeOrderClosed(compositePurchaseOrder) && isStatusChanged(compOrderLine, lineFromStorage)
//      || isCompositeOrderClosed(compositePurchaseOrder) && isNonResolutionPoLine(compOrderLine);
//  }

  public static boolean areAllPoLinesCanceled(List<PoLine> poLines) {
    return poLines.stream().allMatch(StatusUtils::isStatusCanceledPoLine);
  }

  public static boolean changeOrderStatus(PurchaseOrder purchaseOrder, List<PoLine> poLines) {
    if (toBeCancelled(purchaseOrder, poLines)) {
      purchaseOrder.setWorkflowStatus(PurchaseOrder.WorkflowStatus.CLOSED);
      purchaseOrder.setCloseReason(new CloseReason().withReason(REASON_CANCELLED));
      return true;
    }
    if (toBeClosed(purchaseOrder, poLines)) {
      purchaseOrder.setWorkflowStatus(PurchaseOrder.WorkflowStatus.CLOSED);
      purchaseOrder.setCloseReason(new CloseReason().withReason(REASON_COMPLETE));
      return true;
    }
    if (toBeReopened(purchaseOrder, poLines)) {
      purchaseOrder.setWorkflowStatus(PurchaseOrder.WorkflowStatus.OPEN);
      return true;
    }
    return false;
  }

  private static boolean toBeClosed(PurchaseOrder purchaseOrder, List<PoLine> poLines) {
    return isOrderOpen(purchaseOrder)
      && poLines.stream().allMatch(StatusUtils::isCompletedPoLine);
  }

  private static boolean toBeCancelled(PurchaseOrder purchaseOrder, List<PoLine> poLines) {
    return isOrderOpen(purchaseOrder)
      && poLines.stream().allMatch(StatusUtils::isCancelledPoLine);
  }

  private static boolean toBeReopened(PurchaseOrder purchaseOrder, List<PoLine> poLines) {
    return isOrderClosed(purchaseOrder)
      && poLines.stream().anyMatch(StatusUtils::isNonResolutionPoLine);
  }

  private static boolean isCompletedPoLine(PoLine line) {
    return resolutionPaymentStatus.contains(line.getPaymentStatus().value())
      && resolutionReceiptStatus.contains(line.getReceiptStatus().value());
  }

  private static boolean isNonResolutionPoLine(PoLine line) {
    return !resolutionPaymentStatus.contains(line.getPaymentStatus().value())
      && !resolutionReceiptStatus.contains(line.getReceiptStatus().value());
  }

  private static boolean isCancelledPoLine(PoLine poLine) {
    return poLine.getPaymentStatus() == PoLine.PaymentStatus.CANCELLED
      && poLine.getReceiptStatus() == PoLine.ReceiptStatus.CANCELLED;
  }

  private static boolean isStatusCanceledPoLine(PoLine poLine) {
    return poLine.getPaymentStatus() == PoLine.PaymentStatus.CANCELLED
      || poLine.getReceiptStatus() == PoLine.ReceiptStatus.CANCELLED;
  }

  public static boolean isStatusCanceledCompositePoLine(CompositePoLine compOrderLine) {
    return compOrderLine.getReceiptStatus() == CompositePoLine.ReceiptStatus.CANCELLED
      || compOrderLine.getPaymentStatus() == CompositePoLine.PaymentStatus.CANCELLED;
  }

  private static boolean isOrderOpen(PurchaseOrder order) {
    return order.getWorkflowStatus() == PurchaseOrder.WorkflowStatus.OPEN;
  }

  private static boolean isOrderClosed(PurchaseOrder order) {
    return order.getWorkflowStatus() == PurchaseOrder.WorkflowStatus.CLOSED;
  }

  private StatusUtils() {}

}
