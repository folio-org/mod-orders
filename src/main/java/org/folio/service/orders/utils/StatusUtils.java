package org.folio.service.orders.utils;

import lombok.extern.log4j.Log4j2;
import org.apache.commons.lang3.StringUtils;
import org.folio.rest.jaxrs.model.CloseReason;
import org.folio.rest.jaxrs.model.PoLine;
import org.folio.rest.jaxrs.model.Piece;
import org.folio.rest.jaxrs.model.PoLine;
import org.folio.rest.jaxrs.model.PoLine.PaymentStatus;
import org.folio.rest.jaxrs.model.PoLine.ReceiptStatus;
import org.folio.rest.jaxrs.model.PurchaseOrder;

import java.util.List;
import java.util.Map;
import java.util.Set;
import java.util.stream.Collectors;

import static org.folio.helper.CheckinReceivePiecesHelper.EXPECTED_STATUSES;
import static org.folio.helper.CheckinReceivePiecesHelper.RECEIVED_STATUSES;
import static org.folio.orders.utils.HelperUtils.REASON_CANCELLED;
import static org.folio.orders.utils.HelperUtils.REASON_COMPLETE;
import static org.folio.rest.jaxrs.model.PoLine.ReceiptStatus.AWAITING_RECEIPT;
import static org.folio.rest.jaxrs.model.PoLine.ReceiptStatus.FULLY_RECEIVED;
import static org.folio.rest.jaxrs.model.PoLine.ReceiptStatus.PARTIALLY_RECEIVED;

@Log4j2
public class StatusUtils {

  private static final Set<String> resolutionPaymentStatus = Set.of(PaymentStatus.CANCELLED.value(), PaymentStatus.PAYMENT_NOT_REQUIRED.value(), PaymentStatus.FULLY_PAID.value());
  private static final Set<String> resolutionReceiptStatus = Set.of(ReceiptStatus.CANCELLED.value(), ReceiptStatus.RECEIPT_NOT_REQUIRED.value(), ReceiptStatus.FULLY_RECEIVED.value());

  public static boolean isStatusChanged(PoLine compOrderLine, PoLine lineFromStorage) {
    return !StringUtils.equals(lineFromStorage.getReceiptStatus().value(), compOrderLine.getReceiptStatus().value()) ||
      !StringUtils.equals(lineFromStorage.getPaymentStatus().value(), compOrderLine.getPaymentStatus().value());
  }

  public static boolean areAllPoLinesCanceled(List<PoLine> poLines) {
    return poLines.stream().allMatch(StatusUtils::isStatusCanceledPoLine);
  }

  public static boolean changeOrderStatusForOrderUpdate(PurchaseOrder purchaseOrder, List<PoLine> poLines) {
    if (toBeCancelled(purchaseOrder, poLines)) {
      return closeOrder(purchaseOrder, REASON_CANCELLED);
    }
    if (toBeClosed(purchaseOrder, poLines)) {
      return closeOrder(purchaseOrder, REASON_COMPLETE);
    }
    return false;
  }

  public static boolean changeOrderStatusForPoLineUpdate(PurchaseOrder purchaseOrder, List<PoLine> poLines) {
    if (toBeCancelled(purchaseOrder, poLines)) {
      return closeOrder(purchaseOrder, REASON_CANCELLED);
    }
    if (toBeClosed(purchaseOrder, poLines)) {
      return closeOrder(purchaseOrder, REASON_COMPLETE);
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

  public static boolean isStatusCanceledCompositePoLine(PoLine compOrderLine) {
    return compOrderLine.getReceiptStatus() == PoLine.ReceiptStatus.CANCELLED
      || compOrderLine.getPaymentStatus() == PoLine.PaymentStatus.CANCELLED;
  }

  private static boolean isOrderOpen(PurchaseOrder order) {
    return order.getWorkflowStatus() == PurchaseOrder.WorkflowStatus.OPEN;
  }

  private static boolean isOrderClosed(PurchaseOrder order) {
    return order.getWorkflowStatus() == PurchaseOrder.WorkflowStatus.CLOSED;
  }

  private static boolean closeOrder(PurchaseOrder purchaseOrder, String reason) {
    purchaseOrder.setWorkflowStatus(PurchaseOrder.WorkflowStatus.CLOSED);
    purchaseOrder.setCloseReason(new CloseReason().withReason(reason));
    return true;
  }

  public static PoLine.ReceiptStatus calculatePoLineReceiptStatus(String poLineId, List<Piece> piecesFromStorage) {
    return calculatePoLineReceiptStatus(poLineId, piecesFromStorage, List.of());
  }

  public static PoLine.ReceiptStatus calculatePoLineReceiptStatus(String poLineId, List<Piece> piecesFromStorage, List<Piece> piecesToUpdate) {
    // 1. Get map of all persistent piece statuses
    var pieceStatues = piecesFromStorage.stream().collect(Collectors.toMap(Piece::getId, Piece::getReceivingStatus));
    // 2. Update new piece statuses (if any)
    piecesToUpdate.forEach(piece -> pieceStatues.put(piece.getId(), piece.getReceivingStatus()));
    // 3. Calculate receipt status
    return calculatePoLineReceiptStatus(poLineId, pieceStatues);
  }

  private static PoLine.ReceiptStatus calculatePoLineReceiptStatus(String poLineId, Map<String, Piece.ReceivingStatus> pieceStatuses) {
    // Count received and expected statuses
    long receivedQuantity = pieceStatuses.values().stream().filter(RECEIVED_STATUSES::contains).count();
    long expectedQuantity = pieceStatuses.values().stream().filter(EXPECTED_STATUSES::contains).count();

    if (expectedQuantity == 0) {
      log.info("calculatePoLineReceiptStatus:: PoLine with id: '{}', status: Fully Received", poLineId);
      return FULLY_RECEIVED;
    }
    if (receivedQuantity > 0) {
      log.info("calculatePoLineReceiptStatus:: PoLine with id: '{}', status: Partially Received. Successfully Received pieces: {}", poLineId, receivedQuantity);
      return PARTIALLY_RECEIVED;
    }
    log.info("calculatePoLineReceiptStatus:: PoLine with id: '{}', status: Awaiting Receipt. Pieces were rolled-back to Expected", poLineId);
    return AWAITING_RECEIPT;
  }

  private StatusUtils() {}

}
