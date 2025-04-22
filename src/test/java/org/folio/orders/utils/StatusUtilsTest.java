package org.folio.orders.utils;

import org.folio.CopilotGenerated;
import org.folio.rest.jaxrs.model.PoLine;
import org.folio.rest.jaxrs.model.Piece;
import org.folio.rest.jaxrs.model.PoLine;
import org.folio.rest.jaxrs.model.PurchaseOrder;
import org.folio.service.orders.utils.StatusUtils;
import org.junit.jupiter.api.BeforeEach;
import org.junit.jupiter.api.Test;

import java.util.Arrays;
import java.util.List;
import java.util.UUID;

import static org.folio.rest.jaxrs.model.Piece.ReceivingStatus.EXPECTED;
import static org.folio.rest.jaxrs.model.Piece.ReceivingStatus.RECEIVED;
import static org.folio.rest.jaxrs.model.Piece.ReceivingStatus.UNRECEIVABLE;
import static org.folio.service.orders.utils.StatusUtils.calculatePoLineReceiptStatus;
import static org.junit.jupiter.api.Assertions.assertEquals;
import static org.junit.jupiter.api.Assertions.assertFalse;
import static org.junit.jupiter.api.Assertions.assertTrue;

@CopilotGenerated(partiallyGenerated = true)
public class StatusUtilsTest {

  private PurchaseOrder purchaseOrder;
  private PoLine poLine1;
  private PoLine poLine2;

  @BeforeEach
  void setUp() {
    purchaseOrder = new PurchaseOrder();
    poLine1 = new PoLine();
    poLine2 = new PoLine();
  }

  @Test
  void isStatusChanged_shouldReturnTrue_whenStatusChanged() {
    PoLine compOrderLine = new PoLine();
    compOrderLine.setReceiptStatus(PoLine.ReceiptStatus.FULLY_RECEIVED);
    compOrderLine.setPaymentStatus(PoLine.PaymentStatus.FULLY_PAID);

    poLine1.setReceiptStatus(PoLine.ReceiptStatus.PENDING);
    poLine1.setPaymentStatus(PoLine.PaymentStatus.PENDING);

    assertTrue(StatusUtils.isStatusChanged(compOrderLine, poLine1));
  }

  @Test
  void isStatusChanged_shouldReturnFalse_whenStatusNotChanged() {
    PoLine compOrderLine = new PoLine();
    compOrderLine.setReceiptStatus(PoLine.ReceiptStatus.PENDING);
    compOrderLine.setPaymentStatus(PoLine.PaymentStatus.PENDING);

    poLine1.setReceiptStatus(PoLine.ReceiptStatus.PENDING);
    poLine1.setPaymentStatus(PoLine.PaymentStatus.PENDING);

    assertFalse(StatusUtils.isStatusChanged(compOrderLine, poLine1));
  }

  @Test
  void areAllPoLinesCanceled_shouldReturnTrue_whenAllPoLinesCanceled() {
    poLine1.setPaymentStatus(PoLine.PaymentStatus.CANCELLED);
    poLine1.setReceiptStatus(PoLine.ReceiptStatus.CANCELLED);
    poLine2.setPaymentStatus(PoLine.PaymentStatus.CANCELLED);
    poLine2.setReceiptStatus(PoLine.ReceiptStatus.CANCELLED);

    assertTrue(StatusUtils.areAllPoLinesCanceled(List.of(poLine1, poLine2)));
  }

  @Test
  void areAllPoLinesCanceled_shouldReturnFalse_whenNotAllPoLinesCanceled() {
    poLine1.setPaymentStatus(PoLine.PaymentStatus.CANCELLED);
    poLine1.setReceiptStatus(PoLine.ReceiptStatus.CANCELLED);
    poLine2.setPaymentStatus(PoLine.PaymentStatus.PENDING);
    poLine2.setReceiptStatus(PoLine.ReceiptStatus.PENDING);

    assertFalse(StatusUtils.areAllPoLinesCanceled(List.of(poLine1, poLine2)));
  }

  @Test
  void changeOrderStatusForOrderUpdate_shouldCloseOrder_whenAllPoLinesCompleted() {
    purchaseOrder.setWorkflowStatus(PurchaseOrder.WorkflowStatus.OPEN);
    poLine1.setPaymentStatus(PoLine.PaymentStatus.FULLY_PAID);
    poLine1.setReceiptStatus(PoLine.ReceiptStatus.FULLY_RECEIVED);
    poLine2.setPaymentStatus(PoLine.PaymentStatus.FULLY_PAID);
    poLine2.setReceiptStatus(PoLine.ReceiptStatus.FULLY_RECEIVED);

    assertTrue(StatusUtils.changeOrderStatusForOrderUpdate(purchaseOrder, List.of(poLine1, poLine2)));
    assertEquals(PurchaseOrder.WorkflowStatus.CLOSED, purchaseOrder.getWorkflowStatus());
  }

  @Test
  void changeOrderStatusForOrderUpdate_shouldNotCloseOrder_whenNotAllPoLinesCompleted() {
    purchaseOrder.setWorkflowStatus(PurchaseOrder.WorkflowStatus.OPEN);
    poLine1.setPaymentStatus(PoLine.PaymentStatus.FULLY_PAID);
    poLine1.setReceiptStatus(PoLine.ReceiptStatus.FULLY_RECEIVED);
    poLine2.setPaymentStatus(PoLine.PaymentStatus.PENDING);
    poLine2.setReceiptStatus(PoLine.ReceiptStatus.PENDING);

    assertFalse(StatusUtils.changeOrderStatusForOrderUpdate(purchaseOrder, List.of(poLine1, poLine2)));
    assertEquals(PurchaseOrder.WorkflowStatus.OPEN, purchaseOrder.getWorkflowStatus());
  }

  @Test
  void changeOrderStatusForPoLineUpdate_shouldReopenOrder_whenAnyPoLineNotCompleted() {
    purchaseOrder.setWorkflowStatus(PurchaseOrder.WorkflowStatus.CLOSED);
    poLine1.setPaymentStatus(PoLine.PaymentStatus.PENDING);
    poLine1.setReceiptStatus(PoLine.ReceiptStatus.PENDING);

    assertTrue(StatusUtils.changeOrderStatusForPoLineUpdate(purchaseOrder, List.of(poLine1)));
    assertEquals(PurchaseOrder.WorkflowStatus.OPEN, purchaseOrder.getWorkflowStatus());
  }

  @Test
  void changeOrderStatusForPoLineUpdate_shouldNotReopenOrder_whenAllPoLinesCompleted() {
    purchaseOrder.setWorkflowStatus(PurchaseOrder.WorkflowStatus.CLOSED);
    poLine1.setPaymentStatus(PoLine.PaymentStatus.FULLY_PAID);
    poLine1.setReceiptStatus(PoLine.ReceiptStatus.FULLY_RECEIVED);

    assertFalse(StatusUtils.changeOrderStatusForPoLineUpdate(purchaseOrder, List.of(poLine1)));
    assertEquals(PurchaseOrder.WorkflowStatus.CLOSED, purchaseOrder.getWorkflowStatus());
  }

  @Test
  void testCalculatePoLineReceiptStatusWhenReceiveLast() {
    // given
    String poLineId = UUID.randomUUID().toString();
    List<Piece> fromStorage = givenPieces(EXPECTED, RECEIVED, UNRECEIVABLE);
    List<Piece> update = List.of(new Piece().withId(fromStorage.get(0).getId()).withReceivingStatus(RECEIVED));

    // when
    var receiptStatus = calculatePoLineReceiptStatus(poLineId, fromStorage, update);

    // then
    assertEquals(PoLine.ReceiptStatus.FULLY_RECEIVED, receiptStatus);
  }

  @Test
  void testCalculatePoLineReceiptStatusWhenExpectLast() {
    // given
    String poLineId = UUID.randomUUID().toString();
    List<Piece> fromStorage = givenPieces(RECEIVED, RECEIVED, UNRECEIVABLE);
    List<Piece> update = List.of(new Piece().withId(fromStorage.get(0).getId()).withReceivingStatus(EXPECTED));

    // when
    var receiptStatus = calculatePoLineReceiptStatus(poLineId, fromStorage, update);

    // then
    assertEquals(PoLine.ReceiptStatus.PARTIALLY_RECEIVED, receiptStatus);
  }

  @Test
  void testCalculatePoLineReceiptStatusWhenExpectAll() {
    // given
    String poLineId = UUID.randomUUID().toString();
    List<Piece> fromStorage = givenPieces(RECEIVED);
    List<Piece> update = List.of(new Piece().withId(fromStorage.get(0).getId()).withReceivingStatus(EXPECTED));

    // when
    var receiptStatus = calculatePoLineReceiptStatus(poLineId, fromStorage, update);

    // then
    assertEquals(PoLine.ReceiptStatus.AWAITING_RECEIPT, receiptStatus);
  }

  @Test
  void testCalculatePoLineReceiptStatusWhenReceivePart() {
    // given
    String poLineId = UUID.randomUUID().toString();
    List<Piece> fromStorage = givenPieces(EXPECTED, EXPECTED);
    List<Piece> update = List.of(new Piece().withId(fromStorage.get(0).getId()).withReceivingStatus(UNRECEIVABLE));

    // when
    var receiptStatus = calculatePoLineReceiptStatus(poLineId, fromStorage, update);

    // then
    assertEquals(PoLine.ReceiptStatus.PARTIALLY_RECEIVED, receiptStatus);
  }

  private static List<Piece> givenPieces(Piece.ReceivingStatus... statuses) {
    return Arrays.stream(statuses)
      .map(status -> new Piece().withId(UUID.randomUUID().toString()).withReceivingStatus(status))
      .toList();
  }

}
