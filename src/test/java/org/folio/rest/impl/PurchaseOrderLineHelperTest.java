package org.folio.rest.impl;

import static org.folio.rest.impl.MockServer.BASE_MOCK_DATA_PATH;
import static org.junit.Assert.assertEquals;
import static org.mockito.Mockito.CALLS_REAL_METHODS;
import static org.mockito.Mockito.mock;

import org.folio.rest.jaxrs.model.CompositePoLine;
import org.folio.rest.jaxrs.model.CompositePurchaseOrder;
import org.junit.Test;

public class PurchaseOrderLineHelperTest extends ApiTestBase{
  private static final String ORDER_ID = "1ab7ef6a-d1d4-4a4f-90a2-882aed18af14";
  private static final String ORDER_PATH = BASE_MOCK_DATA_PATH + "compositeOrders/" + ORDER_ID + ".json";

  @Test
  public void testShouldMakePOLAsPendingIfPaymentAndReceiptStatusesEqualToAwaiting() {
    //given
    PurchaseOrderLineHelper orderLineHelper = mock(PurchaseOrderLineHelper.class, CALLS_REAL_METHODS);
    CompositePurchaseOrder order = getMockAsJson(ORDER_PATH).mapTo(CompositePurchaseOrder.class);
    order.getCompositePoLines().forEach(line -> {
      line.setPaymentStatus(CompositePoLine.PaymentStatus.AWAITING_PAYMENT);
      line.setReceiptStatus(CompositePoLine.ReceiptStatus.AWAITING_RECEIPT);
    });
    //When
    orderLineHelper.makePoLinesPending(order.getCompositePoLines());
    //Then
    order.getCompositePoLines().forEach(line -> {
      assertEquals(CompositePoLine.PaymentStatus.PENDING, line.getPaymentStatus());
      assertEquals(CompositePoLine.ReceiptStatus.PENDING, line.getReceiptStatus());
    });
  }

  @Test
  public void testShouldSkipMakePOLAsPendingIfPaymentAndReceiptStatusesNotEqualToAwaiting() {
    //given
    PurchaseOrderLineHelper orderLineHelper = mock(PurchaseOrderLineHelper.class, CALLS_REAL_METHODS);
    CompositePurchaseOrder order = getMockAsJson(ORDER_PATH).mapTo(CompositePurchaseOrder.class);
    order.getCompositePoLines().forEach(line -> {
      line.setPaymentStatus(CompositePoLine.PaymentStatus.FULLY_PAID);
      line.setReceiptStatus(CompositePoLine.ReceiptStatus.FULLY_RECEIVED);
    });
    //When
    orderLineHelper.makePoLinesPending(order.getCompositePoLines());
    //Then
    order.getCompositePoLines().forEach(line -> {
      assertEquals(CompositePoLine.PaymentStatus.FULLY_PAID, line.getPaymentStatus());
      assertEquals(CompositePoLine.ReceiptStatus.FULLY_RECEIVED, line.getReceiptStatus());
    });
  }
}
