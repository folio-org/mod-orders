package org.folio.orders.utils;

import static org.folio.TestUtils.getMockAsJson;
import static org.folio.rest.impl.MockServer.BASE_MOCK_DATA_PATH;
import static org.junit.jupiter.api.Assertions.assertEquals;

import org.folio.orders.utils.PoLineCommonUtil;
import org.folio.rest.jaxrs.model.CompositePoLine;
import org.folio.rest.jaxrs.model.CompositePurchaseOrder;
import org.folio.rest.jaxrs.model.Eresource;
import org.folio.rest.jaxrs.model.Physical;
import org.junit.jupiter.api.Test;

public class PoLineCommonUtilTest {
  private static final String ORDER_ID = "1ab7ef6a-d1d4-4a4f-90a2-882aed18af14";
  private static final String ORDER_PATH = BASE_MOCK_DATA_PATH + "compositeOrders/" + ORDER_ID + ".json";

  @Test
  void testShouldMakePOLAsPendingIfPaymentAndReceiptStatusesEqualToAwaiting() {
    //given
    CompositePurchaseOrder order = getMockAsJson(ORDER_PATH).mapTo(CompositePurchaseOrder.class);
    order.getCompositePoLines().forEach(line -> {
      line.setPaymentStatus(CompositePoLine.PaymentStatus.AWAITING_PAYMENT);
      line.setReceiptStatus(CompositePoLine.ReceiptStatus.AWAITING_RECEIPT);
    });
    //When
    PoLineCommonUtil.makePoLinesPending(order.getCompositePoLines());
    //Then
    order.getCompositePoLines().forEach(line -> {
      assertEquals(CompositePoLine.PaymentStatus.PENDING, line.getPaymentStatus());
      assertEquals(CompositePoLine.ReceiptStatus.PENDING, line.getReceiptStatus());
    });
  }

  @Test
  void testShouldSkipMakePOLAsPendingIfPaymentAndReceiptStatusesNotEqualToAwaiting() {
    //given
    CompositePurchaseOrder order = getMockAsJson(ORDER_PATH).mapTo(CompositePurchaseOrder.class);
    order.getCompositePoLines().forEach(line -> {
      line.setPaymentStatus(CompositePoLine.PaymentStatus.FULLY_PAID);
      line.setReceiptStatus(CompositePoLine.ReceiptStatus.FULLY_RECEIVED);
    });
    //When
    PoLineCommonUtil.makePoLinesPending(order.getCompositePoLines());
    //Then
    order.getCompositePoLines().forEach(line -> {
      assertEquals(CompositePoLine.PaymentStatus.FULLY_PAID, line.getPaymentStatus());
      assertEquals(CompositePoLine.ReceiptStatus.FULLY_RECEIVED, line.getReceiptStatus());
    });
  }

  @Test
  void testOnlyInstanceUpdateNeededForPhysicalIfCreateInventoryInstance() {
    //given
    CompositePurchaseOrder order = getMockAsJson(ORDER_PATH).mapTo(CompositePurchaseOrder.class);
    order.getCompositePoLines().forEach(line -> {
      line.setPaymentStatus(CompositePoLine.PaymentStatus.FULLY_PAID);
      line.setReceiptStatus(CompositePoLine.ReceiptStatus.FULLY_RECEIVED);
      line.getPhysical().setCreateInventory(Physical.CreateInventory.INSTANCE);
    });
    //When
    boolean actCheck = PoLineCommonUtil.isOnlyInstanceUpdateRequired(order.getCompositePoLines().get(0));
    //Then
    assertEquals(true, actCheck);
  }

  @Test
  void testOnlyInstanceUpdateNeededForElectronicalIfCreateInventoryInstance() {
    //given
    CompositePurchaseOrder order = getMockAsJson(ORDER_PATH).mapTo(CompositePurchaseOrder.class);
    order.getCompositePoLines().forEach(line -> {
      line.setPaymentStatus(CompositePoLine.PaymentStatus.FULLY_PAID);
      line.setReceiptStatus(CompositePoLine.ReceiptStatus.FULLY_RECEIVED);
      line.setPhysical(null);
      line.setEresource(new Eresource().withCreateInventory(Eresource.CreateInventory.INSTANCE));
    });
    //When
    boolean actCheck = PoLineCommonUtil.isOnlyInstanceUpdateRequired(order.getCompositePoLines().get(0));
    //Then
    assertEquals(true, actCheck);
  }

  @Test
  void testOnlyInstanceUpdateNeededIfCreateInventoryIsNotInstance() {
    //given
    CompositePurchaseOrder order = getMockAsJson(ORDER_PATH).mapTo(CompositePurchaseOrder.class);
    order.getCompositePoLines().forEach(line -> {
      line.setPaymentStatus(CompositePoLine.PaymentStatus.FULLY_PAID);
      line.setReceiptStatus(CompositePoLine.ReceiptStatus.FULLY_RECEIVED);
      line.getPhysical().setCreateInventory(Physical.CreateInventory.INSTANCE_HOLDING);
    });
    //When
    boolean actCheck = PoLineCommonUtil.isOnlyInstanceUpdateRequired(order.getCompositePoLines().get(0));
    //Then
    assertEquals(false, actCheck);
  }
}
