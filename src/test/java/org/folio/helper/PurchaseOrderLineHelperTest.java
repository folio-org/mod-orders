package org.folio.helper;

import static org.folio.TestConfig.clearServiceInteractions;
import static org.folio.TestConfig.initSpringContext;
import static org.folio.TestConfig.isVerticleNotDeployed;
import static org.folio.TestUtils.getMockAsJson;
import static org.folio.rest.impl.MockServer.BASE_MOCK_DATA_PATH;
import static org.junit.jupiter.api.Assertions.assertEquals;
import static org.mockito.Mockito.CALLS_REAL_METHODS;
import static org.mockito.Mockito.mock;

import org.folio.ApiTestSuite;
import org.folio.config.ApplicationConfig;
import org.folio.rest.jaxrs.model.CompositePoLine;
import org.folio.rest.jaxrs.model.CompositePurchaseOrder;
import org.junit.jupiter.api.AfterAll;
import org.junit.jupiter.api.AfterEach;
import org.junit.jupiter.api.BeforeAll;
import org.junit.jupiter.api.Test;

import java.util.concurrent.ExecutionException;
import java.util.concurrent.TimeoutException;

public class PurchaseOrderLineHelperTest {
  private static final String ORDER_ID = "1ab7ef6a-d1d4-4a4f-90a2-882aed18af14";
  private static final String ORDER_PATH = BASE_MOCK_DATA_PATH + "compositeOrders/" + ORDER_ID + ".json";

  private static boolean runningOnOwn;

  @BeforeAll
  static void before() throws InterruptedException, ExecutionException, TimeoutException {
    if (isVerticleNotDeployed()) {
      ApiTestSuite.before();
      runningOnOwn = true;
    }
    initSpringContext(ApplicationConfig.class);
  }

  @AfterEach
  void afterEach() {
    clearServiceInteractions();
  }

  @AfterAll
  static void after() {
    if (runningOnOwn) {
      ApiTestSuite.after();
    }
  }

  @Test
  void testShouldMakePOLAsPendingIfPaymentAndReceiptStatusesEqualToAwaiting() {
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
  void testShouldSkipMakePOLAsPendingIfPaymentAndReceiptStatusesNotEqualToAwaiting() {
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
