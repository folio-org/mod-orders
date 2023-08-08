package org.folio.orders.utils;

import static org.folio.orders.utils.HelperUtils.REASON_CANCELLED;
import static org.folio.orders.utils.HelperUtils.isNotFound;
import static org.folio.rest.core.exceptions.ErrorCodes.PREFIX_NOT_FOUND;
import static org.folio.service.exchange.ExchangeRateProviderResolver.RATE_KEY;
import static org.hamcrest.MatcherAssert.assertThat;
import static org.hamcrest.Matchers.emptyOrNullString;
import static org.hamcrest.Matchers.is;
import static org.junit.jupiter.api.Assertions.assertEquals;
import static org.junit.jupiter.api.Assertions.assertFalse;
import static org.junit.jupiter.api.Assertions.assertNull;
import static org.junit.jupiter.api.Assertions.assertTrue;

import java.util.List;
import java.util.UUID;

import javax.money.convert.ConversionQuery;

import org.folio.rest.core.exceptions.ErrorCodes;
import org.folio.rest.core.exceptions.HttpException;
import org.folio.rest.jaxrs.model.CloseReason;
import org.folio.rest.jaxrs.model.Cost;
import org.folio.rest.jaxrs.model.PoLine;
import org.folio.rest.jaxrs.model.PurchaseOrder;
import org.junit.jupiter.api.Test;

public class HelperUtilsTest {

  @Test
  public void testShouldReturnEmptyString(){
    String act = HelperUtils.combineCqlExpressions("");
    assertThat(act, is(emptyOrNullString()));
  }

  @Test
  void testShouldReturnBooleanWhenIsNotFound(){
    var actual = isNotFound(new HttpException(404, PREFIX_NOT_FOUND));
    assertTrue(actual);
    var actual2 = isNotFound(new Throwable(new HttpException(401, PREFIX_NOT_FOUND)));
    assertFalse(actual2);
  }

  @Test
  public void testShouldReturnConversionQueryWithRateKeyForGetConversionQueryWithExchangeRate(){
    ConversionQuery conversionQuery = HelperUtils.getConversionQuery(2.0d, "USD", "EUR");
    assertThat(conversionQuery.get(RATE_KEY, Double.class), is(2.0d));
    assertThat(conversionQuery.getBaseCurrency().getCurrencyCode(), is("USD"));
    assertThat(conversionQuery.getCurrency().getCurrencyCode(), is("EUR"));
  }

  @Test
  public void testShouldBuildQueryWithoutExchangeRate(){
    String systemCurrency = "USD";
    Cost costOneTime = new Cost().withListUnitPrice(595d).withQuantityPhysical(1).withCurrency("EUR").withPoLineEstimatedPrice(595d);
    PoLine poLineOneTime = new PoLine().withId(UUID.randomUUID().toString()).withPurchaseOrderId(UUID.randomUUID().toString()).withCost(costOneTime);
    ConversionQuery actQuery = HelperUtils.buildConversionQuery(poLineOneTime, systemCurrency);
    assertEquals(actQuery.getCurrency().getCurrencyCode(), systemCurrency);
    assertNull(actQuery.get(RATE_KEY, Double.class));
  }

  @Test void testOrderStatusToBeCancelled() {
    PurchaseOrder purchaseOrder = new PurchaseOrder();
    purchaseOrder.setId(UUID.randomUUID().toString());
    purchaseOrder.setWorkflowStatus(PurchaseOrder.WorkflowStatus.OPEN);

    PoLine firstPoLine = new PoLine();
    firstPoLine.setId(UUID.randomUUID().toString());
    firstPoLine.setPaymentStatus(PoLine.PaymentStatus.CANCELLED);
    firstPoLine.setReceiptStatus(PoLine.ReceiptStatus.CANCELLED);

    PoLine secondPoLine = new PoLine();
    secondPoLine.setId(UUID.randomUUID().toString());
    secondPoLine.setPaymentStatus(PoLine.PaymentStatus.CANCELLED);
    secondPoLine.setReceiptStatus(PoLine.ReceiptStatus.CANCELLED);

    List<PoLine> poLines = List.of(firstPoLine, secondPoLine);

    assertTrue(HelperUtils.changeOrderStatus(purchaseOrder, poLines));
    assertEquals(purchaseOrder.getWorkflowStatus(), PurchaseOrder.WorkflowStatus.CLOSED);
    assertEquals(purchaseOrder.getCloseReason(), new CloseReason().withReason(REASON_CANCELLED));
  }

}
