package org.folio.orders.utils;

import io.vertx.core.Future;
import org.folio.rest.core.exceptions.HttpException;
import org.folio.rest.jaxrs.model.CloseReason;
import org.folio.rest.jaxrs.model.PoLine;
import org.folio.rest.jaxrs.model.PurchaseOrder;
import org.folio.service.orders.utils.StatusUtils;
import org.junit.jupiter.api.Test;

import javax.money.convert.ConversionQuery;
import java.util.List;
import java.util.UUID;

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

public class HelperUtilsTest {

  @Test
  void testCombineResultListsOnSuccess() {
    var f1 = Future.succeededFuture(List.of(1, 2, 3));
    var f2 = Future.succeededFuture(List.of(4, 5, 6));
    var f3 = Future.succeededFuture(List.of(7, 8, 9));
    Future<List<Integer>> listFuture = HelperUtils.combineResultListsOnSuccess(List.of(f1, f2, f3));
    listFuture.onSuccess(
      combined -> assertEquals(List.of(1, 2, 3, 4, 5, 6, 7, 8, 9), combined)
    );
  }

  @Test
  void testShouldReturnBooleanWhenIsNotFound() {
    var actual = isNotFound(new HttpException(404, PREFIX_NOT_FOUND));
    assertTrue(actual);
    var actual2 = isNotFound(new Throwable(new HttpException(401, PREFIX_NOT_FOUND)));
    assertFalse(actual2);
  }

  @Test
  void testShouldReturnConversionQueryWithRateKeyForGetConversionQueryWithExchangeRate() {
    String fromCurrency = "USD";
    String toCurrency = "AUD";
    Double exchangeRate = 0.66d;

    ConversionQuery conversionQuery = HelperUtils.getConversionQuery(exchangeRate, fromCurrency, toCurrency);

    assertThat(conversionQuery.get(RATE_KEY, Double.class), is(exchangeRate));
    assertThat(conversionQuery.getBaseCurrency().getCurrencyCode(), is(fromCurrency));
    assertThat(conversionQuery.getCurrency().getCurrencyCode(), is(toCurrency));
  }

  @Test
  void testShouldReturnConversionQueryWithoutExchangeRate() {
    String fromCurrency = "USD";
    String toCurrency = "AUD";

    ConversionQuery conversionQuery = HelperUtils.getConversionQuery(null, toCurrency, fromCurrency);

    assertEquals(conversionQuery.getCurrency().getCurrencyCode(), fromCurrency);
    assertNull(conversionQuery.get(RATE_KEY, Double.class));
  }

  @Test
  void testOrderStatusToBeCancelled() {
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

    assertTrue(StatusUtils.changeOrderStatus(purchaseOrder, poLines));
    assertEquals(purchaseOrder.getWorkflowStatus(), PurchaseOrder.WorkflowStatus.CLOSED);
    assertEquals(purchaseOrder.getCloseReason(), new CloseReason().withReason(REASON_CANCELLED));
  }

}
