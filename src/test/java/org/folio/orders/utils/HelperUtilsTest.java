package org.folio.orders.utils;

import io.vertx.core.Future;
import org.folio.rest.acq.model.finance.Encumbrance;
import org.folio.rest.acq.model.finance.Transaction;
import org.folio.rest.core.exceptions.HttpException;
import org.folio.rest.jaxrs.model.CloseReason;
import org.folio.rest.jaxrs.model.Cost;
import org.folio.rest.jaxrs.model.PoLine;
import org.folio.rest.jaxrs.model.PurchaseOrder;
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
  void testShouldReturnEmptyString() {
    String act = HelperUtils.combineCqlExpressions("");
    assertThat(act, is(emptyOrNullString()));
  }

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
    ConversionQuery conversionQuery = HelperUtils.getConversionQuery(2.0d, "USD", "EUR");
    assertThat(conversionQuery.get(RATE_KEY, Double.class), is(2.0d));
    assertThat(conversionQuery.getBaseCurrency().getCurrencyCode(), is("USD"));
    assertThat(conversionQuery.getCurrency().getCurrencyCode(), is("EUR"));
  }

  @Test
  void testShouldBuildQueryWithoutExchangeRate() {
    String systemCurrency = "USD";
    String expectedCurrency = "EUR";
    Cost costOneTime =
      new Cost().withListUnitPrice(595d).withQuantityPhysical(1).withCurrency(expectedCurrency).withPoLineEstimatedPrice(595d);
    PoLine poLineOneTime =
      new PoLine().withId(UUID.randomUUID().toString()).withPurchaseOrderId(UUID.randomUUID().toString())
        .withCost(costOneTime);
    ConversionQuery actQuery = HelperUtils.buildConversionQuery(poLineOneTime, systemCurrency);
    assertEquals(actQuery.getCurrency().getCurrencyCode(), expectedCurrency);
    assertNull(actQuery.get(RATE_KEY, Double.class));
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

    assertTrue(HelperUtils.changeOrderStatus(purchaseOrder, poLines));
    assertEquals(purchaseOrder.getWorkflowStatus(), PurchaseOrder.WorkflowStatus.CLOSED);
    assertEquals(purchaseOrder.getCloseReason(), new CloseReason().withReason(REASON_CANCELLED));
  }

  @Test
  void testGetCurrencyFromTransactionByPoLineIdStringTransaction() {
    var expectedCurrency = "USD";
    var poLineId = UUID.randomUUID().toString();
    var transactionId = UUID.randomUUID().toString();

    var transactions = List.of(new Transaction().withId(transactionId)
      .withCurrency(expectedCurrency)
      .withEncumbrance(new Encumbrance().withSourcePoLineId(poLineId)));
    var poLine = new PoLine().withId(poLineId);

    var actualCurrency = HelperUtils.getCurrencyFromTransactionByPoLineId(transactions, poLine, null);

    assertEquals(expectedCurrency, actualCurrency);
  }

  @Test
  void testGetCurrencyFromTransactionByPoLineIdMultipleTransactions() {
    var expectedCurrency = "USD";
    var poLineId1 = UUID.randomUUID().toString();
    var poLineId2 = UUID.randomUUID().toString();
    var transactionId1 = UUID.randomUUID().toString();
    var transactionId2 = UUID.randomUUID().toString();

    var transactions = List.of(
      new Transaction().withId(transactionId1).withCurrency("EUR")
        .withEncumbrance(new Encumbrance().withSourcePoLineId(poLineId1)),
      new Transaction().withId(transactionId2).withCurrency(expectedCurrency)
        .withEncumbrance(new Encumbrance().withSourcePoLineId(poLineId2)));
    var poLine = new PoLine().withId(poLineId2);

    var actualCurrency = HelperUtils.getCurrencyFromTransactionByPoLineId(transactions, poLine, null);

    assertEquals(expectedCurrency, actualCurrency);
  }

  @Test
  void testGetCurrencyFromTransactionByPoLineIdTransactionsNotFound() {
    var expectedCurrency = "USD";

    var transactions = List.of(new Transaction().withId(UUID.randomUUID().toString())
      .withCurrency(expectedCurrency)
      .withEncumbrance(new Encumbrance().withSourcePoLineId(UUID.randomUUID().toString())));
    var poLine = new PoLine().withId(UUID.randomUUID().toString());

    var actualCurrency = HelperUtils.getCurrencyFromTransactionByPoLineId(transactions, poLine, expectedCurrency);

    assertEquals(expectedCurrency, actualCurrency);
  }

}
