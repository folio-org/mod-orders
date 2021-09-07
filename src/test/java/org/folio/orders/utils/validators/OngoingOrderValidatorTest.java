package org.folio.orders.utils.validators;

import io.vertx.core.json.JsonObject;
import org.folio.orders.rest.exceptions.HttpException;
import org.folio.rest.jaxrs.model.CompositePurchaseOrder;
import org.folio.rest.jaxrs.model.Ongoing;
import org.junit.jupiter.api.Assertions;
import org.junit.jupiter.api.BeforeEach;
import org.junit.jupiter.api.DisplayName;
import org.junit.jupiter.api.Test;
import org.mockito.MockitoAnnotations;

import java.io.IOException;
import java.util.Date;

import static org.folio.TestConstants.COMP_ORDER_MOCK_DATA_PATH;
import static org.folio.TestConstants.ID;
import static org.folio.TestUtils.getMockAsJson;
import static org.folio.TestUtils.getMockData;
import static org.hamcrest.MatcherAssert.assertThat;
import static org.hamcrest.Matchers.is;

@DisplayName("OngoingOrderValidator class ")
public class OngoingOrderValidatorTest {

  private static final String ORDERS_MOCK_DATA_PATH = COMP_ORDER_MOCK_DATA_PATH + "getOrders.json";

  @Test
  @DisplayName("should return http exception on renewal date is not set")
  void testShouldThrowHttpExceptionOnRenewalDateIsNotSet() throws IOException {
    CompositePurchaseOrder compositePurchaseOrder = getCompositePurchaseOrder();

    Ongoing ongoing = new org.folio.rest.jaxrs.model.Ongoing();
    ongoing.setIsSubscription(true);
    ongoing.setInterval(0);
    ongoing.setRenewalDate(null);

    compositePurchaseOrder.setOrderType(CompositePurchaseOrder.OrderType.ONGOING);
    compositePurchaseOrder.setOngoing(ongoing);

    HttpException httpException = Assertions.assertThrows(HttpException.class,
        () -> OngoingOrderValidator.validate(compositePurchaseOrder));

    assertThat(httpException.getError()
      .getCode(), is("renewalDateIsNotSet"));
    assertThat(httpException.getError()
      .getMessage(), is("Renewal date is not set"));
  }

  @Test
  @DisplayName("should return http exception on renewal interval is not set")
  void shouldThrowHttpExceptionOnRenewalIntervalIsNotSet() throws IOException {
    CompositePurchaseOrder compositePurchaseOrder = getCompositePurchaseOrder();

    Ongoing ongoing = new org.folio.rest.jaxrs.model.Ongoing();
    ongoing.setIsSubscription(true);
    ongoing.setRenewalDate(new Date());
    ongoing.setInterval(null);

    compositePurchaseOrder.setOrderType(CompositePurchaseOrder.OrderType.ONGOING);
    compositePurchaseOrder.setOngoing(ongoing);

    HttpException httpException = Assertions.assertThrows(HttpException.class,
        () -> OngoingOrderValidator.validate(compositePurchaseOrder));

    assertThat(httpException.getError()
      .getCode(), is("renewalIntervalIsNotSet"));
    assertThat(httpException.getError()
      .getMessage(), is("Renewal interval is not set"));
  }

  @Test
  @DisplayName("should not return http exception on no subscription")
  void shouldNotThrowHttpExceptionOnFalseSubscriptionSet() throws IOException {
    CompositePurchaseOrder compositePurchaseOrder = getCompositePurchaseOrder();

    Ongoing ongoing = new org.folio.rest.jaxrs.model.Ongoing();
    ongoing.setIsSubscription(false);
    ongoing.setRenewalDate(null);
    ongoing.setInterval(null);

    compositePurchaseOrder.setOrderType(CompositePurchaseOrder.OrderType.ONGOING);
    compositePurchaseOrder.setOngoing(ongoing);

    OngoingOrderValidator.validate(compositePurchaseOrder);
  }

  // Util method(s) :
  CompositePurchaseOrder getCompositePurchaseOrder() throws IOException {
    JsonObject ordersList = new JsonObject(getMockData(ORDERS_MOCK_DATA_PATH));
    String id = ordersList.getJsonArray("compositePurchaseOrders")
      .getJsonObject(0)
      .getString(ID);

    var order = getMockAsJson(COMP_ORDER_MOCK_DATA_PATH, id).mapTo(CompositePurchaseOrder.class);

    return order;
  }
}
