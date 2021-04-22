package org.folio.orders.utils.validators;

import io.vertx.core.json.JsonObject;
import org.apache.logging.log4j.LogManager;
import org.apache.logging.log4j.Logger;
import org.folio.orders.rest.exceptions.HttpException;
import org.folio.rest.jaxrs.model.CompositePurchaseOrder;
import org.folio.rest.jaxrs.model.Ongoing;
import org.junit.jupiter.api.Assertions;
import org.junit.jupiter.api.BeforeEach;
import org.junit.jupiter.api.Test;
import org.mockito.MockitoAnnotations;

import java.io.IOException;

import static org.folio.TestConstants.COMP_ORDER_MOCK_DATA_PATH;
import static org.folio.TestConstants.ID;
import static org.folio.TestUtils.getMockAsJson;
import static org.folio.TestUtils.getMockData;

public class OngoingOrderValidatorTest {

  private static final Logger LOGGER = LogManager.getLogger();

  private static final String ORDERS_MOCK_DATA_PATH = COMP_ORDER_MOCK_DATA_PATH + "getOrders.json";

  @BeforeEach
  void initMocks() {
    MockitoAnnotations.openMocks(this);
  }

  @Test
  void testShouldThrowHttpExceptionOnRenewalDateIsNotSet() throws IOException {
    LOGGER.info("Test should return http exception on renewal date is not set.");

    CompositePurchaseOrder compositePurchaseOrder = getCompositePurchaseOrder();

    Ongoing ongoing = new org.folio.rest.jaxrs.model.Ongoing();
    ongoing.setRenewalDate(null);

    compositePurchaseOrder.setOrderType(CompositePurchaseOrder.OrderType.ONGOING);
    compositePurchaseOrder.setOngoing(ongoing);

    Assertions.assertThrows(HttpException.class, () -> OngoingOrderValidator.validate(compositePurchaseOrder));
  }

  @Test
  void shouldThrowHttpExceptionOnRenewalIntervalIsNotSet() throws IOException {
    LOGGER.info("Test should return http exception on renewal interval is not set.");

    CompositePurchaseOrder compositePurchaseOrder = getCompositePurchaseOrder();

    Ongoing ongoing = new org.folio.rest.jaxrs.model.Ongoing();
    ongoing.setInterval(null);

    compositePurchaseOrder.setOrderType(CompositePurchaseOrder.OrderType.ONGOING);
    compositePurchaseOrder.setOngoing(ongoing);

    Assertions.assertThrows(HttpException.class, () -> OngoingOrderValidator.validate(compositePurchaseOrder));
  }

  CompositePurchaseOrder getCompositePurchaseOrder() throws IOException {
    JsonObject ordersList = new JsonObject(getMockData(ORDERS_MOCK_DATA_PATH));
    String id = ordersList.getJsonArray("compositePurchaseOrders").getJsonObject(0).getString(ID);
    var order = getMockAsJson(COMP_ORDER_MOCK_DATA_PATH, id).mapTo(CompositePurchaseOrder.class);

    return order;
  }
}

