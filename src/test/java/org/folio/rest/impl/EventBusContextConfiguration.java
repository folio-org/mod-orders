package org.folio.rest.impl;

import java.util.ArrayList;
import java.util.List;

import org.apache.logging.log4j.LogManager;
import org.apache.logging.log4j.Logger;
import org.folio.service.orders.PurchaseOrderLineService;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.context.annotation.Bean;
import org.springframework.context.annotation.Configuration;
import org.springframework.context.annotation.Primary;

import io.vertx.core.Handler;
import io.vertx.core.eventbus.Message;
import io.vertx.core.json.JsonObject;

/**
 * Define unit test specific beans to override actual ones
 */
@Configuration
public class EventBusContextConfiguration {
  private static final Logger logger = LogManager.getLogger();
  // The variable is defined in main thread but the value is going to be inserted in vert.x event loop thread
  public static volatile List<Message<JsonObject>> eventMessages = new ArrayList<>();

  @Autowired
  private PurchaseOrderLineService purchaseOrderLineService;

  @Bean("checkInOrderStatusChangeHandler")
  @Primary
  public Handler<Message<JsonObject>> mockedCheckInOrderStatusChangeHandler() {
    // As an implementation just add received message to list
    return message -> {
      logger.info("New message sent to {} address", message.address());
      eventMessages.add(message);
    };
  }

  @Bean("receiveOrderStatusChangeHandler")
  @Primary
  public Handler<Message<JsonObject>> mockedOrderStatusHandler() {
    // As an implementation just add received message to list
    return message -> {
      logger.info("New message sent to {} address", message.address());
      eventMessages.add(message);
    };
  }


  @Bean("receiptStatusHandler")
  @Primary
  public Handler<Message<JsonObject>> mockedReceiptStatusHandler() {
    // As an implementation just add received message to list
    return message -> {
      logger.info("New message sent to {} address", message.address());
      eventMessages.add(message);
    };
  }
}
