package org.folio.rest.impl;

import org.apache.logging.log4j.LogManager;
import org.apache.logging.log4j.Logger;
import org.folio.okapi.common.GenericCompositeFuture;
import org.folio.orders.events.handlers.MessageAddress;
import org.folio.rest.resource.interfaces.PostDeployVerticle;
import org.folio.spring.SpringContextUtil;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.beans.factory.annotation.Qualifier;

import io.vertx.core.AsyncResult;
import io.vertx.core.Context;
import io.vertx.core.Future;
import io.vertx.core.Handler;
import io.vertx.core.Vertx;
import io.vertx.core.eventbus.EventBus;
import io.vertx.core.eventbus.Message;
import io.vertx.core.eventbus.MessageConsumer;
import io.vertx.core.json.JsonObject;

import java.util.List;

/**
 * The class initializes event bus handlers
 */
public class InitEventBus implements PostDeployVerticle {
  private final Logger logger = LogManager.getLogger();

  @Autowired
  @Qualifier("receiveOrderStatusChangeHandler")
  Handler<Message<JsonObject>> receiveOrderStatusChangeHandler;

  @Autowired
  @Qualifier("checkInOrderStatusChangeHandler")
  Handler<Message<JsonObject>> checkInOrderStatusChangeHandler;

  @Autowired
  @Qualifier("receiptStatusHandler")
  Handler<Message<JsonObject>> receiptStatusHandler;

  public InitEventBus() {
    SpringContextUtil.autowireDependencies(this, Vertx.currentContext());
  }

  @Override
  public void init(Vertx vertx, Context context, Handler<AsyncResult<Boolean>> resultHandler) {
    vertx.executeBlocking(() -> {
      EventBus eb = vertx.eventBus();

      MessageConsumer<JsonObject> orderStatusConsumer = eb.localConsumer(MessageAddress.RECEIVE_ORDER_STATUS_UPDATE.address);
      MessageConsumer<JsonObject> checkInOrderStatusChangeConsumer = eb.localConsumer(MessageAddress.CHECKIN_ORDER_STATUS_UPDATE.address);
      MessageConsumer<JsonObject> receiptStatusConsumer = eb.localConsumer(MessageAddress.RECEIPT_STATUS.address);
      orderStatusConsumer.handler(receiveOrderStatusChangeHandler);
      checkInOrderStatusChangeConsumer.handler(checkInOrderStatusChangeHandler);
      receiptStatusConsumer.handler(receiptStatusHandler);

      return GenericCompositeFuture.join(List.of(orderStatusConsumer.completion(), checkInOrderStatusChangeConsumer.completion(), receiptStatusConsumer.completion()))
        .onComplete(result -> {
          if (result.succeeded()) {
            resultHandler.handle(Future.succeededFuture(true));
          } else {
            logger.error(result.cause());
            resultHandler.handle(Future.failedFuture(result.cause()));
          }
        });
    });
  }
}
