package org.folio.rest.impl;

import org.folio.orders.events.handlers.MessageAddress;
import org.folio.rest.resource.interfaces.PostDeployVerticle;
import org.folio.spring.SpringContextUtil;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.beans.factory.annotation.Qualifier;

import io.vertx.core.AsyncResult;
import io.vertx.core.CompositeFuture;
import io.vertx.core.Context;
import io.vertx.core.Future;
import io.vertx.core.Handler;
import io.vertx.core.Promise;
import io.vertx.core.Vertx;
import io.vertx.core.eventbus.EventBus;
import io.vertx.core.eventbus.Message;
import io.vertx.core.eventbus.MessageConsumer;
import io.vertx.core.json.JsonObject;
import io.vertx.core.logging.Logger;
import io.vertx.core.logging.LoggerFactory;

/**
 * The class initializes event bus handlers
 */
public class InitEventBus implements PostDeployVerticle {
  private final Logger logger = LoggerFactory.getLogger(InitEventBus.class);

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
    vertx.executeBlocking(blockingCodeFuture -> {
      EventBus eb = vertx.eventBus();

      // Create consumers and assign handlers
      Promise<Void> orderStatusRegistrationHandler = Promise.promise();
      Promise<Void> checkInOrderStatusRegistrationHandler = Promise.promise();
      Promise<Void> receiptStatusConsistencyHandler = Promise.promise();

      MessageConsumer<JsonObject> orderStatusConsumer = eb.localConsumer(MessageAddress.RECEIVE_ORDER_STATUS_UPDATE.address);
      MessageConsumer<JsonObject> checkInOrderStatusChangeConsumer = eb.localConsumer(MessageAddress.CHECKIN_ORDER_STATUS_UPDATE.address);
      MessageConsumer<JsonObject> receiptStatusConsumer = eb.localConsumer(MessageAddress.RECEIPT_STATUS.address);
      orderStatusConsumer.handler(receiveOrderStatusChangeHandler)
        .completionHandler(orderStatusRegistrationHandler);
      checkInOrderStatusChangeConsumer.handler(checkInOrderStatusChangeHandler)
        .completionHandler(checkInOrderStatusRegistrationHandler);
      receiptStatusConsumer.handler(receiptStatusHandler)
        .completionHandler(receiptStatusConsistencyHandler);

      CompositeFuture.all(orderStatusRegistrationHandler.future(), receiptStatusConsistencyHandler.future()
            , checkInOrderStatusRegistrationHandler.future())
        .onComplete(result -> {
          if (result.succeeded()) {
            blockingCodeFuture.complete();
          } else {
            blockingCodeFuture.fail(result.cause());
          }
        });
    }, result -> {
      if (result.succeeded()) {
        resultHandler.handle(Future.succeededFuture(true));
      } else {
        logger.error(result.cause());
        resultHandler.handle(Future.failedFuture(result.cause()));
      }
    });
  }
}
