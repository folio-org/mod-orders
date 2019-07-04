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
  @Qualifier("orderStatusHandler")
  Handler<Message<JsonObject>> orderStatusHandler;

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
      Future<Void> orderStatusRegistrationHandler = Future.future();
      Future<Void> receiptStatusConsistencyHandler = Future.future();

      MessageConsumer<JsonObject> orderStatusConsumer = eb.localConsumer(MessageAddress.ORDER_STATUS.address);
      MessageConsumer<JsonObject> receiptStatusConsumer = eb.localConsumer(MessageAddress.RECEIPT_STATUS.address);
      orderStatusConsumer.handler(orderStatusHandler)
        .completionHandler(orderStatusRegistrationHandler);
      receiptStatusConsumer.handler(receiptStatusHandler)
        .completionHandler(receiptStatusConsistencyHandler);

      CompositeFuture.all(orderStatusRegistrationHandler, receiptStatusConsistencyHandler)
        .setHandler(result -> {
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
