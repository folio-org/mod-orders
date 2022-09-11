package org.folio.rest.impl;

import io.vertx.core.AsyncResult;
import io.vertx.core.Context;
import io.vertx.core.DeploymentOptions;
import io.vertx.core.Future;
import io.vertx.core.Handler;
import io.vertx.core.Promise;
import io.vertx.core.Vertx;
import io.vertx.core.spi.VerticleFactory;
import org.apache.logging.log4j.LogManager;
import org.apache.logging.log4j.Logger;
import org.folio.config.ApplicationConfig;
import org.folio.dbschema.ObjectMapperTool;
import org.folio.okapi.common.GenericCompositeFuture;
import org.folio.rest.resource.interfaces.InitAPI;
import org.folio.spring.SpringContextUtil;

import com.fasterxml.jackson.databind.DeserializationConfig;
import com.fasterxml.jackson.databind.SerializationConfig;

import io.vertx.core.json.jackson.DatabindCodec;
import org.folio.verticle.DataImportConsumerVerticle;
import org.folio.verticle.consumers.SpringVerticleFactory;
import org.springframework.beans.factory.annotation.Value;
import org.springframework.context.support.AbstractApplicationContext;

import java.util.Arrays;

/**
 * The class initializes vertx context adding spring context
 */
public class InitAPIs implements InitAPI {

  private final Logger LOGGER = LogManager.getLogger();

  private static final String SPRING_CONTEXT_KEY = "springContext";

  @Value("${orders.kafka.OrderConsumer.instancesNumber:1}")
  private int orderConsumerInstancesNumber;

  @Override
  public void init(Vertx vertx, Context context, Handler<AsyncResult<Boolean>> resultHandler) {
    vertx.executeBlocking(
      handler -> {
        SerializationConfig serializationConfig = ObjectMapperTool.getMapper().getSerializationConfig();
        DeserializationConfig deserializationConfig = ObjectMapperTool.getMapper().getDeserializationConfig();

        DatabindCodec.mapper().setConfig(serializationConfig);
        DatabindCodec.prettyMapper().setConfig(serializationConfig);
        DatabindCodec.mapper().setConfig(deserializationConfig);
        DatabindCodec.prettyMapper().setConfig(deserializationConfig);
        SpringContextUtil.init(vertx, context, ApplicationConfig.class);
        SpringContextUtil.autowireDependencies(this, context);
        deployConsumersVerticles(vertx).onSuccess(hdr -> {
            handler.handle(Future.succeededFuture());
            LOGGER.info("Consumer Verticles were successfully started");
          })
          .onFailure(th -> {
            handler.handle(Future.failedFuture(th));
            LOGGER.error("Consumer Verticles were not started", th);
          });
      },
      result -> {
        if (result.succeeded()) {
          resultHandler.handle(Future.succeededFuture(true));
        } else {
          LOGGER.error("Failure to init API", result.cause());
          resultHandler.handle(Future.failedFuture(result.cause()));
        }
      });
  }

  private Future<?> deployConsumersVerticles(Vertx vertx) {
    AbstractApplicationContext springContext = vertx.getOrCreateContext().get(SPRING_CONTEXT_KEY);
    VerticleFactory verticleFactory = springContext.getBean(SpringVerticleFactory.class);
    vertx.registerVerticleFactory(verticleFactory);

    Promise<String> handler = Promise.promise();

    vertx.deployVerticle(getVerticleName(verticleFactory, DataImportConsumerVerticle.class),
      new DeploymentOptions()
        .setWorker(true)
        .setInstances(orderConsumerInstancesNumber), handler);

    return GenericCompositeFuture.all(Arrays.asList(handler.future()));
  }

  private <T> String getVerticleName(VerticleFactory verticleFactory, Class<T> clazz) {
    return verticleFactory.prefix() + ":" + clazz.getName();
  }
}
