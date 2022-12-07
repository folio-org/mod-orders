package org.folio.rest.impl;

import javax.money.convert.MonetaryConversions;

import org.apache.logging.log4j.LogManager;
import org.apache.logging.log4j.Logger;
import org.folio.config.ApplicationConfig;
import org.folio.dbschema.ObjectMapperTool;
import org.folio.rest.resource.interfaces.InitAPI;
import org.folio.spring.SpringContextUtil;
import org.folio.verticle.DataImportConsumerVerticle;
import org.folio.verticle.consumers.SpringVerticleFactory;
import org.springframework.beans.factory.annotation.Value;
import org.springframework.context.support.AbstractApplicationContext;

import com.fasterxml.jackson.databind.DeserializationConfig;
import com.fasterxml.jackson.databind.SerializationConfig;

import io.vertx.core.AsyncResult;
import io.vertx.core.Context;
import io.vertx.core.DeploymentOptions;
import io.vertx.core.Future;
import io.vertx.core.Handler;
import io.vertx.core.Promise;
import io.vertx.core.Vertx;
import io.vertx.core.json.jackson.DatabindCodec;
import io.vertx.core.spi.VerticleFactory;

/**
 * The class initializes vertx context adding spring context
 */
public class InitAPIs implements InitAPI {

  private final Logger log = LogManager.getLogger();

  private static final String SPRING_CONTEXT_KEY = "springContext";

  @Value("${orders.kafka.consumer.instancesNumber:1}")
  private int dataImportConsumerInstancesNumber;

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
        initJavaMoney();
        handler.complete();

//        TODO: will be uncommented in scope of the https://issues.folio.org/browse/MODORDERS-773
//        SpringContextUtil.autowireDependencies(this, context);
//        deployConsumersVerticles(vertx).onSuccess(hdr -> {
//            handler.handle(Future.succeededFuture());
//            LOGGER.info("Consumer Verticles were successfully started");
//          })
//          .onFailure(th -> {
//            handler.handle(Future.failedFuture(th));
//            LOGGER.error("Consumer Verticles were not started", th);
//          });
      })
      .onComplete(result -> {
        if (result.succeeded()) {
          resultHandler.handle(Future.succeededFuture(true));
        } else {
          log.error("Failure to init API", result.cause());
          resultHandler.handle(Future.failedFuture(result.cause()));
        }
      });
  }

  private Future<?> deployConsumersVerticles(Vertx vertx) {
    AbstractApplicationContext springContext = vertx.getOrCreateContext().get(SPRING_CONTEXT_KEY);
    VerticleFactory verticleFactory = springContext.getBean(SpringVerticleFactory.class);
    vertx.registerVerticleFactory(verticleFactory);

    Promise<String> deployDataImportConsumerPromise = Promise.promise();

    vertx.deployVerticle(getVerticleName(verticleFactory, DataImportConsumerVerticle.class),
      new DeploymentOptions()
        .setWorker(true)
        .setInstances(dataImportConsumerInstancesNumber), deployDataImportConsumerPromise);

    return deployDataImportConsumerPromise.future();
  }

  private <T> String getVerticleName(VerticleFactory verticleFactory, Class<T> clazz) {
    return verticleFactory.prefix() + ":" + clazz.getName();
  }

  private void initJavaMoney() {
    try {
      log.info("Available currency rates providers {}", MonetaryConversions.getDefaultConversionProviderChain());
    } catch (Exception e){
      log.error("Java Money API preload failed", e);
    }
  }
}
