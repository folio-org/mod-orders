package org.folio.rest.impl;

import org.folio.config.ApplicationConfig;
import org.folio.rest.resource.interfaces.InitAPI;
import org.folio.rest.tools.utils.ObjectMapperTool;
import org.folio.spring.SpringContextUtil;

import com.fasterxml.jackson.databind.DeserializationConfig;
import com.fasterxml.jackson.databind.SerializationConfig;

import io.vertx.core.AsyncResult;
import io.vertx.core.Context;
import io.vertx.core.Future;
import io.vertx.core.Handler;
import io.vertx.core.Vertx;
import io.vertx.core.json.jackson.DatabindCodec;
import io.vertx.core.logging.Logger;
import io.vertx.core.logging.LoggerFactory;

/**
 * The class initializes vertx context adding spring context
 */
public class InitAPIs implements InitAPI {
  private final Logger logger = LoggerFactory.getLogger(InitAPIs.class);

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
        handler.complete();
      },
      result -> {
        if (result.succeeded()) {
          resultHandler.handle(Future.succeededFuture(true));
        } else {
          logger.error("Failure to init API", result.cause());
          resultHandler.handle(Future.failedFuture(result.cause()));
        }
      });
  }
}
