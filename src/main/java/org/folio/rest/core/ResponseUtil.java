package org.folio.rest.core;

import static javax.ws.rs.core.Response.Status.INTERNAL_SERVER_ERROR;

import java.util.Optional;

import org.apache.logging.log4j.LogManager;
import org.apache.logging.log4j.Logger;
import org.folio.rest.core.exceptions.ExceptionUtil;
import org.folio.rest.core.exceptions.HttpException;
import org.folio.rest.jaxrs.model.Errors;

import io.vertx.core.AsyncResult;
import io.vertx.core.Promise;

public class ResponseUtil {
  private static final Logger logger = LogManager.getLogger(ResponseUtil.class);

  private ResponseUtil() {
  }

  public static void handleFailure(Promise<?> promise, Throwable throwable) {
    Throwable cause = Optional.ofNullable(throwable.getCause()).orElse(throwable);
    Errors errors = ExceptionUtil.convertToErrors(throwable);
    int httpCode = extractHttpCode(cause);
    if (logger.isErrorEnabled()) {
      logger.error("Failure : {}", ExceptionUtil.errorAsString(errors));
    }
    promise.fail(new HttpException(httpCode, errors));
  }

  public static void handleFailure(Promise<?> promise, AsyncResult<?> reply) {
    handleFailure(promise, reply.cause());
  }


  private static int extractHttpCode(Throwable cause) {
    if (cause instanceof io.vertx.ext.web.handler.HttpException e) {
      return e.getStatusCode();
    } else if (cause instanceof HttpException e){
      return e.getCode();
    }
    return INTERNAL_SERVER_ERROR.getStatusCode();
  }
}

