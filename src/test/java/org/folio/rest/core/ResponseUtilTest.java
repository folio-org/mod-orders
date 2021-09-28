package org.folio.rest.core;

import static org.junit.jupiter.api.Assertions.assertEquals;
import static org.mockito.Mockito.doReturn;
import static org.mockito.Mockito.mock;

import org.folio.rest.core.exceptions.HttpException;
import org.junit.jupiter.api.Test;

import io.vertx.core.AsyncResult;
import io.vertx.core.Promise;
import io.vertx.pgclient.PgException;

public class ResponseUtilTest {


  @Test
  public void testIfBadRequestMessageNotNull() {
    AsyncResult reply = mock(AsyncResult.class);
    doReturn(new PgException("message", "P1", "22P02", "Detail")).when(reply).cause();
    Promise promise = Promise.promise();

    ResponseUtil.handleFailure(promise, reply);

    HttpException exception = (HttpException) promise.future().cause();
    assertEquals(exception.getCode(), 500);
  }
}
