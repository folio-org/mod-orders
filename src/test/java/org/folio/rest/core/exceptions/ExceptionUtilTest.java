package org.folio.rest.core.exceptions;

import static org.folio.rest.core.exceptions.ErrorCodes.GENERIC_ERROR_CODE;
import static org.folio.rest.core.exceptions.ErrorCodes.POSTGRE_SQL_ERROR;
import static org.junit.jupiter.api.Assertions.assertEquals;

import org.folio.rest.jaxrs.model.Errors;
import org.junit.jupiter.api.Assertions;
import org.junit.jupiter.api.Test;

import io.vertx.pgclient.PgException;

public class ExceptionUtilTest {


  @Test
  void testIfBadRequestMessageNotNull() {
    PgException reply = new PgException("Message Error 21P02", "P1", "21P02", "Detail");

    Errors errors = ExceptionUtil.convertToErrors(reply);

    assertEquals(POSTGRE_SQL_ERROR.getDescription(), errors.getErrors().get(0).getMessage());
    assertEquals(POSTGRE_SQL_ERROR.getCode(), errors.getErrors().get(0).getCode());
    assertEquals(3, errors.getErrors().get(0).getParameters().size());
  }

  @Test
  void testIfBadRequestAndExceptionIsVertxHttp() {
    io.vertx.ext.web.handler.HttpException reply = new io.vertx.ext.web.handler.HttpException(400, "Test");

    Errors errors = ExceptionUtil.convertToErrors(reply);

    assertEquals("Test", errors.getErrors().get(0).getMessage());
    assertEquals("400", errors.getErrors().get(0).getCode());
  }

  @Test
  void testIfBadRequestAndExceptionIsLocalHttp() {
    HttpException reply = new HttpException(400, "Test");

    Errors errors = ExceptionUtil.convertToErrors(reply);

    assertEquals("Test", errors.getErrors().get(0).getMessage());
    assertEquals(GENERIC_ERROR_CODE.getCode(), errors.getErrors().get(0).getCode());
  }

  @Test
  void testIsExceptionMessageIsJSON() {
    String errorMsg = "{\"message\":\"Test\",\"code\":\"Test\",\"parameters\":[]}";
    boolean act = ExceptionUtil.isErrorMessageJson(errorMsg);
    Assertions.assertTrue(act);
  }
}
