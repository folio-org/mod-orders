package org.folio.rest.core.exceptions;

import static org.folio.rest.core.exceptions.ErrorCodes.GENERIC_ERROR_CODE;
import static org.folio.rest.core.exceptions.ErrorCodes.POSTGRE_SQL_ERROR;
import static org.junit.jupiter.api.Assertions.assertEquals;
import static org.junit.jupiter.api.Assertions.assertFalse;
import static org.junit.jupiter.api.Assertions.assertTrue;

import org.folio.rest.jaxrs.model.Error;
import org.folio.rest.jaxrs.model.Errors;
import org.folio.rest.jaxrs.model.Parameter;
import org.junit.jupiter.api.Assertions;
import org.junit.jupiter.api.Test;

import io.vertx.pgclient.PgException;

import java.util.List;

public class ExceptionUtilTest {

  private static final String INVALID_TOKEN_USER_DOES_NOT_EXIST = """
    {
      "errors" : [ {
        "message" : "Invalid token: User with id 858cbba3-6fd0-4430-9011-9939574677d8 does not exist",
        "code" : "genericError",
        "parameters" : [ ]
      } ],
      "total_records" : 1
    }""";
  private static final String INVALID_TOKEN_USER_IS_NOT_ACTIVE = """
    {
      "errors" : [ {
        "message" : "Invalid token: user with id d107a00f-b3fe-44b2-ae88-44c43733d6cc is not active",
        "code" : "genericError",
        "parameters" : [ ]
      } ],
      "total_records" : 1
    }""";

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

  @Test
  void testGetHttpExceptionMissedAffiliationWhenUserDoesNotExist() {
    HttpException httpException = ExceptionUtil.getHttpException(401, INVALID_TOKEN_USER_DOES_NOT_EXIST);
    Error expected = ErrorCodes.USER_HAS_MISSED_AFFILIATIONS.toError()
      .withParameters(List.of(new Parameter().withKey("cause").withValue(INVALID_TOKEN_USER_DOES_NOT_EXIST)));
    assertEquals(expected, httpException.getError());
  }

  @Test
  void testGetHttpExceptionMissedAffiliationWhenUserIsNotActive() {
    HttpException httpException = ExceptionUtil.getHttpException(401, INVALID_TOKEN_USER_IS_NOT_ACTIVE);
    Error expected = ErrorCodes.USER_HAS_MISSED_AFFILIATIONS.toError()
      .withParameters(List.of(new Parameter().withKey("cause").withValue(INVALID_TOKEN_USER_IS_NOT_ACTIVE)));
    assertEquals(expected, httpException.getError());
  }

  @Test
  void testGetHttpExceptionOtherError() {
    HttpException httpException = ExceptionUtil.getHttpException(500, "Module failure");
    assertEquals(500, httpException.getCode());
    assertEquals(GENERIC_ERROR_CODE.getCode(), httpException.getError().getCode());
    assertEquals("Module failure", httpException.getError().getMessage());
  }

  @Test
  void testIsAffiliationMissedTrue() {
    boolean act = ExceptionUtil.isAffiliationMissedError(INVALID_TOKEN_USER_DOES_NOT_EXIST);
    assertTrue(act);
  }

  @Test
  void testIsAffiliationMissedFalse() {
    String errorMsg = "{\"message\":\"Test\",\"code\":\"Test\",\"parameters\":[]}";
    boolean act = ExceptionUtil.isAffiliationMissedError(errorMsg);
    assertFalse(act);
  }
}
