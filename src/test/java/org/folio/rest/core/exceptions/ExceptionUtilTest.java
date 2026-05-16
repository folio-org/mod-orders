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
import org.junit.jupiter.params.ParameterizedTest;
import org.junit.jupiter.params.provider.NullAndEmptySource;

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

    @ParameterizedTest
  @NullAndEmptySource
  void testIsAffiliationMissedWhenEmptyShouldReturnFalse(String errorMessage) {
    // TestMate-2bb210f8584f90e17dd9d2c09163579c
    // Given
    // errorMessage is null or empty
    // When
    boolean act = ExceptionUtil.isAffiliationMissedError(errorMessage);
    // Then
    assertFalse(act);
  }

    @Test
  void testGetHttpExceptionWhenErrorIsJsonErrorsObject() {
    // TestMate-e0f5d081e68f167412e62853b0b998ac
    // Given
    int statusCode = 422;
    String errorJson = "{\"errors\": [{\"message\": \"msg\", \"code\": \"code\", \"parameters\": []}], \"total_records\": 1}";
    // When
    HttpException httpException = ExceptionUtil.getHttpException(statusCode, errorJson);
    // Then
    assertEquals(422, httpException.getCode());
    Errors actualErrors = httpException.getErrors();
    assertEquals(1, actualErrors.getTotalRecords());
    List<Error> errorList = actualErrors.getErrors();
    assertEquals(1, errorList.size());
    assertEquals("code", errorList.get(0).getCode());
    assertEquals("msg", errorList.get(0).getMessage());
  }

    @Test
  void testGetHttpExceptionWhenErrorIsJsonErrorsObjectWithFormatting() {
    // TestMate-a0fb16b812b87bd16c84479f77598570
    // Given
    int statusCode = 400;
    String formattedErrorJson = "{\n \"errors\" : [ {\r\n \"message\": \"msg\", \"code\": \"code\", \"parameters\": [] } ]\n, \"total_records\": 1}";
    // When
    HttpException httpException = ExceptionUtil.getHttpException(statusCode, formattedErrorJson);
    // Then
    assertEquals(400, httpException.getCode());
    Errors actualErrors = httpException.getErrors();
    assertEquals(1, actualErrors.getTotalRecords());
    List<Error> errorList = actualErrors.getErrors();
    assertEquals(1, errorList.size());
    assertEquals("code", errorList.get(0).getCode());
    assertEquals("msg", errorList.get(0).getMessage());
  }

    @Test
  void testGetHttpExceptionWhenErrorIsEmpty() {
    // TestMate-ab949d92af727354fce5bdc95d854fb8
    // Given
    int statusCode = 400;
    String error = "";
    // When
    HttpException httpException = ExceptionUtil.getHttpException(statusCode, error);
    // Then
    assertEquals(400, httpException.getCode());
    Errors actualErrors = httpException.getErrors();
    assertEquals(1, actualErrors.getTotalRecords());
    List<Error> errorList = actualErrors.getErrors();
    assertEquals(1, errorList.size());
    assertEquals(GENERIC_ERROR_CODE.getCode(), errorList.get(0).getCode());
    // Based on HttpException(int code, String message) constructor implementation,
    // the Error object in the list preserves the empty string message passed to it,
    // even though the exception's super message defaults to the generic description.
    assertEquals("", errorList.get(0).getMessage());
  }

    @Test
  void testGetHttpExceptionWhenErrorIsMalformedJsonErrors() {
    // TestMate-4eda9f7e475bc0c5401d2a94dfc7111b
    // Given
    int statusCode = 400;
    String jsonMissingParameters = "{\"errors\": [{\"message\": \"msg\", \"code\": \"code\"}], \"total_records\": 1}";
    // When
    HttpException httpException = ExceptionUtil.getHttpException(statusCode, jsonMissingParameters);
    // Then
    assertEquals(400, httpException.getCode());
    Errors actualErrors = httpException.getErrors();
    assertEquals(1, actualErrors.getTotalRecords());
    List<Error> errorList = actualErrors.getErrors();
    assertEquals(1, errorList.size());
    assertEquals(GENERIC_ERROR_CODE.getCode(), errorList.get(0).getCode());
    assertEquals(jsonMissingParameters, errorList.get(0).getMessage());
  }
}
