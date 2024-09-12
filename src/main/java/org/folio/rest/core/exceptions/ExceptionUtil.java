package org.folio.rest.core.exceptions;

import static org.folio.rest.core.exceptions.ErrorCodes.GENERIC_ERROR_CODE;
import static org.folio.rest.core.exceptions.ErrorCodes.POSTGRE_SQL_ERROR;
import static org.folio.rest.core.exceptions.ErrorCodes.USER_HAS_MISSED_AFFILIATIONS;

import java.util.ArrayList;
import java.util.Collections;
import java.util.List;
import java.util.Map;
import java.util.Optional;
import java.util.regex.Matcher;
import java.util.regex.Pattern;
import java.util.stream.Collectors;

import org.apache.commons.collections4.MapUtils;
import org.apache.commons.lang3.StringUtils;
import org.folio.rest.jaxrs.model.Error;
import org.folio.rest.jaxrs.model.Errors;
import org.folio.rest.jaxrs.model.Parameter;
import org.folio.rest.persist.PgExceptionUtil;

import io.vertx.core.json.JsonObject;

public class ExceptionUtil {
  private static final String ERROR_CAUSE = "cause";
  private static final String SQL_STATE = "sqlstate";
  private static final String DETAIL = "detail";
  private static final String MESSAGE = "message";
  public static final String NOT_PROVIDED = "Not Provided";
  private static final Pattern ERROR_PATTERN = Pattern.compile("(message).*(code).*(parameters)");
  private static final Pattern ERRORS_PATTERN = Pattern.compile("(errors).*(message).*(code).*(parameters)");
  private static final Pattern AFFILIATION_MISSED_PATTERN = Pattern.compile("Invalid token: User with id ([0-9a-fA-F]{8}-[0-9a-fA-F]{4}-[0-9a-fA-F]{4}-[0-9a-fA-F]{4}-[0-9a-fA-F]{12}) does not exist");

  private ExceptionUtil() {
  }

  public static Errors convertToErrors(Throwable throwable) {
    final Throwable cause = Optional.ofNullable(throwable.getCause()).orElse(throwable);
    Errors errors;
    Map<Character,String> pgFields = PgExceptionUtil.getBadRequestFields(throwable);
    if (!MapUtils.isEmpty(pgFields)) {
      errors = convertPgExceptions(pgFields);
    } else if (cause instanceof io.vertx.ext.web.handler.HttpException httpException) {
      errors = convertVertxHttpException(httpException);
    } else if (cause instanceof HttpException httpException) {
      errors = httpException.getErrors();
      List<Error> errorList = errors.getErrors().stream().map(ExceptionUtil::mapToError).collect(Collectors.toList());
      errors.setErrors(errorList);
    } else {
      errors = new Errors().withErrors(Collections.singletonList(GENERIC_ERROR_CODE.toError()
                           .withAdditionalProperty(ERROR_CAUSE, cause.getMessage())))
                           .withTotalRecords(1);
    }
    return errors;
  }

  public static HttpException getHttpException(int statusCode, String error) {
    if (isAffiliationMissedError(error)) {
      return new HttpException(statusCode, USER_HAS_MISSED_AFFILIATIONS);
    }
    if (isErrorsMessageJson(error)) {
      return new HttpException(statusCode, mapToErrors(error));
    }
    return new HttpException(statusCode, error);
  }

  public static boolean isErrorMessageJson(String errorMessage) {
    if (!StringUtils.isEmpty(errorMessage)) {
      Matcher matcher = ERROR_PATTERN.matcher(errorMessage);
      if (matcher.find()) {
        return matcher.groupCount() == 3;
      }
    }
    return false;
  }

  public static boolean isErrorsMessageJson(String errorsMessage) {
    if (!StringUtils.isEmpty(errorsMessage)) {
      errorsMessage = errorsMessage.replace("\r", "").replace("\n", "");
      Matcher matcher = ERRORS_PATTERN.matcher(errorsMessage);
      if (matcher.find()) {
        return matcher.groupCount() == 4;
      }
    }
    return false;
  }

  public static boolean isAffiliationMissedError(String errorMessage) {
    if (StringUtils.isEmpty(errorMessage)) {
      return false;
    }
    Matcher matcher = AFFILIATION_MISSED_PATTERN.matcher(errorMessage);
    return matcher.find();
  }

  public static String errorAsString(Errors errors) {
    return Optional.ofNullable(JsonObject.mapFrom(errors).encode()).orElse(ErrorCodes.GENERIC_ERROR_CODE.getDescription());
  }

  public static String errorAsString(Error error) {
    return Optional.ofNullable(JsonObject.mapFrom(error).encode()).orElse(ErrorCodes.GENERIC_ERROR_CODE.getDescription());
  }

  private static Error mapToError(Error error) {
    if (isErrorMessageJson(error.getMessage())) {
      String jsonMessage = error.getMessage().substring(error.getMessage().indexOf("{"), error.getMessage().lastIndexOf("}") + 1);
      return new JsonObject(jsonMessage).mapTo(Error.class);
    }
    return error;
  }

  public static Errors mapToErrors(String errorsStr) {
    return new JsonObject(errorsStr).mapTo(Errors.class);
  }

  private static Errors convertVertxHttpException(io.vertx.ext.web.handler.HttpException throwable) {
    Errors errors;
    int code = throwable.getStatusCode();
    String message =  Optional.ofNullable(throwable.getPayload()).orElse(throwable.getMessage());
    Error error = new Error().withCode(String.valueOf(code)).withMessage(message);
    errors = new Errors().withErrors(Collections.singletonList(error)).withTotalRecords(1);
    return errors;
  }

  private static Errors convertPgExceptions( Map<Character,String> pgFields) {
    List<Parameter> parameters = new ArrayList<>();
    if (!MapUtils.isEmpty(pgFields)) {
      String sqlstate = pgFields.getOrDefault('C', NOT_PROVIDED);
      String detail = pgFields.getOrDefault('D', NOT_PROVIDED);
      String message = pgFields.getOrDefault('M', NOT_PROVIDED);
      parameters.add(new Parameter().withKey(SQL_STATE).withValue(sqlstate));
      parameters.add(new Parameter().withKey(DETAIL).withValue(detail));
      parameters.add(new Parameter().withKey(MESSAGE).withValue(message));
    } else {
      parameters.add(new Parameter().withKey(SQL_STATE).withValue(POSTGRE_SQL_ERROR.getCode()));
    }
    List<Error> errorList =  Collections.singletonList(POSTGRE_SQL_ERROR.toError().withParameters(parameters));
    return new Errors().withErrors(errorList).withTotalRecords(1);
  }

}
