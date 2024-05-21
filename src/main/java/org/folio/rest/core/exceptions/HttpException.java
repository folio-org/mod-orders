package org.folio.rest.core.exceptions;

import java.util.Collections;
import java.util.List;

import org.apache.commons.collections4.CollectionUtils;
import org.apache.commons.lang3.StringUtils;
import org.folio.rest.jaxrs.model.Error;
import org.folio.rest.jaxrs.model.Errors;
import org.folio.rest.jaxrs.model.Parameter;

import com.google.common.collect.Lists;

public class HttpException extends RuntimeException {
  private final int code;
  private final transient Errors errors;

  public HttpException(int code, String message) {
    super(StringUtils.isNotEmpty(message) ? message : ErrorCodes.GENERIC_ERROR_CODE.getDescription());
    this.code = code;
    this.errors = new Errors()
      .withErrors(Collections.singletonList(new Error().withCode(ErrorCodes.GENERIC_ERROR_CODE.getCode()).withMessage(message)))
      .withTotalRecords(1);
  }

  public HttpException(int code, String message, Throwable cause) {
    super(message, cause);
    this.code = code;
    Parameter causeParam = new Parameter().withKey("cause").withValue(cause.getMessage());
    Error error = new Error()
      .withCode(ErrorCodes.GENERIC_ERROR_CODE.getCode())
      .withMessage(message)
      .withParameters(List.of(causeParam));
    this.errors = new Errors()
      .withErrors(List.of(error))
      .withTotalRecords(1);
  }

  public HttpException(int code, ErrorCodes errCodes) {
    this(code, errCodes, Lists.newArrayList());
  }

  public HttpException(int code, ErrorCodes errCodes, List<Parameter> parameters) {
    super(errCodes.getDescription());
    this.errors = new Errors()
      .withErrors(Collections.singletonList(new Error()
        .withCode(errCodes.getCode())
        .withMessage(errCodes.getDescription())
        .withParameters(parameters)))
      .withTotalRecords(1);
    this.code = code;
  }

  public HttpException(int code, Error error) {
    super(ExceptionUtil.errorAsString(error));
    this.code = code;
    this.errors = new Errors().withErrors(Collections.singletonList(error)).withTotalRecords(1);
  }

  public HttpException(int code, Errors errors) {
    super(ExceptionUtil.errorAsString(errors));
    this.code = code;
    this.errors = errors;
  }

  public int getCode() {
    return code;
  }

  public Errors getErrors() {
    if (errors != null && CollectionUtils.isNotEmpty(errors.getErrors())) {
      return errors;
    }
    return new Errors().withErrors(List.of(ErrorCodes.GENERIC_ERROR_CODE.toError())).withTotalRecords(1);
  }

  public Error getError() {
    if (CollectionUtils.isEmpty(errors.getErrors())) {
      return ErrorCodes.GENERIC_ERROR_CODE.toError();
    }
    return errors.getErrors().get(0);
  }
}
