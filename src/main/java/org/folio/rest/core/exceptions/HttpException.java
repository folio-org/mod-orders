package org.folio.rest.core.exceptions;

import java.util.ArrayList;
import java.util.Collections;

import org.apache.commons.lang3.StringUtils;
import org.folio.rest.jaxrs.model.Error;
import org.folio.rest.jaxrs.model.Errors;

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

  public HttpException(int code, ErrorCodes errCodes) {
    super(errCodes.getDescription());
    this.errors = new Errors()
      .withErrors(Collections.singletonList(new Error().withCode(errCodes.getCode()).withMessage(errCodes.getDescription())))
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
    return errors;
  }

  public Error getError() {
    return errors.getErrors().get(0);
  }
}
