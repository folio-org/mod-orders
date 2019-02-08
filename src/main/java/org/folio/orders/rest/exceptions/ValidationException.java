package org.folio.orders.rest.exceptions;

import org.folio.orders.utils.ErrorCodes;
import org.folio.rest.jaxrs.model.Error;

public class ValidationException extends IllegalStateException {

  private static final String DEFAULT_ERROR_CODE = "-1";

  private final String errorCode;

  public ValidationException(String message, String errorCode) {
    super(message);
    this.errorCode = errorCode;
  }

  public ValidationException(ErrorCodes errorCodes) {
    super(errorCodes.getDescription());
    this.errorCode = errorCodes.getCode();
  }

  public ValidationException(String message) {
    this(message, DEFAULT_ERROR_CODE);
  }

  public Error getError() {
    return new Error().withMessage(getMessage()).withCode(errorCode);
  }
}
