package org.folio.orders.rest.exceptions;

import org.folio.rest.jaxrs.model.Error;

public class ValidationException extends IllegalStateException {

  private final String errorCode;

  public ValidationException(String message, String errorCode) {
    super(message);
    this.errorCode = errorCode;
  }

  public Error getError() {
    return new Error().withMessage(getMessage()).withCode(errorCode);
  }
}
