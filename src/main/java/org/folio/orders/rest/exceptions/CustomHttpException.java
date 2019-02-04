package org.folio.orders.rest.exceptions;

import org.folio.orders.utils.ErrorCodes;

public class CustomHttpException extends Exception {
  private static final long serialVersionUID = 8109197918434861503L;

  private final int httpCode;
  private final ErrorCodes errorCode;

  public CustomHttpException(int httpCode, ErrorCodes errorCode) {
    this.errorCode = errorCode;
    this.httpCode = httpCode;
  }

  public CustomHttpException(int httpCode, ErrorCodes errorCode, String message) {
    super(message);
    this.errorCode = errorCode;
    this.httpCode = httpCode;
  }

  public CustomHttpException(int httpCode, ErrorCodes errorCode, Throwable cause) {
    super(cause);
    this.errorCode = errorCode;
    this.httpCode = httpCode;
  }

  public CustomHttpException(int httpCode, ErrorCodes errorCode, String message, Throwable cause) {
    super(message, cause);
    this.errorCode = errorCode;
    this.httpCode = httpCode;
  }

  public CustomHttpException(int httpCode, ErrorCodes errorCode, String message, Throwable cause,
                             boolean enableSuppression, boolean writableStackTrace) {
    super(message, cause, enableSuppression, writableStackTrace);
    this.errorCode = errorCode;
    this.httpCode = httpCode;
  }

  public int getHttpCode() {
    return httpCode;
  }

  public ErrorCodes getErrorCode() {
    return errorCode;
  }
}
