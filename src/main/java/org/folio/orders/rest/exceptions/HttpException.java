package org.folio.orders.rest.exceptions;

import org.folio.orders.utils.ErrorCodes;

public class HttpException extends Exception {
  private static final long serialVersionUID = 8109197948434861504L;

  private static final String GENERIC_ERROR_CODE = "genericError";
  private final int code;
  private final String errorCode;

  public HttpException(int code, String message) {
    super(message);
    this.code = code;
    this.errorCode = GENERIC_ERROR_CODE;
  }

  public HttpException(int code, ErrorCodes errCodes) {
    super(errCodes.getDescription());
    this.errorCode = errCodes.getCode();
    this.code = code;
  }

  public int getCode() {
    return code;
  }

  public String getErrorCode() {
    return errorCode;
  }
}
