package org.folio.orders.rest.exceptions;

import org.apache.commons.lang3.StringUtils;
import org.folio.orders.utils.ErrorCodes;

public class HttpException extends RuntimeException {
  private static final long serialVersionUID = 8109197948434861504L;

  private final int code;
  private final String errorCode;

  public HttpException(int code, String message) {
    super(StringUtils.isNotEmpty(message) ? message : ErrorCodes.GENERIC_ERROR_CODE.getDescription());
    this.code = code;
    this.errorCode = ErrorCodes.GENERIC_ERROR_CODE.getCode();
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
