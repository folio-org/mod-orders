package org.folio.rest;

public final class RestConstants {
  public static final String OKAPI_URL = "x-okapi-url";
  public static final String OKAPI_TENANT_LOWCASE = "x-okapi-tenant";
  public static final String SEARCH_ENDPOINT = "%s?limit=%s&offset=%s%s";
  public static final String SEARCH_PARAMS = "?limit=%s&offset=%s%s";
  public static final String ERROR_CAUSE = "cause";

  public static final int MAX_IDS_FOR_GET_RQ_15 = 15;
  public static final int MAX_IDS_FOR_GET_RQ_30 = 30;
  public static final String MAX_POLINE_LIMIT = "999";

  public static final String PATH_PARAM_PLACE_HOLDER = ":id";
  public static final int BAD_REQUEST = 400;
  public static final int ACCESS_DENIED = 401;
  public static final int FORBIDDEN = 403;
  public static final int NOT_FOUND = 404;
  public static final int REQUEST_TIMEOUT = 408;
  public static final int VALIDATION_ERROR = 422;
  public static final int INTERNAL_SERVER_ERROR = 500;
  public static final String ERROR_MESSAGE = "errorMessage";
  public static final String EN = "en";
  public static final String ID = "id";

  public static final int SEMAPHORE_MAX_ACTIVE_THREADS = 5;


  private RestConstants() {

  }
}
