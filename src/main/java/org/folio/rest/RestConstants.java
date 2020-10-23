package org.folio.rest;

public final class RestConstants {
  public static final String OKAPI_URL = "X-Okapi-Url";
  public static final String SEARCH_ENDPOINT = "%s?limit=%s&offset=%s%s";
  public static final int MAX_IDS_FOR_GET_RQ = 15;

  private RestConstants() {

  }
}
