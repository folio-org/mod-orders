package org.folio.service;

import static org.folio.rest.RestVerticle.OKAPI_USERID_HEADER;

import java.util.Map;

public class UserService {

  public static String getCurrentUserId(Map<String, String> okapiHeaders) {
    return okapiHeaders.get(OKAPI_USERID_HEADER);
  }
}
