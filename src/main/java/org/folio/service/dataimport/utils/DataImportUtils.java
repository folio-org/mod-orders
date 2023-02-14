package org.folio.service.dataimport.utils;

import org.apache.commons.lang3.StringUtils;
import org.folio.DataImportEventPayload;
import org.folio.rest.RestConstants;
import org.folio.rest.RestVerticle;

import java.util.HashMap;
import java.util.Map;

public class DataImportUtils {

  public static final String PERMISSIONS_KEY = "USER_PERMISSIONS";
  public static final String USER_ID_KEY = "USER_ID";
  public static final String OKAPI_PERMISSIONS_HEADER = "X-Okapi-Permissions";

  private DataImportUtils() {
  }

  public static Map<String, String> extractOkapiHeaders(DataImportEventPayload eventPayload) {
    Map<String, String> headers = new HashMap<>();
    headers.put(RestVerticle.OKAPI_HEADER_TENANT, eventPayload.getTenant());
    headers.put(RestVerticle.OKAPI_HEADER_TOKEN, eventPayload.getToken());
    headers.put(RestConstants.OKAPI_URL, eventPayload.getOkapiUrl());

    String permissionsHeader = eventPayload.getContext().get(PERMISSIONS_KEY);
    if (StringUtils.isNotBlank(permissionsHeader)) {
      headers.put(OKAPI_PERMISSIONS_HEADER, permissionsHeader);
    }
    String userId = eventPayload.getContext().get(USER_ID_KEY);
    if (StringUtils.isNotBlank(userId)) {
      headers.put(RestVerticle.OKAPI_USERID_HEADER, userId);
    }
    return headers;
  }
}
