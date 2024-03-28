package org.folio.service;

import static org.folio.orders.utils.HelperUtils.convertIdsToCqlQuery;
import static org.folio.orders.utils.ResourcePathResolver.USERS;
import static org.folio.orders.utils.ResourcePathResolver.resourcesPath;
import static org.folio.rest.RestVerticle.OKAPI_USERID_HEADER;

import java.util.List;
import java.util.Map;

import io.vertx.core.Future;
import io.vertx.core.json.JsonObject;
import org.folio.rest.core.RestClient;
import org.folio.rest.core.models.RequestContext;
import org.folio.rest.core.models.RequestEntry;

public class UserService {
  private static final String ENDPOINT = resourcesPath(USERS);
  private final RestClient restClient;

  public UserService(RestClient restClient) {
    this.restClient = restClient;
  }

  public static String getCurrentUserId(Map<String, String> okapiHeaders) {
    return okapiHeaders.get(OKAPI_USERID_HEADER);
  }

  public Future<JsonObject> getUsersByIds(List<String> userIds, RequestContext requestContext) {
    var requestEntry = new RequestEntry(USERS)
      .withOffset(0)
      .withLimit(Integer.MAX_VALUE)
      .withQuery(convertIdsToCqlQuery(userIds, "sourceInvoiceId"));

    return restClient.get(requestEntry, JsonObject.class, requestContext);
  }
}
