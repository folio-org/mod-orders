package org.folio.service;

import static org.folio.orders.utils.HelperUtils.convertIdsToCqlQuery;
import static org.folio.orders.utils.ResourcePathResolver.USERS;
import static org.folio.orders.utils.ResourcePathResolver.resourcesPath;
import static org.folio.rest.RestVerticle.OKAPI_USERID_HEADER;

import java.util.List;
import java.util.Map;

import io.vertx.core.Future;
import org.folio.models.UserCollection;
import org.folio.rest.core.RestClient;
import org.folio.rest.core.models.RequestContext;
import org.folio.rest.core.models.RequestEntry;

public class UserService {

  private static final String USERS_ENDPOINT = resourcesPath(USERS);
  private final RestClient restClient;

  public UserService(RestClient restClient) {
    this.restClient = restClient;
  }

  public static String getCurrentUserId(Map<String, String> okapiHeaders) {
    return okapiHeaders.get(OKAPI_USERID_HEADER);
  }

  public Future<UserCollection> getUsersByIds(List<String> userIds, RequestContext requestContext) {
    var requestEntry = new RequestEntry(USERS_ENDPOINT).withOffset(0).withLimit(Integer.MAX_VALUE)
      .withQuery(convertIdsToCqlQuery(userIds, "id"));

    return restClient.get(requestEntry, UserCollection.class, requestContext);
  }
}
