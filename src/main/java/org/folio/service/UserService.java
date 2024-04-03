package org.folio.service;

import static org.folio.orders.utils.HelperUtils.collectResultsOnSuccess;
import static org.folio.orders.utils.HelperUtils.convertIdsToCqlQuery;
import static org.folio.orders.utils.ResourcePathResolver.USERS;
import static org.folio.orders.utils.ResourcePathResolver.resourcesPath;
import static org.folio.rest.RestConstants.MAX_IDS_FOR_GET_RQ_15;
import static org.folio.rest.RestVerticle.OKAPI_USERID_HEADER;

import java.util.List;
import java.util.Map;

import io.vertx.core.Future;
import one.util.streamex.StreamEx;
import org.apache.commons.lang3.ObjectUtils;
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
    var futures = StreamEx.ofSubLists(userIds, MAX_IDS_FOR_GET_RQ_15)
      .map(ids -> getBatchUsersByIds(ids, requestContext))
      .toList();
    return collectResultsOnSuccess(futures)
      .map(this::combineUserCollections);
  }

  private Future<UserCollection> getBatchUsersByIds(List<String> userIds, RequestContext requestContext) {
    var requestEntry = new RequestEntry(USERS_ENDPOINT).withOffset(0).withLimit(Integer.MAX_VALUE)
      .withQuery(convertIdsToCqlQuery(userIds, "id"));
    return restClient.get(requestEntry, UserCollection.class, requestContext);
  }

  private UserCollection combineUserCollections(List<UserCollection> userCollections) {
    var userList = userCollections.stream()
      .filter(ObjectUtils::isNotEmpty)
      .flatMap(userCollection -> userCollection.getUsers().stream())
      .toList();
    return new UserCollection().withUsers(userList);
  }
}
