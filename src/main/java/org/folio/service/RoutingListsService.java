package org.folio.service;

import static org.folio.orders.utils.ResourcePathResolver.ROUTING_LISTS;
import static org.folio.orders.utils.ResourcePathResolver.TEMPLATE_REQUEST;
import static org.folio.orders.utils.ResourcePathResolver.resourcesPath;

import java.util.List;
import java.util.UUID;

import io.vertx.core.Future;
import io.vertx.core.json.JsonObject;
import org.apache.logging.log4j.LogManager;
import org.apache.logging.log4j.Logger;
import org.folio.models.TemplateProcessingRequest;
import org.folio.rest.core.RestClient;
import org.folio.rest.core.models.RequestContext;
import org.folio.rest.core.models.RequestEntry;
import org.folio.rest.jaxrs.model.RoutingList;

public class RoutingListsService {

  private static final Logger log = LogManager.getLogger();
  private static final String ENDPOINT = resourcesPath(ROUTING_LISTS);
  private static final String BY_ID_ENDPOINT = ENDPOINT + "/{id}";
  private final RestClient restClient;
  private final UserService userService;

  public RoutingListsService(RestClient restClient, UserService userService) {
    this.restClient = restClient;
    this.userService = userService;
  }

  public Future<JsonObject> processTemplateRequest(String routingListId, RequestContext requestContext) {
    log.debug("processTemplateRequest: Tying to process template request for routingListId={}", routingListId);
    return getRoutingListById(routingListId, requestContext)
      .compose(routingList -> fetchUsersAndCreateTemplate(routingList, requestContext))
      .compose(templateProcessingRequest -> postTemplateRequest(templateProcessingRequest, requestContext));
  }

  public Future<RoutingList> getRoutingListById(String routingListId, RequestContext requestContext) {
    var requestEntry = new RequestEntry(BY_ID_ENDPOINT).withId(routingListId);
    return restClient.get(requestEntry, RoutingList.class, requestContext);
  }

  private Future<TemplateProcessingRequest> fetchUsersAndCreateTemplate(RoutingList routingList, RequestContext requestContext) {
    return userService.getUsersByIds(routingList.getUserIds(), requestContext)
      .map(users -> createTemplateRequest(routingList, users));
  }

  private TemplateProcessingRequest createTemplateRequest(RoutingList routingList, JsonObject users) {
    var templateRequest = createBaseTemplateRequest();
    var userListForContext = createUserListForContext(users);
    var context = new TemplateProcessingRequest.Context().withUsers(userListForContext);
    templateRequest.withContext(context);
    log.info("createTemplateRequest:: TemplateProcessingRequest object created : {}", JsonObject.mapFrom(templateRequest).encodePrettily());
    return templateRequest;
  }

  private TemplateProcessingRequest createBaseTemplateRequest() {
    return new TemplateProcessingRequest()
      .withTemplateId(UUID.randomUUID())
      .withLang("en")
      .withOutputFormat("text/plain");
  }

  private List<TemplateProcessingRequest.User> createUserListForContext(JsonObject users) {
    return users.getJsonArray("users").stream()
      .map(JsonObject.class::cast)
      .map(user -> new TemplateProcessingRequest.User()
        .withName(user.getJsonObject("personal").getString("firstName"))
      )
      .toList();
  }

  private Future<JsonObject> postTemplateRequest(TemplateProcessingRequest templateProcessingRequest, RequestContext requestContext) {
    return restClient.post(TEMPLATE_REQUEST, JsonObject.mapFrom(templateProcessingRequest), JsonObject.class, requestContext);
  }
}
