package org.folio.service.routinglist;

import static org.folio.orders.utils.ResourcePathResolver.ROUTING_LISTS;
import static org.folio.orders.utils.ResourcePathResolver.TEMPLATE_REQUEST;
import static org.folio.orders.utils.ResourcePathResolver.resourcesPath;

import java.util.List;
import java.util.UUID;

import io.vertx.core.Future;
import io.vertx.core.json.JsonObject;
import lombok.extern.log4j.Log4j2;
import org.folio.models.template.TemplateProcessingRequest;
import org.folio.rest.core.RestClient;
import org.folio.rest.core.models.RequestContext;
import org.folio.rest.core.models.RequestEntry;
import org.folio.rest.jaxrs.model.RoutingList;
import org.folio.service.UserService;

@Log4j2
public class RoutingListService {

  private static final String ENDPOINT = resourcesPath(ROUTING_LISTS);
  private static final String BY_ID_ENDPOINT = ENDPOINT + "/{id}";
  private final RestClient restClient;
  private final UserService userService;

  public RoutingListService(RestClient restClient, UserService userService) {
    this.restClient = restClient;
    this.userService = userService;
  }

  public Future<JsonObject> processTemplateEngine(String id, RequestContext requestContext) {
    return getRoutingListById(id, requestContext)
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
