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
  private static final UUID TEMPLATE_REQUEST_ID = UUID.fromString("9465105a-e8a1-470c-9817-142d33bc4fcd");
  private static final String TEMPLATE_REQUEST_LANG = "en";
  private static final String TEMPLATE_REQUEST_OUTPUT = "text/html";
  private static final String ROUTING_LIST_ENDPOINT = resourcesPath(ROUTING_LISTS);
  private static final String ROUTING_LIST_BY_ID_ENDPOINT = ROUTING_LIST_ENDPOINT + "/{id}";
  private static final String TEMPLATE_REQUEST_ENDPOINT = resourcesPath(TEMPLATE_REQUEST);

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
    var requestEntry = new RequestEntry(ROUTING_LIST_BY_ID_ENDPOINT).withId(routingListId);
    return restClient.get(requestEntry, RoutingList.class, requestContext);
  }

  private Future<TemplateProcessingRequest> fetchUsersAndCreateTemplate(RoutingList routingList, RequestContext requestContext) {
    return userService.getUsersByIds(routingList.getUserIds(), requestContext)
      .map(users -> createTemplateRequest(routingList, users));
  }

  private TemplateProcessingRequest createTemplateRequest(RoutingList routingList, JsonObject users) {
    var templateRequest = createBaseTemplateRequest();
    var usersForContext = createUsersForContext(users);
    var routingListForContext = createRoutingListForContext(routingList);
    templateRequest.setContext(new TemplateProcessingRequest.Context()
        .setRoutingList(routingListForContext)
        .setUsers(usersForContext));
    log.info("createTemplateRequest:: TemplateProcessingRequest object created for routing list name: {}",
      templateRequest.getContext().getRoutingList().getName());
    // troubleshoot purposes, it will be removed because it contains personal information
    log.info("createTemplateRequest:: TemplateProcessingRequest object created : {}",
      JsonObject.mapFrom(templateRequest).encodePrettily());
    return templateRequest;
  }

  private TemplateProcessingRequest createBaseTemplateRequest() {
    return new TemplateProcessingRequest()
      .setTemplateId(TEMPLATE_REQUEST_ID)
      .setLang(TEMPLATE_REQUEST_LANG)
      .setOutputFormat(TEMPLATE_REQUEST_OUTPUT);
  }

  private List<TemplateProcessingRequest.User> createUsersForContext(JsonObject users) {
    return users.getJsonArray("users").stream()
      .map(JsonObject.class::cast)
      .map(user -> user.getJsonObject("personal"))
      .map(personalData ->
        new TemplateProcessingRequest.User()
          .setFirstName(personalData.getString("firstName"))
          .setLastName(personalData.getString("lastName"))
          .setRoutingAddress(personalData.getJsonArray("addresses")
            .getJsonObject(0).getString("addressLine1"))
      ).toList();
  }

  private TemplateProcessingRequest.RoutingList createRoutingListForContext(RoutingList routingList) {
    return new TemplateProcessingRequest.RoutingList()
      .setName(routingList.getName())
      .setNotes(routingList.getNotes());
  }

  private Future<JsonObject> postTemplateRequest(TemplateProcessingRequest templateRequest, RequestContext requestContext) {
    var requestEntry = new RequestEntry(TEMPLATE_REQUEST_ENDPOINT);
    log.info("postTemplateRequest:: Sending template request with routing list name={}", templateRequest.getContext().getRoutingList().getName());
    return restClient.postJsonObject(requestEntry, JsonObject.mapFrom(templateRequest), requestContext);
  }
}
