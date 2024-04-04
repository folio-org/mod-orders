package org.folio.service;

import static org.folio.orders.utils.ResourcePathResolver.ORDER_SETTINGS;
import static org.folio.orders.utils.ResourcePathResolver.ROUTING_LISTS;
import static org.folio.orders.utils.ResourcePathResolver.TEMPLATE_REQUEST;
import static org.folio.orders.utils.ResourcePathResolver.resourcesPath;

import java.util.Collections;
import java.util.List;
import java.util.UUID;

import io.vertx.core.Future;
import io.vertx.core.json.JsonObject;
import org.apache.commons.lang3.ObjectUtils;
import org.apache.commons.lang3.StringUtils;
import org.apache.kafka.common.errors.ResourceNotFoundException;
import org.apache.logging.log4j.LogManager;
import org.apache.logging.log4j.Logger;
import org.folio.models.TemplateProcessingRequest;
import org.folio.models.UserCollection;
import org.folio.rest.acq.model.SettingCollection;
import org.folio.rest.core.RestClient;
import org.folio.rest.core.models.RequestContext;
import org.folio.rest.core.models.RequestEntry;
import org.folio.rest.jaxrs.model.RoutingList;

public class RoutingListService {

  private static final Logger log = LogManager.getLogger();
  private static final UUID TEMPLATE_REQUEST_ID = UUID.fromString("9465105a-e8a1-470c-9817-142d33bc4fcd");
  private static final String TEMPLATE_REQUEST_LANG = "en";
  private static final String TEMPLATE_REQUEST_OUTPUT = "text/html";
  private static final String ROUTING_LIST_ENDPOINT = resourcesPath(ROUTING_LISTS);
  private static final String ORDER_SETTINGS_ENDPOINT = resourcesPath(ORDER_SETTINGS);
  private static final String ROUTING_USER_ADDRESS_TYPE_ID = "ROUTING_USER_ADDRESS_TYPE_ID";
  private static final String ROUTING_LIST_BY_ID_ENDPOINT = ROUTING_LIST_ENDPOINT + "/{id}";
  private static final String TEMPLATE_REQUEST_ENDPOINT = resourcesPath(TEMPLATE_REQUEST);

  private final RestClient restClient;
  private final UserService userService;

  public RoutingListService(RestClient restClient, UserService userService) {
    this.restClient = restClient;
    this.userService = userService;
  }

  public Future<JsonObject> processTemplateRequest(String routingListId, RequestContext requestContext) {
    log.debug("processTemplateRequest: Tying to process template request for routingListId={}", routingListId);
    return getRoutingListById(routingListId, requestContext)
      .compose(routingList -> getUsersAndCreateTemplate(routingList, requestContext))
      .compose(templateProcessingRequest -> postTemplateRequest(templateProcessingRequest, requestContext));
  }

  public Future<RoutingList> getRoutingListById(String routingListId, RequestContext requestContext) {
    var requestEntry = new RequestEntry(ROUTING_LIST_BY_ID_ENDPOINT).withId(routingListId);
    return restClient.get(requestEntry, RoutingList.class, requestContext);
  }

  private Future<TemplateProcessingRequest> getUsersAndCreateTemplate(RoutingList routingList, RequestContext requestContext) {
    return getAddressTypeId(requestContext)
      .compose(addressTypId -> userService.getUsersByIds(routingList.getUserIds(), requestContext)
        .map(users -> createTemplateRequest(routingList, users, addressTypId)));
  }

  private TemplateProcessingRequest createTemplateRequest(RoutingList routingList, UserCollection users, String addressTypeId) {
    var templateRequest = createBaseTemplateRequest();
    templateRequest.withContext(new TemplateProcessingRequest.Context()
      .withRoutingList(fillRoutingListForContext(routingList))
      .withUsers(fillUsersForContext(users, addressTypeId)));

    log.info("createTemplateRequest:: TemplateProcessingRequest object created for routing list name: {}",
      templateRequest.getContext().getRoutingList().getName());
    return templateRequest;
  }

  private TemplateProcessingRequest createBaseTemplateRequest() {
    return new TemplateProcessingRequest()
      .withTemplateId(TEMPLATE_REQUEST_ID)
      .withLang(TEMPLATE_REQUEST_LANG)
      .withOutputFormat(TEMPLATE_REQUEST_OUTPUT);
  }

  private List<TemplateProcessingRequest.User> fillUsersForContext(UserCollection userCollection, String addressTypeId) {
    if (userCollection.getUsers().isEmpty()) {
      return Collections.emptyList();
    }
    return userCollection.getUsers().stream()
      .map(UserCollection.User::getPersonal)
      .filter(ObjectUtils::isNotEmpty)
      .map(personalData -> {
          var userForContext = new TemplateProcessingRequest.User()
            .withFirstName(personalData.getFirstName())
            .withLastName(personalData.getLastName());
          List<UserCollection.User.Personal.Address> addressList = personalData.getAddresses();
          if (addressList != null && !addressList.isEmpty()) {
            userForContext.withRoutingAddress(getUserAddress(addressList, addressTypeId));
          }
          return userForContext;
        }
      ).toList();
  }

  private String getUserAddress(List<UserCollection.User.Personal.Address> addressList, String addressTypeId) {
    for (UserCollection.User.Personal.Address address : addressList) {
      if (address.getAddressTypeId().equals(addressTypeId)) {
        log.info("getUserAddress:: Required address with addressTypeId={} is found", addressTypeId);
        return address.getAddressLine1();
      }
    }
    log.warn("getUserAddress:: Required address is not found with addressTypId={}", addressTypeId);
    return "";
  }

  private Future<String> getAddressTypeId(RequestContext requestContext) {
    var requestEntry = new RequestEntry(ORDER_SETTINGS_ENDPOINT)
      .withQuery("key=" + ROUTING_USER_ADDRESS_TYPE_ID);
    return restClient.get(requestEntry, SettingCollection.class, requestContext)
      .map(settingCollection -> {
        var settings = settingCollection.getSettings();
        if (ObjectUtils.isEmpty(settings) || StringUtils.isBlank(settings.get(0).getValue())) {
          log.error("getAddressTypeId:: Setting is not found with key={}", ROUTING_USER_ADDRESS_TYPE_ID);
          throw new ResourceNotFoundException("Setting is not found with key={}", ROUTING_USER_ADDRESS_TYPE_ID);
        }
        return settings.get(0).getValue();
      });
  }

  private RoutingList fillRoutingListForContext(RoutingList routingList) {
    return new RoutingList()
      .withName(routingList.getName())
      .withNotes(routingList.getNotes());
  }

  private Future<JsonObject> postTemplateRequest(TemplateProcessingRequest templateRequest, RequestContext requestContext) {
    var requestEntry = new RequestEntry(TEMPLATE_REQUEST_ENDPOINT);
    log.info("postTemplateRequest:: Sending template request with routing list name={}", templateRequest.getContext().getRoutingList().getName());
    return restClient.postJsonObject(requestEntry, JsonObject.mapFrom(templateRequest), requestContext);
  }
}
