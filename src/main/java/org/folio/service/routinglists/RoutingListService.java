package org.folio.service.routinglists;

import static org.folio.orders.utils.ResourcePathResolver.ORDER_SETTINGS;
import static org.folio.orders.utils.ResourcePathResolver.ROUTING_LISTS;
import static org.folio.orders.utils.ResourcePathResolver.TEMPLATE_REQUEST;
import static org.folio.orders.utils.ResourcePathResolver.resourcesPath;

import java.util.ArrayList;
import java.util.Collections;
import java.util.Comparator;
import java.util.List;
import java.util.Map;
import java.util.Objects;
import java.util.UUID;
import java.util.stream.Collectors;

import org.apache.commons.collections4.CollectionUtils;
import org.apache.commons.lang3.ObjectUtils;
import org.apache.commons.lang3.StringUtils;
import org.apache.kafka.common.errors.ResourceNotFoundException;
import org.apache.logging.log4j.LogManager;
import org.apache.logging.log4j.Logger;
import org.folio.models.TemplateProcessingRequest;
import org.folio.models.UserCollection;
import org.folio.okapi.common.GenericCompositeFuture;
import org.folio.rest.RestConstants;
import org.folio.rest.acq.model.SettingCollection;
import org.folio.rest.core.RestClient;
import org.folio.rest.core.exceptions.HttpException;
import org.folio.rest.core.models.RequestContext;
import org.folio.rest.core.models.RequestEntry;
import org.folio.rest.jaxrs.model.Error;
import org.folio.rest.jaxrs.model.Errors;
import org.folio.rest.jaxrs.model.RoutingList;
import org.folio.rest.jaxrs.model.RoutingListCollection;
import org.folio.service.UserService;
import org.folio.service.orders.PurchaseOrderLineService;
import org.folio.service.routinglists.validators.RoutingListValidatorUtil;

import io.vertx.core.Future;
import io.vertx.core.json.JsonObject;

public class RoutingListService {
  private static final Logger logger = LogManager.getLogger(RoutingListService.class);

  private static final String ROUTING_LIST_ENDPOINT = resourcesPath(ROUTING_LISTS);
  private static final String ORDER_SETTINGS_ENDPOINT = resourcesPath(ORDER_SETTINGS);
  private static final String ROUTING_USER_ADDRESS_TYPE_ID = "ROUTING_USER_ADDRESS_TYPE_ID";
  private static final String ROUTING_LIST_BY_ID_ENDPOINT = ROUTING_LIST_ENDPOINT + "/{id}";
  private static final String ROUTING_LIST_BY_POL_ID = "poLineId==%s";

  private static final String TEMPLATE_REQUEST_ENDPOINT = resourcesPath(TEMPLATE_REQUEST);
  private static final UUID TEMPLATE_REQUEST_ID = UUID.fromString("9465105a-e8a1-470c-9817-142d33bc4fcd");
  private static final String TEMPLATE_REQUEST_LANG = "en";
  private static final String TEMPLATE_REQUEST_OUTPUT = "text/html";

  private final PurchaseOrderLineService poLineService;
  private final UserService userService;
  private final RestClient restClient;

  public RoutingListService(RestClient restClient, PurchaseOrderLineService poLineService, UserService userService) {
    this.restClient = restClient;
    this.poLineService = poLineService;
    this.userService = userService;
  }

  public Future<RoutingList> getRoutingList(String rListId, RequestContext requestContext) {
    RequestEntry requestEntry = new RequestEntry(ROUTING_LIST_BY_ID_ENDPOINT).withId(rListId);
    return restClient.get(requestEntry, RoutingList.class, requestContext);
  }

  public Future<Void> updateRoutingList(RoutingList routingList, RequestContext requestContext) {
    return validateRoutingList(routingList, requestContext)
      .compose(f -> {
        RequestEntry requestEntry = new RequestEntry(ROUTING_LIST_BY_ID_ENDPOINT).withId(routingList.getId());
        return restClient.put(requestEntry, routingList, requestContext);
      });
  }

  public Future<Void> deleteRoutingList(String rListId, RequestContext requestContext) {
    RequestEntry requestEntry = new RequestEntry(ROUTING_LIST_BY_ID_ENDPOINT).withId(rListId);
    return restClient.delete(requestEntry, requestContext);
  }

  public Future<RoutingList> createRoutingList(RoutingList routingList, RequestContext requestContext) {
    return validateRoutingList(routingList, requestContext)
      .compose(f -> {
        RequestEntry requestEntry = new RequestEntry(ROUTING_LIST_ENDPOINT).withId(routingList.getId());
        return restClient.post(requestEntry, routingList, RoutingList.class, requestContext);
      });
  }

  public Future<RoutingListCollection> getRoutingLists(int limit, int offset, String query, RequestContext requestContext) {
    RequestEntry requestEntry = new RequestEntry(ROUTING_LIST_ENDPOINT)
      .withLimit(limit)
      .withOffset(offset)
      .withQuery(query);
    return restClient.get(requestEntry, RoutingListCollection.class, requestContext);
  }

  private Future<RoutingListCollection> getRoutingListsByPoLineId(String poLineId, RequestContext requestContext) {
    String query = String.format(ROUTING_LIST_BY_POL_ID, poLineId);
    return getRoutingLists(Integer.MAX_VALUE, 0, query, requestContext);
  }

  private Future<Void> validateRoutingList(RoutingList rList, RequestContext requestContext) throws HttpException {
    var poLineFuture = poLineService.getOrderLineById(rList.getPoLineId(), requestContext);
    var routingListsFuture = getRoutingListsByPoLineId(rList.getPoLineId(), requestContext);
    return GenericCompositeFuture.all(List.of(poLineFuture, routingListsFuture)).compose(f -> {
      var poLine = poLineFuture.result();
      var routingLists = routingListsFuture.result();
      List<Error> combinedErrors = RoutingListValidatorUtil.validateRoutingList(rList, routingLists, poLine);
      if (CollectionUtils.isNotEmpty(combinedErrors)) {
        Errors errors = new Errors().withErrors(combinedErrors).withTotalRecords(combinedErrors.size());
        logger.error("Validation error: {}", JsonObject.mapFrom(errors).encodePrettily());
        throw new HttpException(RestConstants.VALIDATION_ERROR, errors);
      }
      return Future.succeededFuture();
    });
  }

  public Future<JsonObject> processTemplateRequest(String routingListId, RequestContext requestContext) {
    logger.debug("processTemplateRequest: Tying to process template request for routingListId={}", routingListId);
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
        .map(users -> sortUsersOrder(routingList, users))
        .map(users -> createTemplateRequest(routingList, users, addressTypId)));
  }

  private Future<String> getAddressTypeId(RequestContext requestContext) {
    var requestEntry = new RequestEntry(ORDER_SETTINGS_ENDPOINT)
      .withQuery("key=" + ROUTING_USER_ADDRESS_TYPE_ID);
    return restClient.get(requestEntry, SettingCollection.class, requestContext)
      .map(settingCollection -> {
        var settings = settingCollection.getSettings();
        if (ObjectUtils.isEmpty(settings) || StringUtils.isBlank(settings.get(0).getValue())) {
          logger.error("getAddressTypeId:: Setting is not found with key={}", ROUTING_USER_ADDRESS_TYPE_ID);
          throw new ResourceNotFoundException(String.format("Setting is not found with key=%s", ROUTING_USER_ADDRESS_TYPE_ID));
        }
        return settings.get(0).getValue();
      });
  }

  private UserCollection sortUsersOrder(RoutingList routingList, UserCollection userCollection) {
    Map<UUID, Integer> userIdToIndex = routingList.getUserIds().stream()
      .filter(Objects::nonNull)
      .collect(Collectors.toMap(UUID::fromString, routingList.getUserIds()::indexOf));

    // Sorting the users based on the index of their UUIDs in the routing list
    List<UserCollection.User> sortedUsers = new ArrayList<>(userCollection.getUsers());
    sortedUsers.sort(Comparator.comparingInt(user -> userIdToIndex.getOrDefault(user.getId(), Integer.MAX_VALUE)));

    userCollection.withUsers(sortedUsers);
    return userCollection;
  }

  private TemplateProcessingRequest createTemplateRequest(RoutingList routingList, UserCollection users, String addressTypeId) {
    var templateRequest = createBaseTemplateRequest();
    templateRequest.withContext(new TemplateProcessingRequest.Context()
      .withRoutingList(fillRoutingListForContext(routingList))
      .withUsers(fillUsersForContext(users, addressTypeId)));

    logger.info("createTemplateRequest:: TemplateProcessingRequest object created for routing list name: {}",
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
        logger.info("getUserAddress:: Required address with addressTypeId={} is found", addressTypeId);
        return address.getAddressLine1();
      }
    }
    logger.warn("getUserAddress:: Required address is not found with addressTypId={}", addressTypeId);
    return "";
  }

  private RoutingList fillRoutingListForContext(RoutingList routingList) {
    return new RoutingList()
      .withName(routingList.getName())
      .withNotes(routingList.getNotes());
  }

  private Future<JsonObject> postTemplateRequest(TemplateProcessingRequest templateRequest, RequestContext requestContext) {
    var requestEntry = new RequestEntry(TEMPLATE_REQUEST_ENDPOINT);
    logger.info("postTemplateRequest:: Sending template request with routing list name={}", templateRequest.getContext().getRoutingList().getName());
    return restClient.postJsonObject(requestEntry, JsonObject.mapFrom(templateRequest), requestContext);
  }

}
