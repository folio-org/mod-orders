package org.folio.service.routinglists;

import io.vertx.core.Future;
import io.vertx.core.json.JsonObject;
import org.apache.commons.collections4.CollectionUtils;
import org.apache.logging.log4j.LogManager;
import org.apache.logging.log4j.Logger;
import org.folio.rest.RestConstants;
import org.folio.rest.core.RestClient;
import org.folio.rest.core.exceptions.HttpException;
import org.folio.rest.core.models.RequestContext;
import org.folio.rest.core.models.RequestEntry;
import org.folio.rest.jaxrs.model.Error;
import org.folio.rest.jaxrs.model.Errors;
import org.folio.rest.jaxrs.model.PoLine;
import org.folio.rest.jaxrs.model.RoutingList;
import org.folio.rest.jaxrs.model.RoutingListCollection;
import org.folio.service.orders.PurchaseOrderLineService;
import org.folio.service.routinglists.validators.RoutingListValidatorUtil;

import java.util.List;

import static org.folio.orders.utils.ResourcePathResolver.ROUTING_LIST_STORAGE;
import static org.folio.orders.utils.ResourcePathResolver.resourcesPath;

public class RoutingListsStorageService {
  private static final Logger logger = LogManager.getLogger(RoutingListsStorageService.class);

  private static final String ROUTING_LIST_STORAGE_ENDPOINT = resourcesPath(ROUTING_LIST_STORAGE);
  private static final String ROUTING_LIST_STORAGE_BY_ID_ENDPOINT = ROUTING_LIST_STORAGE_ENDPOINT + "/{id}";
  private static final String ROUTING_LIST_BY_POL_ID = "poLineId==%s";

  private final PurchaseOrderLineService poLineService;
  private final RestClient restClient;

  public RoutingListsStorageService(PurchaseOrderLineService poLineService, RestClient restClient) {
    this.poLineService = poLineService;
    this.restClient = restClient;
  }

  public Future<RoutingList> getRoutingList(String rListId, RequestContext requestContext) {
    RequestEntry requestEntry = new RequestEntry(ROUTING_LIST_STORAGE_BY_ID_ENDPOINT).withId(rListId);
    return restClient.get(requestEntry, RoutingList.class, requestContext);
  }

  public Future<Void> updateRoutingList(RoutingList routingList, RequestContext requestContext) {
    validateRoutingList(routingList, requestContext);
    RequestEntry requestEntry = new RequestEntry(ROUTING_LIST_STORAGE_BY_ID_ENDPOINT).withId(routingList.getId());
    return restClient.put(requestEntry, requestContext, requestContext);
  }

  public Future<Void> deleteRoutingList(String rListId, RequestContext requestContext) {
    RequestEntry requestEntry = new RequestEntry(ROUTING_LIST_STORAGE_BY_ID_ENDPOINT).withId(rListId);
    return restClient.delete(requestEntry, requestContext);
  }

  public Future<RoutingList> createRoutingList(RoutingList routingList, RequestContext requestContext) {
    validateRoutingList(routingList, requestContext);
    RequestEntry requestEntry = new RequestEntry(ROUTING_LIST_STORAGE_ENDPOINT);
    return restClient.post(requestEntry, routingList, RoutingList.class, requestContext);
  }

  public Future<RoutingListCollection> getRoutingLists(int limit, int offset, String query, RequestContext requestContext) {
    RequestEntry requestEntry = new RequestEntry(ROUTING_LIST_STORAGE_ENDPOINT)
      .withQuery(query)
      .withOffset(offset)
      .withLimit(limit);
    return restClient.get(requestEntry, RoutingListCollection.class, requestContext);
  }

  private Future<RoutingListCollection> getRoutingListsByPoLineId(String poLineId, RequestContext requestContext) {
    String query = String.format(ROUTING_LIST_BY_POL_ID, poLineId);
    return getRoutingLists(Integer.MAX_VALUE, 0, query, requestContext);
  }

  private void validateRoutingList(RoutingList routingList, RequestContext requestContext) {
    RoutingListCollection routingLists = getRoutingListsByPoLineId(routingList.getPoLineId(), requestContext).result();
    PoLine poLine = poLineService.getOrderLineById(routingList.getPoLineId(), requestContext).result();
    List<Error> combinedErrors = RoutingListValidatorUtil.validateRoutingList(routingLists, poLine);
    if (CollectionUtils.isNotEmpty(combinedErrors)) {
      Errors errors = new Errors().withErrors(combinedErrors).withTotalRecords(combinedErrors.size());
      logger.error("Validation error : " + JsonObject.mapFrom(errors).encodePrettily());
      throw new HttpException(RestConstants.VALIDATION_ERROR, errors);
    }
  }

}
