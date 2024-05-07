package org.folio.service.inventory;

import static org.folio.service.inventory.InventoryUtils.INVENTORY_LOOKUP_ENDPOINTS;
import static org.folio.service.inventory.InventoryUtils.REQUESTS;
import static org.folio.service.inventory.util.RequestFields.DESTINATION_KEY;

import java.util.List;
import java.util.Map;
import java.util.function.Function;

import org.apache.logging.log4j.LogManager;
import org.apache.logging.log4j.Logger;
import org.folio.okapi.common.GenericCompositeFuture;
import org.folio.rest.core.RestClient;
import org.folio.rest.core.models.RequestContext;
import org.folio.rest.core.models.RequestEntry;
import org.folio.service.CirculationRequestsRetriever;

import io.vertx.core.Future;
import io.vertx.core.json.JsonObject;

public class InventoryItemRequestService {

  private static final Logger logger = LogManager.getLogger(InventoryItemRequestService.class);

  private static final String CANCEL_MESSAGE = "cancelRequest:: Cancelling Request with id='{}'";
  private static final String MOVE_MESSAGE = "transferRequest:: Moving Request with id='{}' to item with id='{}'";
  private static final String FILTER_ITEMS_MESSAGE = "getItemsWithActiveRequests:: Filtering itemIds with active requests: {}";
  private static final String REQUEST_ENDPOINT = INVENTORY_LOOKUP_ENDPOINTS.get(REQUESTS) + "/%s";
  private static final String REQUEST_MOVE_ENDPOINT = REQUEST_ENDPOINT + "/move";

  private final CirculationRequestsRetriever circulationRequestsRetriever;
  private final RestClient restClient;

  public InventoryItemRequestService(RestClient restClient, CirculationRequestsRetriever circulationRequestsRetriever) {
    this.restClient = restClient;
    this.circulationRequestsRetriever = circulationRequestsRetriever;
  }

  public Future<List<String>> getItemsWithActiveRequests(List<String> itemIds, RequestContext requestContext) {
    if (logger.isDebugEnabled()) {
      logger.debug(FILTER_ITEMS_MESSAGE, itemIds);
    }
    return circulationRequestsRetriever.getNumbersOfRequestsByItemIds(itemIds, requestContext)
      .map(map -> map.entrySet().stream()
        .filter(e -> e.getValue() > 0)
        .map(Map.Entry::getKey)
        .toList());
  }

  public Future<Void> cancelItemsRequests(List<String> itemIds, RequestContext requestContext) {
    return handleItemsRequests(itemIds, requestContext, reqId -> cancelRequest(reqId, requestContext));
  }

  public Future<Void> transferItemsRequests(List<String> originItemIds, String destinationItemId, RequestContext requestContext) {
    return handleItemsRequests(originItemIds, requestContext, reqId -> transferRequest(reqId, destinationItemId, requestContext));
  }

  private Future<Void> handleItemsRequests(List<String> itemId, RequestContext requestContext, Function<String, Future<Void>> handler) {
    return circulationRequestsRetriever.getRequestIdsByItemIds(itemId, requestContext)
      .compose(reqIds -> {
        var futures = reqIds.stream().map(handler).toList();
        return GenericCompositeFuture.all(futures);
      })
      .mapEmpty();
  }

  private Future<Void> cancelRequest(String reqId, RequestContext requestContext) {
    logger.info(CANCEL_MESSAGE, reqId);
    RequestEntry requestEntry = new RequestEntry(String.format(REQUEST_ENDPOINT, reqId));
    return restClient.delete(requestEntry, requestContext);
  }

  private Future<Void> transferRequest(String reqId, String itemId, RequestContext requestContext) {
      logger.info(MOVE_MESSAGE, reqId, itemId);
      JsonObject jsonObject =  JsonObject.of(DESTINATION_KEY.getValue(), itemId);
      RequestEntry requestEntry = new RequestEntry(String.format(REQUEST_MOVE_ENDPOINT, reqId));
      return restClient.postJsonObject(requestEntry, jsonObject, requestContext)
        .mapEmpty();
  }

}
