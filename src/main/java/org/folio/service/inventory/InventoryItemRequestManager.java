package org.folio.service.inventory;

import static org.folio.service.inventory.InventoryUtils.INVENTORY_LOOKUP_ENDPOINTS;
import static org.folio.service.inventory.InventoryUtils.REQUESTS;

import java.util.List;
import java.util.Objects;
import java.util.function.Function;
import java.util.stream.Collectors;

import org.apache.logging.log4j.LogManager;
import org.apache.logging.log4j.Logger;
import org.folio.okapi.common.GenericCompositeFuture;
import org.folio.rest.core.RestClient;
import org.folio.rest.core.models.RequestContext;
import org.folio.rest.core.models.RequestEntry;
import org.folio.service.CirculationRequestsRetriever;

import io.vertx.core.Future;
import io.vertx.core.json.JsonObject;

public class InventoryItemRequestManager {

  private static final Logger logger = LogManager.getLogger(InventoryItemRequestManager.class);

  private static final String REQUEST_CANCEL_MESSAGE = "Cancelling Request with id='{}'";
  private static final String REQUEST_MOVE_MESSAGE = "Moving Request with id='{}' to item with id='{}'";
  private static final String REQUEST_ENDPOINT = INVENTORY_LOOKUP_ENDPOINTS.get(REQUESTS) + "/%s";
  private static final String REQUEST_MOVE_ENDPOINT = REQUEST_ENDPOINT + "/move";
  private static final String REQUEST_JSON_KEY = "destinationItemId";

  private final CirculationRequestsRetriever circulationRequestsRetriever;
  private final RestClient restClient;

  public InventoryItemRequestManager(RestClient restClient, CirculationRequestsRetriever circulationRequestsRetriever) {
    this.restClient = restClient;
    this.circulationRequestsRetriever = circulationRequestsRetriever;
  }

  public Future<List<String>> getItemsWithActiveRequests(List<String> itemIds, RequestContext requestContext) {
    if (logger.isDebugEnabled()) {
      logger.debug("Filtering itemIds with active requests: {}", itemIds);
    }
    var futures = itemIds.stream()
      .map(itemId -> getItemWithRequests(itemId, requestContext))
      .collect(Collectors.toList());
    return GenericCompositeFuture.all(futures)
      .map(f -> f.<String>list().stream()
        .filter(Objects::nonNull)
        .toList());
  }

  private Future<String> getItemWithRequests(String itemId, RequestContext requestContext) {
    return circulationRequestsRetriever.getNumberOfRequestsByItemId(itemId, requestContext)
      .map(requests -> requests > 0 ? itemId : null);
  }

  public Future<Void> cancelItemRequests(String itemId, RequestContext requestContext) {
    return handleItemRequest(itemId, requestContext, reqId -> cancelRequest(reqId, requestContext));
  }

  public Future<Void> transferItemRequests(String originItemId, String destinationItemId, RequestContext requestContext) {
    return handleItemRequest(originItemId, requestContext, reqId -> transferRequest(reqId, destinationItemId, requestContext));
  }

  private Future<Void> handleItemRequest(String itemId, RequestContext requestContext, Function<String, Future<Void>> handler) {
    return circulationRequestsRetriever.getRequestIdsByItemId(itemId, requestContext)
      .compose(reqIds -> {
        var futures = reqIds.stream().map(handler).toList();
        return GenericCompositeFuture.all(futures);
      })
      .mapEmpty();
  }

  private Future<Void> cancelRequest(String reqId, RequestContext requestContext) {
    logger.info(REQUEST_CANCEL_MESSAGE, reqId);
    RequestEntry requestEntry = new RequestEntry(String.format(REQUEST_ENDPOINT, reqId));
    return restClient.delete(requestEntry, requestContext);
  }

  private Future<Void> transferRequest(String reqId, String itemId, RequestContext requestContext) {
      logger.info(REQUEST_MOVE_MESSAGE, reqId, itemId);
      JsonObject jsonObject =  JsonObject.of(REQUEST_JSON_KEY, itemId);
      RequestEntry requestEntry = new RequestEntry(String.format(REQUEST_MOVE_ENDPOINT, reqId));
      return restClient.postJsonObject(requestEntry, jsonObject, requestContext)
        .mapEmpty();
  }

}
