package org.folio.service.inventory;

import io.vertx.core.Future;
import io.vertx.core.json.JsonObject;
import org.apache.logging.log4j.LogManager;
import org.apache.logging.log4j.Logger;
import org.folio.okapi.common.GenericCompositeFuture;
import org.folio.rest.core.RestClient;
import org.folio.rest.core.models.RequestContext;
import org.folio.rest.core.models.RequestEntry;
import org.folio.service.CirculationRequestsRetriever;

import java.time.Instant;
import java.util.ArrayList;
import java.util.List;
import java.util.Map;
import java.util.function.Function;

import static org.folio.orders.utils.HelperUtils.extractCreatedDate;
import static org.folio.orders.utils.HelperUtils.extractId;
import static org.folio.service.inventory.InventoryUtils.INVENTORY_LOOKUP_ENDPOINTS;
import static org.folio.service.inventory.InventoryUtils.REQUESTS;
import static org.folio.service.inventory.util.RequestFields.DESTINATION_ITEM_ID;
import static org.folio.service.inventory.util.RequestFields.STATUS;

public class InventoryItemRequestService {

  private static final Logger logger = LogManager.getLogger(InventoryItemRequestService.class);

  private static final String REQUEST_ENDPOINT = INVENTORY_LOOKUP_ENDPOINTS.get(REQUESTS) + "/%s";
  private static final String REQUEST_MOVE_ENDPOINT = REQUEST_ENDPOINT + "/move";
  private static final String REQUEST_CANCEL_STATUS = "Closed - Cancelled";

  private final CirculationRequestsRetriever circulationRequestsRetriever;
  private final RestClient restClient;

  public InventoryItemRequestService(RestClient restClient, CirculationRequestsRetriever circulationRequestsRetriever) {
    this.restClient = restClient;
    this.circulationRequestsRetriever = circulationRequestsRetriever;
  }

  public Future<List<String>> getItemIdsWithActiveRequests(List<String> itemIds, RequestContext requestContext) {
    if (logger.isDebugEnabled()) {
      logger.debug("getItemsWithActiveRequests:: Filtering itemIds with active requests: {}", itemIds);
    }
    return circulationRequestsRetriever.getNumbersOfRequestsByItemIds(itemIds, requestContext)
      .map(map -> map.entrySet().stream()
        .filter(e -> e.getValue() > 0)
        .map(Map.Entry::getKey)
        .toList());
  }

  public Future<Void> transferItemRequests(List<String> originItemIds, String destinationItemId, RequestContext requestContext) {
    return circulationRequestsRetriever.getRequesterIdsToRequestsByItemIds(originItemIds, requestContext)
      .compose(requesterToRequestsMap -> {
        var requestsToCancel = new ArrayList<JsonObject>();
        var requestsToTransfer = new ArrayList<JsonObject>();
        for (var requests : requesterToRequestsMap.values()) {
          requestsToCancel.addAll(requests);
          requests.stream().min(this::compareRequests).ifPresent(request -> {
            requestsToTransfer.add(request);
            requestsToCancel.remove(request);
          });
        }

        return handleItemRequests(requestsToTransfer, request -> transferRequest(request, destinationItemId, requestContext))
          .compose(v -> handleItemRequests(requestsToCancel, request -> cancelRequest(request, requestContext)));
      });
  }

  private Future<Void> handleItemRequests(List<JsonObject> requests, Function<JsonObject, Future<Void>> handler) {
    return GenericCompositeFuture.all(
      requests.stream()
        .map(handler)
        .toList())
      .mapEmpty();
  }

  private Future<Void> cancelRequest(JsonObject request, RequestContext requestContext) {
    String reqId = extractId(request);
    request.put(STATUS.getValue(), REQUEST_CANCEL_STATUS);
    RequestEntry requestEntry = new RequestEntry(String.format(REQUEST_ENDPOINT, reqId));
    logger.info("cancelRequest:: Cancelling Request with id='{}'", reqId);
    return restClient.put(requestEntry, request, requestContext);
  }

  private Future<Void> transferRequest(JsonObject request, String itemId, RequestContext requestContext) {
    String reqId = extractId(request);
    JsonObject jsonObject =  JsonObject.of(DESTINATION_ITEM_ID.getValue(), itemId);
    RequestEntry requestEntry = new RequestEntry(String.format(REQUEST_MOVE_ENDPOINT, reqId));
    logger.info("transferRequest:: Moving Request with id='{}' to item with id='{}'", reqId, itemId);
    return restClient.postJsonObject(requestEntry, jsonObject, requestContext)
      .mapEmpty();
  }

  private int compareRequests(JsonObject r1, JsonObject r2) {
    Instant createdDate1 = Instant.parse(extractCreatedDate(r1));
    Instant createdDate2 = Instant.parse(extractCreatedDate(r2));
    return createdDate1.compareTo(createdDate2);
  }

}
