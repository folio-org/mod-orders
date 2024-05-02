package org.folio.service;

import static org.folio.orders.utils.HelperUtils.ITEM_ID;
import static org.folio.orders.utils.HelperUtils.convertIdsToCqlQuery;
import static org.folio.rest.RestConstants.MAX_IDS_FOR_GET_RQ_15;
import static org.folio.service.inventory.InventoryUtils.INVENTORY_LOOKUP_ENDPOINTS;
import static org.folio.service.inventory.InventoryUtils.REQUESTS;

import java.util.ArrayList;
import java.util.Collection;
import java.util.List;
import java.util.stream.IntStream;

import org.folio.okapi.common.GenericCompositeFuture;
import org.folio.rest.core.RestClient;
import org.folio.rest.core.models.RequestContext;
import org.folio.rest.core.models.RequestEntry;

import io.vertx.core.Future;
import io.vertx.core.json.JsonObject;
import one.util.streamex.StreamEx;

public class CirculationRequestsRetriever {

  private static final String REQUESTS_RECORDS = "requests";
  private static final String REQUESTS_TOTAL = "totalRecords";
  private static final String REQUEST_ID = "id";

  private final RestClient restClient;

  public CirculationRequestsRetriever(RestClient restClient) {
    this.restClient = restClient;
  }

  /**
   * Returns number of requests for specified item.
   *
   * @param itemId id of Item
   * @return future with number of requests
   */
  public Future<Integer> getNumberOfRequestsByItemId(String itemId, RequestContext requestContext) {
    return getRequestById(itemId, 0, requestContext)
      .map(json -> json.getInteger(REQUESTS_TOTAL));
  }

  /**
   * Returns list of requestIds for specified item.
   *
   * @param itemId id of Item
   * @return future with list of requestIds
   */
  public Future<List<String>> getRequestIdsByItemId(String itemId, RequestContext requestContext) {
    return getRequestById(itemId, 0, requestContext)
      .map(json -> {
        List<String> requestIds = new ArrayList<>();
        var totalRecords = json.getInteger(REQUESTS_TOTAL);
        var requests = json.getJsonArray(REQUESTS_RECORDS);
        for (int i = 0; i < totalRecords; i++) {
          var jsonObj = requests.getJsonObject(i);
          requestIds.add(jsonObj.getString(REQUEST_ID));
        }
        return requestIds;
      });
  }

  private Future<JsonObject> getRequestById(String itemId, int limit, RequestContext requestContext) {
    String query = String.format("(itemId==%s and status=\"*\")", itemId);
    RequestEntry requestEntry = new RequestEntry(INVENTORY_LOOKUP_ENDPOINTS.get(REQUESTS))
      .withQuery(query).withOffset(0).withLimit(limit);
    return restClient.getAsJsonObject(requestEntry, requestContext);
  }

  /**
   * Returns list of requestIds for specified item.
   *
   * @param itemIds ids of Items
   * @return future with number of requests
   */
  public Future<Integer> getNumberOfRequestsByItemIds(List<String> itemIds, RequestContext requestContext) {
    return getRequestsByIds(itemIds, Integer.MAX_VALUE, requestContext)
      .map(jsonList -> jsonList.stream()
        .mapToInt(json -> json.getInteger(REQUESTS_TOTAL))
        .sum()
      );
  }

  /**
   * Returns list of requestIds for specified item.
   *
   * @param itemIds ids of Items
   * @return future with list of requestIds
   */
  public Future<List<String>> getRequestIdsByItemIds(List<String> itemIds, RequestContext requestContext) {
    return getRequestsByIds(itemIds, Integer.MAX_VALUE, requestContext)
      .map(jsonList -> jsonList.stream()
        .flatMap(json -> {
          var totalRecords = json.getInteger(REQUESTS_TOTAL);
          var requests = json.getJsonArray(REQUESTS_RECORDS);
          return IntStream.range(0, totalRecords)
            .mapToObj(i -> requests.getJsonObject(i).getString(REQUEST_ID));
        })
        .toList()
      );
  }

  private Future<List<JsonObject>> getRequestsByIds(List<String> itemIds, int limit, RequestContext requestContext) {
    var futures = StreamEx.ofSubLists(itemIds, MAX_IDS_FOR_GET_RQ_15)
      .map(ids -> String.format("(%s and status=\"*\")", convertIdsToCqlQuery(ids, ITEM_ID)))
      .map(query -> new RequestEntry(INVENTORY_LOOKUP_ENDPOINTS.get(REQUESTS))
        .withQuery(query).withOffset(0).withLimit(limit))
      .map(entry -> restClient.getAsJsonObject(entry, requestContext))
      .toList();

    return GenericCompositeFuture.all(futures)
      .map(f -> f.<List<JsonObject>>list().stream()
        .flatMap(Collection::stream)
        .toList());
  }

}
