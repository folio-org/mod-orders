package org.folio.service;

import static org.folio.service.inventory.InventoryUtils.INVENTORY_LOOKUP_ENDPOINTS;
import static org.folio.service.inventory.InventoryUtils.REQUESTS;

import java.util.ArrayList;
import java.util.List;

import org.folio.rest.core.RestClient;
import org.folio.rest.core.models.RequestContext;
import org.folio.rest.core.models.RequestEntry;

import io.vertx.core.Future;

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
    String query = String.format("(itemId==%s and status=\"*\")", itemId);
    RequestEntry requestEntry = new RequestEntry(INVENTORY_LOOKUP_ENDPOINTS.get(REQUESTS))
      .withQuery(query).withOffset(0).withLimit(0);
    return restClient.getAsJsonObject(requestEntry, requestContext)
      .map(json -> json.getInteger(REQUESTS_TOTAL));
  }

  /**
   * Returns list of requestIds for specified item.
   *
   * @param itemId id of Item
   * @return future with list of requestIds
   */
  public Future<List<String>> getRequestIdsByItemId(String itemId, RequestContext requestContext) {
    String query = String.format("(itemId==%s and status=\"*\")", itemId);
    RequestEntry requestEntry = new RequestEntry(INVENTORY_LOOKUP_ENDPOINTS.get(REQUESTS))
      .withQuery(query).withOffset(0).withLimit(0);
    return restClient.getAsJsonObject(requestEntry, requestContext)
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

}
