package org.folio.service;

import static org.folio.orders.utils.HelperUtils.convertIdsToCqlQuery;
import static org.folio.rest.RestConstants.MAX_IDS_FOR_GET_RQ_15;
import static org.folio.service.inventory.InventoryUtils.INVENTORY_LOOKUP_ENDPOINTS;
import static org.folio.service.inventory.InventoryUtils.REQUESTS;
import static org.folio.service.inventory.util.RequestFields.COLLECTION_RECORDS;
import static org.folio.service.inventory.util.RequestFields.COLLECTION_TOTAL;
import static org.folio.service.inventory.util.RequestFields.ID_KEY;
import static org.folio.service.inventory.util.RequestFields.ITEM_ID_KEY;

import java.util.Collection;
import java.util.List;
import java.util.Map;
import java.util.stream.Collectors;
import java.util.stream.IntStream;

import org.folio.okapi.common.GenericCompositeFuture;
import org.folio.rest.core.RestClient;
import org.folio.rest.core.models.RequestContext;
import org.folio.rest.core.models.RequestEntry;

import io.vertx.core.Future;
import io.vertx.core.json.JsonObject;
import one.util.streamex.StreamEx;

public class CirculationRequestsRetriever {

  private final RestClient restClient;

  public CirculationRequestsRetriever(RestClient restClient) {
    this.restClient = restClient;
  }

  public Future<Integer> getNumberOfRequestsByItemId(String itemId, RequestContext requestContext) {
    String query = String.format("(itemId==%s and status=\"*\")", itemId);
    RequestEntry requestEntry = new RequestEntry(INVENTORY_LOOKUP_ENDPOINTS.get(REQUESTS))
      .withQuery(query).withOffset(0).withLimit(0); // limit = 0 means payload will include only totalRecords value
    return restClient.getAsJsonObject(requestEntry, requestContext)
      .map(json -> json.getInteger(COLLECTION_TOTAL.getValue()));
  }

  public Future<Map<String, Long>> getNumbersOfRequestsByItemIds(List<String> itemIds, RequestContext requestContext) {
    return getRequestsByIds(itemIds, requestContext)
      .map(jsonList -> jsonList.stream()
        .collect(Collectors.groupingBy(json -> json.getString(ITEM_ID_KEY.getValue()), Collectors.counting()))
      );
  }

  public Future<List<String>> getRequestIdsByItemIds(List<String> itemIds, RequestContext requestContext) {
    return getRequestsByIds(itemIds, requestContext)
      .map(jsonList -> jsonList.stream()
        .map(json -> json.getString(ID_KEY.getValue()))
        .toList()
      );
  }

  private Future<List<JsonObject>> getRequestsByIds(List<String> itemIds, RequestContext requestContext) {
    var futures = StreamEx.ofSubLists(itemIds, MAX_IDS_FOR_GET_RQ_15)
      .map(ids -> String.format("(%s and status=\"*\")", convertIdsToCqlQuery(ids, ITEM_ID_KEY.getValue())))
      .map(query -> new RequestEntry(INVENTORY_LOOKUP_ENDPOINTS.get(REQUESTS))
        .withQuery(query).withOffset(0).withLimit(Integer.MAX_VALUE))
      .map(entry -> restClient.getAsJsonObject(entry, requestContext))
      .toList();

    // Mapping logic:
    // Lists<List<Collection of Requests>> ->
    // List<Collection of Requests> ->
    // List<Requests>
    return GenericCompositeFuture.all(futures)
      .map(f -> f.<List<JsonObject>>list().stream()
        .flatMap(Collection::stream)
        .flatMap(json -> {
          var totalRecords = json.getInteger(COLLECTION_TOTAL.getValue());
          var requests = json.getJsonArray(COLLECTION_RECORDS.getValue());
          return IntStream.range(0, totalRecords)
            .mapToObj(requests::getJsonObject);
        })
        .toList());
  }

}
