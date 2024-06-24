package org.folio.service;

import io.vertx.core.Future;
import io.vertx.core.json.JsonObject;
import one.util.streamex.StreamEx;
import org.apache.commons.lang3.StringUtils;
import org.folio.okapi.common.GenericCompositeFuture;
import org.folio.rest.core.RestClient;
import org.folio.rest.core.models.RequestContext;
import org.folio.rest.core.models.RequestEntry;
import org.folio.rest.jaxrs.model.CirculationRequest;
import org.folio.rest.jaxrs.model.Piece;
import org.folio.rest.jaxrs.model.RequestsCollection;
import org.folio.service.pieces.PieceStorageService;

import java.util.List;
import java.util.Map;
import java.util.function.Function;
import java.util.stream.Collectors;
import java.util.stream.IntStream;

import static java.util.stream.Collectors.mapping;
import static java.util.stream.Collectors.toList;
import static org.folio.orders.utils.HelperUtils.convertIdsToCqlQuery;
import static org.folio.orders.utils.RequestContextUtil.createContextWithNewTenantId;
import static org.folio.rest.RestConstants.MAX_IDS_FOR_GET_RQ_15;
import static org.folio.service.inventory.InventoryUtils.INVENTORY_LOOKUP_ENDPOINTS;
import static org.folio.service.inventory.InventoryUtils.REQUESTS;
import static org.folio.service.inventory.util.RequestFields.COLLECTION_RECORDS;
import static org.folio.service.inventory.util.RequestFields.COLLECTION_TOTAL;
import static org.folio.service.inventory.util.RequestFields.REQUESTER_ID;
import static org.folio.service.inventory.util.RequestFields.ITEM_ID;

public class CirculationRequestsRetriever {

  private final static String OPEN_REQUEST_STATUS = "Open - *";

  private final PieceStorageService pieceStorageService;

  private final RestClient restClient;

  public CirculationRequestsRetriever(PieceStorageService pieceStorageService, RestClient restClient) {
    this.pieceStorageService = pieceStorageService;
    this.restClient = restClient;
  }

  public Future<Integer> getNumberOfRequestsByItemId(String itemId, RequestContext requestContext) {
    String query = String.format("(itemId==%s and status=\"%s*\")", itemId, OPEN_REQUEST_STATUS);
    RequestEntry requestEntry = new RequestEntry(INVENTORY_LOOKUP_ENDPOINTS.get(REQUESTS))
      .withQuery(query).withOffset(0).withLimit(0); // limit = 0 means payload will include only totalRecords value
    return restClient.getAsJsonObject(requestEntry, requestContext)
      .map(json -> json.getInteger(COLLECTION_TOTAL.getValue()));
  }

  public Future<Map<String, Long>> getNumbersOfRequestsByItemIds(List<String> itemIds, RequestContext requestContext) {
    return getRequestsByItemIds(itemIds, OPEN_REQUEST_STATUS, requestContext)
      .map(jsonList -> jsonList.stream()
        .collect(Collectors.groupingBy(json -> json.getString(ITEM_ID.getValue()), Collectors.counting()))
      );
  }

  public Future<Map<String, List<JsonObject>>> getRequesterIdsToRequestsByItemIds(List<String> itemIds, RequestContext requestContext) {
    return getRequestsByItemIds(itemIds, OPEN_REQUEST_STATUS, requestContext)
      .map(jsonList -> jsonList.stream()
        .collect(Collectors.groupingBy(json -> json.getString(REQUESTER_ID.getValue()))));
  }

  private Future<List<JsonObject>> getRequestsByItemIds(List<String> itemIds, String status, RequestContext requestContext) {
    var futures = StreamEx.ofSubLists(itemIds, MAX_IDS_FOR_GET_RQ_15)
      .map(ids -> String.format("(%s and status=\"%s\")", convertIdsToCqlQuery(ids, ITEM_ID.getValue()), status))
      .map(query -> new RequestEntry(INVENTORY_LOOKUP_ENDPOINTS.get(REQUESTS))
        .withQuery(query).withOffset(0).withLimit(Integer.MAX_VALUE))
      .map(entry -> restClient.getAsJsonObject(entry, requestContext))
      .toList();

    return GenericCompositeFuture.all(futures)
      .map(f -> f.<JsonObject>list().stream()
        .flatMap(json -> {
          var totalRecords = json.getInteger(COLLECTION_TOTAL.getValue());
          var requests = json.getJsonArray(COLLECTION_RECORDS.getValue());
          return IntStream.range(0, totalRecords)
            .mapToObj(requests::getJsonObject);
        })
        .toList());
  }

  public Future<RequestsCollection> getRequesterIdsToRequestsByPieceIds(List<String> pieceIds, String status, RequestContext requestContext) {
    // 1. Fetch all pieces
    return pieceStorageService.getPiecesByIds(pieceIds, requestContext)
      // 2. Convert list of pieces to a map where we map tenants to items that are in this tenant
      .map(pieces -> getLocationContextToItems(pieces, requestContext))
      // 3. For each tenant fetch its items -> resulting in a list of Future<List<JsonObject>>
      .map(ctxToItemsMap -> StreamEx.of(ctxToItemsMap.entrySet())
        .map(ctxToItems -> getRequestsByItemIds(ctxToItems.getValue(), status, ctxToItems.getKey())).toList())
      // 4. Put the list of futures in a composite one and flatMap all futures into a single list
      .compose(requests -> GenericCompositeFuture.all(requests)
        .map(cf -> StreamEx.of(cf.<List<JsonObject>>list()).toFlatList(Function.identity())))
      // 5. Prepare response body
      .map(this::prepareRequestsCollection);
  }

  private Map<RequestContext, List<String>> getLocationContextToItems(List<Piece> pieces, RequestContext requestContext) {
    return StreamEx.of(pieces)
      .filter(piece -> StringUtils.isNotEmpty(piece.getItemId()))
      .groupingBy(piece -> createContextWithNewTenantId(requestContext, piece.getReceivingTenantId()),
        mapping(Piece::getItemId, toList()));
  }

  private RequestsCollection prepareRequestsCollection(List<JsonObject> requests) {
    return new RequestsCollection()
      .withTotalRecords(requests.size())
      .withCirculationRequests(requests.stream()
        .map(requestJson -> {
          var request = new CirculationRequest();
          requestJson.getMap().forEach(request::setAdditionalProperty);
          return request;
        }).toList()
      );
  }

}
