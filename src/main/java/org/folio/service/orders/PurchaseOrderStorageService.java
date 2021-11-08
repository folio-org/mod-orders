package org.folio.service.orders;

import static java.util.stream.Collectors.toList;
import static one.util.streamex.StreamEx.ofSubLists;
import static org.folio.orders.utils.HelperUtils.collectResultsOnSuccess;
import static org.folio.orders.utils.HelperUtils.convertIdsToCqlQuery;
import static org.folio.orders.utils.HelperUtils.convertToCompositePurchaseOrder;
import static org.folio.rest.RestConstants.MAX_IDS_FOR_GET_RQ;

import java.util.Collection;
import java.util.List;
import java.util.concurrent.CompletableFuture;

import org.folio.rest.core.PostResponseType;
import org.folio.rest.core.RestClient;
import org.folio.rest.core.models.RequestContext;
import org.folio.rest.core.models.RequestEntry;
import org.folio.rest.jaxrs.model.CompositePurchaseOrder;
import org.folio.rest.jaxrs.model.PurchaseOrder;
import org.folio.rest.jaxrs.model.PurchaseOrderCollection;

import io.vertx.core.json.JsonObject;

public class PurchaseOrderStorageService {

  private static final String ENDPOINT = "/orders-storage/purchase-orders";
  private static final String BY_ID_ENDPOINT = ENDPOINT + "/{id}";

  private final RestClient restClient;

  private final PurchaseOrderLineService purchaseOrderLineService;

  public PurchaseOrderStorageService(RestClient restClient, PurchaseOrderLineService purchaseOrderLineService) {
    this.restClient = restClient;
    this.purchaseOrderLineService = purchaseOrderLineService;
  }

  public CompletableFuture<PurchaseOrder> getPurchaseOrderById(String id, RequestContext requestContext) {
    RequestEntry requestEntry = new RequestEntry(BY_ID_ENDPOINT).withId(id);
    return restClient.get(requestEntry, requestContext, PurchaseOrder.class);
  }

  public CompletableFuture<JsonObject> getPurchaseOrderByIdAsJson(String id, RequestContext requestContext) {
    RequestEntry requestEntry = new RequestEntry(BY_ID_ENDPOINT).withId(id);
    return restClient.getAsJsonObject(requestEntry, requestContext);
  }

  public CompletableFuture<CompositePurchaseOrder> getCompositeOrderById(String orderId, RequestContext requestContext) {
    return getPurchaseOrderById(orderId, requestContext)
      .thenCompose(purchaseOrder -> purchaseOrderLineService.getCompositePoLinesByOrderId( orderId, requestContext)
        .thenApply(poLines -> convertToCompositePurchaseOrder(JsonObject.mapFrom(purchaseOrder)).withCompositePoLines(poLines)));
  }

  public CompletableFuture<PurchaseOrderCollection> getPurchaseOrders(String query, int limit, int offset, RequestContext requestContext) {
    RequestEntry requestEntry = new RequestEntry(ENDPOINT)
            .withQuery(query)
            .withLimit(limit)
            .withOffset(offset);
    return restClient.get(requestEntry, requestContext, PurchaseOrderCollection.class);
  }
  public CompletableFuture<List<PurchaseOrder>> getPurchaseOrdersByIds(List<String> orderIds, RequestContext requestContext) {

    return collectResultsOnSuccess(ofSubLists(orderIds, MAX_IDS_FOR_GET_RQ)
      .map(ids -> getOrdersChunk(ids, requestContext)).toList())
      .thenApply(lists -> lists.stream()
        .flatMap(Collection::stream)
        .collect(toList()));
  }

  public CompletableFuture<CompositePurchaseOrder> getCompositeOrderByPoLineId(String poLineId, RequestContext requestContext) {
    return purchaseOrderLineService.getOrderLineById(poLineId, requestContext)
      .thenCompose(poLine -> getCompositeOrderById(poLine.getPurchaseOrderId(), requestContext));
  }


  public CompletableFuture<JsonObject> getPurchaseOrderByPONumber(String poNumber, RequestContext requestContext) {
    String query = String.format("poNumber==%s", poNumber);
    RequestEntry requestEntry = new RequestEntry(ENDPOINT).withQuery(query).withOffset(0).withLimit(1);
    return restClient.getAsJsonObject(requestEntry, requestContext);
  }


  private CompletableFuture<List<PurchaseOrder>> getOrdersChunk(List<String> orderIds, RequestContext requestContext) {

    String query = convertIdsToCqlQuery(orderIds);
    RequestEntry requestEntry = new RequestEntry(ENDPOINT)
      .withQuery(query)
      .withOffset(0)
      .withLimit(MAX_IDS_FOR_GET_RQ);
    return restClient.get(requestEntry, requestContext, PurchaseOrderCollection.class)
      .thenApply(PurchaseOrderCollection::getPurchaseOrders);
  }

  public CompletableFuture<Void> deleteOrderById(String orderId, RequestContext requestContext) {
    RequestEntry requestEntry = new RequestEntry(BY_ID_ENDPOINT).withId(orderId);
    return restClient.delete(requestEntry, requestContext);
  }

  public CompletableFuture<PurchaseOrder> createPurchaseOrder(JsonObject jsonOrder, RequestContext requestContext) {
    RequestEntry requestEntry = new RequestEntry(ENDPOINT);
    return restClient.post(requestEntry, jsonOrder, PostResponseType.BODY, PurchaseOrder.class, requestContext);
  }
  public CompletableFuture<Void> saveOrder(PurchaseOrder purchaseOrder, RequestContext requestContext) {
    RequestEntry requestEntry = new RequestEntry(BY_ID_ENDPOINT).withId(purchaseOrder.getId());
    return restClient.put(requestEntry, purchaseOrder, requestContext);
  }
}
