package org.folio.service.orders;

import java.util.concurrent.CompletableFuture;

import org.folio.rest.core.RestClient;
import org.folio.rest.core.models.RequestContext;
import org.folio.rest.core.models.RequestEntry;
import org.folio.rest.jaxrs.model.PurchaseOrder;
import org.folio.rest.jaxrs.model.PurchaseOrderCollection;

import static org.folio.orders.utils.ResourcePathResolver.resourcesPath;

public class PurchaseOrderService {

  private static final String ENDPOINT = "/orders-storage/purchase-orders";
  private static final String BY_ID_ENDPOINT = ENDPOINT + "/{id}";
  private final RestClient restClient;

  public PurchaseOrderService(RestClient restClient) {
    this.restClient = restClient;
  }

  public CompletableFuture<PurchaseOrder> getPurchaseOrderById(String id, RequestContext requestContext) {
    RequestEntry requestEntry = new RequestEntry(BY_ID_ENDPOINT).withId(id);
    return restClient.get(requestEntry, requestContext, PurchaseOrder.class);
  }

  public CompletableFuture<PurchaseOrderCollection> getPurchaseOrders(String query, int limit, int offset, RequestContext requestContext) {
    RequestEntry requestEntry = new RequestEntry(ENDPOINT)
            .withQuery(query)
            .withLimit(limit)
            .withOffset(offset);
    return restClient.get(requestEntry, requestContext, PurchaseOrderCollection.class);
  }
}
