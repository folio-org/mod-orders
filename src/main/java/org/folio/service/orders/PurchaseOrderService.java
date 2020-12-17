package org.folio.service.orders;

import java.util.concurrent.CompletableFuture;

import org.folio.rest.core.RestClient;
import org.folio.rest.core.models.RequestContext;
import org.folio.rest.jaxrs.model.PurchaseOrderCollection;

public class PurchaseOrderService {
  private final RestClient purchaseOrderRestClient;

  public PurchaseOrderService(RestClient purchaseOrderRestClient) {
    this.purchaseOrderRestClient = purchaseOrderRestClient;
  }

  public CompletableFuture<PurchaseOrderCollection> getPurchaseOrdersByFunds(String query, int limit, int offset, RequestContext requestContext) {
    return purchaseOrderRestClient.get(query, offset, limit, requestContext, PurchaseOrderCollection.class);
  }

}
