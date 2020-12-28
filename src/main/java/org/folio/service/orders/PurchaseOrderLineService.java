package org.folio.service.orders;

import java.util.List;
import java.util.concurrent.CompletableFuture;

import org.folio.rest.core.RestClient;
import org.folio.rest.core.models.RequestContext;
import org.folio.rest.jaxrs.model.PoLine;
import org.folio.rest.jaxrs.model.PoLineCollection;

public class PurchaseOrderLineService {
  private final RestClient orderLinesClient;

  public PurchaseOrderLineService(RestClient orderLinesClient) {
    this.orderLinesClient = orderLinesClient;
  }

  public CompletableFuture<List<PoLine>> getOrderLines(String query, int offset, int limit, RequestContext requestContext) {
      return orderLinesClient.get(query, offset, limit, requestContext, PoLineCollection.class)
                  .thenApply(PoLineCollection::getPoLines);
  }

  public CompletableFuture<Void> updateOrderLine(PoLine poLine, RequestContext requestContext) {
    return orderLinesClient.put(poLine.getId(), poLine, requestContext);
  }


  public CompletableFuture<Void> updateOrderLines(List<PoLine> orderLines, RequestContext requestContext) {
    return CompletableFuture.allOf(orderLines.stream()
                            .map(poLine -> updateOrderLine(poLine, requestContext))
                            .toArray(CompletableFuture[]::new));
  }
}
