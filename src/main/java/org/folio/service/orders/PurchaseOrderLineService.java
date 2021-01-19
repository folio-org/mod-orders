package org.folio.service.orders;

import java.util.List;
import java.util.concurrent.CompletableFuture;

import org.folio.rest.core.RestClient;
import org.folio.rest.core.models.RequestContext;
import org.folio.rest.core.models.RequestEntry;
import org.folio.rest.jaxrs.model.PoLine;
import org.folio.rest.jaxrs.model.PoLineCollection;

public class PurchaseOrderLineService {

  private static final String ENDPOINT = "/orders-storage/po-lines";
  private static final String BY_ID_ENDPOINT = "/orders-storage/po-lines/{id}";

  private final RestClient restClient;

  public PurchaseOrderLineService(RestClient restClient) {
    this.restClient = restClient;
  }

  public CompletableFuture<List<PoLine>> getOrderLines(String query, int offset, int limit, RequestContext requestContext) {
    RequestEntry requestEntry = new RequestEntry(ENDPOINT).withQuery(query).withOffset(offset).withLimit(limit);
    return restClient.get(requestEntry, requestContext, PoLineCollection.class)
      .thenApply(PoLineCollection::getPoLines);
  }

  public CompletableFuture<Void> updateOrderLine(PoLine poLine, RequestContext requestContext) {
    RequestEntry requestEntry = new RequestEntry(BY_ID_ENDPOINT).withId(poLine.getId());
    return restClient.put(requestEntry, poLine, requestContext);
  }

  public CompletableFuture<Void> updateOrderLines(List<PoLine> orderLines, RequestContext requestContext) {
    return CompletableFuture.allOf(orderLines.stream()
      .map(poLine -> updateOrderLine(poLine, requestContext))
      .toArray(CompletableFuture[]::new));
  }
}
