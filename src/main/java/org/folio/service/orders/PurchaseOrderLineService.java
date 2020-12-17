package org.folio.service.orders;

import java.util.Collection;
import java.util.List;
import java.util.concurrent.CompletableFuture;

import org.folio.rest.acq.model.finance.Fund;
import org.folio.rest.acq.model.finance.FundCollection;
import org.folio.rest.core.RestClient;
import org.folio.rest.core.models.RequestContext;
import org.folio.rest.jaxrs.model.PoLine;
import org.folio.rest.jaxrs.model.PoLineCollection;
import static org.folio.rest.RestConstants.MAX_IDS_FOR_GET_RQ;

public class PurchaseOrderLineService {
  private final RestClient orderLinesClient;

  public PurchaseOrderLineService(RestClient orderLinesClient) {
    this.orderLinesClient = orderLinesClient;
  }

  public CompletableFuture<List<PoLine>> getOrderLines(String query, int offset, int limit, RequestContext requestContext) {
      return orderLinesClient.get(query, offset, limit, requestContext, PoLineCollection.class)
                  .thenApply(PoLineCollection::getPoLines);
  }
}
