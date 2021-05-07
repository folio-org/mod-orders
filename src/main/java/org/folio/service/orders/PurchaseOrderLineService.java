package org.folio.service.orders;

import static java.util.stream.Collectors.toList;
import static org.folio.orders.utils.HelperUtils.collectResultsOnSuccess;

import java.util.List;
import java.util.concurrent.CompletableFuture;

import org.apache.logging.log4j.LogManager;
import org.apache.logging.log4j.Logger;
import org.folio.completablefuture.FolioVertxCompletableFuture;
import org.folio.orders.rest.exceptions.HttpException;
import org.folio.orders.utils.ErrorCodes;
import org.folio.orders.utils.HelperUtils;
import org.folio.rest.core.RestClient;
import org.folio.rest.core.models.RequestContext;
import org.folio.rest.core.models.RequestEntry;
import org.folio.rest.jaxrs.model.CompositePoLine;
import org.folio.rest.jaxrs.model.PoLine;
import org.folio.rest.jaxrs.model.PoLineCollection;

import io.vertx.core.http.HttpMethod;
import io.vertx.core.json.JsonObject;

public class PurchaseOrderLineService {
  private static final Logger logger = LogManager.getLogger(PurchaseOrderLineService.class);
  private static final String ENDPOINT = "/orders-storage/po-lines";
  private static final String BY_ID_ENDPOINT = ENDPOINT + "/{id}";

  private final RestClient restClient;

  public PurchaseOrderLineService(RestClient restClient) {
    this.restClient = restClient;
  }

  public CompletableFuture<List<PoLine>> getOrderLines(String query, int offset, int limit, RequestContext requestContext) {
    RequestEntry requestEntry = new RequestEntry(ENDPOINT).withQuery(query).withOffset(offset).withLimit(limit);
    return restClient.get(requestEntry, requestContext, PoLineCollection.class)
                     .thenApply(PoLineCollection::getPoLines);
  }

  public CompletableFuture<PoLine> getOrderLineById(String orderLineId, RequestContext requestContext) {
    RequestEntry requestEntry = new RequestEntry(BY_ID_ENDPOINT).withId(orderLineId);
    return restClient.get(requestEntry, requestContext, PoLine.class);
  }

  public CompletableFuture<Void> updateOrderLine(PoLine poLine, RequestContext requestContext) {
    RequestEntry requestEntry = new RequestEntry(BY_ID_ENDPOINT).withId(poLine.getId());
    return restClient.put(requestEntry, poLine, requestContext);
  }

  public CompletableFuture<Void> updateOrderLines(List<PoLine> orderLines, RequestContext requestContext) {
    return FolioVertxCompletableFuture.allOf(requestContext.getContext(), orderLines.stream()
      .map(poLine -> updateOrderLine(poLine, requestContext)
                    .exceptionally(t -> {
                      throw new HttpException(400, ErrorCodes.POL_LINES_LIMIT_EXCEEDED.toError());
                    }))
      .toArray(CompletableFuture[]::new));
  }


  public CompletableFuture<List<CompositePoLine>> getCompositePoLinesByOrderId(String orderId, RequestContext requestContext) {
    CompletableFuture<List<CompositePoLine>> future = new CompletableFuture<>();

    getOrderLines("purchaseOrderId==" + orderId, 0, Integer.MAX_VALUE, requestContext)
      .thenAccept(jsonLines ->
        collectResultsOnSuccess(jsonLines.stream().map(line -> operateOnPoLine(HttpMethod.GET, line, requestContext)).collect(toList()))
          .thenAccept(future::complete)
          .exceptionally(t -> {
            future.completeExceptionally(t.getCause());
            return null;
          })
      )
      .exceptionally(t -> {
        logger.error("Exception gathering poLine data:", t);
        future.completeExceptionally(t);
        return null;
      });
    return future;
  }

  public CompletableFuture<CompositePoLine> operateOnPoLine(HttpMethod operation, PoLine line, RequestContext requestContext) {
    return HelperUtils.operateOnPoLine(operation, JsonObject.mapFrom(line),
        restClient.getHttpClient(requestContext.getHeaders()), requestContext.getHeaders(), logger);
  }
}

