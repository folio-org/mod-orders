package org.folio.service.orders;

import static java.util.concurrent.CompletableFuture.completedFuture;
import static java.util.stream.Collectors.toList;
import static one.util.streamex.StreamEx.ofSubLists;
import static org.folio.orders.utils.HelperUtils.collectResultsOnSuccess;
import static org.folio.orders.utils.HelperUtils.convertIdsToCqlQuery;
import static org.folio.rest.RestConstants.MAX_IDS_FOR_GET_RQ;

import java.util.ArrayList;
import java.util.Collection;
import java.util.List;
import java.util.concurrent.CompletableFuture;
import java.util.concurrent.CompletionException;

import org.apache.commons.collections4.CollectionUtils;
import org.apache.logging.log4j.LogManager;
import org.apache.logging.log4j.Logger;
import org.folio.completablefuture.FolioVertxCompletableFuture;
import org.folio.orders.utils.HelperUtils;
import org.folio.orders.utils.PoLineCommonUtil;
import org.folio.rest.core.RestClient;
import org.folio.rest.core.exceptions.ErrorCodes;
import org.folio.rest.core.exceptions.HttpException;
import org.folio.rest.core.models.RequestContext;
import org.folio.rest.core.models.RequestEntry;
import org.folio.rest.jaxrs.model.CompositePoLine;
import org.folio.rest.jaxrs.model.CompositePurchaseOrder;
import org.folio.rest.jaxrs.model.Parameter;
import org.folio.rest.jaxrs.model.PoLine;
import org.folio.rest.jaxrs.model.PoLineCollection;

import io.vertx.core.http.HttpMethod;
import io.vertx.core.json.JsonObject;

public class PurchaseOrderLineService {
  private static final Logger logger = LogManager.getLogger(PurchaseOrderLineService.class);
  private static final String ENDPOINT = "/orders-storage/po-lines";
  private static final String BY_ID_ENDPOINT = ENDPOINT + "/{id}";
  private static final String ORDER_LINES_BY_ORDER_ID_QUERY = "purchaseOrderId == %s";

  private final RestClient restClient;

  public PurchaseOrderLineService(RestClient restClient) {
    this.restClient = restClient;
  }

  public CompletableFuture<PoLineCollection> getOrderLineCollection(String query, int offset, int limit, RequestContext requestContext) {
    RequestEntry requestEntry = new RequestEntry(ENDPOINT).withQuery(query).withOffset(offset).withLimit(limit);
    return restClient.get(requestEntry, requestContext, PoLineCollection.class);
  }


  public CompletableFuture<List<PoLine>> getOrderLines(String query, int offset, int limit, RequestContext requestContext) {
    return getOrderLineCollection(query, offset, limit, requestContext).thenApply(PoLineCollection::getPoLines);
  }

  public CompletableFuture<PoLine> getOrderLineById(String lineId, RequestContext requestContext) {
    RequestEntry requestEntry = new RequestEntry(BY_ID_ENDPOINT).withId(lineId);
    return restClient.get(requestEntry, requestContext, PoLine.class);
  }

  public CompletableFuture<List<PoLine>> getOrderLinesByIds(List<String> orderLineIds, RequestContext requestContext) {

    return collectResultsOnSuccess(ofSubLists(orderLineIds, MAX_IDS_FOR_GET_RQ)
      .map(ids -> getOrderLinesChunk(ids, requestContext)).toList())
      .thenApply(lists -> lists.stream()
        .flatMap(Collection::stream)
        .collect(toList()));
  }

  public CompletableFuture<Void> saveOrderLine(PoLine poLine, RequestContext requestContext) {
    RequestEntry requestEntry = new RequestEntry(BY_ID_ENDPOINT).withId(poLine.getId());
    return restClient.put(requestEntry, poLine, requestContext);
  }

  public CompletableFuture<Void> saveOrderLine(CompositePoLine compositePoLine, RequestContext requestContext) {
    PoLine poLine = HelperUtils.convertToPoLine(compositePoLine);
    return saveOrderLine(poLine, requestContext);
  }


  public CompletableFuture<Void> saveOrderLines(List<PoLine> orderLines, RequestContext requestContext) {
    return FolioVertxCompletableFuture.allOf(requestContext.getContext(), orderLines.stream()
      .map(poLine -> saveOrderLine(poLine, requestContext)
        .exceptionally(t -> {
          throw new HttpException(400, ErrorCodes.POL_LINES_LIMIT_EXCEEDED.toError());
        }))
      .toArray(CompletableFuture[]::new));
  }

  public CompletableFuture<List<PoLine>> getPoLinesByOrderId(String orderId, RequestContext requestContext) {
    CompletableFuture<List<PoLine>> future = new CompletableFuture<>();
    getOrderLines("purchaseOrderId==" + orderId, 0, Integer.MAX_VALUE, requestContext)
      .thenAccept(future::complete)
      .exceptionally(t -> {
        logger.error("Exception gathering poLine data:", t);
        future.completeExceptionally(t);
        return null;
      });
    return future;
  }

  public CompletableFuture<List<CompositePoLine>> getCompositePoLinesByOrderId(String orderId, RequestContext requestContext) {
    return getOrderLines("purchaseOrderId==" + orderId, 0, Integer.MAX_VALUE, requestContext)
      .thenApply(poLines -> {
        List<CompletableFuture<CompositePoLine>> poLineFutures = new ArrayList<>();
        CompletableFuture<CompositePoLine> lineFuture = completedFuture(null);
        for (PoLine line : poLines) {
          lineFuture = lineFuture.thenCompose(v -> operateOnPoLine(HttpMethod.GET, line, requestContext));
          poLineFutures.add(lineFuture);
        }
        return poLineFutures;
      })
      .thenCompose(HelperUtils::collectResultsOnSuccess);
  }

  private CompletableFuture<CompositePoLine> operateOnPoLine(HttpMethod operation, PoLine line, RequestContext requestContext) {
    return HelperUtils.operateOnPoLine(operation, JsonObject.mapFrom(line), requestContext.getHeaders(), logger);
  }

  /**
   * Does nothing if the order already has lines.
   * Otherwise, populates the order with its lines from storage, without fetching alerts and reporting codes.
   */
  public CompletableFuture<CompositePurchaseOrder> populateOrderLines(CompositePurchaseOrder compPO, RequestContext requestContext) {
    if (CollectionUtils.isEmpty(compPO.getCompositePoLines())) {
      return getCompositePoLinesByOrderId(compPO.getId(), requestContext)
        .thenApply(poLines -> {
          PoLineCommonUtil.sortPoLinesByPoLineNumber(poLines);
          return compPO.withCompositePoLines(poLines);
        })
        .thenApply(v -> compPO)
        .exceptionally(t -> {
          Parameter idParam = new Parameter().withKey("orderId").withValue(compPO.getId());
          Parameter causeParam = new Parameter().withKey("cause").withValue(t.getCause().getMessage());
          HttpException ex = new HttpException(500, ErrorCodes.ERROR_RETRIEVING_PO_LINES.toError()
            .withParameters(List.of(idParam, causeParam)));
          logger.error(ex.getMessage(), t);
          throw new CompletionException(ex);
        });
    } else {
      return completedFuture(compPO);
    }
  }

  private CompletableFuture<List<PoLine>> getOrderLinesChunk(List<String> orderLineIds, RequestContext requestContext) {

    String query = convertIdsToCqlQuery(orderLineIds);
    RequestEntry requestEntry = new RequestEntry(ENDPOINT)
      .withQuery(query)
      .withOffset(0)
      .withLimit(MAX_IDS_FOR_GET_RQ);
    return restClient.get(requestEntry, requestContext, PoLineCollection.class)
      .thenApply(PoLineCollection::getPoLines);
  }

  public CompletableFuture<Void> deleteLineById(String lineId, RequestContext requestContext) {
    RequestEntry requestEntry = new RequestEntry(BY_ID_ENDPOINT).withId(lineId);
    return restClient.delete(requestEntry, requestContext);
  }

  public CompletableFuture<List<PoLine>> getLinesByOrderId(String orderId, RequestContext requestContext) {
    String query = String.format(ORDER_LINES_BY_ORDER_ID_QUERY, orderId);
    return getOrderLines(query, 0, Integer.MAX_VALUE, requestContext);
  }

  public CompletableFuture<Void> deletePoLinesByOrderId(String orderId, RequestContext requestContext) {
    return getLinesByOrderId(orderId, requestContext)
      .thenCompose(jsonObjects -> CompletableFuture.allOf(jsonObjects.stream()
        .map(line -> deleteLineById(line.getId(), requestContext)).toArray(CompletableFuture[]::new)))
      .exceptionally(t -> {
        logger.error("Exception deleting poLine data for order id={}", orderId, t);
        throw new CompletionException(t.getCause());
      });
  }
}

