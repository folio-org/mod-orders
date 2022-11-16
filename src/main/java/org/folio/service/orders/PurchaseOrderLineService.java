package org.folio.service.orders;

import static java.util.stream.Collectors.toList;
import static one.util.streamex.StreamEx.ofSubLists;
import static org.folio.orders.utils.HelperUtils.collectResultsOnSuccess;
import static org.folio.orders.utils.HelperUtils.convertIdsToCqlQuery;
import static org.folio.orders.utils.ResourcePathResolver.PO_LINES_STORAGE;
import static org.folio.orders.utils.ResourcePathResolver.resourceByIdPath;
import static org.folio.rest.RestConstants.MAX_IDS_FOR_GET_RQ;
import static org.folio.rest.jaxrs.model.PoLine.ReceiptStatus.FULLY_RECEIVED;

import java.util.ArrayList;
import java.util.Collection;
import java.util.Date;
import java.util.List;
import java.util.concurrent.CompletionException;

import org.apache.commons.collections4.CollectionUtils;
import org.apache.logging.log4j.LogManager;
import org.apache.logging.log4j.Logger;
import org.folio.okapi.common.GenericCompositeFuture;
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

import io.vertx.core.Future;
import io.vertx.core.Promise;
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

  public Future<PoLineCollection> getOrderLineCollection(String query, int offset, int limit, RequestContext requestContext) {
    RequestEntry requestEntry = new RequestEntry(ENDPOINT).withQuery(query).withOffset(offset).withLimit(limit);
    return restClient.get(requestEntry, PoLineCollection.class, requestContext);
  }


  public Future<List<PoLine>> getOrderLines(String query, int offset, int limit, RequestContext requestContext) {
    return getOrderLineCollection(query, offset, limit, requestContext).map(PoLineCollection::getPoLines);
  }

  public Future<PoLine> getOrderLineById(String lineId, RequestContext requestContext) {
    RequestEntry requestEntry = new RequestEntry(BY_ID_ENDPOINT).withId(lineId);
    return restClient.get(requestEntry, PoLine.class, requestContext);
  }

  public Future<List<PoLine>> getOrderLinesByIds(List<String> orderLineIds, RequestContext requestContext) {

    return collectResultsOnSuccess(ofSubLists(orderLineIds, MAX_IDS_FOR_GET_RQ)
      .map(ids -> getOrderLinesChunk(ids, requestContext)).toList())
      .map(lists -> lists.stream()
        .flatMap(Collection::stream)
        .collect(toList()));
  }

  public Future<Void> saveOrderLine(PoLine poLine, RequestContext requestContext) {
    RequestEntry requestEntry = new RequestEntry(BY_ID_ENDPOINT).withId(poLine.getId());
    return restClient.put(requestEntry, poLine, requestContext);
  }

  public Future<Void> saveOrderLine(CompositePoLine compositePoLine, RequestContext requestContext) {
    PoLine poLine = HelperUtils.convertToPoLine(compositePoLine);
    return saveOrderLine(poLine, requestContext);
  }


  public Future<Void> saveOrderLines(List<PoLine> orderLines, RequestContext requestContext) {
    return GenericCompositeFuture.all(orderLines.stream()
      .map(poLine -> saveOrderLine(poLine, requestContext).onFailure(t -> {
        throw new HttpException(400, ErrorCodes.POL_LINES_LIMIT_EXCEEDED.toError());
      }))
      .collect(toList()))
      .mapEmpty();
  }

  public Future<List<PoLine>> getPoLinesByOrderId(String orderId, RequestContext requestContext) {
    Promise<List<PoLine>> promise = Promise.promise();
    getOrderLines("purchaseOrderId==" + orderId, 0, Integer.MAX_VALUE, requestContext)
      .onSuccess(promise::complete)
      .onFailure(t -> {
        logger.error("Exception gathering poLine data:", t);
        promise.fail(t);
      });
    return promise.future();
  }

  public Future<List<CompositePoLine>> getCompositePoLinesByOrderId(String orderId, RequestContext requestContext) {
    return getOrderLines("purchaseOrderId==" + orderId, 0, Integer.MAX_VALUE, requestContext)
      .map(poLines -> {
        List<Future<CompositePoLine>> poLineFutures = new ArrayList<>();
        Future<CompositePoLine> lineFuture = Future.succeededFuture();
        for (PoLine line : poLines) {
          lineFuture = lineFuture.compose(v -> operateOnPoLine(HttpMethod.GET, line));
          poLineFutures.add(lineFuture);
        }
        return poLineFutures;
      })
      .compose(HelperUtils::collectResultsOnSuccess);
  }

  private Future<CompositePoLine> operateOnPoLine(HttpMethod operation, PoLine line) {
    return operateOnPoLine(operation, line);
  }

  /**
   * Does nothing if the order already has lines.
   * Otherwise, populates the order with its lines from storage, without fetching alerts and reporting codes.
   */
  public Future<CompositePurchaseOrder> populateOrderLines(CompositePurchaseOrder compPO, RequestContext requestContext) {
    if (CollectionUtils.isEmpty(compPO.getCompositePoLines())) {
      return getCompositePoLinesByOrderId(compPO.getId(), requestContext)
        .map(poLines -> {
          PoLineCommonUtil.sortPoLinesByPoLineNumber(poLines);
          return compPO.withCompositePoLines(poLines);
        })
         .onFailure(t -> {
          Parameter idParam = new Parameter().withKey("orderId").withValue(compPO.getId());
          Parameter causeParam = new Parameter().withKey("cause").withValue(t.getCause().getMessage());
          HttpException ex = new HttpException(500, ErrorCodes.ERROR_RETRIEVING_PO_LINES.toError()
            .withParameters(List.of(idParam, causeParam)));
          logger.error(ex.getMessage(), t);
          throw new CompletionException(ex);
        });
    } else {
      return Future.succeededFuture(compPO);
    }
  }

  private Future<List<PoLine>> getOrderLinesChunk(List<String> orderLineIds, RequestContext requestContext) {

    String query = convertIdsToCqlQuery(orderLineIds);
    RequestEntry requestEntry = new RequestEntry(ENDPOINT)
      .withQuery(query)
      .withOffset(0)
      .withLimit(MAX_IDS_FOR_GET_RQ);
    return restClient.get(requestEntry, PoLineCollection.class, requestContext)
      .map(PoLineCollection::getPoLines);
  }

  public Future<Void> deleteLineById(String lineId, RequestContext requestContext) {
    RequestEntry requestEntry = new RequestEntry(BY_ID_ENDPOINT).withId(lineId);
    return restClient.delete(requestEntry, requestContext);
  }

  public Future<List<PoLine>> getLinesByOrderId(String orderId, RequestContext requestContext) {
    String query = String.format(ORDER_LINES_BY_ORDER_ID_QUERY, orderId);
    return getOrderLines(query, 0, Integer.MAX_VALUE, requestContext);
  }

  public Future<Void> deletePoLinesByOrderId(String orderId, RequestContext requestContext) {
    return getLinesByOrderId(orderId, requestContext)
      .compose(jsonObjects -> GenericCompositeFuture.all(jsonObjects.stream()
        .map(line -> deleteLineById(line.getId(), requestContext)).collect(toList())))
       .onFailure(t -> {
        logger.error("Exception deleting poLine data for order id={}", orderId, t);
        throw new CompletionException(t.getCause());
      })
      .mapEmpty();
  }

  public Future<String> updatePoLineReceiptStatus(PoLine poLine, PoLine.ReceiptStatus status, RequestContext requestContext) {

    if (status == null || poLine.getReceiptStatus() == status) {
      return Future.succeededFuture();
    }

    // Update receipt date and receipt status
    if (status == FULLY_RECEIVED) {
      poLine.setReceiptDate(new Date());
    } else if (Boolean.TRUE.equals(poLine.getCheckinItems())
      && poLine.getReceiptStatus() == PoLine.ReceiptStatus.AWAITING_RECEIPT
      && status == PoLine.ReceiptStatus.PARTIALLY_RECEIVED) {
      // if checking in, set the receipt date only for the first piece
      poLine.setReceiptDate(new Date());
    } else {
      poLine.setReceiptDate(null);
    }

    poLine.setReceiptStatus(status);
    // Update PO Line in storage
    return restClient.put(resourceByIdPath(PO_LINES_STORAGE, poLine.getId()), JsonObject.mapFrom(poLine), requestContext)
      .map(v -> poLine.getId())
      .onFailure(e -> {
        logger.error("The PO Line '{}' cannot be updated with new receipt status", poLine.getId(), e);
      });
  }
}

