package org.folio.service.orders;

import static one.util.streamex.StreamEx.ofSubLists;
import static org.folio.orders.utils.HelperUtils.collectResultsOnSuccess;
import static org.folio.orders.utils.QueryUtils.convertFieldListToCqlQuery;
import static org.folio.orders.utils.QueryUtils.convertIdsToCqlQuery;
import static org.folio.orders.utils.ResourcePathResolver.PO_LINES_STORAGE;
import static org.folio.orders.utils.ResourcePathResolver.resourceByIdPath;
import static org.folio.rest.RestConstants.MAX_IDS_FOR_GET_RQ_15;
import static org.folio.rest.jaxrs.model.PoLine.ReceiptStatus.FULLY_RECEIVED;

import java.util.Collection;
import java.util.Date;
import java.util.List;
import java.util.NoSuchElementException;
import java.util.concurrent.CompletionException;

import lombok.extern.log4j.Log4j2;
import one.util.streamex.StreamEx;
import org.apache.commons.collections4.CollectionUtils;
import org.apache.commons.collections4.ListUtils;
import org.apache.commons.lang3.StringUtils;
import org.folio.models.PoLineLocationsPair;
import org.folio.orders.utils.PoLineCommonUtil;
import org.folio.rest.core.RestClient;
import org.folio.rest.core.exceptions.ErrorCodes;
import org.folio.rest.core.exceptions.HttpException;
import org.folio.rest.core.models.RequestContext;
import org.folio.rest.core.models.RequestEntry;
import org.folio.rest.jaxrs.model.CompositePurchaseOrder;
import org.folio.rest.jaxrs.model.Location;
import org.folio.rest.jaxrs.model.Parameter;
import org.folio.rest.jaxrs.model.PoLine;
import org.folio.rest.jaxrs.model.PoLineCollection;

import io.vertx.core.Future;
import io.vertx.core.Promise;
import io.vertx.core.http.HttpMethod;
import io.vertx.core.json.JsonObject;
import org.folio.service.inventory.InventoryHoldingManager;
import org.folio.service.orders.utils.PoLineFields;

@Log4j2
public class PurchaseOrderLineService {

  private static final String ENDPOINT = "/orders-storage/po-lines";
  private static final String BATCH_ENDPOINT = "/orders-storage/po-lines-batch";
  private static final String BY_ID_ENDPOINT = ENDPOINT + "/{id}";

  private static final String ORDER_LINES_BY_ORDER_ID_QUERY = "purchaseOrderId == %s";
  private static final String ORDER_LINES_BY_HOLDING_ID_QUERY = "locations = /@holdingId \"%s\"";

  private static final String EXCEPTION_CALLING_ENDPOINT_MSG = "Exception calling %s %s - %s";
  private static final int PO_LINE_BATCH_PARTITION_SIZE = 100;

  private final RestClient restClient;
  private final InventoryHoldingManager inventoryHoldingManager;

  public PurchaseOrderLineService(RestClient restClient, InventoryHoldingManager inventoryHoldingManager) {
    this.restClient = restClient;
    this.inventoryHoldingManager = inventoryHoldingManager;
  }

  public Future<PoLineCollection> getOrderLineCollection(String query, int offset, int limit, RequestContext requestContext) {
    RequestEntry requestEntry = new RequestEntry(ENDPOINT).withQuery(query).withOffset(offset).withLimit(limit);
    return restClient.get(requestEntry, PoLineCollection.class, requestContext);
  }

  public Future<List<PoLine>> getOrderLines(String query, int offset, int limit, RequestContext requestContext) {
    RequestEntry requestEntry = new RequestEntry(ENDPOINT).withQuery(query).withOffset(offset).withLimit(limit);
    return restClient.get(requestEntry, PoLineCollection.class, requestContext)
      .map(PoLineCollection::getPoLines);
  }

  public Future<PoLine> getOrderLineById(String lineId, RequestContext requestContext) {
    RequestEntry requestEntry = new RequestEntry(BY_ID_ENDPOINT).withId(lineId);
    return restClient.get(requestEntry, PoLine.class, requestContext);
  }

  public Future<List<PoLine>> getOrderLinesByIds(List<String> orderLineIds, RequestContext requestContext) {
    return collectResultsOnSuccess(ofSubLists(orderLineIds, MAX_IDS_FOR_GET_RQ_15)
      .map(ids -> getOrderLinesChunk(ids, requestContext)).toList())
      .map(lists -> lists.stream()
        .flatMap(Collection::stream)
        .toList());
  }

  public Future<Void> saveOrderLine(PoLine poLine, RequestContext requestContext) {
    return saveOrderLine(poLine, poLine.getLocations(), requestContext);
  }

  public Future<Void> saveOrderLine(PoLine poLine, List<Location> locations, RequestContext requestContext) {
    RequestEntry requestEntry = new RequestEntry(BY_ID_ENDPOINT).withId(poLine.getId());
    return updateSearchLocations(poLine, locations, requestContext)
      .compose(v -> restClient.put(requestEntry, poLine, requestContext));
  }

  public Future<Void> saveOrderLinesWithoutSearchLocationsUpdate(List<PoLine> orderLines, RequestContext requestContext) {
    List<PoLineCollection> poLineCollections = getPartitionedPoLines(orderLines);
    return saveOrderLinesCollections(poLineCollections, requestContext);
  }

  public Future<Void> saveOrderLinesWithLocations(List<PoLineLocationsPair> pairs, RequestContext requestContext) {
    List<PoLineCollection> poLineCollections = getPartitionedPoLines(pairs.stream().map(PoLineLocationsPair::getPoLine).toList());

    for (PoLineCollection collection: poLineCollections) {
      for (PoLine poLine: collection.getPoLines()) {
        List<Location> locations = pairs.stream()
          .filter(pair -> StringUtils.equals(pair.getPoLine().getId(), poLine.getId()))
          .findFirst().orElseThrow(() -> new NoSuchElementException("No matching PoLine found"))
          .getLocations();
        updateSearchLocations(poLine, locations, requestContext);
      }
    }

    return saveOrderLinesCollections(poLineCollections, requestContext);
  }

  public Future<Void> saveOrderLines(List<PoLine> orderLines, RequestContext requestContext) {
    List<PoLineCollection> poLineCollections = getPartitionedPoLines(orderLines);

    for (PoLineCollection collection: poLineCollections) {
      for (PoLine poLine: collection.getPoLines()) {
        updateSearchLocations(poLine, requestContext);
      }
    }

    return saveOrderLinesCollections(poLineCollections, requestContext);
  }

  private List<PoLineCollection> getPartitionedPoLines(List<PoLine> orderLines) {
    return ListUtils.partition(orderLines, PO_LINE_BATCH_PARTITION_SIZE)
      .stream()
      .map(lines -> new PoLineCollection().withPoLines(lines).withTotalRecords(lines.size()))
      .toList();
  }

  private Future<Void> saveOrderLinesCollections(List<PoLineCollection> poLineCollections, RequestContext requestContext) {
    return Future.join(poLineCollections.stream()
        .map(poLineCollection -> {
          log.info("saveOrderLines:: start saving {} po lines in batch", poLineCollection.getTotalRecords());
          RequestEntry requestEntry = new RequestEntry(BATCH_ENDPOINT);
          return restClient.put(requestEntry, poLineCollection, requestContext);
        }).toList())
      .mapEmpty();
  }

  public Future<List<PoLine>> getPoLinesByOrderId(String orderId, RequestContext requestContext) {
    Promise<List<PoLine>> promise = Promise.promise();
    getOrderLines("purchaseOrderId==" + orderId, 0, Integer.MAX_VALUE, requestContext)
      .onSuccess(promise::complete)
      .onFailure(t -> {
        log.error("Exception gathering poLine data:", t);
        promise.fail(t);
      });
    return promise.future();
  }

  public Future<JsonObject> operateOnObject(HttpMethod operation, String url, RequestContext requestContext) {
    return operateOnObject(operation, url, null, requestContext);
  }

  public Future<JsonObject> operateOnObject(HttpMethod operation, String url, JsonObject jsonObject, RequestContext requestContext) {
    Promise<JsonObject> promise = Promise.promise();
    Future<JsonObject> future = Future.succeededFuture();
    log.info("Calling {} {}", operation, url);
    if (operation.equals(HttpMethod.GET)) {
      future = restClient.getAsJsonObject(url, true, requestContext);
    }
    else if (operation.equals(HttpMethod.POST)) {
      future = restClient.postJsonObject(new RequestEntry(url), jsonObject, requestContext);
    }
    else if (operation.equals(HttpMethod.PUT)) {
      future = restClient.put(url, jsonObject, requestContext)
        .map(v -> new JsonObject());
    }
    else if (operation.equals(HttpMethod.DELETE))
    {
      future = restClient.delete(url, requestContext)
        .map(v -> new JsonObject());
    }

    future
      .onSuccess(jsonResponse -> {
        log.info("The {} {} operation completed", operation, url);
        promise.complete(jsonResponse);
      })
      .onFailure(t -> {
        Throwable cause = t instanceof CompletionException ? t.getCause() : t;
        int code = cause instanceof HttpException httpException? httpException.getCode() : 500;
        String message = String.format(EXCEPTION_CALLING_ENDPOINT_MSG, operation, url, cause.getMessage());
        log.error(message, t);
        promise.fail(new HttpException(code, message));
      });

    return promise.future();
  }

  /**
   * Does nothing if the order already has lines.
   * Otherwise, populates the order with its lines from storage.
   */
  public Future<CompositePurchaseOrder> populateOrderLines(CompositePurchaseOrder compPO, RequestContext requestContext) {
    if (CollectionUtils.isEmpty(compPO.getPoLines())) {
      return getPoLinesByOrderId(compPO.getId(), requestContext)
        .map(poLines -> {
          PoLineCommonUtil.sortPoLinesByPoLineNumber(poLines);
          return compPO.withPoLines(poLines);
        })
         .recover(t -> {
          Parameter idParam = new Parameter().withKey("orderId").withValue(compPO.getId());
          Parameter causeParam = new Parameter().withKey("cause").withValue(t.getCause().getMessage());
          HttpException ex = new HttpException(500, ErrorCodes.ERROR_RETRIEVING_PO_LINES.toError()
            .withParameters(List.of(idParam, causeParam)));
          log.error(ex.getMessage(), t);
          // TODO: replace CompletionException with more meaningful exception type everywhere it possible. No need after MODORDERS-780
          throw new CompletionException(ex);
        });
    } else {
      return Future.succeededFuture(compPO);
    }
  }

  public Future<List<PoLine>> getPoLinesByHoldingIds(List<String> holdingIds, RequestContext requestContext) {
    holdingIds = StreamEx.of(holdingIds).map(ORDER_LINES_BY_HOLDING_ID_QUERY::formatted).distinct().toList();
    var futures = ofSubLists(holdingIds, MAX_IDS_FOR_GET_RQ_15)
      .map(holdingIdsChunk -> convertFieldListToCqlQuery(holdingIdsChunk, PoLineFields.LOCATIONS.getValue(), false))
      .map(query -> getOrderLines(query, 0, Integer.MAX_VALUE, requestContext))
      .toList();
    return collectResultsOnSuccess(futures)
      .map(lists -> StreamEx.of(lists).flatMap(List::stream).toList());
  }

  private Future<List<PoLine>> getOrderLinesChunk(List<String> orderLineIds, RequestContext requestContext) {
    String query = convertIdsToCqlQuery(orderLineIds);
    RequestEntry requestEntry = new RequestEntry(ENDPOINT)
      .withQuery(query)
      .withOffset(0)
      .withLimit(MAX_IDS_FOR_GET_RQ_15);
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
      .compose(jsonObjects -> Future.join(jsonObjects.stream()
        .map(line -> deleteLineById(line.getId(), requestContext))
        .toList()))
      .recover(t -> {
        log.error("Exception deleting poLine data for order id={}", orderId, t);
        throw new CompletionException(t.getCause());
      })
      .mapEmpty();
  }

  public Future<Void> deletePoLine(PoLine poLine, RequestContext requestContext) {
    String polineId = poLine.getId();
    return operateOnObject(HttpMethod.DELETE, resourceByIdPath(PO_LINES_STORAGE, polineId), requestContext)
      .mapEmpty();
  }

  public boolean updatePoLineReceiptStatusWithoutSave(PoLine poLine, PoLine.ReceiptStatus status) {

    if (status == null || poLine.getReceiptStatus() == status) {
      return false;
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
    return true;
  }


  public Future<Void> updateSearchLocations(PoLine poLine, RequestContext requestContext) {
    return retrieveSearchLocationIds(poLine.getLocations(), requestContext)
      .map(poLine::withSearchLocationIds)
      .mapEmpty();
  }

  private Future<Void> updateSearchLocations(PoLine poLine, List<Location> locations, RequestContext requestContext) {
    return retrieveSearchLocationIds(locations, requestContext)
      .map(poLine::withSearchLocationIds)
      .mapEmpty();
  }

  private Future<List<String>> retrieveSearchLocationIds(List<Location> locations, RequestContext requestContext) {
    if (CollectionUtils.isEmpty(locations)) {
      return Future.succeededFuture(List.of());
    }

    var locationIds = StreamEx.of(locations).map(Location::getLocationId).nonNull().toList();

    /*
     * Possible scenarios where holding can be removed but the operation is not yet complete, and this would
     * result in halting the entire flow. To avoid this, we do not compare the number of holdingIds with
     * the final result from the inventory.
     */
    return inventoryHoldingManager.getLocationIdsFromHoldings(locations, requestContext)
      .map(holdingsPermanentLocationIds -> StreamEx.of(locationIds).append(holdingsPermanentLocationIds)
        .distinct().toList());
  }
}
