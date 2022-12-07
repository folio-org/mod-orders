package org.folio.service.orders;

import static io.vertx.core.json.JsonObject.mapFrom;
import static java.util.stream.Collectors.toList;
import static one.util.streamex.StreamEx.ofSubLists;
import static org.folio.helper.BaseHelper.ID;
import static org.folio.orders.utils.HelperUtils.URL_WITH_LANG_PARAM;
import static org.folio.orders.utils.HelperUtils.collectResultsOnSuccess;
import static org.folio.orders.utils.HelperUtils.convertIdsToCqlQuery;
import static org.folio.orders.utils.ResourcePathResolver.ALERTS;
import static org.folio.orders.utils.ResourcePathResolver.PO_LINES_STORAGE;
import static org.folio.orders.utils.ResourcePathResolver.REPORTING_CODES;
import static org.folio.orders.utils.ResourcePathResolver.resourceByIdPath;
import static org.folio.orders.utils.ResourcePathResolver.resourcesPath;
import static org.folio.rest.RestConstants.EN;
import static org.folio.rest.RestConstants.MAX_IDS_FOR_GET_RQ;
import static org.folio.rest.jaxrs.model.PoLine.ReceiptStatus.FULLY_RECEIVED;

import java.util.ArrayList;
import java.util.Collection;
import java.util.Date;
import java.util.List;
import java.util.concurrent.CompletionException;

import org.apache.commons.collections4.CollectionUtils;
import org.apache.commons.lang3.StringUtils;
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
import org.folio.rest.jaxrs.model.Error;
import org.folio.rest.jaxrs.model.Parameter;
import org.folio.rest.jaxrs.model.PoLine;
import org.folio.rest.jaxrs.model.PoLineCollection;

import io.vertx.core.Future;
import io.vertx.core.Promise;
import io.vertx.core.http.HttpMethod;
import io.vertx.core.json.JsonArray;
import io.vertx.core.json.JsonObject;

public class PurchaseOrderLineService {
  private static final Logger logger = LogManager.getLogger(PurchaseOrderLineService.class);
  private static final String ENDPOINT = "/orders-storage/po-lines";
  private static final String BY_ID_ENDPOINT = ENDPOINT + "/{id}";
  private static final String ORDER_LINES_BY_ORDER_ID_QUERY = "purchaseOrderId == %s";
  private static final String EXCEPTION_CALLING_ENDPOINT_MSG = "Exception calling %s %s - %s";

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
    return GenericCompositeFuture.join(orderLines.stream()
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
          lineFuture = lineFuture.compose(v -> operateOnPoLine(HttpMethod.GET, line, requestContext));
          poLineFutures.add(lineFuture);
        }
        return poLineFutures;
      })
      .compose(HelperUtils::collectResultsOnSuccess);
  }

  public Future<CompositePoLine> operateOnPoLine(HttpMethod operation, PoLine poline, RequestContext requestContext) {
    Promise<CompositePoLine> promise = Promise.promise();
    JsonObject line = JsonObject.mapFrom(poline);
    if (logger.isDebugEnabled()) {
      logger.debug("The PO line prior to {} operation: {}", operation, line.encodePrettily());
    }

    List<Future<Void>> futures = new ArrayList<>();
    futures.addAll(operateOnSubObjsIfPresent(operation, line, ALERTS, requestContext));
    futures.addAll(operateOnSubObjsIfPresent(operation, line, REPORTING_CODES, requestContext));

    GenericCompositeFuture.join(new ArrayList<>(futures))
      .onSuccess(v -> {
        if (logger.isDebugEnabled()) {
          logger.debug("The PO line after {} operation on sub-objects: {}", operation, line.encodePrettily());
        }
        promise.complete(line.mapTo(CompositePoLine.class));
      })
      .onFailure(t -> {
        logger.error("Exception resolving one or more poLine sub-object(s) on {} operation", operation,t);
        promise.fail(t);
      });
    return promise.future();
  }

  private List<Future<Void>> operateOnSubObjsIfPresent(HttpMethod operation, JsonObject pol, String field, RequestContext requestContext) {
    JsonArray array = new JsonArray();
    List<Future<Void>> futures = new ArrayList<>();
    ((Iterable<?>) pol.remove(field)).forEach(
      fieldId -> futures.add(operateOnObject(operation, resourceByIdPath(field) + fieldId, requestContext)
        .map(value -> {
          if (value != null && !value.isEmpty()) {
            array.add(value);
          }
          return null;
        })));
    pol.put(field, array);
    return futures;
  }

  public Future<JsonObject> operateOnObject(HttpMethod operation, String url, RequestContext requestContext) {
    return operateOnObject(operation, url, null, requestContext);
  }

  public Future<JsonObject> operateOnObject(HttpMethod operation, String url, JsonObject jsonObject, RequestContext requestContext) {
    Promise<JsonObject> promise = Promise.promise();
    Future<JsonObject> future = Future.succeededFuture();
    logger.info("Calling {} {}", operation, url);
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
        logger.info("The {} {} operation completed with following response body: {}", operation, url, jsonResponse.encodePrettily());
        promise.complete(jsonResponse);
      })
      .onFailure(t -> {
        Throwable cause = t instanceof CompletionException ? t.getCause() : t;
        int code = cause instanceof HttpException ? ((HttpException) cause).getCode() : 500;
        String message = String.format(EXCEPTION_CALLING_ENDPOINT_MSG, operation, url, cause.getMessage());
        logger.error(message, t);
        promise.fail(new HttpException(code, message));
      });

    return promise.future();
  }

  public Future<JsonObject> updateOrderLineSummary(String poLineId, JsonObject poLine, RequestContext requestContext) {
    logger.debug("Updating PO line...");
    String endpoint = String.format(URL_WITH_LANG_PARAM, resourceByIdPath(PO_LINES_STORAGE, poLineId), EN);
    return operateOnObject(HttpMethod.PUT, endpoint, poLine, requestContext);
  }
  public Future<JsonObject> updatePoLineSubObjects(CompositePoLine compOrderLine, JsonObject lineFromStorage, RequestContext requestContext) {
    JsonObject updatedLineJson = mapFrom(compOrderLine);
    logger.debug("Updating PO line sub-objects...");

    List<Future<Void>> futures = new ArrayList<>();

    futures.add(handleSubObjsOperation(ALERTS, updatedLineJson, lineFromStorage, requestContext));
    futures.add(handleSubObjsOperation(REPORTING_CODES, updatedLineJson, lineFromStorage, requestContext));

    // Once all operations completed, return updated PO Line with new sub-object id's as json object
    return GenericCompositeFuture.join(new ArrayList<>(futures))
      .map(cf -> updatedLineJson);
  }

  private Future<String> handleSubObjOperation(String prop, JsonObject subObjContent, String storageId, RequestContext requestContext) {
    final String url;
    final HttpMethod operation;
    // In case the id is available in the PO line from storage, depending on the request content the sub-object is going to be updated or removed
    if (StringUtils.isNotEmpty(storageId)) {
      url = String.format(URL_WITH_LANG_PARAM, resourceByIdPath(prop, storageId), EN);
      operation = (subObjContent != null) ? HttpMethod.PUT : HttpMethod.DELETE;
    } else if (subObjContent != null) {
      operation = HttpMethod.POST;
      url = String.format(URL_WITH_LANG_PARAM, resourcesPath(prop), EN);
    } else {
      // There is no object in storage nor in request - skipping operation
      return Future.succeededFuture();
    }

    return operateOnObject(operation, url, subObjContent, requestContext)
      .map(json -> {
        if (operation == HttpMethod.PUT) {
          return storageId;
        } else if (operation == HttpMethod.POST && json.getString(ID) != null) {
          return json.getString(ID);
        }
        return null;
      });
  }

  private Future<Void> handleSubObjsOperation(String prop, JsonObject updatedLine, JsonObject lineFromStorage, RequestContext requestContext) {
    List<Future<String>> futures = new ArrayList<>();
    JsonArray idsInStorage = lineFromStorage.getJsonArray(prop);
    JsonArray jsonObjects = updatedLine.getJsonArray(prop);

    // Handle updated sub-objects content
    if (jsonObjects != null && !jsonObjects.isEmpty()) {
      // Clear array of object which will be replaced with array of id's
      updatedLine.remove(prop);
      for (int i = 0; i < jsonObjects.size(); i++) {
        JsonObject subObj = jsonObjects.getJsonObject(i);
        if (subObj != null  && subObj.getString(ID) != null) {
          String id = idsInStorage.remove(subObj.getString(ID)) ? subObj.getString(ID) : null;

          futures.add(handleSubObjOperation(prop, subObj, id, requestContext)
            .recover(throwable -> {
              Error error = handleProcessingError(throwable, prop, id);
              throw new HttpException(500, error);
            })
          );
        }
      }
    }

    // The remaining unprocessed objects should be removed
    for (int i = 0; i < idsInStorage.size(); i++) {
      String id = idsInStorage.getString(i);
      if (id != null) {
        futures.add(handleSubObjOperation(prop, null, id, requestContext)
          .otherwise(throwable -> {
            handleProcessingError(throwable, prop, id);
            // In case the object is not deleted, still keep reference to old id
            return id;
          })
        );
      }
    }

    return GenericCompositeFuture.join(new ArrayList<>(futures))
      .map(newIds -> updatedLine.put(prop, newIds.list()))
      .mapEmpty();
  }

  private Error handleProcessingError(Throwable exc, String propName, String propId) {
    Error error = new Error().withMessage(exc.getMessage());
    error.getParameters()
      .add(new Parameter().withKey(propName)
        .withValue(propId));

    return error;
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
         .recover(t -> {
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
      .compose(jsonObjects -> GenericCompositeFuture.join(jsonObjects.stream()
        .map(line -> deleteLineById(line.getId(), requestContext)).collect(toList())))
       .recover(t -> {
        logger.error("Exception deleting poLine data for order id={}", orderId, t);
        throw new CompletionException(t.getCause());
      })
      .mapEmpty();
  }

  public Future<Void> deletePoLine(PoLine line, RequestContext requestContext) {
    return operateOnPoLine(HttpMethod.DELETE, line, requestContext)
      .compose(poline -> {
        String polineId = poline.getId();
        return operateOnObject(HttpMethod.DELETE, resourceByIdPath(PO_LINES_STORAGE, polineId), requestContext)
          .mapEmpty();
      });
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
      .onFailure(e -> logger.error("The PO Line '{}' cannot be updated with new receipt status", poLine.getId(), e));
  }
}

