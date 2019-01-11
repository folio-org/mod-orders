package org.folio.rest.impl;

import static io.vertx.core.Future.succeededFuture;
import static org.folio.orders.utils.HelperUtils.URL_WITH_LANG_PARAM;
import static org.folio.orders.utils.HelperUtils.operateOnSubObj;
import static org.folio.orders.utils.SubObjects.*;
import static org.folio.rest.jaxrs.resource.Orders.PutOrdersLinesByIdAndLineIdResponse.*;

import java.util.ArrayList;
import java.util.Collections;
import java.util.List;
import java.util.Map;
import java.util.concurrent.CompletableFuture;
import java.util.concurrent.CompletionStage;

import javax.ws.rs.core.Response;

import org.apache.commons.lang3.StringUtils;
import org.folio.rest.jaxrs.model.Error;
import org.folio.rest.jaxrs.model.Errors;
import org.folio.rest.jaxrs.model.Parameter;
import org.folio.rest.jaxrs.model.PoLine;
import org.folio.rest.tools.client.interfaces.HttpClientInterface;

import io.vertx.core.AsyncResult;
import io.vertx.core.Context;
import io.vertx.core.Handler;
import io.vertx.core.http.HttpMethod;
import io.vertx.core.json.JsonArray;
import io.vertx.core.json.JsonObject;
import me.escoffier.vertx.completablefuture.VertxCompletableFuture;

public class PutOrderLineByIdHelper extends AbstractHelper {

  public static final String ID = "id";
  private Errors processingErrors = new Errors();

  public PutOrderLineByIdHelper(HttpClientInterface httpClient, Map<String, String> okapiHeaders,
                                Handler<AsyncResult<Response>> asyncResultHandler, Context ctx, String lang) {
    super(httpClient, okapiHeaders, asyncResultHandler, ctx, lang);
  }

  public PutOrderLineByIdHelper(Map<String, String> okapiHeaders, Handler<AsyncResult<Response>> asyncResultHandler,
                                Context ctx, String lang) {
    this(AbstractHelper.getHttpClient(okapiHeaders), okapiHeaders, asyncResultHandler, ctx, lang);
    setDefaultHeaders(httpClient);
  }

  /**
   * Handles update of the order line. First retrieve the PO line from storage and depending on its content handle passed PO line.
   */
  public void updateOrderLine(String orderId, PoLine compOrderLine) {
    getPoLineByIdAndValidate(orderId, compOrderLine.getId())
      .thenCompose(lineFromStorage -> updateOrderLine(compOrderLine, lineFromStorage))
      .thenAccept(v -> {
        httpClient.closeClient();
        Response response;
        if (getProcessingErrors().isEmpty()) {
          response = respond204();
        } else {
          response = buildErrorResponse(500,  new Error().withMessage("PO Line partially updated but there are issues processing some PO Line sub-objects"));
        }
        asyncResultHandler.handle(succeededFuture(response));
      })
      .exceptionally(this::handleError);
  }

  /**
   * Handles update of the order line depending on the content in the storage. Returns {@link CompletableFuture} as a result.
   * In case the exception is happened in future lifecycle, the caller should handle it. However the sub-objects operations's
   * exceptions are handled generating an error. All the errors can be retrieved by calling {@link #getProcessingErrors()}
   *
   * @param compOrderLine The composite {@link PoLine} to use for storage data update
   * @param lineFromStorage {@link JsonObject} representing PO line from storage (/acq-models/mod-orders-storage/schemas/po_line.json)
   */
  public CompletableFuture<Void> updateOrderLine(PoLine compOrderLine, JsonObject lineFromStorage) {
    CompletableFuture<Void> future = new VertxCompletableFuture<>(ctx);
    updatePoLineSubObjects(compOrderLine, lineFromStorage)
      .thenCompose(poLine -> {
        logger.debug("Updating PO line...");
        String endpoint = String.format(URL_WITH_LANG_PARAM, resourceByIdPath(PO_LINES, compOrderLine.getId()), lang);
        return operateOnSubObj(HttpMethod.PUT, endpoint, poLine, httpClient, ctx, okapiHeaders, logger);
      })
      .thenAccept(entries -> future.complete(null))
      .exceptionally(throwable -> {
        future.completeExceptionally(throwable);
        return null;
      });

    return future;
  }

  /**
   * @return unmodifiable List of the {@link Error} objects. In case the list is empty, the update of the PO line was successful
   */
  public List<Error> getProcessingErrors() {
    return Collections.unmodifiableList(processingErrors.getErrors());
  }

  private CompletionStage<JsonObject> updatePoLineSubObjects(PoLine compOrderLine, JsonObject lineFromStorage) {
    JsonObject updatedLineJson = JsonObject.mapFrom(compOrderLine);
    logger.debug("Updating PO line sub-objects...");

    List<CompletableFuture<Void>> futures = new ArrayList<>();
    futures.add(handleSubObjOperation(ADJUSTMENT, updatedLineJson, lineFromStorage));
    futures.add(handleSubObjOperation(COST, updatedLineJson, lineFromStorage));
    futures.add(handleSubObjOperation(DETAILS, updatedLineJson, lineFromStorage));
    futures.add(handleSubObjOperation(ERESOURCE, updatedLineJson, lineFromStorage));
    futures.add(handleSubObjOperation(LOCATION, updatedLineJson, lineFromStorage));
    futures.add(handleSubObjOperation(PHYSICAL, updatedLineJson, lineFromStorage));
    futures.add(handleSubObjOperation(SOURCE, updatedLineJson, lineFromStorage));
    futures.add(handleSubObjOperation(VENDOR_DETAIL, updatedLineJson, lineFromStorage));
    futures.add(handleSubObjsOperation(ALERTS, updatedLineJson, lineFromStorage));
    futures.add(handleSubObjsOperation(CLAIMS, updatedLineJson, lineFromStorage));
    futures.add(handleSubObjsOperation(FUND_DISTRIBUTION, updatedLineJson, lineFromStorage));
    futures.add(handleSubObjsOperation(REPORTING_CODES, updatedLineJson, lineFromStorage));

    // Once all operations completed, return updated PO Line with new sub-object id's as json object
    return CompletableFuture.allOf(futures.toArray(new CompletableFuture[0]))
                            .thenApply(v -> updatedLineJson);
  }

  private CompletableFuture<Void> handleSubObjOperation(String prop, JsonObject updatedLine, JsonObject lineFromStorage) {
    String objId = lineFromStorage.getString(prop);
    JsonObject jsonObject = updatedLine.getJsonObject(prop);

    // Remove sub-object which will be replaced by id
    updatedLine.remove(prop);

    return handleSubObjOperation(prop, jsonObject, objId)
      .thenAccept(id -> {
        if (id != null) {
          updatedLine.put(prop, id);
        }
      })
      .exceptionally(throwable -> addProcessingError(throwable, prop, objId));
  }

  private CompletableFuture<String> handleSubObjOperation(String prop, JsonObject subObjContent, String storageId) {
    final String url;
    final HttpMethod operation;
    // In case the id is available in the PO line from storage, depending on the request content the sub-object is going to be updated or removed
    if (StringUtils.isNotEmpty(storageId)) {
      url = String.format(URL_WITH_LANG_PARAM, resourceByIdPath(prop, storageId), lang);
      operation = (subObjContent != null) ? HttpMethod.PUT : HttpMethod.DELETE;
    } else if (subObjContent != null) {
      operation = HttpMethod.POST;
      url = String.format(URL_WITH_LANG_PARAM, resourcesPath(prop), lang);
    } else {
      // There is no object in storage nor in request - skipping operation
      return CompletableFuture.completedFuture(null);
    }

    return operateOnSubObj(operation, url, subObjContent, httpClient, ctx, okapiHeaders, logger)
      .thenApply(json -> {
        if (operation == HttpMethod.PUT) {
          return storageId;
        } else if (operation == HttpMethod.POST) {
          return json.getString(ID);
        }
        return null;
      });
  }

  private CompletableFuture<Void> handleSubObjsOperation(String prop, JsonObject updatedLine, JsonObject lineFromStorage) {
    List<CompletableFuture<Void>> futures = new ArrayList<>();
    JsonArray idsInStorage = lineFromStorage.getJsonArray(prop);
    JsonArray jsonObjects = updatedLine.getJsonArray(prop);

    JsonArray newIds = new JsonArray();

    // Handle updated sub-objects content
    if (jsonObjects != null && !jsonObjects.isEmpty()) {
      // Clear array of object which will be replaced with array of id's
      updatedLine.remove(prop);
      for (int i = 0; i < jsonObjects.size(); i++) {
        JsonObject subObj = jsonObjects.getJsonObject(i);
        if (subObj != null) {
          // In case there is existing id in the sub-object, the content will be replaced
          String id = idsInStorage.remove(subObj.getString(ID)) ? subObj.getString(ID) : null;

          futures.add(handleSubObjOperation(prop, subObj, id)
            .thenAccept(newIds::add)
            .exceptionally(throwable -> addProcessingError(throwable, prop, id))
          );
        }
      }
    }

    // The remaining unprocessed objects should be removed
    for (int i = 0; i < idsInStorage.size(); i++) {
      String id = idsInStorage.getString(i);
      if (id != null) {
        futures.add(handleSubObjOperation(prop, null, id)
          .thenAccept(s -> {
          })
          .exceptionally(throwable -> {
            // In case the object is not deleted, still keep reference to old id
            newIds.add(id);
            return addProcessingError(throwable, prop, id);
          })
        );
      }
    }

    return CompletableFuture.allOf(futures.toArray(new CompletableFuture[0]))
                            .thenAccept(v -> updatedLine.put(prop, newIds));
  }

  private Void addProcessingError(Throwable exc, String propName, String propId) {
    Error error = new Error().withMessage(exc.getMessage());
    error.getParameters()
         .add(new Parameter().withKey(propName)
                             .withValue(propId));
    processingErrors.getErrors()
                    .add(error);
    return null;
  }

  @Override
  protected Response buildErrorResponse(int code, Error error) {
    final Response result;
    switch (code) {
      case 404:
        result = respond404WithTextPlain(error.getMessage());
        break;
      case 422:
        result = respond422WithApplicationJson(withErrors(error));
        break;
      default:
        if (getProcessingErrors().isEmpty()) {
          result = respond500WithTextPlain(error.getMessage());
        } else {
          processingErrors.getErrors().add(error);
          result = respond500WithApplicationJson(processingErrors);
        }
    }
    return result;
  }
}
