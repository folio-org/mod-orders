package org.folio.rest.impl;

import static io.vertx.core.Future.succeededFuture;
import static org.folio.orders.utils.HelperUtils.URL_WITH_LANG_PARAM;
import static org.folio.orders.utils.HelperUtils.getPoLineById;
import static org.folio.orders.utils.HelperUtils.operateOnSubObj;
import static org.folio.orders.utils.SubObjects.*;

import java.util.ArrayList;
import java.util.Collections;
import java.util.List;
import java.util.Map;
import java.util.concurrent.CompletableFuture;
import java.util.concurrent.CompletionException;
import java.util.concurrent.CompletionStage;

import javax.ws.rs.core.Response;

import org.apache.commons.lang3.StringUtils;
import org.folio.orders.rest.exceptions.ValidationException;
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
import org.folio.orders.rest.exceptions.HttpException;
import org.folio.rest.jaxrs.model.ProductId;

import java.io.UnsupportedEncodingException;
import java.net.URLEncoder;
import java.util.Arrays;
import java.util.concurrent.CompletionException;

import static java.util.Collections.singletonList;
import static java.util.concurrent.CompletableFuture.completedFuture;
import static java.util.stream.Collectors.joining;
import static java.util.stream.Collectors.toList;
import static java.util.stream.Collectors.toMap;
import static org.folio.orders.utils.HelperUtils.handleGetRequest;
import static org.folio.rest.jaxrs.resource.Orders.PutOrdersLinesByIdAndLineIdResponse.respond204;
import static org.folio.rest.jaxrs.resource.Orders.PutOrdersLinesByIdAndLineIdResponse.respond404WithTextPlain;
import static org.folio.rest.jaxrs.resource.Orders.PutOrdersLinesByIdAndLineIdResponse.respond422WithApplicationJson;
import static org.folio.rest.jaxrs.resource.Orders.PutOrdersLinesByIdAndLineIdResponse.respond500WithApplicationJson;
import static org.folio.rest.jaxrs.resource.Orders.PutOrdersLinesByIdAndLineIdResponse.respond500WithTextPlain;
import static org.folio.rest.tools.client.Response.isSuccess;

public class PutOrderLineByIdHelper extends AbstractHelper {

  private static final String DEFAULT_INSTANCE_TYPE_CODE = "zzz";
  private static final String DEFAULT_STATUS_CODE = "temp";
  private static final String LOCATION_HEADER = "Location";

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
  public void updateOrderLine(PoLine compOrderLine) {
    getPoLineByIdAndValidate(compOrderLine.getPurchaseOrderId(),compOrderLine.getId())
      .thenCompose(lineFromStorage -> {
        // override PO line number in the request with one from the storage, because it's not allowed to change it during PO line update
        compOrderLine.setPoLineNumber(lineFromStorage.getString(PO_LINE_NUMBER));
        return updateOrderLine(compOrderLine, lineFromStorage);
      })
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
      .thenCompose(poLine -> updateOrderLineSummary(compOrderLine.getId(), poLine))
      .thenAccept(entries -> future.complete(null))
      .exceptionally(throwable -> {
        future.completeExceptionally(throwable);
        return null;
      });

    return future;
  }

  /**
   * Handle update of the order line without sub-objects
   */
  public CompletableFuture<JsonObject> updateOrderLineSummary(String poLineId, JsonObject poLine) {
    logger.debug("Updating PO line...");
    String endpoint = String.format(URL_WITH_LANG_PARAM, resourceByIdPath(PO_LINES, poLineId), lang);
    return operateOnSubObj(HttpMethod.PUT, endpoint, poLine, httpClient, ctx, okapiHeaders, logger);
  }

  /**
   * @return unmodifiable List of the {@link Error} objects. In case the list is empty, the update of the PO line was successful
   */
  public List<Error> getProcessingErrors() {
    return Collections.unmodifiableList(processingErrors.getErrors());
  }

  /**
   * Creates Inventory records associated with given PO line and updates PO line with corresponding links.
   *
   * @param compPOL Composite PO line to update Inventory for
   * @return CompletableFuture with updated PO line.
   */
  public CompletableFuture<Void> updateInventory(PoLine compPOL) {
    return handleInstanceRecord(compPOL)
      .thenCompose(withInstId -> getPoLineById(compPOL.getId(), lang, httpClient, ctx, okapiHeaders, logger))
      .thenCompose(jsonObj -> updateOrderLine(compPOL, jsonObj))
      .thenAccept(json -> {});
  }

  private CompletableFuture<PoLine> handleInstanceRecord(PoLine compPOL) {
    return getProductTypesMap(compPOL)
      .thenCompose(productTypesMap -> getInstanceRecord(compPOL, productTypesMap))
      .thenApply(compPOL::withInstanceId);
  }

  /**
   * Retrieves product type details associated with given PO line
   * and builds 'product type name' -> 'product type id' map.
   *
   * @param compPOL the PO line to retrieve product type details for
   * @return product types map
   */
  private CompletableFuture<Map<String, String>> getProductTypesMap(PoLine compPOL) {
    // do not fail if no productId is provided, should be enforced on schema level if it's required
    if (compPOL.getDetails() == null || compPOL.getDetails().getProductIds().isEmpty()) {
      return completedFuture(Collections.emptyMap());
    }

    String endpoint = compPOL.getDetails().getProductIds().stream()
      .map(productId -> String.format("name==%s", productId.getProductIdType().toString()))
      .collect(joining(" or ", "/identifier-types?query=", ""));

    return handleGetRequest(endpoint, httpClient, ctx, okapiHeaders, logger)
      .thenApply(productTypes -> {
        if (productTypes.getJsonArray("identifierTypes").size() != compPOL.getDetails().getProductIds().size()) {
          throw new CompletionException(new HttpException(422,
            "Invalid product type(s) is specified for the PO line with id " + compPOL.getId()));
        }
        return productTypes;
      })
      .thenApply(productTypes -> productTypes.getJsonArray("identifierTypes").stream()
        .collect(toMap(jsonObj -> ((JsonObject) jsonObj).getString("name"),
          jsonObj -> ((JsonObject) jsonObj).getString("id"),
          (k1, k2) -> k1)));
  }

  /**
   * Returns Id of the Instance Record corresponding to given PO line.
   * Instance record is either retrieved from Inventory or a new one is created if no corresponding Record exists.
   *
   * @param compPOL PO line to retrieve Instance Record Id for
   * @param productTypesMap product types Map used to build Inventory query
   * @return future with Instance Id
   */
  private CompletionStage<String> getInstanceRecord(PoLine compPOL, Map<String, String> productTypesMap) {
    // proceed with new Instance Record creation if no productId is provided
    if (compPOL.getDetails() == null || compPOL.getDetails().getProductIds().isEmpty()) {
      return createInstanceRecord(compPOL, productTypesMap);
    }

    String query = compPOL.getDetails().getProductIds().stream()
      .map(productId -> buildProductIdQuery(productId, productTypesMap))
      .collect(joining(" or "));

    // query contains special characters so must be encoded before submitting
    String endpoint = null;
    try {
      endpoint = "/inventory/instances?query=" + URLEncoder.encode(query, "UTF-8");
    } catch (UnsupportedEncodingException e) {
      logger.error(String.format("Error during query encoding: %s", e.getMessage()));
      throw new CompletionException(e);
    }

    return handleGetRequest(endpoint, httpClient, ctx, okapiHeaders, logger)
      .thenCompose(instances -> {
        if (instances.getJsonArray("instances").size() > 0) {
          return completedFuture(instances.getJsonArray("instances").getJsonObject(0).getString("id"));
        }
        return createInstanceRecord(compPOL, productTypesMap);
      });
  }

  /**
   * Creates Instance Record in Inventory and returns its Id.
   *
   * @param compPOL PO line to create Instance Record for
   * @param productTypesMap product types Map used to build Instance Record json object
   * @return id of newly created Instance Record
   */
  private CompletableFuture<String> createInstanceRecord(PoLine compPOL, Map<String, String> productTypesMap) {
    JsonObject lookupObj = new JsonObject();
    CompletableFuture<Void> instanceTypeFuture = getInstanceType(DEFAULT_INSTANCE_TYPE_CODE)
      .thenAccept(lookupObj::mergeIn);
    CompletableFuture<Void> statusFuture = getStatus(DEFAULT_STATUS_CODE)
      .thenAccept(lookupObj::mergeIn);

    return VertxCompletableFuture.allOf(ctx, instanceTypeFuture, statusFuture)
      .thenApply(v -> buildInstanceRecordJsonObject(compPOL, productTypesMap, lookupObj))
      .thenCompose(instanceRecJson -> {
        try {
          return httpClient.request(HttpMethod.POST, instanceRecJson.toBuffer(), "/inventory/instances", okapiHeaders)
            .thenApply(response -> {
              logger.debug("Validating received response");
              if (!isSuccess(response.getCode())) {
                throw new CompletionException(
                  new HttpException(response.getCode(), response.getError().getString("errorMessage")));
              }
              return response;
            })
            .thenApply(response -> {
              String location = response.getHeaders().get(LOCATION_HEADER);
              return location.substring(location.lastIndexOf('/')+1);
            });
        } catch (Exception e) {
          throw new CompletionException(e);
        }
      });
  }

  private CompletableFuture<JsonObject> getInstanceType(String typeName) {
    String endpoint = String.format("/instance-types?query=code==%s", typeName);
    return handleGetRequest(endpoint, httpClient, ctx, okapiHeaders, logger);
  }

  private CompletableFuture<JsonObject> getStatus(String statusCode) {
    String endpoint = String.format("/instance-statuses?query=code==%s", statusCode);
    return handleGetRequest(endpoint, httpClient, ctx, okapiHeaders, logger);
  }

  private String buildProductIdQuery(ProductId productId, Map<String, String> productTypes) {
    return String.format("(identifiers adj \"\\\"identifierTypeId\\\": \\\"%s\\\"\" " +
        "and identifiers adj \"\\\"value\\\": \\\"%s\\\"\")",
      productTypes.get(productId.getProductIdType().toString()),
      productId.getProductId());
  }

  private JsonObject buildInstanceRecordJsonObject(PoLine compPOL, Map<String, String> productTypes, JsonObject lookupObj) {
    JsonObject instance = new JsonObject();
    if (compPOL.getSource() != null) {
      instance.put("source", compPOL.getSource().getCode());
    }
    if (compPOL.getTitle() != null) {
      instance.put("title", compPOL.getTitle());
    }
    if (compPOL.getEdition() != null) {
      instance.put("editions", new JsonArray(singletonList(compPOL.getEdition())));
    }
    instance.put("statusId", lookupObj.getJsonArray("instanceStatuses").getJsonObject(0).getString("id"));
    instance.put("instanceTypeId", lookupObj.getJsonArray("instanceTypes").getJsonObject(0).getString("id"));

    if (compPOL.getPublisher() != null || compPOL.getPublicationDate() != null) {
      JsonObject publication = new JsonObject();
      publication.put("publisher", compPOL.getPublisher());
      publication.put("dateOfPublication", compPOL.getPublicationDate());
      instance.put("publication", new JsonArray(Arrays.asList(publication)));
    }

    if (compPOL.getDetails() != null && compPOL.getDetails().getProductIds() != null) {
      List<JsonObject> identifiers = compPOL.getDetails().getProductIds().stream()
        .map(pId -> {
          JsonObject identifier = new JsonObject();
          identifier.put("identifierTypeId", productTypes.get(pId.getProductIdType().toString()));
          identifier.put("value", pId.getProductId());
          return identifier;
        })
        .collect(toList());
      instance.put("identifiers", new JsonArray(identifiers));
    }
    return instance;
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

  /**
   * Retrieves PO line from storage by PO line id as JsonObject and validates order id match.
   */
  protected CompletableFuture<JsonObject> getPoLineByIdAndValidate(String orderId, String lineId) {
    return getPoLineById(lineId, lang, httpClient, ctx, okapiHeaders, logger)
      .thenApply(line -> {
        logger.debug("Validating if the retrieved PO line corresponds to PO");
        validateOrderId(orderId, line);
        return line;
      });
  }

  /**
   * Validates if the retrieved PO line corresponds to PO (orderId). In case the PO line does not correspond to order id the exception is thrown
   * @param orderId order identifier
   * @param line PO line retrieved from storage
   */
  private void validateOrderId(String orderId, JsonObject line) {
    if (!StringUtils.equals(orderId, line.getString("purchase_order_id"))) {
      String msg = String.format("The PO line with id=%s does not belong to order with id=%s", line.getString("id"), orderId);
      throw new CompletionException(new ValidationException(msg, ID_MISMATCH_ERROR_CODE));
    }
  }
}
