package org.folio.rest.impl;

import static io.vertx.core.Future.succeededFuture;
import static me.escoffier.vertx.completablefuture.VertxCompletableFuture.allOf;
import static me.escoffier.vertx.completablefuture.VertxCompletableFuture.completedFuture;
import static org.folio.orders.utils.HelperUtils.URL_WITH_LANG_PARAM;
import static org.folio.orders.utils.HelperUtils.calculateInventoryItemsQuantity;
import static org.folio.orders.utils.HelperUtils.collectResultsOnSuccess;
import static org.folio.orders.utils.HelperUtils.getPoLineById;
import static org.folio.orders.utils.HelperUtils.handleGetRequest;
import static org.folio.orders.utils.HelperUtils.operateOnSubObj;
import static org.folio.orders.utils.ResourcePathResolver.ADJUSTMENT;
import static org.folio.orders.utils.ResourcePathResolver.ALERTS;
import static org.folio.orders.utils.ResourcePathResolver.CLAIMS;
import static org.folio.orders.utils.ResourcePathResolver.COST;
import static org.folio.orders.utils.ResourcePathResolver.DETAILS;
import static org.folio.orders.utils.ResourcePathResolver.ERESOURCE;
import static org.folio.orders.utils.ResourcePathResolver.FUND_DISTRIBUTION;
import static org.folio.orders.utils.ResourcePathResolver.LOCATIONS;
import static org.folio.orders.utils.ResourcePathResolver.PHYSICAL;
import static org.folio.orders.utils.ResourcePathResolver.PO_LINES;
import static org.folio.orders.utils.ResourcePathResolver.PO_LINE_NUMBER;
import static org.folio.orders.utils.ResourcePathResolver.REPORTING_CODES;
import static org.folio.orders.utils.ResourcePathResolver.SOURCE;
import static org.folio.orders.utils.ResourcePathResolver.VENDOR_DETAIL;
import static org.folio.orders.utils.ResourcePathResolver.PIECES;
import static org.folio.orders.utils.ResourcePathResolver.resourceByIdPath;
import static org.folio.orders.utils.ResourcePathResolver.resourcesPath;
import static org.folio.rest.jaxrs.resource.Orders.PutOrdersOrderLinesByIdResponse.respond204;
import static org.folio.rest.jaxrs.resource.Orders.PutOrdersOrderLinesByIdResponse.respond404WithApplicationJson;
import static org.folio.rest.jaxrs.resource.Orders.PutOrdersOrderLinesByIdResponse.respond422WithApplicationJson;
import static org.folio.rest.jaxrs.resource.Orders.PutOrdersOrderLinesByIdResponse.respond500WithApplicationJson;

import java.util.ArrayList;
import java.util.Collections;
import java.util.List;
import java.util.Map;
import java.util.concurrent.CompletableFuture;
import java.util.concurrent.CompletionStage;
import java.util.stream.Collectors;

import javax.ws.rs.core.Response;

import org.apache.commons.lang3.StringUtils;
import org.folio.orders.rest.exceptions.InventoryException;
import org.folio.orders.rest.exceptions.ValidationException;
import org.folio.orders.rest.exceptions.HttpException;
import org.folio.orders.utils.ErrorCodes;
import org.folio.rest.acq.model.Piece;
import org.folio.rest.acq.model.PieceCollection;
import org.folio.rest.jaxrs.model.CompositePoLine;
import org.folio.rest.jaxrs.model.Error;
import org.folio.rest.jaxrs.model.Errors;
import org.folio.rest.jaxrs.model.Parameter;
import org.folio.rest.tools.client.interfaces.HttpClientInterface;

import io.vertx.core.AsyncResult;
import io.vertx.core.Context;
import io.vertx.core.Handler;
import io.vertx.core.http.HttpMethod;
import io.vertx.core.json.JsonArray;
import io.vertx.core.json.JsonObject;
import me.escoffier.vertx.completablefuture.VertxCompletableFuture;

public class PutOrderLineByIdHelper extends AbstractHelper {

  private final InventoryHelper inventoryHelper;
  private final Errors processingErrors = new Errors();

  private static final String PIECES_ENDPOINT = resourcesPath(PIECES) + "?query=poLineId=%s";

  public PutOrderLineByIdHelper(HttpClientInterface httpClient, Map<String, String> okapiHeaders,
                                Handler<AsyncResult<Response>> asyncResultHandler, Context ctx, String lang) {
    super(httpClient, okapiHeaders, asyncResultHandler, ctx, lang);
    inventoryHelper = new InventoryHelper(httpClient, okapiHeaders, ctx, lang);
  }

  public PutOrderLineByIdHelper(Map<String, String> okapiHeaders, Handler<AsyncResult<Response>> asyncResultHandler,
                                Context ctx, String lang) {
    this(AbstractHelper.getHttpClient(okapiHeaders), okapiHeaders, asyncResultHandler, ctx, lang);
    setDefaultHeaders(httpClient);
  }

  /**
   * Handles update of the order line. First retrieve the PO line from storage and depending on its content handle passed PO line.
   */
  public void updateOrderLine(CompositePoLine compOrderLine) {
    getPoLineByIdAndValidate(compOrderLine.getPurchaseOrderId(), compOrderLine.getId())
      .thenCompose(lineFromStorage -> {
        // override PO line number in the request with one from the storage, because it's not allowed to change it during PO line update
        compOrderLine.setPoLineNumber(lineFromStorage.getString(PO_LINE_NUMBER));
        return updateOrderLine(compOrderLine, lineFromStorage);
      })
      .thenAccept(v -> {
        httpClient.closeClient();
        asyncResultHandler.handle(succeededFuture(respond204()));
      })
      .exceptionally(this::handleError);
  }

  /**
   * Handles update of the order line depending on the content in the storage. Returns {@link CompletableFuture} as a result.
   * In case the exception happened in future lifecycle, the caller should handle it. The logic is like following:<br/>
   * 1. Handle sub-objects operations's. All the exception happened for any sub-object are handled generating an error.
   * All errors can be retrieved by calling {@link #getProcessingErrors()}.<br/>
   * 2. Store PO line summary. On success, the logic checks if there are no errors happened on sub-objects operations and
   * returns succeeded future. Otherwise {@link HttpException} will be returned as result of the future.
   *
   * @param compOrderLine The composite {@link CompositePoLine} to use for storage data update
   * @param lineFromStorage {@link JsonObject} representing PO line from storage (/acq-models/mod-orders-storage/schemas/po_line.json)
   */
  public CompletableFuture<Void> updateOrderLine(CompositePoLine compOrderLine, JsonObject lineFromStorage) {
    CompletableFuture<Void> future = new VertxCompletableFuture<>(ctx);
    updatePoLineSubObjects(compOrderLine, lineFromStorage)
      .thenCompose(poLine -> updateOrderLineSummary(compOrderLine.getId(), poLine))
      .thenAccept(json -> {
        if (isUpdateSuccessful()) {
          future.complete(null);
        } else {
          String message = String.format("PO Line with '%s' id partially updated but there are issues processing some PO Line sub-objects",
            compOrderLine.getId());
          future.completeExceptionally(new HttpException(500, message));
        }
      })
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
   * @return {@code true} if the the update of the PO line sub-objects was successful and there were no error happened communicating with other services
   */
  public boolean isUpdateSuccessful() {
    return processingErrors.getErrors().isEmpty();
  }

  /**
   * Creates Inventory records associated with given PO line and updates PO line with corresponding links.
   *
   * @param compPOL Composite PO line to update Inventory for
   * @return CompletableFuture with updated PO line.
   */
  public CompletableFuture<Void> updateInventory(CompositePoLine compPOL) {
    // Check if any item should be created
    int expectedItemsQuantity = calculateInventoryItemsQuantity(compPOL);
    if (expectedItemsQuantity == 0) {
      logger.debug("PO Line with '{}' id does not require inventory updates", compPOL.getId());
      return completedFuture(null);
    }
    return inventoryHelper.handleInstanceRecord(compPOL)
      .thenCompose(withInstId -> getPoLineById(compPOL.getId(), lang, httpClient, ctx, okapiHeaders, logger))
      .thenCompose(jsonObj -> updateOrderLine(compPOL, jsonObj))
      .thenCompose(holdingsId -> inventoryHelper.handleItemRecords(compPOL))
      .thenCompose(itemIds -> {
        int itemsSize = itemIds.size();

        // Create piece records for successfully created items
        return createPieces(compPOL, itemIds)
          .thenAccept(v -> {
            if (itemsSize != expectedItemsQuantity) {
              String message = String.format("The issue happened creating items for PO Line with '%s' id. Expected %d but %d created",
                compPOL.getId(), expectedItemsQuantity, itemsSize);
              throw new InventoryException(message);
            }
          });
      });
  }

  /**
   * Creates Piece records corresponding to each item record associated with PO Line
   *
   * @param compPOL Composite PO line to update Inventory for
   * @param itemIds List of item id
   * @return CompletableFuture with newly created Pieces associated with PO line.
   */
  private CompletableFuture<Void> createPieces(CompositePoLine compPOL, List<String> itemIds) {
		CompletableFuture<Void> future = new VertxCompletableFuture<>(ctx);
		if (itemIds.isEmpty()) {
			future.complete(null);
			return future;
		}
		List<CompletableFuture<Void>> futuresList = new ArrayList<>();
		String poLineId = compPOL.getId();
		String endpoint = String.format(PIECES_ENDPOINT, poLineId);

		handleGetRequest(endpoint, httpClient, ctx, okapiHeaders, logger)
      .thenAccept(body -> {
        PieceCollection pieces = body.mapTo(PieceCollection.class);

        // Extract item Id's which already associated with piece records
        List<String> existingItemIds = pieces.getPieces()
            .stream()
            .map(Piece::getItemId)
            .filter(StringUtils::isNotEmpty)
            .collect(Collectors.toList());

        // Create piece records for item id's which do not have piece records yet
        itemIds.stream()
               .filter(id -> !existingItemIds.contains(id))
               .forEach(itemId -> futuresList.add(createPiece(poLineId, itemId)));

        allOf(futuresList.toArray(new CompletableFuture[0]))
          .thenAccept(v -> future.complete(null))
          .exceptionally(t -> {
            logger.error("One or more piece record failed to be created for PO Line with '{}' id", poLineId);
            future.completeExceptionally(t);
            return null;
          });
      })
      .exceptionally(t -> {
        future.completeExceptionally(t);
        return null;
      });

		return future;
  }

  /**
   * Construct Piece object associated with PO Line and create in the storage
   *
   * @param poLineId PO line identifier
   * @param itemId itemId that was created for PO Line
   * @return CompletableFuture with new Piece record.
   */
  private CompletableFuture<Void> createPiece(String poLineId, String itemId) {
		// Construct Piece object
  	Piece piece = new Piece();
  	piece.setPoLineId(poLineId);
  	piece.setItemId(itemId);
  	piece.setReceivingStatus(Piece.ReceivingStatus.EXPECTED);

    CompletableFuture<Void> future = new VertxCompletableFuture<>(ctx);

    JsonObject pieceObj = JsonObject.mapFrom(piece);
    operateOnSubObj(HttpMethod.POST, resourcesPath(PIECES), pieceObj, httpClient, ctx, okapiHeaders, logger)
      .thenAccept(body -> future.complete(null))
      .exceptionally(t -> {
        logger.error("The piece record failed to be created. The request body: {}", pieceObj.encodePrettily());
        future.completeExceptionally(t);
        return null;
      });

    return future;
	}

  private CompletionStage<JsonObject> updatePoLineSubObjects(CompositePoLine compOrderLine, JsonObject lineFromStorage) {
    JsonObject updatedLineJson = JsonObject.mapFrom(compOrderLine);
    logger.debug("Updating PO line sub-objects...");

    List<CompletableFuture<Void>> futures = new ArrayList<>();
    futures.add(handleSubObjOperation(ADJUSTMENT, updatedLineJson, lineFromStorage));
    futures.add(handleSubObjOperation(COST, updatedLineJson, lineFromStorage));
    futures.add(handleSubObjOperation(DETAILS, updatedLineJson, lineFromStorage));
    futures.add(handleSubObjOperation(ERESOURCE, updatedLineJson, lineFromStorage));
    futures.add(handleSubObjOperation(PHYSICAL, updatedLineJson, lineFromStorage));
    futures.add(handleSubObjOperation(SOURCE, updatedLineJson, lineFromStorage));
    futures.add(handleSubObjOperation(VENDOR_DETAIL, updatedLineJson, lineFromStorage));
    futures.add(handleSubObjsOperation(ALERTS, updatedLineJson, lineFromStorage));
    futures.add(handleSubObjsOperation(CLAIMS, updatedLineJson, lineFromStorage));
    futures.add(handleSubObjsOperation(FUND_DISTRIBUTION, updatedLineJson, lineFromStorage));
    futures.add(handleSubObjsOperation(LOCATIONS, updatedLineJson, lineFromStorage));
    futures.add(handleSubObjsOperation(REPORTING_CODES, updatedLineJson, lineFromStorage));

    // Once all operations completed, return updated PO Line with new sub-object id's as json object
    return allOf(futures.toArray(new CompletableFuture[0]))
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
      return completedFuture(null);
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
    List<CompletableFuture<String>> futures = new ArrayList<>();
    JsonArray idsInStorage = lineFromStorage.getJsonArray(prop);
    JsonArray jsonObjects = updatedLine.getJsonArray(prop);

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
            .exceptionally(throwable -> {
              addProcessingError(throwable, prop, id);
              return null;
            })
          );
        }
      }
    }

    // The remaining unprocessed objects should be removed
    for (int i = 0; i < idsInStorage.size(); i++) {
      String id = idsInStorage.getString(i);
      if (id != null) {
        futures.add(handleSubObjOperation(prop, null, id)
          .exceptionally(throwable -> {
            addProcessingError(throwable, prop, id);
            // In case the object is not deleted, still keep reference to old id
            return id;
          })
        );
      }
    }

    return collectResultsOnSuccess(futures)
      .thenAccept(newIds -> updatedLine.put(prop, newIds));
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
        result = respond404WithApplicationJson(withErrors(error));
        break;
      case 422:
        result = respond422WithApplicationJson(withErrors(error));
        break;
      default:
        if (getProcessingErrors().isEmpty()) {
          result = respond500WithApplicationJson(withErrors(error));
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
      throw new ValidationException(ErrorCodes.INCORRECT_ORDER_ID_IN_POL);
    }
  }
}
