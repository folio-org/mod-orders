package org.folio.rest.impl;

import static io.vertx.core.json.JsonObject.mapFrom;
import static java.util.concurrent.CompletableFuture.allOf;
import static me.escoffier.vertx.completablefuture.VertxCompletableFuture.completedFuture;
import static org.apache.commons.lang3.StringUtils.EMPTY;
import static org.apache.commons.lang3.StringUtils.isEmpty;
import static org.folio.orders.utils.HelperUtils.*;
import static org.folio.orders.utils.ResourcePathResolver.ALERTS;
import static org.folio.orders.utils.ResourcePathResolver.PO_LINES;
import static org.folio.orders.utils.ResourcePathResolver.PO_LINE_NUMBER;
import static org.folio.orders.utils.ResourcePathResolver.REPORTING_CODES;
import static org.folio.orders.utils.ResourcePathResolver.PIECES;
import static org.folio.orders.utils.ResourcePathResolver.resourceByIdPath;
import static org.folio.orders.utils.ResourcePathResolver.resourcesPath;

import java.util.*;
import java.util.concurrent.CompletableFuture;
import java.util.concurrent.CompletionException;
import java.util.concurrent.CompletionStage;
import java.util.regex.Matcher;
import java.util.regex.Pattern;
import java.util.stream.Collectors;

import org.apache.commons.lang3.StringUtils;
import org.folio.orders.rest.exceptions.InventoryException;
import org.folio.orders.rest.exceptions.HttpException;
import org.folio.orders.utils.ErrorCodes;
import org.folio.orders.utils.HelperUtils;
import org.folio.rest.acq.model.Piece;
import org.folio.rest.acq.model.PieceCollection;
import org.folio.rest.acq.model.SequenceNumber;
import org.folio.rest.jaxrs.model.*;
import org.folio.rest.jaxrs.model.Error;
import org.folio.rest.tools.client.interfaces.HttpClientInterface;

import io.vertx.core.Context;
import io.vertx.core.http.HttpMethod;
import io.vertx.core.json.JsonArray;
import io.vertx.core.json.JsonObject;
import me.escoffier.vertx.completablefuture.VertxCompletableFuture;

import javax.ws.rs.core.Response;

class PurchaseOrderLineHelper extends AbstractHelper {

  private static final String GET_PO_LINES_BY_QUERY = resourcesPath(PO_LINES) + "?limit=%s&offset=%s%s&lang=%s";
  private static final String LOOKUP_PIECES_ENDPOINT = resourcesPath(PIECES) + "?query=poLineId==%s&limit=%d&lang=%s";
  private static final String PO_LINE_NUMBER_ENDPOINT = resourcesPath(PO_LINE_NUMBER) + "?purchaseOrderId=";
  private static final Pattern PO_LINE_NUMBER_PATTERN = Pattern.compile("([a-zA-Z0-9]{5,16}-)([0-9]{1,3})");
  private static final String CREATE_INVENTORY = "createInventory";
  private static final String ERESOURCE = "eresource";
  private static final String PHYSICAL = "physical";

  private final InventoryHelper inventoryHelper;

  PurchaseOrderLineHelper(HttpClientInterface httpClient, Map<String, String> okapiHeaders, Context ctx, String lang) {
    super(httpClient, okapiHeaders, ctx, lang);
    inventoryHelper = new InventoryHelper(httpClient, okapiHeaders, ctx, lang);
  }

  PurchaseOrderLineHelper(Map<String, String> okapiHeaders, Context ctx, String lang) {
    this(getHttpClient(okapiHeaders), okapiHeaders, ctx, lang);
  }

  CompletableFuture<PoLineCollection> getPoLines(int limit, int offset, String query) {
    CompletableFuture<PoLineCollection> future = new VertxCompletableFuture<>(ctx);
    try {
      String queryParam = isEmpty(query) ? EMPTY : "&query=" + encodeQuery(query, logger);
      String endpoint = String.format(GET_PO_LINES_BY_QUERY, limit, offset, queryParam, lang);
      handleGetRequest(endpoint, httpClient, ctx, okapiHeaders, logger)
        .thenAccept(jsonOrderLines -> {
          if (logger.isInfoEnabled()) {
            logger.info("Successfully retrieved order lines: {}", jsonOrderLines.encodePrettily());
          }
          future.complete(jsonOrderLines.mapTo(PoLineCollection.class));
        })
        .exceptionally(t -> {
          future.completeExceptionally(t);
          return null;
        });
    } catch (Exception e) {
      future.completeExceptionally(e);
    }

    return future;
  }

  CompletableFuture<CompositePoLine> createPoLine(CompositePoLine compPOL) {
    return getPurchaseOrderById(compPOL.getPurchaseOrderId(), lang, httpClient, ctx, okapiHeaders, logger)
      .thenApply(HelperUtils::convertToCompositePurchaseOrder)
      .thenCompose(po -> createPoLine(compPOL, po))
      .exceptionally(t -> {
        // The case when specified order does not exist
        Throwable cause = t.getCause();
        if (cause instanceof HttpException && ((HttpException) cause).getCode() == Response.Status.NOT_FOUND.getStatusCode()) {
          throw new HttpException(422, ErrorCodes.ORDER_NOT_FOUND);
        }
        throw new CompletionException(cause);
      });
  }

  CompletableFuture<CompositePoLine> createPoLine(CompositePoLine compPoLine, CompositePurchaseOrder compOrder) {
    // The id is required because sub-objects are being created first
    compPoLine.setId(UUID.randomUUID().toString());
    compPoLine.setPurchaseOrderId(compOrder.getId());

    JsonObject line = mapFrom(compPoLine);
    List<CompletableFuture<Void>> subObjFuts = new ArrayList<>();

    subObjFuts.add(createAlerts(compPoLine, line));
    subObjFuts.add(createReportingCodes(compPoLine, line));

    return allOf(subObjFuts.toArray(new CompletableFuture[0]))
      .thenCompose(v -> setTenantDefaultCreateInventoryValues(compPoLine))
      .thenCompose(v -> generateLineNumber(compOrder))
      .thenAccept(lineNumber -> line.put(PO_LINE_NUMBER, lineNumber))
      .thenCompose(v -> createPoLineSummary(compPoLine, line));
  }

  private CompletableFuture<Void> setTenantDefaultCreateInventoryValues(CompositePoLine compPOL) {
    CompletableFuture<JsonObject> future = new VertxCompletableFuture<>(ctx);

    if ((compPOL.getPhysical() != null && compPOL.getPhysical().getCreateInventory() == null)
      || (compPOL.getEresource() != null && compPOL.getEresource().getCreateInventory() == null)) {
      loadConfiguration(okapiHeaders, ctx, logger)
        .thenApply(config -> future.complete(Optional.of(config.getJsonObject(CREATE_INVENTORY)).orElse(new JsonObject())))
        .exceptionally(t -> future.complete(new JsonObject()));
      return future
        .thenAccept(jsonConfig -> updateCreateInventory(compPOL, jsonConfig));
    } else return completedFuture(null);
  }

  private void updateCreateInventory(CompositePoLine compPOL, JsonObject jsonConfig) {
    //try to set createInventory by values from mod-configuration. If empty - set default hardcoded values
    if (compPOL.getEresource() != null && compPOL.getEresource().getCreateInventory() == null) {
      String createInventory = jsonConfig.getString(ERESOURCE);
      Eresource.CreateInventory eresource = StringUtils.isEmpty(createInventory) ?
        Eresource.CreateInventory.INSTANCE_HOLDING : Eresource.CreateInventory.fromValue(createInventory);
      compPOL.getEresource().setCreateInventory(eresource);
    }
    if (compPOL.getPhysical() != null && compPOL.getPhysical().getCreateInventory() == null) {
      String createInventory = jsonConfig.getString(PHYSICAL);
      Physical.CreateInventory physical = StringUtils.isEmpty(createInventory) ?
        Physical.CreateInventory.INSTANCE_HOLDING_ITEM : Physical.CreateInventory.fromValue(createInventory);
      compPOL.getPhysical().setCreateInventory(physical);
    }
  }

  CompletableFuture<CompositePoLine> getCompositePoLine(String polineId) {
    return getPoLineById(polineId, lang, httpClient, ctx, okapiHeaders, logger)
      .thenCompose(this::populateCompositeLine);
  }

  CompletableFuture<Void> deleteLine(String lineId) {
    return getPoLineById(lineId, lang, httpClient, ctx, okapiHeaders, logger)
      .thenCompose(line -> {
        logger.debug("Deleting PO line...");
        return deletePoLine(line, httpClient, ctx, okapiHeaders, logger);
      })
      .thenAccept(json -> logger.info("The PO Line with id='{}' has been deleted successfully", lineId));
  }

  /**
   * Handles update of the order line. First retrieve the PO line from storage and depending on its content handle passed PO line.
   */
  CompletableFuture<Void> updateOrderLine(CompositePoLine compOrderLine) {
    return getPoLineByIdAndValidate(compOrderLine.getPurchaseOrderId(), compOrderLine.getId())
      .thenCompose(lineFromStorage -> {
        // override PO line number in the request with one from the storage, because it's not allowed to change it during PO line update
        compOrderLine.setPoLineNumber(lineFromStorage.getString(PO_LINE_NUMBER));
        return updateOrderLine(compOrderLine, lineFromStorage);
      });
  }

  /**
   * Handles update of the order line depending on the content in the storage. Returns {@link CompletableFuture} as a result.
   * In case the exception happened in future lifecycle, the caller should handle it. The logic is like following:<br/>
   * 1. Handle sub-objects operations's. All the exception happened for any sub-object are handled generating an error.
   * All errors can be retrieved by calling {@link #getErrors()}.<br/>
   * 2. Store PO line summary. On success, the logic checks if there are no errors happened on sub-objects operations and
   * returns succeeded future. Otherwise {@link HttpException} will be returned as result of the future.
   *
   * @param compOrderLine The composite {@link CompositePoLine} to use for storage data update
   * @param lineFromStorage {@link JsonObject} representing PO line from storage (/acq-models/mod-orders-storage/schemas/po_line.json)
   */
  CompletableFuture<Void> updateOrderLine(CompositePoLine compOrderLine, JsonObject lineFromStorage) {
    CompletableFuture<Void> future = new VertxCompletableFuture<>(ctx);
    setTenantDefaultCreateInventoryValues(compOrderLine)
      .thenCompose(v -> updatePoLineSubObjects(compOrderLine, lineFromStorage))
      .thenCompose(poLine -> updateOrderLineSummary(compOrderLine.getId(), poLine))
      .thenAccept(json -> {
        if (getErrors().isEmpty()) {
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
  CompletableFuture<JsonObject> updateOrderLineSummary(String poLineId, JsonObject poLine) {
    logger.debug("Updating PO line...");
    String endpoint = String.format(URL_WITH_LANG_PARAM, resourceByIdPath(PO_LINES, poLineId), lang);
    return operateOnObject(HttpMethod.PUT, endpoint, poLine, httpClient, ctx, okapiHeaders, logger);
  }

  /**
   * Creates Inventory records associated with given PO line and updates PO line with corresponding links.
   *
   * @param compPOL Composite PO line to update Inventory for
   * @return CompletableFuture with void.
   */
  CompletableFuture<Void> updateInventory(CompositePoLine compPOL) {
    // Check if any item should be created
    if (compPOL.getReceiptStatus() == CompositePoLine.ReceiptStatus.RECEIPT_NOT_REQUIRED) {
      return completedFuture(null);
    }
    // Set InstanceId only for the checkin flow
    if (compPOL.getCheckinItems() != null && compPOL.getCheckinItems()) {
      return inventoryHelper.handleInstanceRecord(compPOL)
        .thenCompose(cPOL -> completedFuture(null));
    }
    // create pieces only in case of no inventory updates required
    if (inventoryUpdateNotRequired(compPOL)) {
      return createPieces(compPOL, Collections.emptyList())
        .thenRun(() ->
          logger.info("Create pieces for PO Line with '{}' id where inventory updates are not required", compPOL.getId())
        );
    }

    return inventoryHelper.handleInstanceRecord(compPOL)
      .thenCompose(inventoryHelper::handleHoldingRecords)
      .thenCompose(piecesWithItemId -> createPieces(compPOL, piecesWithItemId));
  }

  String buildNewPoLineNumber(JsonObject poLineFromStorage, String poNumber) {
    String oldPoLineNumber = poLineFromStorage.getString(PO_LINE_NUMBER);
    Matcher matcher = PO_LINE_NUMBER_PATTERN.matcher(oldPoLineNumber);
    if (matcher.find()) {
      return buildPoLineNumber(poNumber, matcher.group(2));
    }
    logger.error("PO Line - {} has invalid or missing number.", poLineFromStorage.getString(ID));
    return oldPoLineNumber;
  }

  private CompletableFuture<String> generateLineNumber(CompositePurchaseOrder compOrder) {
    return handleGetRequest(getPoLineNumberEndpoint(compOrder.getId()), httpClient, ctx, okapiHeaders, logger)
      .thenApply(sequenceNumberJson -> {
        SequenceNumber sequenceNumber = sequenceNumberJson.mapTo(SequenceNumber.class);
        return buildPoLineNumber(compOrder.getPoNumber(), sequenceNumber.getSequenceNumber());
      });
  }

  private CompletionStage<CompositePoLine> populateCompositeLine(JsonObject poline) {
    return HelperUtils.operateOnPoLine(HttpMethod.GET, poline, httpClient, ctx, okapiHeaders, logger);
  }

  private String buildPoLineNumber(String poNumber, String sequence) {
    return poNumber + "-" + sequence;
  }

  /**
   * Creates pieces that are not yet in storage
   *
   * @param compPOL PO line to create Pieces Records for
   * @param expectedPiecesWithItem expected Pieces to create with created associated Items records
   * @return void future
   */
  private CompletableFuture<Void> createPieces(CompositePoLine compPOL, List<Piece> expectedPiecesWithItem) {
    int expectedItemsQuantity = isItemsUpdateRequired(compPOL) ? expectedPiecesWithItem.size() : calculateInventoryItemsQuantity(compPOL);

    return searchForExistingPieces(compPOL)
      .thenCompose(existingPieces -> {
        List<Piece> piecesToCreate = new ArrayList<>();
        // For each location collect pieces that need to be created.
        groupLocationsById(compPOL)
          .forEach((locationId, locations) -> {
              List<Piece> filteredExistingPieces = filterByLocationId(existingPieces, locationId);
              List<Piece> filteredExpectedPiecesWithItem = filterByLocationId(expectedPiecesWithItem, locationId);
              piecesToCreate.addAll(collectMissingPiecesWithItem(filteredExpectedPiecesWithItem, filteredExistingPieces));

              int expectedQuantityOfPiecesWithoutItem = calculateExpectedQuantityOfPiecesWithoutItemCreation(compPOL, locations);
              int existingQuantityOfPiecesWithoutItem = calculateQuantityOfExistingPiecesWithoutItem(existingPieces);
              int remainingPiecesQuantity = expectedQuantityOfPiecesWithoutItem - existingQuantityOfPiecesWithoutItem;
              piecesToCreate.addAll(constructMissingPiecesWithoutItem(compPOL, remainingPiecesQuantity, locationId));
            });

        return allOf(piecesToCreate.stream().map(this::createPiece).toArray(CompletableFuture[]::new));
      })
      .thenAccept(v -> validateItemsCreation(compPOL, expectedItemsQuantity));
  }

  /**
   * Search for pieces which might be already created for the PO line
   * @param compPOL PO line to retrieve Piece Records for
   * @return future with list of Pieces
   */
  private CompletableFuture<List<Piece>> searchForExistingPieces(CompositePoLine compPOL) {
    String endpoint = String.format(LOOKUP_PIECES_ENDPOINT, compPOL.getId(), calculateTotalQuantity(compPOL), lang);
    return handleGetRequest(endpoint, httpClient, ctx, okapiHeaders, logger)
      .thenApply(body -> {
        PieceCollection existedPieces = body.mapTo(PieceCollection.class);
        logger.debug("{} existing pieces found out for PO Line with '{}' id", existedPieces.getTotalRecords(), compPOL.getId());
        return existedPieces.getPieces();
      });
  }

  private List<Piece> filterByLocationId(List<Piece> pieces, String locationId) {
    return pieces.stream()
      .filter(piece -> locationId.equals(piece.getLocationId()))
      .collect(Collectors.toList());
  }

  /**
   * Find pieces for which created items, but which are not yet in the storage.
   *
   * @param piecesWithItem pieces for which created items
   * @param existingPieces pieces from storage
   * @return List of Pieces with itemId that are not in storage.
   */
  private List<Piece> collectMissingPiecesWithItem(List<Piece> piecesWithItem, List<Piece> existingPieces) {
    return piecesWithItem.stream()
      .filter(pieceWithItem -> existingPieces.stream()
        .noneMatch(existingPiece -> pieceWithItem.getItemId().equals(existingPiece.getItemId())))
      .collect(Collectors.toList());
  }

  /**
   * Creates Piece objects associated with compPOL for which do not need to create an item.
   *
   * @param compPOL PO line to construct Piece Records for
   * @param remainingPiecesQuantity quantity of pieces to create
   * @param locationId UUID of the (inventory) location record
   * @return List of pieces not requiring item creation
   */
  private List<Piece> constructMissingPiecesWithoutItem(CompositePoLine compPOL, int remainingPiecesQuantity, String locationId) {
    List<Piece> pieces = new ArrayList<>();
    for (int i = 0; i < remainingPiecesQuantity; i++) {
      pieces.add(constructPiece(locationId, compPOL.getId(), null));
    }
    return pieces;
  }

  private int calculateQuantityOfExistingPiecesWithoutItem(List<Piece> pieces) {
    return Math.toIntExact(pieces.stream()
      .filter(piece -> StringUtils.isEmpty(piece.getItemId()))
      .count());
  }

  private void validateItemsCreation(CompositePoLine compPOL, int itemsSize) {
    int expectedItemsQuantity = calculateInventoryItemsQuantity(compPOL);
    if (itemsSize != expectedItemsQuantity) {
      String message = String.format("The issue happened creating items for PO Line with '%s' id. Expected %d but %d created",
        compPOL.getId(), expectedItemsQuantity, itemsSize);
      throw new InventoryException(message);
    }
  }

  /**
   * Create Piece associated with PO Line in the storage
   *
   * @param piece associated with PO Line
   * @return CompletableFuture
   */
  private CompletableFuture<Void> createPiece(Piece piece) {
    CompletableFuture<Void> future = new VertxCompletableFuture<>(ctx);

    JsonObject pieceObj = mapFrom(piece);
    createRecordInStorage(pieceObj, resourcesPath(PIECES))
      .thenAccept(id -> future.complete(null))
      .exceptionally(t -> {
        logger.error("The piece record failed to be created. The request body: {}", pieceObj.encodePrettily());
        future.completeExceptionally(t);
        return null;
      });

    return future;
	}

  private CompletionStage<JsonObject> updatePoLineSubObjects(CompositePoLine compOrderLine, JsonObject lineFromStorage) {
    JsonObject updatedLineJson = mapFrom(compOrderLine);
    logger.debug("Updating PO line sub-objects...");

    List<CompletableFuture<Void>> futures = new ArrayList<>();

    futures.add(handleSubObjsOperation(ALERTS, updatedLineJson, lineFromStorage));
    futures.add(handleSubObjsOperation(REPORTING_CODES, updatedLineJson, lineFromStorage));

    // Once all operations completed, return updated PO Line with new sub-object id's as json object
    return allOf(futures.toArray(new CompletableFuture[0]))
      .thenApply(v -> updatedLineJson);
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

    return operateOnObject(operation, url, subObjContent, httpClient, ctx, okapiHeaders, logger)
                      .thenApply(json -> {
        if (operation == HttpMethod.PUT) {
          return storageId;
        } else if (operation == HttpMethod.POST && json.getString(ID) != null) {
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
        if (subObj != null  && subObj.getString(ID) != null) {
          String id = idsInStorage.remove(subObj.getString(ID)) ? subObj.getString(ID) : null;

          futures.add(handleSubObjOperation(prop, subObj, id)
            .exceptionally(throwable -> {
              handleProcessingError(throwable, prop, id);
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
            handleProcessingError(throwable, prop, id);
            // In case the object is not deleted, still keep reference to old id
            return id;
          })
        );
      }
    }

    return collectResultsOnSuccess(futures)
      .thenAccept(newIds -> updatedLine.put(prop, newIds));
  }

  private void handleProcessingError(Throwable exc, String propName, String propId) {
    Error error = new Error().withMessage(exc.getMessage());
    error.getParameters()
         .add(new Parameter().withKey(propName)
                             .withValue(propId));

    addProcessingError(error);
  }

  /**
   * Retrieves PO line from storage by PO line id as JsonObject and validates order id match.
   */
  private CompletableFuture<JsonObject> getPoLineByIdAndValidate(String orderId, String lineId) {
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
    if (!StringUtils.equals(orderId, line.getString("purchaseOrderId"))) {
      throw new HttpException(422, ErrorCodes.INCORRECT_ORDER_ID_IN_POL);
    }
  }

  private CompletionStage<CompositePoLine> createPoLineSummary(CompositePoLine compPOL, JsonObject line) {
    return createRecordInStorage(line, resourcesPath(PO_LINES))
      // On success set id and number of the created PO Line to composite object
      .thenApply(id -> compPOL.withId(id).withPoLineNumber(line.getString(PO_LINE_NUMBER)));
  }

  private String getPoLineNumberEndpoint(String id) {
    return PO_LINE_NUMBER_ENDPOINT + id;
  }

  private CompletableFuture<Void> createReportingCodes(CompositePoLine compPOL, JsonObject line) {
    List<CompletableFuture<String>> futures = new ArrayList<>();

    List<ReportingCode> reportingCodes = compPOL.getReportingCodes();
    if (null != reportingCodes)
      reportingCodes
        .forEach(reportingObject ->
          futures.add(createSubObjIfPresent(line, reportingObject, REPORTING_CODES, resourcesPath(REPORTING_CODES))
            .thenApply(id -> {
              if (id != null)
                reportingObject.setId(id);
              return id;
            }))
        );

    return collectResultsOnSuccess(futures)
      .thenAccept(reportingIds -> line.put(REPORTING_CODES, reportingIds))
      .exceptionally(t -> {
        logger.error("failed to create Reporting Codes", t);
        throw new CompletionException(t.getCause());
      });
  }

  private CompletableFuture<Void> createAlerts(CompositePoLine compPOL, JsonObject line) {
    List<CompletableFuture<String>> futures = new ArrayList<>();

    List<Alert> alerts = compPOL.getAlerts();
    if (null != alerts)
      alerts.forEach(alertObject ->
        futures.add(createSubObjIfPresent(line, alertObject, ALERTS, resourcesPath(ALERTS))
          .thenApply(id -> {
            if (id != null) {
              alertObject.setId(id);
            }
            return id;
          }))
      );

    return collectResultsOnSuccess(futures)
      .thenAccept(ids -> line.put(ALERTS, ids))
      .exceptionally(t -> {
        logger.error("failed to create Alerts", t);
        throw new CompletionException(t.getCause());
      });

  }

  private CompletableFuture<String> createSubObjIfPresent(JsonObject line, Object obj, String field, String url) {
    if (obj != null) {
      JsonObject json = mapFrom(obj);
      if (!json.isEmpty()) {
        return createRecordInStorage(json, url)
          .thenApply(id -> {
            logger.debug("The '{}' sub-object successfully created with id={}", field, id);
            line.put(field, id);
            return id;
          });
      }
    }
    return completedFuture(null);
  }
}
