package org.folio.rest.impl;

import static io.vertx.core.json.JsonObject.mapFrom;
import static java.util.concurrent.CompletableFuture.allOf;
import static java.util.stream.Collectors.collectingAndThen;
import static java.util.stream.Collectors.toList;
import static me.escoffier.vertx.completablefuture.VertxCompletableFuture.completedFuture;
import static me.escoffier.vertx.completablefuture.VertxCompletableFuture.supplyBlockingAsync;
import static org.apache.commons.lang3.StringUtils.EMPTY;
import static org.apache.commons.lang3.StringUtils.isEmpty;
import static org.folio.orders.utils.HelperUtils.*;
import static org.folio.orders.utils.ResourcePathResolver.ALERTS;
import static org.folio.orders.utils.ResourcePathResolver.PO_LINES;
import static org.folio.orders.utils.ResourcePathResolver.PO_LINE_NUMBER;
import static org.folio.orders.utils.ResourcePathResolver.REPORTING_CODES;
import static org.folio.orders.utils.ResourcePathResolver.PIECES;
import static org.folio.orders.utils.ResourcePathResolver.ORDER_LINES;
import static org.folio.orders.utils.ResourcePathResolver.resourceByIdPath;
import static org.folio.orders.utils.ResourcePathResolver.resourcesPath;
import static org.folio.rest.jaxrs.model.CompositePurchaseOrder.WorkflowStatus.OPEN;
import static org.folio.rest.jaxrs.model.CompositePurchaseOrder.WorkflowStatus.PENDING;

import java.util.*;
import java.util.concurrent.CompletableFuture;
import java.util.concurrent.CompletionException;
import java.util.concurrent.CompletionStage;
import java.util.regex.Matcher;
import java.util.regex.Pattern;
import java.util.stream.Collectors;
import org.apache.commons.lang3.StringUtils;
import org.folio.orders.events.handlers.MessageAddress;
import org.folio.orders.rest.exceptions.InventoryException;
import org.folio.orders.rest.exceptions.HttpException;
import org.folio.orders.utils.ErrorCodes;
import org.folio.orders.utils.HelperUtils;
import org.folio.orders.utils.POLineProtectedFields;
import org.folio.rest.acq.model.Piece;
import org.folio.rest.acq.model.PieceCollection;
import org.folio.rest.acq.model.SequenceNumber;
import org.folio.rest.jaxrs.model.*;
import org.folio.rest.jaxrs.model.CompositePoLine.OrderFormat;
import org.folio.rest.jaxrs.model.Error;
import org.folio.rest.tools.client.interfaces.HttpClientInterface;

import io.vertx.core.Context;
import io.vertx.core.http.HttpMethod;
import io.vertx.core.json.JsonArray;
import io.vertx.core.json.JsonObject;
import me.escoffier.vertx.completablefuture.VertxCompletableFuture;
import one.util.streamex.StreamEx;

import javax.ws.rs.core.Response;

class PurchaseOrderLineHelper extends AbstractHelper {

  private static final String PURCHASE_ORDER_ID = "purchaseOrderId";
  private static final String GET_PO_LINES_BY_QUERY = resourcesPath(PO_LINES) + SEARCH_PARAMS;
  private static final String GET_ORDER_LINES_BY_QUERY = resourcesPath(ORDER_LINES) + SEARCH_PARAMS;
  private static final String LOOKUP_PIECES_ENDPOINT = resourcesPath(PIECES) + "?query=poLineId==%s&limit=%d&lang=%s";
  private static final String PO_LINE_NUMBER_ENDPOINT = resourcesPath(PO_LINE_NUMBER) + "?" + PURCHASE_ORDER_ID + "=";
  private static final Pattern PO_LINE_NUMBER_PATTERN = Pattern.compile("([a-zA-Z0-9]{5,16}-)([0-9]{1,3})");
  private static final String CREATE_INVENTORY = "createInventory";
  private static final String ERESOURCE = "eresource";
  private static final String PHYSICAL = "physical";
  private static final String OTHER = "other";
  private static final String DASH_SEPARATOR = "-";

  private final InventoryHelper inventoryHelper;
  private final ProtectionHelper protectionHelper;

  PurchaseOrderLineHelper(HttpClientInterface httpClient, Map<String, String> okapiHeaders, Context ctx, String lang) {
    super(httpClient, okapiHeaders, ctx, lang);
    inventoryHelper = new InventoryHelper(httpClient, okapiHeaders, ctx, lang);
    protectionHelper = ProtectionHelper.Operation.CREATE.getInstance(okapiHeaders, ctx, lang);
  }

  PurchaseOrderLineHelper(Map<String, String> okapiHeaders, Context ctx, String lang) {
    this(getHttpClient(okapiHeaders), okapiHeaders, ctx, lang);
  }

  CompletableFuture<PoLineCollection> getPoLines(int limit, int offset, String query, String path) {
    CompletableFuture<PoLineCollection> future = new VertxCompletableFuture<>(ctx);
    try {
      String queryParam = isEmpty(query) ? EMPTY : "&query=" + encodeQuery(query, logger);
      String endpoint = String.format(path, limit, offset, queryParam, lang);
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

  /**
   * This method is used for all internal calls to fetch PO lines without or with
   * queries that search/filter on fields present in po_line
   *
   * @param limit Limit the number of elements returned in the response
   * @param offset Skip over a number of elements by specifying an offset value for the query
   * @param query A query expressed as a CQL string (see dev.folio.org/reference/glossary#cql) using valid searchable fields.
   * @return Completable future which holds {@link PoLineCollection}
   */
  CompletableFuture<PoLineCollection> getPoLines(int limit, int offset, String query) {
    return getPoLines(limit, offset, query, GET_PO_LINES_BY_QUERY);
  }

  /**
   * This method queries a view, to limit performance implications this method
   * must be used only when there is a necessity to search/filter on the
   * Composite Purchase Order fields
   *
   * @param limit Limit the number of elements returned in the response
   * @param offset Skip over a number of elements by specifying an offset value for the query
   * @param query A query expressed as a CQL string (see dev.folio.org/reference/glossary#cql) using valid searchable fields.
   * @return Completable future which holds {@link PoLineCollection} on success or an exception on any error
   */
  CompletableFuture<PoLineCollection> getOrderLines(int limit, int offset, String query) {
    AcquisitionsUnitsHelper acqUnitsHelper = new AcquisitionsUnitsHelper(httpClient, okapiHeaders, ctx, lang);
    return acqUnitsHelper.buildAcqUnitsCqlExprToSearchRecords().thenCompose(acqUnitsCqlExpr -> {
      if (isEmpty(query)) {
        return getPoLines(limit, offset, acqUnitsCqlExpr, GET_PO_LINES_BY_QUERY);
      }
      return getPoLines(limit, offset, acqUnitsCqlExpr + " and " + query, GET_ORDER_LINES_BY_QUERY);
    });
  }

  /**
   * Creates PO Line if its content is valid and all restriction checks passed
   * @param compPOL {@link CompositePoLine} to be created
   * @return completable future which might hold {@link CompositePoLine} on success, {@code null} if validation fails or an exception if any issue happens
   */
  CompletableFuture<CompositePoLine> createPoLine(CompositePoLine compPOL) {
    // Validate PO Line content and retrieve order only if this operation is allowed
    return setTenantDefaultCreateInventoryValues(compPOL)
      .thenCompose(v -> validateNewPoLine(compPOL))
      .thenCompose(isValid -> {
        if (isValid) {
          return getCompositePurchaseOrder(compPOL)
            // The PO Line can be created only for order in Pending state
            .thenApply(this::validateOrderState)
            .thenCompose(po -> protectionHelper.isOperationProtected(po.getId())
              .thenApply(isProtected -> {
                if(isProtected) {
                  throw new HttpException(403, "Forbidden");
                } else {
                  return po;
                }
              }))
            .thenCompose(po -> createPoLine(compPOL, po));
        } else {
          return completedFuture(null);
        }
      });
  }

  private CompositePurchaseOrder validateOrderState(CompositePurchaseOrder po) {
    CompositePurchaseOrder.WorkflowStatus poStatus = po.getWorkflowStatus();
    if (poStatus != PENDING) {
      throw new HttpException(422, poStatus == OPEN ? ErrorCodes.ORDER_OPEN : ErrorCodes.ORDER_CLOSED);
    }
    return po;
  }

  /**
   * Creates PO Line assuming its content is valid and all restriction checks have been already passed
   * @param compPoLine {@link CompositePoLine} to be created
   * @param compOrder associated {@link CompositePurchaseOrder} object
   * @return completable future which might hold {@link CompositePoLine} on success or an exception if any issue happens
   */
  CompletableFuture<CompositePoLine> createPoLine(CompositePoLine compPoLine, CompositePurchaseOrder compOrder) {
    // The id is required because sub-objects are being created first
    compPoLine.setId(UUID.randomUUID().toString());
    compPoLine.setPurchaseOrderId(compOrder.getId());
    updateEstimatedPrice(compPoLine);
    updateLocationsQuantity(compPoLine.getLocations());

    JsonObject line = mapFrom(compPoLine);
    List<CompletableFuture<Void>> subObjFuts = new ArrayList<>();

    subObjFuts.add(createAlerts(compPoLine, line));
    subObjFuts.add(createReportingCodes(compPoLine, line));

    return allOf(subObjFuts.toArray(new CompletableFuture[0]))
      .thenCompose(v -> generateLineNumber(compOrder))
      .thenAccept(lineNumber -> line.put(PO_LINE_NUMBER, lineNumber))
      .thenCompose(v -> createPoLineSummary(compPoLine, line));
  }

  CompletableFuture<Void> setTenantDefaultCreateInventoryValues(CompositePoLine compPOL) {
    CompletableFuture<JsonObject> future = new VertxCompletableFuture<>(ctx);

    if (isCreateInventoryNull(compPOL)) {
      getTenantConfiguration()
        .thenApply(config -> {
          if (StringUtils.isNotEmpty(config.getString(CREATE_INVENTORY))) {
            return future.complete(new JsonObject(config.getString(CREATE_INVENTORY)));
          } else {
            return future.complete(new JsonObject());
          }
        })
        .exceptionally(t -> future.complete(new JsonObject()));
      return future
        .thenAccept(jsonConfig -> updateCreateInventory(compPOL, jsonConfig));
    } else {
      return completedFuture(null);
    }
  }

  public static boolean isCreateInventoryNull(CompositePoLine compPOL) {
    switch (compPOL.getOrderFormat()) {
    case P_E_MIX:
      return isEresourceInventoryNotPresent(compPOL)
          || isPhysicalInventoryNotPresent(compPOL);
    case ELECTRONIC_RESOURCE:
      return isEresourceInventoryNotPresent(compPOL);
    case OTHER:
    case PHYSICAL_RESOURCE:
      return isPhysicalInventoryNotPresent(compPOL);
    default:
      return false;
    }
  }

  private static Boolean isPhysicalInventoryNotPresent(CompositePoLine compPOL) {
    return Optional.ofNullable(compPOL.getPhysical())
      .map(physical -> physical.getCreateInventory() == null)
      .orElse(true);
  }

  private static Boolean isEresourceInventoryNotPresent(CompositePoLine compPOL) {
    return Optional.ofNullable(compPOL.getEresource())
      .map(eresource -> eresource.getCreateInventory() == null)
      .orElse(true);
  }

  /**
   * get the tenant configuration for the orderFormat, if not present assign the defaults
   * Default values:
   * Physical : CreateInventory.INSTANCE_HOLDING_ITEM
   * Eresource: CreateInventory.INSTANCE_HOLDING
   *
   * @param compPOL
   * @param jsonConfig
   */
  private void updateCreateInventory(CompositePoLine compPOL, JsonObject jsonConfig) {
    // try to set createInventory by values from mod-configuration. If empty -
    // set default hardcoded values
    if (compPOL.getOrderFormat().equals(OrderFormat.ELECTRONIC_RESOURCE)
        || compPOL.getOrderFormat().equals(OrderFormat.P_E_MIX)) {
      String tenantDefault = jsonConfig.getString(ERESOURCE);
      Eresource.CreateInventory eresourceDefaultValue = getEresourceInventoryDefault(tenantDefault);
      if (compPOL.getEresource() == null) {
        compPOL.setEresource(new Eresource());
      }
      if(isEresourceInventoryNotPresent(compPOL)) {
        compPOL.getEresource().setCreateInventory(eresourceDefaultValue);
      }
    }
    if (!compPOL.getOrderFormat().equals(OrderFormat.ELECTRONIC_RESOURCE)) {
      String tenantDefault = compPOL.getOrderFormat().equals(OrderFormat.OTHER) ? jsonConfig.getString(OTHER)
          : jsonConfig.getString(PHYSICAL);
      Physical.CreateInventory createInventoryDefaultValue = getPhysicalInventoryDefault(tenantDefault);
      if (compPOL.getPhysical() == null) {
        compPOL.setPhysical(new Physical());
      }
      if (isPhysicalInventoryNotPresent(compPOL)) {
        compPOL.getPhysical().setCreateInventory(createInventoryDefaultValue);
      }
    }
  }

  private Physical.CreateInventory getPhysicalInventoryDefault(String tenantDefault) {
   return StringUtils.isEmpty(tenantDefault)
        ? Physical.CreateInventory.INSTANCE_HOLDING_ITEM
        : Physical.CreateInventory.fromValue(tenantDefault);
  }

  private Eresource.CreateInventory getEresourceInventoryDefault(String tenantDefault) {
    return StringUtils.isEmpty(tenantDefault)
        ? Eresource.CreateInventory.INSTANCE_HOLDING
        : Eresource.CreateInventory.fromValue(tenantDefault);
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
      .thenCompose(lineFromStorage -> validatePOLineProtectedFieldsChanged(compOrderLine, lineFromStorage))
      .thenCompose(lineFromStorage -> {
        // override PO line number in the request with one from the storage, because it's not allowed to change it during PO line
        // update
        compOrderLine.setPoLineNumber(lineFromStorage.getString(PO_LINE_NUMBER));
        return updateOrderLine(compOrderLine, lineFromStorage).thenAccept(ok -> updateOrderStatus(compOrderLine, lineFromStorage));
      });
  }

  private CompletableFuture<JsonObject> validatePOLineProtectedFieldsChanged(CompositePoLine compOrderLine, JsonObject lineFromStorage) {
    return HelperUtils.getPurchaseOrderById(compOrderLine.getPurchaseOrderId(), lang, httpClient, ctx, okapiHeaders, logger)
      .thenCompose(purchaseOrder -> {
        if (!purchaseOrder.getString(WORKFLOW_STATUS)
          .equals(CompositePurchaseOrder.WorkflowStatus.PENDING.value())) {
          verifyProtectedFieldsChanged(POLineProtectedFields.getFieldNames(), lineFromStorage, JsonObject.mapFrom(compOrderLine));
        }
        return completedFuture(lineFromStorage);
      });
  }

  private void updateOrderStatus(CompositePoLine compOrderLine, JsonObject lineFromStorage) {
    supplyBlockingAsync(ctx, () -> lineFromStorage.mapTo(PoLine.class))
      .thenAccept(poLine -> {
        // See MODORDERS-218
        if (!StringUtils.equals(poLine.getReceiptStatus().value(), compOrderLine.getReceiptStatus().value())
          || !StringUtils.equals(poLine.getPaymentStatus().value(), compOrderLine.getPaymentStatus().value())) {
          sendEvent(MessageAddress.ORDER_STATUS, createUpdateOrderMessage(compOrderLine));
        }
      });
  }

  private JsonObject createUpdateOrderMessage(CompositePoLine compOrderLine) {
    return new JsonObject().put(ORDER_IDS, new JsonArray().add(compOrderLine.getPurchaseOrderId()));
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
    // The estimated price should be always recalculated
    updateEstimatedPrice(compOrderLine);
    updateLocationsQuantity(compOrderLine.getLocations());

    updatePoLineSubObjects(compOrderLine, lineFromStorage)
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

    // In case of no inventory updates required create pieces only
    if (inventoryUpdateNotRequired(compPOL)) {
      return createPieces(compPOL, Collections.emptyList())
        .thenRun(() ->
          logger.info("Create pieces for PO Line with '{}' id where inventory updates are not required", compPOL.getId())
        );
    }

    return inventoryHelper.handleInstanceRecord(compPOL)
      .thenCompose(inventoryHelper::handleHoldingsAndItemsRecords)
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

  void sortPoLinesByPoLineNumber(List<CompositePoLine> poLines) {
    poLines.sort(this::comparePoLinesByPoLineNumber);
  }

  /**
   * Validates purchase order line content. If content is okay, checks if allowed PO Lines limit is not exceeded.
   * @param compPOL Purchase Order Line to validate
   * @return completable future which might be completed with {@code true} if line is valid, {@code false} if not valid or an exception if processing fails
   */
  private CompletableFuture<Boolean> validateNewPoLine(CompositePoLine compPOL) {
    logger.debug("Validating if PO Line is valid...");

    // PO id is required for PO Line to be created
    if (compPOL.getPurchaseOrderId() == null) {
      addProcessingError(ErrorCodes.MISSING_ORDER_ID_IN_POL.toError());
    }
    addProcessingErrors(validatePoLine(compPOL));

    // If static validation has failed, no need to call other services
    if (!getErrors().isEmpty()) {
      return completedFuture(false);
    }

    return allOf(validatePoLineLimit(compPOL))
      .thenApply(v -> getErrors().isEmpty());
  }

  private CompletableFuture<Boolean> validatePoLineLimit(CompositePoLine compPOL) {
    String query = PURCHASE_ORDER_ID + "==" + compPOL.getPurchaseOrderId();
    return getTenantConfiguration()
      .thenCombine(getPoLines(0, 0, query), (config, poLines) -> {
        boolean isValid = poLines.getTotalRecords() < getPoLineLimit(config);
        if (!isValid) {
          addProcessingError(ErrorCodes.POL_LINES_LIMIT_EXCEEDED.toError());
        }
        return isValid;
      });
  }

  private CompletableFuture<CompositePurchaseOrder> getCompositePurchaseOrder(CompositePoLine compPOL) {
    return getPurchaseOrderById(compPOL.getPurchaseOrderId(), lang, httpClient, ctx, okapiHeaders, logger)
      .thenApply(HelperUtils::convertToCompositePurchaseOrder)
      .exceptionally(t -> {
        Throwable cause = t.getCause();
        // The case when specified order does not exist
        if (cause instanceof HttpException && ((HttpException) cause).getCode() == Response.Status.NOT_FOUND.getStatusCode()) {
          throw new HttpException(422, ErrorCodes.ORDER_NOT_FOUND);
        }
        throw t instanceof CompletionException ? (CompletionException) t : new CompletionException(cause);
      });
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
    return poNumber + DASH_SEPARATOR + sequence;
  }

  /**
   * See MODORDERS-180 for more details.
   * @param compPoLine composite PO Line
   */
  private void updateEstimatedPrice(CompositePoLine compPoLine) {
    Cost cost = compPoLine.getCost();
    cost.setPoLineEstimatedPrice(calculateEstimatedPrice(cost).getNumber().doubleValue());
  }

  private void updateLocationsQuantity(List<Location> locations) {
    locations.forEach(location -> location.setQuantity(calculateTotalLocationQuantity(location)));
  }

  /**
   * Creates pieces that are not yet in storage
   *
   * @param compPOL PO line to create Pieces Records for
   * @param expectedPiecesWithItem expected Pieces to create with created associated Items records
   * @return void future
   */
  private CompletableFuture<Void> createPieces(CompositePoLine compPOL, List<Piece> expectedPiecesWithItem) {
    int createdItemsQuantity = expectedPiecesWithItem.size();
    // do not create pieces in case of check-in flow
    if (compPOL.getCheckinItems() != null && compPOL.getCheckinItems()) {
      return completedFuture(null);
    }
    return searchForExistingPieces(compPOL)
      .thenCompose(existingPieces -> {
        List<Piece> piecesToCreate = new ArrayList<>();

        piecesToCreate.addAll(createPiecesByLocationId(compPOL, expectedPiecesWithItem, existingPieces));
        piecesToCreate.addAll(createPiecesWithoutLocationId(compPOL, existingPieces));

        return allOf(piecesToCreate.stream().map(this::createPiece).toArray(CompletableFuture[]::new));
      })
      .thenAccept(v -> validateItemsCreation(compPOL, createdItemsQuantity));
  }

  private List<Piece> createPiecesWithoutLocationId(CompositePoLine compPOL, List<Piece> existingPieces) {
    List<Piece> piecesToCreate = new ArrayList<>();
    Map<Piece.Format, Integer> expectedQuantitiesWithoutLocation = calculatePiecesQuantityWithoutLocation(compPOL);
    Map<Piece.Format, Integer> existingPiecesQuantities = calculateQuantityOfExistingPiecesWithoutLocation(existingPieces);
    expectedQuantitiesWithoutLocation.forEach((format, expectedQty) -> {
      int remainingPiecesQuantity = expectedQty - existingPiecesQuantities.getOrDefault(format, 0);
      if (remainingPiecesQuantity > 0) {
        for (int i = 0; i < remainingPiecesQuantity; i++) {
          piecesToCreate.add(new Piece().withFormat(format).withPoLineId(compPOL.getId()));
        }
      }
    });
    return piecesToCreate;
  }

  private List<Piece> createPiecesByLocationId(CompositePoLine compPOL, List<Piece> expectedPiecesWithItem, List<Piece> existingPieces) {
    List<Piece> piecesToCreate = new ArrayList<>();
    // For each location collect pieces that need to be created.
    groupLocationsById(compPOL)
      .forEach((locationId, locations) -> {
        List<Piece> filteredExistingPieces = filterByLocationId(existingPieces, locationId);
        List<Piece> filteredExpectedPiecesWithItem = filterByLocationId(expectedPiecesWithItem, locationId);
        piecesToCreate.addAll(collectMissingPiecesWithItem(filteredExpectedPiecesWithItem, filteredExistingPieces));

        Map<Piece.Format, Integer> expectedQuantitiesWithoutItem = calculatePiecesQuantity(compPOL, locations, false);
        Map<Piece.Format, Integer> quantityWithoutItem = calculateQuantityOfExistingPiecesWithoutItem(filteredExistingPieces);
        expectedQuantitiesWithoutItem.forEach((format, expectedQty) -> {
          int remainingPiecesQuantity = expectedQty - quantityWithoutItem.getOrDefault(format, 0);
          if (remainingPiecesQuantity > 0) {
            for (int i = 0; i < remainingPiecesQuantity; i++) {
              piecesToCreate.add(new Piece().withFormat(format).withLocationId(locationId).withPoLineId(compPOL.getId()));
            }
          }
        });
      });
    return piecesToCreate;
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

  private Map<Piece.Format, Integer> calculateQuantityOfExistingPiecesWithoutItem(List<Piece> pieces) {
    return StreamEx.of(pieces)
      .filter(piece -> StringUtils.isEmpty(piece.getItemId()))
      .groupingBy(Piece::getFormat, collectingAndThen(toList(), List::size));
  }

  private Map<Piece.Format, Integer> calculateQuantityOfExistingPiecesWithoutLocation(List<Piece> pieces) {
    return StreamEx.of(pieces)
      .filter(piece -> StringUtils.isEmpty(piece.getLocationId()))
      .groupingBy(Piece::getFormat, collectingAndThen(toList(), List::size));
  }

  private void validateItemsCreation(CompositePoLine compPOL, int itemsSize) {
    int expectedItemsQuantity = calculateInventoryItemsQuantity(compPOL);
    if (itemsSize != expectedItemsQuantity) {
      String message = String.format("Error creating items for PO Line with '%s' id. Expected %d but %d created",
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
    if (!StringUtils.equals(orderId, line.getString(PURCHASE_ORDER_ID))) {
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

  private int comparePoLinesByPoLineNumber(CompositePoLine poLine1, CompositePoLine poLine2) {
    String poLineNumberSuffix1 = poLine1.getPoLineNumber().split(DASH_SEPARATOR)[1];
    String poLineNumberSuffix2 = poLine2.getPoLineNumber().split(DASH_SEPARATOR)[1];
    return Integer.parseInt(poLineNumberSuffix1) - Integer.parseInt(poLineNumberSuffix2);
  }
}
