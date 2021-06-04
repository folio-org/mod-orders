package org.folio.helper;

import static io.vertx.core.json.JsonObject.mapFrom;
import static java.util.concurrent.CompletableFuture.allOf;
import static java.util.concurrent.CompletableFuture.completedFuture;
import static java.util.stream.Collectors.groupingBy;
import static java.util.stream.Collectors.toList;
import static org.apache.commons.collections4.CollectionUtils.isEqualCollection;
import static org.apache.commons.lang3.StringUtils.EMPTY;
import static org.apache.commons.lang3.StringUtils.isEmpty;
import static org.folio.orders.utils.ErrorCodes.LOCATION_CAN_NOT_BE_MODIFIER_AFTER_OPEN;
import static org.folio.orders.utils.HelperUtils.URL_WITH_LANG_PARAM;
import static org.folio.orders.utils.HelperUtils.calculateEstimatedPrice;
import static org.folio.orders.utils.HelperUtils.calculateTotalLocationQuantity;
import static org.folio.orders.utils.HelperUtils.collectResultsOnSuccess;
import static org.folio.orders.utils.HelperUtils.combineCqlExpressions;
import static org.folio.orders.utils.HelperUtils.deletePoLine;
import static org.folio.orders.utils.HelperUtils.encodeQuery;
import static org.folio.orders.utils.HelperUtils.getPoLineLimit;
import static org.folio.orders.utils.HelperUtils.getPurchaseOrderById;
import static org.folio.orders.utils.HelperUtils.handleGetRequest;
import static org.folio.orders.utils.HelperUtils.inventoryUpdateNotRequired;
import static org.folio.orders.utils.HelperUtils.operateOnObject;
import static org.folio.orders.utils.HelperUtils.verifyProtectedFieldsChanged;
import static org.folio.orders.utils.ProtectedOperationType.DELETE;
import static org.folio.orders.utils.ProtectedOperationType.UPDATE;
import static org.folio.orders.utils.ResourcePathResolver.ALERTS;
import static org.folio.orders.utils.ResourcePathResolver.PO_LINES;
import static org.folio.orders.utils.ResourcePathResolver.PO_LINE_NUMBER;
import static org.folio.orders.utils.ResourcePathResolver.REPORTING_CODES;
import static org.folio.orders.utils.ResourcePathResolver.resourceByIdPath;
import static org.folio.orders.utils.ResourcePathResolver.resourcesPath;
import static org.folio.orders.utils.validators.CompositePoLineValidationUtil.validatePoLine;
import static org.folio.rest.jaxrs.model.CompositePurchaseOrder.WorkflowStatus.OPEN;
import static org.folio.rest.jaxrs.model.CompositePurchaseOrder.WorkflowStatus.PENDING;

import java.util.ArrayList;
import java.util.Collections;
import java.util.List;
import java.util.Map;
import java.util.Objects;
import java.util.Optional;
import java.util.UUID;
import java.util.concurrent.CompletableFuture;
import java.util.concurrent.CompletionException;
import java.util.concurrent.CompletionStage;
import java.util.function.Predicate;
import java.util.regex.Matcher;
import java.util.regex.Pattern;

import javax.ws.rs.core.Response;

import org.apache.commons.collections4.CollectionUtils;
import org.apache.commons.lang3.StringUtils;
import org.folio.completablefuture.FolioVertxCompletableFuture;
import org.folio.orders.events.handlers.MessageAddress;
import org.folio.orders.rest.exceptions.HttpException;
import org.folio.orders.utils.ErrorCodes;
import org.folio.orders.utils.HelperUtils;
import org.folio.orders.utils.POLineProtectedFields;
import org.folio.orders.utils.ProtectedOperationType;
import org.folio.rest.acq.model.SequenceNumber;
import org.folio.rest.core.models.RequestContext;
import org.folio.rest.jaxrs.model.Alert;
import org.folio.rest.jaxrs.model.CompositePoLine;
import org.folio.rest.jaxrs.model.CompositePoLine.OrderFormat;
import org.folio.rest.jaxrs.model.CompositePoLine.ReceiptStatus;
import org.folio.rest.jaxrs.model.CompositePurchaseOrder;
import org.folio.rest.jaxrs.model.Cost;
import org.folio.rest.jaxrs.model.Eresource;
import org.folio.rest.jaxrs.model.Error;
import org.folio.rest.jaxrs.model.FundDistribution;
import org.folio.rest.jaxrs.model.Location;
import org.folio.rest.jaxrs.model.Parameter;
import org.folio.rest.jaxrs.model.Physical;
import org.folio.rest.jaxrs.model.Piece;
import org.folio.rest.jaxrs.model.PieceCollection;
import org.folio.rest.jaxrs.model.PoLine;
import org.folio.rest.jaxrs.model.PoLineCollection;
import org.folio.rest.jaxrs.model.ProductId;
import org.folio.rest.jaxrs.model.ReportingCode;
import org.folio.rest.jaxrs.model.Title;
import org.folio.rest.tools.client.interfaces.HttpClientInterface;
import org.folio.service.AcquisitionsUnitsService;
import org.folio.service.ProtectionService;
import org.folio.service.configuration.ConfigurationEntriesService;
import org.folio.service.finance.expenceclass.ExpenseClassValidationService;
import org.folio.service.finance.transaction.EncumbranceService;
import org.folio.service.finance.transaction.EncumbranceWorkflowStrategy;
import org.folio.service.finance.transaction.EncumbranceWorkflowStrategyFactory;
import org.folio.service.inventory.InventoryManager;
import org.folio.service.orders.OrderInvoiceRelationService;
import org.folio.service.orders.OrderWorkflowType;
import org.folio.service.orders.PurchaseOrderLineService;
import org.folio.service.pieces.PiecesService;
import org.folio.service.titles.TitlesService;
import org.springframework.beans.factory.annotation.Autowired;

import io.vertx.core.Context;
import io.vertx.core.http.HttpMethod;
import io.vertx.core.json.JsonArray;
import io.vertx.core.json.JsonObject;

public class PurchaseOrderLineHelper extends AbstractHelper {

  private static final String ISBN = "ISBN";
  private static final String PURCHASE_ORDER_ID = "purchaseOrderId";
  private static final String GET_PO_LINES_BY_QUERY = resourcesPath(PO_LINES) + SEARCH_PARAMS;
  private static final String PO_LINE_NUMBER_ENDPOINT = resourcesPath(PO_LINE_NUMBER) + "?" + PURCHASE_ORDER_ID + "=";
  private static final Pattern PO_LINE_NUMBER_PATTERN = Pattern.compile("([a-zA-Z0-9]{1,22}-)([0-9]{1,3})");
  private static final String CREATE_INVENTORY = "createInventory";
  private static final String ERESOURCE = "eresource";
  private static final String PHYSICAL = "physical";
  private static final String OTHER = "other";
  private static final String DASH_SEPARATOR = "-";
  public static final String QUERY_BY_PO_LINE_ID = "poLineId==";

  @Autowired
  private InventoryManager inventoryManager;
  @Autowired
  private PiecesService piecesService;
  @Autowired
  private EncumbranceService encumbranceService;
  @Autowired
  private ExpenseClassValidationService expenseClassValidationService;
  @Autowired
  private EncumbranceWorkflowStrategyFactory encumbranceWorkflowStrategyFactory;
  @Autowired
  private OrderInvoiceRelationService orderInvoiceRelationService;
  @Autowired
  private ConfigurationEntriesService configurationEntriesService;
  @Autowired
  private TitlesService titlesService;
  @Autowired
  private AcquisitionsUnitsService acquisitionsUnitsService;
  @Autowired
  private ProtectionService protectionService;
  @Autowired
  private PurchaseOrderLineService purchaseOrderLineService;

  public PurchaseOrderLineHelper(HttpClientInterface httpClient, Map<String, String> okapiHeaders, Context ctx, String lang) {
    super(httpClient, okapiHeaders, ctx, lang);
  }

  public PurchaseOrderLineHelper(Map<String, String> okapiHeaders, Context ctx, String lang) {
    this(getHttpClient(okapiHeaders), okapiHeaders, ctx, lang);
  }

  CompletableFuture<PoLineCollection> getPoLines(int limit, int offset, String query, String path) {
    CompletableFuture<PoLineCollection> future = new CompletableFuture<>();
    try {
      String queryParam = isEmpty(query) ? EMPTY : "&query=" + encodeQuery(query, logger);
      String endpoint = String.format(path, limit, offset, queryParam, lang);
      handleGetRequest(endpoint, httpClient, okapiHeaders, logger)
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
  public CompletableFuture<PoLineCollection> getPoLines(int limit, int offset, String query) {
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
  public CompletableFuture<PoLineCollection> getOrderLines(int limit, int offset, String query) {
    return acquisitionsUnitsService.buildAcqUnitsCqlExprToSearchRecords(getRequestContext(), "purchaseOrder.")
      .thenCompose(acqUnitsCqlExpr -> {
        if (isEmpty(query)) {
          return getPoLines(limit, offset, acqUnitsCqlExpr);
        }
        return getPoLines(limit, offset, combineCqlExpressions("and", acqUnitsCqlExpr, query), GET_PO_LINES_BY_QUERY);
      });
  }

  /**
   * Creates PO Line if its content is valid and all restriction checks passed
   * @param compPOL {@link CompositePoLine} to be created
   * @return completable future which might hold {@link CompositePoLine} on success, {@code null} if validation fails or an exception if any issue happens
   */
  public CompletableFuture<CompositePoLine> createPoLine(CompositePoLine compPOL, RequestContext requestContext) {
    // Validate PO Line content and retrieve order only if this operation is allowed
    return setTenantDefaultCreateInventoryValues(compPOL)
      .thenCompose(v -> validateNewPoLine(compPOL, requestContext))
      .thenCompose(isValid -> {
        if (isValid.equals(Boolean.TRUE)) {
          return getCompositePurchaseOrder(compPOL.getPurchaseOrderId())
            // The PO Line can be created only for order in Pending state
            .thenApply(this::validateOrderState)
            .thenCompose(po -> protectionService.isOperationRestricted(po.getAcqUnitIds(), ProtectedOperationType.CREATE, getRequestContext())
                                                .thenApply(vVoid -> po))
            .thenCompose(po -> createPoLine(compPOL, po));
        } else {
          return completedFuture(null);
        }
      });
  }

  public void makePoLinesPending(List<CompositePoLine> compositePoLines) {
    compositePoLines.forEach(poLine -> {
      if (poLine.getPaymentStatus() == CompositePoLine.PaymentStatus.AWAITING_PAYMENT) {
        poLine.setPaymentStatus(CompositePoLine.PaymentStatus.PENDING);
      }
      if (poLine.getReceiptStatus() == CompositePoLine.ReceiptStatus.AWAITING_RECEIPT) {
        poLine.setReceiptStatus(CompositePoLine.ReceiptStatus.PENDING);
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
    if (isEmpty(compPoLine.getId())) {
      compPoLine.setId(UUID.randomUUID().toString());
    }
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

  public CompletableFuture<Void> setTenantDefaultCreateInventoryValues(CompositePoLine compPOL) {
    CompletableFuture<JsonObject> future = new CompletableFuture<>();

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

  private static boolean isPhysicalInventoryNotPresent(CompositePoLine compPOL) {
    return Optional.ofNullable(compPOL.getPhysical())
      .map(physical -> physical.getCreateInventory() == null)
      .orElse(true);
  }

  private static boolean isEresourceInventoryNotPresent(CompositePoLine compPOL) {
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
   * @param compPOL composite purchase order
   * @param jsonConfig createInventory configuration
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

  public CompletableFuture<CompositePoLine> getCompositePoLine(String polineId, RequestContext requestContext) {
    return purchaseOrderLineService.getOrderLineById(polineId, requestContext)
      .thenCompose(line -> getCompositePurchaseOrder(line.getPurchaseOrderId())
        .thenCompose(order -> protectionService.isOperationRestricted(order.getAcqUnitIds(), ProtectedOperationType.READ, requestContext))
        .thenCompose(ok -> populateCompositeLine(line)));
  }

  public CompletableFuture<Void> deleteLine(String lineId, RequestContext requestContext) {
    return purchaseOrderLineService.getOrderLineById(lineId, requestContext)
      .thenCompose(line -> verifyDeleteAllowed(line, requestContext))
      .thenCompose(line -> {
        logger.debug("Deleting PO line...");
        return encumbranceService.deletePoLineEncumbrances(lineId, requestContext)
          .thenCompose(v -> deletePoLine(line, httpClient, okapiHeaders, logger));
      })
      .thenAccept(json -> logger.info("The PO Line with id='{}' has been deleted successfully", lineId));
  }

  private CompletableFuture<JsonObject> verifyDeleteAllowed(PoLine line, RequestContext requestContext) {
    return orderInvoiceRelationService.checkOrderInvoiceRelationship(line.getPurchaseOrderId(), requestContext)
      .thenCompose(v -> getCompositePurchaseOrder(line.getPurchaseOrderId())
        .thenCompose(order -> protectionService.isOperationRestricted(order.getAcqUnitIds(), DELETE, requestContext))
        .thenApply(aVoid -> JsonObject.mapFrom(line)));
  }

  /**
   * Handles update of the order line. First retrieve the PO line from storage and depending on its content handle passed PO line.
   */
  public CompletableFuture<Void> updateOrderLine(CompositePoLine compOrderLine, RequestContext requestContext) {
    return getPoLineByIdAndValidate(compOrderLine.getPurchaseOrderId(), compOrderLine.getId())
        .thenCompose(lineFromStorage -> getCompositePurchaseOrder(compOrderLine.getPurchaseOrderId())
          .thenCompose(compOrder -> {
            validatePOLineProtectedFieldsChanged(compOrderLine, lineFromStorage, compOrder);
            updateLocationsQuantity(compOrderLine.getLocations());
            updateEstimatedPrice(compOrderLine);
            checkLocationCanBeModified(compOrderLine, lineFromStorage.mapTo(PoLine.class), compOrder);

            return protectionService.isOperationRestricted(compOrder.getAcqUnitIds(), UPDATE, requestContext)
                .thenCompose(v -> validateAndNormalizeISBN(compOrderLine, requestContext))
                .thenCompose(v -> validateAccessProviders(compOrderLine))
                .thenCompose(v -> expenseClassValidationService.validateExpenseClassesForOpenedOrder(compOrder, Collections.singletonList(compOrderLine), requestContext))
                .thenCompose(v -> processOpenedPoLine(compOrder, compOrderLine, lineFromStorage))
                .thenApply(v -> lineFromStorage);
          }))
        .thenCompose(lineFromStorage -> {
          // override PO line number in the request with one from the storage, because it's not allowed to change it during PO line
          // update
          compOrderLine.setPoLineNumber(lineFromStorage.getString(PO_LINE_NUMBER));
          return updateOrderLine(compOrderLine, lineFromStorage)
            .thenAccept(ok -> updateOrderStatus(compOrderLine, lineFromStorage));
        });

  }

  private CompletableFuture<Void> processOpenedPoLine(CompositePurchaseOrder compOrder, CompositePoLine compositePoLine,
      JsonObject lineFromStorage) {
    PoLine storagePoLine = lineFromStorage.mapTo(PoLine.class);

    if (isEncumbranceUpdateNeeded(compOrder, compositePoLine, storagePoLine)) {
      EncumbranceWorkflowStrategy strategy = encumbranceWorkflowStrategyFactory.getStrategy(OrderWorkflowType.PENDING_TO_OPEN);
      return strategy.processEncumbrances(compOrder.withCompositePoLines(Collections.singletonList(compositePoLine)), getRequestContext());
    }
    return completedFuture(null);
  }

  private boolean isEncumbranceUpdateNeeded(CompositePurchaseOrder compOrder, CompositePoLine compositePoLine, PoLine storagePoLine) {
    List<FundDistribution> requestFundDistros = compositePoLine.getFundDistribution();
    List<FundDistribution> storageFundDistros = storagePoLine.getFundDistribution();

    if (compOrder.getWorkflowStatus() != OPEN || (requestFundDistros.size() + storageFundDistros.size()) == 0 ) {
      return false;
    }

    if (!compositePoLine.getCost().getPoLineEstimatedPrice().equals(storagePoLine.getCost().getPoLineEstimatedPrice())
      || (requestFundDistros.size() != storageFundDistros.size())) {
      return true;
    }

    return !CollectionUtils.isEqualCollection(requestFundDistros, storageFundDistros);
  }

  private CompletableFuture<Void> validateAccessProviders(CompositePoLine compOrderLine) {
    return new VendorHelper(httpClient, okapiHeaders, ctx, lang)
      .validateAccessProviders(Collections.singletonList(compOrderLine))
      .thenAccept(errors -> {
        if (!errors.getErrors().isEmpty()) {
          throw new HttpException(422, errors.getErrors().get(0));
        }
      });
  }


  private void validatePOLineProtectedFieldsChanged(CompositePoLine compOrderLine, JsonObject lineFromStorage, CompositePurchaseOrder purchaseOrder) {
    if (purchaseOrder.getWorkflowStatus() != PENDING) {
      verifyProtectedFieldsChanged(POLineProtectedFields.getFieldNames(), JsonObject.mapFrom(lineFromStorage.mapTo(PoLine.class)), JsonObject.mapFrom(compOrderLine));
    }
  }

  private void updateOrderStatus(CompositePoLine compOrderLine, JsonObject lineFromStorage) {
    FolioVertxCompletableFuture.supplyBlockingAsync(ctx, () -> lineFromStorage.mapTo(PoLine.class))
      .thenAccept(poLine -> {
        // See MODORDERS-218
        if (!StringUtils.equals(poLine.getReceiptStatus().value(), compOrderLine.getReceiptStatus().value())
          || !StringUtils.equals(poLine.getPaymentStatus().value(), compOrderLine.getPaymentStatus().value())) {
          sendEvent(MessageAddress.RECEIVE_ORDER_STATUS_UPDATE, createUpdateOrderMessage(compOrderLine));
        }
      });
  }

  private JsonObject createUpdateOrderMessage(CompositePoLine compOrderLine) {
    return new JsonObject().put(EVENT_PAYLOAD, new JsonArray().add(new JsonObject().put(ORDER_ID, compOrderLine.getPurchaseOrderId())));
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
    CompletableFuture<Void> future = new CompletableFuture<>();

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
  public CompletableFuture<JsonObject> updateOrderLineSummary(String poLineId, JsonObject poLine) {
    logger.debug("Updating PO line...");
    String endpoint = String.format(URL_WITH_LANG_PARAM, resourceByIdPath(PO_LINES, poLineId), lang);
    return operateOnObject(HttpMethod.PUT, endpoint, poLine, httpClient, okapiHeaders, logger);
  }

  /**
   * Creates Inventory records associated with given PO line and updates PO line with corresponding links.
   *
   * @param compPOL Composite PO line to update Inventory for
   * @return CompletableFuture with void.
   */
  CompletableFuture<Void> openOrderUpdateInventory(CompositePoLine compPOL, String titleId, RequestContext requestContext) {
    if (Boolean.TRUE.equals(compPOL.getIsPackage())) {
      return completedFuture(null);
    }
    if (inventoryUpdateNotRequired(compPOL)) {
      // don't create pieces, if no inventory updates and receiving not required
      if (isReceiptNotRequired(compPOL.getReceiptStatus())) {
        return completedFuture(null);
      }
      return piecesService.createPieces(compPOL, titleId, Collections.emptyList(), true, requestContext).thenRun(
          () -> logger.info("Create pieces for PO Line with '{}' id where inventory updates are not required", compPOL.getId()));
    }

    return inventoryManager.handleInstanceRecord(compPOL, requestContext)
      .thenCompose(compPOLWithInstanceId -> inventoryManager.handleHoldingsAndItemsRecords(compPOLWithInstanceId, requestContext))
      .thenCompose(piecesWithItemId -> {
        if (isReceiptNotRequired(compPOL.getReceiptStatus())) {
          return completedFuture(null);
        }
        //create pieces only if receiving is required
        return piecesService.createPieces(compPOL, titleId, piecesWithItemId, true, requestContext);
      });
  }

  CompletableFuture<Void> updateInventory(CompositePoLine compPOL, PoLine storagePoLine, String titleId, boolean isOpenOrderFlow,
                                          RequestContext requestContext) {
    if (Boolean.TRUE.equals(compPOL.getIsPackage())) {
      return completedFuture(null);
    }
    if (inventoryUpdateNotRequired(compPOL)) {
      // don't create pieces, if no inventory updates and receiving not required
      if (isReceiptNotRequired(compPOL.getReceiptStatus())) {
        return completedFuture(null);
      }
      return piecesService.createPieces(compPOL, titleId, Collections.emptyList(), isOpenOrderFlow, requestContext).thenRun(
        () -> logger.info("Create pieces for PO Line with '{}' id where inventory updates are not required", compPOL.getId()));
    }

    return inventoryManager.handleInstanceRecord(compPOL, requestContext)
      .thenCompose(compPoLineWithInstanceId -> inventoryManager.handleHoldingsAndItemsRecords(compPoLineWithInstanceId, storagePoLine, requestContext))
      .thenCompose(piecesWithItemId -> {
        if (isReceiptNotRequired(compPOL.getReceiptStatus())) {
          return completedFuture(null);
        }
        //create pieces only if receiving is required
        return updatePoLinePieces(compPOL, titleId, piecesWithItemId, isOpenOrderFlow, requestContext);
      });
  }

  private boolean isReceiptNotRequired(ReceiptStatus receiptStatus) {
    return receiptStatus == CompositePoLine.ReceiptStatus.RECEIPT_NOT_REQUIRED;
  }

  String buildNewPoLineNumber(PoLine poLineFromStorage, String poNumber) {
    String oldPoLineNumber = poLineFromStorage.getPoLineNumber();
    Matcher matcher = PO_LINE_NUMBER_PATTERN.matcher(oldPoLineNumber);
    if (matcher.find()) {
      return buildPoLineNumber(poNumber, matcher.group(2));
    }
    logger.error("PO Line - {} has invalid or missing number.", poLineFromStorage.getId());
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
  private CompletableFuture<Boolean> validateNewPoLine(CompositePoLine compPOL, RequestContext requestContext) {
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

    return allOf(validatePoLineLimit(compPOL), validateAndNormalizeISBN(compPOL, requestContext))
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

  private CompletableFuture<CompositePurchaseOrder> getCompositePurchaseOrder(String purchaseOrderId) {
    return getPurchaseOrderById(purchaseOrderId, lang, httpClient, okapiHeaders, logger)
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
    return handleGetRequest(getPoLineNumberEndpoint(compOrder.getId()), httpClient, okapiHeaders, logger)
      .thenApply(sequenceNumberJson -> {
        SequenceNumber sequenceNumber = sequenceNumberJson.mapTo(SequenceNumber.class);
        return buildPoLineNumber(compOrder.getPoNumber(), sequenceNumber.getSequenceNumber());
      });
  }

  private CompletionStage<CompositePoLine> populateCompositeLine(PoLine poline) {
    return HelperUtils.operateOnPoLine(HttpMethod.GET, JsonObject.mapFrom(poline), httpClient, okapiHeaders, logger)
      .thenCompose(this::getLineWithInstanceId);
  }

  private CompletableFuture<CompositePoLine> getLineWithInstanceId(CompositePoLine line) {
     if (!Boolean.TRUE.equals(line.getIsPackage())) {
       return titlesService.getTitles(1, 0, QUERY_BY_PO_LINE_ID + line.getId(), getRequestContext())
         .thenApply(titleCollection -> {
           List<Title> titles = titleCollection.getTitles();
           if (!titles.isEmpty()) {
             line.setInstanceId(titles.get(0).getInstanceId());
           }
           return line;
         });
    } else {
       return CompletableFuture.completedFuture(line);
     }
  }

  private String buildPoLineNumber(String poNumber, String sequence) {
    return poNumber + DASH_SEPARATOR + sequence;
  }

  /**
   * See MODORDERS-180 for more details.
   * @param compPoLine composite PO Line
   */
  public void updateEstimatedPrice(CompositePoLine compPoLine) {
    Cost cost = compPoLine.getCost();
    cost.setPoLineEstimatedPrice(calculateEstimatedPrice(cost).getNumber().doubleValue());
  }

  public void updateLocationsQuantity(List<Location> locations) {
    logger.info("updateAndGetOrderWithLines start");
    locations.forEach(location -> location.setQuantity(calculateTotalLocationQuantity(location)));
  }

  /**
   * Creates pieces that are not yet in storage
   *
   * @param compPOL PO line to create Pieces Records for
   * @param needProcessExpectedPieces expected Pieces to create with created associated Items records
   * @return void future
   */
  private CompletableFuture<Void> updatePoLinePieces(CompositePoLine compPOL, String titleId, List<Piece> needProcessExpectedPieces,
                                                     boolean isOpenOrderFlow, RequestContext requestContext) {
    // do not create pieces in case of check-in flow
    if (compPOL.getCheckinItems() != null && compPOL.getCheckinItems()) {
      return completedFuture(null);
    }
    return inventoryManager.getExpectedPiecesByLineId(compPOL.getId(), requestContext)
      .thenApply(PieceCollection::getPieces)
      .thenCompose(existingPieces -> {
          List<CompletableFuture<Void>> pieceProcessFutures = new ArrayList<>(needProcessExpectedPieces.size());
          if (!needProcessExpectedPieces.isEmpty() && !existingPieces.isEmpty() && !isOpenOrderFlow) {
            List<String> existPieceIds = existingPieces.stream().map(Piece::getId).collect(toList());
            needProcessExpectedPieces.stream()
                       .filter(piece -> existPieceIds.contains(piece.getId()))
                       .forEach(piece -> pieceProcessFutures.add(piecesService.updatePieceRecord(piece, requestContext)));
            needProcessExpectedPieces.removeIf(piece -> existPieceIds.contains(piece.getId()));
          }
          if (!needProcessExpectedPieces.isEmpty()) {
            return piecesService.createPieces(compPOL, titleId, needProcessExpectedPieces,  isOpenOrderFlow, requestContext);
          }
          return allOf(pieceProcessFutures.toArray(new CompletableFuture[0]));
      });
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

    return operateOnObject(operation, url, subObjContent, httpClient, okapiHeaders, logger)
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
    return purchaseOrderLineService.getOrderLineById(lineId, getRequestContext())
      .thenApply(JsonObject::mapFrom)
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

  public CompletableFuture<Void> validateAndNormalizeISBN(CompositePoLine compPOL, RequestContext requestContext) {
    if (HelperUtils.isProductIdsExist(compPOL)) {
      return inventoryManager.getProductTypeUuidByIsbn(ISBN, requestContext)
        .thenCompose(id -> validateIsbnValues(compPOL, id, requestContext)
          .thenAccept(aVoid -> removeISBNDuplicates(compPOL, id)));
    }
    return completedFuture(null);
  }

  CompletableFuture<Void> validateIsbnValues(CompositePoLine compPOL, String isbnTypeId, RequestContext requestContext) {
    CompletableFuture<?>[] futures = compPOL.getDetails()
      .getProductIds()
      .stream()
      .filter(productId -> isISBN(isbnTypeId, productId))
      .map(productID -> inventoryManager.convertToISBN13(productID.getProductId(), requestContext)
        .thenAccept(productID::setProductId))
      .toArray(CompletableFuture[]::new);

    return CompletableFuture.allOf(futures);
  }

  private void removeISBNDuplicates(CompositePoLine compPOL, String isbnTypeId) {

    List<ProductId> notISBNs = getNonISBNProductIds(compPOL, isbnTypeId);

    List<ProductId> isbns = getDeduplicatedISBNs(compPOL, isbnTypeId);

    isbns.addAll(notISBNs);
    compPOL.getDetails().setProductIds(isbns);
  }

  private List<ProductId> getDeduplicatedISBNs(CompositePoLine compPOL, String isbnTypeId) {
    Map<String, List<ProductId>> uniqueISBNProductIds = compPOL.getDetails().getProductIds().stream()
      .filter(productId -> isISBN(isbnTypeId, productId))
      .distinct()
      .collect(groupingBy(ProductId::getProductId));

    return uniqueISBNProductIds.values().stream()
      .flatMap(productIds -> productIds.stream()
        .filter(isUniqueISBN(productIds)))
      .collect(toList());
  }

  private Predicate<ProductId> isUniqueISBN(List<ProductId> productIds) {
    return productId -> productIds.size() == 1 || StringUtils.isNotEmpty(productId.getQualifier());
  }

  private List<ProductId> getNonISBNProductIds(CompositePoLine compPOL, String isbnTypeId) {
    return compPOL.getDetails().getProductIds().stream()
      .filter(productId -> !isISBN(isbnTypeId, productId))
      .collect(toList());
  }

  private boolean isISBN(String isbnTypeId, ProductId productId) {
    return Objects.equals(productId.getProductIdType(), isbnTypeId);
  }

  private void checkLocationCanBeModified(CompositePoLine poLine, PoLine lineFromStorage, CompositePurchaseOrder order) {
    boolean isOrderOpenAndNoNeedToManualAddPiecesForCreationAndLocationModified = order.getWorkflowStatus() == OPEN
      && Boolean.FALSE.equals(poLine.getCheckinItems())
      && !isEqualCollection(poLine.getLocations(), lineFromStorage.getLocations());

    if (isOrderOpenAndNoNeedToManualAddPiecesForCreationAndLocationModified) {
      throw new HttpException(400, LOCATION_CAN_NOT_BE_MODIFIER_AFTER_OPEN);
    }
  }

}
