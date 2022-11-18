package org.folio.helper;

import static io.vertx.core.json.JsonObject.mapFrom;
import static java.util.concurrent.CompletableFuture.allOf;
import static java.util.concurrent.CompletableFuture.completedFuture;
import static java.util.stream.Collectors.groupingBy;
import static java.util.stream.Collectors.toList;
import static org.apache.commons.collections4.CollectionUtils.isEqualCollection;
import static org.apache.commons.collections4.CollectionUtils.isNotEmpty;
import static org.apache.commons.lang3.StringUtils.isEmpty;
import static org.folio.helper.AbstractHelper.EVENT_PAYLOAD;
import static org.folio.helper.AbstractHelper.ID;
import static org.folio.helper.AbstractHelper.ORDER_ID;
import static org.folio.helper.PoNumberHelper.buildPoLineNumber;
import static org.folio.orders.utils.HelperUtils.ORDER_CONFIG_MODULE_NAME;
import static org.folio.orders.utils.HelperUtils.URL_WITH_LANG_PARAM;
import static org.folio.orders.utils.HelperUtils.calculateEstimatedPrice;
import static org.folio.orders.utils.HelperUtils.collectResultsOnSuccess;
import static org.folio.orders.utils.HelperUtils.combineCqlExpressions;
import static org.folio.orders.utils.HelperUtils.deletePoLine;
import static org.folio.orders.utils.HelperUtils.getPoLineLimit;
import static org.folio.orders.utils.HelperUtils.operateOnObject;
import static org.folio.orders.utils.HelperUtils.verifyProtectedFieldsChanged;
import static org.folio.orders.utils.ProtectedOperationType.DELETE;
import static org.folio.orders.utils.ProtectedOperationType.UPDATE;
import static org.folio.orders.utils.ResourcePathResolver.ALERTS;
import static org.folio.orders.utils.ResourcePathResolver.PO_LINES_STORAGE;
import static org.folio.orders.utils.ResourcePathResolver.PO_LINE_NUMBER;
import static org.folio.orders.utils.ResourcePathResolver.REPORTING_CODES;
import static org.folio.orders.utils.ResourcePathResolver.resourceByIdPath;
import static org.folio.orders.utils.ResourcePathResolver.resourcesPath;
import static org.folio.rest.RestConstants.EN;
import static org.folio.rest.core.exceptions.ErrorCodes.LOCATION_CAN_NOT_BE_MODIFIER_AFTER_OPEN;
import static org.folio.rest.jaxrs.model.CompositePurchaseOrder.WorkflowStatus.CLOSED;
import static org.folio.rest.jaxrs.model.CompositePurchaseOrder.WorkflowStatus.OPEN;
import static org.folio.rest.jaxrs.model.CompositePurchaseOrder.WorkflowStatus.PENDING;

import java.time.Instant;
import java.util.ArrayList;
import java.util.Collections;
import java.util.HashMap;
import java.util.Iterator;
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
import java.util.stream.Collectors;
import java.util.stream.Stream;

import javax.ws.rs.core.Response;

import org.apache.commons.collections4.CollectionUtils;
import org.apache.commons.lang3.StringUtils;
import org.apache.logging.log4j.LogManager;
import org.apache.logging.log4j.Logger;
import org.folio.completablefuture.FolioVertxCompletableFuture;
import org.folio.models.ItemStatus;
import org.folio.orders.events.handlers.MessageAddress;
import org.folio.orders.utils.HelperUtils;
import org.folio.orders.utils.POLineProtectedFieldsUtil;
import org.folio.orders.utils.PoLineCommonUtil;
import org.folio.orders.utils.ProtectedOperationType;
import org.folio.rest.RestConstants;
import org.folio.rest.acq.model.SequenceNumbers;
import org.folio.rest.core.PostResponseType;
import org.folio.rest.core.RestClient;
import org.folio.rest.core.exceptions.ErrorCodes;
import org.folio.rest.core.exceptions.HttpException;
import org.folio.rest.core.models.RequestContext;
import org.folio.rest.core.models.RequestEntry;
import org.folio.rest.jaxrs.model.Alert;
import org.folio.rest.jaxrs.model.CompositePoLine;
import org.folio.rest.jaxrs.model.CompositePoLine.OrderFormat;
import org.folio.rest.jaxrs.model.CompositePurchaseOrder;
import org.folio.rest.jaxrs.model.Cost;
import org.folio.rest.jaxrs.model.Eresource;
import org.folio.rest.jaxrs.model.Error;
import org.folio.rest.jaxrs.model.Errors;
import org.folio.rest.jaxrs.model.FundDistribution;
import org.folio.rest.jaxrs.model.Parameter;
import org.folio.rest.jaxrs.model.Physical;
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
import org.folio.service.orders.CompositePoLineValidationService;
import org.folio.service.orders.OrderInvoiceRelationService;
import org.folio.service.orders.OrderWorkflowType;
import org.folio.service.orders.PurchaseOrderLineService;
import org.folio.service.orders.PurchaseOrderStorageService;
import org.folio.service.titles.TitlesService;

import io.vertx.core.http.HttpMethod;
import io.vertx.core.json.JsonArray;
import io.vertx.core.json.JsonObject;

public class PurchaseOrderLineHelper {
  private static final Logger logger = LogManager.getLogger(PurchaseOrderLineHelper.class);

  private static final Pattern PO_LINE_NUMBER_PATTERN = Pattern.compile("([a-zA-Z0-9]{1,22}-)([0-9]{1,3})");
  private static final String PURCHASE_ORDER_ID = "purchaseOrderId";
  private static final String CREATE_INVENTORY = "createInventory";
  public static final String ERESOURCE = "eresource";
  public static final String PHYSICAL = "physical";
  private static final String OTHER = "other";
  private static final String QUERY_BY_PO_LINE_ID = "poLineId==";

  private final InventoryManager inventoryManager;
  private final EncumbranceService encumbranceService;
  private final ExpenseClassValidationService expenseClassValidationService;
  private final EncumbranceWorkflowStrategyFactory encumbranceWorkflowStrategyFactory;
  private final OrderInvoiceRelationService orderInvoiceRelationService;
  private final TitlesService titlesService;
  private final AcquisitionsUnitsService acquisitionsUnitsService;
  private final ProtectionService protectionService;
  private final PurchaseOrderLineService purchaseOrderLineService;
  private final PurchaseOrderStorageService purchaseOrderStorageService;
  private final ConfigurationEntriesService configurationEntriesService;
  private final RestClient restClient;
  private final CompositePoLineValidationService compositePoLineValidationService;

  public PurchaseOrderLineHelper(InventoryManager inventoryManager, EncumbranceService encumbranceService,
    ExpenseClassValidationService expenseClassValidationService,
    EncumbranceWorkflowStrategyFactory encumbranceWorkflowStrategyFactory, OrderInvoiceRelationService orderInvoiceRelationService,
    TitlesService titlesService, AcquisitionsUnitsService acquisitionsUnitsService, ProtectionService protectionService,
    PurchaseOrderLineService purchaseOrderLineService, PurchaseOrderStorageService purchaseOrderStorageService,
    ConfigurationEntriesService configurationEntriesService, RestClient restClient,
    CompositePoLineValidationService compositePoLineValidationService) {
    this.inventoryManager = inventoryManager;
    this.encumbranceService = encumbranceService;
    this.expenseClassValidationService = expenseClassValidationService;
    this.encumbranceWorkflowStrategyFactory = encumbranceWorkflowStrategyFactory;
    this.orderInvoiceRelationService = orderInvoiceRelationService;
    this.titlesService = titlesService;
    this.acquisitionsUnitsService = acquisitionsUnitsService;
    this.protectionService = protectionService;
    this.purchaseOrderLineService = purchaseOrderLineService;
    this.purchaseOrderStorageService = purchaseOrderStorageService;
    this.configurationEntriesService = configurationEntriesService;
    this.restClient = restClient;
    this.compositePoLineValidationService = compositePoLineValidationService;
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
  public CompletableFuture<PoLineCollection> getOrderLines(int limit, int offset, String query, RequestContext requestContext) {
    return acquisitionsUnitsService.buildAcqUnitsCqlExprToSearchRecords("purchaseOrder.", requestContext)
      .thenCompose(acqUnitsCqlExpr -> {
        String finalQuery = acqUnitsCqlExpr;
        if (!isEmpty(query)) {
          finalQuery = combineCqlExpressions("and", acqUnitsCqlExpr, query);
        }
        RequestEntry requestEntry = new RequestEntry(resourcesPath(PO_LINES_STORAGE))
                                              .withQuery(finalQuery).withLimit(limit).withOffset(offset);
        return restClient.get(requestEntry, requestContext, PoLineCollection.class);
      });
  }

  /**
   * Creates PO Line if its content is valid and all restriction checks passed
   * @param compPOL {@link CompositePoLine} to be created
   * @return completable future which might hold {@link CompositePoLine} on success, {@code null} if validation fails or an exception if any issue happens
   */
  public CompletableFuture<CompositePoLine> createPoLine(CompositePoLine compPOL, RequestContext requestContext) {
    // Validate PO Line content and retrieve order only if this operation is allowed
    JsonObject cachedTenantConfiguration= new JsonObject();
    return configurationEntriesService.loadConfiguration(ORDER_CONFIG_MODULE_NAME, requestContext)
      .thenApply(tenantConfiguration -> cachedTenantConfiguration.mergeIn(tenantConfiguration, true))
      .thenCompose(tenantConfiguration -> setTenantDefaultCreateInventoryValues(compPOL, tenantConfiguration))
      .thenCompose(v -> validateNewPoLine(compPOL, cachedTenantConfiguration, requestContext))
      .thenCompose(validationErrors -> {
        if (CollectionUtils.isEmpty(validationErrors)) {
          return getCompositePurchaseOrder(compPOL.getPurchaseOrderId(), requestContext)
            // The PO Line can be created only for order in Pending state
            .thenApply(this::validateOrderState)
            .thenCompose(po -> protectionService.isOperationRestricted(po.getAcqUnitIds(), ProtectedOperationType.CREATE, requestContext)
                                                .thenApply(vVoid -> po))
            .thenCompose(po -> createPoLine(compPOL, po, requestContext));
        } else {
            Errors errors = new Errors().withErrors(validationErrors).withTotalRecords(validationErrors.size());
            logger.error("Create POL validation error : " + JsonObject.mapFrom(errors).encodePrettily());
            throw new HttpException(RestConstants.VALIDATION_ERROR, errors);
        }
      });
  }

  /**
   * Creates PO Line assuming its content is valid and all restriction checks have been already passed
   * @param compPoLine {@link CompositePoLine} to be created
   * @param compOrder associated {@link CompositePurchaseOrder} object
   * @return completable future which might hold {@link CompositePoLine} on success or an exception if any issue happens
   */
  public CompletableFuture<CompositePoLine> createPoLine(CompositePoLine compPoLine, CompositePurchaseOrder compOrder, RequestContext requestContext) {
    // The id is required because sub-objects are being created first
    if (isEmpty(compPoLine.getId())) {
      compPoLine.setId(UUID.randomUUID().toString());
    }
    compPoLine.setPurchaseOrderId(compOrder.getId());
    updateEstimatedPrice(compPoLine);
    PoLineCommonUtil.updateLocationsQuantity(compPoLine.getLocations());

    JsonObject line = mapFrom(compPoLine);
    List<CompletableFuture<Void>> subObjFuts = new ArrayList<>();

    subObjFuts.add(createAlerts(compPoLine, line, requestContext));
    subObjFuts.add(createReportingCodes(compPoLine, line, requestContext));

    return allOf(subObjFuts.toArray(new CompletableFuture[0]))
      .thenCompose(v -> generateLineNumber(compOrder, requestContext))
      .thenAccept(lineNumber -> line.put(PO_LINE_NUMBER, lineNumber))
      .thenCompose(v -> createPoLineSummary(compPoLine, line, requestContext));
  }

  public CompletableFuture<Void> setTenantDefaultCreateInventoryValues(CompositePoLine compPOL, JsonObject tenantConfiguration) {
    CompletableFuture<JsonObject> future = new CompletableFuture<>();

    if (isCreateInventoryNull(compPOL)) {
      if (tenantConfiguration != null && !tenantConfiguration.isEmpty() &&
                      StringUtils.isNotEmpty(tenantConfiguration.getString(CREATE_INVENTORY))) {
        future.complete(new JsonObject(tenantConfiguration.getString(CREATE_INVENTORY)));
      } else {
        future.complete(new JsonObject());
      }
      return future.thenAccept(jsonConfig -> updateCreateInventory(compPOL, jsonConfig));
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

  public CompletableFuture<CompositePoLine> getCompositePoLine(String polineId, RequestContext requestContext) {
    return purchaseOrderLineService.getOrderLineById(polineId, requestContext)
      .thenCompose(line -> getCompositePurchaseOrder(line.getPurchaseOrderId(), requestContext)
        .thenCompose(order -> protectionService.isOperationRestricted(order.getAcqUnitIds(), ProtectedOperationType.READ, requestContext))
        .thenCompose(ok -> populateCompositeLine(line, requestContext)));
  }

  public CompletableFuture<Void> deleteLine(String lineId, RequestContext requestContext) {
    return purchaseOrderLineService.getOrderLineById(lineId, requestContext)
      .thenCompose(line -> verifyDeleteAllowed(line, requestContext)
        .thenCompose(ok -> {
          logger.debug("Deleting PO line...");
          return encumbranceService.deletePoLineEncumbrances(line, requestContext)
            .thenCompose(v -> deletePoLine(line, requestContext.getHeaders(), logger));
      }))
      .thenAccept(json -> logger.info("The PO Line with id='{}' has been deleted successfully", lineId));
  }

  /**
   * Handles update of the order line. First retrieve the PO line from storage and depending on its content handle passed PO line.
   */
  public CompletableFuture<Void> updateOrderLine(CompositePoLine compOrderLine, RequestContext requestContext) {
    return validateAndNormalizeISBN(compOrderLine, requestContext)
        .thenCompose(v -> getPoLineByIdAndValidate(compOrderLine.getPurchaseOrderId(), compOrderLine.getId(), requestContext))
        .thenCompose(lineFromStorage -> getCompositePurchaseOrder(compOrderLine.getPurchaseOrderId(), requestContext)
          .thenApply(compOrder -> addLineToCompOrder(compOrder, lineFromStorage))
          .thenCompose(compOrder -> {
            validatePOLineProtectedFieldsChanged(compOrderLine, lineFromStorage, compOrder);
            PoLineCommonUtil.updateLocationsQuantity(compOrderLine.getLocations());
            updateEstimatedPrice(compOrderLine);
            checkLocationCanBeModified(compOrderLine, lineFromStorage.mapTo(PoLine.class), compOrder);

            return protectionService.isOperationRestricted(compOrder.getAcqUnitIds(), UPDATE, requestContext)
                .thenCompose(v -> validateAccessProviders(compOrderLine, requestContext))
                .thenCompose(v -> expenseClassValidationService.validateExpenseClassesForOpenedOrder(compOrder, Collections.singletonList(compOrderLine), requestContext))
                .thenCompose(v -> processPoLineEncumbrances(compOrder, compOrderLine, lineFromStorage, requestContext))
                .thenApply(v -> lineFromStorage);
          }))
        .thenCompose(lineFromStorage -> {
          // override PO line number in the request with one from the storage, because it's not allowed to change it during PO line
          // update
          compOrderLine.setPoLineNumber(lineFromStorage.getString(PO_LINE_NUMBER));
          return updateOrderLine(compOrderLine, lineFromStorage, requestContext)
            .thenCompose(v -> updateEncumbranceStatus(compOrderLine, lineFromStorage, requestContext))
            .thenAccept(ok -> updateInventoryItemAndOrderStatuses(compOrderLine, lineFromStorage, requestContext));
        });

  }

  private CompletableFuture<Void> updateEncumbranceStatus(CompositePoLine compOrderLine, JsonObject lineFromStorage,
    RequestContext requestContext) {
    return FolioVertxCompletableFuture.from(requestContext.getContext(), completedFuture(lineFromStorage.mapTo(PoLine.class)))
      .thenCompose(poLine -> {
        if(isReleaseEncumbrances(compOrderLine, poLine)) {
          logger.info("Encumbrances releasing for poLineId={} where paymentStatus={}", compOrderLine.getId(), compOrderLine.getPaymentStatus());
          return encumbranceService.getPoLineUnreleasedEncumbrances(compOrderLine.getId(), requestContext)
            .thenCompose(transactionList -> encumbranceService.updateOrderTransactionSummary(compOrderLine, transactionList, requestContext)
              .thenCompose(v -> encumbranceService.releaseEncumbrances(transactionList, requestContext)));
        } else if (isUnreleasedEncumbrances(compOrderLine, poLine)) {
          logger.info("Encumbrances unreleasing for poLineId={} where paymentStatus={}", compOrderLine.getId(), compOrderLine.getPaymentStatus());
          return encumbranceService.getPoLineReleasedEncumbrances(compOrderLine, requestContext)
            .thenCompose(transactionList -> encumbranceService.updateOrderTransactionSummary(compOrderLine, transactionList, requestContext)
              .thenCompose(v -> encumbranceService.unreleaseEncumbrances(transactionList, requestContext)));
        }
        return CompletableFuture.completedFuture(null);
      });
  }

  private boolean isReleaseEncumbrances(CompositePoLine compOrderLine, PoLine poLine) {
    return !StringUtils.equals(poLine.getPaymentStatus().value(), compOrderLine.getPaymentStatus().value())
      && StringUtils.equals(compOrderLine.getPaymentStatus().value(), CompositePoLine.PaymentStatus.CANCELLED.value());
  }

  private boolean isUnreleasedEncumbrances(CompositePoLine compOrderLine, PoLine poLine) {
    return StringUtils.equals(poLine.getPaymentStatus().value(), CompositePoLine.PaymentStatus.CANCELLED.value())
      && (StringUtils.equals(compOrderLine.getPaymentStatus().value(), CompositePoLine.PaymentStatus.AWAITING_PAYMENT.value())
        || StringUtils.equals(compOrderLine.getPaymentStatus().value(), CompositePoLine.PaymentStatus.PARTIALLY_PAID.value())
        || StringUtils.equals(compOrderLine.getPaymentStatus().value(), CompositePoLine.PaymentStatus.ONGOING.value()));
  }

  /**
   * Handles update of the order line depending on the content in the storage. Returns {@link CompletableFuture} as a result.
   * In case the exception happened in future lifecycle, the caller should handle it. The logic is like following:<br/>
   * 1. Handle sub-objects operations's. All the exception happened for any sub-object are handled generating an error.
   * 2. Store PO line summary. On success, the logic checks if there are no errors happened on sub-objects operations and
   * returns succeeded future. Otherwise {@link HttpException} will be returned as result of the future.
   *
   * @param compOrderLine The composite {@link CompositePoLine} to use for storage data update
   * @param lineFromStorage {@link JsonObject} representing PO line from storage (/acq-models/mod-orders-storage/schemas/po_line.json)
   */
  public CompletableFuture<Void> updateOrderLine(CompositePoLine compOrderLine, JsonObject lineFromStorage, RequestContext requestContext) {
    CompletableFuture<Void> future = new CompletableFuture<>();

    updatePoLineSubObjects(compOrderLine, lineFromStorage, requestContext)
      .thenCompose(poLine -> updateOrderLineSummary(compOrderLine.getId(), poLine, requestContext))
      .thenAccept(json -> future.complete(null))
      .exceptionally(throwable -> {
        String message = String.format("PO Line with '%s' id partially updated but there are issues processing some PO Line sub-objects", compOrderLine.getId());
        logger.error(message);
        future.completeExceptionally(throwable);
        return null;
      });
    return future;
  }


  public CompletableFuture<JsonObject> updateOrderLineSummary(String poLineId, JsonObject poLine, RequestContext requestContext) {
    logger.debug("Updating PO line...");
    String endpoint = String.format(URL_WITH_LANG_PARAM, resourceByIdPath(PO_LINES_STORAGE, poLineId), EN);
    return operateOnObject(HttpMethod.PUT, endpoint, poLine, requestContext.getHeaders(), logger);
  }

  public String buildNewPoLineNumber(PoLine poLineFromStorage, String poNumber) {
    String oldPoLineNumber = poLineFromStorage.getPoLineNumber();
    Matcher matcher = PO_LINE_NUMBER_PATTERN.matcher(oldPoLineNumber);
    if (matcher.find()) {
      return buildPoLineNumber(poNumber, matcher.group(2));
    }
    logger.error("PO Line - {} has invalid or missing number.", poLineFromStorage.getId());
    return oldPoLineNumber;
  }

  /**
   * See MODORDERS-180 for more details.
   * @param compPoLine composite PO Line
   */
  public void updateEstimatedPrice(CompositePoLine compPoLine) {
    Cost cost = compPoLine.getCost();
    cost.setPoLineEstimatedPrice(calculateEstimatedPrice(cost).getNumber().doubleValue());
  }

  private CompletionStage<JsonObject> updatePoLineSubObjects(CompositePoLine compOrderLine, JsonObject lineFromStorage, RequestContext requestContext) {
    JsonObject updatedLineJson = mapFrom(compOrderLine);
    logger.debug("Updating PO line sub-objects...");

    List<CompletableFuture<Void>> futures = new ArrayList<>();

    futures.add(handleSubObjsOperation(ALERTS, updatedLineJson, lineFromStorage, requestContext));
    futures.add(handleSubObjsOperation(REPORTING_CODES, updatedLineJson, lineFromStorage, requestContext));

    // Once all operations completed, return updated PO Line with new sub-object id's as json object
    return allOf(futures.toArray(new CompletableFuture[0]))
                .thenApply(v -> updatedLineJson);
  }

  private CompletableFuture<String> handleSubObjOperation(String prop, JsonObject subObjContent, String storageId, RequestContext requestContext) {
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
      return completedFuture(null);
    }

    return operateOnObject(operation, url, subObjContent, requestContext.getHeaders(), logger)
      .thenApply(json -> {
        if (operation == HttpMethod.PUT) {
          return storageId;
        } else if (operation == HttpMethod.POST && json.getString(ID) != null) {
          return json.getString(ID);
        }
        return null;
      });
  }

  private CompletableFuture<Void> handleSubObjsOperation(String prop, JsonObject updatedLine, JsonObject lineFromStorage,
                                                         RequestContext requestContext) {
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

          futures.add(handleSubObjOperation(prop, subObj, id, requestContext)
            .exceptionally(throwable -> {
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

  private Error handleProcessingError(Throwable exc, String propName, String propId) {
    Error error = new Error().withMessage(exc.getMessage());
    error.getParameters()
      .add(new Parameter().withKey(propName)
        .withValue(propId));

    return error;
  }

  /**
   * Retrieves PO line from storage by PO line id as JsonObject and validates order id match.
   */
  private CompletableFuture<JsonObject> getPoLineByIdAndValidate(String orderId, String lineId, RequestContext requestContext) {
    return purchaseOrderLineService.getOrderLineById(lineId, requestContext)
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

  private CompletionStage<CompositePoLine> createPoLineSummary(CompositePoLine compPOL, JsonObject line, RequestContext requestContext) {
    RequestEntry rqEntry = new RequestEntry(resourcesPath(PO_LINES_STORAGE));
    return restClient.post(rqEntry, line, PostResponseType.BODY, PoLine.class, requestContext)
                    .thenApply(createdLine -> compPOL.withId(createdLine.getId()).withPoLineNumber(createdLine.getPoLineNumber()));
  }

  private CompletableFuture<Void> createReportingCodes(CompositePoLine compPOL, JsonObject line, RequestContext requestContext) {
    List<CompletableFuture<String>> futures = new ArrayList<>();

    List<ReportingCode> reportingCodes = compPOL.getReportingCodes();
    if (null != reportingCodes) {
      RequestEntry rqEntry = new RequestEntry(resourcesPath(REPORTING_CODES));
      reportingCodes.forEach(reportingObject -> {
        futures.add(restClient.post(rqEntry, reportingObject, requestContext, ReportingCode.class)
                              .thenApply(ReportingCode::getId));
        });
    }
    return collectResultsOnSuccess(futures)
      .thenAccept(reportingIds -> line.put(REPORTING_CODES, reportingIds))
      .exceptionally(t -> {
        logger.error("failed to create Reporting Codes", t);
        throw new CompletionException(t.getCause());
      });
  }

  private CompletableFuture<Void> createAlerts(CompositePoLine compPOL, JsonObject line, RequestContext requestContext) {
    List<CompletableFuture<String>> futures = new ArrayList<>();

    List<Alert> alerts = compPOL.getAlerts();
    if (null != alerts) {
      RequestEntry rqEntry = new RequestEntry(resourcesPath(ALERTS));
      alerts.forEach(alertObject -> {
        futures.add(restClient.post(rqEntry, alertObject, requestContext, Alert.class)
                              .thenApply(Alert::getId)
        );
      });
    }

    return collectResultsOnSuccess(futures)
      .thenAccept(ids -> line.put(ALERTS, ids))
      .exceptionally(t -> {
        logger.error("failed to create Alerts", t);
        throw new CompletionException(t.getCause());
      });

  }

  public CompletableFuture<Void> validateAndNormalizeISBN(CompositePoLine compPOL, String isbnId, Map<String, String> normalizedIsbnCache, RequestContext requestContext) {
    return validateIsbnValues(compPOL, isbnId, normalizedIsbnCache, requestContext)
      .thenAccept(aVoid -> removeISBNDuplicates(compPOL, isbnId));
  }

  public CompletableFuture<Void> validateAndNormalizeISBN(CompositePoLine compPOL, RequestContext requestContext) {
    Map<String, String> normalizedIsbnCache = new HashMap<>();
    if (HelperUtils.isProductIdsExist(compPOL)) {
      return inventoryManager.getProductTypeUuidByIsbn(requestContext)
                  .thenCompose(id -> validateIsbnValues(compPOL, id, normalizedIsbnCache, requestContext)
                  .thenAccept(aVoid -> removeISBNDuplicates(compPOL, id)));
    }
    return FolioVertxCompletableFuture.from(requestContext.getContext(), completedFuture(null));
  }

  public CompletableFuture<Void> updatePoLines(CompositePurchaseOrder poFromStorage, CompositePurchaseOrder compPO, RequestContext requestContext) {
    logger.debug("updatePoLines start");
    if (isPoLinesUpdateRequired(poFromStorage, compPO)) {
      List<PoLine> existingPoLines = HelperUtils.convertToPoLines(poFromStorage.getCompositePoLines());
      if (isNotEmpty(compPO.getCompositePoLines())) {
        // New PO Line(s) can be added only to Pending order
        if (poFromStorage.getWorkflowStatus() != PENDING && hasNewPoLines(compPO, existingPoLines)) {
          throw new HttpException(422, poFromStorage.getWorkflowStatus() == OPEN ? ErrorCodes.ORDER_OPEN : ErrorCodes.ORDER_CLOSED);
        }
        validatePOLineProtectedFieldsChangedInPO(poFromStorage, compPO, existingPoLines);
        logger.debug("updatePoLines start");
        return handlePoLines(compPO, existingPoLines, requestContext);
      } else {
        return updatePoLinesNumber(compPO, existingPoLines, requestContext);
      }
    }
    return completedFuture(null);
  }

  private void validatePOLineProtectedFieldsChangedInPO(CompositePurchaseOrder poFromStorage, CompositePurchaseOrder compPO,
    List<PoLine> existingPoLines) {
    if (poFromStorage.getWorkflowStatus() != PENDING) {
      compPO.getCompositePoLines()
        .forEach(poLine -> verifyProtectedFieldsChanged(POLineProtectedFieldsUtil.getFieldNames(poLine.getOrderFormat().value()),
          findCorrespondingCompositePoLine(poLine, existingPoLines), JsonObject.mapFrom(poLine)));
    }
  }

  private JsonObject findCorrespondingCompositePoLine(CompositePoLine poLine, List<PoLine> poLinesFromStorage) {
    return poLinesFromStorage.stream()
      .filter(line -> line.getId()
        .equals(poLine.getId()))
      .findFirst()
      .map(JsonObject::mapFrom)
      .orElse(null);
  }

  private boolean isPoLinesUpdateRequired(CompositePurchaseOrder poFromStorage, CompositePurchaseOrder compPO) {
    return isNotEmpty(compPO.getCompositePoLines()) || isPoNumberChanged(poFromStorage, compPO);
  }

  private CompletableFuture<Void> updatePoLinesNumber(CompositePurchaseOrder compOrder, List<PoLine> poLinesFromStorage,
    RequestContext requestContext) {
    CompletableFuture<?>[] futures = poLinesFromStorage
      .stream()
      .map(lineFromStorage -> {
        lineFromStorage.setPoLineNumber(buildNewPoLineNumber(lineFromStorage, compOrder.getPoNumber()));
        return purchaseOrderLineService.saveOrderLine(lineFromStorage, requestContext);
      })
      .toArray(CompletableFuture[]::new);

    return CompletableFuture.allOf(futures);
  }

  private CompletableFuture<Void> handlePoLines(CompositePurchaseOrder compOrder, List<PoLine> poLinesFromStorage, RequestContext requestContext) {
    logger.debug("handlePoLines start");
    List<CompletableFuture<?>> futures = new ArrayList<>(processPoLinesCreation(compOrder, poLinesFromStorage, requestContext));
    if (!poLinesFromStorage.isEmpty()) {
      futures.addAll(processPoLinesUpdate(compOrder, poLinesFromStorage, requestContext));
      // The remaining unprocessed PoLines should be removed
      poLinesFromStorage
        .forEach(poLine -> futures.add(orderInvoiceRelationService.checkOrderInvoiceRelationship(compOrder.getId(), requestContext)
          .thenCompose(v -> encumbranceService.deletePoLineEncumbrances(poLine, requestContext)
            .thenCompose(ok -> HelperUtils.deletePoLine(poLine, requestContext.getHeaders(), logger)))));

    }
    return CompletableFuture.allOf(futures.toArray(new CompletableFuture[0]));
  }

  private List<CompletableFuture<?>> processPoLinesUpdate(CompositePurchaseOrder compOrder, List<PoLine> poLinesFromStorage,
    RequestContext requestContext) {
    List<CompletableFuture<?>> futures = new ArrayList<>();
    Iterator<PoLine> iterator = poLinesFromStorage.iterator();
    while (iterator.hasNext()) {
      PoLine lineFromStorage = iterator.next();
      for (CompositePoLine line : compOrder.getCompositePoLines()) {
        if (StringUtils.equals(lineFromStorage.getId(), line.getId())) {
          line.setPoLineNumber(buildNewPoLineNumber(lineFromStorage, compOrder.getPoNumber()));
          PoLineCommonUtil.updateLocationsQuantity(line.getLocations());
          updateEstimatedPrice(line);

          futures.add(updateOrderLine(line, JsonObject.mapFrom(lineFromStorage), requestContext));
          iterator.remove();
          break;
        }
      }
    }
    return futures;
  }

  private List<CompletableFuture<CompositePoLine>> processPoLinesCreation(CompositePurchaseOrder compOrder, List<PoLine> poLinesFromStorage,
    RequestContext requestContext) {
    return getNewPoLines(compOrder, poLinesFromStorage)
      .map(compPOL -> createPoLine(compPOL, compOrder, requestContext))
      .collect(toList());
  }

  private boolean hasNewPoLines(CompositePurchaseOrder compPO, List<PoLine> poLinesFromStorage) {
    return getNewPoLines(compPO, poLinesFromStorage).findAny().isPresent();
  }

  private Stream<CompositePoLine> getNewPoLines(CompositePurchaseOrder compPO, List<PoLine> poLinesFromStorage) {
    List<String> lineIdsInStorage = poLinesFromStorage
      .stream()
      .map(PoLine::getId)
      .collect(toList());

    return compPO.getCompositePoLines()
      .stream()
      .filter(poLine -> !lineIdsInStorage.contains(poLine.getId()));
  }

  private boolean isPoNumberChanged(CompositePurchaseOrder poFromStorage, CompositePurchaseOrder updatedPo) {
    return !StringUtils.equalsIgnoreCase(poFromStorage.getPoNumber(), updatedPo.getPoNumber());
  }

  private CompletableFuture<Void> validateIsbnValues(CompositePoLine compPOL, String isbnTypeId,
      Map<String, String> normalizedIsbnCache, RequestContext requestContext) {
    CompletableFuture<?>[] futures = compPOL.getDetails()
      .getProductIds()
      .stream()
      .filter(productId -> isISBN(isbnTypeId, productId))
      .map(productID -> {
        if (normalizedIsbnCache.get(productID.getProductId()) != null) {
          productID.setProductId(normalizedIsbnCache.get(productID.getProductId()));
          return completedFuture(null);
        } else {
          return inventoryManager.convertToISBN13(productID.getProductId(), requestContext)
            .thenAccept(normalizedIsbn -> {
              normalizedIsbnCache.put(productID.getProductId(), normalizedIsbn);
              productID.setProductId(normalizedIsbn);
            });
        }
      })

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
      && !poLine.getSource().equals(CompositePoLine.Source.EBSCONET)
      && Boolean.FALSE.equals(poLine.getCheckinItems())
      && !isEqualCollection(poLine.getLocations(), lineFromStorage.getLocations());

    if (isOrderOpenAndNoNeedToManualAddPiecesForCreationAndLocationModified) {
      throw new HttpException(400, LOCATION_CAN_NOT_BE_MODIFIER_AFTER_OPEN);
    }
  }

  private CompositePurchaseOrder validateOrderState(CompositePurchaseOrder po) {
    CompositePurchaseOrder.WorkflowStatus poStatus = po.getWorkflowStatus();
    if (poStatus != PENDING) {
      throw new HttpException(422, poStatus == OPEN ? ErrorCodes.ORDER_OPEN : ErrorCodes.ORDER_CLOSED);
    }
    return po;
  }

  /**
   * Validates purchase order line content. If content is okay, checks if allowed PO Lines limit is not exceeded.
   * @param compPOL Purchase Order Line to validate
   * @param tenantConfiguration Tenant configuration
   * @return completable future which might be completed with {@code true} if line is valid, {@code false} if not valid or an exception if processing fails
   */
  private CompletableFuture<List<Error>> validateNewPoLine(CompositePoLine compPOL, JsonObject tenantConfiguration,
    RequestContext requestContext) {
    logger.debug("Validating if PO Line is valid...");
    List<Error> errors = new ArrayList<>();
    if (compPOL.getPurchaseOrderId() == null) {
      errors.add(ErrorCodes.MISSING_ORDER_ID_IN_POL.toError());
    }

    // If static validation has failed, no need to call other services
    if (CollectionUtils.isNotEmpty(errors)) {
      return completedFuture(errors);
    }

    return compositePoLineValidationService.validatePoLine(compPOL, requestContext)
      .thenCompose(poLineErrors -> {
      if (CollectionUtils.isEmpty(poLineErrors)) {
        return validatePoLineLimit(compPOL, tenantConfiguration, requestContext)
          .thenCompose(aErrors -> {
            if (CollectionUtils.isEmpty(aErrors)) {
              return validateAndNormalizeISBN(compPOL, requestContext)
                .thenApply(v -> errors);
            } else {
              errors.addAll(aErrors);
              return completedFuture(errors);
            }
          });
      } else {
        errors.addAll(poLineErrors);
        return completedFuture(errors);
      }
    });
  }


  private CompletableFuture<List<Error>> validatePoLineLimit(CompositePoLine compPOL, JsonObject tenantConfiguration,
    RequestContext requestContext) {
    String query = PURCHASE_ORDER_ID + "==" + compPOL.getPurchaseOrderId();
    return purchaseOrderLineService.getOrderLineCollection(query, 0, 0, requestContext)
      .thenApply(poLines -> {
        boolean isValid = poLines.getTotalRecords() < getPoLineLimit(tenantConfiguration);
        if (!isValid) {
          return List.of(ErrorCodes.POL_LINES_LIMIT_EXCEEDED.toError());
        }
        return Collections.emptyList();
      });
  }

  private CompletableFuture<CompositePurchaseOrder> getCompositePurchaseOrder(String purchaseOrderId, RequestContext requestContext) {
    return purchaseOrderStorageService.getPurchaseOrderByIdAsJson(purchaseOrderId, requestContext)
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

  private CompletableFuture<String> generateLineNumber(CompositePurchaseOrder compOrder, RequestContext requestContext) {
    RequestEntry rqEntry = new RequestEntry(resourcesPath(PO_LINE_NUMBER)).withQueryParameter(PURCHASE_ORDER_ID, compOrder.getId());
    return restClient.getAsJsonObject(rqEntry, requestContext)
      .thenApply(sequenceNumbersJson -> {
        SequenceNumbers sequenceNumbers = JsonObject.mapFrom(sequenceNumbersJson).mapTo(SequenceNumbers.class);
        return buildPoLineNumber(compOrder.getPoNumber(), sequenceNumbers.getSequenceNumbers().get(0));
      });
  }

  private CompletionStage<CompositePoLine> populateCompositeLine(PoLine poline, RequestContext requestContext) {
    return HelperUtils.operateOnPoLine(HttpMethod.GET, JsonObject.mapFrom(poline), requestContext.getHeaders(), logger)
      .thenCompose(compPol -> getLineWithInstanceId(compPol, requestContext));
  }

  private CompletableFuture<CompositePoLine> getLineWithInstanceId(CompositePoLine line, RequestContext requestContext) {
    if (!Boolean.TRUE.equals(line.getIsPackage())) {
      return titlesService.getTitles(1, 0, QUERY_BY_PO_LINE_ID + line.getId(), requestContext)
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

  private CompositePurchaseOrder addLineToCompOrder(CompositePurchaseOrder compOrder, JsonObject lineFromStorage) {
    PoLine poLine = lineFromStorage.mapTo(PoLine.class);
    poLine.setAlerts(null);
    poLine.setReportingCodes(null);
    CompositePoLine compPoLine = JsonObject.mapFrom(poLine).mapTo(CompositePoLine.class);
    compOrder.getCompositePoLines().add(compPoLine);
    return compOrder;
  }

  private CompletableFuture<Void> processPoLineEncumbrances(CompositePurchaseOrder compOrder, CompositePoLine compositePoLine,
    JsonObject lineFromStorage, RequestContext requestContext) {
    PoLine storagePoLine = lineFromStorage.mapTo(PoLine.class);

    List<String> storageFundIds = storagePoLine.getFundDistribution().stream()
      .map(FundDistribution::getFundId)
      .collect(Collectors.toList());

    compositePoLine.getFundDistribution().stream()
      .filter(fundDistribution -> storageFundIds.contains(fundDistribution.getFundId()) && fundDistribution.getEncumbrance() == null)
      .forEach(fundDistribution -> storagePoLine.getFundDistribution().stream()
          .filter(storageFundDistribution -> storageFundDistribution.getFundId().equals(fundDistribution.getFundId()))
          .findFirst()
          .ifPresent(storageFundDistribution -> fundDistribution.setEncumbrance(storageFundDistribution.getEncumbrance())));

    if (isEncumbranceUpdateNeeded(compOrder, compositePoLine, storagePoLine)) {
      OrderWorkflowType workflowType = compOrder.getWorkflowStatus() == PENDING ?
        OrderWorkflowType.PENDING_TO_PENDING : OrderWorkflowType.PENDING_TO_OPEN;
      EncumbranceWorkflowStrategy strategy = encumbranceWorkflowStrategyFactory.getStrategy(workflowType);
      CompositePurchaseOrder poFromStorage = JsonObject.mapFrom(compOrder).mapTo(CompositePurchaseOrder.class);
      return strategy.processEncumbrances(compOrder.withCompositePoLines(Collections.singletonList(compositePoLine)),
        poFromStorage, requestContext);
    }
    return completedFuture(null);
  }

  private CompletableFuture<Void> updateInventoryItemStatus(CompositePoLine compOrderLine, RequestContext requestContext) {
    return purchaseOrderLineService.getPoLinesByOrderId(compOrderLine.getPurchaseOrderId(), requestContext)
      .thenCompose(poLines -> {
        if (poLines.size() > 1) {
          List<String> poLinesId = poLines.stream().map(PoLine::getId).collect(toList());
          return inventoryManager.getItemsByPoLineIdsAndStatus(poLinesId, ItemStatus.ON_ORDER.value(), requestContext)
            .thenCompose(items -> {
              if (items.size() > 1) {
                Optional<JsonObject> poLineItem = items.stream()
                  .filter(item -> compOrderLine.getId().equals(item.getString("purchaseOrderLineIdentifier"))).findFirst();
                if (poLineItem.isPresent()) {
                  JsonObject updatedItem = updateItemStatus(poLineItem.get(), ItemStatus.ORDER_CLOSED);
                  inventoryManager.updateItem(updatedItem, requestContext);
                }
              }
              return CompletableFuture.completedFuture(null);
            });
        }
        return CompletableFuture.completedFuture(null);
      });
  }

  private JsonObject updateItemStatus(JsonObject poLineItem, ItemStatus itemStatus) {
    JsonObject status = new JsonObject();
    status.put("name", itemStatus);
    status.put("date", Instant.now());
    poLineItem.put("status", status);
    return poLineItem;
  }

  private boolean isEncumbranceUpdateNeeded(CompositePurchaseOrder compOrder, CompositePoLine compositePoLine, PoLine storagePoLine) {
    List<FundDistribution> requestFundDistros = compositePoLine.getFundDistribution();
    List<FundDistribution> storageFundDistros = storagePoLine.getFundDistribution();

    if (compOrder.getWorkflowStatus() == CLOSED || (requestFundDistros.size() + storageFundDistros.size()) == 0 ) {
      return false;
    }

    if (!compositePoLine.getCost().getPoLineEstimatedPrice().equals(storagePoLine.getCost().getPoLineEstimatedPrice())
      || !compositePoLine.getCost().getCurrency().equals(storagePoLine.getCost().getCurrency())
      || (requestFundDistros.size() != storageFundDistros.size())) {
      return true;
    }

    return !CollectionUtils.isEqualCollection(requestFundDistros, storageFundDistros);
  }

  private CompletableFuture<Void> validateAccessProviders(CompositePoLine compOrderLine, RequestContext requestContext) {
    HttpClientInterface httpClient = AbstractHelper.getHttpClient(requestContext.getHeaders());
    return new VendorHelper(httpClient, requestContext.getHeaders(), requestContext.getContext(), EN)
      .validateAccessProviders(Collections.singletonList(compOrderLine)).thenAccept(errors -> {
        httpClient.closeClient();
        if (!errors.getErrors().isEmpty()) {
          throw new HttpException(422, errors.getErrors().get(0));
        }
    }).exceptionally(t -> {
      httpClient.closeClient();
      throw new CompletionException(t.getCause());
    });
  }


  private void validatePOLineProtectedFieldsChanged(CompositePoLine compOrderLine, JsonObject lineFromStorage, CompositePurchaseOrder purchaseOrder) {
    if (purchaseOrder.getWorkflowStatus() != PENDING) {
      verifyProtectedFieldsChanged(POLineProtectedFieldsUtil.getFieldNames(compOrderLine.getOrderFormat().value()), JsonObject.mapFrom(lineFromStorage.mapTo(PoLine.class)), JsonObject.mapFrom(compOrderLine));
    }
  }

  private void updateInventoryItemAndOrderStatuses(CompositePoLine compOrderLine, JsonObject lineFromStorage, RequestContext requestContext) {
    PoLine poLine =  lineFromStorage.mapTo(PoLine.class);
   // See MODORDERS-218
    if (!StringUtils.equals(poLine.getReceiptStatus().value(), compOrderLine.getReceiptStatus().value())
          || !StringUtils.equals(poLine.getPaymentStatus().value(), compOrderLine.getPaymentStatus().value())) {
      updateInventoryItemStatus(compOrderLine, requestContext)
        .thenAccept(v -> HelperUtils.sendEvent(MessageAddress.RECEIVE_ORDER_STATUS_UPDATE, createUpdateOrderMessage(compOrderLine), requestContext));
    }
  }

  private JsonObject createUpdateOrderMessage(CompositePoLine compOrderLine) {
    return new JsonObject().put(EVENT_PAYLOAD, new JsonArray().add(new JsonObject().put(ORDER_ID, compOrderLine.getPurchaseOrderId())));
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

  private CompletableFuture<Void> verifyDeleteAllowed(PoLine line, RequestContext requestContext) {
    return orderInvoiceRelationService.checkOrderPOLineLinkedToInvoiceLine(line, requestContext)
      .thenCompose(v -> getCompositePurchaseOrder(line.getPurchaseOrderId(), requestContext)
        .thenCompose(order -> protectionService.isOperationRestricted(order.getAcqUnitIds(), DELETE, requestContext)));
  }
}
