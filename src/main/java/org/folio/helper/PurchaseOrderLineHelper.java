package org.folio.helper;

import static org.apache.commons.collections4.CollectionUtils.isEqualCollection;
import static org.apache.commons.collections4.CollectionUtils.isNotEmpty;
import static org.apache.commons.collections4.CollectionUtils.isEmpty;
import static org.folio.helper.BaseHelper.EVENT_PAYLOAD;
import static org.folio.helper.BaseHelper.ORDER_ID;
import static org.folio.orders.utils.HelperUtils.calculateEstimatedPrice;
import static org.folio.orders.utils.HelperUtils.getPoLineLimit;
import static org.folio.orders.utils.PoLineCommonUtil.convertToCompositePoLine;
import static org.folio.orders.utils.PoLineCommonUtil.convertToPoLine;
import static org.folio.orders.utils.PoLineCommonUtil.updateLocationsQuantity;
import static org.folio.orders.utils.PoLineCommonUtil.verifyProtectedFieldsChanged;
import static org.folio.orders.utils.ProtectedOperationType.DELETE;
import static org.folio.orders.utils.ProtectedOperationType.UPDATE;
import static org.folio.orders.utils.ResourcePathResolver.ALERTS;
import static org.folio.orders.utils.ResourcePathResolver.PO_LINES_STORAGE;
import static org.folio.orders.utils.ResourcePathResolver.PO_LINE_NUMBER;
import static org.folio.orders.utils.ResourcePathResolver.REPORTING_CODES;
import static org.folio.orders.utils.ResourcePathResolver.resourcesPath;
import static org.folio.rest.core.exceptions.ErrorCodes.LOCATION_CAN_NOT_BE_MODIFIER_AFTER_OPEN;
import static org.folio.rest.jaxrs.model.CompositePurchaseOrder.WorkflowStatus.CLOSED;
import static org.folio.rest.jaxrs.model.CompositePurchaseOrder.WorkflowStatus.OPEN;
import static org.folio.rest.jaxrs.model.CompositePurchaseOrder.WorkflowStatus.PENDING;
import static org.folio.service.orders.utils.StatusUtils.areAllPoLinesCanceled;
import static org.folio.service.orders.utils.StatusUtils.isStatusCanceledCompositePoLine;
import static org.folio.service.orders.utils.StatusUtils.isStatusChanged;

import java.util.ArrayList;
import java.util.Collections;
import java.util.Iterator;
import java.util.List;
import java.util.Optional;
import java.util.UUID;
import java.util.Objects;
import java.util.regex.Matcher;
import java.util.regex.Pattern;
import java.util.stream.Stream;

import javax.ws.rs.core.Response;

import io.vertx.core.json.JsonArray;
import org.apache.commons.collections4.CollectionUtils;
import org.apache.commons.lang3.StringUtils;
import org.apache.logging.log4j.LogManager;
import org.apache.logging.log4j.Logger;
import org.folio.models.ItemStatus;
import org.folio.models.PoLineInvoiceLineHolder;
import org.folio.okapi.common.GenericCompositeFuture;
import org.folio.orders.events.handlers.MessageAddress;
import org.folio.orders.utils.HelperUtils;
import org.folio.orders.utils.POLineProtectedFieldsUtil;
import org.folio.orders.utils.PoLineCommonUtil;
import org.folio.orders.utils.ProtectedOperationType;
import org.folio.rest.RestConstants;
import org.folio.rest.acq.model.SequenceNumbers;
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
import org.folio.rest.jaxrs.model.Physical;
import org.folio.rest.jaxrs.model.PoLine;
import org.folio.rest.jaxrs.model.PoLineCollection;
import org.folio.rest.jaxrs.model.ReportingCode;
import org.folio.service.ProtectionService;
import org.folio.service.finance.expenceclass.ExpenseClassValidationService;
import org.folio.service.finance.transaction.EncumbranceService;
import org.folio.service.finance.transaction.EncumbranceWorkflowStrategy;
import org.folio.service.finance.transaction.EncumbranceWorkflowStrategyFactory;
import org.folio.service.inventory.InventoryInstanceManager;
import org.folio.service.inventory.InventoryItemStatusSyncService;
import org.folio.service.orders.CompositePoLineValidationService;
import org.folio.service.orders.OrderInvoiceRelationService;
import org.folio.service.orders.OrderWorkflowType;
import org.folio.service.orders.PurchaseOrderLineService;
import org.folio.service.orders.PurchaseOrderStorageService;
import org.folio.service.organization.OrganizationService;
import org.folio.service.titles.TitlesService;

import io.vertx.core.Future;
import io.vertx.core.http.HttpMethod;
import io.vertx.core.json.JsonObject;

public class PurchaseOrderLineHelper {

  private static final Logger logger = LogManager.getLogger(PurchaseOrderLineHelper.class);

  private static final Pattern PO_LINE_NUMBER_PATTERN = Pattern.compile("([a-zA-Z0-9]{1,22}-)(\\d{1,3})");
  private static final String PURCHASE_ORDER_ID = "purchaseOrderId";
  private static final String CREATE_INVENTORY = "createInventory";
  public static final String ERESOURCE = "eresource";
  public static final String PHYSICAL = "physical";
  private static final String OTHER = "other";
  private static final String QUERY_BY_PO_LINE_ID = "poLineId==%s";

  private final InventoryItemStatusSyncService itemStatusSyncService;
  private final InventoryInstanceManager inventoryInstanceManager;
  private final EncumbranceService encumbranceService;
  private final ExpenseClassValidationService expenseClassValidationService;
  private final EncumbranceWorkflowStrategyFactory encumbranceWorkflowStrategyFactory;
  private final OrderInvoiceRelationService orderInvoiceRelationService;
  private final TitlesService titlesService;
  private final ProtectionService protectionService;
  private final PurchaseOrderLineService purchaseOrderLineService;
  private final PurchaseOrderStorageService purchaseOrderStorageService;
  private final RestClient restClient;
  private final CompositePoLineValidationService compositePoLineValidationService;
  private final OrganizationService organizationService;

  public PurchaseOrderLineHelper(InventoryItemStatusSyncService inventoryItemStatusSyncService,
                                 InventoryInstanceManager inventoryInstanceManager,
                                 EncumbranceService encumbranceService,
                                 ExpenseClassValidationService expenseClassValidationService,
                                 EncumbranceWorkflowStrategyFactory encumbranceWorkflowStrategyFactory,
                                 OrderInvoiceRelationService orderInvoiceRelationService,
                                 TitlesService titlesService,
                                 ProtectionService protectionService,
                                 PurchaseOrderLineService purchaseOrderLineService,
                                 PurchaseOrderStorageService purchaseOrderStorageService,
                                 RestClient restClient,
                                 CompositePoLineValidationService compositePoLineValidationService,
                                 OrganizationService organizationService) {

    this.itemStatusSyncService = inventoryItemStatusSyncService;
    this.inventoryInstanceManager = inventoryInstanceManager;
    this.encumbranceService = encumbranceService;
    this.expenseClassValidationService = expenseClassValidationService;
    this.encumbranceWorkflowStrategyFactory = encumbranceWorkflowStrategyFactory;
    this.orderInvoiceRelationService = orderInvoiceRelationService;
    this.titlesService = titlesService;
    this.protectionService = protectionService;
    this.purchaseOrderLineService = purchaseOrderLineService;
    this.purchaseOrderStorageService = purchaseOrderStorageService;
    this.restClient = restClient;
    this.compositePoLineValidationService = compositePoLineValidationService;
    this.organizationService = organizationService;
  }

  /**
   * This method queries a view, to limit performance implications this method
   * must be used only when there is a necessity to search/filter on the
   * Composite Purchase Order fields
   *
   * @param query A query expressed as a CQL string (see dev.folio.org/reference/glossary#cql) using valid searchable fields.
   * @param offset Skip over a number of elements by specifying an offset value for the query
   * @param limit Limit the number of elements returned in the response
   * @return Completable future which holds {@link PoLineCollection} on success or an exception on any error
   */
  public Future<PoLineCollection> getOrderLines(String query, int offset, int limit, RequestContext requestContext) {
    return protectionService.getQueryWithAcqUnitsCheck("purchaseOrder.", query, requestContext)
      .compose(finalQuery -> {
        RequestEntry requestEntry = new RequestEntry(resourcesPath(PO_LINES_STORAGE))
          .withQuery(finalQuery)
          .withOffset(offset)
          .withLimit(limit);
        return restClient.get(requestEntry, PoLineCollection.class, requestContext);
      })
      .onFailure(t -> logger.error("Error getting orderLines", t));
  }

  /**
   * Creates PO Line if its content is valid and all restriction checks passed
   * @param compPOL {@link CompositePoLine} to be created
   * @param tenantConfig tenant configuration
   * @return completable future which might hold {@link CompositePoLine} on success, {@code null} if validation fails or an exception if any issue happens
   */
  public Future<CompositePoLine> createPoLine(CompositePoLine compPOL, JsonObject tenantConfig, RequestContext requestContext) {
    // Validate PO Line content and retrieve order only if this operation is allowed
    JsonObject cachedTenantConfiguration = new JsonObject();
    cachedTenantConfiguration.mergeIn(tenantConfig, true);

    return setTenantDefaultCreateInventoryValues(compPOL, cachedTenantConfiguration)
      .compose(v -> validateNewPoLine(compPOL, cachedTenantConfiguration, requestContext))
      .compose(validationErrors -> {
        if (isEmpty(validationErrors)) {
          return getCompositePurchaseOrder(compPOL.getPurchaseOrderId(), requestContext)
            // The PO Line can be created only for order in Pending state
            .map(this::validateOrderState)
            .compose(po -> protectionService.isOperationRestricted(po.getAcqUnitIds(), ProtectedOperationType.CREATE, requestContext)
              .compose(v -> createShadowInstanceIfNeeded(compPOL, requestContext))
              .compose(v -> createPoLineWithOrder(compPOL, po, requestContext)));
        }
        var errors = new Errors().withErrors(validationErrors).withTotalRecords(validationErrors.size());
        logger.error("Create POL validation error: {}", JsonObject.mapFrom(errors).encodePrettily());
        throw new HttpException(RestConstants.VALIDATION_ERROR, errors);
      });
  }

  /**
   * Creates PO Line assuming its content is valid and all restriction checks have been already passed
   * @param compPoLine {@link CompositePoLine} to be created
   * @param compOrder associated {@link CompositePurchaseOrder} object
   * @return completable future which might hold {@link CompositePoLine} on success or an exception if any issue happens
   */
  public Future<CompositePoLine> createPoLineWithOrder(CompositePoLine compPoLine, CompositePurchaseOrder compOrder, RequestContext requestContext) {
    // The id is required because sub-objects are being created first
    if (StringUtils.isEmpty(compPoLine.getId())) {
      compPoLine.setId(UUID.randomUUID().toString());
    }
    compPoLine.setPurchaseOrderId(compOrder.getId());
    updateEstimatedPrice(compPoLine);
    updateLocationsQuantity(compPoLine.getLocations());

    var line = convertToPoLine(compPoLine);
    List<Future<Void>> subObjFuts = new ArrayList<>();
    subObjFuts.add(createAlerts(compPoLine, line, requestContext));
    subObjFuts.add(createReportingCodes(compPoLine, line, requestContext));

    return GenericCompositeFuture.join(new ArrayList<>(subObjFuts))
      .compose(v -> generateLineNumber(compOrder, requestContext))
      .map(line::withPoLineNumber)
      .compose(v -> purchaseOrderLineService.updateSearchLocations(compPoLine, requestContext))
      .map(v -> line.withSearchLocationIds(compPoLine.getSearchLocationIds()))
      .compose(v -> createPoLineSummary(compPoLine, line, requestContext));
  }

  public Future<Void> setTenantDefaultCreateInventoryValues(CompositePoLine compPOL, JsonObject tenantConfiguration) {
    if (!isCreateInventoryNull(compPOL)) {
      return Future.succeededFuture();
    }
    var jsonConfig = new JsonObject();
    if (tenantConfiguration != null && !tenantConfiguration.isEmpty() && StringUtils.isNotEmpty(tenantConfiguration.getString(CREATE_INVENTORY))) {
      jsonConfig = new JsonObject(tenantConfiguration.getString(CREATE_INVENTORY));
    }
    updateCreateInventory(compPOL, jsonConfig);
    return Future.succeededFuture();
  }

  public static boolean isCreateInventoryNull(CompositePoLine compPOL) {
    return switch (compPOL.getOrderFormat()) {
      case P_E_MIX -> isEresourceInventoryNotPresent(compPOL) || isPhysicalInventoryNotPresent(compPOL);
      case ELECTRONIC_RESOURCE -> isEresourceInventoryNotPresent(compPOL);
      case OTHER, PHYSICAL_RESOURCE -> isPhysicalInventoryNotPresent(compPOL);
    };
  }

  public Future<CompositePoLine> getCompositePoLine(String poLineId, RequestContext requestContext) {
    return purchaseOrderLineService.getOrderLineById(poLineId, requestContext)
      .compose(line -> getCompositePurchaseOrder(line.getPurchaseOrderId(), requestContext)
        .compose(order -> protectionService.isOperationRestricted(order.getAcqUnitIds(), ProtectedOperationType.READ, requestContext))
        .compose(v -> populateCompositeLine(line, requestContext)));
  }

  public Future<Void> deleteLine(String lineId, RequestContext requestContext) {
    return purchaseOrderLineService.getOrderLineById(lineId, requestContext)
      .compose(line -> verifyDeleteAllowed(line, requestContext)
        .compose(ok -> {
          logger.debug("Deleting PO line...");
          return encumbranceService.deletePoLineEncumbrances(line, requestContext)
            .compose(v -> purchaseOrderLineService.deletePoLine(line, requestContext));
        }))
      .onSuccess(json -> logger.info("deleteLine:: PoLine with id='{}' has been deleted successfully", lineId));
  }

  /**
   * Handles update of the order line. First retrieve the PO line from storage and depending on its content handle passed PO line.
   */
  public Future<Void> updateOrderLine(CompositePoLine compOrderLine, RequestContext requestContext) {
    return getPoLineByIdAndValidate(compOrderLine.getPurchaseOrderId(), compOrderLine.getId(), requestContext)
      .map(lineFromStorage -> lineFromStorage.mapTo(PoLine.class))
      .compose(poLineFromStorage -> getCompositePurchaseOrder(compOrderLine.getPurchaseOrderId(), requestContext)
        .map(compOrder -> addLineToCompOrder(compOrder, poLineFromStorage))
        .map(compOrder -> {
          validatePOLineProtectedFieldsChanged(compOrderLine, poLineFromStorage, compOrder);
          updateLocationsQuantity(compOrderLine.getLocations());
          updateEstimatedPrice(compOrderLine);
          checkLocationCanBeModified(compOrderLine, poLineFromStorage, compOrder);
          return compOrder;
        })
        .compose(compOrder -> protectionService.isOperationRestricted(compOrder.getAcqUnitIds(), UPDATE, requestContext)
          .compose(v -> validateAccessProviders(compOrderLine, requestContext))
          .compose(v -> compositePoLineValidationService.validateUserUnaffiliatedLocations(compOrderLine.getId(), compOrderLine.getLocations(), requestContext))
          .compose(v -> expenseClassValidationService.validateExpenseClassesForOpenedOrder(compOrder, Collections.singletonList(compOrderLine), requestContext))
          .compose(v -> processPoLineEncumbrances(compOrder, compOrderLine, poLineFromStorage, requestContext)))
        .map(v -> compOrderLine.withPoLineNumber(poLineFromStorage.getPoLineNumber())) // PoLine number must not be modified during PoLine update, set original value
        .map(v -> new PoLineInvoiceLineHolder(compOrderLine, poLineFromStorage))
        .compose(v -> createShadowInstanceIfNeeded(compOrderLine, requestContext))
        .compose(v -> updateOrderLine(compOrderLine, JsonObject.mapFrom(poLineFromStorage), requestContext))
        .compose(v -> updateEncumbranceStatus(compOrderLine, poLineFromStorage, requestContext))
        .compose(v -> updateInventoryItemStatus(compOrderLine, poLineFromStorage, requestContext))
        .compose(v -> updateOrderStatusIfNeeded(compOrderLine, poLineFromStorage, requestContext)));
  }

  private Future<Void> updateEncumbranceStatus(CompositePoLine compOrderLine, PoLine poLineFromStorage, RequestContext requestContext) {
    if (isReleaseEncumbrances(compOrderLine, poLineFromStorage)) {
      logger.info("updateEncumbranceStatus:: Encumbrances releasing for poLineId={} where paymentStatus={}", compOrderLine.getId(), compOrderLine.getPaymentStatus());
      return encumbranceService.getPoLineUnreleasedEncumbrances(compOrderLine, requestContext)
        .compose(transactionList -> encumbranceService.releaseEncumbrances(transactionList, requestContext));
    } else if (isUnreleasedEncumbrances(compOrderLine, poLineFromStorage)) {
      logger.info("updateEncumbranceStatus:: Encumbrances unreleasing for poLineId={} where paymentStatus={}", compOrderLine.getId(), compOrderLine.getPaymentStatus());
      return encumbranceService.getPoLineReleasedEncumbrances(compOrderLine, requestContext)
        .compose(transactionList -> encumbranceService.unreleaseEncumbrances(transactionList, requestContext));
    }
    return Future.succeededFuture();
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
   * Handles update of the order line depending on the content in the storage. Returns {@link Future} as a result.
   * In case the exception happened in future lifecycle, the caller should handle it. The logic is like following:<br/>
   * 1. Handle sub-objects operations's. All the exception happened for any sub-object are handled generating an error.
   * 2. Store PO line summary. On success, the logic checks if there are no errors happened on sub-objects operations and
   * returns succeeded future. Otherwise {@link HttpException} will be returned as result of the future.
   *
   * @param compOrderLine The composite {@link CompositePoLine} to use for storage data update
   * @param lineFromStorage {@link JsonObject} representing PO line from storage (/acq-models/mod-orders-storage/schemas/po_line.json)
   */
  public Future<Void> updateOrderLine(CompositePoLine compOrderLine, JsonObject lineFromStorage, RequestContext requestContext) {
    return purchaseOrderLineService.updateSearchLocations(compOrderLine, requestContext)
      .compose(v -> purchaseOrderLineService.updatePoLineSubObjects(compOrderLine, lineFromStorage, requestContext))
      .compose(poLine -> purchaseOrderLineService.updateOrderLineSummary(compOrderLine.getId(), poLine, requestContext))
      .onFailure(throwable -> logger.error("PoLine with id - '{}' partially updated but there are issues processing some PoLine sub-objects", compOrderLine.getId()))
      .mapEmpty();
  }

  public String buildNewPoLineNumber(PoLine poLineFromStorage, String poNumber) {
    String oldPoLineNumber = poLineFromStorage.getPoLineNumber();
    Matcher matcher = PO_LINE_NUMBER_PATTERN.matcher(oldPoLineNumber);
    if (matcher.find()) {
      return PoLineCommonUtil.buildPoLineNumber(poNumber, matcher.group(2));
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


  /**
   * Retrieves PO line from storage by PO line id as JsonObject and validates order id match.
   */
  private Future<JsonObject> getPoLineByIdAndValidate(String orderId, String lineId, RequestContext requestContext) {
    return purchaseOrderLineService.getOrderLineById(lineId, requestContext)
      .map(poLine -> {
        logger.debug("Validating if the retrieved PO line corresponds to PO");
        validateOrderId(orderId, poLine);
        return JsonObject.mapFrom(poLine);
      });
  }

  /**
   * Validates if the retrieved PO line corresponds to PO (orderId). In case the PO line does not correspond to order id the exception is thrown
   * @param orderId order identifier
   * @param poLine PO line retrieved from storage
   */
  private void validateOrderId(String orderId, PoLine poLine) {
    if (!StringUtils.equals(orderId, poLine.getPurchaseOrderId())) {
      throw new HttpException(422, ErrorCodes.INCORRECT_ORDER_ID_IN_POL);
    }
  }

  private Future<CompositePoLine> createPoLineSummary(CompositePoLine compPOL, PoLine poLine, RequestContext requestContext) {
    var requestEntry = new RequestEntry(resourcesPath(PO_LINES_STORAGE));
    return restClient.post(requestEntry, poLine, PoLine.class, requestContext)
      .map(createdLine -> compPOL.withId(createdLine.getId()).withPoLineNumber(createdLine.getPoLineNumber()));
  }

  private Future<Void> createReportingCodes(CompositePoLine compPOL, PoLine poLine, RequestContext requestContext) {
    var reportingCodes = compPOL.getReportingCodes();
    if (isEmpty(reportingCodes)) {
      return Future.succeededFuture();
    }

    var requestEntry = new RequestEntry(resourcesPath(REPORTING_CODES));
    var futures = reportingCodes.stream()
      .map(reportingObject -> restClient.post(requestEntry, reportingObject, ReportingCode.class, requestContext).map(ReportingCode::getId))
      .toList();
    return GenericCompositeFuture.join(futures)
      .map(reportingIds -> poLine.withReportingCodes(reportingIds.list()))
      .recover(t -> {
        logger.error("Failed to create reporting codes", t);
        throw new HttpException(500, "Failed to create reporting codes", t);
      })
      .mapEmpty();
  }

  private Future<Void> createAlerts(CompositePoLine compPOL, PoLine poLine, RequestContext requestContext) {
    var alerts = compPOL.getAlerts();
    if (isEmpty(alerts)) {
      return Future.succeededFuture();
    }

    var requestEntry = new RequestEntry(resourcesPath(ALERTS));
    var futures = alerts.stream()
      .map(alertObject -> restClient.post(requestEntry, alertObject, Alert.class, requestContext).map(Alert::getId))
      .toList();
    return GenericCompositeFuture.join(futures)
      .map(ids -> poLine.withAlerts(ids.list()))
      .recover(t -> {
        logger.error("Failed to create alerts", t);
        throw new HttpException(500, "Failed to create alerts", t);
      })
      .mapEmpty();
  }

  public Future<Void> updatePoLines(CompositePurchaseOrder poFromStorage, CompositePurchaseOrder compPO, RequestContext requestContext) {
    logger.debug("updatePoLines start");
    if (!isPoLinesUpdateRequired(poFromStorage, compPO)) {
      return Future.succeededFuture();
    }
    var existingPoLines = PoLineCommonUtil.convertToPoLines(poFromStorage.getCompositePoLines());
    if (isEmpty(compPO.getCompositePoLines())) {
      return updatePoLinesNumber(compPO, existingPoLines, requestContext);
    }
    // New PO Line(s) can be added only to Pending order
    if (poFromStorage.getWorkflowStatus() != PENDING && getNewPoLines(compPO, existingPoLines).findAny().isPresent()) {
      throw new HttpException(422, poFromStorage.getWorkflowStatus() == OPEN ? ErrorCodes.ORDER_OPEN : ErrorCodes.ORDER_CLOSED);
    }
    validatePoLineProtectedFieldsChangedOrder(poFromStorage, compPO, existingPoLines);
    return handlePoLines(compPO, existingPoLines, requestContext);
  }

  private void validatePoLineProtectedFieldsChangedOrder(CompositePurchaseOrder poFromStorage, CompositePurchaseOrder compPO, List<PoLine> existingPoLines) {
    if (poFromStorage.getWorkflowStatus() == PENDING) {
      return;
    }
    compPO.getCompositePoLines().forEach(poLine -> {
      var fields = POLineProtectedFieldsUtil.getFieldNames(poLine.getOrderFormat().value());
      var correspondingLine = findCorrespondingCompositePoLine(poLine, existingPoLines);
      verifyProtectedFieldsChanged(fields, correspondingLine, JsonObject.mapFrom(poLine));
    });
  }

  private JsonObject findCorrespondingCompositePoLine(CompositePoLine poLine, List<PoLine> poLinesFromStorage) {
    return poLinesFromStorage.stream()
      .filter(line -> line.getId().equals(poLine.getId()))
      .findFirst()
      .map(JsonObject::mapFrom)
      .orElse(null);
  }

  private boolean isPoLinesUpdateRequired(CompositePurchaseOrder poFromStorage, CompositePurchaseOrder compPO) {
    return isNotEmpty(compPO.getCompositePoLines()) || isPoNumberChanged(poFromStorage, compPO);
  }

  private Future<Void> updatePoLinesNumber(CompositePurchaseOrder compOrder, List<PoLine> poLinesFromStorage, RequestContext requestContext) {
    return GenericCompositeFuture.join(poLinesFromStorage.stream()
        .map(lineFromStorage -> lineFromStorage.withPoLineNumber(buildNewPoLineNumber(lineFromStorage, compOrder.getPoNumber())))
        .map(lineFromStorage -> purchaseOrderLineService.saveOrderLine(lineFromStorage, requestContext))
        .toList())
      .mapEmpty();
  }

  private Future<Void> handlePoLines(CompositePurchaseOrder compOrder, List<PoLine> poLinesFromStorage, RequestContext requestContext) {
    logger.debug("handlePoLines start");
    List<Future<?>> futures = new ArrayList<>(processPoLinesCreation(compOrder, poLinesFromStorage, requestContext));
    if (!poLinesFromStorage.isEmpty()) {
      futures.addAll(processPoLinesUpdate(compOrder, poLinesFromStorage, requestContext));
      // The remaining unprocessed PoLines should be removed
      poLinesFromStorage.forEach(poLine -> futures.add(
        orderInvoiceRelationService.checkOrderInvoiceRelationship(compOrder.getId(), requestContext)
          .compose(v -> encumbranceService.deletePoLineEncumbrances(poLine, requestContext))
          .compose(v -> purchaseOrderLineService.deletePoLine(poLine, requestContext))));
    }
    return GenericCompositeFuture.join(futures).mapEmpty();
  }

  private List<Future<?>> processPoLinesUpdate(CompositePurchaseOrder compOrder, List<PoLine> poLinesFromStorage, RequestContext requestContext) {
    List<Future<?>> futures = new ArrayList<>();
    Iterator<PoLine> iterator = poLinesFromStorage.iterator();
    while (iterator.hasNext()) {
      PoLine lineFromStorage = iterator.next();
      for (CompositePoLine line : compOrder.getCompositePoLines()) {
        if (StringUtils.equals(lineFromStorage.getId(), line.getId())) {
          line.setPoLineNumber(buildNewPoLineNumber(lineFromStorage, compOrder.getPoNumber()));
          updateLocationsQuantity(line.getLocations());
          updateEstimatedPrice(line);

          futures.add(updateOrderLine(line, JsonObject.mapFrom(lineFromStorage), requestContext));
          iterator.remove();
          break;
        }
      }
    }
    return futures;
  }

  private List<Future<CompositePoLine>> processPoLinesCreation(CompositePurchaseOrder compOrder, List<PoLine> poLinesFromStorage, RequestContext requestContext) {
    return getNewPoLines(compOrder, poLinesFromStorage)
      .map(compPOL -> createPoLineWithOrder(compPOL, compOrder, requestContext))
      .toList();
  }

  private Stream<CompositePoLine> getNewPoLines(CompositePurchaseOrder compPO, List<PoLine> poLinesFromStorage) {
    var lineIdsInStorage = poLinesFromStorage.stream().map(PoLine::getId).toList();
    return compPO.getCompositePoLines().stream()
      .filter(poLine -> !lineIdsInStorage.contains(poLine.getId()));
  }

  private boolean isPoNumberChanged(CompositePurchaseOrder poFromStorage, CompositePurchaseOrder updatedPo) {
    return !StringUtils.equalsIgnoreCase(poFromStorage.getPoNumber(), updatedPo.getPoNumber());
  }

  private void checkLocationCanBeModified(CompositePoLine poLine, PoLine lineFromStorage, CompositePurchaseOrder order) {
    if (order.getWorkflowStatus() == OPEN
      && !poLine.getSource().equals(CompositePoLine.Source.EBSCONET)
      && Boolean.FALSE.equals(poLine.getCheckinItems())
      && !isEqualCollection(poLine.getLocations(), lineFromStorage.getLocations())) {
      throw new HttpException(400, LOCATION_CAN_NOT_BE_MODIFIER_AFTER_OPEN);
    }
  }

  private CompositePurchaseOrder validateOrderState(CompositePurchaseOrder po) {
    var poStatus = po.getWorkflowStatus();
    if (poStatus == PENDING) {
      return po;
    }
    throw new HttpException(422, poStatus == OPEN ? ErrorCodes.ORDER_OPEN : ErrorCodes.ORDER_CLOSED);
  }

  /**
   * Validates purchase order line content. If content is okay, checks if allowed PO Lines limit is not exceeded.
   * @param compPOL Purchase Order Line to validate
   * @param tenantConfiguration Tenant configuration
   * @return completable future which might be completed with {@code true} if line is valid, {@code false} if not valid or an exception if processing fails
   */
  private Future<List<Error>> validateNewPoLine(CompositePoLine compPOL, JsonObject tenantConfiguration, RequestContext requestContext) {
    logger.debug("Validating if PO Line is valid...");
    List<Error> errors = new ArrayList<>();
    if (compPOL.getPurchaseOrderId() == null) {
      errors.add(ErrorCodes.MISSING_ORDER_ID_IN_POL.toError());
    }
    // If static validation has failed, no need to call other services
    if (isNotEmpty(errors)) {
      return Future.succeededFuture(errors);
    }
    return compositePoLineValidationService.validatePoLine(compPOL, requestContext)
      .compose(poLineErrors -> {
        if (isNotEmpty(poLineErrors)) {
          errors.addAll(poLineErrors);
          return Future.succeededFuture(errors);
        }
        return validatePoLineLimit(compPOL, tenantConfiguration, requestContext)
          .compose(aErrors -> {
            if (isNotEmpty(aErrors)) {
              errors.addAll(aErrors);
            }
            return Future.succeededFuture(errors);
          });
      });
  }


  private Future<List<Error>> validatePoLineLimit(CompositePoLine compPOL, JsonObject tenantConfiguration, RequestContext requestContext) {
    String query = PURCHASE_ORDER_ID + "==" + compPOL.getPurchaseOrderId();
    return purchaseOrderLineService.getOrderLineCollection(query, 0, 0, requestContext)
      .map(poLines -> {
        boolean isValid = poLines.getTotalRecords() < getPoLineLimit(tenantConfiguration);
        if (!isValid) {
          return List.of(ErrorCodes.POL_LINES_LIMIT_EXCEEDED.toError());
        }
        return Collections.emptyList();
      });
  }

  private Future<CompositePurchaseOrder> getCompositePurchaseOrder(String purchaseOrderId, RequestContext requestContext) {
    return purchaseOrderStorageService.getPurchaseOrderByIdAsJson(purchaseOrderId, requestContext)
      .map(HelperUtils::convertToCompositePurchaseOrder)
      .recover(cause -> {
        // The case when specified order does not exist
        if (cause instanceof HttpException httpException && httpException.getCode() == Response.Status.NOT_FOUND.getStatusCode()) {
          throw new HttpException(422, ErrorCodes.ORDER_NOT_FOUND);
        }
        throw new HttpException(500, cause.getMessage(), cause);
      });
  }

  private Future<String> generateLineNumber(CompositePurchaseOrder compOrder, RequestContext requestContext) {
    RequestEntry rqEntry = new RequestEntry(resourcesPath(PO_LINE_NUMBER)).withQueryParameter(PURCHASE_ORDER_ID, compOrder.getId());
    return restClient.getAsJsonObject(rqEntry, requestContext)
      .map(sequenceNumbersJson -> {
        SequenceNumbers sequenceNumbers = JsonObject.mapFrom(sequenceNumbersJson).mapTo(SequenceNumbers.class);
        return PoLineCommonUtil.buildPoLineNumber(compOrder.getPoNumber(), sequenceNumbers.getSequenceNumbers().get(0));
      });
  }

  private Future<CompositePoLine> populateCompositeLine(PoLine poline, RequestContext requestContext) {
    return purchaseOrderLineService.operateOnPoLine(HttpMethod.GET, poline, requestContext)
      .compose(compPol -> getLineWithInstanceId(compPol, requestContext));
  }

  private Future<CompositePoLine> getLineWithInstanceId(CompositePoLine line, RequestContext requestContext) {
    if (Boolean.TRUE.equals(line.getIsPackage())) {
      return Future.succeededFuture(line);
    }
    var query = String.format(QUERY_BY_PO_LINE_ID, line.getId());
    return titlesService.getTitles(1, 0, query, requestContext)
      .map(titleCollection -> {
        var titles = titleCollection.getTitles();
        if (!titles.isEmpty()) {
          line.setInstanceId(titles.get(0).getInstanceId());
        }
        return line;
      });
  }

  private CompositePurchaseOrder addLineToCompOrder(CompositePurchaseOrder compOrder, PoLine lineFromStorage) {
    var compPoLine = convertToCompositePoLine(lineFromStorage);
    compOrder.getCompositePoLines().add(compPoLine);
    return compOrder;
  }

  private Future<Void> processPoLineEncumbrances(CompositePurchaseOrder compOrder, CompositePoLine compositePoLine,
                                                 PoLine storagePoLine, RequestContext requestContext) {
    List<String> storageFundIds = storagePoLine.getFundDistribution().stream()
      .map(FundDistribution::getFundId)
      .toList();

    compositePoLine.getFundDistribution().stream()
      .filter(fundDistribution -> storageFundIds.contains(fundDistribution.getFundId()) && fundDistribution.getEncumbrance() == null)
      .forEach(fundDistribution -> storagePoLine.getFundDistribution().stream()
        .filter(storageFundDistribution -> storageFundDistribution.getFundId().equals(fundDistribution.getFundId()))
        .filter(storageFundDistribution -> Objects.equals(storageFundDistribution.getExpenseClassId(), fundDistribution.getExpenseClassId()))
        .findFirst()
        .ifPresent(storageFundDistribution -> fundDistribution.setEncumbrance(storageFundDistribution.getEncumbrance())));

    if (isEncumbranceUpdateNeeded(compOrder, compositePoLine, storagePoLine)) {
      OrderWorkflowType workflowType = compOrder.getWorkflowStatus() == PENDING ?
        OrderWorkflowType.PENDING_TO_PENDING : OrderWorkflowType.PENDING_TO_OPEN;
      EncumbranceWorkflowStrategy strategy = encumbranceWorkflowStrategyFactory.getStrategy(workflowType);
      CompositePurchaseOrder poFromStorage = JsonObject.mapFrom(compOrder).mapTo(CompositePurchaseOrder.class);
      return strategy.processEncumbrances(compOrder.withCompositePoLines(Collections.singletonList(compositePoLine)), poFromStorage, requestContext);
    }
    return Future.succeededFuture();
  }

  private Future<Void> updateInventoryItemStatus(CompositePoLine compOrderLine, PoLine lineFromStorage, RequestContext requestContext) {
    var shouldUpdateItemStatus = isStatusChanged(compOrderLine, lineFromStorage) && isStatusCanceledCompositePoLine(compOrderLine);
    if (!shouldUpdateItemStatus) {
      return Future.succeededFuture();
    }
    return purchaseOrderLineService.getPoLinesByOrderId(compOrderLine.getPurchaseOrderId(), requestContext)
      .compose(poLines -> {
        if (areAllPoLinesCanceled(poLines)) {
          logger.info("updateInventoryItemStatus:: All PoLines are canceled, returning...");
          return Future.succeededFuture();
        }
        return itemStatusSyncService.updateInventoryItemStatus(compOrderLine.getId(), compOrderLine.getLocations(),
          ItemStatus.ORDER_CLOSED, requestContext);
      });
  }

  public Future<Void> updateOrderStatusIfNeeded(CompositePoLine compOrderLine, PoLine poLineFromStorage, RequestContext requestContext) {
    // See MODORDERS-218
    if (isStatusChanged(compOrderLine, poLineFromStorage)) {
      var updateOrderMessage = JsonObject.of(EVENT_PAYLOAD, JsonArray.of(JsonObject.of(ORDER_ID, compOrderLine.getPurchaseOrderId())));
      HelperUtils.sendEvent(MessageAddress.RECEIVE_ORDER_STATUS_UPDATE, updateOrderMessage, requestContext);
    }
    return Future.succeededFuture();
  }

  private boolean isEncumbranceUpdateNeeded(CompositePurchaseOrder compOrder, CompositePoLine compositePoLine, PoLine storagePoLine) {
    List<FundDistribution> requestFundDistros = compositePoLine.getFundDistribution();
    List<FundDistribution> storageFundDistros = storagePoLine.getFundDistribution();

    if (compOrder.getWorkflowStatus() == CLOSED || (requestFundDistros.size() + storageFundDistros.size()) == 0) {
      return false;
    }

    if (!compositePoLine.getCost().getPoLineEstimatedPrice().equals(storagePoLine.getCost().getPoLineEstimatedPrice())
      || !compositePoLine.getCost().getCurrency().equals(storagePoLine.getCost().getCurrency())
      || hasAlteredExchangeRate(storagePoLine.getCost(), compositePoLine.getCost())
      || (requestFundDistros.size() != storageFundDistros.size())) {
      return true;
    }

    return !CollectionUtils.isEqualCollection(requestFundDistros, storageFundDistros);
  }

  static boolean hasAlteredExchangeRate(Cost oldCost, Cost newCost) {
    var oldExchangeRate = oldCost.getExchangeRate();
    var newExchangeRate = newCost.getExchangeRate();
    if (Objects.isNull(oldExchangeRate) && Objects.isNull(newExchangeRate)) {
      return false;
    }
    if (Objects.isNull(oldExchangeRate) || Objects.isNull(newExchangeRate)) {
      logger.info("hasAlteredExchangeRate:: Exchange rate is null in one of the costs");
      return true;
    }
    var exchangeRateIsAltered = !newExchangeRate.equals(oldExchangeRate);
    logger.info("hasAlteredExchangeRate:: Exchange rate is altered: {}", exchangeRateIsAltered);
    return exchangeRateIsAltered;
  }

  private Future<Void> validateAccessProviders(CompositePoLine compOrderLine, RequestContext requestContext) {
    return organizationService.validateAccessProviders(Collections.singletonList(compOrderLine), requestContext)
      .map(errors -> {
        if (isNotEmpty(errors.getErrors())) {
          throw new HttpException(422, errors.getErrors().get(0));
        }
        return null;
      })
      .mapEmpty();
  }

  private void validatePOLineProtectedFieldsChanged(CompositePoLine compOrderLine, PoLine poLineFromStorage, CompositePurchaseOrder purchaseOrder) {
    if (purchaseOrder.getWorkflowStatus() != PENDING) {
      verifyProtectedFieldsChanged(POLineProtectedFieldsUtil.getFieldNames(compOrderLine.getOrderFormat().value()), JsonObject.mapFrom(poLineFromStorage), JsonObject.mapFrom(compOrderLine));
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
      if (isEresourceInventoryNotPresent(compPOL)) {
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

  private Future<Void> verifyDeleteAllowed(PoLine line, RequestContext requestContext) {
    return orderInvoiceRelationService.checkOrderPOLineLinkedToInvoiceLine(line, requestContext)
      .compose(v -> getCompositePurchaseOrder(line.getPurchaseOrderId(), requestContext))
      .compose(order -> protectionService.isOperationRestricted(order.getAcqUnitIds(), DELETE, requestContext));
  }

  private Future<Void> createShadowInstanceIfNeeded(CompositePoLine compositePoLine, RequestContext requestContext) {
    String instanceId = compositePoLine.getInstanceId();
    if (Boolean.TRUE.equals(compositePoLine.getIsPackage()) || Objects.isNull(instanceId)) {
      return Future.succeededFuture();
    }
    return inventoryInstanceManager.createShadowInstanceIfNeeded(instanceId, requestContext)
      .mapEmpty();
  }

}
