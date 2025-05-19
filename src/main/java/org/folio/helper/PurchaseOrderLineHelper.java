package org.folio.helper;

import static org.apache.commons.collections4.CollectionUtils.isEqualCollection;
import static org.apache.commons.collections4.CollectionUtils.isNotEmpty;
import static org.apache.commons.collections4.CollectionUtils.isEmpty;
import static org.folio.helper.BaseHelper.EVENT_PAYLOAD;
import static org.folio.helper.BaseHelper.ORDER_ID;
import static org.folio.orders.utils.HelperUtils.calculateEstimatedPrice;
import static org.folio.orders.utils.HelperUtils.getPoLineLimit;
import static org.folio.orders.utils.PoLineCommonUtil.updateLocationsQuantity;
import static org.folio.orders.utils.PoLineCommonUtil.verifyProtectedFieldsChanged;
import static org.folio.orders.utils.ProtectedOperationType.DELETE;
import static org.folio.orders.utils.ProtectedOperationType.UPDATE;
import static org.folio.orders.utils.ResourcePathResolver.PO_LINES_STORAGE;
import static org.folio.orders.utils.ResourcePathResolver.PO_LINE_NUMBER;
import static org.folio.orders.utils.ResourcePathResolver.resourceByIdPath;
import static org.folio.orders.utils.ResourcePathResolver.resourcesPath;
import static org.folio.rest.core.exceptions.ErrorCodes.LOCATION_CAN_NOT_BE_MODIFIER_AFTER_OPEN;
import static org.folio.rest.core.exceptions.ErrorCodes.PIECES_EXIST_FOR_POLINE;
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
import org.folio.rest.jaxrs.model.PoLine;
import org.folio.rest.jaxrs.model.PoLine.OrderFormat;
import org.folio.rest.jaxrs.model.CompositePurchaseOrder;
import org.folio.rest.jaxrs.model.Cost;
import org.folio.rest.jaxrs.model.Eresource;
import org.folio.rest.jaxrs.model.Error;
import org.folio.rest.jaxrs.model.Errors;
import org.folio.rest.jaxrs.model.FundDistribution;
import org.folio.rest.jaxrs.model.Physical;
import org.folio.rest.jaxrs.model.PoLineCollection;
import org.folio.service.ProtectionService;
import org.folio.service.finance.expenceclass.ExpenseClassValidationService;
import org.folio.service.finance.transaction.EncumbranceService;
import org.folio.service.finance.transaction.EncumbranceWorkflowStrategy;
import org.folio.service.finance.transaction.EncumbranceWorkflowStrategyFactory;
import org.folio.service.inventory.InventoryInstanceManager;
import org.folio.service.inventory.InventoryItemStatusSyncService;
import org.folio.service.orders.PoLineValidationService;
import org.folio.service.orders.OrderInvoiceRelationService;
import org.folio.service.orders.OrderWorkflowType;
import org.folio.service.orders.PurchaseOrderLineService;
import org.folio.service.orders.PurchaseOrderStorageService;
import org.folio.service.organization.OrganizationService;
import org.folio.service.pieces.PieceStorageService;
import org.folio.service.titles.TitlesService;

import io.vertx.core.Future;
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
  private final PoLineValidationService poLineValidationService;
  private final OrganizationService organizationService;
  private final PieceStorageService pieceStorageService;

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
                                 PoLineValidationService poLineValidationService,
                                 OrganizationService organizationService,
                                 PieceStorageService pieceStorageService) {

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
    this.poLineValidationService = poLineValidationService;
    this.organizationService = organizationService;
    this.pieceStorageService = pieceStorageService;
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
  public Future<PoLineCollection> getOrderLines(int limit, int offset, String query, RequestContext requestContext) {
    return protectionService.getQueryWithAcqUnitsCheck("purchaseOrder.", query, requestContext)
      .compose(finalQuery -> {
        RequestEntry requestEntry = new RequestEntry(resourcesPath(PO_LINES_STORAGE))
          .withQuery(finalQuery).withLimit(limit).withOffset(offset);
        return restClient.get(requestEntry, PoLineCollection.class, requestContext);
      })
      .onFailure(t -> logger.error("Error getting orderLines", t));
  }

  /**
   * Creates PO Line if its content is valid and all restriction checks passed
   * @param poLine {@link PoLine} to be created
   * @param tenantConfig tenant configuration
   * @return completable future which might hold {@link PoLine} on success, {@code null} if validation fails or an exception if any issue happens
   */
  public Future<PoLine> createPoLine(PoLine poLine, JsonObject tenantConfig, RequestContext requestContext) {
    // Validate PO Line content and retrieve order only if this operation is allowed
    JsonObject cachedTenantConfiguration = new JsonObject();
    cachedTenantConfiguration.mergeIn(tenantConfig, true);

    return setTenantDefaultCreateInventoryValues(poLine, cachedTenantConfiguration)
      .compose(v -> validateNewPoLine(poLine, cachedTenantConfiguration, requestContext))
      .compose(validationErrors -> {
        if (isEmpty(validationErrors)) {
          return getCompositePurchaseOrder(poLine.getPurchaseOrderId(), requestContext)
            // The PO Line can be created only for order in Pending state
            .map(this::validateOrderState)
            .compose(po -> protectionService.isOperationRestricted(po.getAcqUnitIds(), ProtectedOperationType.CREATE, requestContext)
              .compose(v -> createShadowInstanceIfNeeded(poLine, requestContext))
              .compose(v -> createPoLineWithOrder(poLine, po, requestContext)));
        }
        var errors = new Errors().withErrors(validationErrors).withTotalRecords(validationErrors.size());
        logger.error("Create POL validation error: {}", JsonObject.mapFrom(errors).encodePrettily());
        throw new HttpException(RestConstants.VALIDATION_ERROR, errors);
      });
  }

  /**
   * Creates PO Line assuming its content is valid and all restriction checks have been already passed
   * @param poLine {@link PoLine} to be created
   * @param compOrder associated {@link CompositePurchaseOrder} object
   * @return completable future which might hold {@link PoLine} on success or an exception if any issue happens
   */
  public Future<PoLine> createPoLineWithOrder(PoLine poLine, CompositePurchaseOrder compOrder, RequestContext requestContext) {
    // The id is required because sub-objects are being created first
    if (StringUtils.isEmpty(poLine.getId())) {
      poLine.setId(UUID.randomUUID().toString());
    }
    poLine.setPurchaseOrderId(compOrder.getId());
    updateEstimatedPrice(poLine);
    updateLocationsQuantity(poLine.getLocations());

    return generateLineNumber(compOrder, requestContext)
      .map(poLine::withPoLineNumber)
      .compose(v -> purchaseOrderLineService.updateSearchLocations(poLine, requestContext))
      .compose(v -> createPoLineSummary(poLine, requestContext));
  }

  public Future<Void> setTenantDefaultCreateInventoryValues(PoLine poLine, JsonObject tenantConfiguration) {
    if (!isCreateInventoryNull(poLine)) {
      return Future.succeededFuture();
    }
    var jsonConfig = new JsonObject();
    if (tenantConfiguration != null && !tenantConfiguration.isEmpty() && StringUtils.isNotEmpty(tenantConfiguration.getString(CREATE_INVENTORY))) {
      jsonConfig = new JsonObject(tenantConfiguration.getString(CREATE_INVENTORY));
    }
    updateCreateInventory(poLine, jsonConfig);
    return Future.succeededFuture();
  }

  public static boolean isCreateInventoryNull(PoLine poLine) {
    return switch (poLine.getOrderFormat()) {
      case P_E_MIX -> isEresourceInventoryNotPresent(poLine) || isPhysicalInventoryNotPresent(poLine);
      case ELECTRONIC_RESOURCE -> isEresourceInventoryNotPresent(poLine);
      case OTHER, PHYSICAL_RESOURCE -> isPhysicalInventoryNotPresent(poLine);
    };
  }

  public Future<PoLine> getPoLine(String poLineId, RequestContext requestContext) {
    return purchaseOrderLineService.getOrderLineById(poLineId, requestContext)
      .compose(line -> getCompositePurchaseOrder(line.getPurchaseOrderId(), requestContext)
        .compose(order -> protectionService.isOperationRestricted(order.getAcqUnitIds(), ProtectedOperationType.READ, requestContext))
        .compose(v -> getLineWithInstanceId(line, requestContext)));
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
  public Future<Void> updateOrderLine(PoLine poLine, RequestContext requestContext) {
    return getPoLineByIdAndValidate(poLine.getPurchaseOrderId(), poLine.getId(), requestContext)
      .compose(poLineFromStorage -> getCompositePurchaseOrder(poLine.getPurchaseOrderId(), requestContext)
        .map(compOrder -> addLineToCompOrder(compOrder, poLineFromStorage))
        .compose(compOrder -> {
          validatePOLineProtectedFieldsChanged(poLine, poLineFromStorage, compOrder);
          updateLocationsQuantity(poLine.getLocations());
          updateEstimatedPrice(poLine);
          return checkLocationCanBeModified(poLine, poLineFromStorage, compOrder, requestContext)
            .map(v -> compOrder);
        })
        .compose(compOrder -> protectionService.isOperationRestricted(compOrder.getAcqUnitIds(), UPDATE, requestContext)
          .compose(v -> validateAccessProviders(poLine, requestContext))
          .compose(v -> poLineValidationService.validateUserUnaffiliatedLocations(poLine.getId(), poLine.getLocations(), requestContext))
          .compose(v -> expenseClassValidationService.validateExpenseClassesForOpenedOrder(compOrder, Collections.singletonList(poLine), requestContext))
          .compose(v -> processPoLineEncumbrances(compOrder, poLine, poLineFromStorage, requestContext)))
        .map(v -> poLine.withPoLineNumber(poLineFromStorage.getPoLineNumber())) // PoLine number must not be modified during PoLine update, set original value
        .map(v -> new PoLineInvoiceLineHolder(poLine, poLineFromStorage))
        .compose(v -> createShadowInstanceIfNeeded(poLine, requestContext))
        .compose(v -> updateOrderLineInStorage(poLine, requestContext))
        .compose(v -> updateEncumbranceStatus(poLine, poLineFromStorage, requestContext))
        .compose(v -> updateInventoryItemStatus(poLine, poLineFromStorage, requestContext))
        .compose(v -> updateOrderStatusIfNeeded(poLine, poLineFromStorage, requestContext)));
  }

  private Future<Void> updateEncumbranceStatus(PoLine compOrderLine, PoLine poLineFromStorage, RequestContext requestContext) {
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

  private boolean isReleaseEncumbrances(PoLine compOrderLine, PoLine poLine) {
    return !StringUtils.equals(poLine.getPaymentStatus().value(), compOrderLine.getPaymentStatus().value())
      && StringUtils.equals(compOrderLine.getPaymentStatus().value(), PoLine.PaymentStatus.CANCELLED.value());
  }

  private boolean isUnreleasedEncumbrances(PoLine compOrderLine, PoLine poLine) {
    return StringUtils.equals(poLine.getPaymentStatus().value(), PoLine.PaymentStatus.CANCELLED.value())
      && (StringUtils.equals(compOrderLine.getPaymentStatus().value(), PoLine.PaymentStatus.AWAITING_PAYMENT.value())
        || StringUtils.equals(compOrderLine.getPaymentStatus().value(), PoLine.PaymentStatus.PARTIALLY_PAID.value())
        || StringUtils.equals(compOrderLine.getPaymentStatus().value(), PoLine.PaymentStatus.ONGOING.value()));
  }

  public Future<Void> updateOrderLineInStorage(PoLine poLine, RequestContext requestContext) {
    return purchaseOrderLineService.updateSearchLocations(poLine, requestContext)
      .compose(v -> restClient.put(resourceByIdPath(PO_LINES_STORAGE, poLine.getId()), poLine, requestContext))
      .onFailure(t -> logger.error("Error saving poLine with id - '{}'", poLine.getId(), t));
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
   * @param poLine PO Line
   */
  public void updateEstimatedPrice(PoLine poLine) {
    Cost cost = poLine.getCost();
    cost.setPoLineEstimatedPrice(calculateEstimatedPrice(cost).getNumber().doubleValue());
  }


  /**
   * Retrieves PO line from storage by PO line id as Po Line and validates order id match.
   */
  private Future<PoLine> getPoLineByIdAndValidate(String orderId, String lineId, RequestContext requestContext) {
    return purchaseOrderLineService.getOrderLineById(lineId, requestContext)
      .map(poLine -> {
        logger.debug("Validating if the retrieved PO line corresponds to PO");
        validateOrderId(orderId, poLine);
        return poLine;
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

  private Future<PoLine> createPoLineSummary(PoLine poLine, RequestContext requestContext) {
    var requestEntry = new RequestEntry(resourcesPath(PO_LINES_STORAGE));
    return restClient.post(requestEntry, poLine, PoLine.class, requestContext);
  }

  public Future<Void> updatePoLines(CompositePurchaseOrder poFromStorage, CompositePurchaseOrder compPO, RequestContext requestContext) {
    logger.debug("updatePoLines start");
    if (!isPoLinesUpdateRequired(poFromStorage, compPO)) {
      return Future.succeededFuture();
    }
    var existingPoLines = poFromStorage.getPoLines();
    if (isEmpty(compPO.getPoLines())) {
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
    compPO.getPoLines().forEach(poLine -> {
      var fields = POLineProtectedFieldsUtil.getFieldNames(poLine.getOrderFormat().value());
      var correspondingLine = findCorrespondingCompositePoLine(poLine, existingPoLines);
      verifyProtectedFieldsChanged(fields, correspondingLine, JsonObject.mapFrom(poLine));
    });
  }

  private JsonObject findCorrespondingCompositePoLine(PoLine poLine, List<PoLine> poLinesFromStorage) {
    return poLinesFromStorage.stream()
      .filter(line -> line.getId().equals(poLine.getId()))
      .findFirst()
      .map(JsonObject::mapFrom)
      .orElse(null);
  }

  private boolean isPoLinesUpdateRequired(CompositePurchaseOrder poFromStorage, CompositePurchaseOrder compPO) {
    return isNotEmpty(compPO.getPoLines()) || isPoNumberChanged(poFromStorage, compPO);
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
      List<PoLine> linesToProcess = new ArrayList<>(poLinesFromStorage);
      futures.addAll(processPoLinesUpdate(compOrder, linesToProcess, requestContext));
      // The remaining unprocessed PoLines should be removed
      linesToProcess.forEach(poLine -> futures.add(
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
      for (PoLine line : compOrder.getPoLines()) {
        if (StringUtils.equals(lineFromStorage.getId(), line.getId())) {
          line.setPoLineNumber(buildNewPoLineNumber(lineFromStorage, compOrder.getPoNumber()));
          updateLocationsQuantity(line.getLocations());
          updateEstimatedPrice(line);

          futures.add(updateOrderLineInStorage(line, requestContext));
          iterator.remove();
          break;
        }
      }
    }
    return futures;
  }

  private List<Future<PoLine>> processPoLinesCreation(CompositePurchaseOrder compOrder, List<PoLine> poLinesFromStorage, RequestContext requestContext) {
    return getNewPoLines(compOrder, poLinesFromStorage)
      .map(poLine -> createPoLineWithOrder(poLine, compOrder, requestContext))
      .toList();
  }

  private Stream<PoLine> getNewPoLines(CompositePurchaseOrder compPO, List<PoLine> poLinesFromStorage) {
    var lineIdsInStorage = poLinesFromStorage.stream().map(PoLine::getId).toList();
    return compPO.getPoLines().stream()
      .filter(poLine -> !lineIdsInStorage.contains(poLine.getId()));
  }

  private boolean isPoNumberChanged(CompositePurchaseOrder poFromStorage, CompositePurchaseOrder updatedPo) {
    return !StringUtils.equalsIgnoreCase(poFromStorage.getPoNumber(), updatedPo.getPoNumber());
  }

  private Future<Void> checkLocationCanBeModified(PoLine poLine, PoLine lineFromStorage, CompositePurchaseOrder order, RequestContext requestContext) {
    boolean locationsChanged = !isEqualCollection(poLine.getLocations(), lineFromStorage.getLocations());
    boolean synchronizedWorkflow = Boolean.FALSE.equals(poLine.getCheckinItems());
    if (PoLine.Source.EBSCONET == poLine.getSource()) {;
      logger.info("checkLocationCanBeModified:: skip validation for Ebsconet orders");
      return Future.succeededFuture();
    }

    if (order.getWorkflowStatus() == OPEN
      && synchronizedWorkflow
      && locationsChanged) {
      return Future.failedFuture(new HttpException(400, LOCATION_CAN_NOT_BE_MODIFIER_AFTER_OPEN));
    }

    if (synchronizedWorkflow && locationsChanged) {
      return pieceStorageService.getPiecesByLineId(poLine.getId(), requestContext)
        .compose(pieces -> CollectionUtils.isNotEmpty(pieces)
          ? Future.failedFuture(new HttpException(400, PIECES_EXIST_FOR_POLINE))
          : Future.succeededFuture());
    }

    return Future.succeededFuture();
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
   * @param poLine Purchase Order Line to validate
   * @param tenantConfiguration Tenant configuration
   * @return completable future which might be completed with {@code true} if line is valid, {@code false} if not valid or an exception if processing fails
   */
  private Future<List<Error>> validateNewPoLine(PoLine poLine, JsonObject tenantConfiguration, RequestContext requestContext) {
    logger.debug("Validating if PO Line is valid...");
    List<Error> errors = new ArrayList<>();
    if (poLine.getPurchaseOrderId() == null) {
      errors.add(ErrorCodes.MISSING_ORDER_ID_IN_POL.toError());
    }
    // If static validation has failed, no need to call other services
    if (isNotEmpty(errors)) {
      return Future.succeededFuture(errors);
    }
    return poLineValidationService.validatePoLine(poLine, requestContext)
      .compose(poLineErrors -> {
        if (isNotEmpty(poLineErrors)) {
          errors.addAll(poLineErrors);
          return Future.succeededFuture(errors);
        }
        return validatePoLineLimit(poLine, tenantConfiguration, requestContext)
          .compose(aErrors -> {
            if (isNotEmpty(aErrors)) {
              errors.addAll(aErrors);
            }
            return Future.succeededFuture(errors);
          });
      });
  }


  private Future<List<Error>> validatePoLineLimit(PoLine poLine, JsonObject tenantConfiguration, RequestContext requestContext) {
    String query = PURCHASE_ORDER_ID + "==" + poLine.getPurchaseOrderId();
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

  private Future<PoLine> getLineWithInstanceId(PoLine line, RequestContext requestContext) {
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
    compOrder.getPoLines().add(lineFromStorage);
    return compOrder;
  }

  private Future<Void> processPoLineEncumbrances(CompositePurchaseOrder compOrder, PoLine poLine,
                                                 PoLine storagePoLine, RequestContext requestContext) {
    List<String> storageFundIds = storagePoLine.getFundDistribution().stream()
      .map(FundDistribution::getFundId)
      .toList();

    poLine.getFundDistribution().stream()
      .filter(fundDistribution -> storageFundIds.contains(fundDistribution.getFundId()) && fundDistribution.getEncumbrance() == null)
      .forEach(fundDistribution -> storagePoLine.getFundDistribution().stream()
        .filter(storageFundDistribution -> storageFundDistribution.getFundId().equals(fundDistribution.getFundId()))
        .filter(storageFundDistribution -> Objects.equals(storageFundDistribution.getExpenseClassId(), fundDistribution.getExpenseClassId()))
        .findFirst()
        .ifPresent(storageFundDistribution -> fundDistribution.setEncumbrance(storageFundDistribution.getEncumbrance())));

    if (isEncumbranceUpdateNeeded(compOrder, poLine, storagePoLine)) {
      OrderWorkflowType workflowType = compOrder.getWorkflowStatus() == PENDING ?
        OrderWorkflowType.PENDING_TO_PENDING : OrderWorkflowType.PENDING_TO_OPEN;
      EncumbranceWorkflowStrategy strategy = encumbranceWorkflowStrategyFactory.getStrategy(workflowType);
      CompositePurchaseOrder poFromStorage = JsonObject.mapFrom(compOrder).mapTo(CompositePurchaseOrder.class);
      return strategy.processEncumbrances(compOrder.withPoLines(Collections.singletonList(poLine)), poFromStorage, requestContext);
    }
    return Future.succeededFuture();
  }

  private Future<Void> updateInventoryItemStatus(PoLine compOrderLine, PoLine lineFromStorage, RequestContext requestContext) {
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

  public Future<Void> updateOrderStatusIfNeeded(PoLine compOrderLine, PoLine poLineFromStorage, RequestContext requestContext) {
    // See MODORDERS-218
    if (isStatusChanged(compOrderLine, poLineFromStorage)) {
      var updateOrderMessage = JsonObject.of(EVENT_PAYLOAD, JsonArray.of(JsonObject.of(ORDER_ID, compOrderLine.getPurchaseOrderId())));
      HelperUtils.sendEvent(MessageAddress.RECEIVE_ORDER_STATUS_UPDATE, updateOrderMessage, requestContext);
    }
    return Future.succeededFuture();
  }

  private boolean isEncumbranceUpdateNeeded(CompositePurchaseOrder compOrder, PoLine poLine, PoLine storagePoLine) {
    List<FundDistribution> requestFundDistros = poLine.getFundDistribution();
    List<FundDistribution> storageFundDistros = storagePoLine.getFundDistribution();

    if (compOrder.getWorkflowStatus() == CLOSED || (requestFundDistros.size() + storageFundDistros.size()) == 0) {
      return false;
    }

    if (!poLine.getCost().getPoLineEstimatedPrice().equals(storagePoLine.getCost().getPoLineEstimatedPrice())
      || !poLine.getCost().getCurrency().equals(storagePoLine.getCost().getCurrency())
      || hasAlteredExchangeRate(storagePoLine.getCost(), poLine.getCost())
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

  private Future<Void> validateAccessProviders(PoLine compOrderLine, RequestContext requestContext) {
    return organizationService.validateAccessProviders(Collections.singletonList(compOrderLine), requestContext)
      .map(errors -> {
        if (isNotEmpty(errors.getErrors())) {
          throw new HttpException(422, errors.getErrors().get(0));
        }
        return null;
      })
      .mapEmpty();
  }

  private void validatePOLineProtectedFieldsChanged(PoLine compOrderLine, PoLine poLineFromStorage, CompositePurchaseOrder purchaseOrder) {
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

  private static boolean isPhysicalInventoryNotPresent(PoLine poLine) {
    return Optional.ofNullable(poLine.getPhysical())
      .map(physical -> physical.getCreateInventory() == null)
      .orElse(true);
  }

  private static boolean isEresourceInventoryNotPresent(PoLine poLine) {
    return Optional.ofNullable(poLine.getEresource())
      .map(eresource -> eresource.getCreateInventory() == null)
      .orElse(true);
  }

  /**
   * get the tenant configuration for the orderFormat, if not present assign the defaults
   * Default values:
   * Physical : CreateInventory.INSTANCE_HOLDING_ITEM
   * Eresource: CreateInventory.INSTANCE_HOLDING
   *
   * @param poLine composite purchase order
   * @param jsonConfig createInventory configuration
   */
  private void updateCreateInventory(PoLine poLine, JsonObject jsonConfig) {
    // try to set createInventory by values from mod-configuration. If empty -
    // set default hardcoded values
    if (poLine.getOrderFormat().equals(OrderFormat.ELECTRONIC_RESOURCE)
      || poLine.getOrderFormat().equals(OrderFormat.P_E_MIX)) {
      String tenantDefault = jsonConfig.getString(ERESOURCE);
      Eresource.CreateInventory eresourceDefaultValue = getEresourceInventoryDefault(tenantDefault);
      if (poLine.getEresource() == null) {
        poLine.setEresource(new Eresource());
      }
      if (isEresourceInventoryNotPresent(poLine)) {
        poLine.getEresource().setCreateInventory(eresourceDefaultValue);
      }
    }
    if (!poLine.getOrderFormat().equals(OrderFormat.ELECTRONIC_RESOURCE)) {
      String tenantDefault = poLine.getOrderFormat().equals(OrderFormat.OTHER) ? jsonConfig.getString(OTHER)
        : jsonConfig.getString(PHYSICAL);
      Physical.CreateInventory createInventoryDefaultValue = getPhysicalInventoryDefault(tenantDefault);
      if (poLine.getPhysical() == null) {
        poLine.setPhysical(new Physical());
      }
      if (isPhysicalInventoryNotPresent(poLine)) {
        poLine.getPhysical().setCreateInventory(createInventoryDefaultValue);
      }
    }
  }

  private Future<Void> verifyDeleteAllowed(PoLine line, RequestContext requestContext) {
    return orderInvoiceRelationService.checkOrderPOLineLinkedToInvoiceLine(line, requestContext)
      .compose(v -> getCompositePurchaseOrder(line.getPurchaseOrderId(), requestContext))
      .compose(order -> protectionService.isOperationRestricted(order.getAcqUnitIds(), DELETE, requestContext));
  }

  private Future<Void> createShadowInstanceIfNeeded(PoLine poLine, RequestContext requestContext) {
    String instanceId = poLine.getInstanceId();
    if (Boolean.TRUE.equals(poLine.getIsPackage()) || Objects.isNull(instanceId)) {
      return Future.succeededFuture();
    }
    return inventoryInstanceManager.createShadowInstanceIfNeeded(instanceId, requestContext)
      .mapEmpty();
  }

}
