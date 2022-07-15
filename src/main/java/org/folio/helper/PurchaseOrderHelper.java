package org.folio.helper;

import static java.util.concurrent.CompletableFuture.completedFuture;
import static java.util.stream.Collectors.toList;
import static org.apache.commons.collections4.CollectionUtils.isEmpty;
import static org.apache.commons.collections4.CollectionUtils.isNotEmpty;
import static org.folio.helper.AbstractHelper.MAX_REPEAT_ON_FAILURE;
import static org.folio.helper.AbstractHelper.SEARCH_PARAMS;
import static org.folio.orders.utils.AcqDesiredPermissions.ASSIGN;
import static org.folio.orders.utils.AcqDesiredPermissions.MANAGE;
import static org.folio.orders.utils.HelperUtils.COMPOSITE_PO_LINES;
import static org.folio.orders.utils.HelperUtils.ORDER_CONFIG_MODULE_NAME;
import static org.folio.orders.utils.HelperUtils.REASON_CANCELLED;
import static org.folio.orders.utils.HelperUtils.WORKFLOW_STATUS;
import static org.folio.orders.utils.HelperUtils.buildQuery;
import static org.folio.orders.utils.HelperUtils.changeOrderStatus;
import static org.folio.orders.utils.HelperUtils.collectResultsOnSuccess;
import static org.folio.orders.utils.HelperUtils.combineCqlExpressions;
import static org.folio.orders.utils.HelperUtils.convertToCompositePurchaseOrder;
import static org.folio.orders.utils.HelperUtils.getPoLineLimit;
import static org.folio.orders.utils.HelperUtils.handleGetRequest;
import static org.folio.orders.utils.HelperUtils.verifyProtectedFieldsChanged;
import static org.folio.orders.utils.OrderStatusTransitionUtil.isOrderClosing;
import static org.folio.orders.utils.OrderStatusTransitionUtil.isOrderReopening;
import static org.folio.orders.utils.OrderStatusTransitionUtil.isTransitionToApproved;
import static org.folio.orders.utils.OrderStatusTransitionUtil.isTransitionToClosed;
import static org.folio.orders.utils.OrderStatusTransitionUtil.isTransitionToOpen;
import static org.folio.orders.utils.OrderStatusTransitionUtil.isTransitionToPending;
import static org.folio.orders.utils.OrderStatusTransitionUtil.isTransitionToReopen;
import static org.folio.orders.utils.POProtectedFields.getFieldNames;
import static org.folio.orders.utils.POProtectedFields.getFieldNamesForOpenOrder;
import static org.folio.orders.utils.ProtectedOperationType.CREATE;
import static org.folio.orders.utils.ProtectedOperationType.DELETE;
import static org.folio.orders.utils.ProtectedOperationType.UPDATE;
import static org.folio.orders.utils.ResourcePathResolver.PO_LINE_NUMBER;
import static org.folio.orders.utils.ResourcePathResolver.PURCHASE_ORDER_STORAGE;
import static org.folio.orders.utils.ResourcePathResolver.resourcesPath;
import static org.folio.rest.RestConstants.EN;
import static org.folio.rest.RestConstants.MAX_IDS_FOR_GET_RQ;
import static org.folio.rest.RestVerticle.OKAPI_USERID_HEADER;
import static org.folio.rest.core.exceptions.ErrorCodes.APPROVAL_REQUIRED_TO_OPEN;
import static org.folio.rest.core.exceptions.ErrorCodes.MISSING_ONGOING;
import static org.folio.rest.core.exceptions.ErrorCodes.ONGOING_NOT_ALLOWED;
import static org.folio.rest.core.exceptions.ErrorCodes.USER_HAS_NO_ACQ_PERMISSIONS;
import static org.folio.rest.core.exceptions.ErrorCodes.USER_HAS_NO_APPROVAL_PERMISSIONS;
import static org.folio.rest.core.exceptions.ErrorCodes.USER_HAS_NO_REOPEN_PERMISSIONS;
import static org.folio.rest.core.exceptions.ErrorCodes.USER_HAS_NO_UNOPEN_PERMISSIONS;
import static org.folio.rest.jaxrs.model.CompositePurchaseOrder.WorkflowStatus.OPEN;
import static org.folio.rest.jaxrs.model.CompositePurchaseOrder.WorkflowStatus.PENDING;

import java.util.ArrayList;
import java.util.Collection;
import java.util.Collections;
import java.util.Date;
import java.util.HashMap;
import java.util.HashSet;
import java.util.List;
import java.util.Map;
import java.util.Objects;
import java.util.Optional;
import java.util.Set;
import java.util.concurrent.CompletableFuture;
import java.util.concurrent.CompletionStage;
import java.util.stream.Collectors;

import org.apache.commons.collections4.CollectionUtils;
import org.apache.commons.collections4.ListUtils;
import org.apache.commons.lang3.StringUtils;
import org.apache.logging.log4j.LogManager;
import org.apache.logging.log4j.Logger;
import org.folio.HttpStatus;
import org.folio.completablefuture.CompletableFutureRepeater;
import org.folio.completablefuture.FolioVertxCompletableFuture;
import org.folio.models.CompositeOrderRetrieveHolder;
import org.folio.orders.utils.AcqDesiredPermissions;
import org.folio.orders.utils.HelperUtils;
import org.folio.orders.utils.PoLineCommonUtil;
import org.folio.orders.utils.ProtectedOperationType;
import org.folio.rest.core.exceptions.ErrorCodes;
import org.folio.rest.core.exceptions.HttpException;
import org.folio.rest.core.models.RequestContext;
import org.folio.rest.jaxrs.model.CompositePoLine;
import org.folio.rest.jaxrs.model.CompositePoLine.PaymentStatus;
import org.folio.rest.jaxrs.model.CompositePoLine.ReceiptStatus;
import org.folio.rest.jaxrs.model.CompositePurchaseOrder;
import org.folio.rest.jaxrs.model.CompositePurchaseOrder.WorkflowStatus;
import org.folio.rest.jaxrs.model.Error;
import org.folio.rest.jaxrs.model.Parameter;
import org.folio.rest.jaxrs.model.PoLine;
import org.folio.rest.jaxrs.model.PurchaseOrder;
import org.folio.rest.jaxrs.model.PurchaseOrderCollection;
import org.folio.rest.jaxrs.model.Title;
import org.folio.rest.tools.client.interfaces.HttpClientInterface;
import org.folio.service.AcquisitionsUnitsService;
import org.folio.service.ProtectionService;
import org.folio.service.TagService;
import org.folio.service.configuration.ConfigurationEntriesService;
import org.folio.service.finance.transaction.EncumbranceService;
import org.folio.service.finance.transaction.EncumbranceWorkflowStrategy;
import org.folio.service.finance.transaction.EncumbranceWorkflowStrategyFactory;
import org.folio.service.inventory.InventoryManager;
import org.folio.service.orders.CompositeOrderDynamicDataPopulateService;
import org.folio.service.orders.CompositePoLineValidationService;
import org.folio.service.orders.OrderInvoiceRelationService;
import org.folio.service.orders.OrderWorkflowType;
import org.folio.service.orders.PurchaseOrderLineService;
import org.folio.service.orders.PurchaseOrderStorageService;
import org.folio.service.orders.flows.update.open.OpenCompositeOrderFlowValidator;
import org.folio.service.orders.flows.update.open.OpenCompositeOrderManager;
import org.folio.service.orders.flows.update.reopen.ReOpenCompositeOrderManager;
import org.folio.service.orders.flows.update.unopen.UnOpenCompositeOrderManager;
import org.folio.service.titles.TitlesService;

import io.vertx.core.json.JsonArray;
import io.vertx.core.json.JsonObject;
import one.util.streamex.StreamEx;

public class PurchaseOrderHelper {
  private static final Logger logger = LogManager.getLogger(PurchaseOrderHelper.class);

  private static final String PERMISSION_ORDER_APPROVE = "orders.item.approve";
  private static final String PERMISSION_ORDER_UNOPEN = "orders.item.unopen";
  private static final String PERMISSION_ORDER_REOPEN = "orders.item.reopen";
  private static final String SEARCH_ORDERS_BY_LINES_DATA = resourcesPath(PURCHASE_ORDER_STORAGE) + SEARCH_PARAMS;
  public static final String GET_PURCHASE_ORDERS = resourcesPath(PURCHASE_ORDER_STORAGE) + SEARCH_PARAMS;
  public static final String EMPTY_ARRAY = "[]";
  public static final String OKAPI_HEADER_PERMISSIONS = "X-Okapi-Permissions";


  private final PurchaseOrderLineHelper purchaseOrderLineHelper;
  private final CompositeOrderDynamicDataPopulateService orderLinesSummaryPopulateService;
  private final EncumbranceService encumbranceService;
  private final CompositeOrderDynamicDataPopulateService combinedPopulateService;
  private final EncumbranceWorkflowStrategyFactory encumbranceWorkflowStrategyFactory;
  private final OrderInvoiceRelationService orderInvoiceRelationService;
  private final TagService tagService;
  private final PurchaseOrderLineService purchaseOrderLineService;
  private final TitlesService titlesService;
  private final AcquisitionsUnitsService acquisitionsUnitsService;
  private final ProtectionService protectionService;
  private final InventoryManager inventoryManager;
  private final UnOpenCompositeOrderManager unOpenCompositeOrderManager;
  private final OpenCompositeOrderManager openCompositeOrderManager;
  private final OpenCompositeOrderFlowValidator openCompositeOrderFlowValidator;
  private final PurchaseOrderStorageService purchaseOrderStorageService;
  private final ConfigurationEntriesService configurationEntriesService;
  private final PoNumberHelper poNumberHelper;
  private final CompositePoLineValidationService compositePoLineValidationService;
  private final ReOpenCompositeOrderManager reOpenCompositeOrderManager;

  public PurchaseOrderHelper(PurchaseOrderLineHelper purchaseOrderLineHelper, CompositeOrderDynamicDataPopulateService orderLinesSummaryPopulateService, EncumbranceService encumbranceService,
    CompositeOrderDynamicDataPopulateService combinedPopulateService, EncumbranceWorkflowStrategyFactory encumbranceWorkflowStrategyFactory, OrderInvoiceRelationService orderInvoiceRelationService,
    TagService tagService, PurchaseOrderLineService purchaseOrderLineService, TitlesService titlesService, AcquisitionsUnitsService acquisitionsUnitsService, ProtectionService protectionService, InventoryManager inventoryManager,
    UnOpenCompositeOrderManager unOpenCompositeOrderManager, OpenCompositeOrderManager openCompositeOrderManager,
    PurchaseOrderStorageService purchaseOrderStorageService, ConfigurationEntriesService configurationEntriesService, PoNumberHelper poNumberHelper,
    OpenCompositeOrderFlowValidator openCompositeOrderFlowValidator,
    CompositePoLineValidationService compositePoLineValidationService, ReOpenCompositeOrderManager reOpenCompositeOrderManager) {
    this.purchaseOrderLineHelper = purchaseOrderLineHelper;
    this.orderLinesSummaryPopulateService = orderLinesSummaryPopulateService;
    this.encumbranceService = encumbranceService;
    this.combinedPopulateService = combinedPopulateService;
    this.encumbranceWorkflowStrategyFactory = encumbranceWorkflowStrategyFactory;
    this.orderInvoiceRelationService = orderInvoiceRelationService;
    this.tagService = tagService;
    this.purchaseOrderLineService = purchaseOrderLineService;
    this.titlesService = titlesService;
    this.acquisitionsUnitsService = acquisitionsUnitsService;
    this.protectionService = protectionService;
    this.inventoryManager = inventoryManager;
    this.unOpenCompositeOrderManager = unOpenCompositeOrderManager;
    this.openCompositeOrderManager = openCompositeOrderManager;
    this.purchaseOrderStorageService = purchaseOrderStorageService;
    this.configurationEntriesService = configurationEntriesService;
    this.poNumberHelper = poNumberHelper;
    this.openCompositeOrderFlowValidator = openCompositeOrderFlowValidator;
    this.compositePoLineValidationService = compositePoLineValidationService;
    this.reOpenCompositeOrderManager = reOpenCompositeOrderManager;
  }

  /**
   * Retrieve a list of {@link PurchaseOrder} objects retrieved from storage by provided query.
   *
   * @param limit limit the number of elements returned in the response
   * @param offset skip over a number of elements by specifying an offset value for the query
   * @param query A query expressed as a CQL string using valid searchable fields.
   * @return completable future with {@link PurchaseOrderCollection} object on success or an exception if processing fails
   */
  public CompletableFuture<PurchaseOrderCollection> getPurchaseOrders(int limit, int offset, String query, RequestContext requestContext) {
    CompletableFuture<PurchaseOrderCollection> future = new CompletableFuture<>();

    try {
      HttpClientInterface httpClient = AbstractHelper.getHttpClient(requestContext.getHeaders(), true);
      buildGetOrdersPath(limit, offset, query, requestContext)
        .thenCompose(endpoint -> handleGetRequest(endpoint, httpClient, requestContext.getHeaders(), logger))
        .thenAccept(jsonOrders -> {
          httpClient.closeClient();
          future.complete(jsonOrders.mapTo(PurchaseOrderCollection.class));
        })
        .exceptionally(t -> {
          httpClient.closeClient();
          logger.error("Error getting orders", t);
          future.completeExceptionally(t);
          return null;
        });
    } catch (Exception e) {
      future.completeExceptionally(e);
    }
    return future;
  }

  /**
   * Create a purchase order (PO) and a number of PO lines if provided.
   * @param compPO {@link CompositePurchaseOrder} object representing Purchase Order and optionally Purchase Order Line details.
   * @return completable future with {@link CompositePurchaseOrder} object with populated uuid on success or an exception if processing fails
   */
  public CompletableFuture<CompositePurchaseOrder> createPurchaseOrder(CompositePurchaseOrder compPO, RequestContext requestContext) {
    JsonObject cachedTenantConfiguration = new JsonObject();
    return configurationEntriesService.loadConfiguration(ORDER_CONFIG_MODULE_NAME, requestContext)
        .thenApply(tenantConfiguration -> cachedTenantConfiguration.mergeIn(tenantConfiguration, true))
        .thenCompose(tenantConfiguration -> validateAcqUnitsOnCreate(compPO.getAcqUnitIds(), requestContext))
        .thenAccept(ok -> checkOrderApprovalPermissions(compPO, cachedTenantConfiguration, requestContext))
        .thenCompose(ok -> setPoNumberIfMissing(compPO, requestContext)
        .thenCompose(v -> poNumberHelper.checkPONumberUnique(compPO.getPoNumber(), requestContext))
        .thenCompose(v -> processPoLineTags(compPO, requestContext))
        .thenCompose(v -> createPOandPOLines(compPO, cachedTenantConfiguration, requestContext))
        .thenCompose(aCompPO -> populateOrderSummary(aCompPO, requestContext)))
        .thenCompose(compOrder -> encumbranceService.updateEncumbrancesOrderStatus(compOrder, requestContext)
                .thenApply(v -> compOrder));
  }

  /**
   * Handles update of the order. First retrieve the PO from storage and depending on its content handle passed PO.
   * @param compPO updated {@link CompositePurchaseOrder} purchase order
   * @return completable future holding response indicating success (204 No Content) or error if failed
   */
  public CompletableFuture<Void> updateOrder(CompositePurchaseOrder compPO, RequestContext requestContext) {
    JsonObject cachedTenantConfiguration = new JsonObject();
    return configurationEntriesService.loadConfiguration(ORDER_CONFIG_MODULE_NAME, requestContext)
      .thenApply(tenantConfiguration -> cachedTenantConfiguration.mergeIn(tenantConfiguration, true))
      .thenCompose(tenantConfiguration -> purchaseOrderStorageService.getPurchaseOrderByIdAsJson(compPO.getId(), requestContext))
      .thenCompose(jsonPoFromStorage -> validateIfPOProtectedFieldsChanged(compPO, jsonPoFromStorage))
      .thenApply(HelperUtils::convertToCompositePurchaseOrder)
      .thenCompose(poFromStorage -> purchaseOrderLineService.populateOrderLines(poFromStorage, requestContext))
      .thenCompose(poFromStorage -> {
        boolean isTransitionToOpen = isTransitionToOpen(poFromStorage, compPO);
        return validateAcqUnitsOnUpdate(compPO, poFromStorage, requestContext)
          .thenCompose(ok -> poNumberHelper.validatePoNumber(poFromStorage, compPO, requestContext))
          .thenAccept(ok -> {
            if (isTransitionToApproved(poFromStorage, compPO)) {
              checkOrderApprovalPermissions(compPO, cachedTenantConfiguration, requestContext);
            }
          })
          .thenCompose(ok -> {
            if (isTransitionToPending(poFromStorage, compPO)) {
              checkOrderUnopenPermissions(requestContext);
              return unOpenCompositeOrderManager.process(compPO, poFromStorage, requestContext);
            }
            return CompletableFuture.completedFuture(null);
          })
          .thenCompose(ok -> {
            if (isTransitionToClosed(poFromStorage, compPO)) {
              return closeOrder(compPO, poFromStorage, requestContext);
            }
            return CompletableFuture.completedFuture(null);
          })
          .thenCompose(v -> {
            if (isTransitionToOpen) {
              if (CollectionUtils.isEmpty(compPO.getCompositePoLines())) {
                CompositePurchaseOrder clonedPoFromStorage = JsonObject.mapFrom(poFromStorage).mapTo(CompositePurchaseOrder.class);
                compPO.setCompositePoLines(clonedPoFromStorage.getCompositePoLines());
              }
              compPO.getCompositePoLines().forEach(poLine -> PoLineCommonUtil.updateLocationsQuantity(poLine.getLocations()));
              return openCompositeOrderFlowValidator.checkLocationsAndPiecesConsistency(compPO.getCompositePoLines(), requestContext);
            } else {
              return CompletableFuture.completedFuture(null);
            }
          })
          .thenCompose(ok -> {
            if (isTransitionToReopen(poFromStorage, compPO)) {
              checkOrderReopenPermissions(requestContext);
              return reOpenCompositeOrderManager.process(compPO, poFromStorage, requestContext);
            }
            return CompletableFuture.completedFuture(null);
          })
          .thenCompose(v -> purchaseOrderLineHelper.updatePoLines(poFromStorage, compPO, requestContext))
          .thenCompose(v -> {
            if (isTransitionToOpen) {
              checkOrderApprovalRequired(compPO, cachedTenantConfiguration, requestContext);
              return openCompositeOrderManager.process(compPO, poFromStorage, cachedTenantConfiguration, requestContext);
            } else {
              return CompletableFuture.completedFuture(null);
            }
          })
          .thenCompose(ok -> handleFinalOrderStatus(compPO, poFromStorage.getWorkflowStatus().value(), requestContext))
          .thenCompose(v -> encumbranceService.updateEncumbrancesOrderStatus(compPO, requestContext));
      });
  }

  public CompletableFuture<Void> handleFinalOrderStatus(CompositePurchaseOrder compPO, String initialOrdersStatus,
                                                        RequestContext requestContext) {
    PurchaseOrder purchaseOrder = convertToPurchaseOrder(compPO).mapTo(PurchaseOrder.class);
    CompletableFuture<List<PoLine>> future;

    if (isEmpty(compPO.getCompositePoLines())) {
      future = purchaseOrderLineService.getPoLinesByOrderId(compPO.getId(), requestContext);
    } else {
      future = FolioVertxCompletableFuture.supplyBlockingAsync(requestContext.getContext(), () -> {
        List<PoLine> poLines = HelperUtils.convertToPoLines(compPO.getCompositePoLines());
        changeOrderStatus(purchaseOrder, poLines);
        return poLines;
      });
    }

    return future.thenCompose(poLines -> handleFinalOrderItemsStatus(purchaseOrder, poLines, initialOrdersStatus, requestContext))
      .thenAccept(aVoid -> {
        compPO.setWorkflowStatus(WorkflowStatus.fromValue(purchaseOrder.getWorkflowStatus().value()));
        compPO.setCloseReason(purchaseOrder.getCloseReason());
      })
      .thenCompose(aVoid -> purchaseOrderStorageService.saveOrder(purchaseOrder, requestContext));
  }

  public CompletableFuture<Void> handleFinalOrderItemsStatus(PurchaseOrder purchaseOrder, List<PoLine> poLines, String initialOrdersStatus,
                                                             RequestContext requestContext) {
    if (isOrderClosing(purchaseOrder.getWorkflowStatus(), initialOrdersStatus)) {
      return updateItemsStatusInInventory(poLines, "On order", "Order closed", requestContext);
    } else if (isOrderReopening(purchaseOrder.getWorkflowStatus(), initialOrdersStatus)) {
      return updateItemsStatusInInventory(poLines, "Order closed", "On order", requestContext);
    }
    return CompletableFuture.completedFuture(null);
  }

  public CompletableFuture<JsonObject> validateIfPOProtectedFieldsChanged(CompositePurchaseOrder compPO,
      JsonObject compPOFromStorageJson) {
    WorkflowStatus storagePOWorkflowStatus = WorkflowStatus.fromValue(compPOFromStorageJson.getString(WORKFLOW_STATUS));
    if (!PENDING.equals(storagePOWorkflowStatus)) {
      List<String> fieldNames = OPEN.equals(storagePOWorkflowStatus) ? getFieldNamesForOpenOrder() : getFieldNames();
      // MODORDERS-429 double conversion required until HttpClient returns e.g. 'ongoing.renewalDate' in different format
      CompositePurchaseOrder storageCompPO = compPOFromStorageJson.mapTo(CompositePurchaseOrder.class);
      verifyProtectedFieldsChanged(fieldNames, JsonObject.mapFrom(storageCompPO), JsonObject.mapFrom(compPO));
    }
    return completedFuture(compPOFromStorageJson);
  }

  /**
   * Delete a purchase order with given uuid. As a first step the logic deletes all associated PO Lines and then order.
   * @param orderId purchase order id
   * @return completable future which is just completed with nothing on success or an exception if processing fails
   */
  public CompletableFuture<Void> deleteOrder(String orderId, RequestContext requestContext) {
    CompletableFuture<Void> future = new CompletableFuture<>();

    purchaseOrderStorageService.getPurchaseOrderByIdAsJson(orderId, requestContext)
      .thenAccept(purchaseOrder -> {
        CompositePurchaseOrder compPo = convertToCompositePurchaseOrder(purchaseOrder);
        protectionService.isOperationRestricted(compPo.getAcqUnitIds(), DELETE, requestContext)
          .thenCompose(v -> orderInvoiceRelationService.checkOrderInvoiceRelationship(orderId, requestContext))
          .thenAccept(aVoid -> encumbranceService.deleteOrderEncumbrances(orderId, requestContext)
            .thenCompose(v -> purchaseOrderLineService.deletePoLinesByOrderId(orderId, requestContext))
            .thenRun(() -> {
              logger.info("Successfully deleted poLines, proceeding with purchase order");
              purchaseOrderStorageService.deleteOrderById(orderId, requestContext)
                .thenAccept(rs -> {
                  logger.info("Successfully deleted order with id={}", orderId);
                  future.complete(null);
                })
                .exceptionally(t -> {
                  logger.error("Failed to delete the order with id={}", orderId, t.getCause());
                  future.completeExceptionally(t);
                  return null;
                });
            })
            .exceptionally(t -> {
              logger.error("Failed to delete PO Lines of the order with id={}", orderId, t.getCause());
              future.completeExceptionally(t);
              return null;
            })
          )
          .exceptionally(t -> {
            logger.error("User with id={} is forbidden to view delete with id={}", getCurrentUserId(requestContext), orderId, t.getCause());
            future.completeExceptionally(t);
            return null;
          });
      })
    .exceptionally(t -> {
      logger.error("Failed to delete PO Lines", t);
      future.completeExceptionally(t);
      return null;
    });

    return future;
  }

  /**
   * Gets purchase order by id
   *
   * @param orderId purchase order uuid
   * @return completable future with {@link CompositePurchaseOrder} on success or an exception if processing fails
   */
  public CompletableFuture<CompositePurchaseOrder> getCompositeOrder(String orderId, RequestContext requestContext) {

    CompletableFuture<CompositePurchaseOrder> future = new CompletableFuture<>();
    purchaseOrderStorageService.getPurchaseOrderByIdAsJson(orderId, requestContext)
      .thenApply(HelperUtils::convertToCompositePurchaseOrder)
      .thenAccept(compPO -> protectionService.isOperationRestricted(compPO.getAcqUnitIds(), ProtectedOperationType.READ, requestContext)
        .thenAccept(ok -> purchaseOrderLineService.populateOrderLines(compPO, requestContext)
          .thenCompose(compPOWithLines -> titlesService.fetchNonPackageTitles(compPOWithLines, requestContext))
          .thenAccept(linesIdTitles -> populateInstanceId(linesIdTitles, compPO.getCompositePoLines()))
          .thenCompose(po -> combinedPopulateService.populate(new CompositeOrderRetrieveHolder(compPO), requestContext))
          .thenApply(CompositeOrderRetrieveHolder::getOrder)
          .thenAccept(future::complete)
          .exceptionally(t -> {
            logger.error("Failed to get lines for order with id={}", orderId, t.getCause());
            future.completeExceptionally(t);
            return null;
          }))
        .exceptionally(t -> {
          logger.error("User with id={} is forbidden to view order with id={}", getCurrentUserId(requestContext), orderId, t.getCause());
          future.completeExceptionally(t);
          return null;
        }))
      .exceptionally(t -> {
        logger.error("Failed to build composite purchase order with id={}", orderId, t.getCause());
        future.completeExceptionally(t);
        return null;
      });

    return future;
  }

  /**
   * Sets the tenant default values and validates the order. Checks if Orders has
   * PO Lines within limit and validates vendors and access providers.
   *
   * @param compPO
   *          Purchase Order to validate
   * @return completable future which might be completed with {@code true} if
   *         order is valid, {@code false} if not valid or an exception if
   *         processing fails
   */
  public CompletableFuture<List<Error>> validateOrder(CompositePurchaseOrder compPO, JsonObject tenantConfig,
                                                  RequestContext requestContext) {
    List<Error> errors = new ArrayList<>();
    return setCreateInventoryDefaultValues(compPO, tenantConfig)
                .thenCompose(v -> validateOrderPoLines(compPO, requestContext)).thenAccept(errors::addAll)
                .thenAccept(v -> errors.addAll(validatePoLineLimit(compPO, tenantConfig)))
                .thenCompose(v -> validateIsbnValues(compPO, requestContext))
                .thenCompose(v -> validateVendor(compPO, requestContext).thenAccept(errors::addAll))
                .thenApply(v -> {
                  errors.addAll(validateRenewalInfo(compPO));
                  return errors;
                });
  }

  public CompletableFuture<List<Error>> validateOrderPoLines(CompositePurchaseOrder compositeOrder, RequestContext requestContext) {
    List<CompletableFuture<List<Error>>> poLinesErrors = compositeOrder.getCompositePoLines().stream()
      .map(compositePoLine -> compositePoLineValidationService.validatePoLine(compositePoLine, requestContext))
      .collect(toList());

    return collectResultsOnSuccess(poLinesErrors).thenApply(
      lists -> lists.stream()
        .flatMap(Collection::stream)
        .collect(toList()));
  }

  /**
   * Validates purchase order which already exists in the storage.
   * Checks PO Number presence, validates that provided order id corresponds to one set in order and its lines.
   * If all is okay, {@link #validateOrderPoLines(CompositePurchaseOrder)} is called afterwards.
   * @param orderId Purchase Order id
   * @param compPO Purchase Order to validate
   * @return completable future which might be completed with {@code true} if order is valid, {@code false} if not valid or an exception if processing fails
   */
  public CompletableFuture<List<Error>> validateExistingOrder(String orderId, CompositePurchaseOrder compPO,
                                                          RequestContext requestContext) {
    // The PO Number is required for existing orders
    List<Error> resultErrors = new ArrayList<>();
    if (StringUtils.isEmpty(compPO.getPoNumber())) {
      resultErrors.add(ErrorCodes.PO_NUMBER_REQUIRED.toError());
    }

    // Validate order uuid
    if (!compPO.getId().equals(orderId)) {
      resultErrors.add(ErrorCodes.MISMATCH_BETWEEN_ID_IN_PATH_AND_BODY.toError());
    } else if (isNotEmpty(compPO.getCompositePoLines())) {
      // Validate that each PO Line has correct order id
      compPO.getCompositePoLines().forEach(poLine -> {
        if (!orderId.equals(poLine.getPurchaseOrderId())) {
          Error error = ErrorCodes.MISMATCH_BETWEEN_ID_IN_PATH_AND_BODY.toError();
          if (StringUtils.isNotEmpty(poLine.getPoLineNumber())) {
            error.getParameters()
                 .add(new Parameter().withKey(PO_LINE_NUMBER)
                                     .withValue(poLine.getPoLineNumber()));
          }
          resultErrors.add(error);
        }
      });
    }

    return  configurationEntriesService.loadConfiguration(ORDER_CONFIG_MODULE_NAME, requestContext)
                                       .thenCompose(tenantConfig -> validateOrder(compPO, tenantConfig, requestContext))
                                        .thenApply(errors -> {
                                          resultErrors.addAll(errors);
                                          return resultErrors;
                                        });
  }

  private CompletableFuture<Void> setPoNumberIfMissing(CompositePurchaseOrder compPO, RequestContext requestContext) {
    if (null == compPO.getPoNumber()) {
      return poNumberHelper.generatePoNumber(requestContext)
                           .thenAccept(compPO::setPoNumber);
    }
    return completedFuture(null);
  }

  private CompletableFuture<List<Error>> validateVendor(CompositePurchaseOrder compPO, RequestContext requestContext) {
    if (compPO.getWorkflowStatus() == WorkflowStatus.OPEN) {
      HttpClientInterface httpClient = AbstractHelper.getHttpClient(requestContext.getHeaders(), true);
      VendorHelper vendorHelper = new VendorHelper(httpClient, requestContext.getHeaders(), requestContext.getContext(), EN);
      List<Error> combinedErrors = new ArrayList<>();
      return vendorHelper.validateVendor(compPO)
        .thenAccept(aErrors -> combinedErrors.addAll(aErrors.getErrors()))
        .thenCompose(errors -> fetchCompositePoLines(compPO, requestContext)
                                  .thenCompose(vendorHelper::validateAccessProviders)
                                  .thenAccept(aErrors -> combinedErrors.addAll(aErrors.getErrors()))
                                  .thenApply(v -> combinedErrors)
          );
    }
    return completedFuture(Collections.emptyList());
  }

  private List<Error> validateRenewalInfo(CompositePurchaseOrder compPO) {
    if (compPO.getOrderType() == CompositePurchaseOrder.OrderType.ONGOING && Objects.isNull(compPO.getOngoing())) {
      return List.of(MISSING_ONGOING.toError());
    } else if (compPO.getOrderType() == CompositePurchaseOrder.OrderType.ONE_TIME && Objects.nonNull(compPO.getOngoing())) {
      return List.of(ONGOING_NOT_ALLOWED.toError());
    }
    return Collections.emptyList();
  }

  private List<Error> validatePoLineLimit(CompositePurchaseOrder compPO, JsonObject tenantConfig) {
    if (isNotEmpty(compPO.getCompositePoLines())) {
      int limit = getPoLineLimit(tenantConfig);
      if (compPO.getCompositePoLines().size() > limit) {
        return List.of(ErrorCodes.POL_LINES_LIMIT_EXCEEDED.toError());
      }
      return Collections.emptyList();
    }
    return Collections.emptyList();
  }

  private CompletableFuture<CompositePurchaseOrder> createPOandPOLines(CompositePurchaseOrder compPO, JsonObject cachedTenantConfiguration,
                                                                      RequestContext requestContext) {
    final WorkflowStatus finalStatus = compPO.getWorkflowStatus();

    // we should always create PO and PO lines in PENDING status and transition to OPEN only when it's all set
    // (e.g. PO lines are created, Inventory is updated, etc.)
    if (finalStatus == OPEN) {
      compPO.setWorkflowStatus(PENDING);
    }

    return purchaseOrderStorageService.createPurchaseOrder(convertToPurchaseOrder(compPO), requestContext)
      .thenApply(createdOrder -> compPO.withId(createdOrder.getId()))
      .thenCompose(compPOWithId -> createPoLines(compPOWithId, requestContext))
      .thenApply(compPO::withCompositePoLines)
      .thenCompose(createdOrder -> {
        if (finalStatus == OPEN) {
          compPO.setWorkflowStatus(OPEN);
          checkOrderApprovalRequired(compPO, cachedTenantConfiguration, requestContext);
          return purchaseOrderLineService.populateOrderLines(compPO, requestContext)
                        .thenCompose(po -> openCompositeOrderManager.process(po, null, cachedTenantConfiguration, requestContext))
                        .thenCompose(ok -> handleFinalOrderStatus(compPO, finalStatus.value(), requestContext));
        }
        return completedFuture(null);
      })
      .thenApply(v -> compPO);
  }


  /**
   * Checks the value of "isApprovalRequired" in configurations, if the value is set to true, and order is being approved, verifies
   * if the user has required permissions to approve order
   *
   * @param compPO composite purchase order for checking permissions
   */
  private void checkOrderApprovalPermissions(CompositePurchaseOrder compPO, JsonObject tenantConfig, RequestContext requestContext) {
      boolean isApprovalRequired = isApprovalRequiredConfiguration(tenantConfig);
      if (isApprovalRequired && compPO.getApproved().equals(Boolean.TRUE)) {
        if (isUserNotHaveApprovePermission(requestContext)) {
          throw new HttpException(HttpStatus.HTTP_FORBIDDEN.toInt(), USER_HAS_NO_APPROVAL_PERMISSIONS);
        }
        compPO.setApprovalDate(new Date());
        compPO.setApprovedById(getCurrentUserId(requestContext));
      }
  }

  private void checkOrderUnopenPermissions(RequestContext requestContext) {
    if (isUserNotHaveUnopenPermission(requestContext)) {
      throw new HttpException(HttpStatus.HTTP_FORBIDDEN.toInt(), USER_HAS_NO_UNOPEN_PERMISSIONS);
    }
  }

  private void checkOrderReopenPermissions(RequestContext requestContext) {
    if (isUserNotHaveReopenPermission(requestContext)) {
      throw new HttpException(HttpStatus.HTTP_FORBIDDEN.toInt(), USER_HAS_NO_REOPEN_PERMISSIONS);
    }
  }

  private boolean isApprovalRequiredConfiguration(JsonObject config) {
    return Optional.ofNullable(config.getString("approvals"))
      .map(approval -> new JsonObject(approval).getBoolean("isApprovalRequired"))
      .orElse(false);
  }

  /**
   * If an order is transitioning to OPEN, checks if approval is required and throws an error if it is not approved
   *
   * @param compPO composite purchase order
   */
  private void checkOrderApprovalRequired(CompositePurchaseOrder compPO, JsonObject tenantConfig, RequestContext requestContext) {
      boolean isApprovalRequired = isApprovalRequiredConfiguration(tenantConfig);
      if (isApprovalRequired && !compPO.getApproved().equals(Boolean.TRUE)) {
        throw new HttpException(400, APPROVAL_REQUIRED_TO_OPEN);
      }
      compPO.setApprovedById(getCurrentUserId(requestContext));
      compPO.setApprovalDate(new Date());
  }

  private CompletableFuture<List<CompositePoLine>> createPoLines(CompositePurchaseOrder compPO, RequestContext requestContext) {
    List<CompletableFuture<CompositePoLine>> futures =
      compPO.getCompositePoLines()
            .stream()
            .map(compositePoLine -> purchaseOrderLineHelper.createPoLine(compositePoLine, compPO, requestContext))
            .collect(Collectors.toList());
    return collectResultsOnSuccess(futures);
  }

  private CompletableFuture<List<CompositePoLine>> fetchCompositePoLines(CompositePurchaseOrder compPO, RequestContext requestContext) {
    if (CollectionUtils.isEmpty(compPO.getCompositePoLines())) {
      return  purchaseOrderLineService.getCompositePoLinesByOrderId(compPO.getId(), requestContext)
        .thenApply(poLines -> {
          PoLineCommonUtil.sortPoLinesByPoLineNumber(poLines);
          return poLines;
        });
    } else {
      return completedFuture(compPO.getCompositePoLines());
    }
  }

  private void populateInstanceId(Map<String, List<Title>> lineIdsTitles, List<CompositePoLine> lines) {
    getNonPackageLines(lines).forEach(line -> {
      if (lineIdsTitles.containsKey(line.getId())) {
        line.setInstanceId(lineIdsTitles.get(line.getId()).get(0).getInstanceId());
      }
    });
  }

  /**
   * Convert {@link CompositePurchaseOrder} to Json representation of PurchaseOrder.
   * These objects are the same except PurchaseOrder doesn't contain poLines field.
   * @param compPO {@link CompositePurchaseOrder}
   * @return JsonObject representation of PurchaseOrder
   */
  private JsonObject convertToPurchaseOrder(CompositePurchaseOrder compPO) {
    // Remove dynamically calculated data
    compPO.setTotalEstimatedPrice(null);
    compPO.setTotalItems(null);
    compPO.setTotalEncumbered(null);
    compPO.setTotalExpended(null);
    compPO.setNeedReEncumber(null);

    JsonObject purchaseOrder = JsonObject.mapFrom(compPO);
    purchaseOrder.remove(COMPOSITE_PO_LINES);
    return purchaseOrder;
  }

  /**
   * The method checks if the order is assigned to acquisition unit, if yes,
   * then check that if the user has desired permission to assign the record to acquisition unit
   *
   * @throws HttpException if user does not have assign permission
   * @param acqUnitIds acquisitions units assigned to purchase order from request
   * @param requestContext
   */
  private void verifyUserHasAssignPermission(List<String> acqUnitIds, RequestContext requestContext) {
    if (isNotEmpty(acqUnitIds) && isUserDoesNotHaveDesiredPermission(ASSIGN, requestContext)){
      throw new HttpException(HttpStatus.HTTP_FORBIDDEN.toInt(), USER_HAS_NO_ACQ_PERMISSIONS);
    }
  }

  /**
   * @param updatedOrder purchase order from request
   * @param poFromStorage purchase order from storage
   * @return completable future completed successfully if all checks pass or exceptionally in case of error/restriction
   *         caused by acquisitions units
   */
  private CompletableFuture<Void> validateAcqUnitsOnUpdate(CompositePurchaseOrder updatedOrder, CompositePurchaseOrder poFromStorage,
                                                            RequestContext requestContext) {
    List<String> updatedAcqUnitIds = updatedOrder.getAcqUnitIds();
    List<String> currentAcqUnitIds = poFromStorage.getAcqUnitIds();

    return verifyUserHasManagePermission(updatedAcqUnitIds, currentAcqUnitIds, requestContext)
      // Check that all newly assigned units are active/exist
      .thenCompose(ok -> protectionService.verifyIfUnitsAreActive(ListUtils.subtract(updatedAcqUnitIds, currentAcqUnitIds), requestContext))
      .thenApply(ok -> getInvolvedOperations(updatedOrder, poFromStorage))
      // The check should be done against currently assigned (persisted in storage) units
      .thenCompose(protectedOperationTypes -> protectionService.isOperationRestricted(currentAcqUnitIds, protectedOperationTypes, requestContext));
  }

  /**
   * The method checks if list of acquisition units to which the order is assigned is changed, if yes,
   * then check that if the user has desired permission to manage acquisition units assignments
   *
   * @throws HttpException if user does not have manage permission
   * @param newAcqUnitIds acquisitions units assigned to purchase order from request
   * @param currentAcqUnitIds acquisitions units assigned to purchase order from storage
   */
  private CompletableFuture<Void> verifyUserHasManagePermission(List<String> newAcqUnitIds, List<String> currentAcqUnitIds, RequestContext requestContext) {
    Set<String> newAcqUnits = new HashSet<>(CollectionUtils.emptyIfNull(newAcqUnitIds));
    Set<String> acqUnitsFromStorage = new HashSet<>(CollectionUtils.emptyIfNull(currentAcqUnitIds));

    if (isManagePermissionRequired(newAcqUnits, acqUnitsFromStorage) && isUserDoesNotHaveDesiredPermission(MANAGE, requestContext)){
      return CompletableFuture.failedFuture(new HttpException(HttpStatus.HTTP_FORBIDDEN.toInt(), USER_HAS_NO_ACQ_PERMISSIONS));
    }
    return FolioVertxCompletableFuture.from(requestContext.getContext(), completedFuture(null));
  }

  private CompletionStage<Void> processPoLineTags(CompositePurchaseOrder compPO, RequestContext requestContext) {
    // MODORDERS-470 - new tags are all lower-case and no spaces
    for (CompositePoLine line : compPO.getCompositePoLines()) {
      if (line.getTags() != null) {
        var processedTagList = line.getTags()
          .getTagList()
          .stream()
          .filter(StringUtils::isNotBlank)
          .map(tag -> StringUtils.deleteWhitespace(tag).toLowerCase())
          .collect(toList());

        line.getTags().setTagList(processedTagList);
      }
    }

    Set<String> tagLabels = compPO.getCompositePoLines().stream()
      .filter(line -> Objects.nonNull(line.getTags()))
      .flatMap(line -> line.getTags().getTagList().stream())
      .collect(Collectors.toSet());

    if (tagLabels.isEmpty()) {
      return CompletableFuture.completedFuture(null);
    }
    return tagService.createTagsIfMissing(tagLabels, requestContext);
  }

  /**
   * @param acqUnitIds acquisitions units assigned to purchase order from request
   * @return completable future completed successfully if all checks pass or exceptionally in case of error/restriction
   *         caused by acquisitions units
   */
  private CompletableFuture<Void> validateAcqUnitsOnCreate(List<String> acqUnitIds, RequestContext requestContext) {
    if (acqUnitIds.isEmpty()) {
      return completedFuture(null);
    }

    return FolioVertxCompletableFuture.runAsync(requestContext.getContext(), () -> verifyUserHasAssignPermission(acqUnitIds, requestContext))
      .thenCompose(ok -> protectionService.verifyIfUnitsAreActive(acqUnitIds, requestContext))
      .thenCompose(ok -> protectionService.isOperationRestricted(acqUnitIds, ProtectedOperationType.CREATE, requestContext));
  }

  private CompletableFuture<String> buildGetOrdersPath(int limit, int offset, String query, RequestContext requestContext) {
    return acquisitionsUnitsService.buildAcqUnitsCqlExprToSearchRecords(StringUtils.EMPTY, requestContext)
      .thenApply(acqUnitsCqlExpr -> {
        if (StringUtils.isEmpty(query)) {
          String queryParam = buildQuery(acqUnitsCqlExpr);
          return String.format(GET_PURCHASE_ORDERS, limit, offset, queryParam, EN);
        } else {
          String queryParam = buildQuery(combineCqlExpressions("and", acqUnitsCqlExpr, query));
          return String.format(SEARCH_ORDERS_BY_LINES_DATA, limit, offset, queryParam, EN);
        }
      });
  }

  private CompletionStage<Void> closeOrder(CompositePurchaseOrder compPO, CompositePurchaseOrder poFromStorage, RequestContext requestContext) {
    EncumbranceWorkflowStrategy strategy = encumbranceWorkflowStrategyFactory.getStrategy(OrderWorkflowType.OPEN_TO_CLOSED);
    CompositePurchaseOrder clonedCompPO = JsonObject.mapFrom(compPO).mapTo(CompositePurchaseOrder.class);
    if (CollectionUtils.isEmpty(clonedCompPO.getCompositePoLines())) {
      List<CompositePoLine> clonedLines = poFromStorage.getCompositePoLines()
        .stream()
        .map(line -> JsonObject.mapFrom(line).mapTo(CompositePoLine.class))
        .collect(toList());
      clonedCompPO.setCompositePoLines(clonedLines);
    }
    if (compPO.getCloseReason() != null && REASON_CANCELLED.equals(compPO.getCloseReason().getReason())) {
      cancelOrderLines(compPO, poFromStorage);
    }
    return strategy.processEncumbrances(clonedCompPO, poFromStorage, requestContext);
  }

  private void cancelOrderLines(CompositePurchaseOrder compPO, CompositePurchaseOrder poFromStorage) {
    if (CollectionUtils.isEmpty(compPO.getCompositePoLines())) {
      List<CompositePoLine> clonedLines = poFromStorage.getCompositePoLines()
        .stream()
        .map(line -> JsonObject.mapFrom(line).mapTo(CompositePoLine.class))
        .collect(toList());
      compPO.setCompositePoLines(clonedLines);
    }
    compPO.getCompositePoLines().forEach(line -> {
      if (line.getPaymentStatus() != PaymentStatus.FULLY_PAID &&
          line.getPaymentStatus() != PaymentStatus.PAYMENT_NOT_REQUIRED &&
          line.getPaymentStatus() != PaymentStatus.CANCELLED) {
        line.setPaymentStatus(PaymentStatus.CANCELLED);
      }
      if (line.getReceiptStatus() != ReceiptStatus.FULLY_RECEIVED &&
          line.getReceiptStatus() != ReceiptStatus.RECEIPT_NOT_REQUIRED &&
          line.getReceiptStatus() != ReceiptStatus.CANCELLED) {
        line.setReceiptStatus(ReceiptStatus.CANCELLED);
      }
    });
  }

  private CompletableFuture<Void> validateIsbnValues(CompositePurchaseOrder compPO, RequestContext requestContext) {
    if (compPO.getCompositePoLines().isEmpty()){
      return completedFuture(null);
    }

    var filteredCompLines = compPO.getCompositePoLines()
      .stream()
      .filter(HelperUtils::isProductIdsExist)
      .collect(toList());
    List<CompletableFuture<Void>> futures = new ArrayList<>();

    return inventoryManager.getProductTypeUuidByIsbn(requestContext)
      .thenAccept(isbnId -> {
        CompletableFuture<Void> future = completedFuture(null);
        Map<String, String> normalizedIsbnCache = new HashMap<>();

        for (CompositePoLine compLine : filteredCompLines) {
          if (HelperUtils.isProductIdsExist(compLine)) {
            future = future.thenCompose(v -> purchaseOrderLineHelper.validateAndNormalizeISBN(compLine, isbnId, normalizedIsbnCache, requestContext));
            futures.add(future);
          }
        }
      })
      .thenCompose(v -> CompletableFuture.allOf(futures.toArray(new CompletableFuture[0])));
  }

  private CompletableFuture<Void> setCreateInventoryDefaultValues(CompositePurchaseOrder compPO, JsonObject tenantConfiguration) {
    CompletableFuture<?>[] futures = compPO.getCompositePoLines()
      .stream()
      .map(compPOL -> purchaseOrderLineHelper.setTenantDefaultCreateInventoryValues(compPOL, tenantConfiguration))
      .toArray(CompletableFuture[]::new);

    return CompletableFuture.allOf(futures);
  }

  private CompletableFuture<CompositePurchaseOrder> populateOrderSummary(CompositePurchaseOrder order, RequestContext requestContext) {
    return orderLinesSummaryPopulateService.populate(new CompositeOrderRetrieveHolder(order), requestContext)
      .thenApply(CompositeOrderRetrieveHolder::getOrder);
  }


  private CompletableFuture<Void> updateItemsInInventory(List<JsonObject> items, RequestContext requestContext) {
    return CompletableFuture.allOf(items.stream()
      .map(item -> inventoryManager.updateItem(item, requestContext))
      .toArray(CompletableFuture[]::new));
  }

  private List<CompositePoLine> getNonPackageLines(List<CompositePoLine> compositePoLines) {
    return compositePoLines.stream().filter(line -> !line.getIsPackage()).collect(toList());
  }

  private boolean isUserDoesNotHaveDesiredPermission(AcqDesiredPermissions acqPerm, RequestContext requestContext) {
    return !getProvidedPermissions(requestContext).contains(acqPerm.getPermission());
  }

  private boolean isManagePermissionRequired(Set<String> newAcqUnits, Set<String> acqUnitsFromStorage) {
    return !CollectionUtils.isEqualCollection(newAcqUnits, acqUnitsFromStorage);
  }

  private List<String> getProvidedPermissions(RequestContext requestContext) {
    return new JsonArray(requestContext.getHeaders().getOrDefault(OKAPI_HEADER_PERMISSIONS, EMPTY_ARRAY)).stream().
      map(Object::toString)
      .collect(Collectors.toList());
  }

  private boolean isUserNotHaveApprovePermission(RequestContext requestContext) {
    return !getProvidedPermissions(requestContext).contains(PERMISSION_ORDER_APPROVE);
  }

  private boolean isUserNotHaveUnopenPermission(RequestContext requestContext) {
    return !getProvidedPermissions(requestContext).contains(PERMISSION_ORDER_UNOPEN);
  }

  private boolean isUserNotHaveReopenPermission(RequestContext requestContext) {
    return !getProvidedPermissions(requestContext).contains(PERMISSION_ORDER_REOPEN);
  }

  private String getCurrentUserId(RequestContext requestContext) {
    return requestContext.getHeaders().get(OKAPI_USERID_HEADER);
  }

  private List<JsonObject> updateStatusName(List<JsonObject> items, String status) {
    items.forEach(item -> item.getJsonObject("status").put("name", status));
    return items;
  }

  private CompletableFuture<Void> updateItemsStatusInInventory(List<PoLine> poLines,
    String currentStatus, String newStatus, RequestContext requestContext) {

    if (CollectionUtils.isEmpty(poLines)) {
      return CompletableFuture.completedFuture(null);
    }
    List<String> poLineIds = poLines.stream().map(PoLine::getId).collect(toList());
    return CompletableFuture.allOf(
      StreamEx.ofSubLists(poLineIds, MAX_IDS_FOR_GET_RQ)
        .map(chunk -> CompletableFutureRepeater.repeat(MAX_REPEAT_ON_FAILURE,
          () -> updateItemsStatus(chunk, currentStatus, newStatus, requestContext)))
        .toArray(CompletableFuture[]::new));
  }

  private CompletableFuture<Void> updateItemsStatus(List<String> poLineIds,
    String currentStatus, String newStatus, RequestContext requestContext) {

    return inventoryManager.getItemsByPoLineIdsAndStatus(poLineIds, currentStatus, requestContext)
      .thenApply(items -> updateStatusName(items, newStatus))
      .thenCompose(items -> updateItemsInInventory(items, requestContext));
  }

  private Set<ProtectedOperationType> getInvolvedOperations(CompositePurchaseOrder compPO, CompositePurchaseOrder poFromStorage) {
    if (CollectionUtils.isEmpty(compPO.getCompositePoLines())) {
      return Collections.singleton(UPDATE);
    }
    List<PoLine> poLines = HelperUtils.convertToPoLines(poFromStorage.getCompositePoLines());
    Set<String> newIds = compPO.getCompositePoLines().stream().map(CompositePoLine::getId).collect(Collectors.toSet());
    Set<String> storageIds = poLines.stream().map(PoLine::getId).collect(Collectors.toSet());
    Set<ProtectedOperationType> operations = new HashSet<>();
    operations.add(UPDATE);
    operations.addAll(getInvolvedOperations(newIds, storageIds));
    return operations;
  }

  private Set<ProtectedOperationType> getInvolvedOperations(Set<String> newIds, Set<String> storageIds) {
    if (CollectionUtils.isEqualCollection(newIds, storageIds)) {
      return Collections.emptySet();
    } else if (CollectionUtils.isSubCollection(storageIds, newIds)) {
      return Collections.singleton(CREATE);
    } else if (CollectionUtils.isSubCollection(newIds, storageIds)) {
      return Collections.singleton(DELETE);
    }
    Set<ProtectedOperationType> operations = new HashSet<>();
    operations.add(CREATE);
    operations.add(DELETE);
    return operations;
  }
}
