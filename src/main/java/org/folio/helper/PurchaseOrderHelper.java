package org.folio.helper;

import static java.util.stream.Collectors.toList;
import static org.apache.commons.collections4.CollectionUtils.isEmpty;
import static org.apache.commons.collections4.CollectionUtils.isNotEmpty;
import static org.folio.helper.BaseHelper.MAX_REPEAT_ON_FAILURE;
import static org.folio.orders.utils.AcqDesiredPermissions.MANAGE;
import static org.folio.orders.utils.HelperUtils.COMPOSITE_PO_LINES;
import static org.folio.orders.utils.HelperUtils.ORDER_CONFIG_MODULE_NAME;
import static org.folio.orders.utils.HelperUtils.REASON_CANCELLED;
import static org.folio.orders.utils.HelperUtils.WORKFLOW_STATUS;
import static org.folio.orders.utils.HelperUtils.changeOrderStatus;
import static org.folio.orders.utils.HelperUtils.collectResultsOnSuccess;
import static org.folio.orders.utils.HelperUtils.convertToCompositePurchaseOrder;
import static org.folio.orders.utils.HelperUtils.getPoLineLimit;
import static org.folio.orders.utils.OrderStatusTransitionUtil.isOrderClosing;
import static org.folio.orders.utils.OrderStatusTransitionUtil.isOrderReopening;
import static org.folio.orders.utils.OrderStatusTransitionUtil.isTransitionToApproved;
import static org.folio.orders.utils.OrderStatusTransitionUtil.isTransitionToClosed;
import static org.folio.orders.utils.OrderStatusTransitionUtil.isTransitionToOpen;
import static org.folio.orders.utils.OrderStatusTransitionUtil.isTransitionToPending;
import static org.folio.orders.utils.OrderStatusTransitionUtil.isTransitionToReopen;
import static org.folio.orders.utils.POProtectedFields.getFieldNames;
import static org.folio.orders.utils.POProtectedFields.getFieldNamesForOpenOrder;
import static org.folio.orders.utils.PermissionsUtil.*;
import static org.folio.orders.utils.PoLineCommonUtil.verifyOngoingFieldsChanged;
import static org.folio.orders.utils.PoLineCommonUtil.verifyProtectedFieldsChanged;
import static org.folio.orders.utils.ProtectedOperationType.CREATE;
import static org.folio.orders.utils.ProtectedOperationType.DELETE;
import static org.folio.orders.utils.ProtectedOperationType.UPDATE;
import static org.folio.orders.utils.ResourcePathResolver.PO_LINE_NUMBER;
import static org.folio.orders.utils.ResourcePathResolver.PURCHASE_ORDER_STORAGE;
import static org.folio.orders.utils.ResourcePathResolver.resourcesPath;
import static org.folio.rest.RestConstants.MAX_IDS_FOR_GET_RQ_15;
import static org.folio.rest.core.exceptions.ErrorCodes.APPROVAL_REQUIRED_TO_OPEN;
import static org.folio.rest.core.exceptions.ErrorCodes.MISSING_ONGOING;
import static org.folio.rest.core.exceptions.ErrorCodes.ONGOING_NOT_ALLOWED;
import static org.folio.rest.core.exceptions.ErrorCodes.USER_HAS_NO_APPROVAL_PERMISSIONS;
import static org.folio.rest.core.exceptions.ErrorCodes.USER_HAS_NO_REOPEN_PERMISSIONS;
import static org.folio.rest.core.exceptions.ErrorCodes.USER_HAS_NO_UNOPEN_PERMISSIONS;
import static org.folio.rest.jaxrs.model.CompositePurchaseOrder.WorkflowStatus.OPEN;
import static org.folio.rest.jaxrs.model.CompositePurchaseOrder.WorkflowStatus.PENDING;
import static org.folio.service.UserService.getCurrentUserId;

import java.util.ArrayList;
import java.util.Arrays;
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
import java.util.stream.Collectors;

import org.apache.commons.collections4.CollectionUtils;
import org.apache.commons.lang3.StringUtils;
import org.apache.logging.log4j.LogManager;
import org.apache.logging.log4j.Logger;
import org.folio.HttpStatus;
import org.folio.completablefuture.VertxFutureRepeater;
import org.folio.models.CompositeOrderRetrieveHolder;
import org.folio.models.ItemStatus;
import org.folio.okapi.common.GenericCompositeFuture;
import org.folio.orders.utils.AcqDesiredPermissions;
import org.folio.orders.utils.HelperUtils;
import org.folio.orders.utils.PoLineCommonUtil;
import org.folio.orders.utils.ProtectedOperationType;
import org.folio.orders.utils.RequestContextUtil;
import org.folio.rest.core.RestClient;
import org.folio.rest.core.exceptions.ErrorCodes;
import org.folio.rest.core.exceptions.HttpException;
import org.folio.rest.core.models.RequestContext;
import org.folio.rest.core.models.RequestEntry;
import org.folio.rest.jaxrs.model.CompositePoLine;
import org.folio.rest.jaxrs.model.CompositePoLine.PaymentStatus;
import org.folio.rest.jaxrs.model.CompositePoLine.ReceiptStatus;
import org.folio.rest.jaxrs.model.CompositePurchaseOrder;
import org.folio.rest.jaxrs.model.CompositePurchaseOrder.WorkflowStatus;
import org.folio.rest.jaxrs.model.Error;
import org.folio.rest.jaxrs.model.Location;
import org.folio.rest.jaxrs.model.Parameter;
import org.folio.rest.jaxrs.model.PoLine;
import org.folio.rest.jaxrs.model.PurchaseOrder;
import org.folio.rest.jaxrs.model.PurchaseOrderCollection;
import org.folio.rest.jaxrs.model.Title;
import org.folio.service.PrefixService;
import org.folio.service.ProtectionService;
import org.folio.service.SuffixService;
import org.folio.service.TagService;
import org.folio.service.caches.ConfigurationEntriesCache;
import org.folio.service.finance.transaction.EncumbranceService;
import org.folio.service.finance.transaction.EncumbranceWorkflowStrategy;
import org.folio.service.finance.transaction.EncumbranceWorkflowStrategyFactory;
import org.folio.service.inventory.InventoryItemManager;
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
import org.folio.service.organization.OrganizationService;
import org.folio.service.titles.TitlesService;

import io.vertx.core.Future;
import io.vertx.core.Promise;
import io.vertx.core.json.JsonObject;
import one.util.streamex.StreamEx;

public class PurchaseOrderHelper {
  private static final Logger logger = LogManager.getLogger(PurchaseOrderHelper.class);

  private final PurchaseOrderLineHelper purchaseOrderLineHelper;
  private final CompositeOrderDynamicDataPopulateService orderLinesSummaryPopulateService;
  private final EncumbranceService encumbranceService;
  private final CompositeOrderDynamicDataPopulateService combinedPopulateService;
  private final EncumbranceWorkflowStrategyFactory encumbranceWorkflowStrategyFactory;
  private final OrderInvoiceRelationService orderInvoiceRelationService;
  private final TagService tagService;
  private final PurchaseOrderLineService purchaseOrderLineService;
  private final TitlesService titlesService;
  private final ProtectionService protectionService;
  private final PrefixService prefixService;
  private final SuffixService suffixService;
  private final InventoryItemManager inventoryItemManager;
  private final UnOpenCompositeOrderManager unOpenCompositeOrderManager;
  private final OpenCompositeOrderManager openCompositeOrderManager;
  private final OpenCompositeOrderFlowValidator openCompositeOrderFlowValidator;
  private final PurchaseOrderStorageService purchaseOrderStorageService;
  private final ConfigurationEntriesCache configurationEntriesCache;
  private final PoNumberHelper poNumberHelper;
  private final CompositePoLineValidationService compositePoLineValidationService;
  private final ReOpenCompositeOrderManager reOpenCompositeOrderManager;
  private final OrganizationService organizationService;
  private final RestClient restClient;

  public PurchaseOrderHelper(PurchaseOrderLineHelper purchaseOrderLineHelper,
      CompositeOrderDynamicDataPopulateService orderLinesSummaryPopulateService, EncumbranceService encumbranceService,
      CompositeOrderDynamicDataPopulateService combinedPopulateService,
      EncumbranceWorkflowStrategyFactory encumbranceWorkflowStrategyFactory,
      OrderInvoiceRelationService orderInvoiceRelationService, TagService tagService,
      PurchaseOrderLineService purchaseOrderLineService, TitlesService titlesService,
      ProtectionService protectionService, PrefixService prefixService,
      SuffixService suffixService, InventoryItemManager inventoryItemManager, UnOpenCompositeOrderManager unOpenCompositeOrderManager,
      OpenCompositeOrderManager openCompositeOrderManager, PurchaseOrderStorageService purchaseOrderStorageService,
      ConfigurationEntriesCache configurationEntriesCache, PoNumberHelper poNumberHelper,
      OpenCompositeOrderFlowValidator openCompositeOrderFlowValidator,
      CompositePoLineValidationService compositePoLineValidationService, ReOpenCompositeOrderManager reOpenCompositeOrderManager,
      OrganizationService organizationService, RestClient restClient) {
    this.purchaseOrderLineHelper = purchaseOrderLineHelper;
    this.orderLinesSummaryPopulateService = orderLinesSummaryPopulateService;
    this.encumbranceService = encumbranceService;
    this.combinedPopulateService = combinedPopulateService;
    this.encumbranceWorkflowStrategyFactory = encumbranceWorkflowStrategyFactory;
    this.orderInvoiceRelationService = orderInvoiceRelationService;
    this.tagService = tagService;
    this.purchaseOrderLineService = purchaseOrderLineService;
    this.titlesService = titlesService;
    this.protectionService = protectionService;
    this.prefixService = prefixService;
    this.suffixService = suffixService;
    this.inventoryItemManager = inventoryItemManager;
    this.unOpenCompositeOrderManager = unOpenCompositeOrderManager;
    this.openCompositeOrderManager = openCompositeOrderManager;
    this.purchaseOrderStorageService = purchaseOrderStorageService;
    this.configurationEntriesCache = configurationEntriesCache;
    this.poNumberHelper = poNumberHelper;
    this.openCompositeOrderFlowValidator = openCompositeOrderFlowValidator;
    this.compositePoLineValidationService = compositePoLineValidationService;
    this.reOpenCompositeOrderManager = reOpenCompositeOrderManager;
    this.organizationService = organizationService;
    this.restClient = restClient;
  }

  /**
   * Retrieve a list of {@link PurchaseOrder} objects retrieved from storage by provided query.
   *
   * @param limit limit the number of elements returned in the response
   * @param offset skip over a number of elements by specifying an offset value for the query
   * @param query A query expressed as a CQL string using valid searchable fields.
   * @return completable future with {@link PurchaseOrderCollection} object on success or an exception if processing fails
   */
  public Future<PurchaseOrderCollection> getPurchaseOrders(int limit, int offset, String query, RequestContext requestContext) {
    return protectionService.getQueryWithAcqUnitsCheck(StringUtils.EMPTY, query, requestContext)
      .compose(finalQuery -> {
        RequestEntry requestEntry = new RequestEntry(resourcesPath(PURCHASE_ORDER_STORAGE))
          .withQuery(finalQuery)
          .withLimit(limit)
          .withOffset(offset);
        return restClient.get(requestEntry, PurchaseOrderCollection.class, requestContext);
      })
      .onFailure(t -> logger.error("Error getting orders", t));
  }

  /**
   * Get purchase order by Id
   *
   * @param orderId purchase order id
   * @return completable future with {@link PurchaseOrder} object
   */
  public Future<PurchaseOrder> getPurchaseOrderById(String orderId, RequestContext requestContext) {
    logger.info("getPurchaseOrderById:: orderId: {}", orderId);
    return purchaseOrderStorageService.getPurchaseOrderById(orderId, requestContext);
  }

  /**
   * Create a purchase order (PO) and a number of PO lines if provided.
   * @param compPO {@link CompositePurchaseOrder} object representing Purchase Order and optionally Purchase Order Line details.
   * @return completable future with {@link CompositePurchaseOrder} object with populated uuid on success or an exception if processing fails
   */
  public Future<CompositePurchaseOrder> createPurchaseOrder(CompositePurchaseOrder compPO, JsonObject tenantConfiguration, RequestContext requestContext) {
    logger.info("createPurchaseOrder :: orderId: {}", compPO.getId());
    return validateNewPurchaseOrders(compPO, requestContext)
      .compose(v -> setPoNumberIfMissing(compPO, requestContext))
      .compose(v -> processPoLineTags(compPO, requestContext))
      .compose(v -> createPOandPOLines(compPO, tenantConfiguration, requestContext))
      .compose(aCompPO -> populateOrderSummary(aCompPO, requestContext))
      .compose(compOrder -> encumbranceService.updateEncumbrancesOrderStatusAndReleaseIfClosed(compOrder, requestContext)
        .map(v -> compOrder));
  }

  private Future<Void> validateNewPurchaseOrders(CompositePurchaseOrder compPO, RequestContext requestContext) {
    logger.info("validateNewPurchaseOrders :: orderId: {}", compPO.getId());
    List<Future<Void>> futures = new ArrayList<>();

    futures.add(protectionService.validateAcqUnitsOnCreate(compPO.getAcqUnitIds(), AcqDesiredPermissions.ASSIGN, requestContext));
    futures.add(checkOrderApprovalPermissions(compPO, requestContext));
    futures.add(prefixService.validatePrefixAvailability(compPO.getPoNumberPrefix(), requestContext));
    futures.add(suffixService.validateSuffixAvailability(compPO.getPoNumberSuffix(), requestContext));
    futures.add(poNumberHelper.checkPONumberUnique(compPO.getPoNumber(), requestContext));

    return GenericCompositeFuture.join(futures)
      .onSuccess(v -> logger.info("validation successful"))
      .onFailure(v-> logger.error("validation failed"))
      .mapEmpty();

  }

  /**
   * Handles update of the order. First retrieve the PO from storage and depending on its content handle passed PO.
   * @param compPO updated {@link CompositePurchaseOrder} purchase order
   * @return completable future holding response indicating success (204 No Content) or error if failed
   */
  public Future<Void> updateOrder(CompositePurchaseOrder compPO, boolean deleteHoldings, RequestContext requestContext) {
    return purchaseOrderStorageService.getPurchaseOrderByIdAsJson(compPO.getId(), requestContext)
      .map(jsonPoFromStorage -> validateIfPOProtectedAndOngoingFieldsChanged(compPO, jsonPoFromStorage))
      .map(HelperUtils::convertToCompositePurchaseOrder)
      .compose(lines -> purchaseOrderLineService.populateOrderLines(lines, requestContext))
      .compose(poFromStorage -> {
        boolean isTransitionToOpen = isTransitionToOpen(poFromStorage, compPO);
        return validateAcqUnitsOnUpdate(compPO, poFromStorage, requestContext)
          .compose(ok -> prefixService.validatePrefixAvailability(compPO.getPoNumberPrefix(), requestContext))
          .compose(ok -> suffixService.validateSuffixAvailability(compPO.getPoNumberSuffix(), requestContext))
          .compose(ok -> poNumberHelper.validatePoNumberPrefixAndSuffix(compPO))
          .compose(ok -> poNumberHelper.validatePoNumber(poFromStorage, compPO, requestContext))
          .compose(ok -> {
            if (isTransitionToApproved(poFromStorage, compPO)) {
              return checkOrderApprovalPermissions(compPO, requestContext);
            }
            return Future.succeededFuture();
          })
          .compose(ok -> {
            if (isTransitionToPending(poFromStorage, compPO)) {
              checkOrderUnopenPermissions(requestContext);
              return unOpenCompositeOrderManager.process(compPO, poFromStorage, deleteHoldings, requestContext);
            }
            return Future.succeededFuture();
          })
          .compose(ok -> {
            if (isTransitionToClosed(poFromStorage, compPO)) {
              return closeOrder(compPO, poFromStorage, requestContext);
            }
            return Future.succeededFuture();
          })
          .compose(v -> {
            if (isTransitionToOpen) {
              if (CollectionUtils.isEmpty(compPO.getCompositePoLines())) {
                CompositePurchaseOrder clonedPoFromStorage = JsonObject.mapFrom(poFromStorage).mapTo(CompositePurchaseOrder.class);
                compPO.setCompositePoLines(clonedPoFromStorage.getCompositePoLines());
              }
              compPO.getCompositePoLines().forEach(poLine -> PoLineCommonUtil.updateLocationsQuantity(poLine.getLocations()));
              return openCompositeOrderFlowValidator.checkLocationsAndPiecesConsistency(compPO.getCompositePoLines(), requestContext)
                .compose(ok -> openCompositeOrderFlowValidator.checkFundLocationRestrictions(compPO.getCompositePoLines(), requestContext));
            }
            return Future.succeededFuture();
          })
          .compose(ok -> {
            if (isTransitionToReopen(poFromStorage, compPO)) {
              return Future.succeededFuture()
                .map(v -> {
                  checkOrderReopenPermissions(requestContext);
                  return null;
                })
                .compose(v -> reOpenCompositeOrderManager.process(compPO, poFromStorage, requestContext));
            }
            return Future.succeededFuture();
          })
          .compose(v -> purchaseOrderLineHelper.updatePoLines(poFromStorage, compPO, requestContext))
          .compose(v -> {
            if (isTransitionToOpen) {
              return checkOrderApprovalRequired(compPO, requestContext)
                .compose(ok -> configurationEntriesCache.loadConfiguration(ORDER_CONFIG_MODULE_NAME, requestContext))
                .compose(tenantConfiguration -> openCompositeOrderManager.process(compPO, poFromStorage, tenantConfiguration, requestContext));
            } else {
              return Future.succeededFuture();
            }
          })
          .compose(ok -> handleFinalOrderStatus(compPO, poFromStorage.getWorkflowStatus().value(), requestContext))
          .compose(v -> encumbranceService.updateEncumbrancesOrderStatusAndReleaseIfClosed(compPO, requestContext));
      });
  }

  public Future<Void> handleFinalOrderStatus(CompositePurchaseOrder compPO, String initialOrdersStatus,
                                                        RequestContext requestContext) {
    PurchaseOrder purchaseOrder = convertToPurchaseOrder(compPO);
    Promise<List<PoLine>> promise = Promise.promise();

    if (isEmpty(compPO.getCompositePoLines())) {
      purchaseOrderLineService.getPoLinesByOrderId(compPO.getId(), requestContext)
        .onSuccess(promise::complete)
        .onFailure(promise::fail);
    } else {
      Future.succeededFuture()
        .map(v -> {
          List<PoLine> poLines = HelperUtils.convertToPoLines(compPO.getCompositePoLines());
          if (initialOrdersStatus.equals(compPO.getWorkflowStatus().value()))
            changeOrderStatus(purchaseOrder, poLines);
          promise.complete(poLines);
          return null;
        })
        .onFailure(promise::fail);
    }

    return promise.future()
      .compose(poLines -> handleFinalOrderItemsStatus(purchaseOrder, poLines, initialOrdersStatus, requestContext))
      .map(v -> {
        compPO.setWorkflowStatus(WorkflowStatus.fromValue(purchaseOrder.getWorkflowStatus().value()));
        compPO.setCloseReason(purchaseOrder.getCloseReason());
        return null;
      })
      .compose(ok -> purchaseOrderStorageService.saveOrder(purchaseOrder, requestContext));
  }

  public Future<Void> handleFinalOrderItemsStatus(PurchaseOrder purchaseOrder, List<PoLine> poLines, String initialOrdersStatus,
                                                             RequestContext requestContext) {
    if (isOrderClosing(purchaseOrder.getWorkflowStatus(), initialOrdersStatus)) {
      return updateItemsStatusInInventory(poLines, ItemStatus.ON_ORDER, ItemStatus.ORDER_CLOSED, requestContext);
    } else if (isOrderReopening(purchaseOrder.getWorkflowStatus(), initialOrdersStatus)) {
      return updateItemsStatusInInventory(poLines, ItemStatus.ORDER_CLOSED, ItemStatus.ON_ORDER, requestContext);
    }
    return Future.succeededFuture();
  }

  public JsonObject validateIfPOProtectedAndOngoingFieldsChanged(CompositePurchaseOrder compPO,
                                                                 JsonObject compPOFromStorageJson) {
    WorkflowStatus storagePOWorkflowStatus = WorkflowStatus.fromValue(compPOFromStorageJson.getString(WORKFLOW_STATUS));
    if (!PENDING.equals(storagePOWorkflowStatus)) {
      List<String> fieldNames = OPEN.equals(storagePOWorkflowStatus) ? getFieldNamesForOpenOrder() : getFieldNames();
      verifyProtectedFieldsChanged(fieldNames, compPOFromStorageJson, JsonObject.mapFrom(compPO));
      verifyOngoingFieldsChanged(compPOFromStorageJson, compPO);
    }
    return compPOFromStorageJson;
  }

  /**
   * Delete a purchase order with given uuid. As a first step the logic deletes all associated PO Lines and then order.
   * @param orderId purchase order id
   * @return completable future which is just completed with nothing on success or an exception if processing fails
   */
  public Future<Void> deleteOrder(String orderId, RequestContext requestContext) {
    Promise<Void> promise = Promise.promise();

    purchaseOrderStorageService.getPurchaseOrderByIdAsJson(orderId, requestContext)
      .map(purchaseOrder -> {
        CompositePurchaseOrder compPo = convertToCompositePurchaseOrder(purchaseOrder);
        protectionService.isOperationRestricted(compPo.getAcqUnitIds(), DELETE, requestContext)
          .compose(v -> orderInvoiceRelationService.checkOrderInvoiceRelationship(orderId, requestContext))
          .compose(aVoid -> encumbranceService.deleteOrderEncumbrances(orderId, requestContext)
            .compose(v -> purchaseOrderLineService.deletePoLinesByOrderId(orderId, requestContext))
            .compose(v -> {
              logger.info("Successfully deleted poLines, proceeding with purchase order");
              return purchaseOrderStorageService.deleteOrderById(orderId, requestContext)
                .onSuccess(rs -> {
                  logger.info("Successfully deleted order with id={}", orderId);
                  promise.complete();
                })
                .onFailure(t -> {
                  logger.error("Failed to delete the order with id={}", orderId, t.getCause());
                  promise.fail(t);
                });
            })
            .onFailure(t -> {
              logger.error("Failed to delete PO Lines of the order with id={}", orderId, t.getCause());
              promise.fail(t);
            })
          )
          .onFailure(t -> {
            logger.error("User with id={} is forbidden to view delete with id={}", getCurrentUserId(requestContext.getHeaders()), orderId, t.getCause());
            promise.fail(t);
          });
        return null;
      })
      .onFailure(t -> {
        logger.error("Failed to delete PO Lines", t);
        promise.fail(t);
      });

    return promise.future();
  }

  /**
   * Gets purchase order by id
   *
   * @param orderId purchase order uuid
   * @return completable future with {@link CompositePurchaseOrder} on success or an exception if processing fails
   */
  public Future<CompositePurchaseOrder> getCompositeOrder(String orderId, RequestContext requestContext) {
    Promise<CompositePurchaseOrder> promise = Promise.promise();
    purchaseOrderStorageService.getPurchaseOrderByIdAsJson(orderId, requestContext)
      .map(HelperUtils::convertToCompositePurchaseOrder)
      .compose(compPO -> protectionService.isOperationRestricted(compPO.getAcqUnitIds(), ProtectedOperationType.READ, requestContext)
        .onFailure(t -> {
          logger.error("User with id={} is forbidden to view order with id={}", getCurrentUserId(requestContext.getHeaders()),
              orderId, t.getCause());
          promise.fail(t);
        })
        .compose(ok -> purchaseOrderLineService.populateOrderLines(compPO, requestContext)
          .compose(compPOWithLines -> titlesService.fetchNonPackageTitles(compPOWithLines, requestContext))
          .map(linesIdTitles -> {
            populateInstanceId(linesIdTitles, compPO.getCompositePoLines());
            return null;
          })
          .compose(po -> combinedPopulateService.populate(new CompositeOrderRetrieveHolder(compPO), requestContext)))
        .map(CompositeOrderRetrieveHolder::getOrder))
      .onSuccess(promise::complete)
      .onFailure(t -> {
        logger.error("Failed to build composite purchase order with id={}", orderId, t.getCause());
        promise.fail(t);
      });
    return promise.future();
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
  public Future<List<Error>> validateOrder(CompositePurchaseOrder compPO, JsonObject tenantConfig, RequestContext requestContext) {
    List<Error> errors = new ArrayList<>();
    return setCreateInventoryDefaultValues(compPO, tenantConfig)
      .compose(v -> validateOrderPoLines(compPO, requestContext))
      .map(errors::addAll)
      .map(v -> errors.addAll(validatePoLineLimit(compPO, tenantConfig)))
      .compose(v -> purchaseOrderLineService.validateAndNormalizeISBN(compPO.getCompositePoLines(), requestContext))
      .compose(v -> validateVendor(compPO, requestContext))
      .map(errors::addAll)
      .map(v -> {
        errors.addAll(validateRenewalInfo(compPO));
        return errors;
      });
  }

  public Future<List<Error>> validateOrderPoLines(CompositePurchaseOrder compositeOrder, RequestContext requestContext) {
    List<Future<List<Error>>> poLinesErrors = compositeOrder.getCompositePoLines().stream()
      .map(compositePoLine -> compositePoLineValidationService.validatePoLine(compositePoLine, requestContext))
      .collect(toList());

    return collectResultsOnSuccess(poLinesErrors).map(
      lists -> lists.stream()
        .flatMap(Collection::stream)
        .collect(toList()));
  }

  /**
   * Validates purchase order which already exists in the storage.
   * Checks PO Number presence, validates that provided order id corresponds to one set in order and its lines.
   * @param orderId Purchase Order id
   * @param compPO Purchase Order to validate
   * @return completable future which might be completed with {@code true} if order is valid, {@code false} if not valid or an exception if processing fails
   */
  public Future<List<Error>> validateExistingOrder(String orderId, CompositePurchaseOrder compPO,
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

    return configurationEntriesCache.loadConfiguration(ORDER_CONFIG_MODULE_NAME, requestContext)
      .compose(tenantConfig -> validateOrder(compPO, tenantConfig, requestContext))
      .map(errors -> {
        resultErrors.addAll(errors);
        return resultErrors;
      });
  }

  private Future<Void> setPoNumberIfMissing(CompositePurchaseOrder compPO, RequestContext requestContext) {
    logger.info("setPoNumberIfMissing :: orderId: {}", compPO.getId());
    if (null == compPO.getPoNumber()) {
      return poNumberHelper.generatePoNumber(requestContext)
        .compose(poNumber -> adjustPrefixAndSuffix(poNumber, compPO))
        .onSuccess(compPO::setPoNumber)
        .mapEmpty();
    }
    return Future.succeededFuture();
  }

  private Future<String> adjustPrefixAndSuffix(String poNumber, CompositePurchaseOrder compPO) {
    List<String> valuesToConcat = Arrays.asList(compPO.getPoNumberPrefix(), poNumber, compPO.getPoNumberSuffix());
    StringBuilder result = new StringBuilder();
    for (String value: valuesToConcat) {
      result.append(value == null ? "" : value);
    }
    return Future.succeededFuture(result.toString());
  }

  private Future<List<Error>> validateVendor(CompositePurchaseOrder compPO, RequestContext requestContext) {
    if (compPO.getWorkflowStatus() == WorkflowStatus.OPEN) {
      List<Error> combinedErrors = new ArrayList<>();
      return organizationService.validateVendor(compPO.getVendor(), requestContext)
        .map(aErrors -> combinedErrors.addAll(aErrors.getErrors()))
        .compose(errors -> fetchCompositePoLines(compPO, requestContext)
          .compose(poLines -> organizationService.validateAccessProviders(poLines, requestContext))
          .map(aErrors -> combinedErrors.addAll(aErrors.getErrors()))
          .map(v -> combinedErrors));
    }
    return Future.succeededFuture(Collections.emptyList());
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

  private Future<CompositePurchaseOrder> createPOandPOLines(CompositePurchaseOrder compPO, JsonObject cachedTenantConfiguration,
                                                                      RequestContext requestContext) {
    logger.info("createPOandPOLines :: orderId: {}", compPO.getId());
    final WorkflowStatus finalStatus = compPO.getWorkflowStatus();

    // we should always create PO and PO lines in PENDING status and transition to OPEN only when it's all set
    // (e.g. PO lines are created, Inventory is updated, etc.)
    if (finalStatus == OPEN) {
      compPO.setWorkflowStatus(PENDING);
    }

    return purchaseOrderStorageService.createPurchaseOrder(convertToPurchaseOrder(compPO), requestContext)
      .map(createdOrder -> compPO.withId(createdOrder.getId()))
      .compose(compPOWithId -> createPoLines(compPOWithId, requestContext))
      .map(compPO::withCompositePoLines)
      .compose(createdOrder -> {
        if (finalStatus == OPEN) {
          compPO.setWorkflowStatus(OPEN);
          return checkOrderApprovalRequired(compPO, requestContext)
            .compose(v -> purchaseOrderLineService.populateOrderLines(compPO, requestContext))
            .compose(po -> openCompositeOrderManager.process(po, null, cachedTenantConfiguration, requestContext))
            .compose(ok -> handleFinalOrderStatus(compPO, finalStatus.value(), requestContext));
        }
        return Future.succeededFuture();
      })
      .map(v -> compPO);
  }


  /**
   * Checks the value of "isApprovalRequired" in configurations, if the value is set to true, and order is being approved, verifies
   * if the user has required permissions to approve order
   *
   * @param compPO composite purchase order for checking permissions
   */
  private Future<Void> checkOrderApprovalPermissions(CompositePurchaseOrder compPO, RequestContext requestContext) {
    return configurationEntriesCache.loadConfiguration(ORDER_CONFIG_MODULE_NAME, requestContext)
      .map(tenantConfig -> {
        boolean isApprovalRequired = isApprovalRequiredConfiguration(tenantConfig);
        if (isApprovalRequired && compPO.getApproved()
          .equals(Boolean.TRUE)) {
          if (userDoesNotHaveApprovePermission(requestContext)) {
            throw new HttpException(HttpStatus.HTTP_FORBIDDEN.toInt(), USER_HAS_NO_APPROVAL_PERMISSIONS);
          }
          compPO.setApprovalDate(new Date());
          compPO.setApprovedById(getCurrentUserId(requestContext.getHeaders()));
        }
        return null;
      });

  }

  private void checkOrderUnopenPermissions(RequestContext requestContext) {
    if (userDoesNotHaveUnopenPermission(requestContext)) {
      throw new HttpException(HttpStatus.HTTP_FORBIDDEN.toInt(), USER_HAS_NO_UNOPEN_PERMISSIONS);
    }
  }

  private void checkOrderReopenPermissions(RequestContext requestContext) {
    if (userDoesNotHaveReopenPermission(requestContext)) {
      throw new HttpException(HttpStatus.HTTP_FORBIDDEN.toInt(), USER_HAS_NO_REOPEN_PERMISSIONS);
    }
  }

  public static boolean isApprovalRequiredConfiguration(JsonObject config) {
    return Optional.ofNullable(config.getString("approvals"))
      .map(approval -> new JsonObject(approval).getBoolean("isApprovalRequired"))
      .orElse(false);
  }

  /**
   * If an order is transitioning to OPEN, checks if approval is required and throws an error if it is not approved
   *
   * @param compPO composite purchase order
   */
  private Future<Void> checkOrderApprovalRequired(CompositePurchaseOrder compPO, RequestContext requestContext) {
    return configurationEntriesCache.loadConfiguration(ORDER_CONFIG_MODULE_NAME, requestContext)
      .map(tenantConfig -> {
        boolean isApprovalRequired = isApprovalRequiredConfiguration(tenantConfig);
        if (isApprovalRequired && !compPO.getApproved().equals(Boolean.TRUE)) {
          throw new HttpException(400, APPROVAL_REQUIRED_TO_OPEN);
        }
        compPO.setApprovedById(getCurrentUserId(requestContext.getHeaders()));
        compPO.setApprovalDate(new Date());
        return null;
      });
  }

  private Future<List<CompositePoLine>> createPoLines(CompositePurchaseOrder compPO, RequestContext requestContext) {
    List<Future<CompositePoLine>> futures =
      compPO.getCompositePoLines()
            .stream()
            .map(compositePoLine -> purchaseOrderLineHelper.createPoLine(compositePoLine, compPO, requestContext))
            .collect(Collectors.toList());
    return collectResultsOnSuccess(futures);
  }

  private Future<List<CompositePoLine>> fetchCompositePoLines(CompositePurchaseOrder compPO, RequestContext requestContext) {
    if (CollectionUtils.isEmpty(compPO.getCompositePoLines())) {
      return  purchaseOrderLineService.getCompositePoLinesByOrderId(compPO.getId(), requestContext)
        .map(poLines -> {
          PoLineCommonUtil.sortPoLinesByPoLineNumber(poLines);
          return poLines;
        });
    } else {
      return Future.succeededFuture(compPO.getCompositePoLines());
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
   *
   * @param compPO {@link CompositePurchaseOrder}
   * @return JsonObject representation of PurchaseOrder
   */
  private PurchaseOrder convertToPurchaseOrder(CompositePurchaseOrder compPO) {
    //nextPolNumber should be ignored if it exists in request payload and should be generated by DB hooks only [MODORDERS-903].
    compPO.setNextPolNumber(null);

    // Remove dynamically calculated data
    compPO.setTotalEstimatedPrice(null);
    compPO.setTotalItems(null);
    compPO.setTotalEncumbered(null);
    compPO.setTotalExpended(null);
    compPO.setNeedReEncumber(null);
    JsonObject purchaseOrder = JsonObject.mapFrom(compPO);
    purchaseOrder.remove(COMPOSITE_PO_LINES);
    return purchaseOrder.mapTo(PurchaseOrder.class);
  }

  /**
   * @param updatedOrder purchase order from request
   * @param poFromStorage purchase order from storage
   * @return completable future completed successfully if all checks pass or exceptionally in case of error/restriction
   *         caused by acquisitions units
   */
  private Future<Void> validateAcqUnitsOnUpdate(CompositePurchaseOrder updatedOrder, CompositePurchaseOrder poFromStorage,
                                                            RequestContext requestContext) {
    List<String> newAcqUnitIds = updatedOrder.getAcqUnitIds();
    List<String> acqUnitUdsFromStorage = poFromStorage.getAcqUnitIds();

    return Future.succeededFuture()
      .map(ok -> getInvolvedOperations(updatedOrder, poFromStorage))
      .compose(involvedOperations -> protectionService.validateAcqUnitsOnUpdate(newAcqUnitIds, acqUnitUdsFromStorage, MANAGE, involvedOperations, requestContext));
  }

  private Future<Void> processPoLineTags(CompositePurchaseOrder compPO, RequestContext requestContext) {
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
      return Future.succeededFuture();
    }
    return tagService.createTagsIfMissing(tagLabels, requestContext);
  }

  private Future<Void> closeOrder(CompositePurchaseOrder compPO, CompositePurchaseOrder poFromStorage, RequestContext requestContext) {
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

  private Future<Void> setCreateInventoryDefaultValues(CompositePurchaseOrder compPO, JsonObject tenantConfiguration) {
    logger.info("setCreateInventoryDefaultValues:: orderId: {}", compPO.getId());
    List<Future<Void>> futures = compPO.getCompositePoLines()
      .stream()
      .map(compPOL -> purchaseOrderLineHelper.setTenantDefaultCreateInventoryValues(compPOL, tenantConfiguration))
      .collect(toList());

    return GenericCompositeFuture.join(futures)
      .mapEmpty();
  }

  private Future<CompositePurchaseOrder> populateOrderSummary(CompositePurchaseOrder order, RequestContext requestContext) {
    logger.info("populateOrderSummary :: orderId: {}", order.getId());
    return orderLinesSummaryPopulateService.populate(new CompositeOrderRetrieveHolder(order), requestContext)
      .map(CompositeOrderRetrieveHolder::getOrder);
  }


  private Future<Void> updateItemsInInventory(List<JsonObject> items, RequestContext requestContext) {
    return GenericCompositeFuture.join(items.stream()
      .map(item -> inventoryItemManager.updateItem(item, requestContext))
      .collect(toList()))
      .mapEmpty();
  }

  private List<CompositePoLine> getNonPackageLines(List<CompositePoLine> compositePoLines) {
    return compositePoLines.stream().filter(line -> !line.getIsPackage()).collect(toList());
  }

  private List<JsonObject> updateStatusName(List<JsonObject> items, ItemStatus status) {
    items.forEach(item -> item.getJsonObject("status").put("name", status.value()));
    return items;
  }

  private Future<Void> updateItemsStatusInInventory(List<PoLine> poLines,
    ItemStatus currentStatus, ItemStatus newStatus, RequestContext requestContext) {

    if (CollectionUtils.isEmpty(poLines)) {
      return Future.succeededFuture();
    }
    return GenericCompositeFuture.join(
      StreamEx.ofSubLists(poLines, MAX_IDS_FOR_GET_RQ_15)
        .map(chunk -> VertxFutureRepeater.repeat(MAX_REPEAT_ON_FAILURE, () -> updateItemsStatus(chunk, currentStatus, newStatus, requestContext)))
        .collect(toList()))
      .mapEmpty();
  }

  private Future<Void> updateItemsStatus(List<PoLine> poLines,
    ItemStatus currentStatus, ItemStatus newStatus, RequestContext requestContext) {

    Map<String, List<String>> tenantIdToPoLineIds = getTenantIdsToPoLineIdsMap(poLines);
    List<Future<Void>> futures = new ArrayList<>();
    for (Map.Entry<String, List<String>> entry : tenantIdToPoLineIds.entrySet()) {
      String tenantId = entry.getKey();
      List<String> poLineIds = entry.getValue();
      RequestContext updatedContext = RequestContextUtil.createContextWithNewTenantId(requestContext, tenantId);
      Future<Void> updateItemFeature = inventoryItemManager.getItemsByPoLineIdsAndStatus(poLineIds, currentStatus.value(), updatedContext)
        .map(items -> updateStatusName(items, newStatus))
        .compose(items -> updateItemsInInventory(items, requestContext));
      futures.add(updateItemFeature);
    }
    return GenericCompositeFuture.all(futures)
      .mapEmpty();
  }

  private Map<String, List<String>> getTenantIdsToPoLineIdsMap(List<PoLine> poLines) {
    Map<String, List<String>> result = new HashMap<>();
    for (PoLine poLine : poLines) {
      for (Location location : poLine.getLocations()) {
        String tenantId = location.getTenantId();
        if (!result.containsKey(tenantId)) {
          result.put(tenantId, new ArrayList<>());
        }
        result.get(tenantId).add(poLine.getId());
      }
    }
    return result;
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
