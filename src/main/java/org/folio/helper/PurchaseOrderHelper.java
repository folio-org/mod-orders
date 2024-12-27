package org.folio.helper;

import static java.util.stream.Collectors.toList;
import static org.apache.commons.collections4.CollectionUtils.isEmpty;
import static org.folio.orders.utils.HelperUtils.COMPOSITE_PO_LINES;
import static org.folio.orders.utils.HelperUtils.ORDER_CONFIG_MODULE_NAME;
import static org.folio.orders.utils.HelperUtils.REASON_CANCELLED;
import static org.folio.orders.utils.HelperUtils.WORKFLOW_STATUS;
import static org.folio.orders.utils.HelperUtils.collectResultsOnSuccess;
import static org.folio.orders.utils.HelperUtils.convertToCompositePurchaseOrder;
import static org.folio.orders.utils.OrderStatusTransitionUtil.isOrderClosing;
import static org.folio.orders.utils.OrderStatusTransitionUtil.isOrderReopening;
import static org.folio.orders.utils.OrderStatusTransitionUtil.isTransitionToClosed;
import static org.folio.orders.utils.OrderStatusTransitionUtil.isTransitionToOpen;
import static org.folio.orders.utils.OrderStatusTransitionUtil.isTransitionToReopen;
import static org.folio.orders.utils.POProtectedFields.getFieldNames;
import static org.folio.orders.utils.POProtectedFields.getFieldNamesForOpenOrder;
import static org.folio.orders.utils.PermissionsUtil.*;
import static org.folio.orders.utils.PoLineCommonUtil.verifyOngoingFieldsChanged;
import static org.folio.orders.utils.PoLineCommonUtil.verifyProtectedFieldsChanged;
import static org.folio.orders.utils.ProtectedOperationType.DELETE;
import static org.folio.rest.core.exceptions.ErrorCodes.USER_HAS_NO_REOPEN_PERMISSIONS;
import static org.folio.rest.jaxrs.model.CompositePurchaseOrder.WorkflowStatus.OPEN;
import static org.folio.rest.jaxrs.model.CompositePurchaseOrder.WorkflowStatus.PENDING;
import static org.folio.service.UserService.getCurrentUserId;
import static org.folio.service.orders.utils.StatusUtils.changeOrderStatusForOrderUpdate;

import java.util.ArrayList;
import java.util.Arrays;
import java.util.List;
import java.util.Map;
import java.util.Objects;
import java.util.Set;
import java.util.stream.Collectors;

import org.apache.commons.collections4.CollectionUtils;
import org.apache.commons.lang3.StringUtils;
import org.apache.logging.log4j.LogManager;
import org.apache.logging.log4j.Logger;
import org.folio.HttpStatus;
import org.folio.models.CompositeOrderRetrieveHolder;
import org.folio.models.ItemStatus;
import org.folio.orders.utils.HelperUtils;
import org.folio.orders.utils.PoLineCommonUtil;
import org.folio.orders.utils.ProtectedOperationType;
import org.folio.rest.RestConstants;
import org.folio.rest.core.exceptions.HttpException;
import org.folio.rest.core.models.RequestContext;
import org.folio.rest.jaxrs.model.CompositePoLine;
import org.folio.rest.jaxrs.model.CompositePoLine.PaymentStatus;
import org.folio.rest.jaxrs.model.CompositePoLine.ReceiptStatus;
import org.folio.rest.jaxrs.model.CompositePurchaseOrder;
import org.folio.rest.jaxrs.model.CompositePurchaseOrder.WorkflowStatus;
import org.folio.rest.jaxrs.model.Errors;
import org.folio.rest.jaxrs.model.PoLine;
import org.folio.rest.jaxrs.model.PurchaseOrder;
import org.folio.rest.jaxrs.model.PurchaseOrderCollection;
import org.folio.rest.jaxrs.model.Title;
import org.folio.service.ProtectionService;
import org.folio.service.TagService;
import org.folio.service.caches.ConfigurationEntriesCache;
import org.folio.service.finance.transaction.EncumbranceService;
import org.folio.service.finance.transaction.EncumbranceWorkflowStrategy;
import org.folio.service.finance.transaction.EncumbranceWorkflowStrategyFactory;
import org.folio.service.inventory.InventoryItemStatusSyncService;
import org.folio.service.orders.CompositeOrderDynamicDataPopulateService;
import org.folio.service.orders.CompositePoLineValidationService;
import org.folio.service.orders.OrderInvoiceRelationService;
import org.folio.service.orders.OrderValidationService;
import org.folio.service.orders.OrderWorkflowType;
import org.folio.service.orders.PurchaseOrderLineService;
import org.folio.service.orders.PurchaseOrderStorageService;
import org.folio.service.orders.flows.update.open.OpenCompositeOrderFlowValidator;
import org.folio.service.orders.flows.update.open.OpenCompositeOrderManager;
import org.folio.service.orders.flows.update.reopen.ReOpenCompositeOrderManager;
import org.folio.service.titles.TitlesService;

import io.vertx.core.Future;
import io.vertx.core.Promise;
import io.vertx.core.json.JsonObject;

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
  private final InventoryItemStatusSyncService itemStatusSyncService;
  private final OpenCompositeOrderManager openCompositeOrderManager;
  private final OpenCompositeOrderFlowValidator openCompositeOrderFlowValidator;
  private final PurchaseOrderStorageService purchaseOrderStorageService;
  private final ConfigurationEntriesCache configurationEntriesCache;
  private final PoNumberHelper poNumberHelper;
  private final ReOpenCompositeOrderManager reOpenCompositeOrderManager;
  private final OrderValidationService orderValidationService;
  private final CompositePoLineValidationService compositePoLineValidationService;

  public PurchaseOrderHelper(PurchaseOrderLineHelper purchaseOrderLineHelper,
      CompositeOrderDynamicDataPopulateService orderLinesSummaryPopulateService, EncumbranceService encumbranceService,
      CompositeOrderDynamicDataPopulateService combinedPopulateService,
      EncumbranceWorkflowStrategyFactory encumbranceWorkflowStrategyFactory,
      OrderInvoiceRelationService orderInvoiceRelationService, TagService tagService,
      PurchaseOrderLineService purchaseOrderLineService, TitlesService titlesService,
      ProtectionService protectionService, InventoryItemStatusSyncService itemStatusSyncService,
      OpenCompositeOrderManager openCompositeOrderManager, PurchaseOrderStorageService purchaseOrderStorageService,
      ConfigurationEntriesCache configurationEntriesCache, PoNumberHelper poNumberHelper,
      OpenCompositeOrderFlowValidator openCompositeOrderFlowValidator,
      ReOpenCompositeOrderManager reOpenCompositeOrderManager, OrderValidationService orderValidationService,
      CompositePoLineValidationService compositePoLineValidationService) {
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
    this.itemStatusSyncService = itemStatusSyncService;
    this.openCompositeOrderManager = openCompositeOrderManager;
    this.purchaseOrderStorageService = purchaseOrderStorageService;
    this.configurationEntriesCache = configurationEntriesCache;
    this.poNumberHelper = poNumberHelper;
    this.openCompositeOrderFlowValidator = openCompositeOrderFlowValidator;
    this.reOpenCompositeOrderManager = reOpenCompositeOrderManager;
    this.orderValidationService = orderValidationService;
    this.compositePoLineValidationService = compositePoLineValidationService;
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
      .compose(finalQuery -> purchaseOrderStorageService.getPurchaseOrders(finalQuery, limit, offset, requestContext))
      .onFailure(t -> logger.error("Error getting orders", t));
  }

  /**
   * Get purchase order by Id
   *
   * @param orderId purchase order id
   * @return completable future with {@link PurchaseOrder} object
   */
  public Future<PurchaseOrder> getPurchaseOrderById(String orderId, RequestContext requestContext) {
    logger.info("getPurchaseOrderById :: orderId: {}", orderId);
    return purchaseOrderStorageService.getPurchaseOrderById(orderId, requestContext);
  }

  public Future<CompositePurchaseOrder> postCompositeOrder(CompositePurchaseOrder compPO, RequestContext requestContext) {
    // First validate content of the PO and proceed only if all is okay
    return configurationEntriesCache.loadConfiguration(ORDER_CONFIG_MODULE_NAME, requestContext)
      .compose(tenantConfig -> orderValidationService.validateOrderForPost(compPO, tenantConfig, requestContext)
        .compose(errors -> {
          if (CollectionUtils.isEmpty(errors)) {
            logger.info("postCompositeOrder :: Creating PO and POLines...");
            return createPurchaseOrder(compPO, tenantConfig, requestContext)
              .onSuccess(po -> logger.info("postCompositeOrder :: Successfully Placed Order: {}",
                JsonObject.mapFrom(po).encodePrettily()));
          } else {
            throw new HttpException(422, new Errors().withErrors(errors)
              .withTotalRecords(errors.size()));
          }
        }))
      .onFailure(t -> logger.error("postCompositeOrder :: Failed to create order: {}",
        JsonObject.mapFrom(compPO).encodePrettily(), t));
  }

  /**
   * Create a purchase order (PO) and a number of PO lines if provided.
   * @param compPO {@link CompositePurchaseOrder} object representing Purchase Order and optionally Purchase Order Line details.
   * @return completable future with {@link CompositePurchaseOrder} object with populated uuid on success or an exception if processing fails
   */
  public Future<CompositePurchaseOrder> createPurchaseOrder(CompositePurchaseOrder compPO, JsonObject tenantConfiguration,
      RequestContext requestContext) {
    logger.info("createPurchaseOrder :: orderId: {}", compPO.getId());
    return orderValidationService.validateOrderForCreation(compPO, requestContext)
      .compose(v -> setPoNumberIfMissing(compPO, requestContext))
      .compose(v -> processPoLineTags(compPO, requestContext))
      .compose(v -> createPOandPOLines(compPO, tenantConfiguration, requestContext))
      .compose(aCompPO -> populateOrderSummary(aCompPO, requestContext))
      .compose(compOrder -> encumbranceService.updateEncumbrancesOrderStatusAndReleaseIfClosed(compOrder, requestContext)
        .map(v -> compOrder));
  }

  public Future<Void> putCompositeOrderById(String orderId, boolean deleteHoldings, CompositePurchaseOrder compPO,
      RequestContext requestContext) {

    // Set order id from path if not specified in body
    populateOrderId(orderId, compPO);

    return orderValidationService.validateOrderForPut(orderId, compPO, requestContext)
      .map(validationErrors -> {
        if (CollectionUtils.isNotEmpty(validationErrors)) {
          Errors errors = new Errors().withErrors(validationErrors).withTotalRecords(validationErrors.size());
          logger.error("putCompositeOrderById :: Validation error. Failed to update purchase order : {}",
            JsonObject.mapFrom(errors).encodePrettily());
          throw new HttpException(RestConstants.VALIDATION_ERROR, errors);
        }
        return null;
      }).compose(v -> updateOrder(compPO, deleteHoldings, requestContext))
      .onSuccess(v -> logger.info("putCompositeOrderById :: Successfully updated order: {}", compPO.getId()))
      .onFailure(t -> logger.error("putCompositeOrderById :: Failed to update order: {}",
        JsonObject.mapFrom(compPO).encodePrettily(), t));
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
        CompositePurchaseOrder clonedPoFromStorage = JsonObject.mapFrom(poFromStorage).mapTo(CompositePurchaseOrder.class);
        boolean isTransitionToOpen = isTransitionToOpen(poFromStorage, compPO);
        return orderValidationService.validateOrderForUpdate(compPO, poFromStorage, deleteHoldings, requestContext)
          .compose(v -> {
            if (isTransitionToOpen && CollectionUtils.isEmpty(compPO.getCompositePoLines())) {
              compPO.setCompositePoLines(clonedPoFromStorage.getCompositePoLines());
            }
            return Future.succeededFuture();
          })
          .compose(v -> validatePurchaseOrderHasPoLines(compPO))
          .compose(v -> validateUserUnaffiliatedPoLineLocations(compPO, requestContext))
          .compose(v -> {
            if (isTransitionToClosed(poFromStorage, compPO)) {
              return closeOrder(compPO, poFromStorage, requestContext);
            }
            return Future.succeededFuture();
          })
          .compose(v -> {
            if (isTransitionToOpen) {
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
              return orderValidationService.checkOrderApprovalRequired(compPO, requestContext)
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

  private Future<Void> validatePurchaseOrderHasPoLines(CompositePurchaseOrder purchaseOrder) {
    return compositePoLineValidationService.validatePurchaseOrderHasPoLines(purchaseOrder.getCompositePoLines());
  }

  private Future<List<Void>> validateUserUnaffiliatedPoLineLocations(CompositePurchaseOrder purchaseOrder, RequestContext requestContext) {
    var poLineFutures = new ArrayList<Future<Void>>();
    purchaseOrder.getCompositePoLines().stream().filter(Objects::nonNull).forEach(poLine -> {
      var poLineFuture = compositePoLineValidationService.validateUserUnaffiliatedLocations(poLine.getId(), poLine.getLocations(), requestContext);
      poLineFutures.add(poLineFuture);
    });
    return collectResultsOnSuccess(poLineFutures);
  }

  public Future<Void> handleFinalOrderStatus(CompositePurchaseOrder compPO, String initialOrdersStatus,
                                                        RequestContext requestContext) {
    PurchaseOrder purchaseOrder = convertToPurchaseOrder(compPO);

    return Future.succeededFuture()
      .compose(v -> {
        if (isEmpty(compPO.getCompositePoLines())) {
          return purchaseOrderLineService.getPoLinesByOrderId(compPO.getId(), requestContext);
        } else {
          List<PoLine> poLines = PoLineCommonUtil.convertToPoLines(compPO.getCompositePoLines());
          changeOrderStatusForOrderUpdate(purchaseOrder, poLines);
          return Future.succeededFuture(poLines);
        }
      })
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
      return itemStatusSyncService.updateItemStatusesInInventory(poLines, ItemStatus.ON_ORDER, ItemStatus.ORDER_CLOSED, requestContext);
    } else if (isOrderReopening(purchaseOrder.getWorkflowStatus(), initialOrdersStatus)) {
      return itemStatusSyncService.updateItemStatusesInInventory(poLines, ItemStatus.ORDER_CLOSED, ItemStatus.ON_ORDER, requestContext);
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
              logger.info("deleteOrder :: Successfully deleted poLines, proceeding with purchase order");
              return purchaseOrderStorageService.deleteOrderById(orderId, requestContext)
                .onSuccess(rs -> {
                  logger.info("deleteOrder :: Successfully deleted order with id={}", orderId);
                  promise.complete();
                })
                .onFailure(t -> {
                  logger.error("deleteOrder :: Failed to delete the order with id={}", orderId, t.getCause());
                  promise.fail(t);
                });
            })
            .onFailure(t -> {
              logger.error("deleteOrder :: Failed to delete PO Lines of the order with id={}", orderId, t.getCause());
              promise.fail(t);
            })
          )
          .onFailure(t -> {
            logger.error("deleteOrder :: User with id={} is forbidden to view delete with id={}",
              getCurrentUserId(requestContext.getHeaders()), orderId, t.getCause());
            promise.fail(t);
          });
        return null;
      })
      .onFailure(t -> {
        logger.error("deleteOrder :: Failed to delete PO Lines", t);
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
          logger.error("getCompositeOrder :: User with id={} is forbidden to view order with id={}",
            getCurrentUserId(requestContext.getHeaders()), orderId, t.getCause());
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
        logger.error("getCompositeOrder :: Failed to build composite purchase order with id={}", orderId, t.getCause());
        promise.fail(t);
      });
    return promise.future();
  }

  private void populateOrderId(String orderId, CompositePurchaseOrder compPO) {
    if (StringUtils.isEmpty(compPO.getId())) {
      compPO.setId(orderId);
    }
    if (CollectionUtils.isNotEmpty(compPO.getCompositePoLines())) {
      compPO.getCompositePoLines().forEach(poLine -> {
        if (StringUtils.isEmpty(poLine.getPurchaseOrderId())) {
          poLine.setPurchaseOrderId(orderId);
        }
      });
    }
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
          return orderValidationService.checkOrderApprovalRequired(compPO, requestContext)
            .compose(v -> purchaseOrderLineService.populateOrderLines(compPO, requestContext))
            .compose(po -> openCompositeOrderManager.process(po, null, cachedTenantConfiguration, requestContext))
            .compose(ok -> handleFinalOrderStatus(compPO, finalStatus.value(), requestContext));
        }
        return Future.succeededFuture();
      })
      .map(v -> compPO);
  }

  private void checkOrderReopenPermissions(RequestContext requestContext) {
    if (userDoesNotHaveReopenPermission(requestContext)) {
      throw new HttpException(HttpStatus.HTTP_FORBIDDEN.toInt(), USER_HAS_NO_REOPEN_PERMISSIONS);
    }
  }

  private Future<List<CompositePoLine>> createPoLines(CompositePurchaseOrder compPO, RequestContext requestContext) {
    List<Future<CompositePoLine>> futures =
      compPO.getCompositePoLines()
            .stream()
            .map(compositePoLine -> purchaseOrderLineHelper.createPoLineWithOrder(compositePoLine, compPO, requestContext))
            .collect(Collectors.toList());
    return collectResultsOnSuccess(futures);
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
    compPO.setTotalCredited(null);
    compPO.setNeedReEncumber(null);
    JsonObject purchaseOrder = JsonObject.mapFrom(compPO);
    purchaseOrder.remove(COMPOSITE_PO_LINES);
    return purchaseOrder.mapTo(PurchaseOrder.class);
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

  private Future<CompositePurchaseOrder> populateOrderSummary(CompositePurchaseOrder order, RequestContext requestContext) {
    logger.info("populateOrderSummary :: orderId: {}", order.getId());
    return orderLinesSummaryPopulateService.populate(new CompositeOrderRetrieveHolder(order), requestContext)
      .map(CompositeOrderRetrieveHolder::getOrder);
  }


  private List<CompositePoLine> getNonPackageLines(List<CompositePoLine> compositePoLines) {
    return compositePoLines.stream().filter(line -> !line.getIsPackage()).collect(toList());
  }

}
