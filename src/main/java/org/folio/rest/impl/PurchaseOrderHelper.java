package org.folio.rest.impl;

import static java.util.concurrent.CompletableFuture.completedFuture;
import static java.util.stream.Collectors.toList;
import static org.apache.commons.collections4.CollectionUtils.isNotEmpty;
import static org.folio.orders.utils.AcqDesiredPermissions.ASSIGN;
import static org.folio.orders.utils.AcqDesiredPermissions.MANAGE;
import static org.folio.orders.utils.ErrorCodes.APPROVAL_REQUIRED_TO_OPEN;
import static org.folio.orders.utils.ErrorCodes.MISSING_ONGOING;
import static org.folio.orders.utils.ErrorCodes.ONGOING_NOT_ALLOWED;
import static org.folio.orders.utils.ErrorCodes.USER_HAS_NO_ACQ_PERMISSIONS;
import static org.folio.orders.utils.ErrorCodes.USER_HAS_NO_APPROVAL_PERMISSIONS;
import static org.folio.orders.utils.HelperUtils.COMPOSITE_PO_LINES;
import static org.folio.orders.utils.HelperUtils.WORKFLOW_STATUS;
import static org.folio.orders.utils.HelperUtils.buildQuery;
import static org.folio.orders.utils.HelperUtils.calculateTotalEstimatedPrice;
import static org.folio.orders.utils.HelperUtils.changeOrderStatus;
import static org.folio.orders.utils.HelperUtils.combineCqlExpressions;
import static org.folio.orders.utils.HelperUtils.convertToCompositePurchaseOrder;
import static org.folio.orders.utils.HelperUtils.deletePoLine;
import static org.folio.orders.utils.HelperUtils.deletePoLines;
import static org.folio.orders.utils.HelperUtils.getCompositePoLines;
import static org.folio.orders.utils.HelperUtils.getPoLineLimit;
import static org.folio.orders.utils.HelperUtils.getPoLines;
import static org.folio.orders.utils.HelperUtils.getPurchaseOrderById;
import static org.folio.orders.utils.HelperUtils.handleDeleteRequest;
import static org.folio.orders.utils.HelperUtils.handleGetRequest;
import static org.folio.orders.utils.HelperUtils.handlePutRequest;
import static org.folio.orders.utils.HelperUtils.verifyNonPackageTitles;
import static org.folio.orders.utils.HelperUtils.verifyProtectedFieldsChanged;
import static org.folio.orders.utils.POProtectedFields.getFieldNames;
import static org.folio.orders.utils.POProtectedFields.getFieldNamesForOpenOrder;
import static org.folio.orders.utils.ProtectedOperationType.CREATE;
import static org.folio.orders.utils.ProtectedOperationType.DELETE;
import static org.folio.orders.utils.ProtectedOperationType.UPDATE;
import static org.folio.orders.utils.ResourcePathResolver.PO_LINE_NUMBER;
import static org.folio.orders.utils.ResourcePathResolver.PURCHASE_ORDER;
import static org.folio.orders.utils.ResourcePathResolver.SEARCH_ORDERS;
import static org.folio.orders.utils.ResourcePathResolver.resourceByIdPath;
import static org.folio.orders.utils.ResourcePathResolver.resourcesPath;
import static org.folio.rest.RestVerticle.OKAPI_HEADER_PERMISSIONS;
import static org.folio.rest.jaxrs.model.CompositePurchaseOrder.WorkflowStatus.OPEN;
import static org.folio.rest.jaxrs.model.CompositePurchaseOrder.WorkflowStatus.PENDING;

import java.util.ArrayList;
import java.util.Collections;
import java.util.Date;
import java.util.HashSet;
import java.util.Iterator;
import java.util.List;
import java.util.Map;
import java.util.Objects;
import java.util.Optional;
import java.util.Set;
import java.util.concurrent.CompletableFuture;
import java.util.concurrent.CompletionStage;
import java.util.stream.Collectors;
import java.util.stream.Stream;

import org.apache.commons.collections4.CollectionUtils;
import org.apache.commons.collections4.ListUtils;
import org.apache.commons.lang3.StringUtils;
import org.folio.HttpStatus;
import org.folio.orders.rest.exceptions.HttpException;
import org.folio.orders.utils.AcqDesiredPermissions;
import org.folio.orders.utils.ErrorCodes;
import org.folio.orders.utils.HelperUtils;
import org.folio.orders.utils.POLineProtectedFields;
import org.folio.orders.utils.ProtectedOperationType;
import org.folio.rest.jaxrs.model.CompositePoLine;
import org.folio.rest.jaxrs.model.CompositePurchaseOrder;
import org.folio.rest.jaxrs.model.CompositePurchaseOrder.WorkflowStatus;
import org.folio.rest.jaxrs.model.Error;
import org.folio.rest.jaxrs.model.Errors;
import org.folio.rest.jaxrs.model.Parameter;
import org.folio.rest.jaxrs.model.PoLine;
import org.folio.rest.jaxrs.model.PurchaseOrder;
import org.folio.rest.jaxrs.model.PurchaseOrders;
import org.folio.rest.jaxrs.model.Title;

import io.vertx.core.Context;
import io.vertx.core.json.JsonArray;
import io.vertx.core.json.JsonObject;
import me.escoffier.vertx.completablefuture.VertxCompletableFuture;

public class PurchaseOrderHelper extends AbstractHelper {

  private static final String PERMISSION_ORDER_APPROVE = "orders.item.approve";
  private static final String SEARCH_ORDERS_BY_LINES_DATA = resourcesPath(SEARCH_ORDERS) + SEARCH_PARAMS;
  public static final String GET_PURCHASE_ORDERS = resourcesPath(PURCHASE_ORDER) + SEARCH_PARAMS;
  public static final String EMPTY_ARRAY = "[]";

  // Using variable to "cache" lines for particular order base on assumption that the helper is stateful and new instance is used
  private List<PoLine> orderLines;

  private final PoNumberHelper poNumberHelper;
  private final PurchaseOrderLineHelper orderLineHelper;
  private final ProtectionHelper protectionHelper;
  private TitlesHelper titlesHelper;


  PurchaseOrderHelper(Map<String, String> okapiHeaders, Context ctx, String lang) {
    super(getHttpClient(okapiHeaders), okapiHeaders, ctx, lang);

    poNumberHelper = new PoNumberHelper(httpClient, okapiHeaders, ctx, lang);
    orderLineHelper = new PurchaseOrderLineHelper(httpClient, okapiHeaders, ctx, lang);
    protectionHelper = new ProtectionHelper(httpClient, okapiHeaders, ctx, lang);
    titlesHelper = new TitlesHelper(httpClient, okapiHeaders, ctx, lang);
  }

  /**
   * Retrieve a list of {@link PurchaseOrder} objects retrieved from storage by provided query.
   *
   * @param limit limit the number of elements returned in the response
   * @param offset skip over a number of elements by specifying an offset value for the query
   * @param query A query expressed as a CQL string using valid searchable fields.
   * @return completable future with {@link PurchaseOrders} object on success or an exception if processing fails
   */
  public CompletableFuture<PurchaseOrders> getPurchaseOrders(int limit, int offset, String query) {
    CompletableFuture<PurchaseOrders> future = new VertxCompletableFuture<>(ctx);

    try {
      buildGetOrdersPath(limit, offset, query)
        .thenCompose(endpoint -> handleGetRequest(endpoint, httpClient, ctx, okapiHeaders, logger))
        .thenAccept(jsonOrders -> future.complete(jsonOrders.mapTo(PurchaseOrders.class)))
        .exceptionally(t -> {
          logger.error("Error getting orders", t);
          future.completeExceptionally(t);
          return null;
        });
    } catch (Exception e) {
      future.completeExceptionally(e);
    }
    return future;
  }

  private CompletableFuture<String> buildGetOrdersPath(int limit, int offset, String query) {
    AcquisitionsUnitsHelper acqUnitsHelper = new AcquisitionsUnitsHelper(httpClient, okapiHeaders, ctx, lang);
    return acqUnitsHelper.buildAcqUnitsCqlExprToSearchRecords()
      .thenApply(acqUnitsCqlExpr -> {
        if (StringUtils.isEmpty(query)) {
          String queryParam = buildQuery(acqUnitsCqlExpr, logger);
          return String.format(GET_PURCHASE_ORDERS, limit, offset, queryParam, lang);
        } else {
          String queryParam = buildQuery(combineCqlExpressions("and", acqUnitsCqlExpr, query), logger);
          return String.format(SEARCH_ORDERS_BY_LINES_DATA, limit, offset, queryParam, lang);
        }
      });
  }

  /**
   * Create a purchase order (PO) and a number of PO lines if provided.
   * @param compPO {@link CompositePurchaseOrder} object representing Purchase Order and optionally Purchase Order Line details.
   * @return completable future with {@link CompositePurchaseOrder} object with populated uuid on success or an exception if processing fails
   */
  public CompletableFuture<CompositePurchaseOrder> createPurchaseOrder(CompositePurchaseOrder compPO) {

    return validateAcqUnitsOnCreate(compPO.getAcqUnitIds())
        .thenCompose(ok -> checkOrderApprovalPermissions(compPO))
        .thenCompose(ok -> setPoNumberIfMissing(compPO)
        .thenCompose(v -> poNumberHelper.checkPONumberUnique(compPO.getPoNumber()))
        .thenCompose(v -> createPOandPOLines(compPO))
        .thenApply(this::populateOrderSummary));
  }

  /**
   * @param acqUnitIds acquisitions units assigned to purchase order from request
   * @return completable future completed successfully if all checks pass or exceptionally in case of error/restriction
   *         caused by acquisitions units
   */
  private CompletableFuture<Void> validateAcqUnitsOnCreate(List<String> acqUnitIds) {
    if (acqUnitIds.isEmpty()) {
      return completedFuture(null);
    }

    return VertxCompletableFuture.runAsync(ctx, () -> verifyUserHasAssignPermission(acqUnitIds))
      .thenCompose(ok -> protectionHelper.verifyIfUnitsAreActive(acqUnitIds))
      .thenCompose(ok -> protectionHelper.isOperationRestricted(acqUnitIds, ProtectedOperationType.CREATE));
  }

  /**
   * Handles update of the order. First retrieve the PO from storage and depending on its content handle passed PO.
   * @param compPO updated {@link CompositePurchaseOrder} purchase order
   * @return completable future holding response indicating success (204 No Content) or error if failed
   */
  public CompletableFuture<Void> updateOrder(CompositePurchaseOrder compPO) {
    return getPurchaseOrderById(compPO.getId(), lang, httpClient, ctx, okapiHeaders, logger)
      .thenCompose(jsonPoFromStorage -> validateIfPOProtectedFieldsChanged(compPO, jsonPoFromStorage))
      .thenApply(HelperUtils::convertToCompositePurchaseOrder)
      .thenCompose(poFromStorage -> validateAcqUnitsOnUpdate(compPO, poFromStorage)
        .thenCompose(ok -> validatePoNumber(poFromStorage, compPO))
        .thenCompose(ok -> {
          if (isTransitionToApproved(poFromStorage, compPO)) {
            return checkOrderApprovalPermissions(compPO);
          }
          return completedFuture(null);
        })
        .thenCompose(v -> updatePoLines(poFromStorage, compPO))
        .thenCompose(v -> {
          if (isTransitionToOpen(poFromStorage, compPO)) {
            return checkOrderApprovalRequired(compPO).thenCompose(ok -> openOrder(compPO));
          } else {
            return updateOrderSummary(compPO);
          }
        }));
  }

  private boolean isTransitionToApproved(CompositePurchaseOrder poFromStorage, CompositePurchaseOrder compPO) {
    return !poFromStorage.getApproved() && compPO.getApproved();
  }

  private CompletableFuture<Set<ProtectedOperationType>> getInvolvedOperations(CompositePurchaseOrder compPO) {
    if (CollectionUtils.isEmpty(compPO.getCompositePoLines())) {
      return CompletableFuture.completedFuture(Collections.singleton(UPDATE));

    }
    return fetchOrderLinesByOrderId(compPO.getId())
      .thenApply(poLines -> {
        Set<String> newIds = compPO.getCompositePoLines().stream().map(CompositePoLine::getId).collect(Collectors.toSet());
        Set<String> storageIds = poLines.stream().map(PoLine::getId).collect(Collectors.toSet());
        Set<ProtectedOperationType> operations = new HashSet<>();
        operations.add(UPDATE);
        operations.addAll(getInvolvedOperations(newIds, storageIds));
        return operations;
      });
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


  public CompletableFuture<JsonObject> validateIfPOProtectedFieldsChanged(CompositePurchaseOrder compPO,
      JsonObject compPOFromStorage) {
    WorkflowStatus storagePOWorkflowStatus = WorkflowStatus.fromValue(compPOFromStorage.getString(WORKFLOW_STATUS));
    if (!PENDING.equals(storagePOWorkflowStatus)) {
      List<String> fieldNames = OPEN.equals(storagePOWorkflowStatus) ? getFieldNamesForOpenOrder() : getFieldNames();
      verifyProtectedFieldsChanged(fieldNames, compPOFromStorage, JsonObject.mapFrom(compPO));
    }
    return completedFuture(compPOFromStorage);
  }

  private JsonObject findCorrespondingCompositePoLine(CompositePoLine poLine, List<PoLine> poLinesFromStorage) {
    return poLinesFromStorage.stream()
      .filter(line -> line.getId()
        .equals(poLine.getId()))
      .findFirst()
      .map(JsonObject::mapFrom)
      .orElse(null);
  }

  /**
   * Delete a purchase order with given uuid. As a first step the logic deletes all associated PO Lines and then order.
   * @param id purchase order id
   * @return completable future which is just completed with nothing on success or an exception if processing fails
   */
  public CompletableFuture<Void> deleteOrder(String id) {
    CompletableFuture<Void> future = new VertxCompletableFuture<>(ctx);

    getPurchaseOrderById(id, lang, httpClient, ctx, okapiHeaders, logger)
      .thenAccept(purchaseOrder -> {
        CompositePurchaseOrder compPo = convertToCompositePurchaseOrder(purchaseOrder);
        protectionHelper.isOperationRestricted(compPo.getAcqUnitIds(), DELETE)
          .thenAccept(aVoid -> deletePoLines(id, lang, httpClient, ctx, okapiHeaders, logger)
            .thenRun(() -> {
              logger.info("Successfully deleted poLines, proceeding with purchase order");
              handleDeleteRequest(resourceByIdPath(PURCHASE_ORDER, id), httpClient, ctx, okapiHeaders, logger)
                .thenAccept(rs -> {
                  logger.info("Successfully deleted order with id={}", id);
                  future.complete(null);
                })
                .exceptionally(t -> {
                  logger.error("Failed to delete the order with id={}", t.getCause(), id);
                  future.completeExceptionally(t);
                  return null;
                });
            })
            .exceptionally(t -> {
              logger.error("Failed to delete PO Lines of the order with id={}", t.getCause(), id);
              future.completeExceptionally(t);
              return null;
            })
          )
          .exceptionally(t -> {
            logger.error("User with id={} is forbidden to view delete with id={}", t.getCause(), getCurrentUserId(), id);
            future.completeExceptionally(t);
            return null;
          });
      })
    .exceptionally(t ->{
      logger.error("Failed to delete PO Lines", t);
      future.completeExceptionally(t);
      return null;
    });

    return future;
  }

  /**
   * Gets purchase order by id
   *
   * @param id purchase order uuid
   * @return completable future with {@link CompositePurchaseOrder} on success or an exception if processing fails
   */
  public CompletableFuture<CompositePurchaseOrder> getCompositeOrder(String id) {

    CompletableFuture<CompositePurchaseOrder> future = new VertxCompletableFuture<>(ctx);

    getPurchaseOrderById(id, lang, httpClient, ctx, okapiHeaders, logger)
      .thenApply(HelperUtils::convertToCompositePurchaseOrder)
      .thenAccept(compPO -> protectionHelper.isOperationRestricted(compPO.getAcqUnitIds(), ProtectedOperationType.READ)
        .thenAccept(ok -> fetchCompositePoLines(compPO)
          .thenCompose(this::fetchNonPackageTitles)
          .thenAccept(linesIdTitles -> populateInstanceId(linesIdTitles, compPO.getCompositePoLines()))
          .thenApply(v -> populateOrderSummary(compPO))
          .thenAccept(future::complete)
          .exceptionally(t -> {
            logger.error("Failed to get lines for order with id={}", t.getCause(), id);
            future.completeExceptionally(t);
            return null;
          }))
        .exceptionally(t -> {
          logger.error("User with id={} is forbidden to view order with id={}", t.getCause(), getCurrentUserId(), id);
          future.completeExceptionally(t);
          return null;
        }))
      .exceptionally(t -> {
        logger.error("Failed to build composite purchase order with id={}", t.getCause(), id);
        future.completeExceptionally(t);
        return null;
      });

    return future;
  }

  private CompositePurchaseOrder populateOrderSummary(CompositePurchaseOrder compPO) {
    List<CompositePoLine> compositePoLines = compPO.getCompositePoLines();
    compPO.setTotalEstimatedPrice(calculateTotalEstimatedPrice(compositePoLines));
    compPO.setTotalItems(calculateTotalItemsQuantity(compositePoLines));

    return compPO;
  }

  private int calculateTotalItemsQuantity(List<CompositePoLine> poLines) {
    return poLines.stream().mapToInt(HelperUtils::calculateTotalQuantity).sum();
  }

  /**
   * Handles transition of given order to OPEN status.
   *
   * @param compPO Purchase Order to open
   * @return CompletableFuture that indicates when transition is completed
   */
  public CompletableFuture<Void> openOrder(CompositePurchaseOrder compPO) {
    compPO.setWorkflowStatus(OPEN);
    compPO.setDateOrdered(new Date());
    return fetchCompositePoLines(compPO)
      .thenCompose(this::fetchNonPackageTitles)
      .thenCompose(lineIdTitles -> {
        populateInstanceId(lineIdTitles, compPO.getCompositePoLines());
        verifyNonPackageTitles(lineIdTitles, getNonPackageLineIds(compPO.getCompositePoLines()));
        return updateInventory(compPO)
          .thenCompose(ok -> createEncumbrances(compPO))
          .thenCompose(ok -> updateTitlesInstanceId(lineIdTitles, compPO))
          .thenAccept(ok -> changePoLineStatuses(compPO))
          .thenCompose(ok -> updatePoLinesSummary(compPO))
          .thenCompose(ok -> updateOrderSummary(compPO));
      });
  }

  private CompletableFuture<Void> updateTitlesInstanceId(Map<String, List<Title>> lineIdTitles, CompositePurchaseOrder compPO) {
    return  VertxCompletableFuture.allOf(ctx,
        getNonPackageLines(compPO.getCompositePoLines()).stream()
          .map(line -> titlesHelper.updateTitle(lineIdTitles.get(line.getId()).get(0).withInstanceId(line.getInstanceId()))          )
          .toArray(CompletableFuture[]::new));
  }


  private CompletableFuture<Map<String, List<Title>>> fetchNonPackageTitles(CompositePurchaseOrder compPO) {
    List<String> lineIds = getNonPackageLineIds(compPO.getCompositePoLines());
    return titlesHelper.getTitlesByPoLineIds(lineIds);
  }

  private List<CompositePoLine> getNonPackageLines(List<CompositePoLine> compositePoLines) {
    return compositePoLines.stream().filter(line -> !line.getIsPackage()).collect(toList());
  }

  private List<String> getNonPackageLineIds(List<CompositePoLine> compositePoLines) {
    return compositePoLines.stream().filter(line -> !line.getIsPackage()).map(CompositePoLine::getId).collect(toList());
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
  public CompletableFuture<Boolean> validateOrder(CompositePurchaseOrder compPO) {

    return setCreateInventoryDefaultValues(compPO)
      .thenAccept(v -> addProcessingErrors(HelperUtils.validateOrder(compPO)))
      .thenCompose(v -> validateIsbnValues(compPO))
      .thenCompose(v -> validatePoLineLimit(compPO))
      .thenCompose(v -> validateVendor(compPO))
      .thenAccept(v -> validateRenewalInfo(compPO))
      .thenApply(v -> getErrors().isEmpty());
  }


  CompletableFuture<Void> validateIsbnValues(CompositePurchaseOrder compPO) {
    CompletableFuture[] futures = compPO.getCompositePoLines()
      .stream()
      .map(orderLineHelper::validateAndNormalizeISBN)
      .toArray(CompletableFuture[]::new);

    return VertxCompletableFuture.allOf(ctx, futures);
  }


  CompletableFuture<Void> setCreateInventoryDefaultValues(CompositePurchaseOrder compPO) {
    CompletableFuture[] futures = compPO.getCompositePoLines()
      .stream()
      .map(orderLineHelper::setTenantDefaultCreateInventoryValues)
      .toArray(CompletableFuture[]::new);

    return VertxCompletableFuture.allOf(ctx, futures);
  }

  /**
   * Validates purchase order which already exists in the storage.
   * Checks PO Number presence, validates that provided order id corresponds to one set in order and its lines.
   * If all is okay, {@link #validateOrder(CompositePurchaseOrder)} is called afterwards.
   * @param orderId Purchase Order id
   * @param compPO Purchase Order to validate
   * @return completable future which might be completed with {@code true} if order is valid, {@code false} if not valid or an exception if processing fails
   */
  public CompletableFuture<Boolean> validateExistingOrder(String orderId, CompositePurchaseOrder compPO) {
    // The PO Number is required for existing orders
    if (StringUtils.isEmpty(compPO.getPoNumber())) {
      addProcessingError(ErrorCodes.PO_NUMBER_REQUIRED.toError());
    }

    // Validate order uuid
    if (!compPO.getId().equals(orderId)) {
      addProcessingError(ErrorCodes.MISMATCH_BETWEEN_ID_IN_PATH_AND_BODY.toError());
    } else if (CollectionUtils.isNotEmpty(compPO.getCompositePoLines())) {
      // Validate that each PO Line has correct order id
      compPO.getCompositePoLines().forEach(poLine -> {
        if (!orderId.equals(poLine.getPurchaseOrderId())) {
          Error error = ErrorCodes.MISMATCH_BETWEEN_ID_IN_PATH_AND_BODY.toError();
          if (StringUtils.isNotEmpty(poLine.getPoLineNumber())) {
            error.getParameters()
                 .add(new Parameter().withKey(PO_LINE_NUMBER)
                                     .withValue(poLine.getPoLineNumber()));
          }
          addProcessingError(error);
        }
      });
    }

    return validateOrder(compPO);
  }

  @Override
  protected Errors getProcessingErrors() {
    addProcessingErrors(orderLineHelper.getErrors());
    return super.getProcessingErrors();
  }

  private CompletableFuture<Void> setPoNumberIfMissing(CompositePurchaseOrder compPO) {
    if (null == compPO.getPoNumber()) {
      return poNumberHelper.generatePoNumber()
                           .thenAccept(compPO::setPoNumber);
    }
    return completedFuture(null);
  }

  private CompletableFuture<Void> validateVendor(CompositePurchaseOrder compPO) {
    if (compPO.getWorkflowStatus() == WorkflowStatus.OPEN) {
      VendorHelper vendorHelper = new VendorHelper(httpClient, okapiHeaders, ctx, lang);
      return fetchCompositePoLines(compPO)
        .thenCompose(vendorHelper::validateVendor)
        .thenCompose(errors -> {
          addProcessingErrors(errors.getErrors());
          return vendorHelper.validateAccessProviders(compPO);
        })
        .thenAccept(errors -> addProcessingErrors(errors.getErrors()));
    }
    return completedFuture(null);
  }

  private void validateRenewalInfo(CompositePurchaseOrder compPO) {
    if (compPO.getOrderType() == CompositePurchaseOrder.OrderType.ONGOING && Objects.isNull(compPO.getOngoing())) {
      addProcessingError(MISSING_ONGOING.toError());
    } else if (compPO.getOrderType() == CompositePurchaseOrder.OrderType.ONE_TIME && Objects.nonNull(compPO.getOngoing())) {
      addProcessingError(ONGOING_NOT_ALLOWED.toError());
    }
  }

  private CompletableFuture<Void> validatePoLineLimit(CompositePurchaseOrder compPO) {
    if (CollectionUtils.isNotEmpty(compPO.getCompositePoLines())) {
       return getTenantConfiguration()
        .thenAccept(config -> {
          int limit = getPoLineLimit(config);
          if (compPO.getCompositePoLines().size() > limit) {
            addProcessingError(ErrorCodes.POL_LINES_LIMIT_EXCEEDED.toError());
          }
        });
    }
    return completedFuture(null);
  }

  private CompletableFuture<CompositePurchaseOrder> createPOandPOLines(CompositePurchaseOrder compPO) {
    final WorkflowStatus finalStatus = compPO.getWorkflowStatus();

    // we should always create PO and PO lines in PENDING status and transition to OPEN only when it's all set
    // (e.g. PO lines are created, Inventory is updated, etc.)
    if (finalStatus == OPEN) {
      compPO.setWorkflowStatus(PENDING);
    }

    return createRecordInStorage(convertToPurchaseOrder(compPO), resourcesPath(PURCHASE_ORDER))
      .thenApply(compPO::withId)
      .thenCompose(this::createPoLines)
      .thenAccept(compPO::setCompositePoLines)
      .thenCompose(v -> {
        if (finalStatus == OPEN) {
          return checkOrderApprovalRequired(compPO).
              thenCompose(ok -> openOrder(compPO));
        }
        return completedFuture(null);
      })
      .thenApply(v -> compPO);
  }

  /**
   * Checks the value of "isApprovalRequired" in configurations, if the value is set to true, and order is being approved, verifies
   * if the user has required permissions to approve order
   *
   * @param compPO
   */
  private CompletableFuture<Void> checkOrderApprovalPermissions(CompositePurchaseOrder compPO) {
    return getTenantConfiguration().thenAccept(config -> {
      boolean isApprovalRequired = isApprovalRequiredConfiguration(config);
      if (isApprovalRequired && compPO.getApproved()) {
        if (isUserNotHaveApprovePermission()) {
          throw new HttpException(HttpStatus.HTTP_FORBIDDEN.toInt(), USER_HAS_NO_APPROVAL_PERMISSIONS);
        }
        compPO.setApprovalDate(new Date());
        compPO.setApprovedById(getCurrentUserId());
      }
    });
  }

  private boolean isApprovalRequiredConfiguration(JsonObject config) {
    return Optional.ofNullable(config.getString("approvals"))
      .map(approval -> new JsonObject(approval).getBoolean("isApprovalRequired"))
      .orElse(false);
  }

  /**
   * If an order is transitioning to OPEN, checks if approval is required and throws an error if it is not approved
   *
   * @param compPO
   */
  private CompletableFuture<Void> checkOrderApprovalRequired(CompositePurchaseOrder compPO) {
    return getTenantConfiguration().thenAccept(config -> {
      boolean isApprovalRequired = isApprovalRequiredConfiguration(config);
      if (isApprovalRequired && !compPO.getApproved()) {
        throw new HttpException(400, APPROVAL_REQUIRED_TO_OPEN);
      }
      compPO.setApprovedById(getCurrentUserId());
      compPO.setApprovalDate(new Date());
    });
  }

  private CompletableFuture<List<CompositePoLine>> createPoLines(CompositePurchaseOrder compPO) {
    List<CompletableFuture<CompositePoLine>> futures =
      compPO.getCompositePoLines()
            .stream()
            .map(compositePoLine -> orderLineHelper.createPoLine(compositePoLine, compPO))
            .collect(Collectors.toList());
    return HelperUtils.collectResultsOnSuccess(futures);
  }

  private CompletableFuture<CompositePurchaseOrder> fetchCompositePoLines(CompositePurchaseOrder compPO) {
    if (CollectionUtils.isEmpty(compPO.getCompositePoLines())) {
      return  getCompositePoLines(compPO.getId(), lang, httpClient, ctx, okapiHeaders, logger)
        .thenApply(poLines -> {
          orderLineHelper.sortPoLinesByPoLineNumber(poLines);
          return poLines;
        })
        .thenApply(compPO::withCompositePoLines);
    } else {
      return completedFuture(compPO);
    }
  }

  private void populateInstanceId(Map<String, List<Title>> lineIdsTitles, List<CompositePoLine> lines) {
    getNonPackageLines(lines).forEach(line -> {
      if (lineIdsTitles.containsKey(line.getId())) {
        line.setInstanceId(lineIdsTitles.get(line.getId()).get(0).getInstanceId());
      }
    });
  }

  private void changePoLineStatuses(CompositePurchaseOrder compPO) {
    compPO.getCompositePoLines().forEach(poLine -> {
      changeReceiptStatus(poLine);
      changePaymentStatus(poLine);
    });
  }

  private void changePaymentStatus(CompositePoLine poLine) {
    if (poLine.getPaymentStatus() == CompositePoLine.PaymentStatus.PENDING) {
      poLine.setPaymentStatus(CompositePoLine.PaymentStatus.AWAITING_PAYMENT);
    }
  }

  private void changeReceiptStatus(CompositePoLine poLine) {
    if (poLine.getReceiptStatus() == CompositePoLine.ReceiptStatus.PENDING) {
      poLine.setReceiptStatus(CompositePoLine.ReceiptStatus.AWAITING_RECEIPT);
    }
  }

  private CompletionStage<Void> validatePoNumber(CompositePurchaseOrder poFromStorage, CompositePurchaseOrder updatedPo) {
    if (isPoNumberChanged(poFromStorage, updatedPo)) {
      return poNumberHelper.checkPONumberUnique(updatedPo.getPoNumber());
    }
    return completedFuture(null);
  }

  private boolean isTransitionToOpen(CompositePurchaseOrder poFromStorage, CompositePurchaseOrder compPO) {
    return poFromStorage.getWorkflowStatus() == PENDING && compPO.getWorkflowStatus() == OPEN;
  }

  private boolean isPoNumberChanged(CompositePurchaseOrder poFromStorage, CompositePurchaseOrder updatedPo) {
    return !StringUtils.equalsIgnoreCase(poFromStorage.getPoNumber(), updatedPo.getPoNumber());
  }

  private CompletableFuture<Void> updatePoLines(CompositePurchaseOrder poFromStorage, CompositePurchaseOrder compPO) {
    if (isPoLinesUpdateRequired(poFromStorage, compPO)) {
      return fetchOrderLinesByOrderId(compPO.getId())
        .thenCompose(existingPoLines -> {
          if (isNotEmpty(compPO.getCompositePoLines())) {
            // New PO Line(s) can be added only to Pending order
            if (poFromStorage.getWorkflowStatus() != PENDING && hasNewPoLines(compPO, existingPoLines)) {
              throw new HttpException(422, poFromStorage.getWorkflowStatus() == OPEN ? ErrorCodes.ORDER_OPEN : ErrorCodes.ORDER_CLOSED);
            }
            validatePOLineProtectedFieldsChangedInPO(poFromStorage, compPO, existingPoLines);
            //check if the order is in open status and the fields are being changed
            return handlePoLines(compPO, existingPoLines);
          } else {
            return updatePoLinesNumber(compPO, existingPoLines);
          }
        });
    }
    return completedFuture(null);
  }

  private void validatePOLineProtectedFieldsChangedInPO(CompositePurchaseOrder poFromStorage, CompositePurchaseOrder compPO,
                                                        List<PoLine> existingPoLines) {
    if (poFromStorage.getWorkflowStatus() != PENDING) {
      compPO.getCompositePoLines()
        .forEach(poLine -> verifyProtectedFieldsChanged(POLineProtectedFields.getFieldNames(),
            findCorrespondingCompositePoLine(poLine, existingPoLines), JsonObject.mapFrom(poLine)));
    }
  }

  private boolean isPoLinesUpdateRequired(CompositePurchaseOrder poFromStorage, CompositePurchaseOrder compPO) {
    return isNotEmpty(compPO.getCompositePoLines()) || isPoNumberChanged(poFromStorage, compPO);
  }

  private CompletableFuture<Void> updatePoLinesSummary(CompositePurchaseOrder compPO) {
    return VertxCompletableFuture.allOf(ctx, compPO.getCompositePoLines().stream()
      .map(HelperUtils::convertToPoLine)
      .map(line -> orderLineHelper.updateOrderLineSummary(line.getId(), JsonObject.mapFrom(line))).toArray(CompletableFuture[]::new));
  }

  private CompletableFuture<Void> updateOrderSummary(CompositePurchaseOrder compPO) {
    logger.debug("Updating order...");
    PurchaseOrder purchaseOrder = convertToPurchaseOrder(compPO).mapTo(PurchaseOrder.class);
    List<PoLine> poLines = HelperUtils.convertToPoLines(compPO.getCompositePoLines());

    if (changeOrderStatus(purchaseOrder, poLines)) {
      logger.debug("Workflow status update required for order with id={}", compPO.getId());
      compPO.setWorkflowStatus(CompositePurchaseOrder.WorkflowStatus.fromValue(purchaseOrder.getWorkflowStatus().value()));
      compPO.setCloseReason(purchaseOrder.getCloseReason());
    }

    return handlePutRequest(resourceByIdPath(PURCHASE_ORDER, compPO.getId()), JsonObject.mapFrom(purchaseOrder), httpClient, ctx, okapiHeaders, logger);
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

    JsonObject purchaseOrder = JsonObject.mapFrom(compPO);
    purchaseOrder.remove(COMPOSITE_PO_LINES);
    return purchaseOrder;
  }

  private CompletableFuture<Void> updatePoLinesNumber(CompositePurchaseOrder compOrder, List<PoLine> poLinesFromStorage) {
    CompletableFuture[] futures = poLinesFromStorage
      .stream()
      .map(lineFromStorage -> {
        lineFromStorage.setPoLineNumber(orderLineHelper.buildNewPoLineNumber(lineFromStorage, compOrder.getPoNumber()));
        return orderLineHelper
          .updateOrderLineSummary(lineFromStorage.getId(), JsonObject.mapFrom(lineFromStorage));
      })
       .toArray(CompletableFuture[]::new);

    return VertxCompletableFuture.allOf(ctx, futures);
  }

  private CompletableFuture<Void> createEncumbrances(CompositePurchaseOrder compPO) {
    if (isFundDistributionsPresent(compPO)) {
      FinanceHelper helper = new FinanceHelper(httpClient, okapiHeaders, ctx, lang);
      return helper.handleEncumbrances(compPO);
    }
    return CompletableFuture.completedFuture(null);
  }

  private boolean isFundDistributionsPresent(CompositePurchaseOrder compPO) {
    return compPO.getCompositePoLines().stream().mapToLong(compositePoLine -> compositePoLine.getFundDistribution().size()).sum() >= 1;
  }

  private CompletableFuture<Void> updateInventory(CompositePurchaseOrder compPO) {
    return CompletableFuture.allOf(
      compPO.getCompositePoLines()
            .stream()
            .map(orderLineHelper::updateInventory)
            .toArray(CompletableFuture[]::new)
    );
  }

  private CompletableFuture<Void> handlePoLines(CompositePurchaseOrder compOrder, List<PoLine> poLinesFromStorage) {
    List<CompletableFuture<?>> futures = new ArrayList<>(processPoLinesCreation(compOrder, poLinesFromStorage));
    if (!poLinesFromStorage.isEmpty()) {
      futures.addAll(processPoLinesUpdate(compOrder, poLinesFromStorage));
      // The remaining unprocessed PoLines should be removed
      poLinesFromStorage.forEach(poLine -> futures.add(deletePoLine(JsonObject.mapFrom(poLine), httpClient, ctx, okapiHeaders, logger)));
    }
    return VertxCompletableFuture.allOf(ctx, futures.toArray(new CompletableFuture[0]));
  }

  private List<CompletableFuture<?>> processPoLinesUpdate(CompositePurchaseOrder compOrder, List<PoLine> poLinesFromStorage) {
    List<CompletableFuture<?>> futures = new ArrayList<>();
    Iterator<PoLine> iterator = poLinesFromStorage.iterator();
    while (iterator.hasNext()) {
      PoLine lineFromStorage = iterator.next();
      for (CompositePoLine line : compOrder.getCompositePoLines()) {
        if (StringUtils.equals(lineFromStorage.getId(), line.getId())) {
          line.setPoLineNumber(orderLineHelper.buildNewPoLineNumber(lineFromStorage, compOrder.getPoNumber()));
          futures.add(orderLineHelper.updateOrderLine(line, JsonObject.mapFrom(lineFromStorage)));
          iterator.remove();
          break;
        }
      }
    }
    return futures;
  }

  private List<CompletableFuture<CompositePoLine>> processPoLinesCreation(CompositePurchaseOrder compOrder, List<PoLine> poLinesFromStorage) {
    return getNewPoLines(compOrder, poLinesFromStorage)
      .map(compPOL -> orderLineHelper.createPoLine(compPOL, compOrder))
      .collect(toList());
  }

  private boolean hasNewPoLines(CompositePurchaseOrder compPO, List<PoLine> poLinesFromStorage) {
    return getNewPoLines(compPO, poLinesFromStorage).count() > 0;
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

  /**
   * The method checks if the order is assigned to acquisition unit, if yes,
   * then check that if the user has desired permission to assign the record to acquisition unit
   *
   * @throws HttpException if user does not have assign permission
   * @param acqUnitIds acquisitions units assigned to purchase order from request
   */
  private void verifyUserHasAssignPermission(List<String> acqUnitIds) {
    if (CollectionUtils.isNotEmpty(acqUnitIds) && isUserDoesNotHaveDesiredPermission(ASSIGN)){
      throw new HttpException(HttpStatus.HTTP_FORBIDDEN.toInt(), USER_HAS_NO_ACQ_PERMISSIONS);
    }
  }

  /**
   * @param updatedOrder purchase order from request
   * @param persistedOrder purchase order from storage
   * @return completable future completed successfully if all checks pass or exceptionally in case of error/restriction
   *         caused by acquisitions units
   */
  private CompletableFuture<Void> validateAcqUnitsOnUpdate(CompositePurchaseOrder updatedOrder,
      CompositePurchaseOrder persistedOrder) {
    List<String> updatedAcqUnitIds = updatedOrder.getAcqUnitIds();
    List<String> currentAcqUnitIds = persistedOrder.getAcqUnitIds();

    return VertxCompletableFuture.runAsync(ctx, () -> verifyUserHasManagePermission(updatedAcqUnitIds, currentAcqUnitIds))
      // Check that all newly assigned units are active/exist
      .thenCompose(ok -> protectionHelper.verifyIfUnitsAreActive(ListUtils.subtract(updatedAcqUnitIds, currentAcqUnitIds)))
      .thenCompose(ok -> getInvolvedOperations(updatedOrder))
      // The check should be done against currently assigned (persisted in storage) units
      .thenCompose(protectedOperationTypes -> protectionHelper.isOperationRestricted(currentAcqUnitIds, protectedOperationTypes));
  }

  /**
   * The method checks if list of acquisition units to which the order is assigned is changed, if yes,
   * then check that if the user has desired permission to manage acquisition units assignments
   *
   * @throws HttpException if user does not have manage permission
   * @param newAcqUnitIds acquisitions units assigned to purchase order from request
   * @param currentAcqUnitIds acquisitions units assigned to purchase order from storage
   */
  private void verifyUserHasManagePermission(List<String> newAcqUnitIds, List<String> currentAcqUnitIds) {
    Set<String> newAcqUnits = new HashSet<>(CollectionUtils.emptyIfNull(newAcqUnitIds));
    Set<String> acqUnitsFromStorage = new HashSet<>(CollectionUtils.emptyIfNull(currentAcqUnitIds));

    if (isManagePermissionRequired(newAcqUnits, acqUnitsFromStorage) && isUserDoesNotHaveDesiredPermission(MANAGE)){
      throw new HttpException(HttpStatus.HTTP_FORBIDDEN.toInt(), USER_HAS_NO_ACQ_PERMISSIONS);
    }
  }

  private boolean isUserDoesNotHaveDesiredPermission(AcqDesiredPermissions acqPerm) {
    return !getProvidedPermissions().contains(acqPerm.getPermission());
  }

  private boolean isManagePermissionRequired(Set<String> newAcqUnits, Set<String> acqUnitsFromStorage) {
    return !CollectionUtils.isEqualCollection(newAcqUnits, acqUnitsFromStorage);
  }

  private List<String> getProvidedPermissions() {
    return new JsonArray(okapiHeaders.getOrDefault(OKAPI_HEADER_PERMISSIONS, EMPTY_ARRAY)).stream().
      map(Object::toString)
      .collect(Collectors.toList());
  }

  private CompletableFuture<List<PoLine>> fetchOrderLinesByOrderId(String orderId) {
    if (orderLines != null) {
      return CompletableFuture.completedFuture(new ArrayList<>(orderLines));
    }

    return getPoLines(orderId, lang, httpClient, ctx, okapiHeaders, logger)
      .thenApply(linesJsonArray -> {
        orderLines = HelperUtils.convertJsonToPoLines(linesJsonArray);
        return new ArrayList<>(orderLines);
      });
  }


  private boolean isUserNotHaveApprovePermission() {
    return !getProvidedPermissions().contains(PERMISSION_ORDER_APPROVE);
  }
}
