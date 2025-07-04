package org.folio.service.orders;

import io.vertx.core.Future;
import io.vertx.core.json.JsonObject;
import org.apache.commons.collections4.CollectionUtils;
import org.apache.commons.lang3.StringUtils;
import org.apache.logging.log4j.LogManager;
import org.apache.logging.log4j.Logger;
import org.folio.HttpStatus;
import org.folio.helper.PoNumberHelper;
import org.folio.helper.PurchaseOrderLineHelper;
import org.folio.okapi.common.GenericCompositeFuture;
import org.folio.orders.utils.AcqDesiredPermissions;
import org.folio.orders.utils.PoLineCommonUtil;
import org.folio.orders.utils.ProtectedOperationType;
import org.folio.rest.core.exceptions.ErrorCodes;
import org.folio.rest.core.exceptions.HttpException;
import org.folio.rest.core.models.RequestContext;
import org.folio.rest.jaxrs.model.PoLine;
import org.folio.rest.jaxrs.model.CompositePurchaseOrder;
import org.folio.rest.jaxrs.model.Error;
import org.folio.rest.jaxrs.model.Parameter;
import org.folio.service.PrefixService;
import org.folio.service.ProtectionService;
import org.folio.service.SuffixService;
import org.folio.service.caches.CommonSettingsCache;
import org.folio.service.orders.flows.update.unopen.UnOpenCompositeOrderManager;
import org.folio.service.organization.OrganizationService;

import java.util.ArrayList;
import java.util.Collection;
import java.util.Collections;
import java.util.Date;
import java.util.HashSet;
import java.util.List;
import java.util.Objects;
import java.util.Optional;
import java.util.Set;
import java.util.stream.Collectors;

import static org.apache.commons.collections4.CollectionUtils.isNotEmpty;
import static org.folio.orders.utils.AcqDesiredPermissions.MANAGE;
import static org.folio.orders.utils.HelperUtils.ORDER_CONFIG_MODULE_NAME;
import static org.folio.orders.utils.HelperUtils.collectResultsOnSuccess;
import static org.folio.orders.utils.HelperUtils.getPoLineLimit;
import static org.folio.orders.utils.OrderStatusTransitionUtil.isTransitionToApproved;
import static org.folio.orders.utils.OrderStatusTransitionUtil.isTransitionToPending;
import static org.folio.orders.utils.PermissionsUtil.userDoesNotHaveApprovePermission;
import static org.folio.orders.utils.PermissionsUtil.userDoesNotHaveUnopenPermission;
import static org.folio.orders.utils.ProtectedOperationType.CREATE;
import static org.folio.orders.utils.ProtectedOperationType.DELETE;
import static org.folio.orders.utils.ProtectedOperationType.UPDATE;
import static org.folio.orders.utils.ResourcePathResolver.PO_LINE_NUMBER;
import static org.folio.rest.core.exceptions.ErrorCodes.APPROVAL_REQUIRED_TO_OPEN;
import static org.folio.rest.core.exceptions.ErrorCodes.MISSING_ONGOING;
import static org.folio.rest.core.exceptions.ErrorCodes.ONGOING_NOT_ALLOWED;
import static org.folio.rest.core.exceptions.ErrorCodes.USER_HAS_NO_APPROVAL_PERMISSIONS;
import static org.folio.rest.core.exceptions.ErrorCodes.USER_HAS_NO_UNOPEN_PERMISSIONS;
import static org.folio.service.UserService.getCurrentUserId;

public class OrderValidationService {
  private static final Logger logger = LogManager.getLogger();

  private final PoLineValidationService poLineValidationService;
  private final CommonSettingsCache commonSettingsCache;
  private final OrganizationService organizationService;
  private final PoNumberHelper poNumberHelper;
  private final PrefixService prefixService;
  private final ProtectionService protectionService;
  private final PurchaseOrderLineHelper purchaseOrderLineHelper;
  private final PurchaseOrderLineService purchaseOrderLineService;
  private final SuffixService suffixService;
  private final UnOpenCompositeOrderManager unOpenCompositeOrderManager;

  public OrderValidationService(
    PoLineValidationService poLineValidationService,
    CommonSettingsCache commonSettingsCache, OrganizationService organizationService,
    ProtectionService protectionService, PrefixService prefixService, PurchaseOrderLineHelper purchaseOrderLineHelper,
    PurchaseOrderLineService purchaseOrderLineService, SuffixService suffixService, PoNumberHelper poNumberHelper,
    UnOpenCompositeOrderManager unOpenCompositeOrderManager) {
    this.poLineValidationService = poLineValidationService;
    this.commonSettingsCache = commonSettingsCache;
    this.organizationService = organizationService;
    this.poNumberHelper = poNumberHelper;
    this.prefixService = prefixService;
    this.purchaseOrderLineHelper = purchaseOrderLineHelper;
    this.purchaseOrderLineService = purchaseOrderLineService;
    this.protectionService = protectionService;
    this.suffixService = suffixService;
    this.unOpenCompositeOrderManager = unOpenCompositeOrderManager;
  }

  /**
   * This validation is only used when the POST endpoint is called.
   * Additional creation validation is done in validateOrderForCreation().
   * Sets the tenant default values and validates the order. Checks if Orders has
   * PO Lines within limit and validates vendors and access providers.
   *
   * @param compPO
   *          Purchase Order to validate
   * @return completable future which might be completed with {@code true} if
   *         order is valid, {@code false} if not valid or an exception if
   *         processing fails
   */
  public Future<List<Error>> validateOrderForPost(CompositePurchaseOrder compPO, JsonObject tenantConfig,
      RequestContext requestContext) {
    List<Error> errors = new ArrayList<>();
    return setCreateInventoryDefaultValues(compPO, tenantConfig)
      .compose(v -> validateOrderPoLines(compPO, requestContext))
      .map(errors::addAll)
      .map(v -> errors.addAll(validatePoLineLimit(compPO, tenantConfig)))
      .compose(v -> validateVendor(compPO, requestContext))
      .map(errors::addAll)
      .map(v -> {
        errors.addAll(validateRenewalInfo(compPO));
        return errors;
      });
  }

  /**
   * This validation is used for both the POST endpoint and data import.
   */
  public Future<Void> validateOrderForCreation(CompositePurchaseOrder compPO, RequestContext requestContext) {
    logger.info("validateOrderForCreation :: orderId: {}", compPO.getId());
    List<Future<Void>> futures = new ArrayList<>();

    futures.add(protectionService.validateAcqUnitsOnCreate(compPO.getAcqUnitIds(), AcqDesiredPermissions.ASSIGN, requestContext));
    futures.add(checkOrderApprovalPermissions(compPO, requestContext));
    futures.add(prefixService.validatePrefixAvailability(compPO.getPoNumberPrefix(), requestContext));
    futures.add(suffixService.validateSuffixAvailability(compPO.getPoNumberSuffix(), requestContext));
    futures.add(poNumberHelper.checkPONumberUnique(compPO.getPoNumber(), requestContext));

    return GenericCompositeFuture.join(futures)
      .onSuccess(v -> logger.info("validateOrderForCreation :: successful"))
      .onFailure(t -> logger.error("validateOrderForCreation :: failed", t))
      .mapEmpty();
  }

  /**
   * This validation is only used when the PUT endpoint is called.
   * Additional update validation is done in validateOrderForUpdate().
   * Validates purchase order which already exists in the storage.
   * Checks PO Number presence, validates that provided order id corresponds to one set in order and its lines.
   * @param orderId Purchase Order id
   * @param compPO Purchase Order to validate
   * @return completable future which might be completed with {@code true} if order is valid, {@code false} if not valid or an exception if processing fails
   */
  public Future<List<Error>> validateOrderForPut(String orderId, CompositePurchaseOrder compPO,
      RequestContext requestContext) {
    // The PO Number is required for existing orders
    List<Error> resultErrors = new ArrayList<>();
    if (StringUtils.isEmpty(compPO.getPoNumber())) {
      resultErrors.add(ErrorCodes.PO_NUMBER_REQUIRED.toError());
    }

    // Validate order uuid
    if (!compPO.getId().equals(orderId)) {
      resultErrors.add(ErrorCodes.MISMATCH_BETWEEN_ID_IN_PATH_AND_BODY.toError());
    } else if (isNotEmpty(compPO.getPoLines())) {
      // Validate that each PO Line has correct order id
      compPO.getPoLines().forEach(poLine -> {
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

    return commonSettingsCache.loadConfiguration(ORDER_CONFIG_MODULE_NAME, requestContext)
      .compose(tenantConfig -> validateOrderForPost(compPO, tenantConfig, requestContext))
      .map(errors -> {
        resultErrors.addAll(errors);
        return resultErrors;
      });
  }

  /**
   * This validation is used for both the PUT endpoint and data import.
   */
  public Future<Void> validateOrderForUpdate(CompositePurchaseOrder compPO, CompositePurchaseOrder poFromStorage,
      boolean deleteHoldings, RequestContext requestContext) {
    logger.info("validateOrderForUpdate :: orderId: {}", compPO.getId());
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
      });
  }

  public Future<List<Error>> validateOrderPoLines(CompositePurchaseOrder compositeOrder, RequestContext requestContext) {
    List<Future<List<Error>>> poLinesErrors = compositeOrder.getPoLines().stream()
      .map(poLine -> poLineValidationService.validatePoLine(poLine, requestContext))
      .toList();

    return collectResultsOnSuccess(poLinesErrors).map(
      lists -> lists.stream()
        .flatMap(Collection::stream)
        .toList());
  }

  /**
   * If an order is transitioning to OPEN, checks if approval is required and throws an error if it is not approved
   *
   * @param compPO composite purchase order
   */
  public Future<Void> checkOrderApprovalRequired(CompositePurchaseOrder compPO, RequestContext requestContext) {
    return commonSettingsCache.loadConfiguration(ORDER_CONFIG_MODULE_NAME, requestContext)
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

  public static boolean isApprovalRequiredConfiguration(JsonObject config) {
    return Optional.ofNullable(config.getString("approvals"))
      .map(approval -> new JsonObject(approval).getBoolean("isApprovalRequired"))
      .orElse(false);
  }


  /**
   * Checks the value of "isApprovalRequired" in configurations, if the value is set to true, and order is being approved, verifies
   * if the user has required permissions to approve order
   *
   * @param compPO composite purchase order for checking permissions
   */
  private Future<Void> checkOrderApprovalPermissions(CompositePurchaseOrder compPO, RequestContext requestContext) {
    return commonSettingsCache.loadConfiguration(ORDER_CONFIG_MODULE_NAME, requestContext)
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

  private void checkOrderUnopenPermissions(RequestContext requestContext) {
    if (userDoesNotHaveUnopenPermission(requestContext)) {
      throw new HttpException(HttpStatus.HTTP_FORBIDDEN.toInt(), USER_HAS_NO_UNOPEN_PERMISSIONS);
    }
  }

  private Set<ProtectedOperationType> getInvolvedOperations(CompositePurchaseOrder compPO, CompositePurchaseOrder poFromStorage) {
    if (CollectionUtils.isEmpty(compPO.getPoLines())) {
      return Collections.singleton(UPDATE);
    }
    List<PoLine> poLines = poFromStorage.getPoLines();
    Set<String> newIds = compPO.getPoLines().stream().map(PoLine::getId).collect(Collectors.toSet());
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

  private Future<Void> setCreateInventoryDefaultValues(CompositePurchaseOrder compPO, JsonObject tenantConfiguration) {
    logger.info("setCreateInventoryDefaultValues:: orderId: {}", compPO.getId());
    List<Future<Void>> futures = compPO.getPoLines()
      .stream()
      .map(poLine -> purchaseOrderLineHelper.setTenantDefaultCreateInventoryValues(poLine, tenantConfiguration))
      .toList();

    return GenericCompositeFuture.join(futures)
      .mapEmpty();
  }

  private Future<List<Error>> validateVendor(CompositePurchaseOrder compPO, RequestContext requestContext) {
    if (compPO.getWorkflowStatus() == CompositePurchaseOrder.WorkflowStatus.OPEN) {
      List<Error> combinedErrors = new ArrayList<>();
      return organizationService.validateVendor(compPO.getVendor(), requestContext)
        .map(aErrors -> combinedErrors.addAll(aErrors.getErrors()))
        .compose(errors -> fetchPoLines(compPO, requestContext)
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
    if (isNotEmpty(compPO.getPoLines())) {
      int limit = getPoLineLimit(tenantConfig);
      if (compPO.getPoLines().size() > limit) {
        return List.of(ErrorCodes.POL_LINES_LIMIT_EXCEEDED.toError());
      }
    }
    return Collections.emptyList();
  }

  private Future<List<PoLine>> fetchPoLines(CompositePurchaseOrder compPO, RequestContext requestContext) {
    if (CollectionUtils.isEmpty(compPO.getPoLines())) {
      return purchaseOrderLineService.getPoLinesByOrderId(compPO.getId(), requestContext)
        .map(poLines -> {
          PoLineCommonUtil.sortPoLinesByPoLineNumber(poLines);
          return poLines;
        });
    } else {
      return Future.succeededFuture(compPO.getPoLines());
    }
  }

}
