package org.folio.service;

import static org.apache.commons.collections4.CollectionUtils.isNotEmpty;
import static org.folio.orders.utils.AcqDesiredPermissions.BYPASS_ACQ_UNITS;
import static org.folio.orders.utils.QueryUtils.combineCqlExpressions;
import static org.folio.orders.utils.QueryUtils.convertIdsToCqlQuery;
import static org.folio.orders.utils.PermissionsUtil.isManagePermissionRequired;
import static org.folio.orders.utils.PermissionsUtil.userDoesNotHaveDesiredPermission;
import static org.folio.orders.utils.PermissionsUtil.userHasDesiredPermission;
import static org.folio.rest.core.exceptions.ErrorCodes.ORDER_UNITS_NOT_FOUND;
import static org.folio.rest.core.exceptions.ErrorCodes.USER_HAS_NO_ACQ_PERMISSIONS;
import static org.folio.rest.core.exceptions.ErrorCodes.USER_NOT_A_MEMBER_OF_THE_ACQ;
import static org.folio.service.AcquisitionsUnitsService.ACQUISITIONS_UNIT_IDS;
import static org.folio.service.AcquisitionsUnitsService.ENDPOINT_ACQ_UNITS;
import static org.folio.service.UserService.getCurrentUserId;

import java.util.Collections;
import java.util.HashSet;
import java.util.List;
import java.util.Set;
import java.util.stream.Collectors;

import org.apache.commons.collections4.CollectionUtils;
import org.apache.commons.collections4.ListUtils;
import org.apache.commons.lang3.StringUtils;
import org.apache.logging.log4j.LogManager;
import org.apache.logging.log4j.Logger;
import org.folio.HttpStatus;
import org.folio.orders.utils.AcqDesiredPermissions;
import org.folio.orders.utils.ProtectedOperationType;
import org.folio.orders.utils.QueryUtils;
import org.folio.rest.core.exceptions.HttpException;
import org.folio.rest.core.models.RequestContext;
import org.folio.rest.core.models.RequestEntry;
import org.folio.rest.jaxrs.model.AcquisitionsUnit;
import org.folio.rest.jaxrs.model.AcquisitionsUnitCollection;
import org.folio.rest.jaxrs.model.CompositePurchaseOrder;
import org.folio.rest.jaxrs.model.Error;

import io.vertx.core.Future;

public class ProtectionService {
  private static final Logger log = LogManager.getLogger(ProtectionService.class);

  public static final String ACQUISITIONS_UNIT_ID = "acquisitionsUnitId";
  private final AcquisitionsUnitsService acquisitionsUnitsService;

  public ProtectionService(AcquisitionsUnitsService acquisitionsUnitsService) {
    this.acquisitionsUnitsService = acquisitionsUnitsService;
  }

  /**
   * This method determines status of operation restriction based on unit IDs from {@link CompositePurchaseOrder}.
   * @param unitIds list of unit IDs.
   *
   * @throws HttpException if user hasn't permissions or units not found
   */
  public Future<Void> isOperationRestricted(List<String> unitIds, ProtectedOperationType operation, RequestContext requestContext) {
    return isOperationRestricted(unitIds, Collections.singleton(operation), requestContext);
  }

  /**
   * This method determines status of operation restriction based on unit IDs from {@link CompositePurchaseOrder}.
   *
   * @param unitIds list of unit IDs.
   *
   * @return completable future completed exceptionally if user does not have rights to perform operation or any unit does not
   *         exist; successfully otherwise
   */
  public Future<Void> isOperationRestricted(List<String> unitIds, Set<ProtectedOperationType> operations, RequestContext requestContext) {
    if (userHasDesiredPermission(BYPASS_ACQ_UNITS, requestContext)) {
      log.info("isOperationRestricted:: Bypassing checking acq units");
      return Future.succeededFuture();
    }
    if (CollectionUtils.isNotEmpty(unitIds)) {
      return getUnitsByIds(unitIds, requestContext)
        .compose(units -> {
          if (unitIds.size() == units.size()) {
            // In case any unit is "soft deleted", just skip it (refer to MODORDERS-294)
            List<AcquisitionsUnit> activeUnits = units.stream()
              .filter(unit -> !unit.getIsDeleted())
              .collect(Collectors.toList());

            if (!activeUnits.isEmpty() && applyMergingStrategy(activeUnits, operations)) {
              log.info("isOperationRestricted:: Acq unit ids are merged");
              return verifyUserIsMemberOfOrdersUnits(extractUnitIds(activeUnits), requestContext);
            }
            return Future.succeededFuture();
          } else {
            // In case any unit "hard deleted" or never existed by specified uuid
            throw new HttpException(HttpStatus.HTTP_UNPROCESSABLE_ENTITY.toInt(), buildUnitsNotFoundError(unitIds, extractUnitIds(units)));
          }
        });
    } else {
      log.info("isOperationRestricted:: Acq unit ids are empty");
      return Future.succeededFuture();
    }
  }

  /**
   * This method validates acq units during entity CREATE operation.
   *
   * @param acqUnitIds acquisitions units assigned to purchase order from request
   * @param permission the acq desired permission to check
   * @return completable future completed successfully if all checks pass or exceptionally in case of error/restriction
   * caused by acquisitions units
   */
  public Future<Void> validateAcqUnitsOnCreate(List<String> acqUnitIds,
                                               AcqDesiredPermissions permission,
                                               RequestContext requestContext) {
    if (CollectionUtils.isEmpty(acqUnitIds)) {
      return Future.succeededFuture();
    }
    return Future.succeededFuture()
      .map(v -> verifyUserHasAssignPermission(acqUnitIds, permission, requestContext))
      .compose(ok -> verifyIfUnitsAreActive(acqUnitIds, requestContext))
      .compose(ok -> isOperationRestricted(acqUnitIds, ProtectedOperationType.CREATE, requestContext));
  }

  /**
   * The method checks if the order is assigned to acquisition unit, if yes,
   * then check that if the user has desired permission to assign the record to acquisition unit
   *
   * @param acqUnitIds acquisitions units assigned to purchase order from request
   * @param permission the desired permission to check
   * @throws HttpException if user does not have assign permission
   */
  private Void verifyUserHasAssignPermission(List<String> acqUnitIds,
                                             AcqDesiredPermissions permission,
                                             RequestContext requestContext) {
    if (isNotEmpty(acqUnitIds) && userDoesNotHaveDesiredPermission(permission, requestContext)){
      throw new HttpException(HttpStatus.HTTP_FORBIDDEN.toInt(), USER_HAS_NO_ACQ_PERMISSIONS);
    }
    return null;
  }

  /**
   * This method validates acq units during entity UPDATE operation.
   *
   * @param newAcqUnitIds         purchase order from request
   * @param acqUnitIdsFromStorage purchase order from storage
   * @param permission            the acq desired permission to check
   * @param protectedOperations   list of protected operations like CREATE, UPDATE, DELETE to check
   * @return completable future completed successfully if all checks pass or exceptionally in case of error/restriction
   * caused by acquisitions units
   */
  public Future<Void> validateAcqUnitsOnUpdate(List<String> newAcqUnitIds,
                                               List<String> acqUnitIdsFromStorage,
                                               AcqDesiredPermissions permission,
                                               Set<ProtectedOperationType> protectedOperations,
                                               RequestContext requestContext) {
    return Future.succeededFuture()
      .map(v -> {
        verifyUserHasManagePermission(newAcqUnitIds, acqUnitIdsFromStorage, permission, requestContext);
        return null;
      })
      .compose(ok -> verifyIfUnitsAreActive(ListUtils.subtract(newAcqUnitIds, acqUnitIdsFromStorage), requestContext))
      // The check should be done against currently assigned (persisted in storage) units
      .compose(ok -> isOperationRestricted(acqUnitIdsFromStorage, protectedOperations, requestContext));
  }

  /**
   * This method wraps existing cql query with acq units check.
   *
   * @param tablePrefix    table prefix in format tableName + '.' or just empty string if acq units will be checked from the same table
   * @param query          original cql string
   * @param requestContext request context
   * @return new string with acq check
   */
  public Future<String> getQueryWithAcqUnitsCheck(String tablePrefix, String query, RequestContext requestContext) {
    if (userHasDesiredPermission(BYPASS_ACQ_UNITS, requestContext)) {
      return Future.succeededFuture(query);
    }
    return acquisitionsUnitsService.buildAcqUnitsCqlExprToSearchRecords(tablePrefix, requestContext)
      .map(acqUnitsCqlExpr -> {
        if (StringUtils.isNotEmpty(query)) {
          return combineCqlExpressions("and", acqUnitsCqlExpr, query);
        }
        return acqUnitsCqlExpr;
      });
  }

  /**
   * The method checks if list of acquisition units to which the order is assigned is changed, if yes,
   * then check that if the user has desired permission to manage acquisition units assignments
   *
   * @param newAcqUnitIds         acquisitions units assigned to purchase order from request
   * @param acqUnitIdsFromStorage acquisitions units assigned to purchase order from storage
   * @param permission the acq desired permission to check
   * @throws HttpException if user does not have manage permission
   */
  private void verifyUserHasManagePermission(List<String> newAcqUnitIds,
                                             List<String> acqUnitIdsFromStorage,
                                             AcqDesiredPermissions permission,
                                             RequestContext requestContext) {
    Set<String> newAcqUnits = new HashSet<>(CollectionUtils.emptyIfNull(newAcqUnitIds));
    Set<String> acqUnitsFromStorage = new HashSet<>(CollectionUtils.emptyIfNull(acqUnitIdsFromStorage));

    if (isManagePermissionRequired(newAcqUnits, acqUnitsFromStorage) && userDoesNotHaveDesiredPermission(permission, requestContext)){
      throw new HttpException(HttpStatus.HTTP_FORBIDDEN.toInt(), USER_HAS_NO_ACQ_PERMISSIONS);
    }
  }

  /**
   * Verifies if all acquisition units exist and active based on passed ids
   *
   * @param acqUnitIds list of unit IDs.
   * @return completable future completed successfully if all units exist and active or exceptionally otherwise
   */
  private Future<Void> verifyIfUnitsAreActive(List<String> acqUnitIds, RequestContext requestContext) {
    if (acqUnitIds.isEmpty()) {
      return Future.succeededFuture();
    }

    return getUnitsByIds(acqUnitIds, requestContext).map(units -> {
      List<String> activeUnitIds = units.stream()
        .filter(unit -> !unit.getIsDeleted())
        .map(AcquisitionsUnit::getId)
        .collect(Collectors.toList());

      if (acqUnitIds.size() != activeUnitIds.size()) {
        throw new HttpException(HttpStatus.HTTP_UNPROCESSABLE_ENTITY.toInt(), buildUnitsNotFoundError(acqUnitIds, activeUnitIds));
      }
      return null;
    });
  }

  private Error buildUnitsNotFoundError(List<String> expectedUnitIds, List<String> availableUnitIds) {
    List<String> missingUnitIds = ListUtils.subtract(expectedUnitIds, availableUnitIds);
    return ORDER_UNITS_NOT_FOUND.toError().withAdditionalProperty(ACQUISITIONS_UNIT_IDS, missingUnitIds);
  }

  private List<String> extractUnitIds(List<AcquisitionsUnit> activeUnits) {
    return activeUnits.stream().map(AcquisitionsUnit::getId).collect(Collectors.toList());
  }

  /**
   * Check whether the user is a member of at least one group from which the related order belongs.
   *
   * @return list of unit ids associated with user.
   */
  private Future<Void> verifyUserIsMemberOfOrdersUnits(List<String> unitIdsAssignedToOrder, RequestContext requestContext) {
    String query = String.format("userId==%s AND %s", getCurrentUserId(requestContext.getHeaders()), QueryUtils.convertFieldListToCqlQuery(unitIdsAssignedToOrder, ACQUISITIONS_UNIT_ID, true));
    return acquisitionsUnitsService.getAcquisitionsUnitsMemberships(query, 0, 0, requestContext)
      .map(unit -> {
        if (unit.getTotalRecords() == 0) {
          throw new HttpException(HttpStatus.HTTP_FORBIDDEN.toInt(), USER_NOT_A_MEMBER_OF_THE_ACQ);
        }
        return null;
      })
      .mapEmpty();
  }

  /**
   * This method returns list of {@link AcquisitionsUnit} based on list of unit ids
   * @param unitIds list of unit ids
   *
   * @return list of {@link AcquisitionsUnit}
   */
  private Future<List<AcquisitionsUnit>> getUnitsByIds(List<String> unitIds, RequestContext requestContext) {
    return Future.succeededFuture()
      .map(v-> {
        String query = combineCqlExpressions("and", convertIdsToCqlQuery(unitIds));
        return new RequestEntry(ENDPOINT_ACQ_UNITS)
          .withQuery(query)
          .withLimit(Integer.MAX_VALUE)
          .withOffset(0);
      })
      .compose(requestEntry -> acquisitionsUnitsService.getAcquisitionsUnits(requestEntry, requestContext))
      .map(AcquisitionsUnitCollection::getAcquisitionsUnits);
  }

  /**
   * This method returns operation protection resulted status based on list of units with using least restrictive wins strategy.
   *
   * @param units list of {@link AcquisitionsUnit}.
   * @return true if operation is protected, otherwise - false.
   */
  private boolean applyMergingStrategy(List<AcquisitionsUnit> units, Set<ProtectedOperationType> operations) {
    return units.stream().allMatch(unit -> operations.stream().anyMatch(operation -> operation.isProtected(unit)));
  }

}
