package org.folio.service;

import static org.folio.rest.core.exceptions.ErrorCodes.ORDER_UNITS_NOT_FOUND;
import static org.folio.rest.core.exceptions.ErrorCodes.USER_HAS_NO_PERMISSIONS;
import static org.folio.orders.utils.HelperUtils.combineCqlExpressions;
import static org.folio.orders.utils.HelperUtils.convertIdsToCqlQuery;
import static org.folio.rest.RestVerticle.OKAPI_USERID_HEADER;
import static org.folio.service.AcquisitionsUnitsService.ACQUISITIONS_UNIT_IDS;

import java.util.Collections;
import java.util.List;
import java.util.Set;
import java.util.concurrent.CompletableFuture;
import java.util.stream.Collectors;

import org.apache.commons.collections4.CollectionUtils;
import org.apache.commons.collections4.ListUtils;
import org.folio.HttpStatus;
import org.folio.rest.core.exceptions.HttpException;
import org.folio.orders.utils.HelperUtils;
import org.folio.orders.utils.ProtectedOperationType;
import org.folio.rest.core.models.RequestContext;
import org.folio.rest.jaxrs.model.AcquisitionsUnit;
import org.folio.rest.jaxrs.model.AcquisitionsUnitCollection;
import org.folio.rest.jaxrs.model.CompositePurchaseOrder;
import org.folio.rest.jaxrs.model.Error;

public class ProtectionService {

  public static final String ACQUISITIONS_UNIT_ID = "acquisitionsUnitId";
  private static final String IS_DELETED_PROP = "isDeleted";
  private static final String ALL_UNITS_CQL = IS_DELETED_PROP + "=*";

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
  public CompletableFuture<Void> isOperationRestricted(List<String> unitIds, ProtectedOperationType operation, RequestContext requestContext) {
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
  public CompletableFuture<Void> isOperationRestricted(List<String> unitIds, Set<ProtectedOperationType> operations, RequestContext requestContext) {
    if (CollectionUtils.isNotEmpty(unitIds)) {
      return getUnitsByIds(unitIds, requestContext)
        .thenCompose(units -> {
          if (unitIds.size() == units.size()) {
            // In case any unit is "soft deleted", just skip it (refer to MODORDERS-294)
            List<AcquisitionsUnit> activeUnits = units.stream()
              .filter(unit -> !unit.getIsDeleted())
              .collect(Collectors.toList());

            if (!activeUnits.isEmpty() && applyMergingStrategy(activeUnits, operations)) {
              return verifyUserIsMemberOfOrdersUnits(extractUnitIds(activeUnits), requestContext);
            }
            return CompletableFuture.completedFuture(null);
          } else {
            // In case any unit "hard deleted" or never existed by specified uuid
            throw new HttpException(HttpStatus.HTTP_UNPROCESSABLE_ENTITY.toInt(), buildUnitsNotFoundError(unitIds, extractUnitIds(units)));
          }
        });
    } else {
      return CompletableFuture.completedFuture(null);
    }
  }

  /**
   * Verifies if all acquisition units exist and active based on passed ids
   *
   * @param acqUnitIds list of unit IDs.
   * @return completable future completed successfully if all units exist and active or exceptionally otherwise
   */
  public CompletableFuture<Void> verifyIfUnitsAreActive(List<String> acqUnitIds, RequestContext requestContext) {
    if (acqUnitIds.isEmpty()) {
      return CompletableFuture.completedFuture(null);
    }

    return getUnitsByIds(acqUnitIds, requestContext).thenAccept(units -> {
      List<String> activeUnitIds = units.stream()
        .filter(unit -> !unit.getIsDeleted())
        .map(AcquisitionsUnit::getId)
        .collect(Collectors.toList());

      if (acqUnitIds.size() != activeUnitIds.size()) {
        throw new HttpException(HttpStatus.HTTP_UNPROCESSABLE_ENTITY.toInt(), buildUnitsNotFoundError(acqUnitIds, activeUnitIds));
      }
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
  private CompletableFuture<Void> verifyUserIsMemberOfOrdersUnits(List<String> unitIdsAssignedToOrder, RequestContext requestContext) {
    String query = String.format("userId==%s AND %s", getCurrentUserId(requestContext), HelperUtils.convertFieldListToCqlQuery(unitIdsAssignedToOrder, ACQUISITIONS_UNIT_ID, true));
    return acquisitionsUnitsService.getAcquisitionsUnitsMemberships(query, 0, 0, requestContext)
      .thenAccept(unit -> {
        if (unit.getTotalRecords() == 0) {
          throw new HttpException(HttpStatus.HTTP_FORBIDDEN.toInt(), USER_HAS_NO_PERMISSIONS);
        }
      });
  }

  /**
   * This method returns list of {@link AcquisitionsUnit} based on list of unit ids
   * @param unitIds list of unit ids
   *
   * @return list of {@link AcquisitionsUnit}
   */
  private CompletableFuture<List<AcquisitionsUnit>> getUnitsByIds(List<String> unitIds, RequestContext requestContext) {
    String query = combineCqlExpressions("and", ALL_UNITS_CQL, convertIdsToCqlQuery(unitIds));
    return acquisitionsUnitsService.getAcquisitionsUnits(query, 0, Integer.MAX_VALUE, requestContext)
      .thenApply(AcquisitionsUnitCollection::getAcquisitionsUnits);
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

  private String getCurrentUserId(RequestContext requestContext) {
    return requestContext.getHeaders().get(OKAPI_USERID_HEADER);
  }
}
