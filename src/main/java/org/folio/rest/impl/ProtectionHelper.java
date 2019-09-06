package org.folio.rest.impl;

import static org.folio.orders.utils.ErrorCodes.ORDER_UNITS_NOT_FOUND;
import static org.folio.orders.utils.ErrorCodes.USER_HAS_NO_PERMISSIONS;
import static org.folio.orders.utils.HelperUtils.combineCqlExpressions;
import static org.folio.orders.utils.HelperUtils.convertIdsToCqlQuery;
import static org.folio.rest.impl.AcquisitionsUnitsHelper.ACQUISITIONS_UNIT_IDS;
import static org.folio.rest.impl.AcquisitionsUnitsHelper.ALL_UNITS_CQL;

import java.util.ArrayList;
import java.util.Collections;
import java.util.List;
import java.util.Map;
import java.util.Set;
import java.util.concurrent.CompletableFuture;
import java.util.stream.Collectors;

import org.apache.commons.collections4.CollectionUtils;
import org.apache.commons.collections4.ListUtils;
import org.folio.HttpStatus;
import org.folio.orders.rest.exceptions.HttpException;
import org.folio.orders.utils.ProtectedOperationType;
import org.folio.rest.jaxrs.model.AcquisitionsUnit;
import org.folio.rest.jaxrs.model.CompositePurchaseOrder;
import org.folio.rest.jaxrs.model.Error;
import org.folio.rest.tools.client.interfaces.HttpClientInterface;

import io.vertx.core.Context;

public class ProtectionHelper extends AbstractHelper {

  public static final String ACQUISITIONS_UNIT_ID = "acquisitionsUnitId";
  private AcquisitionsUnitsHelper acquisitionsUnitsHelper;
  private List<AcquisitionsUnit> fetchedUnits = new ArrayList<>();

  public ProtectionHelper(HttpClientInterface httpClient, Map<String, String> okapiHeaders, Context ctx, String lang) {
    super(httpClient, okapiHeaders, ctx, lang);
    acquisitionsUnitsHelper = new AcquisitionsUnitsHelper(httpClient, okapiHeaders, ctx, lang);
  }

  /**
   * This method determines status of operation restriction based on unit IDs from {@link CompositePurchaseOrder}.
   * @param unitIds list of unit IDs.
   *
   * @throws HttpException if user hasn't permissions or units not found
   */
  public CompletableFuture<Void> isOperationRestricted(List<String> unitIds, ProtectedOperationType operation) {
    return isOperationRestricted(unitIds, Collections.singleton(operation));
  }

  /**
   * This method determines status of operation restriction based on unit IDs from {@link CompositePurchaseOrder}.
   *
   * @param unitIds list of unit IDs.
   *
   * @return completable future completed exceptionally if user does not have rights to perform operation or any unit does not
   *         exist; successfully otherwise
   */
  public CompletableFuture<Void> isOperationRestricted(List<String> unitIds, Set<ProtectedOperationType> operations) {
    if (CollectionUtils.isNotEmpty(unitIds)) {
      return getUnitsByIds(unitIds)
        .thenCompose(units -> {
          if (unitIds.size() == units.size()) {
            // In case any unit is "soft deleted", just skip it (refer to MODORDERS-294)
            List<AcquisitionsUnit> activeUnits = units.stream()
              .filter(unit -> !unit.getIsDeleted())
              .collect(Collectors.toList());

            if (!activeUnits.isEmpty() && applyMergingStrategy(activeUnits, operations)) {
              return verifyUserIsMemberOfOrdersUnits(extractUnitIds(activeUnits));
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
  public CompletableFuture<Void> verifyIfUnitsAreActive(List<String> acqUnitIds) {
    if (acqUnitIds.isEmpty()) {
      return CompletableFuture.completedFuture(null);
    }

    return getUnitsByIds(acqUnitIds).thenAccept(units -> {
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
  private CompletableFuture<Void> verifyUserIsMemberOfOrdersUnits(List<String> unitIdsAssignedToOrder) {
    String query = String.format("userId==%s AND %s", getCurrentUserId(), convertIdsToCqlQuery(unitIdsAssignedToOrder, ACQUISITIONS_UNIT_ID, true));
    return acquisitionsUnitsHelper.getAcquisitionsUnitsMemberships(query, 0, 0)
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
  private CompletableFuture<List<AcquisitionsUnit>> getUnitsByIds(List<String> unitIds) {
    // Check if all required units are already available
    List<AcquisitionsUnit> units = fetchedUnits.stream()
      .filter(unit -> unitIds.contains(unit.getId()))
      .distinct()
      .collect(Collectors.toList());

    if (units.size() == unitIds.size()) {
      return CompletableFuture.completedFuture(units);
    }

    String query = combineCqlExpressions("and", ALL_UNITS_CQL, convertIdsToCqlQuery(unitIds));
    return acquisitionsUnitsHelper.getAcquisitionsUnits(query, 0, Integer.MAX_VALUE)
      .thenApply(acquisitionsUnitCollection -> {
        List<AcquisitionsUnit> acquisitionsUnits = acquisitionsUnitCollection.getAcquisitionsUnits();
        fetchedUnits.addAll(acquisitionsUnits);
        return acquisitionsUnits;
      });
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
