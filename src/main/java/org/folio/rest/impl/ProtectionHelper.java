package org.folio.rest.impl;

import static org.folio.orders.utils.ErrorCodes.ORDER_UNITS_NOT_FOUND;
import static org.folio.orders.utils.ErrorCodes.USER_HAS_NO_PERMISSIONS;
import static org.folio.orders.utils.HelperUtils.convertIdsToCqlQuery;

import java.util.Collection;
import java.util.Collections;
import java.util.List;
import java.util.Map;
import java.util.Set;
import java.util.concurrent.CompletableFuture;

import org.apache.commons.collections4.CollectionUtils;
import org.folio.HttpStatus;
import org.folio.orders.rest.exceptions.HttpException;
import org.folio.orders.utils.ProtectedOperationType;
import org.folio.rest.jaxrs.model.AcquisitionsUnit;
import org.folio.rest.jaxrs.model.AcquisitionsUnitCollection;
import org.folio.rest.jaxrs.model.CompositePurchaseOrder;

import io.vertx.core.Context;
import org.folio.rest.tools.client.interfaces.HttpClientInterface;

public class ProtectionHelper extends AbstractHelper {

  public static final String ACQUISITIONS_UNIT_ID = "acquisitionsUnitId";
  private AcquisitionsUnitsHelper acquisitionsUnitsHelper;

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
   * @param unitIds list of unit IDs.
   *
   * @throws HttpException if user hasn't permissions or units not found
   */
  public CompletableFuture<Void> isOperationRestricted(List<String> unitIds, Set<ProtectedOperationType> operations) {
    if (CollectionUtils.isNotEmpty(unitIds)) {
      return getUnitsByIds(unitIds)
        .thenCompose(units -> {
          if (unitIds.size() == units.size()) {
            if (applyMergingStrategy(units, operations)) {
              return verifyUserIsMemberOfOrdersUnits(unitIds);
            }
            return CompletableFuture.completedFuture(null);
          } else {
            throw new HttpException(HttpStatus.HTTP_VALIDATION_ERROR.toInt(), ORDER_UNITS_NOT_FOUND);
          }
        });
    } else {
      return CompletableFuture.completedFuture(null);
    }
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
    String query = convertIdsToCqlQuery(unitIds);
    return acquisitionsUnitsHelper.getAcquisitionsUnits(query, 0, Integer.MAX_VALUE)
      .thenApply(AcquisitionsUnitCollection::getAcquisitionsUnits);
  }

  /**
   * This method returns operation protection resulted status based on list of units with using least restrictive wins strategy.
   *
   * @param units list of {@link AcquisitionsUnit}.
   * @return true if operation is protected, otherwise - false.
   */
  private Boolean applyMergingStrategy(List<AcquisitionsUnit> units, Set<ProtectedOperationType> operations) {
    return units.stream().allMatch(unit -> operations.stream().anyMatch(operation -> operation.isProtected(unit)));
  }

}
