package org.folio.rest.impl;

import io.vertx.core.Context;
import org.folio.HttpStatus;
import org.folio.orders.rest.exceptions.HttpException;
import org.folio.orders.utils.ProtectedOperationType;
import org.folio.rest.jaxrs.model.AcquisitionsUnit;
import org.folio.rest.jaxrs.model.AcquisitionsUnitAssignment;
import org.folio.rest.jaxrs.model.AcquisitionsUnitCollection;

import java.util.List;
import java.util.Map;
import java.util.concurrent.CompletableFuture;

import static java.util.stream.Collectors.toList;
import static org.folio.orders.utils.ErrorCodes.ORDER_UNITS_NOT_FOUND;
import static org.folio.orders.utils.ErrorCodes.USER_HAS_NO_PERMISSIONS;
import static org.folio.orders.utils.HelperUtils.convertIdsToCqlQuery;

public class ProtectionHelper extends AbstractHelper {

  private AcquisitionsUnitsHelper acquisitionsUnitsHelper;
  private AcquisitionsUnitAssignmentsHelper acquisitionsUnitAssignmentsHelper;
  private ProtectedOperationType operation;


  public ProtectionHelper(Map<String, String> okapiHeaders, Context ctx, String lang, ProtectedOperationType operation) {
    super(okapiHeaders, ctx, lang);
    acquisitionsUnitsHelper = new AcquisitionsUnitsHelper(okapiHeaders, ctx, lang);
    acquisitionsUnitAssignmentsHelper = new AcquisitionsUnitAssignmentsHelper(okapiHeaders, ctx, lang);
    this.operation = operation;
  }

  /**
   * This method determines status of operation restriction based on ID of {@link CompositePurchaseOrder}.
   * @param recordId corresponding record ID.
   *
   * @return true if operation is restricted, otherwise - false.
   */
  public CompletableFuture<Void> isOperationRestricted(String recordId) {
    return getUnitIdsAssignedToOrder(recordId)
      .thenCompose(this::isOperationRestricted);
  }

  /**
   * This method determines status of operation restriction based on unit IDs from {@link CompositePurchaseOrder}.
   * @param unitIds list of unit IDs.
   *
   * @return true if operation is restricted, otherwise - false.
   */
  public CompletableFuture<Void> isOperationRestricted(List<String> unitIds) {
    if(!unitIds.isEmpty()) {
      return getUnitsByIds(unitIds)
        .thenCompose(units -> {
          if(unitIds.size() == units.size()) {
                if(applyMergingStrategy(units)) {
                  return isUserMemberOfOrdersUnits(getCurrentUserId(), unitIds)
                    .thenAccept(isProtected -> {
                      if(isProtected) {
                        throw new HttpException(HttpStatus.HTTP_FORBIDDEN.toInt(), USER_HAS_NO_PERMISSIONS);
                      }
                    });
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
   * This method checks existence of units associated with order.
   * @param recordId id of order.
   *
   * @return true if units exist, otherwise - false.
   */
  private CompletableFuture<List<String>> getUnitIdsAssignedToOrder(String recordId) {
    return acquisitionsUnitAssignmentsHelper.getAcquisitionsUnitAssignments(String.format("recordId==%s", recordId), 0, Integer.MAX_VALUE)
      .thenApply(assignment -> assignment.getAcquisitionsUnitAssignments().stream().map(AcquisitionsUnitAssignment::getAcquisitionsUnitId).collect(toList()));
  }

  /**
   * This method returns list of units ids associated with User.
   * @param userId id of User.
   *
   * @return list of unit ids associated with user.
   */
  private CompletableFuture<Boolean> isUserMemberOfOrdersUnits(String userId, List<String> unitIdsAssignedToOrder) {
    String query = String.format("userId==%s AND %s", userId, convertIdsToCqlQuery(unitIdsAssignedToOrder, "acquisitionsUnitId"));
    return acquisitionsUnitsHelper.getAcquisitionsUnitsMemberships(query, 0, 0)
      .thenApply(unit -> unit.getTotalRecords() == 0);
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
  private Boolean applyMergingStrategy(List<AcquisitionsUnit> units) {
    return units.stream().allMatch(unit -> operation.isProtected(unit));
  }

}
