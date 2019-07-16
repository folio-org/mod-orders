package org.folio.rest.impl;

import io.vertx.core.Context;
import org.folio.HttpStatus;
import org.folio.orders.rest.exceptions.HttpException;
import org.folio.rest.jaxrs.model.*;

import java.util.List;
import java.util.Map;
import java.util.concurrent.CompletableFuture;

import static java.util.stream.Collectors.toList;
import static org.folio.orders.utils.ErrorCodes.*;
import static org.folio.orders.utils.HelperUtils.convertIdsToCqlQuery;
import static org.folio.rest.RestVerticle.OKAPI_USERID_HEADER;

public class ProtectionHelper extends AbstractHelper {

  private AcquisitionsUnitsHelper acquisitionsUnitsHelper;
  private AcquisitionsUnitAssignmentsHelper acquisitionsUnitAssignmentsHelper;
  private Operation operation;


  private ProtectionHelper(Map<String, String> okapiHeaders, Context ctx, String lang, Operation operation) {
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
  public CompletableFuture<Boolean> isOperationRestrictedDefault(String recordId) {
    String userId;
    if(okapiHeaders != null && (userId = okapiHeaders.get(OKAPI_USERID_HEADER)) != null) {
      return getUnitIdsAssignedToOrder(recordId)
        .thenCompose(unitIds -> {
          CompletableFuture<Boolean> future = new CompletableFuture<>();
          if(!unitIds.isEmpty()) {
            getUnitsByIds(unitIds)
              .thenApply(units -> {
                if(!units.isEmpty()) {
                  return applyMergingStrategy(units)
                    .thenAccept(isOperationProtected -> {
                      if(isOperationProtected) {
                        getUnitIdsAssignedToUserAndOrder(userId, unitIds)
                          .thenAccept(ids -> future.complete(ids.isEmpty()));
                      } else {
                        future.complete(false);
                      }
                    });
                } else {
                  future.complete(false);
                }
                return future;
              });
          } else {
            future.complete(false);
          }
          return future;
        });
    } else {
      throw new HttpException(HttpStatus.HTTP_FORBIDDEN.toInt(), UNKNOWN_USER);
    }
  }

  /**
   * This method determines status of operation restriction based on unit IDs from {@link CompositePurchaseOrder}.
   * @param unitIds list of unit IDs.
   *
   * @return true if operation is restricted, otherwise - false.
   */
  public CompletableFuture<Boolean> isOrderCreationRestricted(List<String> unitIds) {
    if (unitIds != null && !unitIds.isEmpty()) {
      String userId;
      if(okapiHeaders != null && (userId = okapiHeaders.get(OKAPI_USERID_HEADER)) != null) {
        CompletableFuture<Boolean> future = new CompletableFuture<>();
        getUnitsByIds(unitIds)
          .thenApply(units -> {
            if(!units.isEmpty()) {
              return applyMergingStrategy(units)
                .thenAccept(isOperationProtected -> {
                  if(isOperationProtected) {
                    getUnitIdsAssignedToUserAndOrder(userId, unitIds)
                      .thenAccept(ids -> future.complete(ids.isEmpty()));
                  } else {
                    future.complete(false);
                  }
                });
            } else {
              future.completeExceptionally(new HttpException(HttpStatus.HTTP_VALIDATION_ERROR.toInt(), ORDER_UNITS_NOT_FOUND));
              return null;
            }
          });
        return future;
      } else {
        throw new HttpException(HttpStatus.HTTP_FORBIDDEN.toInt(), UNKNOWN_USER);
      }
    } else {
      throw new HttpException(HttpStatus.HTTP_FORBIDDEN.toInt(), ORDER_UNITS_NOT_FOUND);
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
  private CompletableFuture<List<String>> getUnitIdsAssignedToUserAndOrder(String userId, List<String> unitIdsAssignedToOrder) {
    String query = String.format("userId==%s AND %s", userId, convertIdsToCqlQuery(unitIdsAssignedToOrder, "acquisitionsUnitId"));
    return acquisitionsUnitsHelper.getAcquisitionsUnitsMemberships(query, 0, Integer.MAX_VALUE)
      .thenApply(memberships -> memberships.getAcquisitionsUnitMemberships().stream()
        .map(AcquisitionsUnitMembership::getAcquisitionsUnitId).collect(toList()));
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
  private CompletableFuture<Boolean> applyMergingStrategy(List<AcquisitionsUnit> units) {
    return CompletableFuture.completedFuture(units.stream().allMatch(unit -> operation.isProtected(unit)));
  }

  public enum Operation {

    CREATE {
      @Override
      public boolean isProtected(AcquisitionsUnit unit) {
        return unit.getProtectCreate();
      }
    },
    READ {
      @Override
      public boolean isProtected(AcquisitionsUnit unit) {
        return unit.getProtectRead();
      }
    },
    UPDATE {
      @Override
      public boolean isProtected(AcquisitionsUnit unit) {
        return unit.getProtectUpdate();
      }
    },
    DELETE {
      @Override
      public boolean isProtected(AcquisitionsUnit unit) {
        return unit.getProtectDelete();
      }
    };

    public abstract boolean isProtected(AcquisitionsUnit unit);

    public ProtectionHelper getInstance(Map<String, String> okapiHeaders, Context ctx, String lang) {
      return new ProtectionHelper(okapiHeaders, ctx, lang, this);
    }
  }

}
