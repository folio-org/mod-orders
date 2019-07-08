package org.folio.rest.impl;

import io.vertx.core.Context;
import org.folio.rest.jaxrs.model.AcquisitionsUnit;
import org.folio.rest.jaxrs.model.AcquisitionsUnitCollection;
import org.folio.rest.jaxrs.model.AcquisitionsUnitMembership;

import java.util.Iterator;
import java.util.List;
import java.util.Map;
import java.util.Optional;
import java.util.concurrent.CompletableFuture;
import java.util.function.BinaryOperator;

import static java.util.stream.Collectors.toList;

public class ProtectionHelper extends AbstractHelper {

  private AcquisitionsUnitsHelper acquisitionsUnitsHelper;
  private AcquisitionsMembershipsHelper acquisitionsMembershipsHelper;
  private AcquisitionsUnitAssignmentsHelper acquisitionsUnitAssignmentsHelper;
  private Operation operation;

  private static final String OKAPI_USER_ID_HEADER = "X-Okapi-User-Id";

  private ProtectionHelper(Map<String, String> okapiHeaders, Context ctx, String lang, Operation operation) {
    super(okapiHeaders, ctx, lang);
    acquisitionsUnitsHelper = new AcquisitionsUnitsHelper(okapiHeaders, ctx, lang);
    acquisitionsMembershipsHelper = new AcquisitionsMembershipsHelper(okapiHeaders, ctx, lang);
    acquisitionsUnitAssignmentsHelper = new AcquisitionsUnitAssignmentsHelper(okapiHeaders, ctx, lang);
    this.operation = operation;
  }

  /**
   * This method returns operation protection status.
   * False means that operation can be executed, true - operation is protected and can't be executed.
   *
   * @return true if operation is protected, otherwise - false.
   */
  public CompletableFuture<Boolean> isOperationProtected(String recordId) {
    CompletableFuture<Boolean> future = new CompletableFuture<>();
    isUnitsExist(recordId).thenAccept(isUnitsExist -> {
      if(isUnitsExist) {
        String userId;
        if(okapiHeaders != null && (userId = okapiHeaders.get(OKAPI_USER_ID_HEADER)) != null) {
            getUnitIds(userId)
              .thenCompose(this::getUnitsByIds)
              .thenCompose(this::isOperationProtected)
              .thenAccept(future::complete);
        } else {
          future.complete(true);
        }
      } else {
        future.complete(false);
      }
    });
    return future;
  }

  /**
   * This method checks existence of units associated with order.
   * @param recordId id of order.
   * @return true if units exist, otherwise - false.
   */
  private CompletableFuture<Boolean> isUnitsExist(String recordId) {
    return acquisitionsUnitAssignmentsHelper.getAcquisitionsUnitAssignments(String.format("recordId==%s", recordId), 0, Integer.MAX_VALUE)
      .thenApply(units -> units.getTotalRecords() > 0);
  }

  /**
   * This method returns list of units ids associated with User.
   * @param userId id of User.
   *
   * @return list of unit ids associated with user.
   */
  private CompletableFuture<List<String>> getUnitIds(String userId) {
    return acquisitionsMembershipsHelper.getAcquisitionsUnitsMemberships(String.format("userId==%s", userId), 0, Integer.MAX_VALUE)
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
    String query = buildUnitsQuery(unitIds);
    return acquisitionsUnitsHelper.getAcquisitionsUnits(query, 0, Integer.MAX_VALUE)
      .thenApply(AcquisitionsUnitCollection::getAcquisitionsUnits);
  }

  /**
   * This method returns operation protection resulted status based on list of units.
   *
   * @param units list of {@link AcquisitionsUnit}.
   * @return true if operation is protected, otherwise - false.
   */
  private CompletableFuture<Boolean> isOperationProtected(List<AcquisitionsUnit> units) {
    Optional<Boolean> result = units.stream().map(unit -> operation.isProtected(unit)).reduce(getMergingStrategy());
    return CompletableFuture.completedFuture(result.orElse(true));
  }

  /**
   * This method implements protection statuses merging strategy (least restrictive wins (AND) at the moment).
   *
   * @return result of protection statuses merging.
   */
  private BinaryOperator<Boolean> getMergingStrategy() {
    return (a, b) -> a && b;
  }

  private String buildUnitsQuery(List<String> unitIds) {
    StringBuilder query = new StringBuilder();
    Iterator<String> it = unitIds.iterator();
    while (it.hasNext()) {
      String id = it.next();
      if(it.hasNext()) {
        query.append(String.format("id==%s OR ", id));
      } else {
        query.append(String.format("id==%s", id));
      }
    }
    return new String(query);
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
