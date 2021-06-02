package org.folio.service;

import static org.folio.orders.utils.HelperUtils.combineCqlExpressions;
import static org.folio.orders.utils.ResourcePathResolver.ACQUISITIONS_MEMBERSHIPS;
import static org.folio.orders.utils.ResourcePathResolver.ACQUISITIONS_UNITS;
import static org.folio.orders.utils.ResourcePathResolver.resourcesPath;
import static org.folio.rest.RestVerticle.OKAPI_USERID_HEADER;

import java.util.List;
import java.util.concurrent.CompletableFuture;
import java.util.stream.Collectors;

import org.apache.commons.lang3.StringUtils;
import org.apache.logging.log4j.LogManager;
import org.apache.logging.log4j.Logger;
import org.folio.orders.utils.HelperUtils;
import org.folio.rest.core.RestClient;
import org.folio.rest.core.models.RequestContext;
import org.folio.rest.core.models.RequestEntry;
import org.folio.rest.jaxrs.model.AcquisitionsUnit;
import org.folio.rest.jaxrs.model.AcquisitionsUnitCollection;
import org.folio.rest.jaxrs.model.AcquisitionsUnitMembership;
import org.folio.rest.jaxrs.model.AcquisitionsUnitMembershipCollection;

import one.util.streamex.StreamEx;

public class AcquisitionsUnitsService {
  private static final Logger logger = LogManager.getLogger();

  public static final String ACQUISITIONS_UNIT_IDS = "acqUnitIds";
  private static final String NO_ACQ_UNIT_ASSIGNED_CQL = "cql.allRecords=1 not " + ACQUISITIONS_UNIT_IDS + " <> []";

  private static final String IS_DELETED_PROP = "isDeleted";
  private static final String ACTIVE_UNITS_CQL = IS_DELETED_PROP + "==false";
  private static final String ENDPOINT_ACQ_UNITS_MEMBERSHIPS = resourcesPath(ACQUISITIONS_MEMBERSHIPS);
  private static final String ENDPOINT_ACQ_UNITS_MEMBERSHIPS_BY_ID = ENDPOINT_ACQ_UNITS_MEMBERSHIPS + "/{id}";
  private static final String ENDTOPINT_ACQ_UNITS = resourcesPath(ACQUISITIONS_UNITS);
  private static final String ENDTOPINT_ACQ_UNITS_BY_ID = ENDTOPINT_ACQ_UNITS + "/{id}";

  private final RestClient restClient;

  public AcquisitionsUnitsService(RestClient restClient) {
    this.restClient = restClient;
  }

  public CompletableFuture<AcquisitionsUnitCollection> getAcquisitionsUnits(String query, int offset, int limit, RequestContext requestContext) {
    CompletableFuture<AcquisitionsUnitCollection> future = new CompletableFuture<>();

    try {
      // In case if client did not specify filter by "deleted" units, return only "active" units
      if (StringUtils.isEmpty(query)) {
        query = ACTIVE_UNITS_CQL;
      } else if (!query.contains(IS_DELETED_PROP)) {
        query = combineCqlExpressions("and", ACTIVE_UNITS_CQL, query);
      }
      RequestEntry requestEntry = new RequestEntry(ENDTOPINT_ACQ_UNITS).withQuery(query).withLimit(limit).withOffset(offset);
      restClient.get(requestEntry, requestContext, AcquisitionsUnitCollection.class)
                .thenAccept(future::complete)
                .exceptionally(t -> {
                  future.completeExceptionally(t.getCause());
                  return null;
                });
    } catch (Exception e) {
      future.completeExceptionally(e);
    }

    return future;
  }

  public CompletableFuture<AcquisitionsUnit> createAcquisitionsUnit(AcquisitionsUnit unit, RequestContext requestContext) {
    RequestEntry requestEntry = new RequestEntry(ENDTOPINT_ACQ_UNITS);
    return restClient.post(requestEntry, unit, requestContext, AcquisitionsUnit.class);
  }

  public CompletableFuture<Void> updateAcquisitionsUnit(AcquisitionsUnit unit, RequestContext requestContext) {
    RequestEntry requestEntry = new RequestEntry(ENDTOPINT_ACQ_UNITS_BY_ID).withId(unit.getId());
    return restClient.put(requestEntry, unit, requestContext);
  }

  public CompletableFuture<AcquisitionsUnit> getAcquisitionsUnit(String id, RequestContext requestContext) {
    RequestEntry requestEntry = new RequestEntry(ENDTOPINT_ACQ_UNITS_BY_ID).withId(id);
    return restClient.get(requestEntry, requestContext, AcquisitionsUnit.class);
  }

  public CompletableFuture<Void> deleteAcquisitionsUnit(String id, RequestContext requestContext) {
    return getAcquisitionsUnit(id, requestContext).thenApply(unit -> unit.withIsDeleted(true))
      .thenCompose(unit -> updateAcquisitionsUnit(unit, requestContext));
  }

  public CompletableFuture<String> buildAcqUnitsCqlExprToSearchRecords(RequestContext requestContext, String tableAlias) {
    return getAcqUnitIdsForSearch(requestContext).thenApply(ids -> {
      if (ids.isEmpty()) {
        return NO_ACQ_UNIT_ASSIGNED_CQL;
      }
      return String.format("%s or (%s)", HelperUtils.convertFieldListToCqlQuery(ids, tableAlias + ACQUISITIONS_UNIT_IDS, false), NO_ACQ_UNIT_ASSIGNED_CQL);
    });
  }

  private CompletableFuture<List<String>> getAcqUnitIdsForSearch(RequestContext requestContext) {
    return getAcqUnitIdsForUser(getCurrentUserId(requestContext), requestContext)
      .thenCombine(getOpenForReadAcqUnitIds(requestContext), (unitsForUser, unitsAllowRead) -> StreamEx.of(unitsForUser, unitsAllowRead)
        .flatCollection(strings -> strings)
        .distinct()
        .toList());
  }

  private CompletableFuture<List<String>> getAcqUnitIdsForUser(String userId, RequestContext requestContext) {
    return getAcquisitionsUnitsMemberships("userId==" + userId, 0, Integer.MAX_VALUE, requestContext)
      .thenApply(memberships -> {
        List<String> ids = memberships.getAcquisitionsUnitMemberships()
          .stream()
          .map(AcquisitionsUnitMembership::getAcquisitionsUnitId)
          .collect(Collectors.toList());

        if (logger.isDebugEnabled()) {
          logger.debug("User belongs to {} acq units: {}", ids.size(), StreamEx.of(ids).joining(", "));
        }

        return ids;
      });
  }

  public CompletableFuture<AcquisitionsUnitMembershipCollection> getAcquisitionsUnitsMemberships(String query, int offset, int limit,
                                                                                                 RequestContext requestContext) {
    CompletableFuture<AcquisitionsUnitMembershipCollection> future = new CompletableFuture<>();
    try {
      RequestEntry requestEntry = new RequestEntry(ENDPOINT_ACQ_UNITS_MEMBERSHIPS).withQuery(query).withLimit(limit).withOffset(offset);
      restClient.get(requestEntry, requestContext, AcquisitionsUnitMembershipCollection.class)
        .thenAccept(future::complete)
        .exceptionally(t -> {
          future.completeExceptionally(t.getCause());
          return null;
        });
    } catch (Exception e) {
      future.completeExceptionally(e);
    }
    return future;
  }

  public CompletableFuture<AcquisitionsUnitMembership> createAcquisitionsUnitsMembership(AcquisitionsUnitMembership membership,
                                                                                         RequestContext requestContext) {
    RequestEntry requestEntry = new RequestEntry(ENDPOINT_ACQ_UNITS_MEMBERSHIPS);
    return restClient.post(requestEntry, membership, requestContext, AcquisitionsUnitMembership.class);
  }

  public CompletableFuture<Void> updateAcquisitionsUnitsMembership(AcquisitionsUnitMembership membership, RequestContext requestContext) {
    RequestEntry requestEntry = new RequestEntry(ENDPOINT_ACQ_UNITS_MEMBERSHIPS_BY_ID).withId(membership.getId());
    return restClient.put(requestEntry, membership, requestContext);
  }

  public CompletableFuture<AcquisitionsUnitMembership> getAcquisitionsUnitsMembership(String id, RequestContext requestContext) {
    RequestEntry requestEntry = new RequestEntry(ENDPOINT_ACQ_UNITS_MEMBERSHIPS_BY_ID).withId(id);
    return restClient.get(requestEntry, requestContext, AcquisitionsUnitMembership.class);
  }

  public CompletableFuture<Void> deleteAcquisitionsUnitsMembership(String id, RequestContext requestContext) {
    RequestEntry requestEntry = new RequestEntry(ENDPOINT_ACQ_UNITS_MEMBERSHIPS_BY_ID).withId(id);
    return restClient.delete(requestEntry, requestContext);
  }

  private CompletableFuture<List<String>> getOpenForReadAcqUnitIds(RequestContext requestContext) {
    return getAcquisitionsUnits("protectRead==false", 0, Integer.MAX_VALUE, requestContext).thenApply(units -> {
      List<String> ids = units.getAcquisitionsUnits()
        .stream()
        .map(AcquisitionsUnit::getId)
        .collect(Collectors.toList());

      if (logger.isDebugEnabled()) {
        logger.debug("{} acq units with 'protectRead==false' are found: {}", ids.size(), StreamEx.of(ids).joining(", "));
      }

      return ids;
    });
  }

  private String getCurrentUserId(RequestContext requestContext) {
    return requestContext.getHeaders().get(OKAPI_USERID_HEADER);
  }
}
