package org.folio.service;

import static org.folio.orders.utils.HelperUtils.combineCqlExpressions;
import static org.folio.orders.utils.ResourcePathResolver.ACQUISITIONS_MEMBERSHIPS;
import static org.folio.orders.utils.ResourcePathResolver.ACQUISITIONS_UNITS;
import static org.folio.orders.utils.ResourcePathResolver.resourcesPath;
import static org.folio.service.UserService.getCurrentUserId;

import java.util.List;
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

import io.vertx.core.Future;
import one.util.streamex.StreamEx;

public class AcquisitionsUnitsService {
  private static final Logger logger = LogManager.getLogger();

  public static final String ACQUISITIONS_UNIT_IDS = "acqUnitIds";
  private static final String NO_ACQ_UNIT_ASSIGNED_CQL = "cql.allRecords=1 not %s" + ACQUISITIONS_UNIT_IDS + " <> []";

  private static final String IS_DELETED_PROP = "isDeleted";
  private static final String ACTIVE_UNITS_CQL = IS_DELETED_PROP + "==false";
  private static final String ENDPOINT_ACQ_UNITS_MEMBERSHIPS = resourcesPath(ACQUISITIONS_MEMBERSHIPS);
  private static final String ENDPOINT_ACQ_UNITS_MEMBERSHIPS_BY_ID = ENDPOINT_ACQ_UNITS_MEMBERSHIPS + "/{id}";
  public static final String ENDPOINT_ACQ_UNITS = resourcesPath(ACQUISITIONS_UNITS);
  private static final String ENDPOINT_ACQ_UNITS_BY_ID = ENDPOINT_ACQ_UNITS + "/{id}";

  private final RestClient restClient;

  public AcquisitionsUnitsService(RestClient restClient) {
    this.restClient = restClient;
  }

  public Future<AcquisitionsUnitCollection> getAcquisitionsUnits(String query, int offset, int limit, RequestContext requestContext) {
    return Future.succeededFuture()
      .map(v -> {
        String combinedQuery = query;
        // In case if client did not specify filter by "deleted" units, return only "active" units
        if (StringUtils.isEmpty(query)) {
          combinedQuery = ACTIVE_UNITS_CQL;
        } else if (!query.contains(IS_DELETED_PROP)) {
          combinedQuery = combineCqlExpressions("and", ACTIVE_UNITS_CQL, query);
        }
        return new RequestEntry(ENDPOINT_ACQ_UNITS)
          .withQuery(combinedQuery)
          .withLimit(limit)
          .withOffset(offset);
      })
      .compose(requestEntry -> getAcquisitionsUnits(requestEntry, requestContext));
  }
  public Future<AcquisitionsUnitCollection> getAcquisitionsUnits(RequestEntry requestEntry, RequestContext requestContext) {
    return restClient.get(requestEntry, AcquisitionsUnitCollection.class, requestContext)
      .onFailure(t -> logger.error("Failed to retrieve acq units", t));
  }

  public Future<AcquisitionsUnit> createAcquisitionsUnit(AcquisitionsUnit unit, RequestContext requestContext) {
    RequestEntry requestEntry = new RequestEntry(ENDPOINT_ACQ_UNITS);
    return restClient.post(requestEntry, unit, AcquisitionsUnit.class, requestContext);
  }

  public Future<Void> updateAcquisitionsUnit(AcquisitionsUnit unit, RequestContext requestContext) {
    RequestEntry requestEntry = new RequestEntry(ENDPOINT_ACQ_UNITS_BY_ID).withId(unit.getId());
    return restClient.put(requestEntry, unit, requestContext);
  }

  public Future<AcquisitionsUnit> getAcquisitionsUnit(String id, RequestContext requestContext) {
    RequestEntry requestEntry = new RequestEntry(ENDPOINT_ACQ_UNITS_BY_ID).withId(id);
    return restClient.get(requestEntry, AcquisitionsUnit.class, requestContext);
  }

  public Future<Void> deleteAcquisitionsUnit(String id, RequestContext requestContext) {
    return getAcquisitionsUnit(id, requestContext).map(unit -> unit.withIsDeleted(true))
      .compose(unit -> updateAcquisitionsUnit(unit, requestContext));
  }

  public Future<String> buildAcqUnitsCqlExprToSearchRecords(String tableAlias, RequestContext requestContext) {
    return getAcqUnitIdsForSearch(requestContext).map(ids -> {
      String noAcqUnitAssignedQuery = String.format(NO_ACQ_UNIT_ASSIGNED_CQL, tableAlias);
      if (ids.isEmpty()) {
        return noAcqUnitAssignedQuery;
      }
      return String.format("%s or (%s)",
        HelperUtils.convertFieldListToCqlQuery(ids, tableAlias + ACQUISITIONS_UNIT_IDS, false),
        noAcqUnitAssignedQuery);
    });
  }

  private Future<List<String>> getAcqUnitIdsForSearch(RequestContext requestContext) {
    return getAcqUnitIdsForUser(getCurrentUserId(requestContext.getHeaders()), requestContext)
      .compose(unitsForUser -> getOpenForReadAcqUnitIds(requestContext)
        .map(unitsAllowRead -> StreamEx.of(unitsForUser, unitsAllowRead)
        .flatCollection(strings -> strings)
        .distinct()
        .toList()));
  }

  private Future<List<String>> getAcqUnitIdsForUser(String userId, RequestContext requestContext) {
    return getAcquisitionsUnitsMemberships("userId==" + userId, 0, Integer.MAX_VALUE, requestContext)
      .map(memberships -> {
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

  public Future<AcquisitionsUnitMembershipCollection> getAcquisitionsUnitsMemberships(String query, int offset, int limit, RequestContext requestContext) {
    RequestEntry requestEntry = new RequestEntry(ENDPOINT_ACQ_UNITS_MEMBERSHIPS)
      .withQuery(query)
      .withLimit(limit)
      .withOffset(offset);
    return restClient.get(requestEntry, AcquisitionsUnitMembershipCollection.class, requestContext);
  }

  public Future<AcquisitionsUnitMembership> createAcquisitionsUnitsMembership(AcquisitionsUnitMembership membership,
                                                                                         RequestContext requestContext) {
    RequestEntry requestEntry = new RequestEntry(ENDPOINT_ACQ_UNITS_MEMBERSHIPS);
    return restClient.post(requestEntry, membership, AcquisitionsUnitMembership.class, requestContext);
  }

  public Future<Void> updateAcquisitionsUnitsMembership(AcquisitionsUnitMembership membership, RequestContext requestContext) {
    RequestEntry requestEntry = new RequestEntry(ENDPOINT_ACQ_UNITS_MEMBERSHIPS_BY_ID).withId(membership.getId());
    return restClient.put(requestEntry, membership, requestContext);
  }

  public Future<AcquisitionsUnitMembership> getAcquisitionsUnitsMembership(String id, RequestContext requestContext) {
    RequestEntry requestEntry = new RequestEntry(ENDPOINT_ACQ_UNITS_MEMBERSHIPS_BY_ID).withId(id);
    return restClient.get(requestEntry, AcquisitionsUnitMembership.class, requestContext);
  }

  public Future<Void> deleteAcquisitionsUnitsMembership(String id, RequestContext requestContext) {
    RequestEntry requestEntry = new RequestEntry(ENDPOINT_ACQ_UNITS_MEMBERSHIPS_BY_ID).withId(id);
    return restClient.delete(requestEntry, requestContext);
  }

  private Future<List<String>> getOpenForReadAcqUnitIds(RequestContext requestContext) {
    return getAcquisitionsUnits("protectRead==false", 0, Integer.MAX_VALUE, requestContext)
      .map(units -> {
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
}
