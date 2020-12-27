package org.folio.helper;

import static org.folio.orders.utils.HelperUtils.buildQuery;
import static org.folio.orders.utils.HelperUtils.combineCqlExpressions;
import static org.folio.orders.utils.HelperUtils.convertIdsToCqlQuery;
import static org.folio.orders.utils.HelperUtils.handleDeleteRequest;
import static org.folio.orders.utils.HelperUtils.handleGetRequest;
import static org.folio.orders.utils.HelperUtils.handlePutRequest;
import static org.folio.orders.utils.ResourcePathResolver.ACQUISITIONS_MEMBERSHIPS;
import static org.folio.orders.utils.ResourcePathResolver.ACQUISITIONS_UNITS;
import static org.folio.orders.utils.ResourcePathResolver.resourceByIdPath;
import static org.folio.orders.utils.ResourcePathResolver.resourcesPath;

import java.util.List;
import java.util.Map;
import java.util.concurrent.CompletableFuture;
import java.util.stream.Collectors;

import org.apache.commons.lang3.StringUtils;
import org.folio.rest.jaxrs.model.AcquisitionsUnit;
import org.folio.rest.jaxrs.model.AcquisitionsUnitCollection;
import org.folio.rest.jaxrs.model.AcquisitionsUnitMembership;
import org.folio.rest.jaxrs.model.AcquisitionsUnitMembershipCollection;
import org.folio.rest.tools.client.interfaces.HttpClientInterface;

import io.vertx.core.Context;
import io.vertx.core.json.JsonObject;
import one.util.streamex.StreamEx;

public class AcquisitionsUnitsHelper extends AbstractHelper {
  public static final String ACQUISITIONS_UNIT_IDS = "acqUnitIds";
  static final String IS_DELETED_PROP = "isDeleted";
  static final String ACTIVE_UNITS_CQL = IS_DELETED_PROP + "==false";
  static final String ALL_UNITS_CQL = IS_DELETED_PROP + "=*";
  static final String NO_ACQ_UNIT_ASSIGNED_CQL = "cql.allRecords=1 not " + ACQUISITIONS_UNIT_IDS + " <> []";
  private static final String GET_UNITS_BY_QUERY = resourcesPath(ACQUISITIONS_UNITS) + SEARCH_PARAMS;
  private static final String GET_UNITS_MEMBERSHIPS_BY_QUERY = resourcesPath(ACQUISITIONS_MEMBERSHIPS) + SEARCH_PARAMS;

  public AcquisitionsUnitsHelper(HttpClientInterface httpClient, Map<String, String> okapiHeaders, Context ctx, String lang) {
    super(httpClient, okapiHeaders, ctx, lang);
  }

  public AcquisitionsUnitsHelper(Map<String, String> okapiHeaders, Context ctx, String lang) {
    super(okapiHeaders, ctx, lang);
  }

  public CompletableFuture<AcquisitionsUnitCollection> getAcquisitionsUnits(String query, int offset, int limit) {
    CompletableFuture<AcquisitionsUnitCollection> future = new CompletableFuture<>();

    try {
      // In case if client did not specify filter by "deleted" units, return only "active" units
      if (StringUtils.isEmpty(query)) {
        query = ACTIVE_UNITS_CQL;
      } else if (!query.contains(IS_DELETED_PROP)) {
        query = combineCqlExpressions("and", ACTIVE_UNITS_CQL, query);
      }

      String endpoint = String.format(GET_UNITS_BY_QUERY, limit, offset, buildQuery(query, logger), lang);
      handleGetRequest(endpoint, httpClient, okapiHeaders, logger)
        .thenApply(jsonUnits -> jsonUnits.mapTo(AcquisitionsUnitCollection.class))
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

  public CompletableFuture<AcquisitionsUnit> createAcquisitionsUnit(AcquisitionsUnit unit) {
    return createRecordInStorage(JsonObject.mapFrom(unit), resourcesPath(ACQUISITIONS_UNITS)).thenApply(unit::withId);
  }

  public CompletableFuture<Void> updateAcquisitionsUnit(AcquisitionsUnit unit) {
    String endpoint = resourceByIdPath(ACQUISITIONS_UNITS, unit.getId());
    return handlePutRequest(endpoint, JsonObject.mapFrom(unit), httpClient, okapiHeaders, logger);
  }

  public CompletableFuture<AcquisitionsUnit> getAcquisitionsUnit(String id) {
    return handleGetRequest(resourceByIdPath(ACQUISITIONS_UNITS, id), httpClient, okapiHeaders, logger)
      .thenApply(json -> json.mapTo(AcquisitionsUnit.class));
  }

  public CompletableFuture<Void> deleteAcquisitionsUnit(String id) {
    return getAcquisitionsUnit(id).thenApply(unit -> unit.withIsDeleted(true))
      .thenCompose(this::updateAcquisitionsUnit);
  }

  public CompletableFuture<String> buildAcqUnitsCqlExprToSearchRecords() {
    return getAcqUnitIdsForSearch().thenApply(ids -> {
      if (ids.isEmpty()) {
        return NO_ACQ_UNIT_ASSIGNED_CQL;
      }

      return String.format("%s or (%s)", convertIdsToCqlQuery(ids, ACQUISITIONS_UNIT_IDS, false), NO_ACQ_UNIT_ASSIGNED_CQL);
    });
  }

  CompletableFuture<List<String>> getAcqUnitIdsForSearch() {
    return getAcqUnitIdsForUser(getCurrentUserId())
      .thenCombine(getOpenForReadAcqUnitIds(), (unitsForUser, unitsAllowRead) -> StreamEx.of(unitsForUser, unitsAllowRead)
        .flatCollection(strings -> strings)
        .distinct()
        .toList());
  }

  public CompletableFuture<AcquisitionsUnitMembershipCollection> getAcquisitionsUnitsMemberships(String query, int offset, int limit) {
    CompletableFuture<AcquisitionsUnitMembershipCollection> future = new CompletableFuture<>();
    try {
      String endpoint = String.format(GET_UNITS_MEMBERSHIPS_BY_QUERY, limit, offset, buildQuery(query, logger), lang);
      handleGetRequest(endpoint, httpClient, okapiHeaders, logger)
        .thenApply(jsonUnitsMembership -> jsonUnitsMembership.mapTo(AcquisitionsUnitMembershipCollection.class))
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

  CompletableFuture<List<String>> getAcqUnitIdsForUser(String userId) {
    return getAcquisitionsUnitsMemberships("userId==" + userId, 0, Integer.MAX_VALUE)
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

  public CompletableFuture<AcquisitionsUnitMembership> createAcquisitionsUnitsMembership(AcquisitionsUnitMembership membership) {
    return createRecordInStorage(JsonObject.mapFrom(membership), resourcesPath(ACQUISITIONS_MEMBERSHIPS)).thenApply(membership::withId);
  }

  public CompletableFuture<Void> updateAcquisitionsUnitsMembership(AcquisitionsUnitMembership membership) {
    String endpoint = resourceByIdPath(ACQUISITIONS_MEMBERSHIPS, membership.getId());
    return handlePutRequest(endpoint, JsonObject.mapFrom(membership), httpClient, okapiHeaders, logger);
  }

  public CompletableFuture<AcquisitionsUnitMembership> getAcquisitionsUnitsMembership(String id) {
    return handleGetRequest(resourceByIdPath(ACQUISITIONS_MEMBERSHIPS, id), httpClient, okapiHeaders, logger)
      .thenApply(json -> json.mapTo(AcquisitionsUnitMembership.class));
  }

  public CompletableFuture<Void> deleteAcquisitionsUnitsMembership(String id) {
    return handleDeleteRequest(resourceByIdPath(ACQUISITIONS_MEMBERSHIPS, id), httpClient, okapiHeaders, logger);
  }

  private CompletableFuture<List<String>> getOpenForReadAcqUnitIds() {
    return getAcquisitionsUnits("protectRead==false", 0, Integer.MAX_VALUE).thenApply(units -> {
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
