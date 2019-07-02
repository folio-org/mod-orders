package org.folio.rest.impl;

import io.vertx.core.Context;
import io.vertx.core.json.JsonObject;
import me.escoffier.vertx.completablefuture.VertxCompletableFuture;
import org.folio.rest.jaxrs.model.AcquisitionsUnitMembership;
import org.folio.rest.jaxrs.model.AcquisitionsUnitMembershipCollection;

import java.util.Map;
import java.util.concurrent.CompletableFuture;

import static org.folio.orders.utils.HelperUtils.buildQuery;
import static org.folio.orders.utils.HelperUtils.handleDeleteRequest;
import static org.folio.orders.utils.HelperUtils.handleGetRequest;
import static org.folio.orders.utils.HelperUtils.handlePutRequest;
import static org.folio.orders.utils.ResourcePathResolver.ACQUISITIONS_MEMBERSHIPS;
import static org.folio.orders.utils.ResourcePathResolver.resourceByIdPath;
import static org.folio.orders.utils.ResourcePathResolver.resourcesPath;

public class AcquisitionsMembershipsHelper extends AbstractHelper {
  private static final String GET_UNITS_MEMBERSHIPS_BY_QUERY = resourcesPath(ACQUISITIONS_MEMBERSHIPS) + "?offset=%s&limit=%s%s&lang=%s";

  AcquisitionsMembershipsHelper(Map<String, String> okapiHeaders, Context ctx, String lang) {
    super(okapiHeaders, ctx, lang);
  }

  CompletableFuture<AcquisitionsUnitMembershipCollection> getAcquisitionsUnitsMemberships(String query, int offset, int limit) {
    CompletableFuture<AcquisitionsUnitMembershipCollection> future = new VertxCompletableFuture<>(ctx);
    try {
      String endpoint = String.format(GET_UNITS_MEMBERSHIPS_BY_QUERY, offset, limit, buildQuery(query, logger), lang);
      handleGetRequest(endpoint, httpClient, ctx, okapiHeaders, logger)
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

  CompletableFuture<AcquisitionsUnitMembership> createAcquisitionsUnitsMembership(AcquisitionsUnitMembership membership) {
    return createRecordInStorage(JsonObject.mapFrom(membership), resourcesPath(ACQUISITIONS_MEMBERSHIPS)).thenApply(membership::withId);
  }

  CompletableFuture<Void> updateAcquisitionsUnitsMembership(AcquisitionsUnitMembership membership) {
    String endpoint = resourceByIdPath(ACQUISITIONS_MEMBERSHIPS, membership.getId());
    return handlePutRequest(endpoint, JsonObject.mapFrom(membership), httpClient, ctx, okapiHeaders, logger);
  }

  CompletableFuture<AcquisitionsUnitMembership> getAcquisitionsUnitsMembership(String id) {
    return handleGetRequest(resourceByIdPath(ACQUISITIONS_MEMBERSHIPS, id), httpClient, ctx, okapiHeaders, logger)
      .thenApply(json -> json.mapTo(AcquisitionsUnitMembership.class));
  }

  CompletableFuture<Void> deleteAcquisitionsUnitsMembership(String id) {
    return handleDeleteRequest(resourceByIdPath(ACQUISITIONS_MEMBERSHIPS, id), httpClient, ctx, okapiHeaders, logger);
  }
}
