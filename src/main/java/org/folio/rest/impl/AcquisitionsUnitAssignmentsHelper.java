package org.folio.rest.impl;

import static org.folio.orders.utils.HelperUtils.buildQuery;
import static org.folio.orders.utils.HelperUtils.handleDeleteRequest;
import static org.folio.orders.utils.HelperUtils.handleGetRequest;
import static org.folio.orders.utils.HelperUtils.handlePutRequest;
import static org.folio.orders.utils.ResourcePathResolver.*;

import java.util.Map;
import java.util.concurrent.CompletableFuture;

import org.folio.rest.jaxrs.model.AcquisitionsUnitAssignment;
import org.folio.rest.jaxrs.model.AcquisitionsUnitAssignmentCollection;

import io.vertx.core.Context;
import io.vertx.core.json.JsonObject;
import me.escoffier.vertx.completablefuture.VertxCompletableFuture;

public class AcquisitionsUnitAssignmentsHelper extends AbstractHelper {

  private static final String GET_UNIT_ASSIGNMENTS_BY_QUERY = resourcesPath(ACQUISITIONS_UNIT_ASSIGNMENTS) + "?offset=%s&limit=%s%s&lang=%s";

  AcquisitionsUnitAssignmentsHelper(Map<String, String> okapiHeaders, Context ctx, String lang) {
    super(okapiHeaders, ctx, lang);
  }

  CompletableFuture<AcquisitionsUnitAssignmentCollection> getAcquisitionsUnitAssignments(String query, int offset, int limit) {
    CompletableFuture<AcquisitionsUnitAssignmentCollection> future = new VertxCompletableFuture<>(ctx);

    try {
      String endpoint = String.format(GET_UNIT_ASSIGNMENTS_BY_QUERY, offset, limit, buildQuery(query, logger), lang);

      handleGetRequest(endpoint, httpClient, ctx, okapiHeaders, logger)
        .thenApply(jsonUnits -> jsonUnits.mapTo(AcquisitionsUnitAssignmentCollection.class))
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

  CompletableFuture<AcquisitionsUnitAssignment> createAcquisitionsUnitAssignment(AcquisitionsUnitAssignment unitAssignment) {
    return createRecordInStorage(JsonObject.mapFrom(unitAssignment), resourcesPath(ACQUISITIONS_UNIT_ASSIGNMENTS)).thenApply(unitAssignment::withId);
  }

  CompletableFuture<Void> updateAcquisitionsUnitAssignment(AcquisitionsUnitAssignment unitAssignment) {
    String endpoint = resourceByIdPath(ACQUISITIONS_UNIT_ASSIGNMENTS, unitAssignment.getId());
    return handlePutRequest(endpoint, JsonObject.mapFrom(unitAssignment), httpClient, ctx, okapiHeaders, logger);
  }

  CompletableFuture<AcquisitionsUnitAssignment> getAcquisitionsUnitAssignment(String id) {
    return handleGetRequest(resourceByIdPath(ACQUISITIONS_UNIT_ASSIGNMENTS, id), httpClient, ctx, okapiHeaders, logger)
      .thenApply(json -> json.mapTo(AcquisitionsUnitAssignment.class));
  }

  CompletableFuture<Void> deleteAcquisitionsUnitAssignment(String id) {
    return handleDeleteRequest(resourceByIdPath(ACQUISITIONS_UNIT_ASSIGNMENTS, id), httpClient, ctx, okapiHeaders, logger);
  }
}
