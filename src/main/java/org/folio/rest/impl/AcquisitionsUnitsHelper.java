package org.folio.rest.impl;

import static org.folio.orders.utils.HelperUtils.buildQuery;
import static org.folio.orders.utils.HelperUtils.handleDeleteRequest;
import static org.folio.orders.utils.HelperUtils.handleGetRequest;
import static org.folio.orders.utils.HelperUtils.handlePutRequest;
import static org.folio.orders.utils.ResourcePathResolver.ACQUISITIONS_UNITS;
import static org.folio.orders.utils.ResourcePathResolver.resourceByIdPath;
import static org.folio.orders.utils.ResourcePathResolver.resourcesPath;

import java.util.Map;
import java.util.concurrent.CompletableFuture;

import org.folio.rest.jaxrs.model.AcquisitionsUnit;
import org.folio.rest.jaxrs.model.AcquisitionsUnitCollection;

import io.vertx.core.Context;
import io.vertx.core.json.JsonObject;
import me.escoffier.vertx.completablefuture.VertxCompletableFuture;

public class AcquisitionsUnitsHelper extends AbstractHelper {
  private static final String GET_UNITS_BY_QUERY = resourcesPath(ACQUISITIONS_UNITS) + "?offset=%s&limit=%s%s&lang=%s";

  AcquisitionsUnitsHelper(Map<String, String> okapiHeaders, Context ctx, String lang) {
    super(okapiHeaders, ctx, lang);
  }

  CompletableFuture<AcquisitionsUnitCollection> getAcquisitionsUnits(String query, int offset, int limit) {
    CompletableFuture<AcquisitionsUnitCollection> future = new VertxCompletableFuture<>(ctx);

    try {
      String endpoint = String.format(GET_UNITS_BY_QUERY, limit, offset, buildQuery(query, logger), lang);

      handleGetRequest(endpoint, httpClient, ctx, okapiHeaders, logger)
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

  CompletableFuture<AcquisitionsUnit> createAcquisitionsUnit(AcquisitionsUnit unit) {
    return createRecordInStorage(JsonObject.mapFrom(unit), resourcesPath(ACQUISITIONS_UNITS)).thenApply(unit::withId);
  }

  CompletableFuture<Void> updateAcquisitionsUnit(AcquisitionsUnit unit) {
    String endpoint = resourceByIdPath(ACQUISITIONS_UNITS, unit.getId());
    return handlePutRequest(endpoint, JsonObject.mapFrom(unit), httpClient, ctx, okapiHeaders, logger);
  }

  CompletableFuture<AcquisitionsUnit> getAcquisitionsUnit(String id) {
    return handleGetRequest(resourceByIdPath(ACQUISITIONS_UNITS, id), httpClient, ctx, okapiHeaders, logger)
      .thenApply(json -> json.mapTo(AcquisitionsUnit.class));
  }

  CompletableFuture<Void> deleteAcquisitionsUnit(String id) {
    return handleDeleteRequest(resourceByIdPath(ACQUISITIONS_UNITS, id), httpClient, ctx, okapiHeaders, logger);
  }
}
