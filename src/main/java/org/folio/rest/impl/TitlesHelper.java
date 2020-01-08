package org.folio.rest.impl;

import java.util.Map;
import java.util.concurrent.CompletableFuture;

import io.vertx.core.Context;
import io.vertx.core.json.JsonObject;
import me.escoffier.vertx.completablefuture.VertxCompletableFuture;
import org.folio.orders.utils.HelperUtils;
import org.folio.rest.jaxrs.model.Title;
import org.folio.rest.jaxrs.model.TitleCollection;

import static org.folio.orders.utils.HelperUtils.buildQuery;
import static org.folio.orders.utils.ResourcePathResolver.TITLES;
import static org.folio.orders.utils.ResourcePathResolver.resourceByIdPath;
import static org.folio.orders.utils.ResourcePathResolver.resourcesPath;

public class TitlesHelper extends AbstractHelper {
  private static final String GET_TITLES_BY_QUERY = resourcesPath(TITLES) + SEARCH_PARAMS;

  TitlesHelper(Map<String, String> okapiHeaders, Context ctx, String lang) {
    super(okapiHeaders, ctx, lang);
  }

  public CompletableFuture<Title> createTitle(Title title) {
    return createRecordInStorage(JsonObject.mapFrom(title), resourcesPath(TITLES)).thenApply(title::withId);
  }

  public CompletableFuture<TitleCollection> getTitles(int limit, int offset, String query) {
    String endpoint = String.format(GET_TITLES_BY_QUERY, limit, offset, buildQuery(query, logger), lang);
    return HelperUtils.handleGetRequest(endpoint, httpClient, ctx, okapiHeaders, logger)
      .thenCompose(json -> VertxCompletableFuture.supplyBlockingAsync(ctx, () -> json.mapTo(TitleCollection.class)));
  }

  public CompletableFuture<Title> getTitle(String id) {
    return HelperUtils.handleGetRequest(resourceByIdPath(TITLES, id), httpClient, ctx, okapiHeaders,logger)
      .thenApply(json -> json.mapTo(Title.class));
  }

  public CompletableFuture<Void> updateTitle(Title title) {
    return HelperUtils.handlePutRequest(resourceByIdPath(TITLES, title.getId()), JsonObject.mapFrom(title), httpClient, ctx, okapiHeaders, logger);
  }

  public CompletableFuture<Void> deleteTitle(String id) {
    return HelperUtils.handleDeleteRequest(resourceByIdPath(TITLES, id), httpClient, ctx, okapiHeaders, logger);
  }

}
