package org.folio.rest.impl;

import static java.util.stream.Collectors.groupingBy;
import static org.folio.orders.utils.HelperUtils.buildQuery;
import static org.folio.orders.utils.HelperUtils.collectResultsOnSuccess;
import static org.folio.orders.utils.ResourcePathResolver.TITLES;
import static org.folio.orders.utils.ResourcePathResolver.resourceByIdPath;
import static org.folio.orders.utils.ResourcePathResolver.resourcesPath;

import java.util.List;
import java.util.Map;
import java.util.concurrent.CompletableFuture;
import java.util.function.Function;

import org.folio.orders.utils.HelperUtils;
import org.folio.rest.jaxrs.model.Title;
import org.folio.rest.jaxrs.model.TitleCollection;
import org.folio.rest.tools.client.interfaces.HttpClientInterface;

import io.vertx.core.Context;
import io.vertx.core.json.JsonObject;
import me.escoffier.vertx.completablefuture.VertxCompletableFuture;
import one.util.streamex.StreamEx;

public class TitlesHelper extends AbstractHelper {
  private static final String GET_TITLES_BY_QUERY = resourcesPath(TITLES) + SEARCH_PARAMS;

  public TitlesHelper(Map<String, String> okapiHeaders, Context ctx, String lang) {
    super(okapiHeaders, ctx, lang);
  }

  public TitlesHelper(HttpClientInterface httpClient, Map<String, String> okapiHeaders, Context ctx, String lang) {
    super(httpClient, okapiHeaders, ctx, lang);
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

  public CompletableFuture<Map<String, List<Title>>> getTitlesByPoLineIds(List<String> poLineIds) {
    return collectResultsOnSuccess(StreamEx
      .ofSubLists(poLineIds, MAX_IDS_FOR_GET_RQ)
      // Transform piece id's to CQL query
      .map(ids -> HelperUtils.convertIdsToCqlQuery(ids, "poLineId", true))
      // Send get request for each CQL query
      .map(this::getTitlesByQuery)
      .toList())
      .thenApply(lists -> StreamEx.of(lists)
        .toFlatList(Function.identity()).stream().collect(groupingBy(Title::getPoLineId)));
  }

  private CompletableFuture<List<Title>> getTitlesByQuery(String query) {
    return getTitles(MAX_IDS_FOR_GET_RQ, 0, query)
      .thenApply(TitleCollection::getTitles)
      .exceptionally(e -> {
        logger.error("The issue happened getting PO Lines", e);
        return null;
      });
  }

}
