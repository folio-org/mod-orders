package org.folio.rest.impl;

import static java.util.stream.Collectors.toMap;
import static org.folio.orders.utils.HelperUtils.buildQuery;
import static org.folio.orders.utils.HelperUtils.collectResultsOnSuccess;
import static org.folio.orders.utils.HelperUtils.handlePutRequest;
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
import one.util.streamex.StreamEx;

public class TitlesHelper extends AbstractHelper {

  private static final String GET_TITLES_BY_QUERY = resourcesPath(TITLES) + SEARCH_PARAMS;

  public TitlesHelper(HttpClientInterface httpClient, Map<String, String> okapiHeaders, Context ctx, String lang) {
    super(httpClient, okapiHeaders, ctx, lang);
  }

  public CompletableFuture<TitleCollection> getTitles(String query, int offset, int limit) {
      String endpoint = String.format(GET_TITLES_BY_QUERY, limit, offset, buildQuery(query, logger), lang);
      return HelperUtils.getTitles(endpoint, httpClient, ctx, okapiHeaders, logger);
  }

  public CompletableFuture<Map<String, Title>> getTitlesByPoLineIds(List<String> poLineIds) {
    return collectResultsOnSuccess(StreamEx
      .ofSubLists(poLineIds, MAX_IDS_FOR_GET_RQ)
      // Transform piece id's to CQL query
      .map(ids -> HelperUtils.convertIdsToCqlQuery(ids, "poLineId", true))
      // Send get request for each CQL query
      .map(this::getTitlesByQuery)
      .toList())
      .thenApply(lists -> StreamEx.of(lists)
        .toFlatList(Function.identity()).stream().collect(toMap(Title::getPoLineId, Function.identity())));
  }

  private CompletableFuture<List<Title>> getTitlesByQuery(String query) {
    return getTitles(query, 0, MAX_IDS_FOR_GET_RQ )
      .thenApply(TitleCollection::getTitles)
      .exceptionally(e -> {
        logger.error("The issue happened getting PO Lines", e);
        return null;
      });
  }

  CompletableFuture<Void> updateTitle(Title title) {
    String endpoint = resourceByIdPath(TITLES, title.getId());
    return handlePutRequest(endpoint, JsonObject.mapFrom(title), httpClient, ctx, okapiHeaders, logger);
  }
}
