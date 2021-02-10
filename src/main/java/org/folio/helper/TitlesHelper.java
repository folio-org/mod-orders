package org.folio.helper;

import static java.util.Collections.singletonList;
import static java.util.stream.Collectors.groupingBy;
import static org.folio.orders.utils.HelperUtils.buildQuery;
import static org.folio.orders.utils.HelperUtils.collectResultsOnSuccess;
import static org.folio.orders.utils.HelperUtils.getPoLineById;
import static org.folio.orders.utils.ResourcePathResolver.TITLES;
import static org.folio.orders.utils.ResourcePathResolver.resourceByIdPath;
import static org.folio.orders.utils.ResourcePathResolver.resourcesPath;

import java.util.List;
import java.util.Map;
import java.util.Objects;
import java.util.concurrent.CompletableFuture;
import java.util.function.Function;

import org.folio.orders.rest.exceptions.HttpException;
import org.folio.orders.utils.ErrorCodes;
import org.folio.orders.utils.HelperUtils;
import org.folio.rest.jaxrs.model.PoLine;
import org.folio.rest.jaxrs.model.Title;
import org.folio.rest.jaxrs.model.TitleCollection;
import org.folio.rest.tools.client.interfaces.HttpClientInterface;

import io.vertx.core.Context;
import io.vertx.core.json.JsonObject;
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
    CompletableFuture<Title> future = new CompletableFuture<>();
    populateTitle(title, title.getPoLineId())
      .thenCompose(v -> createRecordInStorage(JsonObject.mapFrom(title), resourcesPath(TITLES)).thenApply(title::withId))
      .thenAccept(future::complete)
      .exceptionally(t -> {
        future.completeExceptionally(t);
        return null;
      });
    return future;
  }

  private CompletableFuture<Void> populateTitle(Title title, String poLineId) {
    CompletableFuture<Void> future = new CompletableFuture<>();

    getPoLineById(poLineId, lang, httpClient, okapiHeaders, logger)
      .thenApply(json -> json.mapTo(PoLine.class))
      .thenAccept(poLine -> {
        if(Boolean.TRUE.equals(poLine.getIsPackage())) {
          populateTitleByPoLine(title, poLine);
          future.complete(null);
        }
        else {
          getTitlesByPoLineIds(singletonList(poLineId))
            .thenAccept(titles -> {
              if (titles.isEmpty()) {
                populateTitleByPoLine(title, poLine);
                future.complete(null);
              } else {
                future.completeExceptionally(new HttpException(422, ErrorCodes.TITLE_EXIST));
              }
            });
        }
      })
      .exceptionally(t -> {
        future.completeExceptionally(t);
        return null;
      });
    return future;
  }

  private void populateTitleByPoLine(Title title, PoLine poLine) {
    title.setPackageName(poLine.getTitleOrPackage());
    title.setExpectedReceiptDate(Objects.nonNull(poLine.getPhysical()) ? poLine.getPhysical().getExpectedReceiptDate() : null);
    title.setPoLineNumber(poLine.getPoLineNumber());
    if(poLine.getDetails() != null) {
      title.setReceivingNote(poLine.getDetails().getReceivingNote());
    }
  }

  public CompletableFuture<TitleCollection> getTitles(int limit, int offset, String query) {
    String endpoint = String.format(GET_TITLES_BY_QUERY, limit, offset, buildQuery(query, logger), lang);
    return HelperUtils.handleGetRequest(endpoint, httpClient, okapiHeaders, logger)
      .thenApply(json -> json.mapTo(TitleCollection.class));
  }

  public CompletableFuture<Title> getTitle(String id) {
    return HelperUtils.handleGetRequest(resourceByIdPath(TITLES, id), httpClient, okapiHeaders,logger)
      .thenApply(json -> json.mapTo(Title.class));
  }

  public CompletableFuture<Void> updateTitle(Title title) {
    return HelperUtils.handlePutRequest(resourceByIdPath(TITLES, title.getId()), JsonObject.mapFrom(title), httpClient, okapiHeaders, logger);
  }

  public CompletableFuture<Void> deleteTitle(String id) {
    return HelperUtils.handleDeleteRequest(resourceByIdPath(TITLES, id), httpClient, okapiHeaders, logger);
  }

  public CompletableFuture<Map<String, List<Title>>> getTitlesByPoLineIds(List<String> poLineIds) {
    return collectResultsOnSuccess(StreamEx
      .ofSubLists(poLineIds, MAX_IDS_FOR_GET_RQ)
      // Transform piece id's to CQL query
      .map(ids -> HelperUtils.convertFieldListToCqlQuery(ids, "poLineId", true))
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
