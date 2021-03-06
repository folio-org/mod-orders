package org.folio.service.titles;

import static java.util.Collections.singletonList;
import static java.util.stream.Collectors.groupingBy;
import static org.folio.orders.utils.HelperUtils.collectResultsOnSuccess;
import static org.folio.orders.utils.ResourcePathResolver.TITLES;
import static org.folio.orders.utils.ResourcePathResolver.resourcesPath;
import static org.folio.rest.RestConstants.MAX_IDS_FOR_GET_RQ;

import java.util.List;
import java.util.Map;
import java.util.Objects;
import java.util.concurrent.CompletableFuture;
import java.util.function.Function;

import org.apache.logging.log4j.LogManager;
import org.apache.logging.log4j.Logger;
import org.folio.orders.rest.exceptions.HttpException;
import org.folio.orders.utils.ErrorCodes;
import org.folio.orders.utils.HelperUtils;
import org.folio.rest.core.RestClient;
import org.folio.rest.core.models.RequestContext;
import org.folio.rest.core.models.RequestEntry;
import org.folio.rest.jaxrs.model.PoLine;
import org.folio.rest.jaxrs.model.Title;
import org.folio.rest.jaxrs.model.TitleCollection;
import org.folio.service.orders.PurchaseOrderLineService;

import one.util.streamex.StreamEx;

public class TitlesService {
  private static final Logger logger = LogManager.getLogger(TitlesService.class);
  private static final String ENDPOINT = resourcesPath(TITLES);
  private static final String BY_ID_ENDPOINT = ENDPOINT + "/{id}";

  private final PurchaseOrderLineService purchaseOrderLineService;
  private final RestClient restClient;

  public TitlesService(RestClient restClient, PurchaseOrderLineService purchaseOrderLineService) {
    this.restClient = restClient;
    this.purchaseOrderLineService = purchaseOrderLineService;
  }

  public CompletableFuture<Title> createTitle(Title title, RequestContext requestContext) {
    CompletableFuture<Title> future = new CompletableFuture<>();
    populateTitle(title, title.getPoLineId(), requestContext)
      .thenCompose(v -> {
        RequestEntry requestEntry = new RequestEntry(ENDPOINT);
        return restClient.post(requestEntry, title, requestContext, Title.class);
      })
      .thenAccept(future::complete)
      .exceptionally(t -> {
        future.completeExceptionally(t);
        return null;
      });
    return future;
  }

  public CompletableFuture<TitleCollection> getTitles(int limit, int offset, String query, RequestContext requestContext) {
    RequestEntry requestEntry = new RequestEntry(ENDPOINT).withQuery(query).withOffset(offset).withLimit(limit);
    return restClient.get(requestEntry, requestContext, TitleCollection.class);
  }

  public CompletableFuture<Title> getTitleById(String titleId, RequestContext requestContext) {
    RequestEntry requestEntry = new RequestEntry(BY_ID_ENDPOINT).withId(titleId);
    return restClient.get(requestEntry, requestContext, Title.class);
  }

  public CompletableFuture<Void> updateTitle(Title title, RequestContext requestContext) {
    RequestEntry requestEntry = new RequestEntry(BY_ID_ENDPOINT).withId(title.getId());
    return restClient.put(requestEntry, title, requestContext);
  }

  public CompletableFuture<Void> deleteTitle(String id, RequestContext requestContext) {
    RequestEntry requestEntry = new RequestEntry(BY_ID_ENDPOINT).withId(id);
    return restClient.delete(requestEntry, requestContext);
  }

  public CompletableFuture<Map<String, List<Title>>> getTitlesByPoLineIds(List<String> poLineIds, RequestContext requestContext) {
    return collectResultsOnSuccess(StreamEx
      .ofSubLists(poLineIds, MAX_IDS_FOR_GET_RQ)
      // Transform piece id's to CQL query
      .map(ids -> HelperUtils.convertFieldListToCqlQuery(ids, "poLineId", true))
      // Send get request for each CQL query
      .map(query -> getTitlesByQuery(query, requestContext))
      .toList())
      .thenApply(lists -> StreamEx.of(lists)
        .toFlatList(Function.identity()).stream().collect(groupingBy(Title::getPoLineId)));
  }

  private CompletableFuture<Void> populateTitle(Title title, String poLineId, RequestContext requestContext) {
    CompletableFuture<Void> future = new CompletableFuture<>();

    purchaseOrderLineService.getOrderLineById(poLineId, requestContext)
      .thenAccept(poLine -> {
        if(Boolean.TRUE.equals(poLine.getIsPackage())) {
          populateTitleByPoLine(title, poLine);
          future.complete(null);
        }
        else {
          getTitlesByPoLineIds(singletonList(poLineId), requestContext)
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

  private CompletableFuture<List<Title>> getTitlesByQuery(String query, RequestContext requestContext) {
    return getTitles(MAX_IDS_FOR_GET_RQ, 0, query, requestContext)
      .thenApply(TitleCollection::getTitles)
      .exceptionally(e -> {
        logger.error("The issue happened getting PO Lines", e);
        return null;
      });
  }

}
