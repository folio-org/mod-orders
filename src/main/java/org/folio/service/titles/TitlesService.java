package org.folio.service.titles;

import static java.util.Collections.singletonList;
import static java.util.stream.Collectors.groupingBy;
import static java.util.stream.Collectors.toList;
import static org.apache.commons.lang3.StringUtils.isEmpty;
import static org.folio.orders.utils.HelperUtils.collectResultsOnSuccess;
import static org.folio.orders.utils.HelperUtils.combineCqlExpressions;
import static org.folio.orders.utils.ResourcePathResolver.TITLES;
import static org.folio.orders.utils.ResourcePathResolver.resourcesPath;
import static org.folio.rest.RestConstants.MAX_IDS_FOR_GET_RQ_15;

import java.util.List;
import java.util.Map;
import java.util.Objects;
import java.util.function.Function;

import org.folio.orders.utils.HelperUtils;
import org.folio.rest.core.RestClient;
import org.folio.rest.core.exceptions.ErrorCodes;
import org.folio.rest.core.exceptions.HttpException;
import org.folio.rest.core.models.RequestContext;
import org.folio.rest.core.models.RequestEntry;
import org.folio.rest.jaxrs.model.CompositePoLine;
import org.folio.rest.jaxrs.model.CompositePurchaseOrder;
import org.folio.rest.jaxrs.model.PoLine;
import org.folio.rest.jaxrs.model.Title;
import org.folio.rest.jaxrs.model.TitleCollection;
import org.folio.service.AcquisitionsUnitsService;
import org.folio.service.orders.PurchaseOrderLineService;

import io.vertx.core.Future;
import io.vertx.core.Promise;
import lombok.extern.log4j.Log4j2;
import one.util.streamex.StreamEx;

@Log4j2
public class TitlesService {
  private static final String ENDPOINT = resourcesPath(TITLES);
  private static final String BY_ID_ENDPOINT = ENDPOINT + "/{id}";

  private final PurchaseOrderLineService purchaseOrderLineService;
  private final RestClient restClient;

  private final AcquisitionsUnitsService acquisitionsUnitsService;

  public TitlesService(RestClient restClient, PurchaseOrderLineService purchaseOrderLineService,
                       AcquisitionsUnitsService acquisitionsUnitsService) {
    this.restClient = restClient;
    this.purchaseOrderLineService = purchaseOrderLineService;
    this.acquisitionsUnitsService = acquisitionsUnitsService;
  }

  public Future<Title> createTitle(Title title, RequestContext requestContext) {
    return populateTitle(title, title.getPoLineId(), requestContext)
      .compose(v -> {
        RequestEntry requestEntry = new RequestEntry(ENDPOINT);
        return restClient.post(requestEntry, title, Title.class, requestContext);
      });
  }

  public Future<TitleCollection> getTitles(int limit, int offset, String query, RequestContext requestContext) {
    return acquisitionsUnitsService.buildAcqUnitsCqlExprToSearchRecords("purchaseOrder.", requestContext)
      .compose(acqUnitsCqlExpr -> {
        String resultQuery = acqUnitsCqlExpr;
        if (!isEmpty(query)) {
          resultQuery = combineCqlExpressions("and", acqUnitsCqlExpr, query);
        }
        RequestEntry requestEntry = new RequestEntry(ENDPOINT).withQuery(resultQuery)
          .withOffset(offset)
          .withLimit(limit);
        return restClient.get(requestEntry, TitleCollection.class, requestContext);
      });

  }

  public Future<Title> getTitleById(String titleId, RequestContext requestContext) {
    RequestEntry requestEntry = new RequestEntry(BY_ID_ENDPOINT).withId(titleId);
    return restClient.get(requestEntry, Title.class, requestContext);
  }

  public Future<Void> saveTitle(Title title, RequestContext requestContext) {
    RequestEntry requestEntry = new RequestEntry(BY_ID_ENDPOINT).withId(title.getId());
    return restClient.put(requestEntry, title, requestContext);
  }

  public Future<Void> deleteTitle(String id, RequestContext requestContext) {
    RequestEntry requestEntry = new RequestEntry(BY_ID_ENDPOINT).withId(id);
    return restClient.delete(requestEntry, requestContext);
  }

  public Future<Map<String, List<Title>>> getTitlesByPoLineIds(List<String> poLineIds, RequestContext requestContext) {
    return collectResultsOnSuccess(StreamEx
      .ofSubLists(poLineIds, MAX_IDS_FOR_GET_RQ_15)
      // Transform piece id's to CQL query
      .map(ids -> HelperUtils.convertFieldListToCqlQuery(ids, "poLineId", true))
      // Send get request for each CQL query
      .map(query -> getTitlesByQuery(query, requestContext))
      .toList())
      .map(lists -> StreamEx.of(lists)
        .toFlatList(Function.identity()).stream().collect(groupingBy(Title::getPoLineId)));
  }

  private Future<Void> populateTitle(Title title, String poLineId, RequestContext requestContext) {
    Promise<Void> promise = Promise.promise();
    purchaseOrderLineService.getOrderLineById(poLineId, requestContext)
      .map(poLine -> {
        if(Boolean.TRUE.equals(poLine.getIsPackage())) {
          populateTitleByPoLine(title, poLine);
          promise.complete();
        }
        else {
          getTitlesByPoLineIds(singletonList(poLineId), requestContext)
            .onSuccess(titles -> {
              if (titles.isEmpty()) {
                populateTitleByPoLine(title, poLine);
                promise.complete();
              } else {
                promise.fail(new HttpException(422, ErrorCodes.TITLE_EXIST));
              }
            });
        }
        return null;
      })
      .onFailure(promise::fail);
    return promise.future();
  }

  private void populateTitleByPoLine(Title title, PoLine poLine) {
    title.setPackageName(poLine.getTitleOrPackage());
    title.setExpectedReceiptDate(Objects.nonNull(poLine.getPhysical()) ? poLine.getPhysical().getExpectedReceiptDate() : null);
    title.setPoLineNumber(poLine.getPoLineNumber());
    if(poLine.getDetails() != null) {
      title.setReceivingNote(poLine.getDetails().getReceivingNote());
    }
  }

  private Future<List<Title>> getTitlesByQuery(String query, RequestContext requestContext) {
    return getTitles(MAX_IDS_FOR_GET_RQ_15, 0, query, requestContext)
      .map(TitleCollection::getTitles);
  }

  public Future<Map<String, List<Title>>> fetchNonPackageTitles(CompositePurchaseOrder compPO, RequestContext requestContext) {
    List<String> lineIds = getNonPackageLineIds(compPO.getCompositePoLines());
    return getTitlesByPoLineIds(lineIds, requestContext);
  }


  private List<String> getNonPackageLineIds(List<CompositePoLine> compositePoLines) {
    return compositePoLines.stream().filter(line -> !line.getIsPackage()).map(CompositePoLine::getId).collect(toList());
  }
}
