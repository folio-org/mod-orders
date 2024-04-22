package org.folio.service.titles;

import static java.util.stream.Collectors.groupingBy;
import static java.util.stream.Collectors.toList;
import static org.folio.orders.utils.AcqDesiredPermissions.TITLES_ASSIGN;
import static org.folio.orders.utils.AcqDesiredPermissions.TITLES_MANAGE;
import static org.folio.orders.utils.HelperUtils.collectResultsOnSuccess;
import static org.folio.orders.utils.ProtectedOperationType.DELETE;
import static org.folio.orders.utils.ResourcePathResolver.TITLES;
import static org.folio.orders.utils.ResourcePathResolver.resourcesPath;
import static org.folio.rest.RestConstants.MAX_IDS_FOR_GET_RQ_15;

import java.util.List;
import java.util.Map;
import java.util.function.Function;

import com.google.common.collect.Sets;
import org.apache.commons.lang3.StringUtils;
import org.folio.orders.utils.HelperUtils;
import org.folio.orders.utils.ProtectedOperationType;
import org.folio.rest.core.RestClient;
import org.folio.rest.core.models.RequestContext;
import org.folio.rest.core.models.RequestEntry;
import org.folio.rest.jaxrs.model.CompositePoLine;
import org.folio.rest.jaxrs.model.CompositePurchaseOrder;
import org.folio.rest.jaxrs.model.Title;
import org.folio.rest.jaxrs.model.TitleCollection;
import org.folio.rest.tools.utils.TenantTool;
import org.folio.service.ProtectionService;
import org.folio.service.inventory.InventoryInstanceManager;

import io.vertx.core.Future;
import lombok.extern.log4j.Log4j2;
import one.util.streamex.StreamEx;

@Log4j2
public class TitlesService {
  private static final String ENDPOINT = resourcesPath(TITLES);
  private static final String BY_ID_ENDPOINT = ENDPOINT + "/{id}";
  private final RestClient restClient;
  private final ProtectionService protectionService;
  private final InventoryInstanceManager inventoryInstanceManager;

  public TitlesService(RestClient restClient, ProtectionService protectionService, InventoryInstanceManager inventoryInstanceManager) {
    this.restClient = restClient;
    this.protectionService = protectionService;
    this.inventoryInstanceManager = inventoryInstanceManager;
  }

  public Future<Title> createTitle(Title title, RequestContext requestContext) {
    return protectionService.validateAcqUnitsOnCreate(title.getAcqUnitIds(), TITLES_ASSIGN, requestContext)
      .compose(v -> inventoryInstanceManager.getOrCreateInstanceRecord(title, requestContext))
      .compose(instId -> {
        RequestEntry requestEntry = new RequestEntry(ENDPOINT);
        return restClient.post(requestEntry, title.withTitle(instId), Title.class, requestContext);
      });
  }

  public Future<TitleCollection> getTitles(int limit, int offset, String query, RequestContext requestContext) {
    return protectionService.getQueryWithAcqUnitsCheck(StringUtils.EMPTY, query, requestContext)
      .compose(finalQuery -> {
        RequestEntry requestEntry = new RequestEntry(ENDPOINT).withQuery(finalQuery)
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
    return saveTitleWithInstance(title, requestContext).mapEmpty();
  }

  public Future<String> saveTitleWithInstance(Title title, RequestContext requestContext) {
    return saveTitleWithInstance(title, false, requestContext);
  }

  public Future<String> saveTitleWithInstance(Title title, boolean isInstanceMatchingDisabled, RequestContext requestContext) {
    return inventoryInstanceManager.getOrCreateInstanceRecord(title, isInstanceMatchingDisabled, requestContext)
      .compose(instId -> {
        RequestEntry requestEntry = new RequestEntry(BY_ID_ENDPOINT).withId(title.getId());
        restClient.put(requestEntry, title, requestContext);
        return Future.succeededFuture(instId);
      });
  }

  public Future<Void> saveTitleWithAcqUnitsCheck(Title entity, RequestContext requestContext) {
    return getTitleById(entity.getId(), requestContext)
      .compose(titleFromStorage -> protectionService.validateAcqUnitsOnUpdate(entity.getAcqUnitIds(), titleFromStorage.getAcqUnitIds(),
        TITLES_MANAGE, Sets.newHashSet(ProtectedOperationType.UPDATE), requestContext))
      .compose(v -> saveTitle(entity, requestContext));
  }

  public Future<Void> deleteTitle(String id, RequestContext requestContext) {
    return getTitleById(id, requestContext)
      .compose(title -> protectionService.isOperationRestricted(title.getAcqUnitIds(), DELETE, requestContext))
      .compose(v -> {
        RequestEntry requestEntry = new RequestEntry(BY_ID_ENDPOINT).withId(id);
        return restClient.delete(requestEntry, requestContext);
      })
      .mapEmpty();
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

  public Future<List<Title>> getTitlesByPieceIds(List<String> pieceIds, RequestContext requestContext) {
    return collectResultsOnSuccess(StreamEx
      .ofSubLists(pieceIds, MAX_IDS_FOR_GET_RQ_15)
      // Transform piece id's to CQL query
      .map(ids -> HelperUtils.convertIdsToCqlQuery(ids, "pieces.id"))
      // Send get request for each CQL query
      .map(query -> getTitlesByQuery(query, requestContext))
      .toList())
      .map(lists -> StreamEx.of(lists).toFlatList(Function.identity()).stream().toList());
  }

  private Future<List<Title>> getTitlesByQuery(String query, RequestContext requestContext) {
    return getTitles(Integer.MAX_VALUE, 0, query, requestContext)
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
