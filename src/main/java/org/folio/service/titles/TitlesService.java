package org.folio.service.titles;

import static java.util.stream.Collectors.groupingBy;
import static java.util.stream.Collectors.toList;
import static org.folio.orders.utils.AcqDesiredPermissions.TITLES_ASSIGN;
import static org.folio.orders.utils.AcqDesiredPermissions.TITLES_MANAGE;
import static org.folio.orders.utils.HelperUtils.collectResultsOnSuccess;
import static org.folio.orders.utils.HelperUtils.combineResultListsOnSuccess;
import static org.folio.orders.utils.ProtectedOperationType.DELETE;
import static org.folio.orders.utils.ResourcePathResolver.TITLES;
import static org.folio.orders.utils.ResourcePathResolver.resourcesPath;
import static org.folio.rest.RestConstants.MAX_IDS_FOR_GET_RQ_15;

import java.util.ArrayList;
import java.util.List;
import java.util.Map;
import java.util.function.Function;

import com.google.common.collect.Sets;

import io.vertx.core.Future;
import lombok.extern.log4j.Log4j2;
import one.util.streamex.StreamEx;
import org.apache.commons.lang3.StringUtils;
import org.folio.orders.utils.PoLineCommonUtil;
import org.folio.orders.utils.ProtectedOperationType;
import org.folio.orders.utils.QueryUtils;
import org.folio.rest.core.RestClient;
import org.folio.rest.core.models.RequestContext;
import org.folio.rest.core.models.RequestEntry;
import org.folio.rest.jaxrs.model.CompositePoLine;
import org.folio.rest.jaxrs.model.CompositePurchaseOrder;
import org.folio.rest.jaxrs.model.Piece;
import org.folio.rest.jaxrs.model.PoLine;
import org.folio.rest.jaxrs.model.Title;
import org.folio.rest.jaxrs.model.TitleCollection;
import org.folio.service.ProtectionService;
import org.folio.service.inventory.InventoryHoldingManager;
import org.folio.service.inventory.InventoryItemManager;
import org.folio.service.orders.PurchaseOrderLineService;
import org.folio.service.pieces.PieceStorageService;

@Log4j2
public class TitlesService {
  private static final String ENDPOINT = resourcesPath(TITLES);
  private static final String BY_ID_ENDPOINT = ENDPOINT + "/{id}";
  private final RestClient restClient;
  private final ProtectionService protectionService;
  private final TitleInstanceService titleInstanceService;
  private final InventoryHoldingManager inventoryHoldingManager;
  private final InventoryItemManager inventoryItemManager;
  private final PurchaseOrderLineService purchaseOrderLineService;
  private final PieceStorageService pieceStorageService;

  public TitlesService(RestClient restClient, ProtectionService protectionService, TitleInstanceService titleInstanceService,
                       InventoryHoldingManager inventoryHoldingManager, InventoryItemManager inventoryItemManager, PurchaseOrderLineService purchaseOrderLineService, PieceStorageService pieceStorageService) {
    this.restClient = restClient;
    this.protectionService = protectionService;
    this.titleInstanceService = titleInstanceService;
    this.inventoryHoldingManager = inventoryHoldingManager;
    this.inventoryItemManager = inventoryItemManager;
    this.purchaseOrderLineService = purchaseOrderLineService;
    this.pieceStorageService = pieceStorageService;
  }

  public Future<Title> createTitle(Title title, RequestContext requestContext) {
    return protectionService.validateAcqUnitsOnCreate(title.getAcqUnitIds(), TITLES_ASSIGN, requestContext)
      .compose(v -> titleInstanceService.getOrCreateInstance(title, requestContext))
      .compose(instId -> {
        RequestEntry requestEntry = new RequestEntry(ENDPOINT);
        return restClient.post(requestEntry, title.withInstanceId(instId), Title.class, requestContext);
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
    RequestEntry requestEntry = new RequestEntry(BY_ID_ENDPOINT).withId(title.getId());
    return restClient.put(requestEntry, title, requestContext);
  }

  public Future<Void> saveTitleWithAcqUnitsCheck(Title entity, RequestContext requestContext) {
    return getTitleById(entity.getId(), requestContext)
      .compose(titleFromStorage -> protectionService.validateAcqUnitsOnUpdate(entity.getAcqUnitIds(), titleFromStorage.getAcqUnitIds(),
        TITLES_MANAGE, Sets.newHashSet(ProtectedOperationType.UPDATE), requestContext))
      .compose(v -> titleInstanceService.getOrCreateInstance(entity, requestContext))
      .map(entity::withInstanceId)
      .compose(title -> saveTitle(title, requestContext));
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
      .map(ids -> QueryUtils.convertFieldListToCqlQuery(ids, "poLineId", true))
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
      .map(ids -> QueryUtils.convertIdsToCqlQuery(ids, "pieces.id"))
      // Send get request for each CQL query
      .map(query -> getTitlesByQuery(query, requestContext))
      .toList())
      .map(lists -> StreamEx.of(lists).toFlatList(Function.identity()).stream().toList());
  }

  public Future<List<Title>> getTitlesByQuery(String query, RequestContext requestContext) {
    return getTitles(Integer.MAX_VALUE, 0, query, requestContext)
      .map(TitleCollection::getTitles);
  }

  public Future<List<Title>> getTitleByPoLineId(String poLineId, RequestContext requestContext) {
    return getTitlesByQuery("poLineId==" + poLineId, requestContext);
  }

  public Future<List<Title>> getTitlesByInstanceId(String instanceId, RequestContext requestContext) {
    return getTitlesByQuery("instanceId==" + instanceId, requestContext);
  }

  public Future<Map<String, List<Title>>> fetchNonPackageTitles(CompositePurchaseOrder compPO, RequestContext requestContext) {
    List<String> lineIds = getNonPackageLineIds(compPO.getCompositePoLines());
    return getTitlesByPoLineIds(lineIds, requestContext);
  }

  private List<String> getNonPackageLineIds(List<CompositePoLine> compositePoLines) {
    return compositePoLines.stream().filter(line -> !line.getIsPackage()).map(CompositePoLine::getId).collect(toList());
  }

  public Future<String> updateTitleWithInstance(String titleId, RequestContext locationContext, RequestContext requestContext) {
    return getTitleById(titleId, requestContext)
      .compose(title -> updateTitleWithInstance(title, false, locationContext, requestContext));
  }

  public Future<String> updateTitleWithInstance(Title title, boolean isInstanceMatchingDisabled, RequestContext locationContext, RequestContext requestContext) {
    return titleInstanceService.getOrCreateInstance(title, isInstanceMatchingDisabled, locationContext)
      .map(title::withInstanceId)
      .compose(entity -> saveTitle(entity, requestContext)
        .map(v -> entity.getInstanceId()));
  }

  public Future<Void> unlinkTitleFromPackage(String titleId, boolean deleteHolding, RequestContext requestContext) {
    // Check holding and item existence for title
    // if holdings have reference other titles then skip,
    // if holding doesn't have reference and ask confirm if yes then delete related pieces, items, holdings

    // check first holding and item existence
    // if holding exists and check if holding has connected to multiple titles through poLine
    // if holding exists and holding don't have connected titles and then return holding object to get confirmation delete holding true/false
    return getTitleById(titleId, requestContext)
      .compose(title -> purchaseOrderLineService.getOrderLineById(title.getPoLineId(), requestContext)
        .compose(poLine -> processHoldings(poLine, deleteHolding, requestContext))
        .compose(v -> unlinkTitleFromPackage(title, requestContext)));
  }

  private Future<Object> processHoldings(PoLine poLine, boolean deleteHolding, RequestContext requestContext) {
    return getTitleByPoLineId(poLine.getId(), requestContext)
      .compose(titles -> {
        if (titles.size() > 1) {
          return Future.succeededFuture();
        }
        if (Boolean.FALSE.equals(deleteHolding)) {
          List<String> holdingIds = checkHoldingExistence(poLine);
          return Future.succeededFuture(holdingIds);
        }
        return deleteHoldingItemPieces(poLine, requestContext)
          .map((String) null);
      });
  }

  private List<String> checkHoldingExistence(PoLine poLine) {
    return PoLineCommonUtil.getHoldings(poLine);
  }

  private Future<Void> deleteHoldingItemPieces(PoLine poLine, RequestContext requestContext) {
    var holdingIds = PoLineCommonUtil.getHoldings(poLine);

    return deleteItemsForHolding(holdingIds, requestContext)
      .compose(v -> deletePiecesForPoLine(poLine.getId(), requestContext))
      .compose(v -> deleteHoldings(holdingIds, requestContext));
  }

  private Future<Void> deleteItemsForHolding(List<String> holdingIds, RequestContext requestContext) {
    List<Future<List<Void>>> deleteItemFutures = new ArrayList<>();

    for (String holdingId : holdingIds) {
      var future = inventoryItemManager.getItemsByHoldingId(holdingId, requestContext)
        .compose(items -> {
          var itemIds = items.stream().map(item -> item.getString("id")).toList();
          return inventoryItemManager.deleteItems(itemIds, true, requestContext);
        });
      deleteItemFutures.add(future);
    }

    return combineResultListsOnSuccess(deleteItemFutures)
      .mapEmpty();
  }

  private Future<Void> deletePiecesForPoLine(String poLineId, RequestContext requestContext) {
    return pieceStorageService.getPiecesByLineId(poLineId, requestContext)
      .compose(pieces -> {
        var pieceIds = pieces.stream().map(Piece::getId).toList();
        return pieceStorageService.deletePiecesByIds(pieceIds, requestContext);
      });
  }

  private Future<Void> deleteHoldings(List<String> holdingIds, RequestContext requestContext) {
    var deleteHoldingFutures = holdingIds.stream()
      .map(holdingId -> inventoryHoldingManager.deleteHoldingById(holdingId, true, requestContext))
      .toList();

    return collectResultsOnSuccess(deleteHoldingFutures)
      .mapEmpty();
  }

  public Future<Void> unlinkTitleFromPackage(Title title, RequestContext requestContext) {
    title.setPackageName(null);
    title.setPoLineId(null);
    return saveTitleWithAcqUnitsCheck(title, requestContext);
  }
}
