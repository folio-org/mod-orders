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
import org.apache.commons.collections4.CollectionUtils;
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

  /**
   * Unlinks a title from a package. This involves checking the existence of holdings and items for the title.
   * If the holdings have references to other titles, the unlinking process skips them.
   * If the holdings do not have references, it asks for confirmation to delete related pieces, items, and holdings.
   *
   * @param titleId        the ID of the title to unlink
   * @param deleteHolding  flag indicating whether to delete the holding if it has no other references
   * @param requestContext the request context
   * @return a Future representing the result of the unlinking operation
   */
  public Future<List<String>> unlinkTitleFromPackage(String titleId, String deleteHolding, RequestContext requestContext) {
    log.debug("Trying to unlink title with id: {} and deleteHolding: {}", titleId, deleteHolding);

    return getTitleById(titleId, requestContext)
      .compose(title -> purchaseOrderLineService.getOrderLineById(title.getPoLineId(), requestContext)
        .compose(poLine -> processHoldings(poLine, deleteHolding, requestContext))
        .compose(holdings -> deleteTitle(title, holdings, requestContext)));
  }

  private Future<List<String>> processHoldings(PoLine poLine, String deleteHolding, RequestContext requestContext) {
    return getTitleByPoLineId(poLine.getId(), requestContext)
      .compose(titles -> {
        if (titles.size() > 1) {
          log.info("processHoldings:: Holdings in poLine '{}' connected to multiple '{}' titles", titles.size(), poLine.getId());
          return Future.succeededFuture();
        }

        if (StringUtils.isEmpty(deleteHolding)) {
          log.info("processHoldings:: Holdings in poLine '{}' will not be deleted", poLine.getId());
          return getHoldings(poLine, requestContext);
        }

        if (!"true".equalsIgnoreCase(deleteHolding) && !"false".equalsIgnoreCase(deleteHolding)) {
          throw new IllegalArgumentException("deleteHolding must be either 'true' or 'false'");
        }

        if (Boolean.parseBoolean(deleteHolding)) {
          return deleteHoldingItemPieces(poLine, requestContext)
            .mapEmpty();
        }

        return Future.succeededFuture();
      });
  }

  private Future<List<String>> getHoldings(PoLine poLine, RequestContext requestContext) {
    return pieceStorageService.getPiecesByLineId(poLine.getId(), requestContext)
      .compose(pieces -> {
        var holdingIdsFromPieces = pieces.stream().map(Piece::getHoldingId).toList();
        var holdingIdsFromPoLine = PoLineCommonUtil.getHoldings(poLine);
        var holdingIds = StreamEx.of(holdingIdsFromPieces, holdingIdsFromPoLine).flatMap(List::stream).distinct().toList();
        return Future.succeededFuture(CollectionUtils.isNotEmpty(holdingIds) ? holdingIds : List.of());
      });
  }

  private Future<Void> deleteHoldingItemPieces(PoLine poLine, RequestContext requestContext) {
    // TODO: add correct context for deletion of holdings reference change instance connection
    return getHoldings(poLine, requestContext)
      .compose(holdingIds -> deleteItemsForHolding(holdingIds, requestContext)
        .compose(v -> deletePiecesForPoLine(poLine.getId(), requestContext))
        .compose(v -> deleteHoldings(holdingIds, requestContext)));
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
      .onSuccess(v -> log.info("deleteItemsForHolding:: Items were deleted successfully for holdingIds: {}", holdingIds))
      .onFailure(t -> log.error("deleteItemsForHolding:: Failed to delete items for holdingIds: {}", holdingIds, t))
      .mapEmpty();
  }

  private Future<Void> deletePiecesForPoLine(String poLineId, RequestContext requestContext) {
    return pieceStorageService.getPiecesByLineId(poLineId, requestContext)
      .compose(pieces -> {
        var pieceIds = pieces.stream().map(Piece::getId).toList();
        return pieceStorageService.deletePiecesByIds(pieceIds, requestContext);
      })
      .onSuccess(v -> log.info("deletePiecesForPoLine:: Pieces were deleted successfully for poLineId: {}", poLineId))
      .onFailure(t -> log.error("deletePiecesForPoLine:: Failed to delete pieces for poLineId: {}", poLineId, t));
  }

  private Future<Void> deleteHoldings(List<String> holdingIds, RequestContext requestContext) {
    var deleteHoldingFutures = holdingIds.stream()
      .map(holdingId -> inventoryHoldingManager.deleteHoldingById(holdingId, true, requestContext))
      .toList();

    return collectResultsOnSuccess(deleteHoldingFutures)
      .onSuccess(v -> log.info("deleteHoldings:: Holdings '{}' were deleted successfully", holdingIds))
      .onFailure(t -> log.error("deleteHoldings:: Failed to delete holdings '{}'", holdingIds, t))
      .mapEmpty();
  }

  private Future<List<String>> deleteTitle(Title title, List<String> holdings, RequestContext requestContext) {
    if (CollectionUtils.isNotEmpty(holdings)) {
      return Future.succeededFuture(holdings);
    }

    log.info("unlinkTitleFromPackage:: Title '{}' is being unlinked from the package", title.getId());
    return deleteTitle(title.getId(), requestContext)
      .mapEmpty();
  }
}
