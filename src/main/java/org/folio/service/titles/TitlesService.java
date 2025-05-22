package org.folio.service.titles;

import static java.util.stream.Collectors.groupingBy;
import static java.util.stream.Collectors.mapping;
import static java.util.stream.Collectors.toList;
import static org.folio.orders.utils.AcqDesiredPermissions.TITLES_ASSIGN;
import static org.folio.orders.utils.AcqDesiredPermissions.TITLES_MANAGE;
import static org.folio.orders.utils.FutureUtils.asFuture;
import static org.folio.orders.utils.HelperUtils.ID;
import static org.folio.orders.utils.HelperUtils.collectResultsOnSuccess;
import static org.folio.orders.utils.ProtectedOperationType.DELETE;
import static org.folio.orders.utils.RequestContextUtil.createContextWithNewTenantId;
import static org.folio.orders.utils.ResourcePathResolver.TITLES;
import static org.folio.orders.utils.ResourcePathResolver.resourcesPath;
import static org.folio.rest.RestConstants.BAD_REQUEST;
import static org.folio.rest.RestConstants.MAX_IDS_FOR_GET_RQ_15;
import static org.folio.rest.core.exceptions.ErrorCodes.EXISTING_HOLDINGS_FOR_DELETE_CONFIRMATION;
import static org.folio.rest.core.exceptions.ErrorCodes.EXISTING_RECEIVED_PIECES_TITLE_REMOVAL;
import static org.folio.service.inventory.InventoryUtils.canDeleteHoldingForTitleRemoval;
import static org.folio.service.inventory.InventoryUtils.canDeleteItemForTitleRemoval;
import static org.folio.service.pieces.PieceUtil.canDeletePieceForTitleRemoval;
import static org.folio.service.pieces.PieceUtil.getPieceTenantId;

import java.util.ArrayList;
import java.util.List;
import java.util.Map;
import java.util.Optional;
import java.util.function.Function;

import com.google.common.collect.Sets;

import io.vertx.core.Future;
import lombok.extern.log4j.Log4j2;
import one.util.streamex.StreamEx;
import org.apache.commons.collections4.CollectionUtils;
import org.apache.commons.lang3.StringUtils;
import org.apache.commons.lang3.tuple.Pair;
import org.folio.models.TitleHolder;
import org.folio.orders.utils.ProtectedOperationType;
import org.folio.orders.utils.QueryUtils;
import org.folio.rest.core.RestClient;
import org.folio.rest.core.exceptions.HttpException;
import org.folio.rest.core.models.RequestContext;
import org.folio.rest.core.models.RequestEntry;
import org.folio.rest.jaxrs.model.PoLine;
import org.folio.rest.jaxrs.model.CompositePurchaseOrder;
import org.folio.rest.jaxrs.model.Parameter;
import org.folio.rest.jaxrs.model.Piece;
import org.folio.rest.jaxrs.model.Title;
import org.folio.rest.jaxrs.model.TitleCollection;
import org.folio.service.ProtectionService;
import org.folio.service.consortium.ConsortiumConfigurationService;
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
  private final ConsortiumConfigurationService consortiumConfigurationService;

  public TitlesService(RestClient restClient, ProtectionService protectionService,
                       TitleInstanceService titleInstanceService,
                       InventoryHoldingManager inventoryHoldingManager, InventoryItemManager inventoryItemManager,
                       PurchaseOrderLineService purchaseOrderLineService, PieceStorageService pieceStorageService,
                       ConsortiumConfigurationService consortiumConfigurationService) {
    this.restClient = restClient;
    this.protectionService = protectionService;
    this.titleInstanceService = titleInstanceService;
    this.inventoryHoldingManager = inventoryHoldingManager;
    this.inventoryItemManager = inventoryItemManager;
    this.purchaseOrderLineService = purchaseOrderLineService;
    this.pieceStorageService = pieceStorageService;
    this.consortiumConfigurationService = consortiumConfigurationService;
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
      .compose(titleFromStorage -> protectionService.validateAcqUnitsOnUpdate(entity.getAcqUnitIds(),
        titleFromStorage.getAcqUnitIds(),
        TITLES_MANAGE, Sets.newHashSet(ProtectedOperationType.UPDATE), requestContext))
      .compose(v -> titleInstanceService.getOrCreateInstance(entity, requestContext))
      .map(entity::withInstanceId)
      .compose(title -> saveTitle(title, requestContext));
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

  public Future<Map<String, List<Title>>> fetchNonPackageTitles(CompositePurchaseOrder compPO,
                                                                RequestContext requestContext) {
    List<String> lineIds = getNonPackageLineIds(compPO.getPoLines());
    return getTitlesByPoLineIds(lineIds, requestContext);
  }

  private List<String> getNonPackageLineIds(List<PoLine> poLines) {
    return poLines.stream().filter(line -> !line.getIsPackage()).map(PoLine::getId).collect(toList());
  }

  public Future<String> updateTitleWithInstance(String titleId, RequestContext locationContext,
                                                RequestContext requestContext) {
    return getTitleById(titleId, requestContext)
      .compose(title -> updateTitleWithInstance(title, false, locationContext, requestContext));
  }

  public Future<String> updateTitleWithInstance(Title title, boolean isInstanceMatchingDisabled,
                                                RequestContext locationContext, RequestContext requestContext) {
    return titleInstanceService.getOrCreateInstance(title, isInstanceMatchingDisabled, locationContext)
      .map(title::withInstanceId)
      .compose(entity -> saveTitle(entity, requestContext)
        .map(v -> entity.getInstanceId()));
  }

  /**
   * Delete title and unlink poLine from title instance.
   * This involves checking the existence of holdings and items for the title.
   * If the holdings have references to other titles, the unlinking process skips them.
   * If the holdings do not have references, it asks for confirmation to delete related pieces, items, and holdings.
   *
   * @param titleId        the ID of the title to unlink
   * @param deleteHoldings flag indicating whether to delete the holding if it has no other references
   * @param requestContext the request context
   * @return a Future representing the result of the deleting title operation
   */
  public Future<Void> deleteTitle(String titleId, String deleteHoldings, RequestContext requestContext) {
    log.debug("Trying to delete title with id: {} and deleteHoldings: {}", titleId, deleteHoldings);

    return getTitleById(titleId, requestContext)
      .map(TitleHolder::new)
      .compose(holder -> protectionService.isOperationRestricted(holder.getTitle().getAcqUnitIds(), DELETE, requestContext)
        .map(v -> holder))
      .compose(holder -> purchaseOrderLineService.getOrderLineById(holder.getTitle().getPoLineId(), requestContext)
        .map(holder::setPoLine))
      .compose(holder -> pieceStorageService.getPiecesByLineIdAndTitleId(holder.getPoLineId(), holder.getTitleId(), requestContext)
        .map(holder::setPieces))
      .compose(holder -> consortiumConfigurationService.isCentralOrderingEnabled(requestContext)
        .map(holder::setCentralEnabled))
      .compose(holder -> getHoldingIdsGroupedByTenantId(holder, requestContext)
        .map(holder::setAllHoldingIdsByTenant))
      .compose(holder -> getHoldingsToDeleteWithTenants(holder, requestContext)
        .map(holder::setHoldingIdsToDeleteByTenant))
      .compose(holder -> processItemsPiecesHoldings(holder, deleteHoldings, requestContext)
        .map(v -> holder))
      .compose(holder -> deleteTitle(holder.getTitleId(), requestContext));
  }

  private Future<Void> processItemsPiecesHoldings(TitleHolder holder, String deleteHoldings, RequestContext requestContext) {
    if (CollectionUtils.isEmpty(holder.getAllHoldings())) {
      return Future.succeededFuture();
    }

    if (holder.getPieces().stream().anyMatch(piece -> piece.getReceivingStatus() == Piece.ReceivingStatus.RECEIVED)) {
      var error = EXISTING_RECEIVED_PIECES_TITLE_REMOVAL.toError();
      throw new HttpException(BAD_REQUEST, error);
    }

    // The parameter "deleteHoldings" is required if there are any holdings found for deletion
    if (CollectionUtils.isNotEmpty(holder.getHoldingIdsToDelete())) {
      if (StringUtils.isEmpty(deleteHoldings)) {
        log.info("processHoldings:: Holdings in poLine '{}' will not be deleted", holder.getPoLine().getId());
        var param = new Parameter().withKey("holdingIds").withValue(holder.getHoldingIdsToDelete().toString());
        var error = EXISTING_HOLDINGS_FOR_DELETE_CONFIRMATION.toError().withParameters(List.of(param));
        throw new HttpException(BAD_REQUEST, error);
      }
      if (!"true".equalsIgnoreCase(deleteHoldings) && !"false".equalsIgnoreCase(deleteHoldings)) {
        throw new IllegalArgumentException("deleteHolding must be either 'true' or 'false'");
      }
    }

    holder.setDeleteHoldings(Boolean.parseBoolean(deleteHoldings));
    return deleteItemsPiecesHoldings(holder, requestContext);
  }

  private Future<Map<String, List<String>>> getHoldingIdsGroupedByTenantId(TitleHolder holder, RequestContext requestContext) {
    return asFuture(() -> StreamEx.of(holder.getPieces())
        .filter(piece -> StringUtils.isNotEmpty(piece.getHoldingId()))
        .groupingBy(piece -> getPieceTenantId(piece, requestContext), mapping(Piece::getHoldingId, toList())));
  }

  private Future<Map<String, List<String>>> getHoldingsToDeleteWithTenants(TitleHolder holder, RequestContext requestContext) {
    if (holder.getAllHoldingIdsByTenant().isEmpty()) {
      return Future.succeededFuture(Map.of());
    }

    List<Future<Pair<String, String>>> deletableHoldingsFuture = new ArrayList<>();
    holder.getAllHoldingIdsByTenant().forEach((tenantId, holdingIds) -> {
      var tenantContext = holder.isCentralEnabled()
        ? createContextWithNewTenantId(requestContext, tenantId)
        : requestContext;

      holdingIds.forEach(holdingId -> deletableHoldingsFuture.add(
        inventoryItemManager.getItemsByHoldingId(holdingId, tenantContext)
          .map(items -> canDeleteHoldingForTitleRemoval(items, holder.getPoLineId())
            ? Pair.of(tenantId, holdingId)
            : null)
      ));
    });

    return collectResultsOnSuccess(deletableHoldingsFuture)
      .map(cf -> StreamEx.of(deletableHoldingsFuture)
        .map(Future::result)
        .nonNull()
        .distinct()
        .groupingBy(Pair::getKey, mapping(Pair::getValue, toList())));
  }

  private Future<Void> deleteItemsPiecesHoldings(TitleHolder holder, RequestContext centralContext) {
    return holder.isCentralEnabled()
      ? deleteItemsPiecesHoldingsForMultipleTenants(holder, centralContext)
      : deleteItemsPiecesHoldingsForTenant(holder, null, centralContext);
  }

  private Future<Void> deleteItemsPiecesHoldingsForMultipleTenants(TitleHolder holder, RequestContext centralContext) {
    return collectResultsOnSuccess(holder.getAllHoldingIdsByTenant()
      .keySet().stream()
      .map(tenantId -> deleteItemsPiecesHoldingsForTenant(holder, tenantId, centralContext))
      .toList())
      .mapEmpty();
  }

  private Future<Void> deleteItemsPiecesHoldingsForTenant(TitleHolder holder, String tenantId, RequestContext centralContext) {
    var tenantContext = Optional.ofNullable(tenantId)
      .map(tId -> createContextWithNewTenantId(centralContext, tId))
      .orElse(centralContext);
    return deleteItems(holder, tenantContext)
      .compose(v -> deletePieces(holder, tenantId, centralContext))
      .compose(v -> deleteHoldings(holder, tenantContext));
  }

  private Future<Void> deleteItems(TitleHolder holder, RequestContext tenantContext) {
    var holdingIds = holder.getAllHoldings();
    if (CollectionUtils.isEmpty(holdingIds)) {
      return Future.succeededFuture();
    }

    List<Future<List<Void>>> deleteItemFutures = holdingIds.stream()
      .map(holdingId -> inventoryItemManager.getItemsByHoldingId(holdingId, tenantContext)
        .compose(items -> {
          if (items.isEmpty()) {
            log.info("deleteItemsForHolding:: No items found for holding: {}", holdingId);
            return Future.succeededFuture();
          }

          var itemIds = items.stream()
            .filter(item -> canDeleteItemForTitleRemoval(item, holder.getPoLineId()))
            .map(item -> item.getString(ID))
            .toList();

          if (itemIds.isEmpty()) {
            log.info("deleteItemsForHolding:: No items to delete for holdingId: {} and poLine: {}", holdingId, holder.getPoLineId());
            return Future.succeededFuture();
          }

          return inventoryItemManager.deleteItems(itemIds, true, tenantContext);
        }))
      .toList();

    return collectResultsOnSuccess(deleteItemFutures)
      .onSuccess(v -> log.info("deleteItemsForHolding:: Items were deleted successfully for holdingIds: {}", holdingIds))
      .onFailure(t -> log.error("deleteItemsForHolding:: Failed to delete items for holdingIds: {}", holdingIds, t))
      .mapEmpty();
  }

  private Future<Void> deletePieces(TitleHolder holder, String tenantId, RequestContext centralContext) {
    var allHoldings = holder.getAllHoldings();
    if (CollectionUtils.isEmpty(allHoldings)) {
      return Future.succeededFuture();
    }

    var pieces = holder.getPieces();
    var pieceIdsToDelete = pieces.stream()
      .filter(piece -> canDeletePieceForTitleRemoval(piece, allHoldings, tenantId))
      .map(Piece::getId)
      .toList();

    if (pieceIdsToDelete.isEmpty()) {
      return Future.succeededFuture();
    }

    return pieceStorageService.deletePiecesByIds(pieceIdsToDelete, centralContext)
      .onSuccess(v -> log.info("deletePiecesForPoLine:: Pieces were deleted successfully for poLineId: {}", holder.getPoLineId()))
      .onFailure(t -> log.error("deletePiecesForPoLine:: Failed to delete pieces for poLineId: {}", holder.getPoLineId(), t));
  }

  private Future<Void> deleteHoldings(TitleHolder holder, RequestContext tenantContext) {
    var holdingIds = holder.getHoldingIdsToDelete();
    if (CollectionUtils.isEmpty(holdingIds) || !holder.isDeleteHoldings()) {
      return Future.succeededFuture();
    }

    var deleteHoldingFutures = holdingIds.stream()
      .map(holdingId -> inventoryHoldingManager.deleteHoldingById(holdingId, true, tenantContext))
      .toList();

    return collectResultsOnSuccess(deleteHoldingFutures)
      .onSuccess(v -> log.info("deleteHoldings:: Holdings '{}' were deleted successfully", holdingIds))
      .onFailure(t -> log.error("deleteHoldings:: Failed to delete holdings '{}'", holdingIds, t))
      .mapEmpty();
  }

  private Future<Void> deleteTitle(String id, RequestContext requestContext) {
    log.info("deleteTitle:: Title '{}' is being deleted", id);
    var deleteEndpoint = new RequestEntry(BY_ID_ENDPOINT).withId(id).buildEndpoint();
    return restClient.delete(deleteEndpoint, requestContext);
  }
}
