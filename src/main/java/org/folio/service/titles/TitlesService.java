package org.folio.service.titles;

import static java.util.stream.Collectors.groupingBy;
import static java.util.stream.Collectors.mapping;
import static java.util.stream.Collectors.toList;
import static org.folio.orders.utils.AcqDesiredPermissions.TITLES_ASSIGN;
import static org.folio.orders.utils.AcqDesiredPermissions.TITLES_MANAGE;
import static org.folio.orders.utils.HelperUtils.collectResultsOnSuccess;
import static org.folio.orders.utils.HelperUtils.combineResultListsOnSuccess;
import static org.folio.orders.utils.ProtectedOperationType.DELETE;
import static org.folio.orders.utils.RequestContextUtil.createContextWithNewTenantId;
import static org.folio.orders.utils.ResourcePathResolver.TITLES;
import static org.folio.orders.utils.ResourcePathResolver.resourcesPath;
import static org.folio.rest.RestConstants.BAD_REQUEST;
import static org.folio.rest.RestConstants.MAX_IDS_FOR_GET_RQ_15;
import static org.folio.rest.core.exceptions.ErrorCodes.EXISTING_HOLDINGS_FOR_DELETE_CONFIRMATION;

import java.util.AbstractMap;
import java.util.ArrayList;
import java.util.List;
import java.util.Map;
import java.util.Objects;
import java.util.function.Function;
import java.util.stream.Collectors;

import com.google.common.collect.Sets;

import io.vertx.core.Future;
import lombok.extern.log4j.Log4j2;
import one.util.streamex.StreamEx;
import org.apache.commons.collections4.CollectionUtils;
import org.apache.commons.lang3.StringUtils;
import org.folio.models.TitleHolder;
import org.folio.orders.utils.ProtectedOperationType;
import org.folio.orders.utils.QueryUtils;
import org.folio.rest.core.RestClient;
import org.folio.rest.core.exceptions.HttpException;
import org.folio.rest.core.models.RequestContext;
import org.folio.rest.core.models.RequestEntry;
import org.folio.rest.jaxrs.model.CompositePoLine;
import org.folio.rest.jaxrs.model.CompositePurchaseOrder;
import org.folio.rest.jaxrs.model.Parameter;
import org.folio.rest.jaxrs.model.Piece;
import org.folio.rest.jaxrs.model.PoLine;
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

  public Future<Map<String, List<Title>>> fetchNonPackageTitles(CompositePurchaseOrder compPO,
                                                                RequestContext requestContext) {
    List<String> lineIds = getNonPackageLineIds(compPO.getCompositePoLines());
    return getTitlesByPoLineIds(lineIds, requestContext);
  }

  private List<String> getNonPackageLineIds(List<CompositePoLine> compositePoLines) {
    return compositePoLines.stream().filter(line -> !line.getIsPackage()).map(CompositePoLine::getId).collect(toList());
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
   * Unlinks a title from a package. This involves checking the existence of holdings and items for the title.
   * If the holdings have references to other titles, the unlinking process skips them.
   * If the holdings do not have references, it asks for confirmation to delete related pieces, items, and holdings.
   *
   * @param titleId        the ID of the title to unlink
   * @param deleteHoldings  flag indicating whether to delete the holding if it has no other references
   * @param requestContext the request context
   * @return a Future representing the result of the unlinking operation
   */
  public Future<List<String>> unlinkTitleFromPackage(String titleId, String deleteHoldings,
                                                     RequestContext requestContext) {
    log.debug("Trying to unlink title with id: {} and deleteHoldings: {}", titleId, deleteHoldings);

    return getTitleById(titleId, requestContext)
      .map(TitleHolder::new)
      .compose(holder -> purchaseOrderLineService.getOrderLineById(holder.getTitle().getPoLineId(), requestContext)
        .map(holder::withPoLine))
      .compose(holder -> consortiumConfigurationService.isCentralOrderingEnabled(requestContext)
        .map(holder::withCentralEnabled))
      .compose(holder -> getHoldingsToDeleteWithTenants(holder, requestContext)
        .map(holder::withHoldingIdsToDeleteByTenant))
      .compose(holder -> processHoldingItemPieces(holder, deleteHoldings, requestContext)
        .map(v -> holder))
      .compose(holder -> unlinkTitle(holder.getTitle(), requestContext));
  }

  private Future<Void> processHoldingItemPieces(TitleHolder holder, String deleteHoldings, RequestContext requestContext) {
    List<String> holdingIdsToDelete = holder.getHoldingIdsToDelete();

    if (CollectionUtils.isEmpty(holdingIdsToDelete)) {
      return Future.succeededFuture();
    }

    if (StringUtils.isEmpty(deleteHoldings)) {
      log.info("processHoldings:: Holdings in poLine '{}' will not be deleted", holder.getPoLine().getId());
      var param = new Parameter().withKey("holdingIds").withValue(holdingIdsToDelete.toString());
      var error = EXISTING_HOLDINGS_FOR_DELETE_CONFIRMATION.toError().withParameters(List.of(param));
      throw new HttpException(BAD_REQUEST, error);
    }

    if (!"true".equalsIgnoreCase(deleteHoldings) && !"false".equalsIgnoreCase(deleteHoldings)) {
      throw new IllegalArgumentException("deleteHolding must be either 'true' or 'false'");
    }

    return Boolean.parseBoolean(deleteHoldings)
      ? deleteHoldingItemPieces(holder, requestContext)
      : Future.succeededFuture();
  }

  private Future<Map<String, List<String>>> getHoldingsToDeleteWithTenants(TitleHolder holder, RequestContext requestContext) {
    return getHoldingIdsGroupedByTenantId(holder.getTitle(), holder.getPoLine(), requestContext)
      .compose(holdingIdsGroupedByTenant -> extractDeletableHoldings(holder, holdingIdsGroupedByTenant, requestContext));
  }

  private Future<Map<String, List<String>>> getHoldingIdsGroupedByTenantId(Title title, PoLine poLine, RequestContext requestContext) {
    return pieceStorageService.getPiecesByLineIdAndTitleId(poLine.getId(), title.getId(), requestContext)
      .map(pieces -> pieces.stream()
        .filter(piece -> StringUtils.isNotEmpty(piece.getHoldingId()))
        .collect(groupingBy(Piece::getReceivingTenantId, mapping(Piece::getHoldingId, toList())))
      );
  }

  private Future<Map<String, List<String>>> extractDeletableHoldings(TitleHolder holder, Map<String, List<String>> holdingIdsGroupByTenant,
                                                                     RequestContext requestContext) {
    if (holdingIdsGroupByTenant.isEmpty()) {
      return Future.succeededFuture(Map.of());
    }

    List<Future<AbstractMap.SimpleEntry<String, String>>> deletableHoldingsFuture = new ArrayList<>();
    holdingIdsGroupByTenant.forEach((tenantId, holdingIds) -> {
      var tenantContext = holder.isCentralEnabled()
        ? createContextWithNewTenantId(requestContext, tenantId)
        : requestContext;

      holdingIds.forEach(holdingId -> deletableHoldingsFuture.add(
        inventoryItemManager.getItemsByHoldingId(holdingId, tenantContext)
          .map(items -> items.stream()
            .anyMatch(item -> holder.getPoLineId().equals(item.getString("purchaseOrderLineIdentifier")))
            ? new AbstractMap.SimpleEntry<>(tenantId, holdingId)
            : null
          )
      ));
    });

    return collectResultsOnSuccess(deletableHoldingsFuture)
      .map(cf -> deletableHoldingsFuture.stream()
        .map(Future::result)
        .filter(Objects::nonNull)
        .distinct()
        .collect(Collectors.groupingBy(
          AbstractMap.SimpleEntry::getKey,
          Collectors.mapping(AbstractMap.SimpleEntry::getValue, Collectors.toList())
        ))
      );
  }

  private Future<Void> deleteHoldingItemPieces(TitleHolder holder, RequestContext centralContext) {
    return holder.isCentralEnabled()
      ? deleteWithMultipleTenants(holder, centralContext)
      : deleteWithSingleTenant(holder, centralContext);
  }

  private Future<Void> deleteWithMultipleTenants(TitleHolder holder, RequestContext centralContext) {
    var tenantOperationFutures = new ArrayList<Future<Void>>();
    holder.getHoldingIdsToDeleteByTenant().forEach((tenantId, holdingIds) -> {
      var tenantContext = createContextWithNewTenantId(centralContext, tenantId);

      tenantOperationFutures.add(
        deleteItemsForHolding(holdingIds, tenantContext)
          .compose(v -> deletePiecesForHoldingsInTenant(holder, holdingIds, tenantId, centralContext))
          .compose(v -> deleteHoldings(holdingIds, tenantContext))
      );
    });

    return collectResultsOnSuccess(tenantOperationFutures).mapEmpty();
  }

  private Future<Void> deleteWithSingleTenant(TitleHolder titleHolder, RequestContext requestContext) {
    return deleteItemsForHolding(titleHolder.getHoldingIdsToDelete(), requestContext)
      .compose(v -> deletePiecesForHoldingsInTenant(titleHolder, titleHolder.getHoldingIdsToDelete(), null, requestContext))
      .compose(v -> deleteHoldings(titleHolder.getHoldingIdsToDelete(), requestContext));
  }

  private Future<Void> deleteItemsForHolding(List<String> holdingIds, RequestContext tenantContext) {
    if (CollectionUtils.isEmpty(holdingIds)) {
      return Future.succeededFuture();
    }

    List<Future<List<Void>>> deleteItemFutures = holdingIds.stream()
      .map(holdingId -> inventoryItemManager.getItemsByHoldingId(holdingId, tenantContext)
        .compose(items -> {
          if (items.isEmpty()) return Future.succeededFuture();

          var itemIds = items.stream()
            .map(item -> item.getString("id"))
            .toList();

          return inventoryItemManager.deleteItems(itemIds, true, tenantContext);
        }))
      .collect(Collectors.toList());

    return combineResultListsOnSuccess(deleteItemFutures)
      .onSuccess(v -> log.info("deleteItemsForHolding:: Items were deleted successfully for holdingIds: {}", holdingIds))
      .onFailure(t -> log.error("deleteItemsForHolding:: Failed to delete items for holdingIds: {}", holdingIds, t))
      .mapEmpty();
  }

  private Future<Void> deletePiecesForHoldingsInTenant(TitleHolder holder, List<String> holdings, String tenantId,
                                                       RequestContext centralContext) {
    if (CollectionUtils.isEmpty(holdings)) {
      return Future.succeededFuture();
    }

    return pieceStorageService.getPiecesByLineIdAndTitleId(holder.getPoLineId(), holder.getTitleId(), centralContext)
      .compose(pieces -> {
        var pieceIdsToDelete = pieces.stream()
          .filter(piece ->
            holdings.contains(piece.getHoldingId())
              && (tenantId == null || tenantId.equals(piece.getReceivingTenantId())))
          .map(Piece::getId).toList();

        if (pieceIdsToDelete.isEmpty()) {
          return Future.succeededFuture();
        }

        return pieceStorageService.deletePiecesByIds(pieceIdsToDelete, centralContext)
          .onSuccess(v -> log.info("deletePiecesForPoLine:: Pieces were deleted successfully for poLineId: {}", holder.getPoLineId()))
          .onFailure(t -> log.error("deletePiecesForPoLine:: Failed to delete pieces for poLineId: {}", holder.getPoLineId(), t));
      });
  }

  private Future<Void> deleteHoldings(List<String> holdingIds, RequestContext tenantContext) {
    if (CollectionUtils.isEmpty(holdingIds)) {
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

  private Future<List<String>> unlinkTitle(Title title, RequestContext requestContext) {
    log.info("unlinkTitleFromPackage:: Title '{}' is being unlinked from the package", title.getId());
    return deleteTitle(title.getId(), requestContext)
      .mapEmpty();
  }
}
