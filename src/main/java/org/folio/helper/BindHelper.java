package org.folio.helper;

import io.vertx.core.Context;
import io.vertx.core.Future;
import io.vertx.core.json.JsonObject;
import org.apache.commons.lang3.StringUtils;
import org.folio.models.ItemFields;
import org.folio.models.pieces.BindPiecesHolder;
import org.folio.models.pieces.PiecesHolder;
import org.folio.okapi.common.GenericCompositeFuture;
import org.folio.orders.utils.PoLineCommonUtil;
import org.folio.rest.RestConstants;
import org.folio.rest.core.exceptions.ErrorCodes;
import org.folio.rest.core.exceptions.HttpException;
import org.folio.rest.core.models.RequestContext;
import org.folio.rest.jaxrs.model.BindItem;
import org.folio.rest.jaxrs.model.BindPiecesCollection;
import org.folio.rest.jaxrs.model.BindPiecesResult;
import org.folio.rest.jaxrs.model.CompositePoLine;
import org.folio.rest.jaxrs.model.Piece;
import org.folio.rest.jaxrs.model.ReceivedItem;
import org.folio.rest.jaxrs.model.Title;
import org.folio.rest.tools.utils.TenantTool;
import org.folio.service.inventory.InventoryInstanceManager;
import org.folio.service.inventory.InventoryItemRequestService;
import org.springframework.beans.factory.annotation.Autowired;

import java.util.ArrayList;
import java.util.Collections;
import java.util.Date;
import java.util.List;
import java.util.Map;
import java.util.Objects;
import java.util.Optional;
import java.util.Set;
import java.util.stream.Collectors;

import static org.folio.orders.utils.RequestContextUtil.createContextWithNewTenantId;
import static org.folio.rest.jaxrs.model.BindPiecesCollection.RequestsAction.TRANSFER;


public class BindHelper extends CheckinReceivePiecesHelper<BindPiecesCollection> {

  private static final String TITLE_BY_POLINE_QUERY = "poLineId==%s";

  @Autowired
  private InventoryItemRequestService inventoryItemRequestService;

  @Autowired
  private InventoryInstanceManager inventoryInstanceManager;

  public BindHelper(BindPiecesCollection bindPiecesCollection,
                    Map<String, String> okapiHeaders, Context ctx) {
    super(okapiHeaders, ctx);
    piecesByLineId = groupBindPieceByPoLineId(bindPiecesCollection);
    logger.debug("{} piece records(s) are going to be bound for '{}' PO line",
        bindPiecesCollection.getPoLineId(), bindPiecesCollection.getBindPieceIds().size());
  }

  public BindHelper(Map<String, String> okapiHeaders, Context ctx) {
    super(okapiHeaders, ctx);
  }

  private Map<String, Map<String, BindPiecesCollection>> groupBindPieceByPoLineId(BindPiecesCollection bindPiecesCollection) {
    String poLineId = bindPiecesCollection.getPoLineId();
    Map<String, BindPiecesCollection> bindPieceMap = bindPiecesCollection.getBindPieceIds().stream()
      .collect(Collectors.toMap(
        bindPieceId -> bindPieceId,
        bindPieceId -> bindPiecesCollection
      ));

    return Map.of(poLineId, bindPieceMap);
  }

  public Future<Void> removeBinding(String pieceId, RequestContext requestContext) {
    logger.debug("removeBinding:: Removing binding for piece: {}", pieceId);
    return pieceStorageService.getPieceById(pieceId, requestContext)
      .compose(piece -> {
        var bindItemId = piece.getBindItemId();
        piece.withBindItemId(null).withBindItemTenantId(null).withIsBound(false);
        return removeForbiddenEntities(piece, requestContext)
          .map(v -> Collections.<String, List<Piece>>singletonMap(null, List.of(piece)))
          .compose(piecesGroupedByPoLine -> storeUpdatedPieceRecords(piecesGroupedByPoLine, requestContext))
          .compose(piecesGroupedByPoLine -> clearTitleBindItemsIfNeeded(piece.getTitleId(), bindItemId, requestContext));
      });
  }

  private Future<Void> removeForbiddenEntities(Piece piece, RequestContext requestContext) {
    // Populate piecesByLineId used by removeForbiddenEntities and parent helper methods
    piecesByLineId = Map.of(piece.getPoLineId(), Collections.singletonMap(piece.getId(), null));
    return removeForbiddenEntities(requestContext);
  }

  private Future<Void> clearTitleBindItemsIfNeeded(String titleId, String bindItemId, RequestContext requestContext) {
    String query = String.format("titleId==%s and bindItemId==%s and isBound==true", titleId, bindItemId);
    return pieceStorageService.getPieces(0, 0, query, requestContext)
      .compose(pieceCollection -> {
        var totalRecords = pieceCollection.getTotalRecords();
        if (totalRecords != 0) {
          logger.info("clearTitleBindItemsIfNeeded:: Found '{}' piece(s) associated with bind item '{}'", totalRecords, bindItemId);
          return Future.succeededFuture();
        }
        logger.info("clearTitleBindItemsIfNeeded:: Removing bind item '{}' from title '{}' as no associated piece(s) to the item was found", bindItemId, titleId);
        return titlesService.getTitleById(titleId, requestContext)
          .compose(title -> {
            List<String> bindItemIds = new ArrayList<>(title.getBindItemIds());
            bindItemIds.remove(bindItemId);
            return titlesService.saveTitle(title.withBindItemIds(bindItemIds), requestContext);
          });
      });
  }

  public Future<BindPiecesResult> bindPieces(BindPiecesCollection bindPiecesCollection, RequestContext requestContext) {
    return removeForbiddenEntities(requestContext)
      .compose(vVoid -> processBindPieces(bindPiecesCollection, requestContext));
  }

  private Future<BindPiecesResult> processBindPieces(BindPiecesCollection bindPiecesCollection, RequestContext requestContext) {
    //   1. Get valid piece records from storage
    return getValidPieces(requestContext)
      // 2. Generate holder object to include necessary data
      .map(piecesGroupedByPoLine -> generateHolder(piecesGroupedByPoLine, bindPiecesCollection))
      // 3. Check if there are any open requests for items
      .compose(bindPiecesHolder -> checkRequestsForPieceItems(bindPiecesHolder, requestContext))
      // 4. Update piece isBound flag
      .map(this::updatePieceRecords)
      // 5. Update currently associated items
      .compose(bindPiecesHolder -> updateItemStatus(bindPiecesHolder, requestContext))
      // 6. Crate item for pieces with specific fields
      .compose(bindPiecesHolder -> createItemForPieces(bindPiecesHolder, requestContext))
      // 7. Update received piece records in the storage
      .compose(bindPiecesHolder -> storeUpdatedPieces(bindPiecesHolder, requestContext))
      // 8. Update Title with new bind items
      .compose(bindPiecesHolder -> updateTitleWithBindItems(bindPiecesHolder, requestContext))
      // 9. Return results to the client
      .map(this::prepareResponseBody);
  }

  private BindPiecesHolder generateHolder(Map<String, List<Piece>> piecesGroupedByPoLine, BindPiecesCollection bindPiecesCollection) {
    return new BindPiecesHolder()
      .withBindPiecesCollection(bindPiecesCollection)
      .withPiecesGroupedByPoLine(piecesGroupedByPoLine);
  }

  private Future<Map<String, List<Piece>>> getValidPieces(RequestContext requestContext) {
    return retrievePieceRecords(requestContext)
      .map(piecesGroupedByPoLine -> {
        var areAllPiecesReceived = extractAllPieces(piecesGroupedByPoLine)
          .allMatch(piece -> RECEIVED_STATUSES.contains(piece.getReceivingStatus()));
        if (areAllPiecesReceived) {
          return piecesGroupedByPoLine;
        }
        throw new HttpException(RestConstants.VALIDATION_ERROR, ErrorCodes.PIECES_MUST_HAVE_RECEIVED_STATUS);
      });
  }

  private Future<BindPiecesHolder> checkRequestsForPieceItems(BindPiecesHolder holder, RequestContext requestContext) {
    var tenantToItem = mapTenantIdsToItemIds(holder.getPiecesGroupedByPoLine(), requestContext);
    return GenericCompositeFuture.all(
      tenantToItem.entrySet().stream()
        .map(entry -> {
        var locationContext = createContextWithNewTenantId(requestContext, entry.getKey());
        return inventoryItemRequestService.getItemIdsWithActiveRequests(entry.getValue(), locationContext)
          .compose(items -> validateItemsForRequestTransfer(tenantToItem.keySet(), items, holder.getBindPiecesCollection()));
        })
        .toList())
      .map(f -> holder);
  }

  private Future<Void> validateItemsForRequestTransfer(Set<String> tenants, List<String> items,
                                                       BindPiecesCollection bindPiecesCollection) {
    if (items.isEmpty()) {
      return Future.succeededFuture();
    }

    // requestsAction is required to handle open requests
    if (Objects.isNull(bindPiecesCollection.getRequestsAction())) {
      logger.warn("validateItemsForRequestTransfer:: Found open requests on items with ids: {}", items);
      throw new HttpException(RestConstants.VALIDATION_ERROR, ErrorCodes.REQUESTS_ACTION_REQUIRED);
    }

    var bindItemTenantId = bindPiecesCollection.getBindItem().getTenantId();
    var areItemsInSameTenant = Optional.ofNullable(bindItemTenantId)
      .map(tenants::contains).orElse(true) && tenants.size() == 1;
    // Transferring requests between tenants is not a requirement for R
    // All items should be in same tenant as the bind item tenantId for requests to be transferred
    if (TRANSFER.equals(bindPiecesCollection.getRequestsAction()) && !areItemsInSameTenant) {
      logger.warn("validateItemsForRequestTransfer:: All piece items and bindItem must be in same tenant. Pieces: {}, BindItem: {}", tenants, bindItemTenantId);
      throw new HttpException(RestConstants.VALIDATION_ERROR, ErrorCodes.PIECES_HAVE_DIFFERENT_RECEIVING_TENANT_IDS);
    }
    return Future.succeededFuture();
  }

  private BindPiecesHolder updatePieceRecords(BindPiecesHolder holder) {
    logger.debug("updatePieceRecords:: Updating the piece records to set isBound flag as TRUE");
    holder.getPieces().forEach(piece -> piece.setIsBound(true));
    return holder;
  }

  private Future<BindPiecesHolder> updateItemStatus(BindPiecesHolder holder, RequestContext requestContext) {
    logger.debug("updateItemStatus:: Updating previous item status to 'Unavailable'");
    return GenericCompositeFuture.all(
      mapTenantIdsToItemIds(holder.getPiecesGroupedByPoLine(), requestContext).entrySet().stream()
        .map(entry -> {
          var locationContext = createContextWithNewTenantId(requestContext, entry.getKey());
          return inventoryItemManager.getItemRecordsByIds(entry.getValue(), locationContext)
            .compose(items -> {
              items.forEach(item -> {
                logger.info("updateItemStatus:: '{}' item status set to 'Unavailable'", item.getString(ItemFields.ID.value()));
                item.put(ItemFields.STATUS.value(), new JsonObject()
                  .put(ItemFields.STATUS_DATE.value(), new Date())
                  .put(ItemFields.STATUS_NAME.value(), ReceivedItem.ItemStatus.UNAVAILABLE));
              });
              return inventoryItemManager.updateItemRecords(items, locationContext);
            });
        })
        .toList()
    ).map(f -> holder);
  }

  private Future<BindPiecesHolder> createItemForPieces(BindPiecesHolder holder, RequestContext requestContext) {
    var bindPiecesCollection = holder.getBindPiecesCollection();
    var poLineId = holder.getPoLineId();
    var bindItem = bindPiecesCollection.getBindItem();
    logger.debug("createItemForPiece:: Trying to get poLine by id '{}'", poLineId);
    return purchaseOrderLineService.getOrderLineById(poLineId, requestContext)
      .map(PoLineCommonUtil::convertToCompositePoLine)
      .compose(compPOL -> createInventoryObjects(compPOL, bindPiecesCollection.getInstanceId(), bindItem, requestContext))
      .map(newItemId -> {
        // Move requests if requestsAction is TRANSFER, otherwise do nothing
        if (TRANSFER.equals(bindPiecesCollection.getRequestsAction())) {
          var itemIds = holder.getPieces().map(Piece::getItemId).toList();
          inventoryItemRequestService.transferItemRequests(itemIds, newItemId, requestContext);
        }
        // Set new item ids for pieces and holder
        holder.getPieces().forEach(piece -> piece.withBindItemTenantId(bindItem.getTenantId()).setBindItemId(newItemId));
        return holder.withBindItemId(newItemId);
      });
  }

  private Future<String> createInventoryObjects(CompositePoLine compPOL, String instanceId, BindItem bindItem, RequestContext requestContext) {
    if (!Boolean.TRUE.equals(compPOL.getIsPackage())) {
      instanceId = compPOL.getInstanceId();
    }
    var locationContext = createContextWithNewTenantId(requestContext, bindItem.getTenantId());
    return handleInstance(instanceId, bindItem.getTenantId(), locationContext, requestContext)
      .compose(instId -> handleHolding(bindItem, instId, locationContext))
      .compose(holdingId -> inventoryItemManager.createBindItem(compPOL, holdingId, bindItem, locationContext));
  }

  private Future<String> handleInstance(String instanceId, String targetTenantId,
                                      RequestContext locationContext, RequestContext requestContext) {
    if (StringUtils.isEmpty(targetTenantId) || targetTenantId.equals(TenantTool.tenantId(requestContext.getHeaders()))) {
      return Future.succeededFuture(instanceId);
    }
    return inventoryInstanceManager.createShadowInstanceIfNeeded(instanceId, locationContext)
      .map(s -> instanceId);
  }

  private Future<String> handleHolding(BindItem bindItem, String instanceId, RequestContext locationContext) {
    if (bindItem.getHoldingId() != null) {
      return Future.succeededFuture(bindItem.getHoldingId());
    }
    return inventoryHoldingManager.createHoldingAndReturnId(instanceId, bindItem.getLocationId(), locationContext);
  }

  private Future<BindPiecesHolder> storeUpdatedPieces(BindPiecesHolder holder, RequestContext requestContext) {
    return storeUpdatedPieceRecords(holder.getPiecesGroupedByPoLine(), requestContext)
      .map(m -> holder);
  }

  private Future<BindPiecesHolder> updateTitleWithBindItems(BindPiecesHolder holder, RequestContext requestContext) {
    var itemIds = holder.getPieces().map(Piece::getBindItemId).distinct().toList();
    return titlesService.getTitlesByQuery(String.format(TITLE_BY_POLINE_QUERY, holder.getPoLineId()), requestContext)
      .map(titles -> updateTitle(titles, itemIds, requestContext))
      .map(v -> holder);
  }

  private Future<Void> updateTitle(List<Title> titles, List<String> itemIds, RequestContext requestContext) {
    if (titles.isEmpty() || titles.get(0) == null) {
      return Future.succeededFuture();
    }
    var title = titles.get(0);
    List<String> existingBindItemIds = title.getBindItemIds() != null ? title.getBindItemIds() : new ArrayList<>();
    existingBindItemIds.addAll(itemIds);
    title = title.withBindItemIds(existingBindItemIds);
    return titlesService.saveTitle(title, requestContext);
  }

  private BindPiecesResult prepareResponseBody(BindPiecesHolder holder) {
    return new BindPiecesResult()
      .withPoLineId(holder.getPoLineId())
      .withBoundPieceIds(holder.getPieces().map(Piece::getId).toList())
      .withItemId(holder.getBindItemId());
  }

  @Override
  protected boolean isRevertToOnOrder(Piece piece) {
    // Set to true for piece validation while fetching
    return true;
  }

  @Override
  protected Future<Boolean> receiveInventoryItemAndUpdatePiece(PiecesHolder holder, JsonObject item, Piece piece, RequestContext locationContext) {
    return null;
  }

  @Override
  protected Map<String, List<Piece>> updatePieceRecordsWithoutItems(Map<String, List<Piece>> piecesGroupedByPoLine) {
    return Map.of();
  }

  @Override
  protected String getHoldingId(Piece piece) {
    return StringUtils.EMPTY;
  }

  @Override
  protected String getLocationId(Piece piece) {
    return StringUtils.EMPTY;
  }

  @Override
  protected String getReceivingTenantId(Piece piece) {
    return piece.getReceivingTenantId();
  }

}
