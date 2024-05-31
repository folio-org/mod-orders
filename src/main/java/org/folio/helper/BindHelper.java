package org.folio.helper;

import io.vertx.core.Context;
import io.vertx.core.Future;
import io.vertx.core.json.JsonObject;
import org.apache.commons.lang3.StringUtils;
import org.folio.models.ItemFields;
import org.folio.okapi.common.GenericCompositeFuture;
import org.folio.orders.utils.PoLineCommonUtil;
import org.folio.rest.RestConstants;
import org.folio.rest.core.exceptions.ErrorCodes;
import org.folio.rest.core.exceptions.HttpException;
import org.folio.rest.core.models.RequestContext;
import org.folio.rest.jaxrs.model.BindItem;
import org.folio.rest.jaxrs.model.BindPiecesCollection;
import org.folio.rest.jaxrs.model.CompositePoLine;
import org.folio.rest.jaxrs.model.Error;
import org.folio.rest.jaxrs.model.Parameter;
import org.folio.rest.jaxrs.model.Piece;
import org.folio.rest.jaxrs.model.ProcessingStatus;
import org.folio.rest.jaxrs.model.ReceivedItem;
import org.folio.rest.jaxrs.model.ReceivingResult;
import org.folio.rest.jaxrs.model.ReceivingResults;
import org.folio.rest.jaxrs.model.Title;
import org.folio.rest.tools.utils.TenantTool;
import org.folio.service.inventory.InventoryInstanceManager;
import org.folio.service.inventory.InventoryItemRequestService;
import org.springframework.beans.factory.annotation.Autowired;

import java.util.ArrayList;
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

  private Map<String, Map<String, BindPiecesCollection>> groupBindPieceByPoLineId(BindPiecesCollection bindPiecesCollection) {
    String poLineId = bindPiecesCollection.getPoLineId();
    Map<String, BindPiecesCollection> bindPieceMap = bindPiecesCollection.getBindPieceIds().stream()
      .collect(Collectors.toMap(
        bindPieceId -> bindPieceId,
        bindPieceId -> bindPiecesCollection
      ));

    return Map.of(poLineId, bindPieceMap);
  }

  public Future<ReceivingResults> bindPieces(BindPiecesCollection bindPiecesCollection, RequestContext requestContext) {
    return removeForbiddenEntities(requestContext)
      .compose(vVoid -> processBindPieces(bindPiecesCollection, requestContext));
  }

  private Future<ReceivingResults> processBindPieces(BindPiecesCollection bindPiecesCollection, RequestContext requestContext) {
    //   1. Get piece records from storage
    return retrievePieceRecords(requestContext)
      // 2. Check if there are any open requests for items
      .compose(piecesGroupedByPoLine -> checkRequestsForPieceItems(piecesGroupedByPoLine, bindPiecesCollection, requestContext))
      // 3. Update piece isBound flag
      .map(this::updatePieceRecords)
      // 4. Update currently associated items
      .compose(piecesGroupedByPoLine -> updateItemStatus(piecesGroupedByPoLine, requestContext))
      // 5. Crate item for pieces with specific fields
      .compose(piecesGroupedByPoLine -> createItemForPiece(piecesGroupedByPoLine, bindPiecesCollection, requestContext))
      // 6. Update received piece records in the storage
      .compose(piecesGroupedByPoLine -> storeUpdatedPieceRecords(piecesGroupedByPoLine, requestContext))
      // 7. Update Title with new bind items
      .map(piecesGroupedByPoLine -> updateTitleWithBindItems(piecesGroupedByPoLine, requestContext))
      // 8. Return results to the client
      .map(piecesGroupedByPoLine -> prepareResponseBody(piecesGroupedByPoLine, bindPiecesCollection));
  }

  private Future<Map<String, List<Piece>>> checkRequestsForPieceItems(Map<String, List<Piece>> piecesGroupedByPoLine,
                                                                      BindPiecesCollection bindPiecesCollection,
                                                                      RequestContext requestContext) {
    var tenantToItem = mapTenantIdsToItemIds(piecesGroupedByPoLine, requestContext);
    return GenericCompositeFuture.all(
      tenantToItem.entrySet().stream()
        .map(entry -> {
        var locationContext = createContextWithNewTenantId(requestContext, entry.getKey());
        return inventoryItemRequestService.getItemsWithActiveRequests(entry.getValue(), locationContext)
          .compose(items -> validateItemsForRequestTransfer(tenantToItem.keySet(), items, bindPiecesCollection));
        })
        .toList())
      .map(f -> piecesGroupedByPoLine);
  }

  private Future<Void> validateItemsForRequestTransfer(Set<String> tenants,
                                                       List<String> items,
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

  private Map<String, List<Piece>> updatePieceRecords(Map<String, List<Piece>> piecesGroupedByPoLine) {
    logger.debug("updatePieceRecords:: Updating the piece records to set isBound flag as TRUE");
    extractAllPieces(piecesGroupedByPoLine)
      .forEach(piece -> piece.setIsBound(true));
    return piecesGroupedByPoLine;
  }

  private Future<Map<String, List<Piece>>> updateItemStatus(Map<String, List<Piece>> piecesGroupedByPoLine, RequestContext requestContext) {
    logger.debug("updateItemStatus:: Updating previous item status to 'Unavailable'");
    return GenericCompositeFuture.all(
      mapTenantIdsToItemIds(piecesGroupedByPoLine, requestContext).entrySet().stream()
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
    ).map(f -> piecesGroupedByPoLine);
  }

  private Future<Map<String, List<Piece>>> createItemForPiece(Map<String, List<Piece>> piecesGroupedByPoLine,
                                                      BindPiecesCollection bindPiecesCollection,
                                                      RequestContext requestContext) {
    var poLineId = bindPiecesCollection.getPoLineId();
    var holdingIds = piecesGroupedByPoLine.values()
      .stream().flatMap(List::stream)
      .map(Piece::getHoldingId).distinct().toList();
    validateHoldingIds(holdingIds, bindPiecesCollection);
    logger.debug("createItemForPiece:: Trying to get poLine by id '{}'", poLineId);

    return purchaseOrderLineService.getOrderLineById(poLineId, requestContext)
      .map(PoLineCommonUtil::convertToCompositePoLine)
      .compose(compPOL -> createInventoryObjects(compPOL, holdingIds.get(0), bindPiecesCollection.getBindItem(), requestContext))
      .map(newItemId -> {
        // Move requests if requestsAction is TRANSFER, otherwise do nothing
        if (TRANSFER.equals(bindPiecesCollection.getRequestsAction())) {
          var itemIds = extractAllPieces(piecesGroupedByPoLine).map(Piece::getItemId).toList();
          inventoryItemRequestService.transferItemsRequests(itemIds, newItemId, requestContext);
        }
        // Set new item ids for pieces
        piecesGroupedByPoLine.get(poLineId).forEach(piece -> piece.setItemId(newItemId));
        return piecesGroupedByPoLine;
      });
  }

  private void validateHoldingIds(List<String> holdingIds, BindPiecesCollection bindPiecesCollection) {
    if (holdingIds.size() != 1) {
      var holdingParam = new Parameter().withKey("holdingIds").withValue(holdingIds.toString());
      var pieceParam = new Parameter().withKey("pieceIds").withValue(bindPiecesCollection.getBindPieceIds().toString());
      var error = new Error().withParameters(List.of(holdingParam, pieceParam))
        .withMessage("Holding Id must not be null or different for pieces");
      throw new HttpException(400, error);
    }
  }

  private Future<String> createInventoryObjects(CompositePoLine compPOL, String holdingId, BindItem bindItem, RequestContext requestContext) {
    var locationContext = createContextWithNewTenantId(requestContext, bindItem.getTenantId());
    return createShadowInstanceAndHoldingIfNeeded(compPOL, bindItem, holdingId, locationContext, requestContext)
      .compose(targetHoldingId -> inventoryItemManager.createBindItem(compPOL, targetHoldingId, bindItem, locationContext));
  }

  private Future<String> createShadowInstanceAndHoldingIfNeeded(CompositePoLine compPOL, BindItem bindItem, String holdingId,
                                                                RequestContext locationContext, RequestContext requestContext) {
    // No need to create inventory objects if BindItem tenantId is not different
    var targetTenantId = bindItem.getTenantId();
    if (StringUtils.isEmpty(targetTenantId) || targetTenantId.equals(TenantTool.tenantId(requestContext.getHeaders()))) {
      return Future.succeededFuture(holdingId);
    }
    var instanceId = compPOL.getInstanceId();
    return inventoryInstanceManager.createShadowInstanceIfNeeded(instanceId, locationContext)
      .compose(s -> inventoryHoldingManager.createHoldingAndReturnId(instanceId, bindItem.getPermanentLocationId(), locationContext));
  }

  private Map<String, List<Piece>> updateTitleWithBindItems(Map<String, List<Piece>> piecesByPoLineIds,
                                                            RequestContext requestContext) {
    piecesByPoLineIds.forEach((poLineId, pieces) -> {
      List<String> itemIds = pieces.stream().map(Piece::getItemId).distinct().toList();
      titlesService.getTitlesByQuery(String.format(TITLE_BY_POLINE_QUERY, poLineId), requestContext)
        .map(titles -> updateTitle(titles, itemIds, requestContext));
    });
    return piecesByPoLineIds;
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

  private ReceivingResults prepareResponseBody(Map<String, List<Piece>> piecesGroupedByPoLine,
                                               BindPiecesCollection bindPiecesCollection) {
    String poLineId = bindPiecesCollection.getPoLineId();

    // Get all processed piece records for PO Line
    Map<String, Piece> processedPiecesForPoLine = getProcessedPiecesForPoLine(poLineId, piecesGroupedByPoLine);

    var resultCounts = getEmptyResultCounts();
    ReceivingResult result = new ReceivingResult();
    for (String pieceId : bindPiecesCollection.getBindPieceIds()) {
      calculateProcessingErrors(poLineId, result, processedPiecesForPoLine, resultCounts, pieceId);
    }

    result.withPoLineId(poLineId)
      .withProcessedSuccessfully(resultCounts.get(ProcessingStatus.Type.SUCCESS))
      .withProcessedWithError(resultCounts.get(ProcessingStatus.Type.FAILURE));
    return new ReceivingResults()
      .withTotalRecords(1)
      .withReceivingResults(List.of(result));
  }

  @Override
  protected boolean isRevertToOnOrder(Piece piece) {
    return false;
  }

  @Override
  protected Future<Boolean> receiveInventoryItemAndUpdatePiece(JsonObject item, Piece piece, RequestContext locationContext) {
    return null;
  }

  @Override
  protected Map<String, List<Piece>> updatePieceRecordsWithoutItems(Map<String, List<Piece>> piecesGroupedByPoLine) {
    return Map.of();
  }

  @Override
  protected String getHoldingId(Piece piece) {
    return "";
  }

  @Override
  protected String getLocationId(Piece piece) {
    return "";
  }
}
