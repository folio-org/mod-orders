package org.folio.service.orders.lines.update.instance;

import io.vertx.core.Future;
import io.vertx.core.json.Json;
import io.vertx.core.json.JsonObject;
import one.util.streamex.StreamEx;
import org.apache.commons.collections4.CollectionUtils;
import org.apache.commons.collections4.ListUtils;
import org.apache.logging.log4j.LogManager;
import org.apache.logging.log4j.Logger;
import org.folio.models.orders.lines.update.OrderLineUpdateInstanceHolder;
import org.folio.okapi.common.GenericCompositeFuture;
import org.folio.orders.utils.PoLineCommonUtil;
import org.folio.orders.utils.RequestContextUtil;
import org.folio.rest.acq.model.StoragePatchOrderLineRequest;
import org.folio.rest.core.exceptions.ErrorCodes;
import org.folio.rest.core.exceptions.HttpException;
import org.folio.rest.core.models.RequestContext;
import org.folio.rest.jaxrs.model.CompositePoLine;
import org.folio.rest.jaxrs.model.Error;
import org.folio.rest.jaxrs.model.Location;
import org.folio.rest.jaxrs.model.Parameter;
import org.folio.rest.jaxrs.model.Piece;
import org.folio.rest.jaxrs.model.PoLine;
import org.folio.rest.jaxrs.model.ReplaceInstanceRef;
import org.folio.rest.tools.utils.TenantTool;
import org.folio.service.inventory.InventoryHoldingManager;
import org.folio.service.inventory.InventoryInstanceManager;
import org.folio.service.inventory.InventoryItemManager;
import org.folio.service.pieces.PieceStorageService;

import java.util.ArrayList;
import java.util.HashMap;
import java.util.List;
import java.util.Map;
import java.util.Objects;
import java.util.function.Function;
import java.util.stream.Collectors;

import static java.util.stream.Collectors.toList;
import static org.folio.service.inventory.InventoryItemManager.ID;
import static org.folio.service.inventory.InventoryItemManager.ITEM_HOLDINGS_RECORD_ID;


public class WithHoldingOrderLineUpdateInstanceStrategy extends BaseOrderLineUpdateInstanceStrategy {

  private static final Logger logger = LogManager.getLogger(WithHoldingOrderLineUpdateInstanceStrategy.class);

  private static final String HOLDINGS_ITEMS = "holdingsItems";
  private static final String BARE_HOLDINGS_ITEMS = "bareHoldingsItems";
  private final PieceStorageService pieceStorageService;

  public WithHoldingOrderLineUpdateInstanceStrategy(InventoryInstanceManager inventoryInstanceManager,
                                                    InventoryItemManager inventoryItemManager,
                                                    InventoryHoldingManager inventoryHoldingManager,
                                                    PieceStorageService pieceStorageService) {
    super(inventoryInstanceManager, inventoryItemManager, inventoryHoldingManager);
    this.pieceStorageService = pieceStorageService;
  }

  protected Future<Void> processHoldings(OrderLineUpdateInstanceHolder holder, RequestContext requestContext) {
    ReplaceInstanceRef replaceInstanceRef = holder.getPatchOrderLineRequest().getReplaceInstanceRef();
    if (replaceInstanceRef == null) {
      return Future.succeededFuture();
    }
    String newInstanceId = replaceInstanceRef.getNewInstanceId();

    holder.createStoragePatchOrderLineRequest(StoragePatchOrderLineRequest.PatchOrderLineOperationType.REPLACE_INSTANCE_REF, newInstanceId);
    PoLine poLine = holder.getStoragePoLine();

    return switch (replaceInstanceRef.getHoldingsOperation()) {
      case MOVE -> moveHoldings(holder, newInstanceId, requestContext);
      case FIND_OR_CREATE -> findOrCreateHoldingsAndUpdateItems(holder, newInstanceId, requestContext)
        .compose(v -> deleteAbandonedHoldings(replaceInstanceRef.getDeleteAbandonedHoldings(), poLine, requestContext));
      case CREATE -> createHoldingsAndUpdateItems(holder, newInstanceId, requestContext)
        .compose(v -> deleteAbandonedHoldings(replaceInstanceRef.getDeleteAbandonedHoldings(), poLine, requestContext));
      default -> Future.succeededFuture();
    };
  }


  private Future<Void> moveHoldings(OrderLineUpdateInstanceHolder holder, String newInstanceId, RequestContext requestContext) {
    PoLine poLine = holder.getStoragePoLine();
    CompositePoLine compPOL = PoLineCommonUtil.convertToCompositePoLine(poLine);
    return pieceStorageService.getPiecesByPoLineId(compPOL, requestContext)
      .map(pieces -> getHoldingsByTenants(holder, pieces, requestContext))
      .compose(holdingsByTenant -> {
          var updateHoldings = holdingsByTenant.entrySet()
            .stream()
            .map(entry -> entry.getValue().compose(holdings -> {
              removeHoldingUnrecognizedFields(holdings);
              var locationContext = RequestContextUtil.createContextWithNewTenantId(requestContext, entry.getKey());
              return inventoryInstanceManager.createShadowInstanceIfNeeded(newInstanceId, locationContext)
                .compose(instance -> inventoryHoldingManager.updateInstanceForHoldingRecords(holdings, newInstanceId, locationContext));
            }))
            .toList();
          return GenericCompositeFuture.all(updateHoldings).mapEmpty();
        }
      );
  }

  private Map<String, Future<List<JsonObject>>> getHoldingsByTenants(OrderLineUpdateInstanceHolder holder, List<Piece> pieces, RequestContext requestContext) {
    PoLine poLine = holder.getStoragePoLine();
    Map<String, List<String>> holdingsByTenant = new HashMap<>();
    for (var piece : pieces) {
      String holdingId = piece.getHoldingId();
      if (holdingId != null) {
        putHolding(holder, piece.getReceivingTenantId(), holdingsByTenant, holdingId);
      }
    }
    for (var location : poLine.getLocations()) {
      String holdingId = location.getHoldingId();
      if (holdingId != null) {
        putHolding(holder, location.getTenantId(), holdingsByTenant, holdingId);
      }
    }
    return holdingsByTenant.entrySet()
      .stream()
      .collect(Collectors.toMap(
        Map.Entry::getKey,
        entry -> {
          var newContext = RequestContextUtil.createContextWithNewTenantId(requestContext, entry.getKey());
          return inventoryHoldingManager.getHoldingsByIds(entry.getValue(), newContext);
        }
      ));
  }

  private void putHolding(OrderLineUpdateInstanceHolder holder, String tenantId, Map<String, List<String>> holdingsByTenant, String holdingId) {
    List<String> list = holdingsByTenant.computeIfAbsent(tenantId, k -> new ArrayList<>());
    if (!list.contains(holdingId)) {
      list.add(holdingId);
      holder.addHoldingRefsToStoragePatchOrderLineRequest(holdingId, holdingId);
    }
  }

  private void removeHoldingUnrecognizedFields(List<JsonObject> holdings) {
    holdings.forEach(holding -> {
      holding.remove(HOLDINGS_ITEMS);
      holding.remove(BARE_HOLDINGS_ITEMS);
    });
  }

  private Future<Void> findOrCreateHoldingsAndUpdateItems(OrderLineUpdateInstanceHolder holder,
                                                          String newInstanceId, RequestContext requestContext) {
    return processLocations(holder, requestContext,
      location -> findOrCreateHoldingsAndUpdateItems(holder, newInstanceId, location, requestContext));
  }

  private Future<Void> findOrCreateHoldingsAndUpdateItems(OrderLineUpdateInstanceHolder holder,
                                                          String newInstanceId,
                                                          Location location,
                                                          RequestContext requestContext) {
    PoLine poLine = holder.getStoragePoLine();
    String holdingId = location.getHoldingId();
    var locationContext = RequestContextUtil.createContextWithNewTenantId(requestContext, location.getTenantId());
    logger.info("findOrCreateHoldingsAndUpdateItems:: start processing for new instanceId: {}, holdingId: {}, tenantId(for ECS): {}",
      newInstanceId, holdingId, location.getTenantId());
    return inventoryInstanceManager.createShadowInstanceIfNeeded(newInstanceId, locationContext)
      .compose(instance -> inventoryHoldingManager.getOrCreateHoldingRecordByInstanceAndLocation(newInstanceId, location, locationContext))
      .compose(newHoldingId -> {
        holder.addHoldingRefsToStoragePatchOrderLineRequest(holdingId, newHoldingId);
        if (Objects.equals(holdingId, newHoldingId)) {
          return Future.succeededFuture();
        }
        return updateItemsHolding(holdingId, newHoldingId, poLine.getId(), locationContext);
      });
  }

  private Future<Void> createHoldingsAndUpdateItems(OrderLineUpdateInstanceHolder holder,
                                                    String newInstanceId, RequestContext requestContext) {
    return processLocations(holder, requestContext,
      location -> createHoldingsAndUpdateItems(holder, newInstanceId, location, requestContext));
  }

  private Future<Void> createHoldingsAndUpdateItems(OrderLineUpdateInstanceHolder holder,
                                                    String newInstanceId,
                                                    Location location,
                                                    RequestContext requestContext) {
    PoLine poLine = holder.getStoragePoLine();
    String holdingId = location.getHoldingId();
    var locationContext = RequestContextUtil.createContextWithNewTenantId(requestContext, location.getTenantId());
    logger.info("createHoldingsAndUpdateItems:: start processing for new instanceId: {}, holdingId: {}, tenantId(for ECS): {}",
      newInstanceId, holdingId, location.getTenantId());
    return inventoryInstanceManager.createShadowInstanceIfNeeded(newInstanceId, locationContext)
      .compose(instance -> inventoryHoldingManager.createHolding(newInstanceId, location, locationContext))
      .compose(newHoldingId -> {
        holder.addHoldingRefsToStoragePatchOrderLineRequest(holdingId, newHoldingId);
        return updateItemsHolding(holdingId, newHoldingId, poLine.getId(), locationContext);
      });
  }

  private Future<Void> processLocations(OrderLineUpdateInstanceHolder holder,
                                        RequestContext requestContext,
                                        Function<Location, Future<Void>> processFunction) {
    return retrieveUniqueLocations(holder.getStoragePoLine(), requestContext)
      .compose(tenantIdToLocationsMap ->
        GenericCompositeFuture.all(tenantIdToLocationsMap.values().stream()
          .map(locations ->
            GenericCompositeFuture.join(locations.stream()
              .map(processFunction)
              .toList())
          )
          .collect(toList()))
      )
      .mapEmpty();
  }

  private Future<Map<String, List<Location>>> retrieveUniqueLocations(PoLine poLine, RequestContext requestContext) {
    return pieceStorageService.getPiecesByPoLineId(PoLineCommonUtil.convertToCompositePoLine(poLine), requestContext)
      .map(pieces -> {
        List<Location> pieceHoldingIds = pieces
          .stream()
          .map(piece -> new Location()
            .withHoldingId(piece.getHoldingId())
            .withLocationId(piece.getLocationId())
            .withTenantId(piece.getReceivingTenantId()))
          .collect(toList());
        List<Location> storageHoldingIds = poLine.getLocations();

        List<Location> uniqueLocations = StreamEx.of(ListUtils.union(pieceHoldingIds, storageHoldingIds))
          .distinct(location -> String.format("%s %s", location.getLocationId(), location.getHoldingId()))
          .filter(location -> Objects.nonNull(location.getHoldingId()))
          .toList();
        logger.info("retrieveUniqueLocations:: list of result locations: {}", Json.encodePrettily(uniqueLocations));
        return uniqueLocations.stream()
          .collect(Collectors.groupingBy(location -> Objects.requireNonNullElse(location.getTenantId(), TenantTool.tenantId(requestContext.getHeaders()))));
      });
  }

  private Future<Void> updateItemsHolding(String holdingId, String newHoldingId, String poLineId, RequestContext requestContext) {
    return inventoryItemManager.getItemsByHoldingIdAndOrderLineId(holdingId, poLineId, requestContext)
      .compose(items -> updateItemsInInventory(items, newHoldingId, requestContext));
  }

  private Future<Void> updateItemsInInventory(List<JsonObject> items, String newHoldingId, RequestContext requestContext) {
    items.forEach(item -> item.put(ITEM_HOLDINGS_RECORD_ID, newHoldingId));
    List<Parameter> parameters = new ArrayList<>();
    return GenericCompositeFuture.join(
      items
        .stream()
        .map(item -> inventoryItemManager.updateItem(item, requestContext)
          .otherwise(ex -> {
            var itemIdParam = new Parameter().withKey("itemId").withValue(item.getString(ID));
            if (ex.getCause() instanceof HttpException httpException) {
              itemIdParam.withAdditionalProperty("originalError", httpException.getError().getMessage());
            }
            var tenantIdParam = new Parameter().withKey("tenantId").withValue(TenantTool.tenantId(requestContext.getHeaders()));
            parameters.add(itemIdParam);
            parameters.add(tenantIdParam);
            return null;
          }))
        .toList()
      )
      .mapEmpty()
      .map(v -> {
        if (CollectionUtils.isNotEmpty(parameters)) {
          Error error = ErrorCodes.ITEM_UPDATE_FAILED.toError().withParameters(parameters);
          throw new HttpException(500, error);
        }
        return null;
      });
  }

}
