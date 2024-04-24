package org.folio.service.orders.lines.update.instance;

import static java.util.stream.Collectors.toList;
import static org.folio.service.inventory.InventoryItemManager.ID;
import static org.folio.service.inventory.InventoryItemManager.ITEM_HOLDINGS_RECORD_ID;

import java.util.ArrayList;
import java.util.List;
import java.util.Objects;
import java.util.function.Consumer;
import java.util.stream.Stream;

import org.apache.commons.collections4.CollectionUtils;
import org.apache.commons.collections4.ListUtils;
import org.apache.commons.lang3.ObjectUtils;
import org.folio.models.orders.lines.update.OrderLineUpdateInstanceHolder;
import org.folio.okapi.common.GenericCompositeFuture;
import org.folio.orders.utils.PoLineCommonUtil;
import org.folio.rest.acq.model.StoragePatchOrderLineRequest;
import org.folio.rest.core.exceptions.ErrorCodes;
import org.folio.rest.core.exceptions.HttpException;
import org.folio.rest.core.models.RequestContext;
import org.folio.rest.jaxrs.model.Error;
import org.folio.rest.jaxrs.model.Location;
import org.folio.rest.jaxrs.model.Parameter;
import org.folio.rest.jaxrs.model.Piece;
import org.folio.rest.jaxrs.model.ReplaceInstanceRef;
import org.folio.service.inventory.InventoryHoldingManager;
import org.folio.service.inventory.InventoryItemManager;
import org.folio.service.pieces.PieceStorageService;

import io.vertx.core.Future;
import io.vertx.core.json.JsonObject;
import one.util.streamex.StreamEx;


public class WithHoldingOrderLineUpdateInstanceStrategy extends BaseOrderLineUpdateInstanceStrategy {

  private static final String HOLDINGS_ITEMS = "holdingsItems";
  private static final String BARE_HOLDINGS_ITEMS = "bareHoldingsItems";
  private final PieceStorageService pieceStorageService;

  public WithHoldingOrderLineUpdateInstanceStrategy(InventoryItemManager inventoryItemManager,
                                                    InventoryHoldingManager inventoryHoldingManager,
                                                    PieceStorageService pieceStorageService) {
    super(inventoryItemManager, inventoryHoldingManager);
    this.pieceStorageService = pieceStorageService;
  }

  protected Future<Void> processHoldings(OrderLineUpdateInstanceHolder holder, RequestContext requestContext) {
    if (Objects.nonNull(holder.getPatchOrderLineRequest().getReplaceInstanceRef())) {
      ReplaceInstanceRef replaceInstanceRef = holder.getPatchOrderLineRequest().getReplaceInstanceRef();
      String newInstanceId = replaceInstanceRef.getNewInstanceId();

      holder.createStoragePatchOrderLineRequest(StoragePatchOrderLineRequest.PatchOrderLineOperationType.REPLACE_INSTANCE_REF, newInstanceId);

      return switch (replaceInstanceRef.getHoldingsOperation()) {
        case MOVE -> moveHoldings(holder, newInstanceId, requestContext);
        case FIND_OR_CREATE -> findOrCreateHoldingsAndUpdateItems(holder, newInstanceId, requestContext)
          // TODO: onSuccess or compose ???
          .onSuccess(v -> deleteAbandonedHoldings(replaceInstanceRef.getDeleteAbandonedHoldings(), holder.getStoragePoLine(), requestContext));
        case CREATE -> createHoldingsAndUpdateItems(holder, newInstanceId, requestContext)
          .onSuccess(v -> deleteAbandonedHoldings(replaceInstanceRef.getDeleteAbandonedHoldings(), holder.getStoragePoLine(), requestContext));
        default -> Future.succeededFuture();
      };
    } else {
      return Future.succeededFuture();
    }
  }


  private Future<Void> moveHoldings(OrderLineUpdateInstanceHolder holder, String newInstanceId, RequestContext requestContext) {
    return pieceStorageService.getPiecesByPoLineId(PoLineCommonUtil.convertToCompositePoLine(holder.getStoragePoLine()), requestContext)
      .map(pieces -> extractUniqueHoldingIds(pieces, holder.getStoragePoLine().getLocations()))
      .compose(holdingIds -> {
        holdingIds.forEach(holdingId -> holder.addHoldingRefsToStoragePatchOrderLineRequest(holdingId, holdingId));
        return inventoryHoldingManager.getHoldingsByIds(holdingIds, requestContext);
      })
      .compose(holdings -> {
        removeHoldingUnrecognizedFields(holdings);
        return inventoryHoldingManager.updateInstanceForHoldingRecords(holdings, newInstanceId, requestContext);
      });
  }

  private void removeHoldingUnrecognizedFields(List<JsonObject> holdings) {
    holdings.forEach(holding -> {
      holding.remove(HOLDINGS_ITEMS);
      holding.remove(BARE_HOLDINGS_ITEMS);
    });
  }

  private Future<Void> findOrCreateHoldingsAndUpdateItems(OrderLineUpdateInstanceHolder holder,
      String newInstanceId, RequestContext requestContext) {
    List<Future<Void>> futures = new ArrayList<>();
    return retrieveUniqueLocationsAndConsume(holder, requestContext, location -> {
      String holdingId = location.getHoldingId();
      futures.add(inventoryHoldingManager.getOrCreateHoldingRecordByInstanceAndLocation(newInstanceId, location, requestContext)
        .compose(newHoldingId -> {
          holder.addHoldingRefsToStoragePatchOrderLineRequest(holdingId, newHoldingId);
          if (ObjectUtils.notEqual(holdingId, newHoldingId)) {
            return updateItemsHolding(holdingId, newHoldingId, holder.getStoragePoLine().getId(), requestContext);
          } else {
            return Future.succeededFuture();
          }
        }));
    })
      .compose(v -> GenericCompositeFuture.join(new ArrayList<>(futures))
        .map(ok -> null));
  }

  private Future<Void> createHoldingsAndUpdateItems(OrderLineUpdateInstanceHolder holder,
      String newInstanceId, RequestContext requestContext) {
    List<Future<Void>> futures = new ArrayList<>();
    return retrieveUniqueLocationsAndConsume(holder, requestContext, location -> {
      String holdingId = location.getHoldingId();
      futures.add(inventoryHoldingManager.createHolding(newInstanceId, location, requestContext)
        .compose(newHoldingId -> {
          holder.addHoldingRefsToStoragePatchOrderLineRequest(holdingId, newHoldingId);
          return updateItemsHolding(holdingId, newHoldingId, holder.getStoragePoLine().getId(), requestContext);
        }));
    }).compose(v -> GenericCompositeFuture.join(new ArrayList<>(futures)))
      .mapEmpty();
  }

  private Future<Void> retrieveUniqueLocationsAndConsume(OrderLineUpdateInstanceHolder holder, RequestContext requestContext,
                                                                    Consumer<Location> consumer) {
    return pieceStorageService.getPiecesByPoLineId(PoLineCommonUtil.convertToCompositePoLine(holder.getStoragePoLine()), requestContext)
      .map(pieces -> {
        List<Location> pieceHoldingIds = pieces
          .stream()
          .map(piece -> new Location().withHoldingId(piece.getHoldingId()).withLocationId(piece.getLocationId()))
          .collect(toList());
        List<Location> storageHoldingIds = holder.getStoragePoLine().getLocations();

        StreamEx.of(ListUtils.union(pieceHoldingIds, storageHoldingIds))
          .distinct(location -> String.format("%s %s", location.getLocationId(), location.getHoldingId()))
          .filter(location -> Objects.nonNull(location.getHoldingId()))
          .forEach(consumer);
        return null;
      })
      .map(pieces -> null);
  }

  private Future<Void> updateItemsHolding(String holdingId, String newHoldingId, String poLineId, RequestContext requestContext) {
    return inventoryItemManager.getItemsByHoldingIdAndOrderLineId(holdingId, poLineId, requestContext)
        .map(items -> updateItemHoldingId(items, newHoldingId))
        .compose(items -> updateItemsInInventory(items, requestContext));
  }

  private List<JsonObject> updateItemHoldingId(List<JsonObject> items, String holdingId) {
    items.forEach(item -> item.put(ITEM_HOLDINGS_RECORD_ID, holdingId));
    return items;
  }

  private Future<Void> updateItemsInInventory(List<JsonObject> items, RequestContext requestContext) {
    List<Parameter> parameters = new ArrayList<>();
    return GenericCompositeFuture.join(items.stream()
      .map(item -> inventoryItemManager.updateItem(item, requestContext)
        .otherwise(ex -> {
          Parameter parameter = new Parameter().withKey("itemId").withValue(item.getString(ID));
          if (ex.getCause() instanceof HttpException httpException) {
            parameter.withAdditionalProperty("originalError", httpException.getError().getMessage());
          }
          parameters.add(parameter);
          return null;
        }))
      .collect(toList()))
      .mapEmpty()
      .map(v -> {
        if (CollectionUtils.isNotEmpty(parameters)) {
          Error error = ErrorCodes.ITEM_UPDATE_FAILED.toError().withParameters(parameters);
          throw new HttpException(500, error);
        }
        return null;
      });
  }

  private List<String> extractUniqueHoldingIds(List<Piece> pieces, List<Location> locations) {
    return Stream.concat(pieces.stream().map(Piece::getHoldingId),
        locations.stream().map(Location::getHoldingId))
      .distinct()
      .filter(Objects::nonNull)
      .collect(toList());
  }

}
