package org.folio.service.orders.lines.update.instance;

import static java.util.stream.Collectors.toList;
import static org.folio.service.inventory.InventoryManager.ID;
import static org.folio.service.inventory.InventoryManager.ITEM_HOLDINGS_RECORD_ID;

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
import org.folio.service.inventory.InventoryManager;
import org.folio.service.pieces.PieceStorageService;

import io.vertx.core.Future;
import io.vertx.core.json.JsonObject;
import one.util.streamex.StreamEx;


public class WithHoldingOrderLineUpdateInstanceStrategy extends BaseOrderLineUpdateInstanceStrategy {

  private final PieceStorageService pieceStorageService;

  public WithHoldingOrderLineUpdateInstanceStrategy(InventoryManager inventoryManager, PieceStorageService pieceStorageService) {
    super(inventoryManager);
    this.pieceStorageService = pieceStorageService;
  }

  protected Future<Void> processHoldings(OrderLineUpdateInstanceHolder holder, RequestContext requestContext) {
    if (Objects.nonNull(holder.getPatchOrderLineRequest().getReplaceInstanceRef())) {
      ReplaceInstanceRef replaceInstanceRef = holder.getPatchOrderLineRequest().getReplaceInstanceRef();
      String newInstanceId = replaceInstanceRef.getNewInstanceId();

      holder.createStoragePatchOrderLineRequest(StoragePatchOrderLineRequest.PatchOrderLineOperationType.REPLACE_INSTANCE_REF, newInstanceId);

      switch (replaceInstanceRef.getHoldingsOperation()) {
        case MOVE:
          return moveHoldings(holder, newInstanceId, requestContext);
        case FIND_OR_CREATE:
          return findOrCreateHoldingsAndUpdateItems(holder, newInstanceId, requestContext)
            // TODO: onSuccess or compose ???
              .onSuccess(v -> deleteAbandonedHoldings(replaceInstanceRef.getDeleteAbandonedHoldings(),
                  holder.getStoragePoLine(), requestContext));
        case CREATE:
          return createHoldingsAndUpdateItems(holder, newInstanceId, requestContext)
              .onSuccess(v -> deleteAbandonedHoldings(replaceInstanceRef.getDeleteAbandonedHoldings(),
                  holder.getStoragePoLine(), requestContext));
      case NONE:
        default:
          return Future.succeededFuture();
      }
    } else {
      return Future.succeededFuture();
    }
  }


  private Future<Void> moveHoldings(OrderLineUpdateInstanceHolder holder, String newInstanceId, RequestContext requestContext) {
    return pieceStorageService.getPiecesByPoLineId(PoLineCommonUtil.convertToCompositePoLine(holder.getStoragePoLine()), requestContext)
      .map(pieces -> extractUniqueHoldingIds(pieces, holder.getStoragePoLine().getLocations()))
      .compose(holdingIds -> {
        holdingIds.forEach(holdingId -> holder.addHoldingRefsToStoragePatchOrderLineRequest(holdingId, holdingId));
        return inventoryManager.getHoldingsByIds(holdingIds, requestContext);
      }).compose(holdings -> inventoryManager.updateInstanceForHoldingRecords(holdings, newInstanceId, requestContext));
  }

  private Future<Void> findOrCreateHoldingsAndUpdateItems(OrderLineUpdateInstanceHolder holder,
      String newInstanceId, RequestContext requestContext) {
    List<Future<Void>> futures = new ArrayList<>();
    return retrieveUniqueLocationsAndConsume(holder, requestContext, location -> {
      String holdingId = location.getHoldingId();
      futures.add(inventoryManager.getOrCreateHoldingRecordByInstanceAndLocation(newInstanceId, location, requestContext)
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
      futures.add(inventoryManager.createHolding(newInstanceId, location, requestContext)
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
      .onSuccess(pieces -> {
        List<Location> pieceHoldingIds = pieces
          .stream()
          .map(piece -> new Location().withHoldingId(piece.getHoldingId()).withLocationId(piece.getLocationId()))
          .collect(toList());
        List<Location> storageHoldingIds = holder.getStoragePoLine().getLocations();

        StreamEx.of(ListUtils.union(pieceHoldingIds, storageHoldingIds))
          .distinct(location -> String.format("%s %s", location.getLocationId(), location.getHoldingId()))
          .filter(location -> Objects.nonNull(location.getHoldingId()))
          .forEach(consumer);
      })
      .map(pieces -> null);
  }

  private Future<Void> updateItemsHolding(String holdingId, String newHoldingId, String poLineId, RequestContext requestContext) {
    return inventoryManager.getItemsByHoldingIdAndOrderLineId(holdingId, poLineId, requestContext)
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
        .map(item -> inventoryManager.updateItem(item, requestContext)
             .onFailure(ex -> {
              Parameter parameter = new Parameter().withKey("itemId").withValue(item.getString(ID));
              if (ex.getCause() instanceof HttpException){
                HttpException httpException = (HttpException) ex.getCause();
                parameter.withAdditionalProperty("originalError", httpException.getError().getMessage());
              }
              parameters.add(parameter);
            }))
        .collect(toList())).mapEmpty()
        .onSuccess(v -> {
      if (CollectionUtils.isNotEmpty(parameters)) {
        Error error = ErrorCodes.ITEM_UPDATE_FAILED.toError().withParameters(parameters);
        throw new HttpException(500, error);
      }
    }).mapEmpty();
  }

  private List<String> extractUniqueHoldingIds(List<Piece> pieces, List<Location> locations) {
    return Stream.concat(pieces.stream().map(Piece::getHoldingId),
        locations.stream().map(Location::getHoldingId))
      .distinct()
      .filter(Objects::nonNull)
      .collect(toList());
  }

}
