package org.folio.service.orders.flows.update.open;

import static java.util.stream.Collectors.collectingAndThen;
import static java.util.stream.Collectors.groupingBy;
import static java.util.stream.Collectors.summingInt;
import static java.util.stream.Collectors.toList;
import static org.folio.orders.utils.HelperUtils.calculatePiecesQuantity;
import static org.folio.orders.utils.PoLineCommonUtil.groupLocationsByHoldingId;
import static org.folio.orders.utils.PoLineCommonUtil.groupLocationsByLocationId;
import static org.folio.orders.utils.PoLineCommonUtil.isItemsUpdateRequiredForEresource;
import static org.folio.orders.utils.PoLineCommonUtil.isItemsUpdateRequiredForPhysical;
import static org.folio.orders.utils.PoLineCommonUtil.mapLocationsToTenantIds;

import java.util.ArrayList;
import java.util.Collections;
import java.util.EnumMap;
import java.util.List;
import java.util.Map;
import java.util.Objects;
import java.util.stream.Collectors;

import org.apache.commons.collections4.CollectionUtils;
import org.apache.commons.lang3.StringUtils;
import org.folio.models.pieces.OpenOrderPieceHolder;
import org.folio.rest.core.models.RequestContext;
import org.folio.rest.jaxrs.model.CompositePoLine;
import org.folio.rest.jaxrs.model.Location;
import org.folio.rest.jaxrs.model.Piece;
import org.folio.service.pieces.PieceStorageService;
import org.folio.service.pieces.PieceUtil;

import io.vertx.core.Future;
import one.util.streamex.StreamEx;

public class OpenCompositeOrderHolderBuilder {

  private final PieceStorageService pieceStorageService;

  public OpenCompositeOrderHolderBuilder(PieceStorageService pieceStorageService) {
    this.pieceStorageService = pieceStorageService;
  }

  public Future<OpenOrderPieceHolder> buildHolder(CompositePoLine compPOL, String titleId, List<Piece> expectedPiecesWithItem,
                RequestContext requestContext) {
    OpenOrderPieceHolder holder = new OpenOrderPieceHolder(titleId);
    return pieceStorageService.getPiecesByPoLineId(compPOL, requestContext)
      .map(holder::withExistingPieces)
      .map(aHolder -> {
        holder.withPiecesWithLocationToProcess(buildPiecesByLocationId(compPOL, expectedPiecesWithItem, holder.getExistingPieces()));
        holder.withPiecesWithHoldingToProcess(buildPiecesByHoldingId(compPOL, expectedPiecesWithItem, holder.getExistingPieces()));
        return null;
      })
      .map(aVoid -> holder.withPiecesWithChangedLocation(getPiecesWithChangedLocation(compPOL, holder.getPiecesWithLocationToProcess(), holder.getExistingPieces())))
      .map(aHolder -> {
        if (CollectionUtils.isEmpty(compPOL.getLocations())) {
          holder.withPiecesWithoutLocationId(createPiecesWithoutLocationId(compPOL, holder.getExistingPieces()));
        }
        return null;
      })
      .map(aVoid -> holder);
  }

  private List<Piece> buildPiecesByLocationId(CompositePoLine compPOL, List<Piece> expectedPiecesWithItem, List<Piece> existingPieces) {
    List<Piece> piecesToCreate = new ArrayList<>();
    // For each location collect pieces that need to be created.
    groupLocationsByLocationId(compPOL)
      .forEach((lineLocationId, lineLocations) -> {
        List<Piece> filteredExistingPieces = filterByLocationId(existingPieces, lineLocationId);
        List<Piece> createdPiecesWithItem = processPiecesWithItemAndLocationId(expectedPiecesWithItem, filteredExistingPieces, lineLocationId);
        piecesToCreate.addAll(createdPiecesWithItem);
        List<Piece> piecesWithoutItem =  processPiecesWithoutItemAndLocationId(compPOL, filteredExistingPieces, lineLocationId, lineLocations);
        piecesToCreate.addAll(piecesWithoutItem);
      });
    return piecesToCreate;
  }

  private List<Piece> getPiecesWithChangedLocation(CompositePoLine compPOL, List<Piece> needProcessPieces, List<Piece> existingPieces) {
    Map<String, Map<Piece.Format, Integer>> existingPieceMap = numOfPiecesByFormatAndLocationId(existingPieces, compPOL.getId());
    Map<String, Map<Piece.Format, Integer>> needProcessPiecesMap = numOfPiecesByFormatAndLocationId(needProcessPieces, compPOL.getId());
    Map<String, String> locationToTenantId = mapLocationsToTenantIds(compPOL);

    List<Piece> piecesForLocationUpdate = new ArrayList<>();
    for (Map.Entry<String, Map<Piece.Format, Integer>> entry : existingPieceMap.entrySet()) {
      String existingPieceLocationId = entry.getKey();
      Map<Piece.Format, Integer> existingPieceQtyMap = entry.getValue();
      for (Map.Entry<Piece.Format, Integer> existPieceFormatQty : existingPieceQtyMap.entrySet()) {
        Map<Piece.Format, Integer> pieceLocationMap = needProcessPiecesMap.get(existingPieceLocationId);
        if (pieceLocationMap == null) {
          needProcessPiecesMap.forEach((newLocationId, value) -> {
            Integer pieceQty = value.get(existPieceFormatQty.getKey());
            if (pieceQty != null && pieceQty.equals(existPieceFormatQty.getValue())) {
              List<Piece> piecesWithUpdatedLocation = existingPieces.stream()
                .filter(piece -> existingPieceLocationId.equals(piece.getLocationId())
                  && existPieceFormatQty.getKey() == piece.getFormat())
                .map(piece -> piece
                  .withLocationId(newLocationId)
                  .withReceivingTenantId(locationToTenantId.get(newLocationId)))
                .toList();
              piecesForLocationUpdate.addAll(piecesWithUpdatedLocation);
            }
          });
        }
      }
    }
    return piecesForLocationUpdate;
  }

  private List<Piece> createPiecesWithoutLocationId(CompositePoLine compPOL, List<Piece> existingPieces) {
    List<Piece> piecesToCreate = new ArrayList<>();
    Map<Piece.Format, Integer> expectedQuantitiesWithoutLocation = PieceUtil.calculatePiecesQuantityWithoutLocation(compPOL);
    Map<Piece.Format, Integer> existingPiecesQuantities = calculateQuantityOfExistingPiecesWithoutLocation(existingPieces);
    expectedQuantitiesWithoutLocation.forEach((format, expectedQty) -> {
      int remainingPiecesQuantity = expectedQty - existingPiecesQuantities.getOrDefault(format, 0);
      if (remainingPiecesQuantity > 0) {
        for (int i = 0; i < remainingPiecesQuantity; i++) {
          piecesToCreate.add(new Piece().withFormat(format).withPoLineId(compPOL.getId()));
        }
      }
    });
    return piecesToCreate;
  }

  private Map<Piece.Format, Integer> calculateQuantityOfExistingPiecesWithoutLocation(List<Piece> pieces) {
    return StreamEx.of(pieces)
      .filter(piece -> StringUtils.isEmpty(piece.getLocationId()))
      .groupingBy(Piece::getFormat, collectingAndThen(toList(), List::size));
  }


  private List<Piece> processPiecesWithItemAndLocationId(List<Piece> piecesWithItem, List<Piece> existedPieces, String existingPieceLocationId) {
    List<Piece> expectedPiecesWithItem = filterByLocationId(piecesWithItem, existingPieceLocationId);
    return collectMissingPiecesWithItem(expectedPiecesWithItem, existedPieces);
  }

  private List<Piece> filterByLocationId(List<Piece> pieces, String locationId) {
    return pieces.stream()
      .filter(piece -> locationId.equals(piece.getLocationId()))
      .collect(Collectors.toList());
  }

  private List<Piece> buildPiecesByHoldingId(CompositePoLine compPOL, List<Piece> expectedPiecesWithItem, List<Piece> existingPieces) {
    List<Piece> piecesToCreate = new ArrayList<>();
    // For each location collect pieces that need to be created.
    groupLocationsByHoldingId(compPOL)
      .forEach((existingPieceHoldingId, existingPieceLocations) -> {
        List<Piece> filteredExistingPieces = filterByHoldingId(existingPieces, existingPieceHoldingId);
        List<Piece> createdPiecesWithItem = processPiecesWithItemAndHoldingId(expectedPiecesWithItem, filteredExistingPieces, existingPieceHoldingId);
        piecesToCreate.addAll(createdPiecesWithItem);
        List<Piece> piecesWithoutItem = processPiecesWithoutItemAndHoldingId(compPOL, filteredExistingPieces, existingPieceHoldingId, existingPieceLocations);
        piecesToCreate.addAll(piecesWithoutItem);
      });
    return piecesToCreate;
  }

  private List<Piece> processPiecesWithItemAndHoldingId(List<Piece> piecesWithItem, List<Piece> existedPieces, String existingPieceLocationId) {
    List<Piece> expectedPiecesWithItem = filterByHoldingId(piecesWithItem, existingPieceLocationId);
    return collectMissingPiecesWithItem(expectedPiecesWithItem, existedPieces);
  }


  /**
   * Find pieces for which created items, but which are not yet in the storage.
   *
   * @param piecesWithItem pieces for which created items
   * @param existingPieces pieces from storage
   * @return List of Pieces with itemId that are not in storage.
   */
  private List<Piece> collectMissingPiecesWithItem(List<Piece> piecesWithItem, List<Piece> existingPieces) {
    return piecesWithItem.stream()
      .filter(pieceWithItem -> existingPieces.stream()
        .noneMatch(existingPiece -> pieceWithItem.getItemId().equals(existingPiece.getItemId())))
      .collect(Collectors.toList());
  }


  private List<Piece> filterByHoldingId(List<Piece> pieces, String holdingId) {
    return pieces.stream()
      .filter(piece -> holdingId.equals(piece.getHoldingId()))
      .collect(Collectors.toList());
  }


  private List<Piece> processPiecesWithoutItemAndLocationId(CompositePoLine compPOL, List<Piece> existedPieces, String existingPieceLocationId, List<Location> existingPieceLocations) {
    List<Piece> piecesToCreate = new ArrayList<>();
    Map<Piece.Format, Integer> expectedQuantitiesWithoutItem = calculatePiecesWithoutItemIdQuantity(compPOL, existingPieceLocations);
    Map<Piece.Format, Integer> existedQuantityWithoutItem = calculateQuantityOfExistingPiecesWithoutItem(existedPieces);
    Map<String, String> locationToTenantId = mapLocationsToTenantIds(compPOL);

    expectedQuantitiesWithoutItem.forEach((format, expectedQty) -> {
      int remainingPiecesQuantity = expectedQty - existedQuantityWithoutItem.getOrDefault(format, 0);
      if (remainingPiecesQuantity > 0) {
        for (int i = 0; i < remainingPiecesQuantity; i++) {
          piecesToCreate.add(new Piece()
            .withFormat(format)
            .withLocationId(existingPieceLocationId)
            .withPoLineId(compPOL.getId())
            .withReceivingTenantId(locationToTenantId.get(existingPieceLocationId))
          );
        }
      }
    });
    return piecesToCreate;
  }

  private List<Piece> processPiecesWithoutItemAndHoldingId(CompositePoLine compPOL, List<Piece> existedPieces, String existingPieceHoldingId, List<Location> existingPieceLocations) {
    List<Piece> piecesToCreate = new ArrayList<>();
    Map<Piece.Format, Integer> expectedQuantitiesWithoutItem = calculatePiecesWithoutItemIdQuantity(compPOL, existingPieceLocations);
    Map<Piece.Format, Integer> existedQuantityWithoutItem = calculateQuantityOfExistingPiecesWithoutItem(existedPieces);
    expectedQuantitiesWithoutItem.forEach((format, expectedQty) -> {
      int remainingPiecesQuantity = expectedQty - existedQuantityWithoutItem.getOrDefault(format, 0);
      if (remainingPiecesQuantity > 0) {
        for (int i = 0; i < remainingPiecesQuantity; i++) {
          piecesToCreate.add(new Piece().withFormat(format).withHoldingId(existingPieceHoldingId).withPoLineId(compPOL.getId()));
        }
      }
    });
    return piecesToCreate;
  }

  private Map<String, Map<Piece.Format, Integer>> numOfPiecesByFormatAndLocationId(List<Piece> pieces, String poLineId) {
    return pieces.stream()
      .filter(piece -> Objects.nonNull(piece.getPoLineId())
        && Objects.nonNull(piece.getLocationId())
        && piece.getPoLineId().equals(poLineId))
      .collect(groupingBy(Piece::getLocationId, groupingBy(Piece::getFormat, summingInt(q -> 1))));
  }


  private Map<Piece.Format, Integer> calculateQuantityOfExistingPiecesWithoutItem(List<Piece> pieces) {
    return StreamEx.of(pieces)
      .filter(piece -> StringUtils.isEmpty(piece.getItemId()))
      .groupingBy(Piece::getFormat, collectingAndThen(toList(), List::size));
  }

  /**
   * Calculates pieces quantity for list of locations and return map where piece format is a key and corresponding quantity of pieces as value.
   *
   * @param compPOL composite PO Line
   * @param locations list of locations to calculate quantity for
   * @return quantity of pieces per piece format either not required Inventory item for PO Line
   */
  private Map<Piece.Format, Integer> calculatePiecesWithoutItemIdQuantity(CompositePoLine compPOL, List<Location> locations) {
    // Piece records are not going to be created for PO Line which is going to be checked-in
    if (compPOL.getCheckinItems() != null && compPOL.getCheckinItems()) {
      return Collections.emptyMap();
    }

    EnumMap<Piece.Format, Integer> quantities = new EnumMap<>(Piece.Format.class);
    switch (compPOL.getOrderFormat()) {
    case P_E_MIX:
      if (!isItemsUpdateRequiredForPhysical(compPOL)) {
        quantities.put(Piece.Format.PHYSICAL, calculatePiecesQuantity(Piece.Format.PHYSICAL, locations));
      }
      if (!isItemsUpdateRequiredForEresource(compPOL)) {
        quantities.put(Piece.Format.ELECTRONIC, calculatePiecesQuantity(Piece.Format.ELECTRONIC, locations));
      }
      return quantities;
    case PHYSICAL_RESOURCE:
      if (!isItemsUpdateRequiredForPhysical(compPOL)) {
        quantities.put(Piece.Format.PHYSICAL, calculatePiecesQuantity(Piece.Format.PHYSICAL, locations));
      }
      return quantities;
    case ELECTRONIC_RESOURCE:
      if (!isItemsUpdateRequiredForEresource(compPOL)) {
        quantities.put(Piece.Format.ELECTRONIC, calculatePiecesQuantity(Piece.Format.ELECTRONIC, locations));
      }
      return quantities;
    case OTHER:
      if (!isItemsUpdateRequiredForPhysical(compPOL)) {
        quantities.put(Piece.Format.OTHER, calculatePiecesQuantity(Piece.Format.OTHER, locations));
      }
      return quantities;
    default:
      return Collections.emptyMap();
    }
  }
}
