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
import static org.folio.orders.utils.PoLineCommonUtil.mapHoldingIdsToTenantIds;
import static org.folio.orders.utils.PoLineCommonUtil.mapLocationIdsToTenantIds;
import static org.folio.rest.jaxrs.model.PoLine.OrderFormat.ELECTRONIC_RESOURCE;
import static org.folio.rest.jaxrs.model.PoLine.OrderFormat.OTHER;
import static org.folio.rest.jaxrs.model.PoLine.OrderFormat.PHYSICAL_RESOURCE;
import static org.folio.rest.jaxrs.model.PoLine.OrderFormat.P_E_MIX;

import java.util.ArrayList;
import java.util.Collections;
import java.util.EnumMap;
import java.util.List;
import java.util.Map;
import java.util.Objects;
import java.util.function.Function;
import java.util.function.Supplier;
import java.util.stream.Collectors;

import lombok.RequiredArgsConstructor;
import org.apache.commons.collections4.CollectionUtils;
import org.apache.commons.lang3.StringUtils;
import org.folio.models.pieces.OpenOrderPieceHolder;
import org.folio.rest.core.models.RequestContext;
import org.folio.rest.jaxrs.model.PoLine;
import org.folio.rest.jaxrs.model.Location;
import org.folio.rest.jaxrs.model.Piece;
import org.folio.service.pieces.PieceStorageService;
import org.folio.service.pieces.PieceUtil;

import io.vertx.core.Future;
import one.util.streamex.StreamEx;
import org.folio.service.titles.TitlesService;

@RequiredArgsConstructor
public class OpenCompositeOrderHolderBuilder {

  private final PieceStorageService pieceStorageService;
  private final TitlesService titlesService;

  public Future<OpenOrderPieceHolder> buildHolder(PoLine poLine, String titleId, List<Piece> expectedPiecesWithItem, RequestContext requestContext) {
    return titlesService.getTitleById(titleId, requestContext)
      .compose(title -> pieceStorageService.getPiecesByPoLineId(poLine, requestContext)
        .map(pieces -> new OpenOrderPieceHolder(title).withExistingPieces(pieces)))
      .map(holder -> holder
        .withPiecesWithLocationToProcess(buildPiecesByLocationId(poLine, expectedPiecesWithItem, holder.getExistingPieces()))
        .withPiecesWithHoldingToProcess(buildPiecesByHoldingId(poLine, expectedPiecesWithItem, holder.getExistingPieces()))
        .withPiecesWithChangedLocation(getPiecesWithChangedLocation(poLine, holder.getPiecesWithLocationToProcess(), holder.getExistingPieces()))
        .withPiecesWithoutLocationId(createPiecesWithoutLocationId(poLine, holder.getExistingPieces())));
  }

  private List<Piece> buildPiecesByLocationId(PoLine poLine, List<Piece> expectedPiecesWithItem, List<Piece> existingPieces) {
    List<Piece> piecesToCreate = new ArrayList<>();
    // For each location collect pieces that need to be created.
    groupLocationsByLocationId(poLine)
      .forEach((lineLocationId, lineLocations) -> {
        var tenantId = mapLocationIdsToTenantIds(poLine).get(lineLocationId);
        var filteredExistingPieces = filterByLocationId(existingPieces, lineLocationId);
        var filteredExpectedPiecesWithItem = filterByLocationId(expectedPiecesWithItem, lineLocationId);
        piecesToCreate.addAll(collectMissingPiecesWithItem(filteredExpectedPiecesWithItem, filteredExistingPieces));
        piecesToCreate.addAll(processPiecesWithoutItemAndLocationId(poLine, filteredExistingPieces, lineLocationId, tenantId, lineLocations));
      });
    return piecesToCreate;
  }

  private List<Piece> getPiecesWithChangedLocation(PoLine poLine, List<Piece> needProcessPieces, List<Piece> existingPieces) {
    Map<String, Map<Piece.Format, Integer>> existingPieceMap = numOfPiecesByFormatAndLocationId(existingPieces, poLine.getId());
    Map<String, Map<Piece.Format, Integer>> needProcessPiecesMap = numOfPiecesByFormatAndLocationId(needProcessPieces, poLine.getId());
    Map<String, String> locationToTenantId = mapLocationIdsToTenantIds(poLine);

    List<Piece> piecesForLocationUpdate = new ArrayList<>();
    existingPieceMap.forEach((existingPieceLocationId, existingPieceQtyMap) -> {
      for (var existPieceFormatQty : existingPieceQtyMap.entrySet()) {
        if (needProcessPiecesMap.get(existingPieceLocationId) != null) {
          continue;
        }
        needProcessPiecesMap.forEach((newLocationId, pieceQtyMap) -> {
          var pieceQty = pieceQtyMap.get(existPieceFormatQty.getKey());
          if (pieceQty == null || !pieceQty.equals(existPieceFormatQty.getValue())) {
            return;
          }
          piecesForLocationUpdate.addAll(existingPieces.stream()
            .filter(piece -> existingPieceLocationId.equals(piece.getLocationId()) && existPieceFormatQty.getKey() == piece.getFormat())
            .map(piece -> piece.withLocationId(newLocationId).withReceivingTenantId(locationToTenantId.get(newLocationId)))
            .toList());
        });
      }
    });
    return piecesForLocationUpdate;
  }

  private List<Piece> createPiecesWithoutLocationId(PoLine poLine, List<Piece> existingPieces) {
    if (CollectionUtils.isNotEmpty(poLine.getLocations())) {
      return Collections.emptyList();
    }
    List<Piece> piecesToCreate = new ArrayList<>();
    Map<Piece.Format, Integer> expectedQuantitiesWithoutLocation = PieceUtil.calculatePiecesQuantityWithoutLocation(poLine);
    Map<Piece.Format, Integer> existingPiecesQuantities = calculateQuantityOfExistingPiecesWithoutLocation(existingPieces);
    expectedQuantitiesWithoutLocation.forEach((format, expectedQty) -> {
      int remainingPiecesQuantity = expectedQty - existingPiecesQuantities.getOrDefault(format, 0);
      for (int i = 0; i < remainingPiecesQuantity; i++) {
        piecesToCreate.add(new Piece()
          .withFormat(format)
          .withPoLineId(poLine.getId())
          .withReceiptDate(PieceUtil.getExpectedReceiptDate(format, poLine)));
      }
    });
    return piecesToCreate;
  }

  private Map<Piece.Format, Integer> calculateQuantityOfExistingPiecesWithoutLocation(List<Piece> pieces) {
    return StreamEx.of(pieces)
      .filter(piece -> StringUtils.isEmpty(piece.getLocationId()))
      .groupingBy(Piece::getFormat, collectingAndThen(toList(), List::size));
  }

  private List<Piece> buildPiecesByHoldingId(PoLine poLine, List<Piece> expectedPiecesWithItem, List<Piece> existingPieces) {
    List<Piece> piecesToCreate = new ArrayList<>();
    // For each location collect pieces that need to be created.
    groupLocationsByHoldingId(poLine)
      .forEach((existingPieceHoldingId, existingPieceLocations) -> {
        var tenantId = mapHoldingIdsToTenantIds(poLine).get(existingPieceHoldingId);
        var filteredExistingPieces = filterByHoldingId(existingPieces, existingPieceHoldingId);
        var filteredExpectedPiecesWithItem = filterByHoldingId(expectedPiecesWithItem, existingPieceHoldingId);
        piecesToCreate.addAll(collectMissingPiecesWithItem(filteredExpectedPiecesWithItem, filteredExistingPieces));
        piecesToCreate.addAll(processPiecesWithoutItemAndHoldingId(poLine, filteredExistingPieces, existingPieceHoldingId, tenantId, existingPieceLocations));
      });
    return piecesToCreate;
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
        .filter(existingPiece -> Objects.nonNull(existingPiece.getItemId()))
        .noneMatch(existingPiece -> pieceWithItem.getItemId().equals(existingPiece.getItemId())))
      .collect(Collectors.toList());
  }

  private List<Piece> filterByLocationId(List<Piece> pieces, String locationId) {
    return filterByField(pieces, locationId, Piece::getLocationId);
  }

  private List<Piece> filterByHoldingId(List<Piece> pieces, String holdingId) {
    return filterByField(pieces, holdingId, Piece::getHoldingId);
  }

  private List<Piece> filterByField(List<Piece> pieces, String value, Function<Piece, String> fieldExtractor) {
    return StreamEx.of(pieces)
      .filter(piece -> value.equals(fieldExtractor.apply(piece)))
      .collect(Collectors.toList());
  }


  private List<Piece> processPiecesWithoutItemAndLocationId(PoLine poLine, List<Piece> existedPieces, String existingPieceLocationId,
                                                            String tenantId, List<Location> existingPieceLocations) {
    return processPiecesWithoutItemAndLocationIdOrHoldingId(poLine, existedPieces, existingPieceLocations, tenantId,
      () -> new Piece().withLocationId(existingPieceLocationId));
  }

  private List<Piece> processPiecesWithoutItemAndHoldingId(PoLine poLine, List<Piece> existedPieces, String existingPieceHoldingId,
                                                           String tenantId, List<Location> existingPieceLocations) {
    return processPiecesWithoutItemAndLocationIdOrHoldingId(poLine, existedPieces, existingPieceLocations, tenantId,
      () -> new Piece().withHoldingId(existingPieceHoldingId));
  }

  private List<Piece> processPiecesWithoutItemAndLocationIdOrHoldingId(PoLine poLine, List<Piece> existedPieces, List<Location> existingPieceLocations,
                                                                       String receivingTenantId, Supplier<Piece> pieceSupplier) {
    Map<Piece.Format, Integer> expectedQuantitiesWithoutItem = calculatePiecesWithoutItemIdQuantity(poLine, existingPieceLocations);
    Map<Piece.Format, Integer> existedQuantityWithoutItem = calculateQuantityOfExistingPiecesWithoutItem(existedPieces);

    List<Piece> piecesToCreate = new ArrayList<Piece>();
    expectedQuantitiesWithoutItem.forEach((format, expectedQty) -> {
      int remainingPiecesQuantity = expectedQty - existedQuantityWithoutItem.getOrDefault(format, 0);
      for (int i = 0; i < remainingPiecesQuantity; i++) {
        piecesToCreate.add(pieceSupplier.get()
          .withFormat(format)
          .withPoLineId(poLine.getId())
          .withReceiptDate(PieceUtil.getExpectedReceiptDate(format, poLine))
          .withReceivingTenantId(receivingTenantId));
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
   * @param poLine    PO Line
   * @param locations list of locations to calculate quantity for
   * @return quantity of pieces per piece format either not required Inventory item for PO Line
   */
  private Map<Piece.Format, Integer> calculatePiecesWithoutItemIdQuantity(PoLine poLine, List<Location> locations) {
    // Piece records are not going to be created for PO Line which is going to be checked-in
    if (Boolean.TRUE.equals(poLine.getCheckinItems())) {
      return Collections.emptyMap();
    }

    var quantities = new EnumMap<Piece.Format, Integer>(Piece.Format.class);
    var orderFormat = poLine.getOrderFormat();
    if ((orderFormat == P_E_MIX || orderFormat == PHYSICAL_RESOURCE) && !isItemsUpdateRequiredForPhysical(poLine)) {
      quantities.put(Piece.Format.PHYSICAL, calculatePiecesQuantity(Piece.Format.PHYSICAL, locations));
    }
    if ((orderFormat == P_E_MIX || orderFormat == ELECTRONIC_RESOURCE) && !isItemsUpdateRequiredForEresource(poLine)) {
      quantities.put(Piece.Format.ELECTRONIC, calculatePiecesQuantity(Piece.Format.ELECTRONIC, locations));
    }
    if (orderFormat == OTHER && !isItemsUpdateRequiredForPhysical(poLine)) {
      quantities.put(Piece.Format.OTHER, calculatePiecesQuantity(Piece.Format.OTHER, locations));
    }
    return quantities;
  }
}
