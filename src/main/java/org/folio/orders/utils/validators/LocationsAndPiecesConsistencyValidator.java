package org.folio.orders.utils.validators;

import static java.util.stream.Collectors.groupingBy;
import static java.util.stream.Collectors.summingInt;
import static java.util.stream.Collectors.toMap;
import static org.folio.rest.core.exceptions.ErrorCodes.PIECES_TO_BE_DELETED;

import java.util.ArrayList;
import java.util.List;
import java.util.Map;
import java.util.Objects;
import java.util.Optional;

import org.apache.commons.collections4.CollectionUtils;
import org.folio.rest.core.exceptions.HttpException;
import org.folio.rest.jaxrs.model.CompositePoLine;
import org.folio.rest.jaxrs.model.Location;
import org.folio.rest.jaxrs.model.Piece;
import org.folio.rest.jaxrs.model.PieceCollection;

public class LocationsAndPiecesConsistencyValidator {


  public static void verifyLocationsAndPiecesConsistency(List<CompositePoLine> poLines, PieceCollection pieces) {
    if (CollectionUtils.isNotEmpty(poLines)) {
      Map<String, Map<String, Integer>> numOfLocationsByPoLineIdAndLocationIdV = numOfLocationsByPoLineIdAndLocationId(poLines);
      Map<String, Map<String, Integer>> numOfPiecesByPoLineIdAndLocationIdV = numOfPiecesByPoLineAndLocationId(pieces);

      numOfPiecesByPoLineIdAndLocationIdV.forEach((poLineId, numOfPiecesByLocationId) -> numOfPiecesByLocationId
        .forEach((locationId, quantity) -> {
          Integer numOfPieces = 0;
          if (numOfLocationsByPoLineIdAndLocationIdV.get(poLineId) != null && numOfLocationsByPoLineIdAndLocationIdV.get(poLineId).get(locationId) != null) {
            numOfPieces = numOfLocationsByPoLineIdAndLocationIdV.get(poLineId).get(locationId);
          }
          if (quantity > numOfPieces) {
            throw new HttpException(422, PIECES_TO_BE_DELETED.toError());
          }
        }));
    }
  }

  private static Map<String, Map<String, Integer>> numOfPiecesByPoLineAndLocationId(PieceCollection pieces) {
    return pieces.getPieces().stream()
      .filter(piece -> Objects.nonNull(piece.getPoLineId()) &&
        (Objects.nonNull(piece.getLocationId()) || Objects.nonNull(piece.getHoldingId())))
      .collect(groupingBy(Piece::getPoLineId, groupingBy(LocationsAndPiecesConsistencyValidator::buildPieceKey, summingInt(q -> 1))));
  }

  private static Map<String, Map<String, Integer>> numOfLocationsByPoLineIdAndLocationId(List<CompositePoLine> poLines) {
    return poLines.stream()
      .filter(line -> !line.getIsPackage() &&
        line.getReceiptStatus() != CompositePoLine.ReceiptStatus.RECEIPT_NOT_REQUIRED &&
        !line.getCheckinItems())
      .collect(toMap(CompositePoLine::getId, poLine -> Optional.of(poLine.getLocations())
        .orElse(new ArrayList<>()).stream().collect(toMap(LocationsAndPiecesConsistencyValidator::buildLocationKey, Location::getQuantity, Integer::sum))));
  }

  private static String buildLocationKey(Location location) {
    return Optional.ofNullable(location.getLocationId())
      .map(locationId -> "L" + locationId)
      .orElse("H" + location.getHoldingId());
  }

  private static String buildPieceKey(Piece piece) {
    return Optional.ofNullable(piece.getLocationId())
      .map(locationId -> "L" + locationId)
      .orElse("H" + piece.getHoldingId());
  }
}
